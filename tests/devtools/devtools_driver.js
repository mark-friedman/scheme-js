/**
 * @fileoverview Drives DevTools' own front end, in the DevTools window of a
 * Chrome tab, for tests of debugging Scheme and JavaScript together.
 *
 * Chrome opens DevTools for each tab when launched with `devtools: true`, and
 * Puppeteer can reach that window as a page. What a user of DevTools sees --
 * a breakpoint set in a Scheme file, where a pause is shown, how far a step
 * goes, what is skipped as ignore-listed -- is decided there, from what the
 * engine reports and the source maps: the front end places a pause by its map,
 * steps an original expression at a time, and asks the engine to skip what a
 * map or a pattern ignore-lists. So the tests drive that front end, through
 * its own modules, rather than a stand-in for it. Those modules are its
 * internals, and may change with Chrome; the tests run the Chrome Puppeteer
 * installs.
 */

/**
 * What the tests ask of the front end, run in the DevTools window. Kept whole
 * in one function, since Puppeteer sends a function's text to run there; the
 * state it keeps between calls is on the window.
 * @param {string} op - The operation.
 * @param {Array<*>} args - Its arguments.
 * @returns {Promise<*>}
 */
async function frontEnd(op, args) {
  const SDK = await import('./core/sdk/sdk.js');
  const Common = await import('./core/common/common.js');
  const Bindings = await import('./models/bindings/bindings.js');
  const Workspace = await import('./models/workspace/workspace.js');
  const Breakpoints = await import('./models/breakpoints/breakpoints.js');
  const debuggerModel = SDK.TargetManager.TargetManager.instance().primaryPageTarget()
    .model(SDK.DebuggerModel.DebuggerModel);
  const ignoreList = Workspace.IgnoreListManager.IgnoreListManager.instance();
  const workspace = Workspace.Workspace.WorkspaceImpl.instance();
  const breakpoints = Breakpoints.BreakpointManager.BreakpointManager.instance();
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  // A source by its URL, as a user opens it: a file a page fetched and that
  // source maps also name is listed twice, as fetched and as authored, and
  // only the authored copy -- the source maps' -- has code mapped to it.
  const sourceFor = (url) => workspace.uiSourceCodes().find((ui) => ui.url() === url
    && ui.project().id().startsWith('jsSourceMaps')) ?? workspace.uiSourceCodeForURL(url);

  // Where the top frame of a pause is, as DevTools shows it.
  const place = async (details) => {
    const location = details.callFrames[0].location();
    const ui = await Bindings.DebuggerWorkspaceBinding.DebuggerWorkspaceBinding.instance()
      .rawLocationToUILocation(location);
    if (!ui) return { url: location.script()?.sourceURL ?? '', line: location.lineNumber + 1, ignored: false };
    return {
      url: ui.uiSourceCode.url(),
      line: ui.lineNumber + 1,
      ignored: ignoreList.isUserOrSourceMapIgnoreListedUISourceCode(ui.uiSourceCode)
    };
  };
  // The next pause after the `seen`th, placed, or null.
  const pauseAfter = async (seen, ms) => {
    const state = globalThis.schemeTests;
    for (let waited = 0; state.pauses.length <= seen; waited += 25) {
      if (waited > ms) return null;
      await sleep(25);
    }
    return state.pauses[seen];
  };

  switch (op) {
    case 'attach': {
      const state = { pauses: [] };
      globalThis.schemeTests = state;
      debuggerModel.addEventListener(SDK.DebuggerModel.Events.DebuggerPaused, async () => {
        const details = debuggerModel.debuggerPausedDetails();
        const index = state.pauses.length;
        state.pauses.push(null);
        state.pauses[index] = details ? await place(details) : null;
      });
      return true;
    }
    case 'ignore':
      ignoreList.addRegexToIgnoreList(args[0]);
      await sleep(200);
      return true;
    case 'source': {
      for (let waited = 0; waited < 10000; waited += 50) {
        if (sourceFor(args[0])) return true;
        await sleep(50);
      }
      return false;
    }
    case 'breakpoint': {
      const ui = sourceFor(args[0]);
      if (!ui) return 0;
      await breakpoints.setBreakpoint(ui, args[1] - 1, undefined, '', true, false);
      for (let waited = 0; waited < 5000; waited += 50) {
        const bound = breakpoints.allBreakpointLocations()
          .filter((l) => l.uiLocation.uiSourceCode === ui && l.uiLocation.lineNumber === args[1] - 1);
        if (bound.length > 0) return bound.length;
        await sleep(50);
      }
      return 0;
    }
    case 'pauses':
      return globalThis.schemeTests.pauses.length;
    case 'pauseAfter':
      return pauseAfter(args[0], args[1]);
    case 'step': {
      const seen = globalThis.schemeTests.pauses.length;
      debuggerModel[args[0]]();
      return pauseAfter(seen, 10000);
    }
    case 'resume':
      for (const location of breakpoints.allBreakpointLocations()) await location.breakpoint.remove(false);
      if (debuggerModel.isPaused()) debuggerModel.resume();
      return true;
    case 'all':
      return globalThis.schemeTests.pauses;
    default:
      throw new Error(`no such operation: ${op}`);
  }
}

/**
 * DevTools' front end, for one tab.
 */
export class DevTools {
  /**
   * @param {Object} window - The DevTools window, as a Puppeteer page.
   */
  constructor(window) {
    this.window = window;
  }

  /**
   * Finds the DevTools window of a tab and starts noting its pauses.
   * @param {Object} browser - Puppeteer's browser, launched with `devtools`.
   * @param {Object} page - The tab, at the URL it is to be debugged at.
   * @returns {Promise<DevTools>}
   */
  static async open(browser, page) {
    for (let waited = 0; waited < 15000; waited += 100) {
      for (const target of browser.targets()) {
        if (!target.url().startsWith('devtools://')) continue;
        const window = await target.asPage();
        const inspected = await window.evaluate(async () => {
          const SDK = await import('./core/sdk/sdk.js');
          return SDK.TargetManager.TargetManager.instance().primaryPageTarget()?.inspectedURL() ?? null;
        }).catch(() => null);
        if (inspected === page.url()) {
          const devTools = new DevTools(window);
          await devTools.call('attach');
          return devTools;
        }
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    throw new Error(`no DevTools window for ${page.url()}`);
  }

  /**
   * Runs an operation in the front end.
   * @param {string} op - The operation.
   * @param {...*} args - Its arguments.
   * @returns {Promise<*>}
   */
  call(op, ...args) {
    return this.window.evaluate(frontEnd, op, args);
  }

  /**
   * Ignore-lists the scripts whose URLs match a pattern, as a user does.
   * @param {string} regex - The pattern.
   * @returns {Promise<void>}
   */
  ignore(regex) {
    return this.call('ignore', regex);
  }

  /**
   * Waits until DevTools lists a source, as it does a Scheme file once code
   * compiled from it is mapped to it.
   * @param {string} url - The source's URL.
   * @returns {Promise<boolean>} Whether it came.
   */
  source(url) {
    return this.call('source', url);
  }

  /**
   * Sets a breakpoint at a line of a source, as a user does in its view.
   * @param {string} url - The source's URL.
   * @param {number} line - One-based.
   * @returns {Promise<number>} How many places in the code it is bound to.
   */
  breakpoint(url, line) {
    return this.call('breakpoint', url, line);
  }

  /**
   * How many pauses there have been.
   * @returns {Promise<number>}
   */
  pauses() {
    return this.call('pauses');
  }

  /**
   * The pause after the `seen`th, as DevTools places it, once it comes.
   * @param {number} seen - How many there had been.
   * @param {number} [ms] - How long to wait.
   * @returns {Promise<{url: string, line: number, ignored: boolean}|null>}
   */
  pauseAfter(seen, ms = 15000) {
    return this.call('pauseAfter', seen, ms);
  }

  /**
   * Steps, as a user does with DevTools' buttons.
   * @param {'stepInto'|'stepOver'|'stepOut'} kind - The step.
   * @returns {Promise<{url: string, line: number, ignored: boolean}|null>}
   *   Where it paused next, or null if it did not.
   */
  step(kind) {
    return this.call('step', kind);
  }

  /**
   * Removes every breakpoint, and resumes.
   * @returns {Promise<void>}
   */
  resume() {
    return this.call('resume');
  }

  /**
   * Every pause there has been, placed.
   * @returns {Promise<Array<Object>>}
   */
  all() {
    return this.call('all');
  }
}
