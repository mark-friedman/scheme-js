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
    if (!ui) {
      return {
        url: location.script()?.sourceURL ?? '', line: location.lineNumber + 1, column: location.columnNumber + 1,
        ignored: false
      };
    }
    return {
      url: ui.uiSourceCode.url(),
      line: ui.lineNumber + 1,
      column: (ui.columnNumber ?? 0) + 1,
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
      for (let waited = 0; waited <= args[2]; waited += 50) {
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
    case 'stack': {
      // Every frame of the pause, top first, as DevTools' call stack shows it.
      const details = debuggerModel.debuggerPausedDetails();
      if (!details) return null;
      const places = [];
      for (const frame of details.callFrames) places.push(await place({ callFrames: [frame] }));
      return places;
    }
    case 'customFormatters':
      // As its user turns them on in DevTools' settings.
      Common.Settings.Settings.instance().moduleSetting('custom-formatters').set(args[0]);
      await sleep(200);
      return true;
    case 'drawn': {
      // Each of the top frame's own locals, as the custom formatters draw its
      // header -- the text of their JsonML -- or null where they leave it to
      // DevTools, and whether they give it a body.
      const frame = debuggerModel.debuggerPausedDetails()?.callFrames[0];
      if (!frame) return null;
      const local = frame.scopeChain().find((scope) => scope.type() === 'local');
      const { properties } = await local.object().getAllProperties(false, false, true);
      // An element is a tag, its attributes, then its children; an `object`
      // element, a value DevTools draws, has no text of its own.
      const text = (jsonml) => (typeof jsonml === 'string' ? jsonml
        : Array.isArray(jsonml) && jsonml[0] !== 'object' ? jsonml.slice(2).map(text).join('') : '');
      return Object.fromEntries((properties ?? []).map((property) => {
        const preview = property.value?.customPreview?.() ?? null;
        return [property.name, preview === null ? null
          : { header: text(JSON.parse(preview.header)), body: preview.bodyGetterId !== undefined }];
      }));
    }
    case 'locals': {
      // The top frame's own scope, by the names DevTools' Scope pane shows,
      // which it takes from the source maps where they give any.
      const SourceMapScopes = await import('./models/source_map_scopes/source_map_scopes.js');
      const frame = debuggerModel.debuggerPausedDetails()?.callFrames[0];
      if (!frame) return null;
      const chain = await SourceMapScopes.NamesResolver.resolveScopeChain(frame);
      const local = chain.find((scope) => scope.type() === 'local');
      if (!local) return [];
      const { properties } = await local.object().getAllProperties(false, false);
      return (properties ?? []).map((property) => property.name);
    }
    case 'experiment': {
      // As its user turns one on or off in DevTools' settings, which every
      // DevTools window of the browser shares.
      const Root = await import('./core/root/root.js');
      Root.Runtime.experiments.setEnabled(args[0], args[1]);
      return Root.Runtime.experiments.isEnabled(args[0]);
    }
    case 'scopes': {
      // The top frame's name, as DevTools' call stack shows it, and its
      // scopes, innermost first, as DevTools resolves them from its script's
      // source map (`SourceMapScopesInfo`): each scope's kind, its name, and
      // each of its variables with its value's description, or null for one
      // unavailable. The Scope pane of the DevTools Chrome 146 carries does
      // not ask for them yet, and shows the JavaScript's scopes.
      const SourceMapScopes = await import('./models/source_map_scopes/source_map_scopes.js');
      const frame = debuggerModel.debuggerPausedDetails()?.callFrames[0];
      if (!frame) return null;
      const chain = frame.script.sourceMap()?.resolveScopeChain(frame) ?? [];
      const scopes = [];
      for (const scope of chain) {
        const { properties } = await scope.object().getAllProperties(false, false);
        scopes.push({
          type: scope.type(),
          name: scope.name() ?? '',
          variables: (properties ?? []).map((property) => [property.name, property.value?.description ?? null])
        });
      }
      return { name: await SourceMapScopes.NamesResolver.resolveDebuggerFrameFunctionName(frame), scopes };
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
 * Each DevTools window attached to, as a Puppeteer page, by its target, and
 * those that are a tab's already: attaching to a window again, or asking a
 * window another tab has which tab it inspects, stalled for the length of
 * Puppeteer's protocol timeout, the second tab opened taking a minute.
 * @type {WeakMap<Object, Promise<Object|null>>}
 */
const attached = new WeakMap();
const claimed = new WeakSet();

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
        if (!target.url().startsWith('devtools://') || claimed.has(target)) continue;
        // Not every DevTools target is a window a page can be made of.
        if (!attached.has(target)) attached.set(target, target.asPage().catch(() => null));
        const window = await attached.get(target);
        if (window === null) continue;
        const inspected = await window.evaluate(async () => {
          const SDK = await import('./core/sdk/sdk.js');
          return SDK.TargetManager.TargetManager.instance().primaryPageTarget()?.inspectedURL() ?? null;
        }).catch(() => null);
        if (inspected === page.url()) {
          claimed.add(target);
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
   * DevTools binds it to code generated later too, as that code arrives.
   * @param {string} url - The source's URL.
   * @param {number} line - One-based.
   * @param {number} [ms] - How long to wait for it to be bound.
   * @returns {Promise<number>} How many places in the code it is bound to.
   */
  breakpoint(url, line, ms = 5000) {
    return this.call('breakpoint', url, line, ms);
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
   * @returns {Promise<{url: string, line: number, column: number, ignored: boolean}|null>}
   */
  pauseAfter(seen, ms = 15000) {
    return this.call('pauseAfter', seen, ms);
  }

  /**
   * Steps, as a user does with DevTools' buttons.
   * @param {'stepInto'|'stepOver'|'stepOut'} kind - The step.
   * @returns {Promise<{url: string, line: number, column: number, ignored: boolean}|null>}
   *   Where it paused next, or null if it did not.
   */
  step(kind) {
    return this.call('step', kind);
  }

  /**
   * Where each frame of the pause is, top first, as DevTools' call stack
   * shows it.
   * @returns {Promise<Array<{url: string, line: number, column: number, ignored: boolean}>|null>}
   */
  stack() {
    return this.call('stack');
  }

  /**
   * Turns DevTools' custom formatters on or off, as its user does in its
   * settings.
   * @param {boolean} on - Whether to.
   * @returns {Promise<void>}
   */
  customFormatters(on) {
    return this.call('customFormatters', on);
  }

  /**
   * How the custom formatters draw each of the paused frame's own locals.
   * @returns {Promise<Object<string, {header: string, body: boolean}|null>|null>}
   *   Each local's header's text and whether it has a body, or null where
   *   DevTools draws it itself; null if nothing is paused.
   */
  drawn() {
    return this.call('drawn');
  }

  /**
   * The names of the paused frame's own locals, as DevTools' Scope pane lists
   * them.
   * @returns {Promise<Array<string>|null>} Null if it is not paused.
   */
  locals() {
    return this.call('locals');
  }

  /**
   * Turns one of DevTools' experiments on or off.
   * @param {string} name - The experiment.
   * @param {boolean} on - Whether to.
   * @returns {Promise<boolean>} Whether it is on.
   */
  experiment(name, on) {
    return this.call('experiment', name, on);
  }

  /**
   * The paused frame's name, as DevTools shows it, and its scopes, as DevTools
   * resolves them from its source map.
   * @returns {Promise<{name: (string|null), scopes: Array<{type: string, name: string,
   *   variables: Array<[string, (string|null)]>}>}|null>} Null if it is not paused.
   */
  scopes() {
    return this.call('scopes');
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
