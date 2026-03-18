/**
 * @fileoverview Comprehensive Puppeteer E2E tests for the Scheme-JS debugger.
 *
 * Tests the full user interaction flow that no other test layer can verify:
 *   - Breakpoint setting, hitting, and removing
 *   - Pause/resume with correct source info
 *   - Step Into / Step Over / Step Out
 *   - Call stack correctness during pause
 *   - Variable inspection (getLocals)
 *   - Eval during pause (__schemeDebug.eval)
 *   - Multiple source files (inline + external)
 *   - Breakpoints on different expression types
 *   - Extension cross-world relay (postMessage)
 *   - Execution timing (no 5-second hangs)
 *   - Pause persistence (ackPause)
 *   - Panel UI structure and initial state
 *
 * Prerequisites:
 *   - Chrome installed
 *   - `npm run build && npm run build:panel`
 *
 * Usage:
 *   node tests/extension/run_extension_tests.mjs
 *   npm run test:extension
 */

import { launchTestBrowser, getResults, recordCrash } from './test_harness.mjs';

// Page-side tests
import { testActivationAndSources, testExecutionTiming } from './test_activation.mjs';
import {
  testBreakpointPauseResume, testMultipleBreakpoints,
  testVariableBreakpoint, testLiteralBreakpoint,
  testExternalFileBreakpoint, testBreakpointAPI,
} from './test_breakpoints.mjs';
import { testStepInto, testStepOver, testStepOut } from './test_stepping.mjs';
import { testCallStack, testVariableInspection, testEvalDuringPause } from './test_inspection.mjs';
import { testPostMessageRelay, testPausePersistence, testBreakpointFlowPage } from './test_relay.mjs';

// Panel UI tests (static)
import {
  testPanelUIStructure,
  testPanelToolbarButtons,
  testPanelCallStackEmpty,
  testPanelVariablesEmpty,
  testPanelCSSLoaded,
  testPanelSourceListEmpty,
  testThemeSwitching
} from './test_panel_ui.mjs';

// Panel interaction tests (mocked chrome APIs)
import {
  testPanelSourcesLoad, testPanelSourceSwitch,
  testPanelPauseUpdatesUI, testPanelResumeUpdatesUI,
  testPanelResumeButtonClick, testPanelStepButtonClicks,
  testPanelFrameSelectionUpdatesVariables, testPanelVariableTypeColors,
  testPanelCallStackTCO, testPanelBreakpointGutterToggle,
  testPanelStepReasonStatus, testPanelPauseNavigatesEditor,
  testPanelCallStackBadges, testPanelEmptyVariablesMessage,
  testPanelExpressionHighlightOnPause, testPanelExpressionHighlightClears,
  testPanelDiamondMarkersOnBreakpointLine,
  testPanelDiamondClickSetsBreakpoint,
  testPanelDiamondsOnPausedLine,
  testPanelBreakpointsList,
  testPanelBreakpointsNavigation,
  testKeyboardShortcuts,
  testCodeMirrorSearch
} from './test_panel_interactions.mjs';

import {
  testConsoleEvaluation,
  testConsoleHistory,
  testConsoleClear
} from './test_console.mjs';

// Phase 4: JS debugging tests (CDP bridge + unified debugger)
import {
  testJSPauseShowsJSFrames,
  testUnifiedCallStackInterleaving,
  testUnifiedCallStackBadges,
  testJSStepInto,
  testJSStepOver,
  testJSStepOut,
  testJSResumeFromPause,
  testSchemeStepStillWorks,
  testCDPResumeUpdatesUI,
  testJSFrameClickLoadsJSSource,
  testEditorSwitchesToJSHighlighting,
  testSwitchBetweenSchemeAndJSFrames,
  testJSVariablesDisplayed,
  testPanelShowsSchemeSourcesAsDefault,
  testJSPauseToolbarStatus,
  testSchemeCallsJSShowsBothFrames,
  testJSCallsSchemeShowsBothFrames,
  testButtonsEnabledDuringJSPause,
  testCurrentLineHighlightOnJSPause,
  testBoundaryStepping,
} from './test_js_debugging.mjs';

// Auto-resume tests (cooperative channel vs CDP channel)
import {
  testSchemeProbeAutoResumed,
  testNonSchemeProbeNotAutoResumed,
  testBreakpointPausesCooperatively,
  testBreakpointResumeAndContinue,
  testSteppingWorksAfterAutoResume,
  testPanelPauseShowsUIViaCooperativeChannel,
  testPanelResumeClickAfterCooperativePause,
  testPanelStepButtonsAfterCooperativePause,
  testPanelBreakpointSetViaGutterClick,
} from './test_auto_resume.mjs';

// CDP debugger conflict tests (probe debugger; vs cooperative pause)
import {
  testProbeHitSkipsWhenPanelConnected,
  testProbeHitSkipsDuringSteppingWhenPanelConnected,
  testCooperativePauseWithCDPDebuggerAttached,
} from './test_cdp_debugger_conflict.mjs';

// E2E breakpoint → panel UI tests
import {
  testRealBreakpointUpdatesPanelUI,
  testRealBreakpointSourceLocationInPanel,
} from './test_panel_e2e_breakpoint.mjs';

// Error path tests
import {
  testEvalTimeout,
  testEvalError,
  testMissingSchemeDebug,
  testMalformedPauseEvent,
  testGetLocalsFailure,
  testCDPSendMessageFailure,
  testUnknownSourceContent,
} from './test_error_paths.mjs';

// Phase 4: JS interop integration tests (real page, no mocks)
import {
  testJSInteropPageSelfTest,
  testBreakpointAtJSCallSite,
  testStepIntoJSFromScheme,
  testStepOverJSFromScheme,
  testStepOutFromJSBoundary,
  testVariablesAtJSBoundary,
  testMultipleBreakpointsAcrossJSCalls,
  testDotNotationJSCall,
  testStoredJSFunctionCall,
  testCallStackAtJSBoundary,
  testStepThroughJSInteropSequence,
} from './test_js_interop.mjs';

// =========================================================================
// Main runner
// =========================================================================

// Parse --only filter: `--only panel_interactions,console` runs only matching test modules
const onlyArg = process.argv.find(a => a.startsWith('--only'));
const onlyFilters = onlyArg ? onlyArg.split('=')[1]?.split(',') || process.argv[process.argv.indexOf('--only') + 1]?.split(',') : null;

/**
 * Checks if a test function should run based on --only filter.
 * Matches against the function name or the source module name.
 * @param {Function} fn
 * @param {string} moduleName
 * @returns {boolean}
 */
function shouldRun(fn, moduleName) {
  if (!onlyFilters) return true;
  return onlyFilters.some(f =>
    moduleName.includes(f) || fn.name?.toLowerCase().includes(f.toLowerCase())
  );
}

async function runTests() {
  console.log('\nScheme-JS Comprehensive Puppeteer E2E Tests');
  if (onlyFilters) console.log(`Filter: ${onlyFilters.join(', ')}`);
  console.log('='.repeat(55));

  const { server, browser, extensionId } = await launchTestBrowser();

  try {
    // Page-side API tests (no extension ID needed)
    const pageTests = [
      ['activation', testActivationAndSources],
      ['activation', testExecutionTiming],
      ['breakpoints', testBreakpointPauseResume],
      ['inspection', testCallStack],
      ['inspection', testVariableInspection],
      ['inspection', testEvalDuringPause],
      ['stepping', testStepInto],
      ['stepping', testStepOver],
      ['stepping', testStepOut],
      ['breakpoints', testMultipleBreakpoints],
      ['breakpoints', testVariableBreakpoint],
      ['breakpoints', testLiteralBreakpoint],
      ['breakpoints', testExternalFileBreakpoint],
      ['relay', testPostMessageRelay],
      ['relay', testPausePersistence],
      ['breakpoints', testBreakpointAPI],
      ['relay', testBreakpointFlowPage],
      // Phase 4: JS interop integration tests (real page-side debugging)
      ['js_interop', testJSInteropPageSelfTest],
      ['js_interop', testBreakpointAtJSCallSite],
      ['js_interop', testStepIntoJSFromScheme],
      ['js_interop', testStepOverJSFromScheme],
      ['js_interop', testStepOutFromJSBoundary],
      ['js_interop', testVariablesAtJSBoundary],
      ['js_interop', testMultipleBreakpointsAcrossJSCalls],
      ['js_interop', testDotNotationJSCall],
      ['js_interop', testStoredJSFunctionCall],
      ['js_interop', testCallStackAtJSBoundary],
      ['js_interop', testStepThroughJSInteropSequence],
      // CDP debugger conflict: probe debugger; vs cooperative pause
      ['cdp_conflict', testProbeHitSkipsWhenPanelConnected],
      ['cdp_conflict', testProbeHitSkipsDuringSteppingWhenPanelConnected],
      ['cdp_conflict', testCooperativePauseWithCDPDebuggerAttached],
      // Auto-resume: real page-side verification
      ['auto_resume', testBreakpointPausesCooperatively],
      ['auto_resume', testBreakpointResumeAndContinue],
      ['auto_resume', testSteppingWorksAfterAutoResume],
    ];

    for (const [mod, testFn] of pageTests) {
      if (!shouldRun(testFn, mod)) continue;
      try {
        await testFn(browser);
      } catch (err) {
        recordCrash(testFn.name || 'unknown', err.message);
      }
    }

    // Panel UI tests (require extension ID)
    const panelTests = [
      // Static structure tests
      ['panel_ui', testPanelUIStructure],
      ['panel_ui', testPanelToolbarButtons],
      ['panel_ui', testPanelCallStackEmpty],
      ['panel_ui', testPanelVariablesEmpty],
      ['panel_ui', testPanelCSSLoaded],
      ['panel_ui', testPanelSourceListEmpty],
      ['panel_ui', testThemeSwitching],
      // Interaction tests (mocked chrome APIs)
      ['panel_interactions', testPanelSourcesLoad],
      ['panel_interactions', testPanelSourceSwitch],
      ['panel_interactions', testPanelPauseUpdatesUI],
      ['panel_interactions', testPanelResumeUpdatesUI],
      ['panel_interactions', testPanelResumeButtonClick],
      ['panel_interactions', testPanelStepButtonClicks],
      ['panel_interactions', testPanelFrameSelectionUpdatesVariables],
      ['panel_interactions', testPanelVariableTypeColors],
      ['panel_interactions', testPanelCallStackTCO],
      ['panel_interactions', testPanelBreakpointGutterToggle],
      ['panel_interactions', testPanelStepReasonStatus],
      ['panel_interactions', testPanelPauseNavigatesEditor],
      ['panel_interactions', testPanelCallStackBadges],
      ['panel_interactions', testPanelEmptyVariablesMessage],
      // Expression-level breakpoints (Phase 3)
      ['panel_interactions', testPanelExpressionHighlightOnPause],
      ['panel_interactions', testPanelExpressionHighlightClears],
      ['panel_interactions', testPanelDiamondMarkersOnBreakpointLine],
      ['panel_interactions', testPanelDiamondClickSetsBreakpoint],
      ['panel_interactions', testPanelDiamondsOnPausedLine],
      // Phase 6: Breakpoints and Console
      ['panel_interactions', testPanelBreakpointsList],
      ['panel_interactions', testPanelBreakpointsNavigation],
      ['console', testConsoleEvaluation],
      ['console', testConsoleHistory],
      ['console', testConsoleClear],
      ['panel_interactions', testKeyboardShortcuts],
      ['panel_interactions', testCodeMirrorSearch],
      ['js_debugging', testJSPauseShowsJSFrames],
      ['js_debugging', testUnifiedCallStackInterleaving],
      ['js_debugging', testUnifiedCallStackBadges],
      ['js_debugging', testJSStepInto],
      ['js_debugging', testJSStepOver],
      ['js_debugging', testJSStepOut],
      ['js_debugging', testJSResumeFromPause],
      ['js_debugging', testSchemeStepStillWorks],
      ['js_debugging', testCDPResumeUpdatesUI],
      ['js_debugging', testJSFrameClickLoadsJSSource],
      ['js_debugging', testEditorSwitchesToJSHighlighting],
      ['js_debugging', testSwitchBetweenSchemeAndJSFrames],
      ['js_debugging', testJSVariablesDisplayed],
      ['js_debugging', testPanelShowsSchemeSourcesAsDefault],
      ['js_debugging', testJSPauseToolbarStatus],
      ['js_debugging', testSchemeCallsJSShowsBothFrames],
      ['js_debugging', testJSCallsSchemeShowsBothFrames],
      ['js_debugging', testButtonsEnabledDuringJSPause],
      ['js_debugging', testCurrentLineHighlightOnJSPause],
      ['js_debugging', testBoundaryStepping],
      // Auto-resume: mock-based panel + click-based UI
      ['auto_resume', testSchemeProbeAutoResumed],
      ['auto_resume', testNonSchemeProbeNotAutoResumed],
      ['auto_resume', testPanelPauseShowsUIViaCooperativeChannel],
      ['auto_resume', testPanelResumeClickAfterCooperativePause],
      ['auto_resume', testPanelStepButtonsAfterCooperativePause],
      ['auto_resume', testPanelBreakpointSetViaGutterClick],
      // Error path tests
      ['error_paths', testEvalTimeout],
      ['error_paths', testEvalError],
      ['error_paths', testMissingSchemeDebug],
      ['error_paths', testMalformedPauseEvent],
      ['error_paths', testGetLocalsFailure],
      ['error_paths', testCDPSendMessageFailure],
      ['error_paths', testUnknownSourceContent],
      // E2E breakpoint → panel UI tests
      ['panel_e2e', testRealBreakpointUpdatesPanelUI],
      ['panel_e2e', testRealBreakpointSourceLocationInPanel],
    ];

    if (extensionId) {
      for (const [mod, testFn] of panelTests) {
        if (!shouldRun(testFn, mod)) continue;
        try {
          await testFn(browser, extensionId);
        } catch (err) {
          recordCrash(testFn.name || 'unknown', err.message);
        }
      }
    } else {
      console.log('\n--- Panel UI tests SKIPPED (no extension ID) ---');
    }

  } finally {
    if (browser) await browser.close();
    server.close();
  }

  // Summary
  const { passed, failed, failures } = getResults();
  console.log('\n' + '='.repeat(55));
  console.log(`Results: ${passed} passed, ${failed} failed`);
  if (failures.length > 0) {
    console.log('\nFailures:');
    for (const f of failures) {
      console.log(`  - ${f}`);
    }
  }
  console.log();

  process.exit(failed > 0 ? 1 : 0);
}

runTests().catch(err => {
  console.error('Test runner error:', err);
  process.exit(1);
});
