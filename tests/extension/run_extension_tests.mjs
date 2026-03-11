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
  testPanelUIStructure, testPanelToolbarButtons,
  testPanelCallStackEmpty, testPanelVariablesEmpty,
  testPanelCSSLoaded, testPanelSourceListEmpty,
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
} from './test_panel_interactions.mjs';

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

async function runTests() {
  console.log('\nScheme-JS Comprehensive Puppeteer E2E Tests');
  console.log('='.repeat(55));

  const { server, browser, extensionId } = await launchTestBrowser();

  try {
    // Page-side API tests (no extension ID needed)
    const pageTests = [
      testActivationAndSources,
      testExecutionTiming,
      testBreakpointPauseResume,
      testCallStack,
      testVariableInspection,
      testEvalDuringPause,
      testStepInto,
      testStepOver,
      testStepOut,
      testMultipleBreakpoints,
      testVariableBreakpoint,
      testLiteralBreakpoint,
      testExternalFileBreakpoint,
      testPostMessageRelay,
      testPausePersistence,
      testBreakpointAPI,
      testBreakpointFlowPage,
      // Phase 4: JS interop integration tests (real page-side debugging)
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
    ];

    for (const testFn of pageTests) {
      try {
        await testFn(browser);
      } catch (err) {
        recordCrash(testFn.name || 'unknown', err.message);
      }
    }

    // Panel UI tests (require extension ID)
    const panelTests = [
      // Static structure tests
      testPanelUIStructure,
      testPanelToolbarButtons,
      testPanelCallStackEmpty,
      testPanelVariablesEmpty,
      testPanelCSSLoaded,
      testPanelSourceListEmpty,
      // Interaction tests (mocked chrome APIs)
      testPanelSourcesLoad,
      testPanelSourceSwitch,
      testPanelPauseUpdatesUI,
      testPanelResumeUpdatesUI,
      testPanelResumeButtonClick,
      testPanelStepButtonClicks,
      testPanelFrameSelectionUpdatesVariables,
      testPanelVariableTypeColors,
      testPanelCallStackTCO,
      testPanelBreakpointGutterToggle,
      testPanelStepReasonStatus,
      testPanelPauseNavigatesEditor,
      testPanelCallStackBadges,
      testPanelEmptyVariablesMessage,
      // Expression-level breakpoints (Phase 3)
      testPanelExpressionHighlightOnPause,
      testPanelExpressionHighlightClears,
      testPanelDiamondMarkersOnBreakpointLine,
      testPanelDiamondClickSetsBreakpoint,
      testPanelDiamondsOnPausedLine,
      // Phase 4: JS debugging interaction tests (CDP mock)
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
    ];

    if (extensionId) {
      for (const testFn of panelTests) {
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
