# Fix Extension Breakpoint Pausing in Wrong Tab

## Planning
- [x] Analyze extension architecture and identify the root cause
- [x] Write implementation plan and get user approval

## Execution
- [x] Delete old sidebar files (`panel/sidebar.html`, `panel/sidebar.js`, `panel/sidebar.css`)
- [x] Modify `background.js` to auto-resume Scheme probe pauses via CDP
- [x] Update `background.js` JSDoc/comments to reflect new architecture
- [x] Write `test_auto_resume.mjs` — mock-based panel tests
- [x] Write `test_auto_resume.mjs` — real page-side tests
- [x] Write `test_auto_resume.mjs` — click-based UI simulation tests
- [x] Register tests in `run_extension_tests.mjs`
- [x] Fix diamond icon visibility in dark mode (move colors to dark/light theme compartments)
- [x] Update `MEMORY.md` for extension directory

## Core Bug Fix (Phase 2 — additional work)
- [x] Write `test_cdp_debugger_conflict.mjs` — tests that catch `debugger;` blocking cooperative channel
- [x] Write `test_panel_e2e_breakpoint.mjs` — end-to-end real breakpoint → panel UI integration tests
- [x] Fix probe `hit()` to return `false` when `panelConnected=true` (`probe_runtime.js`)
- [x] Set `_panelConnected` in `html_adapter.js` (on page reload via `__SCHEME_JS_PANELCONNECTED`)
- [x] Set `_panelConnected` in `devtools_debug.js` `activate()` (on live panel attach)
- [x] Register new tests in `run_extension_tests.mjs`
- [x] Fix `onThemeChanged` → `setThemeChangeHandler` (correct Chrome DevTools API)

## Verification
- [x] Run `npm run test:extension` — 306 passed, 0 failed
- [x] Run `npm test` — 2703 passed, 0 failed
- [x] Manual verification with scheme-blocks app confirmed working

## Documentation
- [x] Update CHANGES.md with walkthrough
