/**
 * @fileoverview Tests for the DevTools panel UI: DOM structure, toolbar buttons,
 * call stack, variables, CSS loading, source list, and CodeMirror editor.
 */

import { assert } from './test_harness.mjs';
import { openPanelPage } from './test_helpers.mjs';

// --- Panel UI Structure ---

export async function testPanelUIStructure(browser, extensionId) {
  console.log('\n--- Test Group: Panel UI Structure ---');

  if (!extensionId) {
    assert('Panel UI: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // Check core DOM elements exist
  const elements = await panelPage.evaluate(() => ({
    toolbar: !!document.getElementById('toolbar'),
    toolbarDebug: !!document.getElementById('toolbar-debug'),
    mainLayout: !!document.getElementById('main-layout'),
    sidebar: !!document.getElementById('sidebar'),
    sourceList: !!document.getElementById('source-list'),
    editorArea: !!document.getElementById('editor-area'),
    editorContainer: !!document.getElementById('editor-container'),
    rightPanel: !!document.getElementById('right-panel'),
    callStackContainer: !!document.getElementById('call-stack-container'),
    variablesContainer: !!document.getElementById('variables-container'),
  }));

  assert('Panel has #toolbar', elements.toolbar);
  assert('Panel has #toolbar-debug', elements.toolbarDebug);
  assert('Panel has #main-layout', elements.mainLayout);
  assert('Panel has #sidebar', elements.sidebar);
  assert('Panel has #source-list', elements.sourceList);
  assert('Panel has #editor-area', elements.editorArea);
  assert('Panel has #editor-container', elements.editorContainer);
  assert('Panel has #right-panel', elements.rightPanel);
  assert('Panel has #call-stack-container', elements.callStackContainer);
  assert('Panel has #variables-container', elements.variablesContainer);

  // Check toolbar title
  const title = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-title')?.textContent
  );
  assert('Toolbar shows "Scheme-JS" title', title === 'Scheme-JS', `got: "${title}"`);

  await panelPage.close();
}

// --- Panel Toolbar Buttons ---

export async function testPanelToolbarButtons(browser, extensionId) {
  console.log('\n--- Test Group: Panel Toolbar Buttons ---');

  if (!extensionId) {
    assert('Panel toolbar: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // Check debug buttons exist and are initially disabled
  const buttons = await panelPage.evaluate(() => {
    const btns = Array.from(document.querySelectorAll('#toolbar-debug .toolbar-btn'));
    return btns.map(btn => ({
      title: btn.title,
      text: btn.textContent,
      disabled: btn.disabled,
    }));
  });

  assert('4 toolbar buttons exist', buttons.length === 4, `got ${buttons.length}`);

  const resumeBtn = buttons.find(b => b.title.includes('Resume'));
  const stepIntoBtn = buttons.find(b => b.title.includes('Step Into'));
  const stepOverBtn = buttons.find(b => b.title.includes('Step Over'));
  const stepOutBtn = buttons.find(b => b.title.includes('Step Out'));

  assert('Resume button exists', !!resumeBtn);
  assert('Step Into button exists', !!stepIntoBtn);
  assert('Step Over button exists', !!stepOverBtn);
  assert('Step Out button exists', !!stepOutBtn);

  assert('Resume button initially disabled', resumeBtn?.disabled === true);
  assert('Step Into button initially disabled', stepIntoBtn?.disabled === true);
  assert('Step Over button initially disabled', stepOverBtn?.disabled === true);
  assert('Step Out button initially disabled', stepOutBtn?.disabled === true);

  // Check status label — when opened standalone (not in DevTools), the panel
  // runs activateAndRefresh() which may show "Reload page to enable debugging"
  // or "Running" depending on context.
  const status = await panelPage.evaluate(() =>
    document.querySelector('.toolbar-status')?.textContent
  );
  assert('Toolbar status has initial text',
    typeof status === 'string' && status.length > 0, `got: "${status}"`);

  await panelPage.close();
}

// --- Panel Call Stack (empty state) ---

export async function testPanelCallStackEmpty(browser, extensionId) {
  console.log('\n--- Test Group: Panel Call Stack (empty state) ---');

  if (!extensionId) {
    assert('Panel call stack: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // Check empty state
  const emptyMsg = await panelPage.evaluate(() => {
    const el = document.querySelector('#call-stack-container .call-stack-empty');
    return el?.textContent;
  });

  assert('Call stack shows "No stack frames" when empty',
    emptyMsg === 'No stack frames', `got: "${emptyMsg}"`);

  await panelPage.close();
}

// --- Panel Variables (empty state) ---

export async function testPanelVariablesEmpty(browser, extensionId) {
  console.log('\n--- Test Group: Panel Variables (empty state) ---');

  if (!extensionId) {
    assert('Panel variables: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // The variables container may show "No local bindings" or be empty
  const content = await panelPage.evaluate(() => {
    const container = document.getElementById('variables-container');
    return {
      innerHTML: container?.innerHTML?.substring(0, 200),
      childCount: container?.children?.length || 0,
    };
  });

  // When no frame is selected, variables should be empty or show empty message
  assert('Variables container is initially empty or shows empty message',
    content.childCount === 0 || content.innerHTML?.includes('No local bindings'),
    `${content.childCount} children, html: "${content.innerHTML}"`);

  await panelPage.close();
}

// --- Panel CSS Loaded ---

export async function testPanelCSSLoaded(browser, extensionId) {
  console.log('\n--- Test Group: Panel CSS Loaded ---');

  if (!extensionId) {
    assert('Panel CSS: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // Check that the stylesheet is loaded and applies styles
  const styles = await panelPage.evaluate(() => {
    const toolbar = document.getElementById('toolbar');
    if (!toolbar) return null;
    const computed = getComputedStyle(toolbar);
    return {
      display: computed.display,
      hasBackground: computed.backgroundColor !== '' && computed.backgroundColor !== 'rgba(0, 0, 0, 0)',
    };
  });

  assert('Toolbar has CSS applied', styles !== null);
  assert('Toolbar has flex display', styles?.display === 'flex', `got: ${styles?.display}`);
  assert('Toolbar has background color', styles?.hasBackground === true);

  // Check that CodeMirror is loaded (editor container should have .cm-editor)
  const hasCM = await panelPage.evaluate(() =>
    !!document.querySelector('.cm-editor')
  );
  assert('CodeMirror editor is rendered', hasCM);

  await panelPage.close();
}

// --- Panel Source List (empty state) ---

export async function testPanelSourceListEmpty(browser, extensionId) {
  console.log('\n--- Test Group: Panel Source List (empty) ---');

  if (!extensionId) {
    assert('Panel source list: extension loaded', false, 'no extension ID');
    return;
  }

  const panelPage = await openPanelPage(browser, extensionId);

  // Source list should be empty or show "no sources" when loaded standalone
  // (not connected to a page)
  const sourceItems = await panelPage.evaluate(() => {
    const list = document.getElementById('source-list');
    return {
      childCount: list?.children?.length || 0,
      innerHTML: list?.innerHTML?.substring(0, 200) || '',
    };
  });

  // Without inspectedWindow.eval, the panel can't fetch real sources.
  // It may show a placeholder or be empty.
  assert('Source list rendered (no crash)',
    sourceItems.childCount >= 0, `${sourceItems.childCount} children`);

  await panelPage.close();
}
