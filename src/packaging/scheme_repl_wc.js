import { setupRepl, replTemplate, replStyles } from '../../web/repl.js';

// These will be imported at runtime from the sibling scheme.js file
// NOT bundled into this file
import {
    interpreter,
    env,
    parse,
    analyze,
    prettyPrint,
    isCompleteExpression,
    findMatchingDelimiter,
    SchemeDebugRuntime,
    ReplDebugBackend,
    ReplDebugCommands
} from './scheme.js';

class SchemeRepl extends HTMLElement {
    constructor() {
        super();
        this.attachShadow({ mode: 'open' });
    }

    connectedCallback() {
        this.render();
        this.initializeRepl();
    }

    render() {
        // Inject styles
        const style = document.createElement('style');
        style.textContent = replStyles;
        this.shadowRoot.appendChild(style);

        // Inject HTML
        const container = document.createElement('div');
        // We need to use innerHTML for the template string
        container.innerHTML = replTemplate;

        // Append all children of the container to shadowRoot (unwrapping)
        while (container.firstChild) {
            this.shadowRoot.appendChild(container.firstChild);
        }
    }

    initializeRepl() {
        // We pass the shadowRoot as the root element for querySelector
        setupRepl(interpreter, env, this.shadowRoot, {
            parse,
            analyze,
            prettyPrint,
            isCompleteExpression,
            findMatchingDelimiter,
            SchemeDebugRuntime,
            ReplDebugBackend,
            ReplDebugCommands
        });
    }
}

customElements.define('scheme-repl', SchemeRepl);
