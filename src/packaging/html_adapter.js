import { schemeEvalAsync } from './scheme_entry.js';

async function runScripts() {
    const scripts = document.querySelectorAll('script[type="text/scheme"]');

    for (const script of scripts) {
        try {
            if (script.src) {
                const response = await fetch(script.src);
                if (!response.ok) {
                    throw new Error(`Failed to load Scheme script: ${script.src}`);
                }
                const code = await response.text();
                await schemeEvalAsync(code);
            } else {
                await schemeEvalAsync(script.textContent);
            }
        } catch (err) {
            console.error('Error executing Scheme script:', err);
        }
    }
}

if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', runScripts);
} else {
    runScripts();
}
