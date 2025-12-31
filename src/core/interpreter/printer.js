import { Cons } from './cons.js';
import { Symbol } from './symbol.js';
import { Closure, Continuation } from './values.js';
import { LiteralNode, VariableNode } from './ast.js'; // VariableNode used in web/repl, LiteralNode in both

/**
 * Pretty-prints a Scheme value for the REPL.
 * @param {*} val - The value from the interpreter.
 * @returns {string}
 */
export function prettyPrint(val) {
    if (val instanceof LiteralNode) {
        return prettyPrint(val.value);
    }
    if (val instanceof Closure) {
        return "#<procedure>";
    }
    if (val instanceof Continuation) {
        return "#<continuation>";
    }
    if (typeof val === 'function') {
        return "#<procedure>";
    }
    if (val instanceof VariableNode) {
        return val.name;
    }
    if (val instanceof Symbol) {
        return val.name;
    }

    if (val instanceof Cons) {
        return `(${prettyPrintList(val)})`;
    }
    if (val === null) {
        return "'()";
    }
    if (val === true) {
        return "#t";
    }
    if (val === false) {
        return "#f";
    }
    if (typeof val === 'string') {
        // Check if it's a display string or a symbol-like string
        if (val.startsWith('[Native Error:')) return val;
        return `"${val.replace(/"/g, '\\"')}"`; // Show as string
    }
    if (Array.isArray(val)) {
        return `#(${val.map(prettyPrint).join(' ')})`;
    }
    // Handle BigInt (exact integers)
    if (typeof val === 'bigint') {
        return `${val}`;
    }
    // Handle Numbers (inexact)
    if (typeof val === 'number') {
        // Display inexact integers with decimal point to distinguish from exact
        if (Number.isInteger(val) && Number.isFinite(val)) {
            return `${val}.0`;
        }
        // Handle special values
        if (val === Infinity) return '+inf.0';
        if (val === -Infinity) return '-inf.0';
        if (Number.isNaN(val)) return '+nan.0';
        return `${val}`;
    }
    // Other objects (Rational, Complex, etc.) use their toString method
    return `${val}`;
}

function prettyPrintList(cons) {
    const elems = [];
    let curr = cons;
    while (curr instanceof Cons) {
        elems.push(prettyPrint(curr.car));
        curr = curr.cdr;
    }
    if (curr !== null) {
        // Improper list
        return `${elems.join(' ')} . ${prettyPrint(curr)}`;
    }
    return elems.join(' ');
}
