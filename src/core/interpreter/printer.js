import { Cons } from './cons.js';
import { Symbol } from './symbol.js';
import { isSchemeClosure, isSchemeContinuation } from './values.js';
import { SchemeString } from '../primitives/string_class.js';
import { LiteralNode, VariableNode } from './ast.js'; // VariableNode used in web/repl, LiteralNode in both
import { writeString, isCircular } from '../primitives/io/printer.js';

/**
 * Pretty-prints a Scheme value for the REPL.
 * @param {*} val - The value from the interpreter.
 * @returns {string}
 */
export function prettyPrint(val) {
    if (val instanceof LiteralNode) {
        return prettyPrint(val.value);
    }
    // Circular structure is shown as `write` shows it, with datum labels:
    // followed as a tree, it would never end. Checked once, here, rather
    // than at each level the printing below recurses through.
    if (isCircular(val)) {
        return writeString(val);
    }
    return prettyPrintValue(val);
}

/**
 * Pretty-prints a value that holds no circular structure.
 * @param {*} val
 * @returns {string}
 */
function prettyPrintValue(val) {
    // A mutable string is shown as the characters it holds.
    if (val instanceof SchemeString) val = val.toString();
    // Check for Scheme closures (callable functions with marker)
    if (isSchemeClosure(val)) {
        return "#<procedure>";
    }
    // Check for Scheme continuations (callable functions with marker)
    if (isSchemeContinuation(val)) {
        return "#<continuation>";
    }
    // Regular JS functions
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
        return `#(${val.map(prettyPrintValue).join(' ')})`;
    }
    // Handle BigInt (exact integers)
    if (typeof val === 'bigint') {
        return `${val}`;
    }
    // Inexact numbers, as write writes them: a copy of its rules here showed
    // -0.0 as 0.0 and 1e21 as 1e+21.0, which read takes for a symbol.
    if (typeof val === 'number') {
        return writeString(val);
    }
    // Other objects (Rational, Complex, etc.) use their toString method
    return `${val}`;
}

function prettyPrintList(cons) {
    const elems = [];
    let curr = cons;
    while (curr instanceof Cons) {
        elems.push(prettyPrintValue(curr.car));
        curr = curr.cdr;
    }
    if (curr !== null) {
        // Improper list
        return `${elems.join(' ')} . ${prettyPrintValue(curr)}`;
    }
    return elems.join(' ');
}
