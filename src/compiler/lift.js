/**
 * @fileoverview Deciding which nested procedures to emit once, at top level.
 *
 * ## The problem
 *
 * A procedure is emitted twice -- a fast form and a resumable one -- and each of
 * those emits both forms of every procedure nested inside it. So a lambda at
 * nesting depth *d* appears about 4^d times. Measured, that is 4.2x per level:
 * `earley.scm:make-parser` reaches 3.6 MB of generated source, and `map`,
 * `for-each`, `vector-map` and `string-map` together are a third of the
 * compiled standard library's bundle.
 *
 * ## The fix
 *
 * Emit each nested procedure once, at the top level of the generated unit, as a
 * factory taking its free variables:
 *
 *     function $mk$fn0(s_a, s_b) {
 *       function $fn0(s_x) { ... uses s_a, s_b, and $fn0 for recursion ... }
 *       function $fn0$r($pc, $f) { ... }
 *       $fn0.$resume = $fn0$r;
 *       return R.markProcedure($fn0, 'name');
 *     }
 *
 * and create it with `$t5 = $mk$fn0(s_a, s_b)`. Every form of every parent
 * shares that one emission, so code size becomes linear in the program rather
 * than exponential in its nesting.
 *
 * Nothing about variable *references* changes, which is what makes this cheap:
 * the inner function closes over the factory's parameters, and those have the
 * same names the body already used.
 *
 * ## Why free variables can be passed by value
 *
 * A free variable that is never assigned cannot be observed to differ between
 * the original and a copy, so passing it is safe. One that *is* assigned is
 * already held in a one-element array -- see `boxed` in `ir.js` -- so what gets
 * passed is the array, and every reader still shares one binding.
 *
 * ## `letrec`, which is the whole difficulty
 *
 * A `letrec`-bound lambda refers to names the group is still defining, so they
 * cannot be passed by value at creation time -- they have no value yet.
 *
 * Self-reference needs nothing: the factory declares the name and assigns the
 * procedure to it before returning, so the body's recursive call resolves
 * lexically inside the factory. That is the common case and the hot one, since
 * a named `let` is a single-binding `letrec`, and it stays a direct call.
 *
 * Mutual reference does need indirection, so a name referenced by one of its
 * *siblings* is boxed. The boxes are created before any initializer runs and
 * passed to each factory, so the group can refer to itself in any order. Only
 * such names pay for it: in `map`, `loop` reads `any-null?`, `all-cars` and
 * `all-cdrs`, so those three are boxed while `loop` itself is not.
 */

/**
 * What lifting decided about one compilation unit.
 *
 * @typedef {Object} LiftPlan
 * @property {Map<Object, Array<string>>} free - Free variables of each liftable
 *   lambda node, in a fixed order, which becomes its factory's parameter list.
 * @property {Set<Object>} lift - The lambda nodes to emit as factories.
 * @property {Set<string>} boxed - Locals that must be held in a box: those
 *   assigned anywhere, `letrec` names a sibling refers to, and internal
 *   definitions a nested procedure refers to.
 * @property {Map<Object, Array<string>>} selfNames - For a lifted `letrec`
 *   initializer, the group names it may bind inside its own factory rather than
 *   receive -- only ever its own name.
 */

/**
 * Plans lifting for a lowered procedure.
 *
 * @param {Object} ir - The unit's top-level `lambda` IR node.
 * @returns {LiftPlan} The plan.
 */
export function planLifting(ir) {
  // Every reason a local must be boxed is decided here, in one place. Assigned
  // locals were originally worked out during lowering, which split one decision
  // across two modules for no benefit -- the cases below are found by walking
  // this same IR, so this is a walk that had to happen anyway.
  const boxed = assignedLocals(ir);

  // A `letrec` name referenced by one of its siblings cannot be passed by
  // value, because the sibling is created before it exists.
  const groups = letrecGroups(ir);
  for (const group of groups) {
    for (let i = 0; i < group.names.length; i++) {
      const own = group.names[i];
      for (const name of group.names) {
        if (name === own) continue;
        // Referenced by a sibling rather than only by itself.
        if (referencedBy(group.inits[i], name)) boxed.add(name);
      }
    }
  }

  // An internal `define` has the same difficulty as a `letrec` group and needs
  // the same answer. Its name takes its value when the definition executes, so
  // a procedure created earlier in the same body that refers to it would
  // capture nothing -- which is how mutually recursive internal definitions are
  // written. Any such name a nested procedure refers to is therefore boxed.
  //
  // Names are unique after the analyzer's renaming, so a reference anywhere
  // inside a nested procedure is a reference to *this* binding and cannot be
  // something else of the same name.
  const nested = allNestedLambdas(ir);
  for (const name of internalDefineNames(ir)) {
    if (nested.some((lambda) => referencedBy(lambda, name))) boxed.add(name);
  }

  // Which group names an initializer may bind inside its own factory instead of
  // receiving as a parameter -- only ever its own, and only when it is not
  // boxed. A boxed name is shared through its box, so the factory has to be
  // handed that box rather than declare a separate variable of the same name.
  // Decided after boxing, because it depends on it.
  const selfNames = new Map();
  for (const group of groups) {
    for (let i = 0; i < group.names.length; i++) {
      const own = group.names[i];
      if (group.inits[i].k === 'lambda' && !boxed.has(own)) {
        selfNames.set(group.inits[i], [own]);
      }
    }
  }

  const free = new Map();
  const lift = new Set();
  collect(ir, free, lift, selfNames);
  return { free, lift, boxed, selfNames };
}

/**
 * Walks a lambda's body, recording the free variables of each nested lambda.
 *
 * @param {Object} lambda - A `lambda` IR node.
 * @param {Map<Object, Array<string>>} free - Filled in per nested lambda.
 * @param {Set<Object>} lift - Filled in with the nodes to lift.
 * @param {Map<Object, Array<string>>} selfNames - Group names a node may bind
 *   itself.
 * @returns {void}
 */
function collect(lambda, free, lift, selfNames) {
  for (const nested of nestedLambdas(lambda)) {
    const own = selfNames.get(nested) ?? [];
    const vars = [...freeVariables(nested)].filter((name) => !own.includes(name));
    free.set(nested, vars);
    lift.add(nested);
    collect(nested, free, lift, selfNames);
  }
}

/**
 * The lambda nodes directly inside a lambda's body -- not those nested deeper.
 * @param {Object} lambda - A `lambda` IR node.
 * @returns {Array<Object>} Its immediate nested lambdas, in emission order.
 */
function nestedLambdas(lambda) {
  const found = [];
  walk(lambda.body, (node) => {
    if (node.k !== 'lambda') return true;
    found.push(node);
    return false;   // do not descend: those belong to this one, not to us
  });
  return found;
}

/**
 * The locals a unit assigns with `set!` anywhere inside it.
 *
 * These must be boxed because a spilled frame copies each local's value, while
 * Scheme shares the binding: an assignment made after a continuation is
 * captured is visible when that continuation is invoked again, and to any
 * closure over the same variable. Copying is right for temporaries, which are
 * always written before they are read, and wrong for a variable the program can
 * name.
 *
 * @param {Object} ir - An IR node.
 * @returns {Set<string>} The assigned locals.
 */
function assignedLocals(ir) {
  const found = new Set();
  walk(ir, (node) => {
    if (node.k === 'set' && node.local) found.add(node.name);
    return true;
  });
  return found;
}

/**
 * Every lambda node in a subtree except the outermost one.
 * @param {Object} ir - The unit's top-level `lambda` IR node.
 * @returns {Array<Object>} The nested lambdas, at every depth.
 */
function allNestedLambdas(ir) {
  const found = [];
  walk(ir.body, (node) => {
    if (node.k === 'lambda') found.push(node);
    return true;
  });
  return found;
}

/**
 * The names bound by internal definitions anywhere in a subtree.
 * @param {Object} node - An IR node.
 * @returns {Array<string>} The defined names.
 */
function internalDefineNames(node) {
  const found = [];
  walk(node, (n) => {
    if (n.k === 'define') found.push(n.name);
    return true;
  });
  return found;
}

/**
 * Every `letrec` node in a subtree.
 * @param {Object} node - An IR node.
 * @returns {Array<Object>} The `letrec` nodes.
 */
function letrecGroups(node) {
  const found = [];
  walk(node, (n) => {
    if (n.k === 'letrec') found.push(n);
    return true;
  });
  return found;
}

/**
 * Whether a subtree reads a local by name.
 * @param {Object} node - An IR node.
 * @param {string} name - A renamed local.
 * @returns {boolean} True if it is read or assigned anywhere inside.
 */
function referencedBy(node, name) {
  let found = false;
  walk(node, (n) => {
    if (found) return false;
    if (n.k === 'local' && n.name === name) { found = true; return false; }
    if (n.k === 'set' && n.local && n.name === name) { found = true; return false; }
    return true;
  });
  return found;
}

/**
 * The locals a lambda reads without binding.
 *
 * Order is the order first seen, so a factory's parameter list is stable
 * between the two forms of its parent -- both walk the same IR the same way.
 *
 * @param {Object} lambda - A `lambda` IR node.
 * @returns {Set<string>} Its free variables.
 */
export function freeVariables(lambda) {
  const free = new Set();
  const bound = new Set(lambda.params);
  if (lambda.rest) bound.add(lambda.rest);
  scan(lambda.body, bound, free);
  return free;
}

/**
 * Accumulates free variables, tracking what is bound on the way down.
 *
 * @param {Object} node - An IR node.
 * @param {Set<string>} bound - Names bound by enclosing forms.
 * @param {Set<string>} free - Accumulator.
 * @returns {void}
 */
function scan(node, bound, free) {
  if (node === null || typeof node !== 'object') return;

  switch (node.k) {
    case 'local':
      if (!bound.has(node.name)) free.add(node.name);
      return;

    case 'set':
      if (node.local && !bound.has(node.name)) free.add(node.name);
      scan(node.value, bound, free);
      return;

    case 'lambda': {
      const inner = new Set(bound);
      for (const p of node.params) inner.add(p);
      if (node.rest) inner.add(node.rest);
      scan(node.body, inner, free);
      return;
    }

    case 'let': {
      scan(node.init, bound, free);
      const inner = new Set(bound);
      inner.add(node.name);
      scan(node.body, inner, free);
      return;
    }

    case 'letrec': {
      // Every name is in scope in every initializer, which is what makes the
      // group mutually recursive.
      const inner = new Set(bound);
      for (const name of node.names) inner.add(name);
      for (const init of node.inits) scan(init, inner, free);
      scan(node.body, inner, free);
      return;
    }

    case 'seq': {
      // Every internal definition in a body is in scope throughout it, not just
      // after its own definition -- that is what lets two internal procedures
      // refer to each other, and it is what `lowerBody` predeclares during
      // lowering. So all of the names are bound before any expression is
      // scanned.
      //
      // Binding them as the sequence was scanned instead reported a
      // self-recursive internal definition as *free* of the procedure
      // containing it, which put its name in the enclosing factory's parameter
      // list and left the caller passing a variable it never declared.
      const inner = new Set(bound);
      for (const expr of node.exprs) {
        if (expr.k === 'define') inner.add(expr.name);
      }
      for (const expr of node.exprs) scan(expr, inner, free);
      return;
    }

    case 'define':
      scan(node.value, bound, free);
      return;

    default:
      for (const key of Object.keys(node)) {
        const value = node[key];
        if (Array.isArray(value)) {
          for (const item of value) scan(item, bound, free);
        } else if (value !== null && typeof value === 'object') {
          scan(value, bound, free);
        }
      }
  }
}

/**
 * Visits IR nodes, letting the visitor stop a branch by returning false.
 * @param {*} node - An IR node, array, or leaf.
 * @param {function(Object): boolean} visit - Returns whether to descend.
 * @returns {void}
 */
function walk(node, visit) {
  if (node === null || typeof node !== 'object') return;
  if (Array.isArray(node)) {
    for (const item of node) walk(item, visit);
    return;
  }
  if (node.k !== undefined && !visit(node)) return;
  for (const key of Object.keys(node)) {
    if (key === 'k' || key === 'name' || key === 'names' || key === 'params') continue;
    walk(node[key], visit);
  }
}
