/**
 * Convention A — explicit frame stack.
 *
 * Continuation frames live in a JavaScript array and every transfer of control
 * goes through a trampoline, so the JavaScript stack stays one frame deep no
 * matter how deep the Scheme recursion is. This is the Gambit-JS model
 * (Thivierge & Feeley, SFP 2012).
 *
 * The consequence that matters for the bake-off is that a procedure must be
 * re-enterable after every *non-tail* call, not merely after calls that might
 * capture a continuation: the callee returns by way of the trampoline, so the
 * caller has to be resumable at that point regardless. Each procedure is
 * therefore compiled to a state machine over its call sites, with live
 * variables spilled to a frame object.
 *
 * That is the structural difference from convention B, which pays for
 * re-entrancy only where a capture is actually possible, and it is why the
 * generated code here is bulkier.
 */

import { literal, quoted } from './backend_b.js';

/**
 * Emits JavaScript for a whole program under convention A.
 * @param {Array<Object>} nodes - Top-level nodes from the front end.
 * @returns {string} JavaScript source.
 */
export function emitProgram(nodes) {
  const ctx = { temps: 0, blocks: [] };
  const out = [];
  for (const node of nodes) {
    if (node.t === 'define' && node.value.t === 'lambda') {
      out.push(emitProcedure(node.js, node.value, ctx));
    } else if (node.t === 'define') {
      out.push(`let ${node.js} = ${simpleValue(node.value, ctx)};`);
    } else {
      out.push(`RESULT = STK.run(${simpleValue(node, ctx)});`);
    }
  }
  return out.join('\n\n');
}

/**
 * Emits a procedure as a state machine over its call sites.
 *
 * `$pc` selects the block to resume at and `$f` carries the frame holding
 * spilled locals. Entry is `$pc === 0` with arguments in the parameter list;
 * every other entry restores from the frame.
 *
 * @param {string} name - Generated identifier.
 * @param {Object} lambda - The lambda node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
function emitProcedure(name, lambda, ctx, mode = 'A') {
  const emitter = new BlockEmitter(name, lambda, ctx);
  emitter.mode = mode;
  return emitter.emit();
}

/**
 * Exposes the block emitter so convention B can reuse the re-entry machinery.
 *
 * Both conventions need a procedure that can be resumed at a call site: A needs
 * it because control leaves through the trampoline on every non-tail call, and
 * B needs it because a continuation capture unwinds the JavaScript stack. The
 * difference is *which* call sites need it and what the normal path costs, not
 * whether the machinery exists at all -- which is itself one of the findings.
 */
export { BlockEmitter };

/**
 * Emits a value that needs no call sites, for top-level non-procedure forms.
 * @param {Object} node - The node.
 * @param {Object} ctx - Emitter state.
 * @returns {string} JavaScript source.
 */
function simpleValue(node, ctx) {
  switch (node.t) {
    case 'const': return literal(node.value);
    case 'quote': return quoted(node.value);
    case 'ref': return node.js;
    case 'prim': return `RT.primitives[${JSON.stringify(node.name)}]`;
    default: throw new Error(`stage2a backend A: top-level ${node.t} not supported`);
  }
}

/**
 * Compiles one procedure into numbered basic blocks.
 */
class BlockEmitter {
  /**
   * @param {string} name - Generated identifier for the procedure.
   * @param {Object} lambda - The lambda node.
   * @param {Object} ctx - Shared emitter state.
   */
  constructor(name, lambda, ctx) {
    // Anonymous procedures still need a name, because a non-tail call records
    // the procedure to resume into. A *named function expression* binds its own
    // name inside its body, which gives the self-reference without leaking the
    // name into the enclosing scope.
    this.isExpression = name === '';
    this.name = this.isExpression ? `proc${ctx.temps++}` : name;
    this.lambda = lambda;
    this.ctx = ctx;
    this.blocks = [];
    this.live = new Set(lambda.params);
    if (lambda.rest) this.live.add(lambda.rest);
    this.nested = [];
    /** 'A' emits trampoline transfers; 'resume' emits direct calls with unwind checks. */
    this.mode = 'A';
    /** Maps a front-end call id to the block that resumes after it. */
    this.resumeBlocks = new Map();
  }

  /** @returns {string} A fresh temporary name. */
  temp() {
    return `v${this.ctx.temps++}`;
  }

  /**
   * Emits a nested procedure, assigned to `name`.
   *
   * In resume mode this delegates to convention B's pair emitter rather than
   * emitting another resumable function. A closure is a *value*: it escapes the
   * procedure that built it and is called by whoever receives it, so it must
   * have the ordinary calling convention no matter which half of a procedure
   * happened to construct it. Emitting a resumable function here instead gave a
   * closure the `($pc, $f, ...)` signature, and `call/cc`'s receiver was then
   * called with its continuation landing in `$pc`.
   *
   * @param {string} name - Variable to assign the closure to.
   * @param {Object} lambda - The lambda node.
   * @returns {string} JavaScript source.
   */
  nestedProcedure(name, lambda) {
    if (this.mode === 'resume' && this.ctx.emitNestedPair) {
      return this.ctx.emitNestedPair(name, lambda, this.ctx);
    }
    return `${name} = ${emitProcedure('', lambda, this.ctx, this.mode)};`;
  }

  /**
   * Starts a new basic block and returns its index.
   * @returns {number} The block index.
   */
  newBlock() {
    this.blocks.push([]);
    return this.blocks.length - 1;
  }

  /**
   * Emits the whole procedure.
   * @returns {string} JavaScript source.
   */
  emit() {
    const entry = this.newBlock();
    this.current = entry;
    this.tailReturn(this.lambda.body);

    const params = this.lambda.params.join(', ');
    let restSetup = this.lambda.rest
      ? `    ${this.lambda.rest} = RT.primitives['list'](...$rest);\n`
      : '';
    for (const p of (this.lambda.boxedParams || [])) restSetup += `    ${p} = { v: ${p} };\n`;
    const decls = [...this.live].filter(
      (n) => !this.lambda.params.includes(n) && n !== this.lambda.rest);

    const cases = this.blocks.map((stmts, i) =>
      `      case ${i}: {\n${stmts.map((s) => '        ' + s).join('\n')}\n      }`).join('\n');

    const restParam = this.lambda.rest ? ', ...$rest' : '';
    const header = `${this.isExpression ? 'Object.assign(' : ''}function ${this.name}` +
      `($pc, $f${params ? ', ' + params : ''}${restParam}) {`;
    return [
      ...this.nested,
      header,
      `  let ${['$r', ...decls].join(', ')};`,
      `  if ($pc !== 0) { ({ ${[...this.live, '$r'].join(', ')} } = $f); }`,
      restSetup ? `  else {\n${restSetup}  }` : '',
      `  for (;;) {`,
      `    switch ($pc) {`,
      cases,
      `    }`,
      `  }`,
      this.isExpression
        ? `}, { $arity: ${this.lambda.params.length} })`
        : `}\n${this.name}.$arity = ${this.lambda.params.length};`
    ].filter(Boolean).join('\n');
  }

  /**
   * Appends a statement to the current block.
   * @param {string} stmt - JavaScript statement source.
   * @returns {void}
   */
  push(stmt) {
    this.blocks[this.current].push(stmt);
  }

  /**
   * Captures the live set into a frame literal.
   * @returns {string} JavaScript object literal source.
   */
  frameLiteral() {
    return `{ ${[...this.live, '$r'].join(', ')} }`;
  }

  /**
   * Emits a body sequence in tail position.
   * @param {Array<Object>} body - Body expressions.
   * @returns {void}
   */
  tailReturn(body) {
    for (let i = 0; i < body.length - 1; i++) this.value(body[i]);
    const last = body[body.length - 1];
    if (!last) { this.push('return STK.ret(undefined);'); return; }
    this.tailExpr(last);
  }

  /**
   * Emits one expression in tail position.
   * @param {Object} node - The expression.
   * @returns {void}
   */
  tailExpr(node) {
    if (node.t === 'if') {
      const test = this.value(node.test);
      const thenBlock = this.newBlock();
      const elseBlock = this.newBlock();
      this.push(`if (RT.truthy(${test})) { $pc = ${thenBlock}; continue; } else { $pc = ${elseBlock}; continue; }`);
      this.current = thenBlock;
      this.tailExpr(node.then);
      this.current = elseBlock;
      this.tailExpr(node.else);
      return;
    }
    if (node.t === 'begin') { this.tailReturn(node.body); return; }
    if (node.t === 'call') {
      const fn = this.value(node.fn);
      const args = node.args.map((a) => this.value(a));
      if (node.fn.t === 'prim' && !node.fn.control) {
        this.push(this.mode === 'resume'
          ? `return ${fn}(${args.join(', ')});`
          : `return STK.ret(${fn}(${args.join(', ')}));`);
        return;
      }
      if (this.mode === 'resume') {
        this.push(`RT.tail.fn = ${fn}; RT.tail.args = [${args.join(', ')}]; return RT.TAIL_CALL;`);
        return;
      }
      // A tail call replaces the current activation: no frame is pushed.
      this.push(`return STK.jump(${fn}, [${args.join(', ')}]);`);
      return;
    }
    if (node.t === 'let' || node.t === 'letSeq') {
      const boxedHere = new Set(node.boxedNames || []);
      node.names.forEach((n, i) => {
        this.live.add(n);
        const init = this.value(node.inits[i]);
        this.push(`${n} = ${boxedHere.has(n) ? `{ v: ${init} }` : init};`);
      });
      this.tailReturn(node.body);
      return;
    }
    if (node.t === 'labels') {
      this.live.add(node.js);
      this.push(this.nestedProcedure(node.js, node.lambda));
      this.tailExpr(node.call);
      return;
    }
    const value = this.value(node);
    this.push(this.mode === 'resume' ? `return ${value};` : `return STK.ret(${value});`);
  }

  /**
   * Emits one expression in value position, returning a JavaScript expression.
   * @param {Object} node - The expression.
   * @returns {string} JavaScript source for the value.
   */
  value(node) {
    switch (node.t) {
      case 'const': return literal(node.value);
      case 'quote': return quoted(node.value);
      case 'ref': return node.js;
      case 'boxref': return `${node.js}.v`;
      case 'boxset': {
        const value = this.value(node.value);
        this.push(`${node.js}.v = ${value};`);
        return 'undefined';
      }
      case 'prim':
        if (node.control) return this.mode === 'resume' ? 'RT.callcc' : 'STK.CALLCC';
        return `RT.primitives[${JSON.stringify(node.name)}]`;
      case 'lambda': {
        // Emitted inside the current block, assigned to a spilled local, so the
        // generated function closes over this activation's variables. Hoisting
        // it to the top level would break every closure with a free variable --
        // a real compiler would do closure conversion instead, which is Stage 2b
        // work and not something the convention choice depends on.
        const name = node.tmp ?? this.temp();
        this.live.add(name);
        this.push(this.nestedProcedure(name, node));
        return name;
      }
      case 'set': {
        const value = this.value(node.value);
        this.push(`${node.js} = ${value};`);
        return 'undefined';
      }
      case 'begin': {
        let last = 'undefined';
        for (const b of node.body) last = this.value(b);
        return last;
      }
      case 'if': {
        const test = this.value(node.test);
        const result = node.tmp ?? this.temp();
        this.live.add(result);
        const thenBlock = this.newBlock();
        const elseBlock = this.newBlock();
        const join = this.newBlock();
        this.push(`if (RT.truthy(${test})) { $pc = ${thenBlock}; continue; } else { $pc = ${elseBlock}; continue; }`);
        this.current = thenBlock;
        const thenValue = this.value(node.then);
        this.push(`${result} = ${thenValue}; $pc = ${join}; continue;`);
        this.current = elseBlock;
        const elseValue = this.value(node.else);
        this.push(`${result} = ${elseValue}; $pc = ${join}; continue;`);
        this.current = join;
        return result;
      }
      case 'let': case 'letSeq': {
        const boxedHere = new Set(node.boxedNames || []);
        node.names.forEach((n, i) => {
          this.live.add(n);
          const init = this.value(node.inits[i]);
          this.push(`${n} = ${boxedHere.has(n) ? `{ v: ${init} }` : init};`);
        });
        let last = 'undefined';
        for (const b of node.body) last = this.value(b);
        return last;
      }
      case 'labels': {
        this.live.add(node.js);
        this.push(this.nestedProcedure(node.js, node.lambda));
        return this.value(node.call);
      }
      case 'call': {
        const fn = this.value(node.fn);
        const args = node.args.map((a) => this.value(a));
        const result = node.tmp ?? this.temp();
        this.live.add(result);
        // A known primitive needs no continuation frame: it cannot capture and
        // it cannot tail-call, so it is a plain JavaScript call here too. Both
        // backends get this, so it does not tilt the comparison.
        if (node.fn.t === 'prim' && !node.fn.control) {
          this.push(`${result} = ${fn}(${args.join(', ')});`);
          return result;
        }
        const resume = this.newBlock();
        if (node.callId !== undefined) {
          this.resumeBlocks.set(node.callId, resume);
          if (this.ctx.frameVars) {
            this.ctx.frameVars.set(node.callId, { vars: [...this.live], pc: resume });
          }
        }

        if (this.mode === 'resume') {
          // Convention B's resumable twin: an ordinary call, then the same
          // unwind check the fast path carries. Only on unwind is a frame built.
          this.push(`${result} = ${fn}(${args.join(', ')});`);
          this.push(`while (${result} === RT.TAIL_CALL) { const f = RT.tail.fn, a = RT.tail.args; ${result} = f(...a); }`);
          this.push(`if (${result} === RT.UNWIND) { $f = ${this.frameLiteral()}; RT.unwinding.frames.push({ proc: ${this.name}, pc: ${resume}, frame: $f }); return RT.UNWIND; }`);
          this.push(`$pc = ${resume}; continue;`);
          this.current = resume;
          this.push(`${result} = $r;`);
          return result;
        }

        // A non-tail call pushes a continuation frame naming the block to
        // resume at, then hands control to the trampoline. Every non-tail call
        // costs this, whether or not a continuation could be captured in it --
        // which is the structural difference from convention B.
        this.push(`$f = ${this.frameLiteral()};`);
        this.push(`return STK.call(${fn}, [${args.join(', ')}], ${this.name}, ${resume}, $f);`);
        this.current = resume;
        this.push(`${result} = $r;`);
        return result;
      }
      default:
        throw new Error(`stage2a backend A: cannot emit ${node.t}`);
    }
  }
}
