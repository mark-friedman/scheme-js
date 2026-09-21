/**
 * @fileoverview The resumable form of a compiled procedure.
 *
 * ## Why a second copy exists
 *
 * A compiled procedure runs as straight-line JavaScript: ordinary calls, one
 * check after each. That is what makes it fast, and it is also what makes it
 * impossible to re-enter half-way through. A JavaScript function cannot be
 * resumed at a statement in the middle of its body.
 *
 * Scheme needs exactly that. When a continuation is captured while a compiled
 * procedure is on the stack, and later invoked, execution must resume just
 * after the call the procedure was making. So each procedure is emitted twice:
 * once in the fast form, and once as a state machine over its own call sites.
 * The state machine is used only while a continuation is being reinstated, so
 * it can be as slow as it likes; the cost is code size, paid once at compile
 * time, rather than speed, which would be paid on every call.
 *
 * The arrangement is Marshall's, in the lineage of Pettyjohn et al.'s
 * generalized stack inspection: the pending computation of a frame lives in a
 * separate resumable entry point rather than in the procedure itself, and a
 * capture is signalled by *returning* a distinguished value rather than by
 * throwing. On a high-level virtual machine a throw costs orders of magnitude
 * more than a return, which is what made the original technique perform badly.
 *
 * ## Shape of the generated code
 *
 *     function name$r($pc, $f) {
 *       let a, b, $t1, $t2, ..., $r;
 *       ({ a, b, $t1, $t2, $r } = $f);
 *       for (;;) switch ($pc) {
 *         case 0: ...            // ordinary entry
 *         case 1: $t3 = $r; ...  // resumed just after call site 1
 *       }
 *     }
 *
 * Every branch and every call site ends a block, so any point the procedure can
 * be suspended at is the start of a block and therefore reachable by `$pc`.
 * That is the part the fast form cannot do: it emits `if` as a nested
 * JavaScript `if`, and a statement inside one of those cannot be jumped to.
 *
 * Locals are spilled into `$f` on the way out and restored from it on the way
 * in. They are *copied* rather than shared, because a continuation may be
 * invoked more than once and the second invocation must not see state left by
 * the first.
 */

import { ProcedureEmitter, procedureScope, jsName } from './emitter.js';

/**
 * Stands in for the object literal that spills a frame's locals.
 *
 * A call site needs that literal while it is being emitted, but which names
 * there are is not settled until the whole body has been. Emitting this token
 * and substituting once at the end avoids the alternative, where each call site
 * spills only the names declared before it -- which would leave a procedure's
 * two forms spilling different sets, and the fast form is obliged to spill
 * exactly what this one restores.
 */
const FRAME = '$FRAME$';

/**
 * Emits the resumable twin of a procedure.
 *
 * Subclasses the fast-path emitter rather than duplicating it: everything about
 * *expressions* -- inlined primitives, global access, temporaries, rest
 * arguments -- is identical between the two forms and is inherited. Only
 * control flow differs, so only control flow is overridden. That matters for
 * more than brevity: the two copies must agree on semantics, and the surest way
 * to make them agree is for most of the code to be literally the same code.
 */
export class TwinEmitter extends ProcedureEmitter {
  /**
   * @param {string} name - Identifier for the twin function.
   * @param {Object} ir - The same `lambda` IR node the fast form was built from.
   * @param {Object} ctx - Shared emitter context.
   * @param {{counter: {n: number}, path: string}} [scope] - Where this
   *   procedure's generated names come from. It is the same scope the fast form
   *   was given, which is what makes the two agree on every name.
   */
  constructor(name, ir, ctx, scope) {
    super(name, ir, ctx, scope);
    /** @type {Array<Array<string>>} Statements per basic block. */
    this.blocks = [[]];
    this.current = 0;
    // The inherited code appends to `this.out`; pointing it at a block is what
    // makes all of that code work unchanged in a block-structured emitter.
    this.out = this.blocks[0];
  }

  /**
   * Starts a new basic block.
   * @returns {number} Its index, usable as a `$pc` value.
   */
  newBlock() {
    this.blocks.push([]);
    return this.blocks.length - 1;
  }

  /**
   * Directs subsequent statements into a block.
   * @param {number} id - Block index.
   * @returns {void}
   */
  switchTo(id) {
    this.current = id;
    this.out = this.blocks[id];
  }

  /**
   * Emits a jump to a block.
   * @param {number} id - Block index.
   * @returns {void}
   */
  goto(id) {
    this.out.push(`$pc = ${id}; continue;`);
  }

  /**
   * Emits an `if` whose branches end the procedure.
   * @param {Object} node - An `if` IR node in tail position.
   * @returns {void}
   */
  emitTailIf(node) {
    const test = this.value(node.test);
    const thenBlock = this.newBlock();
    const elseBlock = this.newBlock();
    this.out.push(
      `if (${test} !== false) { $pc = ${thenBlock}; continue; } $pc = ${elseBlock}; continue;`);
    this.switchTo(thenBlock);
    this.statement(node.then);
    this.switchTo(elseBlock);
    this.statement(node.else);
  }

  /**
   * Emits a nested procedure as an *assignment* rather than a declaration.
   *
   * A function declaration inside a `switch` case only takes effect when that
   * case runs. Resuming jumps straight to a later block, so a declaration
   * earlier in the body would never have executed and the name would be
   * undefined. Assigning to a declared variable instead means the value is
   * spilled into the frame with everything else and restored on the way back
   * in.
   *
   * @param {string} name - Identifier for the nested procedure.
   * @param {Object} node - A `lambda` IR node.
   * @returns {void}
   */
  emitLambdaPair(name, node, path) {
    const twinName = `${name}$r`;
    // Generated before the fast form for the same reason the outermost pair is:
    // the fast form's suspension code refers to resume points that only exist
    // once this has run.
    const twinSource =
      new TwinEmitter(twinName, node, this.ctx, procedureScope(path)).emit();

    this.declared.add(name);
    this.out.push(
      `${name} = ${new ProcedureEmitter(name, node, this.ctx, procedureScope(path)).emit()};`);
    this.out.push(`R.markProcedure(${name}, ${JSON.stringify(node.name ?? 'anonymous')});`);
    this.declared.add(twinName);
    this.out.push(`${twinName} = ${twinSource};`);
    this.out.push(`${name}.$resume = ${twinName};`);
  }

  /** @inheritdoc */
  statement(node) {
    // Only a *tail* `if` ends the procedure in its branches. One whose value is
    // discarded rather than returned is emitted through `value`, exactly as the
    // fast form emits it -- and it has to be, because the two forms allocate
    // their temporaries in lockstep and the value form allocates one for the
    // result. Treating every `if` as a tail `if` here left the two disagreeing
    // about which temporary held what, so a frame spilled by one form was
    // restored wrongly by the other.
    if (node.k === 'if' && node.tail) { this.emitTailIf(node); return; }
    super.statement(node);
  }

  /** @inheritdoc */
  value(node) {
    if (node.k === 'if') return this.emitValueIf(node);
    if (node.k === 'capture') return this.emitCapture(node);
    if (node.k === 'call') {
      const inlined = this.tryInline(node);
      if (inlined !== null) return inlined;
      // Every call reached in value position is one whose value is wanted here,
      // so it is made and waited for even when the IR marks it as being in tail
      // position. A tail call reached as a *statement* is handled by the
      // inherited code, which returns a `TailCall` and so leaves this
      // procedure -- which is what makes tail recursion run in constant space.
      return this.emitResumableCall(node);
    }
    return super.value(node);
  }

  /**
   * Emits an `if` in value position, joining the branches at a block.
   * @param {Object} node - An `if` IR node.
   * @returns {string} The expression holding its value.
   */
  emitValueIf(node) {
    const test = this.value(node.test);
    const result = this.temp();
    const thenBlock = this.newBlock();
    const elseBlock = this.newBlock();
    const join = this.newBlock();

    this.out.push(
      `if (${test} !== false) { $pc = ${thenBlock}; continue; } $pc = ${elseBlock}; continue;`);

    this.switchTo(thenBlock);
    this.out.push(`${result} = ${this.value(node.then)};`);
    this.goto(join);

    this.switchTo(elseBlock);
    this.out.push(`${result} = ${this.value(node.else)};`);
    this.goto(join);

    this.switchTo(join);
    return result;
  }

  /**
   * Emits a continuation capture and the block that resumes at it.
   *
   * The shape is a call site's, without the call: this procedure always
   * suspends here, so there is nothing to test. What comes back is the value
   * the continuation was invoked with, which arrives in `$r` exactly as a
   * call's result does -- so everything after the capture is emitted once, in
   * the resume block, and serves every invocation of the continuation.
   *
   * @param {Object} node - A `capture` IR node.
   * @returns {string} The expression holding the captured value.
   */
  emitCapture(node) {
    const receiver = this.value(node.receiver);
    const result = this.temp();

    this.out.push(`R.capture(${receiver});`);
    const resume = this.newBlock();
    this.ctx.resumePoints.set(node, resume);
    this.out.push(`R.reify(${this.name}, ${resume}, ${FRAME}); return R.UNWIND;`);

    this.switchTo(resume);
    this.out.push(`${result} = $r;`);
    return result;
  }

  /**
   * Emits a non-tail call and the block that resumes after it.
   *
   * This is the whole point of the twin. The call's result arrives either
   * normally, or -- when the continuation is being reinstated -- in `$r`,
   * placed there by the frame that was suspended here.
   *
   * @param {Object} node - A `call` IR node.
   * @returns {string} The expression holding the call's value.
   */
  emitResumableCall(node) {
    const args = node.args.map((a) => this.value(a));
    const callee = this.temp();
    const raw = this.temp();
    const result = this.temp();

    this.out.push(`${callee} = ${this.value(node.fn)};`);
    this.out.push(`${raw} = ${callee}[R.SCHEME_RAW_CALL];`);
    this.out.push(
      `${result} = ${raw} === undefined ? ${callee}(${args.join(', ')})`
      + ` : ${raw}(${args.join(', ')});`);
    this.out.push(
      `while (${result} instanceof R.TailCall) { ${result} = R.step(${result}); }`);

    const resume = this.newBlock();
    // Recorded so that the fast form of this same procedure, which splits
    // nowhere and so has no blocks of its own, can name the point to come back
    // to when it has to suspend itself at this call.
    this.ctx.resumePoints.set(node, resume);
    // A capture below this call: spill and report outward, so that every frame
    // between the capture and the interpreter reifies itself on the way out.
    this.out.push(
      `if (${result} === R.UNWIND) { R.reify(${this.name}, ${resume}, ${FRAME});`
      + ` return R.UNWIND; }`);
    this.out.push(`$r = ${result};`);
    this.goto(resume);

    this.switchTo(resume);
    this.out.push(`${result} = $r;`);
    return result;
  }

  /** @inheritdoc */
  emit() {
    // Unlike the fast form, the twin takes no argument list: everything,
    // including a rest parameter, arrives already in `$f`. A rest parameter is
    // therefore already the Scheme list the body expects, and must not be
    // rebuilt from a JavaScript argument array that does not exist here.
    const rest = this.ir.rest;

    // In block zero, so a fresh entry creates the boxes and a resume -- which
    // always enters at a later block -- takes them from the frame instead.
    this.emitDefineBoxes();
    this.statement(this.ir.body);

    // Every declared name is spilled, not just the ones a liveness analysis
    // would keep. Suspension happens only while a continuation is being
    // captured or reinstated, so the waste is never on a path that matters, and
    // being conservative removes a whole class of bug where a variable is
    // needed after a resume and was not saved.
    const slots = [...new Set([
      ...this.declared, ...this.ir.params.map(jsName), ...(rest ? [jsName(rest)] : [])
    ])];

    // The fast form of this procedure spills into the frame this one restores
    // from, so it has to use exactly these names. `$r` is not among them: it
    // holds the value a suspended call is still waiting for, which exists only
    // once the continuation is being reinstated.
    this.ctx.frameSlots.set(this.ir, slots);

    const names = [...slots, '$r'];
    const literal = `{ ${names.join(', ')} }`;

    const cases = this.blocks
      .map((stmts, i) =>
        `      case ${i}:\n`
        + stmts.map((l) => '        ' + l.split(FRAME).join(literal)).join('\n'))
      .join('\n');

    return `function ${this.name}($pc, $f) {\n`
      + `  let ${names.join(', ')};\n`
      + `  ({ ${names.join(', ')} } = $f);\n`
      + `  for (;;) switch ($pc) {\n${cases}\n`
      + `      default: throw new Error('${this.name}: bad resume point ' + $pc);\n`
      + `  }\n`
      + `}`;
  }
}
