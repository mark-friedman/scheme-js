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
 *
 * Only the locals live at the resume point are spilled -- see `liveness.js`.
 * The restore still names every local, and one that was not saved comes back
 * `undefined`, which is safe exactly because nothing reads it before writing
 * it on any path from there.
 */

import { ProcedureEmitter, procedureScope, jsName } from './emitter.js';
import { liveIn } from './liveness.js';

/**
 * Stands in for the object literal that spills a frame's locals at the
 * suspension point that resumes at `block`.
 *
 * A call site needs that literal while it is being emitted, but what goes in
 * it is not known until the whole body has been: it is whatever is live at the
 * block the frame resumes at, and liveness depends on everything after that
 * point. So a token naming the block is emitted now and substituted at the end.
 *
 * @param {number} block - The resume block.
 * @returns {string} The placeholder.
 */
function framePlaceholder(block) {
  return `$FRAME${block}$`;
}

/**
 * Finds every frame placeholder, capturing its resume block. Global, because
 * it is used both to substitute placeholders and to find them for liveness.
 */
const FRAME_PLACEHOLDER = /\$FRAME(\d+)\$/g;

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
    /**
     * This procedure's suspension points, each with the block it resumes at.
     * Kept here rather than read back from the shared context, which holds
     * every procedure's resume points in the unit.
     * @type {Array<{node: Object, block: number}>}
     */
    this.resumeSites = [];
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

  /**
   * Assigns a parameter for the next iteration of a loop. The resumable form
   * never boxes its parameters on entry -- they arrive from a frame already
   * boxed -- so a boxed one is given a fresh box here.
   * @param {string} param - The renamed parameter.
   * @param {string} value - A JavaScript expression for its new value.
   * @returns {string} A statement.
   */
  loopAssign(param, value) {
    return this.isBoxed(param)
      ? `${jsName(param)} = [${value}];` : `${jsName(param)} = ${value};`;
  }

  /**
   * Jumps to block zero, which is the procedure's entry: it makes the boxes for
   * internal definitions and starts the body, as a fresh call would.
   * @returns {string} A statement.
   */
  loopJump() {
    return '$pc = 0; continue;';
  }

  /**
   * Opens an inline loop: its head is a block of its own, and the back edge is
   * a jump to it. Block zero is the procedure's entry, which a loop inside the
   * procedure must not re-run.
   * @param {Object} lambda - The loop's `lambda` IR node.
   * @returns {Object} The loop target; see `loopTarget`.
   */
  enterInlineLoop(lambda) {
    const head = this.newBlock();
    this.goto(head);
    this.switchTo(head);
    return {
      params: lambda.params,
      fixedArity: true,
      procedure: false,
      assign: (param, value) => `${jsName(param)} = ${value};`,
      jump: `$pc = ${head}; continue;`
    };
  }

  /**
   * Closes an inline loop. Its blocks end in returns and jumps already.
   * @returns {void}
   */
  exitInlineLoop() {}

  /**
   * The fast form's identifier. The two forms are declared side by side, and a
   * global self-call has to compare against the procedure the global holds,
   * which is the fast form.
   * @returns {string} An identifier.
   */
  procedureName() {
    return this.name.replace(/\$r$/, '');
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
    this.resumeSites.push({ node, block: resume });
    // Two statements rather than one, so the block visibly ends in `return`.
    // The liveness analysis only trusts a block to end where it can see that
    // it does, and assumes it falls through otherwise.
    this.out.push(`R.reify(${this.name}, ${resume}, ${framePlaceholder(resume)});`);
    this.out.push('return R.UNWIND;');

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
    this.resumeSites.push({ node, block: resume });
    // A capture below this call: spill and report outward, so that every frame
    // between the capture and the interpreter reifies itself on the way out.
    this.out.push(
      `if (${result} === R.UNWIND) { R.reify(${this.name}, ${resume}, ${framePlaceholder(resume)});`
      + ` return R.UNWIND; }`);
    this.out.push(`$r = ${result};`);
    this.goto(resume);

    this.switchTo(resume);
    this.out.push(`${result} = $r;`);
    return result;
  }

  /**
   * Decides which slots each resume block needs saved.
   *
   * Falls back to saving everything if the body contains a function literal.
   * Every nested procedure is lifted into a factory that receives its free
   * variables as arguments, so creating one is an ordinary, visible read of
   * them. A function literal written inline would instead close over this
   * procedure's variables and read them whenever it is *called* -- after a
   * resume, perhaps, with nothing in the text to show it. That does not happen
   * today, and if it ever does the right response is the old behaviour, not a
   * wrong answer.
   *
   * @param {Array<string>} slots - Every name the procedure can hold, in
   *   declaration order.
   * @returns {Map<number, Array<string>>} For each resume block, the slots to
   *   save, in declaration order so that generated code is reproducible.
   */
  liveSlots(slots) {
    const result = new Map();
    const inline = this.blocks.some((stmts) => stmts.some((l) => /\bfunction\b/.test(l)));
    if (inline) {
      for (const { block } of this.resumeSites) result.set(block, slots);
      return result;
    }

    const live = liveIn(this.blocks, new Set(slots), { spill: FRAME_PLACEHOLDER });
    for (const { block } of this.resumeSites) {
      result.set(block, slots.filter((name) => live[block].has(name)));
    }
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

    // Every name this procedure can hold. `$r` is not among them: it holds the
    // value a suspended call is still waiting for, which is supplied when the
    // continuation is reinstated rather than saved when it is captured.
    const slots = [...new Set([
      ...this.declared, ...this.ir.params.map(jsName), ...(rest ? [jsName(rest)] : [])
    ])];

    // What each suspension point saves: the locals live at the block it resumes
    // at, and no others. Saving every local at every point was quadratic in the
    // size of the procedure -- the largest in the benchmark corpus generated
    // 3.18 MB, 94% of it frame literals.
    //
    // Restoring is unchanged and still names every slot. A local that was not
    // saved destructures to `undefined`, and that is safe precisely because it
    // is dead there: every path from the resume block assigns it before
    // reading it.
    const live = this.liveSlots(slots);
    for (const { node, block } of this.resumeSites) {
      // The fast form spills into the frame this one restores from, so at each
      // call site it has to save exactly what this form expects there.
      this.ctx.frameSlots.set(node, live.get(block));
    }

    const names = [...slots, '$r'];
    const spill = (_, block) => `{ ${live.get(Number(block)).join(', ')} }`;

    const cases = this.blocks
      .map((stmts, i) =>
        `      case ${i}:\n`
        + stmts.map((l) => '        ' + l.replace(FRAME_PLACEHOLDER, spill)).join('\n'))
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
