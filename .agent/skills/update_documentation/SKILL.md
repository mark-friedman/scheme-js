---
name: Update Documentation
description: A guide for updating project documentation (CHANGES.md, Roadmap, etc.).
---

# Update Documentation

This skill guides you through maintaining the project documentation. This is critical for tracking progress and history.

## Files to Update

### 1. `CHANGES.md`

Every time you complete a significant task (a "Walkthrough"), append an entry to `CHANGES.md`.

**Format**:
```markdown
# Walkthrough: [Title of Task] [Date]

[Brief summary of what was accomplished]

## Changes

### 1. [Category/Component]
[Description of change]
[Link to file](./relative/path/to/file)

### 2. ...

## Verification Results

### Automated Tests
[Describe test results]
```
> [!TIP]
> Copy the content from your session's `walkthrough.md` artifact if available.

### 2. `ROADMAP.md`

Forward-looking and high-level: the project's constraints, planned user-visible goals, and a short
table of what has been delivered. It does **not** track individual tasks.

Update it when a user-visible goal is planned or met. When one lands, simplify its entry into a line
in the delivered table rather than leaving a detailed plan behind. Do not add per-task status here —
that is `docs/compiler_plan.md`.

### 3. `docs/compiler_plan.md`

The only place that ranks compiler work: one numbered list with status, dependencies, and the
finding that justifies each item's position.

1.  **Read it before starting a task.**
2.  When you finish one, tick it, move it to `Completed` with a one-line outcome, append the same
    row to `docs/compiler_plan_completed.md`, and renumber the live tasks.
3.  Add whatever the work revealed — including anything that should change the order, with the
    reason, so it survives a context compaction.
4.  `Completed` keeps the fifteen most recent rows: when a sixteenth arrives, drop the oldest from
    the plan. It is already in `docs/compiler_plan_completed.md`, which keeps every row under its
    original number and is append-only.

### 4. `docs/compiler_findings.md`

**Append-only.** Add an entry only when a measurement contradicted something we believed, a gate was
missed, or a technique was used that the plan did not anticipate. An entry that records no
falsification belongs in `CHANGES.md` instead.

Never edit an earlier entry away, including one that later proved wrong. Annotate the superseded
passage with a callout pointing at the newer entry.

### 5. `docs/compiler_design.md`

How the compiler works and why. Rewritable — keep it current rather than appending to it.

It carries only the reasoning **no single module can own**: a decision spanning the emitter, the
resumable form, the interpreter's frames and the runtime belongs here; a decision living inside one
module belongs in that module's header, which is fresher because it is edited with the code.
Duplicating a header into this document only rots the copy.

### 6. `docs/architecture.md`

If you added, moved, or deleted files/directories:
1.  Open `docs/architecture.md`.
2.  Update the tree structure to reflect the current state.
3.  Verify that descriptions are accurate.

### 7. `docs/README.md`

Add a one-line summary whenever you add a document under `docs/`, so the directory has an index.

### 8. `README.md`

Update if:
- New setup instructions are needed.
- Architecture overview has changed.
- New major features are available to the user.

## Two rules that hold the set together

**Lifetime decides the destination.** `ROADMAP.md`, `docs/architecture.md` and
`docs/compiler_design.md` are rewritable; `docs/compiler_plan.md` is living; `CHANGES.md`,
`docs/compiler_findings.md` and `docs/compiler_plan_completed.md` are append-only; `docs/archive/` is frozen and not maintained. Current
state must never live in an append-only file — that is how a list of three "known-broken things"
stayed in the documentation for months after all three were fixed.

**Links run one way.** Living documents point at append-only ones, never the reverse. A back-link
out of `CHANGES.md` or the findings log would have to be edited every time priorities move, which is
the same as letting it go stale.
