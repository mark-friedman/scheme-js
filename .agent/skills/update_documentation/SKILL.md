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

If the task involved R7RS compliance or roadmap items:
1.  Open `ROADMAP.md`.
2.  Mark relevant items as checked `[x]`.
3.  Add notes if necessary.

### 3. `directory_structure.md`

If you added, moved, or deleted files/directories:
1.  Open `directory_structure.md`.
2.  Update the tree structure to reflect the current state.
3.  Verify that descriptions are accurate.

### 4. `README.md`

Update if:
- New setup instructions are needed.
- Architecture overview has changed.
- New major features are available to the user.
