/**
 * @fileoverview Unit tests for BreakpointManager.
 *
 * Tests for setting, removing, and querying breakpoints with line and column
 * precision. Part of Phase 1: Debug Runtime Core.
 */

import { assert } from '../harness/helpers.js';
import { BreakpointManager } from '../../src/debug/breakpoint_manager.js';

/**
 * Runs all BreakpointManager tests.
 * @param {Object} logger - Test logger
 */
export function runBreakpointManagerTests(logger) {
    logger.title('BreakpointManager - Basic Operations');

    // Test: Set breakpoint returns unique ID
    {
        const mgr = new BreakpointManager();
        const id1 = mgr.setBreakpoint('test.scm', 10);
        const id2 = mgr.setBreakpoint('test.scm', 20);

        assert(logger, 'setBreakpoint returns string ID', typeof id1, 'string');
        assert(logger, 'setBreakpoint returns unique IDs', id1 !== id2, true);
    }

    // Test: Remove breakpoint by ID
    {
        const mgr = new BreakpointManager();
        const id = mgr.setBreakpoint('test.scm', 10);

        assert(logger, 'removeBreakpoint returns true for existing', mgr.removeBreakpoint(id), true);
        assert(logger, 'removeBreakpoint returns false after removal', mgr.removeBreakpoint(id), false);
    }

    // Test: hasBreakpoint returns true for matching line
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10);

        const source = { filename: 'test.scm', line: 10, column: 1 };
        assert(logger, 'hasBreakpoint true for matching line', mgr.hasBreakpoint(source), true);
    }

    // Test: hasBreakpoint returns false for non-matching line
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10);

        const source = { filename: 'test.scm', line: 20, column: 1 };
        assert(logger, 'hasBreakpoint false for non-matching line', mgr.hasBreakpoint(source), false);
    }

    // Test: Column-level precision when column specified
    logger.title('BreakpointManager - Column Precision');
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10, 5); // Column 5

        const sourceMatch = { filename: 'test.scm', line: 10, column: 5 };
        const sourceNoMatch = { filename: 'test.scm', line: 10, column: 10 };

        assert(logger, 'column breakpoint matches exact column', mgr.hasBreakpoint(sourceMatch), true);
        assert(logger, 'column breakpoint does not match different column', mgr.hasBreakpoint(sourceNoMatch), false);
    }

    // Test: Line-level fallback when no column
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10); // No column

        const source1 = { filename: 'test.scm', line: 10, column: 1 };
        const source2 = { filename: 'test.scm', line: 10, column: 50 };

        assert(logger, 'line breakpoint matches any column (1)', mgr.hasBreakpoint(source1), true);
        assert(logger, 'line breakpoint matches any column (50)', mgr.hasBreakpoint(source2), true);
    }

    // Test: Multiple breakpoints on same line (different columns)
    {
        const mgr = new BreakpointManager();
        const id1 = mgr.setBreakpoint('test.scm', 10, 5);
        const id2 = mgr.setBreakpoint('test.scm', 10, 15);

        assert(logger, 'can set multiple breakpoints same line', id1 !== id2, true);
        assert(logger, 'getAllBreakpoints count', mgr.getAllBreakpoints().length, 2);
    }

    // Test: Multiple breakpoints in same file (different lines)
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10);
        mgr.setBreakpoint('test.scm', 20);
        mgr.setBreakpoint('test.scm', 30);

        assert(logger, 'multiple breakpoints different lines', mgr.getAllBreakpoints().length, 3);

        const source10 = { filename: 'test.scm', line: 10, column: 1 };
        const source20 = { filename: 'test.scm', line: 20, column: 1 };
        const source30 = { filename: 'test.scm', line: 30, column: 1 };

        assert(logger, 'hasBreakpoint line 10', mgr.hasBreakpoint(source10), true);
        assert(logger, 'hasBreakpoint line 20', mgr.hasBreakpoint(source20), true);
        assert(logger, 'hasBreakpoint line 30', mgr.hasBreakpoint(source30), true);
    }

    // Test: Multiple breakpoints across different files
    logger.title('BreakpointManager - Multiple Files');
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('file1.scm', 10);
        mgr.setBreakpoint('file2.scm', 10);
        mgr.setBreakpoint('file3.scm', 10);

        const source1 = { filename: 'file1.scm', line: 10, column: 1 };
        const source2 = { filename: 'file2.scm', line: 10, column: 1 };
        const source3 = { filename: 'file3.scm', line: 10, column: 1 };
        const sourceOther = { filename: 'other.scm', line: 10, column: 1 };

        assert(logger, 'hasBreakpoint file1', mgr.hasBreakpoint(source1), true);
        assert(logger, 'hasBreakpoint file2', mgr.hasBreakpoint(source2), true);
        assert(logger, 'hasBreakpoint file3', mgr.hasBreakpoint(source3), true);
        assert(logger, 'hasBreakpoint other file false', mgr.hasBreakpoint(sourceOther), false);
    }

    // Test: getAllBreakpoints lists all active breakpoints
    logger.title('BreakpointManager - Query Operations');
    {
        const mgr = new BreakpointManager();
        const id1 = mgr.setBreakpoint('test.scm', 10);
        const id2 = mgr.setBreakpoint('test.scm', 20);

        const all = mgr.getAllBreakpoints();
        assert(logger, 'getAllBreakpoints returns array', Array.isArray(all), true);
        assert(logger, 'getAllBreakpoints length', all.length, 2);

        const ids = all.map(bp => bp.id);
        assert(logger, 'getAllBreakpoints contains id1', ids.includes(id1), true);
        assert(logger, 'getAllBreakpoints contains id2', ids.includes(id2), true);
    }

    // Test: Breakpoint on non-existent file (should still store)
    {
        const mgr = new BreakpointManager();
        const id = mgr.setBreakpoint('nonexistent.scm', 10);

        assert(logger, 'can set breakpoint on nonexistent file', typeof id, 'string');
        assert(logger, 'breakpoint stored for nonexistent file', mgr.getBreakpoint(id) !== null, true);
    }

    // Test: Remove non-existent breakpoint (graceful handling)
    {
        const mgr = new BreakpointManager();

        assert(logger, 'remove nonexistent breakpoint returns false', mgr.removeBreakpoint('nonexistent-id'), false);
    }

    // Test: hasBreakpoint handles null/undefined source
    logger.title('BreakpointManager - Edge Cases');
    {
        const mgr = new BreakpointManager();
        mgr.setBreakpoint('test.scm', 10);

        assert(logger, 'hasBreakpoint null source', mgr.hasBreakpoint(null), false);
        assert(logger, 'hasBreakpoint undefined source', mgr.hasBreakpoint(undefined), false);
    }

    // Test: Breakpoint IDs are stable after removal
    {
        const mgr = new BreakpointManager();
        const id1 = mgr.setBreakpoint('test.scm', 10);
        const id2 = mgr.setBreakpoint('test.scm', 20);
        mgr.removeBreakpoint(id1);
        const id3 = mgr.setBreakpoint('test.scm', 30);

        // id3 should be different from both id1 and id2
        assert(logger, 'new ID after removal is unique', id3 !== id1 && id3 !== id2, true);
    }
}

export default runBreakpointManagerTests;
