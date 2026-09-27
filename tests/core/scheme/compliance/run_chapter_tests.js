/**
 * R7RS chapter conformance tests, from the command line.
 *
 * Run with: node tests/core/scheme/compliance/run_chapter_tests.js [--compiled] [file filter...]
 */

import { runFromCommandLine } from './compliance_cli.js';

runFromCommandLine('chapters');
