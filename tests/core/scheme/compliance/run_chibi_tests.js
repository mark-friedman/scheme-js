/**
 * Chibi's R7RS conformance tests, from the command line.
 *
 * Run with: node tests/core/scheme/compliance/run_chibi_tests.js [--compiled] [section filter...]
 */

import { runFromCommandLine } from './compliance_cli.js';

runFromCommandLine('chibi');
