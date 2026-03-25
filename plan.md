# PRD: Migrate Parsing Logic to ANTLR

## Goal
Move all remaining custom parsing logic from CLJ/CLJS runtime code into ANTLR lexer/parser grammars.

## Scope
- In: parser start rules, inline parsing primitives, and grammar-driven AST extraction for both JVM and JS runtimes.
- Out: removing postprocess semantic shaping that is not syntax parsing.

## Requirements
1. Replace remaining custom parsing paths in `src/org_parser/antlr/parser.clj` and `src/org_parser/antlr/parser.cljs` with ANTLR-backed rules.
2. Keep CLJ and CLJS parser behavior aligned with a shared grammar-first approach.
3. Preserve existing AST contract and all current parser test expectations.

## Constraints
- Never remove existing test cases.
- Always run both test suites after each migration step: `lein test` and `lein doo node once`.
- Do not commit plan.md

## Acceptance Criteria
- No remaining custom parsing logic for syntax parsing in CLJ/CLJS runtime code.
- Parsing behavior is grammar-driven via ANTLR for both CLJ and CLJS targets.
- All existing tests pass.

## Completion Promise
<promise>COMPLETE</promise>
