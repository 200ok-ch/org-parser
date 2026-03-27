# PRD: Make Parser Competitive With Organice

## Goal
Reduce `org-parser` parse time and memory usage for large Org documents so it stays competitive with `organice` on the same realistic 31k-line input.

## Scope
- In: parser performance and memory optimizations, realistic large-file benchmarks, regression coverage, profiling workflow that supports step-by-step optimization
- Out: feature changes to Org syntax support, transform/render API redesigns, browser/UI work

## Requirements
1. Keep all existing tests passing throughout the optimization work.
2. Use `large-unique-readme-31kloc.org` and the benchmark documented in `organice-performance.md` as the primary realistic comparison target.
3. The exact `org-parser` regression test case to keep running is `org-parser.large-file-test/parse-large-unique-readme-derived-document`, together with `org-parser.large-file-test/parser-accepts-large-unique-readme-derived-document`.
4. The exact benchmark input to compare is the `:large-readme-unique-derived` fixture in `src/org_parser/benchmark.clj`, which corresponds to `large-unique-readme-31kloc.org`.
5. Add or maintain repeatable benchmark/profiling coverage so each optimization step can be measured independently.
6. Improve parse performance so `org-parser` is not worse than `2x` `organice` on the 31k-line realistic benchmark.
7. Improve memory usage so peak memory is also not worse than `2x` `organice` on the same benchmark.

## Constraints
- Preserve parser correctness and current public behavior.
- Do not remove existing test coverage; add regression/perf coverage where useful.
- Favor changes that can be verified incrementally and bisected if needed.
- Benchmark against realistic inputs, not only highly repetitive synthetic cases.

## Acceptance Criteria
- All existing tests pass.
- `org-parser` successfully parses `large-unique-readme-31kloc.org`.
- Running `lein test :only org-parser.large-file-test/parse-large-unique-readme-derived-document :only org-parser.large-file-test/parser-accepts-large-unique-readme-derived-document` passes.
- On the realistic 31k-line benchmark, parse time is no worse than `2x` the `organice` numbers in `organice-performance.md`.
- On the same benchmark, peak memory usage is no worse than `2x` the `organice` memory numbers in `organice-performance.md`.
- Benchmark instructions and target files are present in the repo so optimization can continue step by step.

## Completion Promise
<promise>COMPLETE</promise>
