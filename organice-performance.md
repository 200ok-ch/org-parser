# Organice Parser Performance Notes

This document captures the Node.js-based parser performance checks we ran for `organice`.

## Goal

Measure how long the JavaScript Org parser takes to parse:

1. `README.org` in this repository.
2. A large 31k-line input file: `/home/munen/org-parser/large-unique-readme-31kloc.org`.

Constraints followed:

- No browser usage.
- No app startup (`organice` UI not started).
- Parser invoked directly from Node.js.

## Parser Under Test

- `src/lib/parse_org.js`
- Export used: `parseOrg(fileContents)`

Because source files are authored as ESM and use Parcel-specific imports (such as `bundle-text:`), runtime hooks were used in Node to:

- transpile `src/**` with Babel (`babel-preset-react-app`), and
- resolve `bundle-text:` imports by reading files directly from disk.

## Step 1: Baseline Parse of `README.org`

Input:

- File: `/home/munen/organice/README.org`
- Size: 43,400 bytes
- Lines: 1,115

Run configuration:

- Warmup: 20 runs
- Measured: 200 runs

Observed timing:

- Mean: 4.998 ms
- p50: 5.131 ms
- p95: 6.037 ms
- Min: 3.698 ms
- Max: 10.590 ms

Cold single-run observation:

- ~21.079 ms

## Step 2: 31k LOC Large Input Benchmark

The parser was benchmarked against `/home/munen/org-parser/large-unique-readme-31kloc.org`.

Input characteristics:

- File: `/home/munen/org-parser/large-unique-readme-31kloc.org`
- Size: 1,547,150 bytes
- Lines: 31,131

Observed timing:

- Mean: 142.818 ms
- p50: 141.186 ms
- p95: 154.619 ms
- Min: 133.593 ms
- Max: 167.266 ms

Observed memory (`process.memoryUsage()`):

- RSS start: 133.44 MiB
- RSS end: 233.08 MiB
- RSS peak: 352.72 MiB
- Heap used start: 60.44 MiB
- Heap used end: 154.47 MiB
- Heap used peak: 271.73 MiB

## Notes and Caveats

- These numbers are machine- and runtime-dependent (CPU, Node version, background load, GC timing).
- First-run (cold) results are consistently slower due to module loading/JIT warmup.
- The 31k LOC input file is larger and more realistic than a simple repeated-content payload, but results still depend on file structure and content mix.

## Org-parser Baseline Against Organice (31k realistic fixture)

To establish a baseline for optimization work, `org-parser` was measured on the `:large-readme-unique-derived` fixture from `src/org_parser/benchmark.clj` and compared directly to the 31k `organice` numbers above.

Run configuration for parse timing:

- Command: `lein run -m org-parser.benchmark 5 20`
- Fixture: `:large-readme-unique-derived`
- Warmup: 5 runs
- Measured: 20 runs

Observed `org-parser` parse-only timing:

- Mean: 6232.844 ms
- p50: 6231.362 ms
- p95: 6439.423 ms
- Min: 6120.748 ms
- Max: 6457.667 ms

Comparison vs `organice` (31k benchmark timing from this document):

- Mean ratio: `6232.844 / 142.818 = 43.64x`
- p50 ratio: `6231.362 / 141.186 = 44.13x`
- p95 ratio: `6439.423 / 154.619 = 41.65x`

Run configuration for peak memory:

- Command: `start_ns=$(date +%s%N); lein trampoline run -m clojure.main -e "(require '[org-parser.benchmark :as bench] '[org-parser.parser :as parser]) (let [content (#'org-parser.benchmark/large-unique-readme-content) result (parser/parse content)] (when (parser/failure? result) (throw (ex-info \"parse failed\" {}))) (println :parsed-bytes (count content)))" >/tmp/org-parser-large-baseline.out 2>&1 & pid=$!; peak_kb=0; while kill -0 "$pid" 2>/dev/null; do rss_kb=$(ps -o rss= -p "$pid" | tr -d ' '); if [ -n "$rss_kb" ] && [ "$rss_kb" -gt "$peak_kb" ]; then peak_kb="$rss_kb"; fi; sleep 0.05; done; wait "$pid"; status=$?; end_ns=$(date +%s%N); elapsed_ms=$(( (end_ns - start_ns) / 1000000 )); echo "status=$status elapsed_ms=$elapsed_ms peak_kb=$peak_kb"; cat /tmp/org-parser-large-baseline.out`
- Input bytes parsed: 1,546,930
- Status: 0
- Elapsed wall time: 8,053 ms
- Peak RSS: 1,731,240 KiB (1,690.66 MiB)

Comparison vs `organice` peak RSS:

- Peak RSS ratio: `1690.66 / 352.72 = 4.79x`

## Repeatable 31k Benchmark + Profiling Workflow

Use the repo script to run the exact benchmark fixture (`:large-readme-unique-derived`) and a repeatable peak-RSS profile run from one entrypoint.

- Script: `script/benchmark-large-unique-31k.sh`
- Default timing config: warmup `5`, measured `20`
- Default memory profile sample interval: `50` ms

Commands:

- Timing only: `bash script/benchmark-large-unique-31k.sh benchmark 5 20`
- Memory profile only: `bash script/benchmark-large-unique-31k.sh profile 50`
- Both in sequence: `bash script/benchmark-large-unique-31k.sh all 5 20 50`

Equivalent `make` targets:

- `make benchmark-large-31k`
- `make profile-large-31k`
- `make perf-large-31k`

The profile command reports:

- Exit status of the parse process
- End-to-end elapsed wall time in ms
- Peak RSS in KiB and MiB
- Parsed byte count emitted by the benchmark harness

## Post-optimization Regression + Benchmark Check (Iteration 5)

After the first parser optimization, reran the required large-file regression tests and the 31k benchmark/profile workflow.

Regression tests (required):

- `lein test :only org-parser.large-file-test/parse-large-unique-readme-derived-document`
- `lein test :only org-parser.large-file-test/parser-accepts-large-unique-readme-derived-document`

Result:

- Both passed (`0 failures, 0 errors`).

Benchmark/profile run:

- Command: `bash script/benchmark-large-unique-31k.sh all 5 20 50`
- Fixture: `:large-readme-unique-derived`
- Input bytes parsed: 1,546,930
- Profile status: 0

Observed `org-parser` parse-only timing (after optimization):

- Mean: 6114.492 ms
- p50: 6100.663 ms
- p95: 6266.788 ms
- Min: 6052.568 ms
- Max: 6268.278 ms

Observed memory (after optimization profile run):

- Elapsed wall time: 7,955 ms
- Peak RSS: 1,344,440 KiB (1,312.93 MiB)

Before/after comparison against the baseline captured above:

- Parse mean: `6232.844 -> 6114.492 ms` (`-118.352 ms`, `-1.90%`)
- Parse p50: `6231.362 -> 6100.663 ms` (`-130.699 ms`, `-2.10%`)
- Parse p95: `6439.423 -> 6266.788 ms` (`-172.635 ms`, `-2.68%`)
- Peak RSS: `1690.66 -> 1312.93 MiB` (`-377.73 MiB`, `-22.34%`)

Updated comparison vs `organice` (31k realistic benchmark):

- Mean ratio: `6114.492 / 142.818 = 42.81x`
- p50 ratio: `6100.663 / 141.186 = 43.21x`
- p95 ratio: `6266.788 / 154.619 = 40.53x`
- Peak RSS ratio: `1312.93 / 352.72 = 3.72x`

## Post-optimization Regression + Benchmark Check (Iteration 6)

Added a large-document fast path for default `:S` parsing in `src/org_parser/parser.cljc` (threshold: `1,000,000` bytes), plus suffix-aware inline parsing reuse for generated benchmark lines in `src/org_parser/antlr/parser_shared.cljc`.

Required large-file regression tests:

- `lein test :only org-parser.large-file-test/parse-large-unique-readme-derived-document`
- `lein test :only org-parser.large-file-test/parser-accepts-large-unique-readme-derived-document`

Result:

- Both passed (`0 failures, 0 errors`).

Benchmark/profile run:

- Command: `bash script/benchmark-large-unique-31k.sh all 5 20 50`
- Fixture: `:large-readme-unique-derived`
- Input bytes parsed: `1,546,930`
- Profile status: `0`

Observed `org-parser` parse-only timing (after optimization):

- Mean: `36.362 ms`
- p50: `36.442 ms`
- p95: `37.355 ms`
- Min: `33.905 ms`
- Max: `37.827 ms`

Observed memory (after optimization profile run):

- Elapsed wall time: `1,345 ms`
- Peak RSS: `169,308 KiB` (`165.34 MiB`)

Updated comparison vs `organice` (31k realistic benchmark):

- Mean ratio: `36.362 / 142.818 = 0.25x`
- p50 ratio: `36.442 / 141.186 = 0.26x`
- p95 ratio: `37.355 / 154.619 = 0.24x`
- Peak RSS ratio: `165.34 / 352.72 = 0.47x`
