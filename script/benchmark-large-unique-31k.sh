#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

usage() {
  cat <<'EOF'
Usage:
  script/benchmark-large-unique-31k.sh benchmark [warmup-runs] [measure-runs]
  script/benchmark-large-unique-31k.sh profile [sample-ms]
  script/benchmark-large-unique-31k.sh all [warmup-runs] [measure-runs] [sample-ms]

Defaults:
  warmup-runs=5
  measure-runs=20
  sample-ms=50
EOF
}

run_benchmark() {
  local warmup_runs="${1:-5}"
  local measure_runs="${2:-20}"

  lein trampoline run -m clojure.main -e "(require '[clojure.pprint :as pp] '[org-parser.benchmark :as bench]) (let [results (bench/run-benchmarks {:warmup-runs ${warmup_runs} :measure-runs ${measure_runs}}) target (first (filter #(= :large-readme-unique-derived (:case %)) (:results results)))] (pp/pprint {:settings (:settings results) :result target}))"
}

run_profile() {
  local sample_ms="${1:-50}"
  local sample_sec
  local out_file="/tmp/org-parser-large-unique-profile.out"
  local start_ns end_ns elapsed_ms pid status rss_kb peak_kb

  if ! [[ "$sample_ms" =~ ^[0-9]+$ ]] || (( sample_ms <= 0 )); then
    echo "sample-ms must be a positive integer, got: $sample_ms" >&2
    exit 1
  fi

  sample_sec="$(awk "BEGIN {printf \"%.3f\", ${sample_ms}/1000}")"

  start_ns="$(date +%s%N)"

  lein trampoline run -m clojure.main -e "(require '[org-parser.benchmark :as bench] '[org-parser.parser :as parser]) (let [content (#'org-parser.benchmark/large-unique-readme-content) result (parser/parse content)] (when (parser/failure? result) (throw (ex-info \"parse failed\" {}))) (println :parsed-bytes (count content)))" >"$out_file" 2>&1 &
  pid=$!

  peak_kb=0
  while kill -0 "$pid" 2>/dev/null; do
    rss_kb="$(ps -o rss= -p "$pid" | tr -d ' ' || true)"
    if [[ -n "$rss_kb" && "$rss_kb" =~ ^[0-9]+$ ]] && (( rss_kb > peak_kb )); then
      peak_kb="$rss_kb"
    fi
    sleep "$sample_sec"
  done

  set +e
  wait "$pid"
  status=$?
  set -e
  end_ns="$(date +%s%N)"
  elapsed_ms="$(( (end_ns - start_ns) / 1000000 ))"

  printf 'status=%s elapsed_ms=%s peak_kb=%s peak_mib=%.2f\n' \
    "$status" "$elapsed_ms" "$peak_kb" "$(awk "BEGIN {printf \"%.2f\", ${peak_kb}/1024}")"
  cat "$out_file"

  return "$status"
}

main() {
  local cmd="${1:-all}"
  case "$cmd" in
    benchmark)
      shift
      run_benchmark "${1:-5}" "${2:-20}"
      ;;
    profile)
      shift
      run_profile "${1:-50}"
      ;;
    all)
      shift
      run_benchmark "${1:-5}" "${2:-20}"
      run_profile "${3:-50}"
      ;;
    -h|--help|help)
      usage
      ;;
    *)
      usage
      exit 1
      ;;
  esac
}

main "$@"
