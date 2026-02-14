#!/usr/bin/env bash
# Runs all runnable examples as a fast smoke test.
# A runnable example is any `.mc` file under `examples/` containing `fn main(...)`.
#
# Behavior:
# - Builds the compiler once up front.
# - Runs examples in parallel (auto CPU count, override with MACHINA_EXAMPLE_JOBS).
# - Suppresses normal example output.
# - Prints one progress dot per completed example.
# - Prints full logs only for failures.
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

# Select the worker count.
# Priority: explicit env var -> nproc -> sysctl -> fallback.
detect_jobs() {
  if [[ -n "${MACHINA_EXAMPLE_JOBS:-}" ]]; then
    echo "$MACHINA_EXAMPLE_JOBS"
    return
  fi
  if command -v nproc >/dev/null 2>&1; then
    nproc
    return
  fi
  if command -v sysctl >/dev/null 2>&1; then
    sysctl -n hw.ncpu 2>/dev/null || true
    return
  fi
  echo 4
}

declare -a runnable_examples=()
while IFS= read -r -d '' file; do
  # Treat files with a main entrypoint as runnable programs.
  if grep -Eq '^[[:space:]]*fn[[:space:]]+main[[:space:]]*\(' "$file"; then
    runnable_examples+=("${file#$repo_root/}")
  fi
done < <(find "$script_dir" -type f -name '*.mc' -print0 | sort -z)

if [[ ${#runnable_examples[@]} -eq 0 ]]; then
  echo "No runnable examples found."
  exit 0
fi

jobs="$(detect_jobs)"
if ! [[ "$jobs" =~ ^[0-9]+$ ]] || ((jobs < 1)); then
  jobs=1
fi

mcc_bin="$repo_root/target/debug/mcc"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
progress_fifo="$tmp_dir/progress.fifo"
mkfifo "$progress_fifo"

echo "Building compiler..."
(cd "$repo_root" && cargo build -q)

echo "Running ${#runnable_examples[@]} examples with ${jobs} worker(s)..."

# Read completion signals from workers and print live progress.
{
  while IFS= read -r _line; do
    printf '.'
  done < "$progress_fifo"
} &
progress_pid=$!

# Keep one writer open so the reader stays alive across brief gaps between workers.
exec 3>"$progress_fifo"

# Fan out example runs in parallel. Each worker writes:
# - `${idx}.rc`  : success/failure status
# - `${idx}.log` : captured stdout/stderr
# - one progress signal to the FIFO
{
  for i in "${!runnable_examples[@]}"; do
    printf '%s\0%s\0' "$i" "${runnable_examples[$i]}"
  done
} | xargs -0 -n2 -P "$jobs" bash -c '
  tmp_dir="$1"
  repo_root="$2"
  mcc_bin="$3"
  progress_fifo="$4"
  idx="$5"
  example="$6"
  log="$tmp_dir/${idx}.log"
  rc="$tmp_dir/${idx}.rc"
  source_path="$repo_root/$example"

  extra_args=()
  if grep -Eq "^[[:space:]]*typestate[[:space:]]" "$source_path"; then
    extra_args=(--experimental typestate)
  fi

  if (cd "$repo_root" && "$mcc_bin" run "${extra_args[@]}" "$example" >"$log" 2>&1); then
    echo 0 >"$rc"
  else
    echo 1 >"$rc"
  fi
  printf "done\n" >"$progress_fifo"
' _ "$tmp_dir" "$repo_root" "$mcc_bin" "$progress_fifo"

exec 3>&-
wait "$progress_pid"
printf '\n'

# Summarize failures in deterministic order (input list order), printing logs only when failed.
failures=0
for i in "${!runnable_examples[@]}"; do
  example="${runnable_examples[$i]}"
  rc_file="$tmp_dir/${i}.rc"
  log_file="$tmp_dir/${i}.log"
  if [[ -f "$rc_file" ]] && [[ "$(cat "$rc_file")" == "0" ]]; then
    continue
  fi
  printf '\nFAILED: %s\n' "$example"
  if [[ -f "$log_file" ]]; then
    cat "$log_file"
  else
    echo "missing run log for $example"
  fi
  failures=$((failures + 1))
done

if ((failures > 0)); then
  echo "$failures example(s) failed."
  exit 1
fi

echo "All examples passed."
