#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

tmp_log="$(mktemp)"
trap 'rm -f "$tmp_log"' EXIT

declare -a runnable_examples=()
while IFS= read -r -d '' file; do
  if grep -Eq '^[[:space:]]*fn[[:space:]]+main[[:space:]]*\(' "$file"; then
    runnable_examples+=("${file#$repo_root/}")
  fi
done < <(find "$script_dir" -type f -name '*.mc' -print0 | sort -z)

if [[ ${#runnable_examples[@]} -eq 0 ]]; then
  echo "No runnable examples found."
  exit 0
fi

echo "Running ${#runnable_examples[@]} examples..."

failures=0
for example in "${runnable_examples[@]}"; do
  if (cd "$repo_root" && cargo mcr "$example" >"$tmp_log" 2>&1); then
    printf '.'
  else
    printf '\nFAILED: %s\n' "$example"
    cat "$tmp_log"
    failures=$((failures + 1))
  fi
done
printf '\n'

if ((failures > 0)); then
  echo "$failures example(s) failed."
  exit 1
fi

echo "All examples passed."
