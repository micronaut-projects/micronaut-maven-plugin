#!/usr/bin/env bash

set -euo pipefail

shopt -s nullglob

workflow_files=(.github/workflows/*.yml .github/workflows/*.yaml)
failures=0

for file in "${workflow_files[@]}"; do
  if rg -n 'raw\.githubusercontent\.com' "$file" >/dev/null; then
    echo "::error file=$file::Remote raw.githubusercontent.com downloads are not allowed in workflows. Vendor the script into this repository or pin the source to an audited commit outside the workflow."
    rg -n 'raw\.githubusercontent\.com' "$file"
    failures=1
  fi

  while IFS=: read -r lineno line; do
    ref=$(printf '%s\n' "$line" | sed -E 's/^[[:space:]-]*uses:[[:space:]]*//; s/[[:space:]]+#.*$//; s/[[:space:]]+$//')

    if [[ -z "$ref" ]]; then
      continue
    fi

    if [[ "$ref" =~ ^\./ ]] || [[ "$ref" =~ ^docker:// ]]; then
      continue
    fi

    if [[ "$ref" =~ ^[^/]+/[^@]+(/[^@]+)*@[0-9a-f]{40}$ ]]; then
      continue
    fi

    echo "::error file=$file,line=$lineno::GitHub Actions must use a full 40-character commit SHA: $ref"
    failures=1
  done < <(rg -n '^[[:space:]-]*uses:[[:space:]]*' "$file")
done

exit "$failures"
