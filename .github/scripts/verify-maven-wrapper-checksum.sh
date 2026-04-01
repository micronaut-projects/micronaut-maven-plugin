#!/usr/bin/env bash
set -euo pipefail

properties_file="${1:-.mvn/wrapper/maven-wrapper.properties}"

if [[ ! -f "$properties_file" ]]; then
  echo "Missing wrapper properties file: $properties_file" >&2
  exit 1
fi

distribution_url=""
distribution_sha256_sum=""

while IFS='=' read -r key value; do
  case "$key" in
    distributionUrl) distribution_url="${value}" ;;
    distributionSha256Sum) distribution_sha256_sum="${value}" ;;
  esac
done <"$properties_file"

if [[ -z "$distribution_url" ]]; then
  echo "distributionUrl is missing from $properties_file" >&2
  exit 1
fi

if [[ -z "$distribution_sha256_sum" ]]; then
  echo "distributionSha256Sum is missing from $properties_file" >&2
  exit 1
fi

if [[ ! "$distribution_sha256_sum" =~ ^[0-9a-f]{64}$ ]]; then
  echo "distributionSha256Sum must be a lowercase SHA-256 hex digest" >&2
  exit 1
fi

tmp_file="$(mktemp)"
trap 'rm -f "$tmp_file"' EXIT

curl -fsSL "$distribution_url" -o "$tmp_file"

actual_sha256_sum=""
if command -v sha256sum >/dev/null 2>&1; then
  actual_sha256_sum="$(sha256sum "$tmp_file" | awk '{print $1}')"
elif command -v shasum >/dev/null 2>&1; then
  actual_sha256_sum="$(shasum -a 256 "$tmp_file" | awk '{print $1}')"
else
  echo "Missing SHA-256 tool: install sha256sum or shasum" >&2
  exit 1
fi

if [[ "$actual_sha256_sum" != "$distribution_sha256_sum" ]]; then
  echo "Maven wrapper distribution checksum mismatch" >&2
  echo "Expected: $distribution_sha256_sum" >&2
  echo "Actual:   $actual_sha256_sum" >&2
  exit 1
fi

echo "Verified Maven wrapper checksum for $distribution_url"
