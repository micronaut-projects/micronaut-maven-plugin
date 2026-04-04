#!/usr/bin/env bash
set -euo pipefail

properties_file="${1:-.mvn/wrapper/maven-wrapper.properties}"
wrapper_dir="$(cd "$(dirname "$properties_file")" && pwd)"
wrapper_jar="$wrapper_dir/maven-wrapper.jar"

normalize_properties_value() {
  printf '%s' "$1" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//;s/\\:/:/g;s/\\\\/\\/g'
}

sha256_file() {
  local file="$1"

  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$file" | awk '{print $1}'
  else
    echo "Missing SHA-256 tool: install sha256sum or shasum" >&2
    exit 1
  fi
}

if [[ ! -f "$properties_file" ]]; then
  echo "Missing wrapper properties file: $properties_file" >&2
  exit 1
fi

distribution_url=""
distribution_sha256_sum=""
wrapper_sha256_sum=""

while IFS= read -r line || [[ -n "$line" ]]; do
  # GitHub Windows runners can check out this properties file with CRLF endings.
  # Normalize the parsed line so the checksum regex still validates the real value.
  line="${line%$'\r'}"

  case "$line" in
    distributionUrl=*)
      distribution_url="$(normalize_properties_value "${line#distributionUrl=}")"
      ;;
    distributionSha256Sum=*)
      distribution_sha256_sum="$(normalize_properties_value "${line#distributionSha256Sum=}")"
      ;;
    wrapperSha256Sum=*)
      wrapper_sha256_sum="$(normalize_properties_value "${line#wrapperSha256Sum=}")"
      ;;
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

distribution_sha256_sum="${distribution_sha256_sum,,}"

if [[ ! "$distribution_sha256_sum" =~ ^[0-9a-f]{64}$ ]]; then
  echo "distributionSha256Sum must be a SHA-256 hex digest" >&2
  exit 1
fi

if [[ -f "$wrapper_jar" ]]; then
  if [[ -z "$wrapper_sha256_sum" ]]; then
    echo "wrapperSha256Sum is missing from $properties_file" >&2
    exit 1
  fi

  wrapper_sha256_sum="${wrapper_sha256_sum,,}"

  if [[ ! "$wrapper_sha256_sum" =~ ^[0-9a-f]{64}$ ]]; then
    echo "wrapperSha256Sum must be a SHA-256 hex digest" >&2
    exit 1
  fi

  actual_wrapper_sha256_sum="$(sha256_file "$wrapper_jar")"
  if [[ "$actual_wrapper_sha256_sum" != "$wrapper_sha256_sum" ]]; then
    echo "Maven wrapper JAR checksum mismatch" >&2
    echo "Expected: $wrapper_sha256_sum" >&2
    echo "Actual:   $actual_wrapper_sha256_sum" >&2
    exit 1
  fi
fi

# Use an explicit template so local verification also works with BSD/macOS mktemp.
tmp_file="$(mktemp "${TMPDIR:-/tmp}/maven-wrapper-checksum.XXXXXX")"
trap 'rm -f "$tmp_file"' EXIT

curl -fsSL \
  --connect-timeout 10 \
  --max-time 300 \
  --retry 3 \
  --retry-delay 5 \
  -o "$tmp_file" \
  -- "$distribution_url"

actual_sha256_sum="$(sha256_file "$tmp_file")"

if [[ "$actual_sha256_sum" != "$distribution_sha256_sum" ]]; then
  echo "Maven wrapper distribution checksum mismatch" >&2
  echo "Expected: $distribution_sha256_sum" >&2
  echo "Actual:   $actual_sha256_sum" >&2
  exit 1
fi

echo "Verified Maven wrapper checksum for $distribution_url"
