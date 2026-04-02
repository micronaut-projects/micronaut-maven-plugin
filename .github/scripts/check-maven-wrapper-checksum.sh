#!/usr/bin/env bash

set -euo pipefail

properties_file=.mvn/wrapper/maven-wrapper.properties

if [[ ! -f "$properties_file" ]]; then
  echo "::error file=$properties_file::Missing Maven wrapper properties file."
  exit 1
fi

distribution_url=$(awk '/^[[:space:]]*distributionUrl=/{print substr($0, index($0, "=") + 1); exit}' "$properties_file")
distribution_sha256=$(awk '/^[[:space:]]*distributionSha256Sum=/{print substr($0, index($0, "=") + 1); exit}' "$properties_file")

if [[ -z "$distribution_url" ]]; then
  echo "::error file=$properties_file::Missing distributionUrl property."
  exit 1
fi

if [[ -z "$distribution_sha256" ]]; then
  echo "::error file=$properties_file::Missing distributionSha256Sum for $distribution_url."
  exit 1
fi

if [[ ! "$distribution_sha256" =~ ^[0-9a-f]{64}$ ]]; then
  echo "::error file=$properties_file::distributionSha256Sum must be a 64-character lowercase SHA-256 hex digest."
  exit 1
fi

echo "Verified Maven wrapper checksum pin for $distribution_url."
