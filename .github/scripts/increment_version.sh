#!/usr/bin/env bash

# Vendored from micronaut-projects/github-actions at commit
# 300bf6db7c062dcba77c90bb90e475df31b2acab, path post-release/increment_version.sh.
# Increment a version string using Semantic Versioning (SemVer) terminology.
# Source: https://github.com/fmahnke/shell-semver

usage() {
  echo "Usage: $0 [-M | -m | -p] version" >&2
}

major=false
minor=false
patch=false
selected=0

while getopts ":Mmp" Option; do
  case $Option in
    M )
      major=true
      ((selected++))
      ;;
    m )
      minor=true
      ((selected++))
      ;;
    p )
      patch=true
      ((selected++))
      ;;
    * )
      usage
      exit 1
      ;;
  esac
done

shift $((OPTIND - 1))

if (( selected != 1 )) || [[ $# -ne 1 ]]; then
  usage
  exit 1
fi

version=$1
IFS=. read -r major_part minor_part patch_part extra_part <<< "$version"
prerelease=false

if [[ -n "${extra_part:-}" ]] || [[ -z "${major_part:-}" ]] || [[ -z "${minor_part:-}" ]] || [[ -z "${patch_part:-}" ]]; then
  echo "Invalid version: $version" >&2
  usage
  exit 1
fi

if ! [[ "$major_part" =~ ^[0-9]+$ ]] || ! [[ "$minor_part" =~ ^[0-9]+$ ]]; then
  echo "Invalid version: $version" >&2
  usage
  exit 1
fi

if [[ "$patch_part" =~ ^([0-9]+)$ ]]; then
  patch_number="${BASH_REMATCH[1]}"
elif [[ "$patch_part" =~ ^([0-9]+)-((M|RC).+)$ ]]; then
  patch_number="${BASH_REMATCH[1]}"
  prerelease=true
elif [[ "$patch_part" =~ ^((M|RC).+)$ ]]; then
  patch_number=0
  prerelease=true
else
  echo "Invalid version: $version" >&2
  usage
  exit 1
fi

a=("$major_part" "$minor_part" "$patch_number")

if [[ "$major" == true ]]; then
  ((a[0]++))
  a[1]=0
  a[2]=0
fi

if [[ "$minor" == true ]]; then
  ((a[1]++))
  a[2]=0
fi

if [[ "$patch" == true ]] && [[ "$prerelease" == false ]]; then
  ((a[2]++))
else
  a[2]=0
fi

echo "${a[0]}.${a[1]}.${a[2]}"
