#!/bin/bash

# Vendored from micronaut-projects/github-actions at commit
# 300bf6db7c062dcba77c90bb90e475df31b2acab, path post-release/increment_version.sh.
# Increment a version string using Semantic Versioning (SemVer) terminology.
# Source: https://github.com/fmahnke/shell-semver

while getopts ":Mmp" Option
do
  case $Option in
    M ) major=true;;
    m ) minor=true;;
    p ) patch=true;;
  esac
done

shift $(($OPTIND - 1))

version=$1

a=( ${version//./ } )

if [ ! -z "$major" ]
then
  ((a[0]++))
  a[1]=0
  a[2]=0
fi

if [ ! -z "$minor" ]
then
  ((a[1]++))
  a[2]=0
fi

if [ ! -z "$patch" ] && ! [[ "${a[2]}" =~ M.*|RC.* ]]
then
  ((a[2]++))
else
  a[2]=0
fi

echo "${a[0]}.${a[1]}.${a[2]}"
