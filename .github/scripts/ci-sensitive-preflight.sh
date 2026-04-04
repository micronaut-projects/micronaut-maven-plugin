#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/../.." && pwd)
windows_script_path="$repo_root/.github/scripts/windows-preflight.cmd"

echo "Running workflow pinning preflight..."
bash "$script_dir/check-workflow-pinning.sh"
echo "Workflow pinning preflight passed."

if command -v cmd.exe >/dev/null 2>&1 && [[ -f "$windows_script_path" ]]; then
  echo "Running Windows wrapper preflight..."
  (
    cd "$repo_root"
    cmd.exe //c ".github\\scripts\\windows-preflight.cmd"
  )
  echo "Windows wrapper preflight passed."
else
  cat <<'EOF'
Windows wrapper preflight still needs a Windows shell.

- Run `.\.github\scripts\windows-preflight.cmd` in a Windows shell, or
- trigger the `Windows Preflight` GitHub Actions workflow before review handoff.
EOF
fi
