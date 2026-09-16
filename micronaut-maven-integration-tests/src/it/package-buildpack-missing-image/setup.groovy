File stub = new File(basedir, 'pack-stub')
stub.text = '''#!/usr/bin/env bash
set -euo pipefail
if [ "${1:-}" = "--version" ]; then
  echo "pack stub 0.0.0"
  exit 0
fi
mkdir -p target
printf '%s\n' "$*" > target/pack-args.txt
echo "pack stub invoked"
'''
stub.setExecutable(true)
