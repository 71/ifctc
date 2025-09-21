#!/usr/bin/env -S bash -euo pipefail
cd "$(dirname "$0")"

update=0

if [ "$#" -eq 1 ]; then
  if [ "$1" = "update" ]; then
    update=1
  else
    echo "Usage: $0 [update]"
    exit 1
  fi
fi

zig build
ifctc=../zig-out/bin/ifctc

for patch in *.patch; do
  out_file="${patch%.patch}.stdout"

  if [ "$update" -eq 1 ]; then
    ("$ifctc" < "$patch" || true) > "$out_file"
  else
    diff "$out_file" <("$ifctc" < "$patch" || true)
  fi
done
