#!/usr/bin/env -S bash -euo pipefail -O extglob

if [ "$#" -ne 4 ] || ! [[ "$1" =~ ^(plain|zig)$ ]]; then
  echo "Usage: $0 <format> <file-name> <old> <new>

Prints a unified Git diff between <old> and <new> under the given <file-name>.

If <format> is 'plain', the diff is printed as-is.
If <format> is 'zig', the diff is printed which each line prefixed with '\\\\'." >&2
  exit 1
fi

output_format="$1"
file_name="$2"
old="$3"
new="$4"

diff="$(git diff --no-index <(echo "$old") <(echo "$new") || true)"
diff="${diff//dev\/fd\/+([0-9])/$file_name}"

if [ "$output_format" = "plain" ]; then
  echo "$diff"
elif [ "$output_format" = "zig" ]; then
  echo "\\\\${diff//$'\n'/$'\n'\\\\}"
fi
