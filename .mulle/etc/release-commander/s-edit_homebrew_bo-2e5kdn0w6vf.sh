#!/bin/bash
# After bottles are built and sha256 values are known, update mulle-gdb.rb
# with the bottle do block and push to mulle-objc/homebrew-software.
#
# Usage: set BOTTLE_SHA_ARM64 and BOTTLE_SHA_X86 env vars before running,
# or edit mulle-gdb.rb manually first.

set -e

FORMULA="/home/src/srcO/mulle-cc/mulle-gdb-11.1.mrc/../mulle-gdb.rb"

# Push updated formula to mulle-objc/homebrew-software
SHA="$(gh api "repos/mulle-objc/homebrew-software/contents/mulle-gdb.rb" \
   --jq '.sha' 2>/dev/null || echo '')"

if [ -z "${SHA}" ]; then
   # new file
   gh api repos/mulle-objc/homebrew-software/contents/mulle-gdb.rb \
      --method PUT \
      --field message="mulle-gdb ${MULLE_GDB_TAG}" \
      --field content="$(base64 -w0 "${FORMULA}")" \
      --field branch="master" \
      --jq '.commit.sha'
else
   gh api repos/mulle-objc/homebrew-software/contents/mulle-gdb.rb \
      --method PUT \
      --field message="mulle-gdb ${MULLE_GDB_TAG}" \
      --field content="$(base64 -w0 "${FORMULA}")" \
      --field sha="${SHA}" \
      --field branch="master" \
      --jq '.commit.sha'
fi

echo "Formula pushed to mulle-objc/homebrew-software"
