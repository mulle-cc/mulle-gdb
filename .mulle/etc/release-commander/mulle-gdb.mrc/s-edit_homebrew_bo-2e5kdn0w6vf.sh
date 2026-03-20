#!/bin/bash
# Copy the updated mulle-gdb.rb formula to mulle-objc/homebrew-software.
# The formula should already have the bottle do block updated with sha256 values.

set -e

REPO="mulle-objc/homebrew-software"
FORMULA="mulle-gdb.rb"

# Use the formula from the repo root
FORMULA_PATH="$(git rev-parse --show-toplevel)/${FORMULA}"

if [ ! -f "${FORMULA_PATH}" ]; then
   echo "Formula not found at ${FORMULA_PATH}" >&2
   exit 1
fi

echo "Pushing ${FORMULA} to ${REPO}..."

SHA="$(gh api "repos/${REPO}/contents/${FORMULA}" \
   --jq '.sha' 2>/dev/null || echo '')"

if [ -z "${SHA}" ]; then
   gh api "repos/${REPO}/contents/${FORMULA}" \
      --method PUT \
      --field message="mulle-gdb ${MULLE_GDB_TAG}" \
      --field content="$(base64 -w0 "${FORMULA_PATH}")" \
      --field branch="master" \
      --jq '.commit.sha'
else
   gh api "repos/${REPO}/contents/${FORMULA}" \
      --method PUT \
      --field message="mulle-gdb ${MULLE_GDB_TAG}" \
      --field content="$(base64 -w0 "${FORMULA_PATH}")" \
      --field sha="${SHA}" \
      --field branch="master" \
      --jq '.commit.sha'
fi

echo "Formula pushed to ${REPO}"
