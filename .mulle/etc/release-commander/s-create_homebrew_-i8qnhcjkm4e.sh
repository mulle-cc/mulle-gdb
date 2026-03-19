#!/bin/bash
# Build Homebrew bottles for mulle-gdb on macOS via GitHub Actions.
# Triggers build-bottle.yml which runs on macos-15 (arm64) and macos-15-large (x86_64).
# The workflow checks out master (which contains mulle-gdb.rb), builds the bottle,
# and uploads it to the GitHub release.
#
# After this completes, manually update the bottle do block in mulle-gdb.rb
# with the sha256 values printed by `brew bottle`, then push to homebrew-software.

set -e

gh workflow run build-bottle.yml \
   --repo mulle-cc/mulle-gdb \
   --ref master \
   --field tag="${MULLE_GDB_TAG}"

echo "Bottle build triggered for ${MULLE_GDB_TAG}"
echo "Waiting for both matrix jobs (arm64_sequoia + sequoia)..."

sleep 15

RUN_ID="$(gh run list --repo mulle-cc/mulle-gdb \
   --workflow build-bottle.yml --limit 1 \
   --json databaseId -q '.[0].databaseId')"

echo "Run ID: ${RUN_ID}"
gh run watch "${RUN_ID}" --repo mulle-cc/mulle-gdb

echo ""
echo "Bottles on release ${MULLE_GDB_TAG}:"
gh release view "${MULLE_GDB_TAG}" --repo mulle-cc/mulle-gdb \
   --json assets -q '.assets[].name' | grep bottle

echo ""
echo "TODO: update bottle do block in mulle-gdb.rb with sha256 values,"
echo "      then push to mulle-objc/homebrew-software."
