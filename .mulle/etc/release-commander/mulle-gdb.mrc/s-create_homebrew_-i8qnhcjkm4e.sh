#!/bin/bash
# Build Homebrew bottles for mulle-gdb on macOS via GitHub Actions.
# Triggers build-bottle.yml which runs on macos-15 (arm64) and macos-15-large (x86_64).
# The workflow checks out the branch, builds the bottle,
# and uploads it to the GitHub release.

set -e

REPO="mulle-cc/mulle-gdb"
BRANCH="mulle/16.3.0"

gh workflow run build-bottle.yml \
   --repo "${REPO}" \
   --ref "${BRANCH}" \
   --field tag="${MULLE_GDB_TAG}"

echo "Bottle build triggered for ${MULLE_GDB_TAG}"
echo "Waiting for workflow to complete..."

sleep 15

RUN_ID="$(gh run list --repo "${REPO}" \
   --workflow build-bottle.yml --limit 1 \
   --json databaseId -q '.[0].databaseId')"

echo "Run ID: ${RUN_ID}"
gh run watch "${RUN_ID}" --repo "${REPO}"

echo ""
echo "Bottles on release ${MULLE_GDB_TAG}:"
gh release view "${MULLE_GDB_TAG}" --repo "${REPO}" \
   --json assets -q '.assets[].name' | grep bottle || echo "No bottles found"

echo ""
echo "TODO: update bottle do block in mulle-gdb.rb with sha256 values,"
echo "      then push to mulle-objc/homebrew-software."
