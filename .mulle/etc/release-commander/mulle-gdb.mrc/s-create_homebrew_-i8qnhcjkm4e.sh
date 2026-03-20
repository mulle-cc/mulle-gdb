#!/bin/bash

set -e

REPO="mulle-cc/mulle-gdb"
BRANCH="mulle/16.3.0"
FORMULA="mulle-gdb.rb"

# verify formula version matches before triggering
FORMULA_VERSION="$(grep '^\s*version ' "${FORMULA}" | sed 's/.*"\(.*\)".*/\1/')"
if [ "${FORMULA_VERSION}" != "${MULLE_GDB_TAG}" ]; then
   echo "FAIL: formula version '${FORMULA_VERSION}' != MULLE_GDB_TAG '${MULLE_GDB_TAG}'" >&2
   echo "Update ${FORMULA} version before building bottles." >&2
   exit 1
fi

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
BOTTLES="$(gh release view "${MULLE_GDB_TAG}" --repo "${REPO}" \
   --json assets -q '.assets[].name' | grep 'bottle' || true)"
echo "${BOTTLES}"

# verify bottle filename contains correct version
if ! echo "${BOTTLES}" | grep -q "${MULLE_GDB_TAG}"; then
   echo ""
   echo "FAIL: no bottle with version ${MULLE_GDB_TAG} in filename" >&2
   echo "Bottle was likely built with wrong formula version." >&2
   exit 1
fi
