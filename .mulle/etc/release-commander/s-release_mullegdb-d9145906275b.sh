#!/bin/bash

set -e

# Trigger the GitHub Actions workflow to build and upload the .deb
gh workflow run build-deb.yml \
   --repo mulle-cc/mulle-gdb \
   --ref mulle/16.3.0 \
   --field tag="${MULLE_GDB_TAG}"

echo "Workflow triggered for ${MULLE_GDB_TAG} - waiting for completion..."

sleep 10

# get the run ID
RUN_ID="$(gh run list --repo mulle-cc/mulle-gdb --limit 1 \
   --json databaseId -q '.[0].databaseId')"

echo "Run ID: ${RUN_ID}"
gh run watch "${RUN_ID}" --repo mulle-cc/mulle-gdb

# verify .deb is on the release
DEB="$(gh release view "${MULLE_GDB_TAG}" --repo mulle-cc/mulle-gdb \
   --json assets -q '.assets[].name' | grep '\.deb' | head -1)"

if [ -z "${DEB}" ]; then
   echo "FAIL: no .deb found on release ${MULLE_GDB_TAG}" >&2
   exit 1
fi

echo "Release ${MULLE_GDB_TAG} has .deb: ${DEB}"
