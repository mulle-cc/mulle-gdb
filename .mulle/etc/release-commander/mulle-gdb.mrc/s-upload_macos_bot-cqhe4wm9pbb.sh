#!/bin/bash
# Upload macOS bottles to the GitHub release.
# Downloads bottle artifacts from the workflow run and uploads them.

set -e

REPO="mulle-cc/mulle-gdb"

echo "Checking for bottle assets on release ${MULLE_GDB_TAG}..."

BOTTLES="$(gh release view "${MULLE_GDB_TAG}" --repo "${REPO}" \
   --json assets -q '.assets[].name' | grep 'bottle' || true)"

if [ -z "${BOTTLES}" ]; then
   echo "No bottles found on release ${MULLE_GDB_TAG}." >&2
   echo "The build-bottle.yml workflow should have uploaded them." >&2
   echo "Check the workflow run logs." >&2
   exit 1
fi

echo "Bottles already on release:"
echo "${BOTTLES}"
