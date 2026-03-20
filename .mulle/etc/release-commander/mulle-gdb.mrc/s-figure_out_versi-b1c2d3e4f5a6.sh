#!/bin/bash

echo "Current version:"
mulle-project-version get --custom-versioning 2>/dev/null | tail -1

echo ""
echo "Version status:"
mulle-project-version status --custom-versioning 2>/dev/null || true

NEW_TAG="$(mulle-project-version get --custom-versioning 2>/dev/null | tail -1)"
echo ""
echo "Release version: ${NEW_TAG}"

# sync gdb/version.in (not managed by mulle-project-version)
printf "%s" "${NEW_TAG}" > gdb/version.in
echo "Updated gdb/version.in to ${NEW_TAG}"

# update db.json with new tag
MRCDIR="${MRCDIR:-$(dirname "$0")}"
sed -i "s|\"MULLE_GDB_TAG\":.*|\"MULLE_GDB_TAG\": \"${NEW_TAG}\",|" "${MRCDIR}/db.json"
echo "Updated MULLE_GDB_TAG to ${NEW_TAG} in db.json"
