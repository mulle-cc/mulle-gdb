#!/bin/bash

set -e

GDB_SRC="/home/src/srcL/mulle-gdb-11.1"
REPO="mulle-cc/mulle-gdb"

# read base version from source
BASE_VERSION="$(cat "${GDB_SRC}/gdb/version.in" | tr -d '[:space:]')"
if [ -z "${BASE_VERSION}" ]; then
   echo "Could not read version from ${GDB_SRC}/gdb/version.in" >&2
   exit 1
fi
echo "Base version: ${BASE_VERSION}"

# find latest release tag matching this base version
LAST_TAG="$(gh release list --repo "${REPO}" --limit 20 \
   | awk '{print $NF}' \
   | grep "^${BASE_VERSION}\." \
   | sort -t. -k3,3n -k4,4n \
   | tail -1)"

echo "Last release tag: ${LAST_TAG:-none}"

# check for new commits since last tag
if [ -n "${LAST_TAG}" ]; then
   COMMITS="$(git -C "${GDB_SRC}" log --oneline "${LAST_TAG}..HEAD" 2>/dev/null | wc -l | tr -d ' ')"
   echo "Commits since ${LAST_TAG}: ${COMMITS}"
   if [ "${COMMITS}" = "0" ]; then
      echo "No new commits since ${LAST_TAG} - nothing to release" >&2
      exit 1
   fi

   # check if .deb already exists for last tag
   EXISTING_DEB="$(gh release view "${LAST_TAG}" --repo "${REPO}" --json assets \
      -q '.assets[].name' 2>/dev/null | grep '\.deb' | head -1)"
   if [ -n "${EXISTING_DEB}" ]; then
      # bump patch version
      MAJOR="$(echo "${LAST_TAG}" | cut -d. -f1)"
      MINOR="$(echo "${LAST_TAG}" | cut -d. -f2)"
      PATCH="$(echo "${LAST_TAG}" | cut -d. -f3)"
      BUILD="$(echo "${LAST_TAG}" | cut -d. -f4)"
      NEW_TAG="${MAJOR}.${MINOR}.${PATCH}.$((BUILD + 1))"
   else
      NEW_TAG="${LAST_TAG}"
   fi
else
   # no prior release for this base version - start at x.y.0.1
   NEW_TAG="${BASE_VERSION}.0.1"
fi

echo "New tag: ${NEW_TAG}"

# create release if it doesn't exist
if ! gh release view "${NEW_TAG}" --repo "${REPO}" > /dev/null 2>&1; then
   gh release create "${NEW_TAG}" \
      --repo "${REPO}" \
      --title "${NEW_TAG}" \
      --notes "mulle-gdb ${NEW_TAG}"
   echo "Created release ${NEW_TAG}"
fi

# update db.json with new tag
MRCDIR="${MRCDIR:-$(dirname "$0")}"
python3 -c "
import json, sys
with open('${MRCDIR}/db.json') as f:
    db = json.load(f)
db['environmentVariables']['MULLE_GDB_TAG'] = sys.argv[1]
with open('${MRCDIR}/db.json', 'w') as f:
    json.dump(db, f, indent=2)
" "${NEW_TAG}" 2>/dev/null || echo "Note: could not auto-update db.json MULLE_GDB_TAG - set it manually to ${NEW_TAG}"
