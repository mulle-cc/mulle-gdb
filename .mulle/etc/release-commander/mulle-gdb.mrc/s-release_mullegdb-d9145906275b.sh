#!/bin/bash

set -e

REPO="mulle-cc/mulle-gdb"
BRANCH="mulle/16.3.0"
FORMULA="mulle-gdb.rb"

echo "=== Updating formula version to ${MULLE_GDB_TAG} ==="

sed -i "s|version \".*\"|version \"${MULLE_GDB_TAG}\"|" "${FORMULA}"
sed -i "s|/tags/.*\.tar\.gz|/tags/${MULLE_GDB_TAG}.tar.gz|" "${FORMULA}"
sed -i "s|/download/.*\"/|/download/${MULLE_GDB_TAG}/\"|" "${FORMULA}"

echo "=== Committing and pushing ==="

git add -u
git add .mulle/etc/release-commander/
git commit -m "release ${MULLE_GDB_TAG}" || echo "Nothing new to commit"
git tag -f "${MULLE_GDB_TAG}"
git push github "${BRANCH}"
git push github "refs/tags/${MULLE_GDB_TAG}" --force

echo ""
echo "=== Creating GitHub release (if needed) ==="

if ! gh release view "${MULLE_GDB_TAG}" --repo "${REPO}" > /dev/null 2>&1; then
   gh release create "${MULLE_GDB_TAG}" \
      --repo "${REPO}" \
      --title "${MULLE_GDB_TAG}" \
      --notes "mulle-gdb ${MULLE_GDB_TAG}"
   echo "Created release ${MULLE_GDB_TAG}"
else
   echo "Release ${MULLE_GDB_TAG} already exists"
fi

echo ""
echo "=== Updating source tarball sha256 ==="

TARBALL_SHA="$(curl -sL "https://github.com/${REPO}/archive/refs/tags/${MULLE_GDB_TAG}.tar.gz" | sha256sum | cut -d' ' -f1)"
echo "Source sha256: ${TARBALL_SHA}"
sed -i "/^  url /{ n; s|sha256 \".*\"|sha256 \"${TARBALL_SHA}\"|; }" "${FORMULA}"

git add "${FORMULA}"
git commit -m "update ${FORMULA} sha256 for ${MULLE_GDB_TAG}" || true
git tag -f "${MULLE_GDB_TAG}"
git push github "${BRANCH}"
git push github "refs/tags/${MULLE_GDB_TAG}" --force

echo ""
echo "=== Triggering .deb build ==="

gh workflow run build-deb.yml \
   --repo "${REPO}" \
   --ref "${BRANCH}" \
   --field tag="${MULLE_GDB_TAG}"

echo "Workflow triggered - waiting for completion..."
sleep 10

RUN_ID="$(gh run list --repo "${REPO}" --workflow build-deb.yml --limit 1 \
   --json databaseId -q '.[0].databaseId')"

echo "Run ID: ${RUN_ID}"
gh run watch "${RUN_ID}" --repo "${REPO}"

# verify .deb is on the release
DEB="$(gh release view "${MULLE_GDB_TAG}" --repo "${REPO}" \
   --json assets -q '.assets[].name' | grep '\.deb' | head -1)"

if [ -z "${DEB}" ]; then
   echo "FAIL: no .deb found on release ${MULLE_GDB_TAG}" >&2
   exit 1
fi

echo "Release ${MULLE_GDB_TAG} has .deb: ${DEB}"
