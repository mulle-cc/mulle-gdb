# Release mulle-gdb

Commit all local changes, tag, and push to GitHub. Then trigger the
`build-deb.yml` workflow to build a `.deb` and upload it to the release.

## Important

Before committing, ensure `mulle-gdb.rb` has the correct `version` and `url`
for `MULLE_GDB_TAG`. The bottle build (next step) uses the formula version
for the bottle filename — if it's wrong, the bottle gets the wrong version.

## Steps

1. Update `mulle-gdb.rb` version and url to `MULLE_GDB_TAG`.
   The source tarball sha256 can only be computed after the tag is pushed,
   so leave it for now — the bottle step will fix it.
2. Commit pending changes with `git add -u` (tracked files only) plus the
   `.mrc` directory. Do NOT use `git add -A` — the source tree contains
   build artifacts that must not be committed.
3. Tag the commit with `MULLE_GDB_TAG`.
4. Push the branch and tag to `github` remote.
5. Create the GitHub release if it doesn't exist yet.
6. Now compute the source tarball sha256 and update `mulle-gdb.rb`.
7. Commit and push the sha256 update.
8. Trigger `build-deb.yml` workflow with the tag.
9. Wait for the workflow to complete and verify the `.deb` appears on the release.
