# Release mulle-gdb

Commit all local changes, tag, and push to GitHub. Then trigger the
`build-deb.yml` workflow to build a `.deb` and upload it to the release.

## Steps

1. Commit pending changes with `git add -u` (tracked files only) plus the
   `.mrc` directory. Do NOT use `git add -A` — the source tree contains
   build artifacts that must not be committed.
2. Tag the commit with `MULLE_GDB_TAG`.
3. Push the branch and tag to `github` remote.
4. Create the GitHub release if it doesn't exist yet.
5. Trigger `build-deb.yml` workflow with the tag.
6. Wait for the workflow to complete and verify the `.deb` appears on the release.
