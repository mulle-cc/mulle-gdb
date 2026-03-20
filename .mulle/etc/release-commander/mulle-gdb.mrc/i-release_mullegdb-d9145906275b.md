# Release mulle-gdb

Commit all local changes, tag, and push to GitHub. Then trigger the
`build-deb.yml` workflow to build a `.deb` and upload it to the release.

## Steps

1. Commit all pending changes (including updated version files and .mrc changes).
2. Tag the commit with `MULLE_GDB_TAG`.
3. Push the branch and tag to `github` remote.
4. The push of the tag should create the GitHub release (or create it manually).
5. Trigger `build-deb.yml` workflow with the tag.
6. Wait for the workflow to complete and verify the `.deb` appears on the release.
