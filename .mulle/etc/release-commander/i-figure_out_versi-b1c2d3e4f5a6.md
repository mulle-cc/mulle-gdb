# Figure out mulle-gdb version to release

## Steps

1. Read the base version from the source:
   ```
   /home/src/srcL/mulle-gdb-11.1/gdb/version.in
   ```
   This gives the major.minor (e.g. `11.1`). The full release tag is `<major.minor>.X.Y`
   where X.Y is a patch counter - check the latest release tag on GitHub to determine
   the next patch number.

2. Check existing releases on GitHub:
   ```bash
   gh release list --repo mulle-cc/mulle-gdb
   ```
   Find the latest `11.1.x.y` tag to determine the next version.

3. Check if there are any commits since the last `11.1` release:
   ```bash
   git -C /home/src/srcL/mulle-gdb-11.1 log --oneline <last-tag>..HEAD
   ```
   If no new commits → stop, nothing to release.

4. If new commits exist and no `.deb` for the new version → proceed.
   Update `MULLE_GDB_TAG` in `environmentVariables` in `db.json`.

5. Create the GitHub release:
   ```bash
   gh release create "${MULLE_GDB_TAG}" \
     --repo mulle-cc/mulle-gdb \
     --title "${MULLE_GDB_TAG}" \
     --notes "mulle-gdb ${MULLE_GDB_TAG}"
   ```
