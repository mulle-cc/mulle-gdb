# Figure out mulle-gdb version to release

## Steps

1. Run `mulle-project-version get --custom-versioning` to see the current version.
2. Run `mulle-project-version status --custom-versioning` to check if a bump is needed.
3. If a bump is needed, increment the custom (fourth) part:
   ```bash
   mulle-project-version --increment-custom --custom-versioning --write
   ```
4. Verify the new version with `mulle-project-version get --custom-versioning`.
5. Sync `gdb/version.in` — this file is not managed by `mulle-project-version`
   and must be updated manually (the script does this).
6. Update `MULLE_GDB_TAG` in `db.json` to match the new version.
