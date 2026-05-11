# Create Homebrew bottle for mulle-gdb

Triggers the GitHub Actions workflow `build-bottle.yml` to build a Homebrew
bottle on macOS.

## Prerequisites

The `mulle-gdb.rb` formula in the repo root MUST have `version` set to
`MULLE_GDB_TAG` before triggering. The bottle filename is derived from the
formula version — if it's wrong, the bottle gets the wrong version number.

The release script (previous step) should have already updated the formula.

## Verification

After the bottle is built, the script checks that the bottle filename
contains `MULLE_GDB_TAG`. If it doesn't, the step fails.
