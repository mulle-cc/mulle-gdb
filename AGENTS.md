# AGENTS.md - mulle-gdb

## Release workflows (GitHub Actions on mulle/16.3.0)

### build-deb.yml
Builds a `.deb` for Linux (ubuntu-latest). Triggered via `workflow_dispatch` with `tag` input (e.g. `16.3.0.1`).
- Checks out `mulle/16.3.0` branch
- Runs `./configure-mulle-gdb --prefix=/usr/local`
- Packages with `dpkg-deb` into `mulle-gdb-<TAG>-<DIST>-<ARCH>.deb`
- Uploads to the GitHub release
- Requires `libgmp-dev libmpfr-dev libncurses-dev texinfo` build deps

### build-bottle.yml
Builds a Homebrew bottle for macOS arm64 (macos-15). Triggered via `workflow_dispatch` with `tag` input.
- Taps `mulle-objc/software` locally, copies `mulle-gdb.rb` from repo root into `Formula/`
- Runs `brew install --formula --build-bottle`, then `brew bottle`
- Renames `mulle-gdb--<TAG>.arm64_sequoia.bottle.tar.gz` → single-dash
- Uploads to the GitHub release
- Key fix: pass `--with-system-zlib` to configure to avoid bundled zlib/fdopen macro conflict on macOS 15

## mulle-gdb.rb (Homebrew formula)
Lives in repo root on `mulle/16.3.0` and in `mulle-objc/homebrew-software`.
- Uses `./configure-mulle-gdb --prefix=#{prefix} --with-system-zlib`
- Must be run from source root (not a build subdir) since the script calls `./configure`
- After building a new bottle: update `bottle do` sha256 and push to both repos

## Release process (mulle-release-commander)

Release steps live in `.mulle/etc/release-commander/` on the `mulle/16.3.0` branch.
Each step is a pair of `i-<name>.md` (instructions) + `s-<name>.sh` (script).

`db.json` and `l-*.log` are gitignored - they are local state only.

### Starting a new release

1. Clone/pull `mulle-cc/mulle-gdb` on the `mulle/16.3.0` branch
2. Copy `.mulle/etc/release-commander/` to a local working dir, e.g.:
   ```
   cp -r .mulle/etc/release-commander/ ~/mulle-gdb-<NEW_TAG>.mrc/
   ```
3. Create `db.json` in that dir (copy from a previous release or build fresh):
   ```json
   {
     "files": [ ... ],
     "environmentVariables": {
       "MULLE_GDB_TAG": "<NEW_TAG>",
       "PWD": "/home/src/srcO"
     }
   }
   ```
4. Reset all step statuses to `"todo"`, update `MULLE_GDB_TAG`
5. Work through each step in order, updating status to `"done"` or `"failed"`

### Step order
1. Figure out version to release
2. Check GDB canary offsets against runtime
3. Smoke test mulle-gdb
4. Release mulle-gdb (trigger build-deb.yml + build-bottle.yml)
5. Create homebrew bottle (handled by build-bottle.yml)
6. Upload macOS bottle to release (handled by workflow)
7. Copy formula to homebrew-software (s-edit_homebrew_bo script)
