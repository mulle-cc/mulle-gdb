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
