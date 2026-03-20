# Create Homebrew bottle for mulle-gdb

Triggers the GitHub Actions workflow `build-bottle.yml` on `mulle-cc/mulle-gdb`
(master branch) to build Homebrew bottles on macOS.

The workflow runs two matrix jobs:
- `macos-15` (Apple Silicon / arm64_sequoia)
- `macos-15-large` (Intel / sequoia)

Each job:
1. Checks out master (which contains `mulle-gdb.rb`)
2. Taps `mulle-objc/software` locally and copies the formula in
3. Runs `brew install --formula --build-bottle`
4. Runs `brew bottle` to produce the `.bottle.tar.gz`
5. Uploads the bottle to the GitHub release as an asset

After the workflow completes:
- Download the bottles and run `shasum -a 256` on them (or read from `brew bottle` output in the logs)
- Update the `bottle do` block in `mulle-gdb.rb` on master
- Copy the updated formula to `mulle-objc/homebrew-software` (step handled by `s-edit_homebrew_bo-2e5kdn0w6vf.sh`)

Note: On macOS, gdb requires codesigning to access Mach ports. The bottle
will work for debugging but users will need to codesign `mulle-gdb` themselves.
See: https://sourceware.org/gdb/wiki/PermissionsDarwin
