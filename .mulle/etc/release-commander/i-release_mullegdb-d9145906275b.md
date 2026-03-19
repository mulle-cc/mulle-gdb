# Release mulle-gdb

Triggers the GitHub Actions workflow `build-deb.yml` on `mulle-cc/mulle-gdb`
to build a `.deb` package and upload it to the release page.

The workflow:
1. Checks out the `mulle/<MAJOR.MINOR>.0` branch
2. Builds mulle-gdb with `configure-mulle-gdb` + `make`
3. Packages with `dpkg-deb` into `mulle-gdb-<TAG>-<DIST>-<ARCH>.deb`
4. Uploads to the GitHub release as an asset

The script waits for the workflow to complete and verifies the `.deb` is present.
