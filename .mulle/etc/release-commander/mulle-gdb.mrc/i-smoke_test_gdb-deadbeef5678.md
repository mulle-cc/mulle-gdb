# Smoke test mulle-gdb: breakpoint, step-in, super call, step-out

## Prerequisites

mulle-gdb must be built and installed locally from source first (the system
installed version may be outdated). The script builds it with
`configure-mulle-gdb` + `make` + `make install` to a temp prefix
(`/tmp/mulle-gdb-smoketest-install`), because the in-tree `gdb/gdb` binary
can crash due to `getcwd` returning NULL in some environments.

## Environment

- `MULLE_GDB_BINARY` — override path to mulle-gdb binary
  (default: builds from source and installs to temp prefix)

## Test

Creates a fresh mulle-objc executable project in `/tmp/mulle-gdb-smoketest`,
builds it, then runs mulle-gdb in batch mode to verify:

1. Breakpoint in `main` at an ObjC method call line hits correctly
2. `step` into `-[Foo compute:]` works
3. `next` to the `[super compute:]` line, then `step` into `-[Base compute:]`
4. Backtrace shows `Base compute:` → `Foo compute:` → `main`
5. `finish` returns correct values
6. Program completes with `result: 43`

If the test fails, check canary offsets or runtime layout changes.
See `mulle-objc-runtime-canary.md` in the mulle-gdb source root for guidance.
