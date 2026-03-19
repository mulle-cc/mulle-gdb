# Smoke test mulle-gdb: breakpoint, step-in, super call, step-out

Automated test. Creates a fresh mulle-objc executable project in `/tmp/mulle-gdb-smoketest`,
builds it, then runs mulle-gdb in batch mode to verify:

1. Breakpoint in `main` at an ObjC method call line hits correctly
2. `step` into `-[Foo compute:]` works
3. `next` to the `[super compute:]` line, then `step` into `-[Base compute:]`
4. Backtrace at that point shows `Base compute:` → `Foo compute:` → `main`
5. `finish` out of `Base compute:` returns 42, lands back in `Foo compute:`
6. `finish` out of `Foo compute:` returns 43, lands back in `main`
7. Program completes with `result: 43`

If the test fails, check canary offsets or runtime layout changes.
See `mulle-objc-runtime-adapt.md` in the mulle-gdb source for guidance.
