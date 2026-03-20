# Check GDB canary offsets against runtime

Before releasing mulle-gdb, verify that the struct offsets hardcoded in
`gdb/objc-lang.c` match the actual offsets from the compiled mulle-objc-runtime.

## Environment

- `MULLE_OBJC_RUNTIME_DIR` — path to mulle-objc-runtime checkout
  (default: `../mulle-objc/mulle-objc-runtime` relative to `PWD`)

## Steps

1. Run the canary tests in the runtime:
   ```bash
   cd "${MULLE_OBJC_RUNTIME_DIR}"
   mulle-sde test run test-compiler-runtime/gdb/canary-no-tao.m
   mulle-sde test run test-compiler-runtime/gdb/canary-tao.m
   ```

2. Compare output against expected values in `gdb/objc-lang.c` (around line 1689):
   - 64-bit NO-TAO: infraclass=16, metaclass=480, protocolclasses=800
   - 64-bit TAO:    infraclass=32, metaclass=512, protocolclasses=832
   - 32-bit NO-TAO: infraclass=8,  metaclass=248, protocolclasses=412

3. If values differ → update the offset table in `gdb/objc-lang.c` and rebuild.
   See `mulle-objc-runtime-canary.md` in the mulle-gdb source root for details.

4. If values match → proceed.
