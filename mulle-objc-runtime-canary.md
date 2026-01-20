# mulle-gdb Canary Test Guide

## Overview

When the [mulle-objc-runtime](https://github.com/mulle-objc/mulle-objc-runtime) changes its internal struct layouts, the GDB debugger needs to be updated with new offset values. The "canary test" detects these changes by printing the actual struct offsets from the compiled runtime.

## Location

Runtime canary tests: `mulle-objc/mulle-objc-runtime/test-compiler-runtime/gdb/`
Read the `test-compiler-runtime/gdb/README.md` to see how to run the test in
32bit and 64 bit mode.

## Quick Check Process

1. **Check current test results** (tests auto-run during runtime build):

   ```bash
   cd /home/src/srcO/mulle-objc/mulle-objc-runtime
   mulle-sde vibecoding on
   mulle-sde test clean all
   mulle-sde test craft
   mulle-sde test run test-compiler-runtime/gdb/canary-no-tao.m
   cat canary-no-tao.test.stdout   # 64-bit without TAO
   cat canary-tao.test.stdout      # 64-bit with TAO
   ```

2. **Compare against expected values**:
   ```bash
   cat canary.stdout.linux.x86_64  # Expected for 64-bit
   cat canary.stdout.linux.i686    # Expected for 32-bit
   ```

3. **Check GDB code** - offsets are in `gdb/objc-lang.c` around line 1468:
   ```c
   * pair.infraclass      = 16    // 64-bit values
   * pair.metaclass       = 480
   * pair.protocolclasses = 800
   * i686: magic offsets
   * pair.infraclass      = 8     // 32-bit values  
   * pair.metaclass       = 248
   * pair.protocolclasses = 412
   ```

4. **If values DON'T match**: Update the struct in `gdb/objc-lang.c`:
   ```c
   struct gdb_objc_runtime_offsets
   {
     int infraclass;
     int metaclass;
     int protocolclasses;
   };
   ```
   And update the initialization code that sets these values (search for where offsets are assigned).


## What These Offsets Do

These offsets allow GDB to navigate the `_mulle_objc_classpair` structure to:
- Jump from infraclass to metaclass: `metaclass_of_infraclass()`
- Find protocol classes array: `protocolclass_array_of_metaclass()`
- Enable protocol method lookups in the debugger

## Manual Test Run (if needed)

Run tests for different architectures:

```bash
cd /home/src/srcO/mulle-objc/mulle-objc-runtime/test-compiler-runtime/gdb

# 64-bit test
mulle-sde test clean all
mulle-sde test craft
mulle-sde test run test-compiler-runtime/gdb/canary-no-tao.m
mulle-sde test run test-compiler-runtime/gdb/canary-tao.m

# 32-bit test (requires gcc-multilib)
mulle-sde test clean all
mulle-sde test craft
mulle-sde -v -DMULLE_ARCH=i686 -DCFLAGS=-m32 test run test-compiler-runtime/gdb/canary-tao.m
mulle-sde -v -DMULLE_ARCH=i686 -DCFLAGS=-m32 test run test-compiler-runtime/gdb/canary-no-tao.m
```

## Files to Check/Update

1. Runtime test output: `test-compiler-runtime/gdb/canary-*.test.stdout`
2. GDB offset comments: `gdb/objc-lang.c:1689-1695`
3. GDB offset struct usage: Search for `gdb_objc_runtime_offsets` in `gdb/objc-lang.c`

## TAO Detection (Implementation Notes)

**Status:** ✅ Implemented (2026-01-19)

The debugger now automatically detects TAO (Thread-Aware Objects) at runtime and uses different offsets:

**64-bit offsets:**
- NO-TAO: infraclass=16, metaclass=480, protocolclasses=800
- TAO: infraclass=32, metaclass=512, protocolclasses=832

**32-bit offsets:**
- NO-TAO: infraclass=8, metaclass=248, protocolclasses=412
- TAO: infraclass=16, metaclass=272, protocolclasses=440

**How it works:**
1. Reads `loadbits` field from universe (at `universe + 2*pointer_size`)
2. Checks `MULLE_OBJC_UNIVERSE_HAVE_TAO_LOADS = 0x10` flag
3. Selects appropriate offsets from table in `get_version_arch_offsets()`

**Key functions:**
- `read_runtime_version_loadbits()` - reads version and loadbits from universe
- `mulle_objc_runtime_tao()` - detects TAO mode
- `get_version_arch_offsets(major, minor, bits, tao)` - returns correct offsets
