# Adapting mulle-gdb to New mulle-objc-runtime Versions

This document describes how to adapt mulle-gdb to work with new versions of the mulle-objc-runtime.

## Overview

mulle-gdb needs to know the memory layout of runtime structures to properly debug mulle-objc code. When the runtime version changes, these offsets may change and need to be updated in the debugger.

## Test Repository

A test repository is available at https://github.com/mulle-cc/test-gdb for testing debugger functionality.

## Key Runtime Structures

The debugger needs to understand the layout of:

1. **`_mulle_objc_class`** - Class structure with fields like `isa`, `super_class`, `name`, `allocation_size`, `infra_class`, `methods`, etc.
2. **`_mulle_objc_classpair`** - Contains offsets to `infraclass`, `metaclass`, and `protocolclasses`
3. **`_mulle_objc_method`** - Method structure with `sel`, `name`, `types`, `imp`

## Runtime Offset Table

The debugger maintains a table of offsets in `gdb/objc-lang.c` around line 1468:

```c
static struct gdb_objc_runtime_version_arch_offsets  runtime_offsets[] =
{
   /* version 0.27 no-TAO */
   { { 0, 27 }, 0, { 16, 480, 800 }, { 8, 248, 412 } },
   /* version 0.27 with TAO */
   { { 0, 27 }, 1, { 32, 512, 832 }, { 16, 272, 440 } }
};
```

Format: `{ { major, minor }, tao, { b64_offsets }, { b32_offsets } }`

Where offsets are: `{ infraclass, metaclass, protocolclasses }`

## Steps to Add Support for a New Runtime Version

### 1. Run Canary Tests

The runtime provides canary tests to determine the correct offsets. These must be run for all combinations:
- 64-bit NO-TAO
- 64-bit TAO
- 32-bit NO-TAO  
- 32-bit TAO

#### 64-bit Tests

```bash
cd /path/to/mulle-objc-runtime

# NO-TAO test
mulle-sde test run test-compiler-runtime/gdb/canary.m

# TAO test
mulle-sde test run test-compiler-runtime/gdb/canary-tao.m
```

#### 32-bit Tests

**Important**: Unset `MULLE_USER_PWD` before running 32-bit tests:

```bash
unset MULLE_USER_PWD

# Build 32-bit dependencies
cd /path/to/mulle-objc-runtime/test-compiler-runtime
mulle-sde -DCFLAGS=-m32 clean all
mulle-sde -DCFLAGS=-m32 craft

# NO-TAO test
mulle-sde -DMULLE_ARCH=i686 -DCFLAGS=-m32 test run gdb/canary.m

# TAO test  
mulle-sde -DMULLE_ARCH=i686 -DCFLAGS=-m32 test run gdb/canary-tao.m
```

The tests output offsets like:
```
pair.infraclass      = 32
pair.metaclass       = 512
pair.protocolclasses = 832
```

### 2. Update Offset Table

Add entries to the `runtime_offsets` array in `gdb/objc-lang.c`:

```c
/* version X.YZ no-TAO */
{ { X, YZ }, 0, { b64_infra, b64_meta, b64_proto }, { b32_infra, b32_meta, b32_proto } },
/* version X.YZ with TAO */
{ { X, YZ }, 1, { b64_infra, b64_meta, b64_proto }, { b32_infra, b32_meta, b32_proto } }
```

### 3. Fix Structure Reading Issues

#### allocation_size Field

The `allocation_size` field in `_mulle_objc_class` is a `long` (8 bytes on 64-bit), not a pointer. It must be read with explicit size:

```c
theclass->allocation_size = read_memory_unsigned_integer( addr, 8, byte_order);
addr += 8;
```

**Not** with pointer size (`len`), or the subsequent fields will be misaligned.

#### Metaclass Detection

To determine if a class is a metaclass or infraclass, check if `infra_class` points to itself:

```c
if (p_class->infra_class && p_class->infra_class != addr)  // is meta
{
   // This is a metaclass - infra_class points to the infraclass
   is_meta   = 1;
   infraAddr = p_class->infra_class;
   metaAddr  = addr;
}
else
{
   // This is an infraclass - infra_class points to itself (or is NULL)
   is_meta   = 0;
   infraAddr = addr;
   // ...
}
```

### 4. Update Method Call Function Table

The debugger intercepts calls to runtime functions to resolve method implementations. The function table is in `gdb/objc-lang.c` around line 2436:

```c
static struct objc_methcall methcalls[] = {
  { "mulle_objc_object_call", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_super", resolve_msgsend_super, 0, 0},
  { "mulle_objc_global_lookup_infraclass_nofail", resolve_msgsend_lookup, 0, 0 },
  // ... with and without underscore prefixes
};
```

**Important**: Check the actual function names in the binary with:
```bash
nm /path/to/executable | grep "objc_object_call"
```

Add entries for both with and without underscore prefixes.

### 5. Test with Runtime Tracing

Use runtime tracing to verify the debugger is finding methods correctly:

```bash
MULLE_OBJC_TRACE_METHOD_SEARCH=YES \
MULLE_OBJC_TRACE_METHOD_CALL=YES \
gdb /path/to/test-program
```

Compare the runtime's method search with what the debugger finds.

## Common Issues

### Protocol Classes Not Found

**Symptom**: Methods in protocol classes (like `+alloc` in MulleObjCRootObject) are not found.

**Cause**: The offset table is missing or incorrect for the current runtime version.

**Solution**: Run canary tests and update the offset table.

### Wrong Class Being Searched

**Symptom**: Debugger searches wrong class (e.g., Animal instead of Dog).

**Cause**: Stepping on wrong source line.

**Solution**: Use correct number of `n` (next) commands to reach the desired line before `s` (step).

### Cannot Access Memory Errors

**Symptom**: "Cannot access memory at address 0x..."

**Cause**: Structure offsets are wrong, causing misaligned field reads.

**Solution**: 
1. Verify `allocation_size` is read with 8 bytes, not pointer size
2. Check canary test offsets match the offset table
3. Add debug output to see what addresses are being read

### Step Into Goes to Runtime Function

**Symptom**: Step into goes to `mulle_objc_object_call` instead of the actual method.

**Cause**: Method resolution failed, so debugger falls back to stepping into the C function.

**Solution**:
1. Enable `DEBUG_VERBOSE` in `gdb/objc-lang.c` (line 55)
2. Rebuild and check debug output for why method lookup failed
3. Common causes:
   - Missing offset table entry
   - Wrong metaclass detection
   - Protocol classes not being searched

## Debugging the Debugger

### Enable Verbose Debug Output

Uncomment in `gdb/objc-lang.c`:
```c
#define DEBUG_VERBOSE 1
```

This prints detailed information about:
- Class structure reading
- Method searches
- Offset calculations
- Protocol class lookups

### Check Runtime Version Detection

The debugger reads the runtime version from the universe structure. Verify it's detecting correctly:

```gdb
(gdb) p/x mulle_objc_runtime_version(gdbarch)
```

Should match the actual runtime version (e.g., `0x001b00` for 0.27).

### Verify Offset Calculation

Check that `gdb_runtime_offset_arch()` returns non-NULL:
- If NULL, the version/TAO combination is not in the offset table
- Add debug output to see what version/TAO/bits it's looking for

## Method Call Resolution Functions

### resolve_msgsend
Handles regular method calls: `[object method]`
- Reads object's ISA to get class
- Searches class hierarchy for method
- Returns method implementation address

### resolve_msgsend_super  
Handles super calls: `[super method]`
- Reads superid parameter (argument 3)
- Looks up super info in universe's super table
- Searches from superclass, skipping current class

### resolve_msgsend_class
Handles class method calls with explicit class parameter
- Reads class address from argument 3
- Searches metaclass hierarchy

### resolve_msgsend_lookup
Handles class lookup functions: `mulle_objc_global_lookup_infraclass_nofail`
- Returns -1 to skip to caller
- Allows stepping into the actual method call that follows

## File Locations

- **Offset table**: `gdb/objc-lang.c` ~line 1468
- **Method call table**: `gdb/objc-lang.c` ~line 2436  
- **Structure reading**: `gdb/objc-lang.c` ~line 1600 (`read_objc_class`)
- **Canary tests**: `mulle-objc-runtime/test-compiler-runtime/gdb/canary*.m`

## Building and Installing

```bash
cd /path/to/mulle-gdb
make -j 8
sudo make install

# Or use directly without installing:
/path/to/mulle-gdb/gdb/gdb /path/to/executable
```

## Testing Checklist

- [ ] Step into class method (`+alloc`)
- [ ] Step into instance method (`-init`)  
- [ ] Step into super call (`[super method]`)
- [ ] Step over runtime functions (not into `mulle_objc_object_call`)
- [ ] Protocol class methods are found (`+alloc` from MulleObjCRootObject)
- [ ] Works with both 32-bit and 64-bit executables
- [ ] Works with both TAO and NO-TAO builds

## Version History

### 0.27 (Current)
- Added support for runtime version 0.27
- Fixed `allocation_size` field reading (8 bytes, not pointer size)
- Fixed metaclass detection (`infra_class != addr`)
- Added `mulle_objc_object_call_super` to method call table
- Added `resolve_msgsend_lookup` for class lookup functions

### 0.20
- Initial support for TAO (Thread-Aware Objects)
- Separate offset tables for TAO and NO-TAO builds

---

## Debugging Methodology: How to Steer the Debugger

This section explains the systematic approach used to diagnose and fix debugger issues.

### 1. Compare Runtime Behavior with Debugger Behavior

**Goal**: Understand what the runtime does vs. what the debugger does.

**Technique**: Run the program with runtime tracing enabled:

```bash
MULLE_OBJC_TRACE_METHOD_SEARCH=YES \
MULLE_OBJC_TRACE_METHOD_CALL=YES \
./your-program
```

This shows:
- Which classes are searched
- In what order
- Where methods are found
- What the actual method addresses are

**Example Output**:
```
start search for methodid ab1bb16b "alloc" in metaclass b6dbda94 "Dog"
search metaclass b6dbda94 "Dog" (0x8) 0x5b1bdc764d40
found in metaclass "Dog" methodid ab1bb16b ( "alloc")"
```

Compare this with what the debugger finds by enabling `DEBUG_VERBOSE`.

### 2. Enable Debugger Verbose Output

**Goal**: See what the debugger is actually doing internally.

**Steps**:
1. Edit `gdb/objc-lang.c` line 55:
   ```c
   #define DEBUG_VERBOSE 1
   ```
2. Rebuild: `make -j 8`
3. Run with debugger - output goes to stderr

**What to Look For**:
- `void read_objc_class() :: name = ClassName` - Which classes are being read
- `CORE_ADDR find_implementation_from_class() :: could not find method` - Method lookup failures
- `protocolclassesAddr=(nil)` - Protocol classes not being found
- `is meta` vs `is infra` - Metaclass detection

### 3. Use Batch Mode for Reproducible Testing

**Goal**: Quickly test specific scenarios without manual interaction.

**Technique**:
```bash
gdb -batch \
  -ex "b main" \
  -ex "run" \
  -ex "n" \
  -ex "n" \
  -ex "s" \
  -ex "where" \
  /path/to/executable 2>&1 | grep "pattern"
```

This allows:
- Automated testing of step-in behavior
- Filtering output to relevant information
- Quick iteration on fixes

### 4. Inspect Memory Directly

**Goal**: Verify structure layouts and offsets are correct.

**Technique**: Use gdb to dump raw memory:

```bash
gdb -batch \
  -ex "b main" \
  -ex "run" \
  -ex "n" \
  -ex "p/x *(long*)0xADDRESS@10" \
  /path/to/executable
```

**Example**: Checking a class structure:
```
$1 = {0x5555556c0b68,    # ISA
      0x5555556d8bf0,    # super_class  
      0x555555655031,    # name
      0x160,             # allocation_size (352 bytes)
      0x5555556d8fe0,    # infra_class
      0x5555556c04c0,    # universe
      ...}
```

Count the offsets to verify field positions match expectations.

### 5. Check Symbol Names in Binary

**Goal**: Verify function names match what the debugger is looking for.

**Technique**:
```bash
nm /path/to/executable | grep "pattern"
```

**Example**: Finding super call functions:
```bash
nm executable | grep "call_super"
# Output:
# mulle_objc_object_call_super    <- Actual name
# _mulle_objc_object_call_super_needcache
```

If the debugger looks for `mulle_objc_object_supercall` but the binary has `mulle_objc_object_call_super`, they won't match!

### 6. Trace the Step-In Flow

**Goal**: Understand why step-in goes to wrong place.

**Flow**:
1. User presses `s` (step)
2. GDB calls `skip_trampoline()` in objc-lang.c
3. `skip_trampoline()` calls `find_objc_msgcall()` to check if we're in a message send
4. If yes, calls the appropriate resolver (`resolve_msgsend`, `resolve_msgsend_super`, etc.)
5. Resolver tries to find the actual method implementation
6. Returns the address to step into

**Debug Points**:
- Does `find_objc_msgcall()` find the function? (Check "match" messages)
- Does the resolver get called? (Check resolver debug output)
- Does the resolver find the method? (Check "found endpoint" or "did not find")
- If not found, why? (Check class searches, protocol class searches)

### 7. Isolate the Problem

**Goal**: Narrow down which component is failing.

**Technique**: Add debug output at each stage:

```c
#if DEBUG_VERBOSE
fprintf(stderr, "%s :: checkpoint 1 - value=%p\n", __PRETTY_FUNCTION__, (void*)value);
#endif
```

**Common Failure Points**:
- **Offset table lookup returns NULL**: Version not in table
- **Protocol classes address is NULL**: Wrong offset calculation
- **Method not found in class**: Wrong class being searched (metaclass vs infraclass)
- **Cannot access memory**: Structure fields misaligned due to wrong field sizes

### 8. Verify with Canary Tests

**Goal**: Ensure offset values are correct.

**Technique**:
1. Run canary test for the architecture/TAO combination
2. Compare output with offset table values
3. If they don't match, update the table

**Example**:
```bash
# Canary output:
pair.infraclass      = 32
pair.metaclass       = 512  
pair.protocolclasses = 832

# Offset table should have:
{ { 0, 27 }, 1, { 32, 512, 832 }, { ... } }
```

### 9. Test Incrementally

**Goal**: Verify each fix works before moving to the next issue.

**Approach**:
1. Fix one issue (e.g., add version to offset table)
2. Rebuild and test
3. Verify the specific issue is resolved
4. Move to next issue

**Don't**: Try to fix multiple issues at once - you won't know which fix worked.

### 10. Use Automated Test Scenarios

**Goal**: Quickly verify all functionality works.

**Test Cases**:
```bash
# Test 1: Step into class method
gdb -batch -ex "b main" -ex "run" -ex "n" -ex "s" -ex "where" ./test-gdb

# Test 2: Step into instance method  
gdb -batch -ex "b main" -ex "run" -ex "n" -ex "n" -ex "s" -ex "where" ./test-gdb

# Test 3: Step into super call
gdb -batch -ex "b main" -ex "run" -ex "n" -ex "n" -ex "n" -ex "s" -ex "s" -ex "s" -ex "where" ./test-gdb
```

Check the final location (`where`) to verify it stepped into the correct method.

### Common Diagnostic Patterns

#### Pattern 1: "could not find method in class hierarchy"

**Diagnosis Flow**:
1. Check which class is being searched (look for "name = ClassName")
2. Is it the right class? (metaclass for class methods, infraclass for instance methods)
3. Check "is meta" vs "is infra" detection
4. If wrong, check `infra_class` field value and metaclass detection logic

#### Pattern 2: "protocolclassesAddr=(nil)"

**Diagnosis Flow**:
1. Check if `gdb_runtime_offset_arch()` returns NULL
2. If yes, check version detection: `version=X.Y, bits=64, tao=1`
3. Check if that combination exists in offset table
4. If no, run canary tests and add entry

#### Pattern 3: "Cannot access memory at address 0x..."

**Diagnosis Flow**:
1. Add debug to show what address is being read
2. Dump memory at that address to see if it's valid
3. If invalid, work backwards to see where the address came from
4. Usually means a field was read with wrong size, causing misalignment
5. Check `allocation_size` is read with 8 bytes, not pointer size

#### Pattern 4: Steps into runtime function instead of method

**Diagnosis Flow**:
1. Check if function is in methcalls table
2. Use `nm` to verify actual function name in binary
3. Check if resolver is being called (look for resolver debug output)
4. If resolver called but returns 0, check why method lookup failed
5. If resolver not called, function name doesn't match - add correct name to table

### Key Insight: The Debugger is a Runtime Simulator

The debugger must simulate what the runtime does:
1. **Runtime**: Walks class hierarchy, searches method lists, finds implementation
2. **Debugger**: Must do the same by reading memory structures

When they disagree, compare:
- Which classes each searches
- In what order
- What offsets they use
- What they find

The runtime is always correct - make the debugger match it.

