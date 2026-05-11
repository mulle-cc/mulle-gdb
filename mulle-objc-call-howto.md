# mulle-objc Call Resolution in GDB

This document explains how mulle-gdb resolves method calls for stepping into Objective-C methods.

## Regular Method Calls

For regular calls like `[Dog alloc]` or `[dog init]`:

1. GDB detects call to `mulle_objc_object_call` or similar runtime function
2. Fetches `obj` (receiver) and `sel` (method selector) from function arguments
3. Gets the class:
   - For instances: reads ISA from object header at `obj - 8`
   - For classes: `obj` IS the class
4. Searches method hierarchy starting from that class
5. Returns implementation address to step into

## Super Method Calls

For super calls like `[super alloc]` inside Dog's `+alloc`:

### The Challenge

The compiler generates: `mulle_objc_object_call_super(obj, methodid, parameter, superid)`

The `superid` identifies the super call site, but we need to:
- Find which class to start searching from
- Skip the current class (Dog) but search its protocol classes
- Handle both instance methods and class methods correctly

### The Solution

1. **Fetch superid** from arg 3
2. **Look up superid in universe's supertable** → gets `{classid, methodid}`
3. **Look up classid in classtable** → gets class address (used only for `startClassid`)
4. **Get receiver's ISA** - this is the key:
   ```c
   // ISA is always 1 pointer before obj, regardless of TAO mode
   CORE_ADDR isa_addr = obj - len;
   CORE_ADDR search_class = read_memory_unsigned_integer(isa_addr, len, byte_order);
   ```
5. **Search from ISA, skipping startClassid**:
   ```c
   find_implementation_from_class(gdbarch, search_class, methodid, -1, startClassid);
   ```

### Why Get ISA from Object Header?

For class methods like `+[Dog alloc]` calling `[super alloc]`:
- `obj` is Dog's **infraclass** (the class object itself)
- `obj`'s ISA is Dog's **metaclass** (where class methods live)
- We need to search the metaclass hierarchy, not the infraclass hierarchy

For instance methods like `-[Dog init]` calling `[super init]`:
- `obj` is a Dog **instance**
- `obj`'s ISA is Dog's **infraclass** (where instance methods live)
- We search the infraclass hierarchy

By always getting the ISA from the object header, we automatically get the right class to search from, regardless of whether it's a class method or instance method.

### Object Header Layout

**Without TAO (Thread-Aware Objects):**
```
obj - 16:  retaincount (8 bytes)
obj - 8:   ISA pointer (8 bytes)
obj:       object data starts here
```

**With TAO:**
```
obj - 32:  retaincount (8 bytes)
obj - 24:  thread (8 bytes)
obj - 16:  foundation (8 bytes)
obj - 8:   ISA pointer (8 bytes)
obj:       object data starts here
```

**ISA is always at `obj - 8` regardless of TAO mode.**

**Tagged pointers (TPS):**
- No object header - class info encoded in pointer itself
- Low bits contain class index: `index = obj & (len == 8 ? 0x7 : 0x3)`
- If index is 0, it's a regular object
- If index is non-zero, look up class in `universe->taggedpointers.pointerclass[index]`

The ISA resolution code checks the low bits first to determine if it's a tagged pointer or regular object, then gets the class accordingly.

### Method Search with startClassid

When `startClassid` is set (from the super table lookup):

1. Start at the ISA class (e.g., Dog's metaclass)
2. If current class matches `startClassid`, skip its **methods** but still search its **protocol classes**
3. Continue to superclass (Animal's metaclass)
4. Search methods and protocol classes
5. Continue up the hierarchy until method is found

This ensures we skip Dog's own `+alloc` but still search Dog's protocol classes, then search Animal and its protocol classes, eventually finding `+alloc` in MulleObjCRootObject.

## Key Insight

Don't try to determine if it's a class method or instance method by examining the class structure. Just get the ISA from the receiver's object header - the runtime has already set it up correctly for us.
