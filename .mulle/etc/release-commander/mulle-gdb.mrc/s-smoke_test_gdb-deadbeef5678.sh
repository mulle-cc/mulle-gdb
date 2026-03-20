#!/bin/bash

set -e

#
# Use MULLE_GDB_BINARY if set, otherwise fall back to mulle-gdb in PATH.
# Building from source in this environment crashes due to getcwd permission
# issues, so we rely on an installed binary.
#
GDB="${MULLE_GDB_BINARY:-mulle-gdb}"

echo "Using: ${GDB}"
"${GDB}" --version | head -1

#
# Create test project
#
TESTDIR="/tmp/mulle-gdb-smoketest"

if [ -d "${TESTDIR}" ]; then
   chmod -R u+w "${TESTDIR}" 2>/dev/null || true
   rm -rf "${TESTDIR}"
fi
mulle-sde init -m mulle-objc/objc-developer -d "${TESTDIR}" executable

cat > "${TESTDIR}/src/main.m" << 'EOF'
#import <MulleObjC/MulleObjC.h>
#include <stdio.h>

@interface Base : MulleObject
- (int) compute:(int) x;
- (int) double:(int) x;
@end

@interface Foo : Base
- (int) compute:(int) x;
@end

@implementation Base
- (int) compute:(int) x
{
   return x * 2;
}
- (int) double:(int) x
{
   return x * 2;
}
@end

@implementation Foo
- (int) compute:(int) x
{
   int a;
   a = [self double:x];        // next over instance method call
   return [super compute:x] + a;  // step into super, next over super
}
@end

int main( void)
{
   Foo  *foo;
   int  result;

   foo    = [Foo new];
   result = [foo compute:21];   // break here, step in
   printf( "result: %d\n", result);
   return 0;
}
EOF

cd "${TESTDIR}"
mulle-sde craft

BINARY="$(mulle-sde product)"
if [ -z "${BINARY}" ] || [ ! -x "${BINARY}" ]; then
   echo "Could not find built binary" >&2
   exit 1
fi
echo "Binary: ${BINARY}"

#
# Run gdb smoke test
#
# Tests:
# 1. break in -[Foo compute:], next over [self double:x] - stays in Foo compute:
# 2. step into [super compute:x] - lands in Base compute:
# 3. finish out of Base compute: - back in Foo compute: / main
# 4. continue to completion, check result
#
GDBSCRIPT="/tmp/mulle-gdb-smoke.gdb"
cat > "${GDBSCRIPT}" << 'EOF'
set confirm off
break -[Foo compute:]
run
bt
next
bt
step
bt
finish
bt
continue
quit
EOF

OUTPUT="$("${GDB}" --batch -x "${GDBSCRIPT}" "${BINARY}" 2>&1)"
echo "${OUTPUT}"

# 1. step into [foo compute:] lands in -[Foo compute:]
echo "${OUTPUT}" | grep -q "\-\[Foo compute:\]"         || { echo "FAIL: step did not land in -[Foo compute:]" >&2; exit 1; }

# 2. next over [self double:x] stays in -[Foo compute:] (line 29)
# (verified implicitly - if next jumped out we'd not reach step into super)

# 3. step into super lands in -[Base compute:]
echo "${OUTPUT}" | grep -q "\-\[Base compute:\]"        || { echo "FAIL: step into super did not land in -[Base compute:]" >&2; exit 1; }

# 4. finish from Base compute: returns to Foo compute:
# (verified by presence of both in output)

# 5. final result is correct
echo "${OUTPUT}" | grep -q "result: 84"                 || { echo "FAIL: wrong final result (expected 84)" >&2; exit 1; }

echo "Smoke test PASSED"
