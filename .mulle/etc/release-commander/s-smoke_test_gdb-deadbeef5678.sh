#!/bin/bash

set -e

TESTDIR="/tmp/mulle-gdb-smoketest"

# clean up stale project if present (mulle-sde sets some dirs read-only)
if [ -d "${TESTDIR}" ]; then
   chmod -R u+w "${TESTDIR}" 2>/dev/null || true
   rm -rf "${TESTDIR}"
fi
mulle-sde init -m mulle-objc/objc-developer -d "${TESTDIR}" executable

# write a simple ObjC test program
cat > "${TESTDIR}/src/main.m" << 'EOF'
#import <MulleObjC/MulleObjC.h>
#include <stdio.h>

@interface Base : MulleObject
- (int) compute:(int) x;
@end

@interface Foo : Base
- (int) compute:(int) x;
@end

@implementation Base
- (int) compute:(int) x
{
   return x * 2;
}
@end

@implementation Foo
- (int) compute:(int) x
{
   return [super compute:x] + 1;
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

# build with debug info
cd "${TESTDIR}"
mulle-sde craft

BINARY="$(find "${HOME}/.mulle/var/cache/sde" -path "*/kitchen/Debug/mulle-gdb-smoketest" -not -type d 2>/dev/null | head -1)"
if [ -z "${BINARY}" ]; then
   echo "Could not find built binary" >&2
   exit 1
fi

# write gdb batch script
GDBSCRIPT="/tmp/mulle-gdb-smoke.gdb"
cat > "${GDBSCRIPT}" << 'EOF'
set confirm off
# break on the method call line in main
break main.m:32
run
bt
# step into -[Foo compute:]
step
bt
# step to the [super compute:] line
next
# step into -[Base compute:]
step
bt
# step out of Base compute: back to Foo compute:
finish
bt
# step out of Foo compute: back to main
finish
bt
continue
quit
EOF

# run mulle-gdb in batch mode
OUTPUT="$(mulle-gdb --batch -x "${GDBSCRIPT}" "${BINARY}" 2>&1)"
echo "${OUTPUT}"

# verify key indicators
echo "${OUTPUT}" | grep -q "\-\[Foo compute:\]"  || { echo "FAIL: Foo compute: not in backtrace" >&2; exit 1; }
echo "${OUTPUT}" | grep -q "\-\[Base compute:\]" || { echo "FAIL: Base compute: not in backtrace after step into super" >&2; exit 1; }
echo "${OUTPUT}" | grep -q "Value returned is.*42" || { echo "FAIL: Base compute: didn't return 42" >&2; exit 1; }
echo "${OUTPUT}" | grep -q "Value returned is.*43" || { echo "FAIL: Foo compute: didn't return 43" >&2; exit 1; }
echo "${OUTPUT}" | grep -q "result: 43"           || { echo "FAIL: wrong final result (expected 43)" >&2; exit 1; }

echo "Smoke test PASSED"
