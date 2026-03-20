#!/bin/bash

set -e

cd "${MULLE_OBJC_RUNTIME_DIR:-../mulle-objc/mulle-objc-runtime}"

mulle-sde test run test-compiler-runtime/gdb/canary-no-tao.m
mulle-sde test run test-compiler-runtime/gdb/canary-tao.m

echo "Canary tests passed - offsets match"
