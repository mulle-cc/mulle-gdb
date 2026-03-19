#!/bin/bash

set -e

cd /home/src/srcO/mulle-objc/mulle-objc-runtime

mulle-sde test run test-compiler-runtime/gdb/canary-no-tao.m
mulle-sde test run test-compiler-runtime/gdb/canary-tao.m

echo "Canary tests passed - offsets match"
