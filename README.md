# mulle-gdb

##### 🦗 gdb debugger for mulle-objc

This is the release version of [gdb](//sourceware.org/gdb/) 10.1 adapted to
work with mulle-objc. Currently only x86_64 is supported.

The version tracks that of gdb, but extends it to 4 parts, where the last
digit is the mulle version. (e.g. 10.1 > 10.1.0.0)

Requires mulle-objc-runtime 0.18, already supports 0.19 currently in
development.

## Releasenotes

* hardcoded so that runtime functions do not appear in the stacktrace
* hardcoded so that runtime functions are skipped in step-in/step-out/finish

## Install

There are no mulle-gdb packages. You have to build and install it manually.

### Manual installation

Prerequisite    | Comment
----------------|------------
autoconf        |
texinfo         |
flex            |
bison           |
build-essential | c++, make etc.


``` bash
MULLE_GDB_VERSION=10.1.0.1
./configure-mulle-gdb --prefix "/opt/mulle-gdb/${MULLE_GDB_VERSION}"
make -j 8
sudo make install
sudo ln -s "/opt/mulle-gdb/${MULLE_GDB_VERSION}/bin/mulle-gdb" \
           "/usr/local/bin/mulle-gdb"
```

You may want to change the options in the [configure-mulle-gdb](configure-mulle-gdb)
script to your liking. I prefer a minimal debugger without python.
