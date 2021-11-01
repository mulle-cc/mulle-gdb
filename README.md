# mulle-gdb

##### 🦗 gdb debugger for mulle-objc

This is the release version of [gdb](//sourceware.org/gdb/) 11.1 adapted to
work with [mulle-objc](//mulle-objc.github.io). Currently only x86_64 is
known to work.

The version tracks that of gdb, but extends it to 4 parts, where the last
digit is the mulle version. (e.g. 11.1 > 11.1.0.1)

Debugs code linked with [mulle-objc-runtime](//github.com/mulle-objc/mulle-objc-runtime) 0.20.

## Source

The majority of the changes are predictably in [gdb/objc-lang.c](). They are
not specifically marked. Changes outside of this file should be marked
'// @mulle-gdb' and therefore easy to find.


## Releasenotes

* (hardcoded) mulle-objc runtime functions do not appear in the stacktrace
* (hardcoded) mulle-objc runtime functions are skipped in step-in/step-out/finish

## Install

There are no `mulle-gdb` packages. You have to build and install `mulle-gdb`
manually.


### Manual installation

Prerequisite    | Comment
----------------|------------
autoconf        |
texinfo         |
flex            |
bison           |
build-essential | c++, make etc.


``` bash
MULLE_GDB_VERSION=11.1.0.1
./configure-mulle-gdb --prefix "/opt/mulle-gdb/${MULLE_GDB_VERSION}"
make -j 8
sudo make install
sudo ln -f -s "/opt/mulle-gdb/${MULLE_GDB_VERSION}/bin/mulle-gdb" \
              "/usr/local/bin/mulle-gdb"
```

You may want to change the options in the [configure-mulle-gdb](configure-mulle-gdb)
script to your liking. I prefer a minimal debugger without python.


### MacOS

Does it work on macOS ? Maybe. Beware though, that macOS is a consumer platform
and not a developer platform.

Here are some links to get `mulle-gdb` to run:

* [GDB Wiki PermissionsDarwin](//sourceware.org/gdb/wiki/PermissionsDarwin#Sign_and_entitle_the_gdb_binary)
* [Stackoverflow 1](//stackoverflow.com/questions/18423124/please-check-gdb-is-codesigned-see-taskgated8-how-to-get-gdb-installed-w)
* [Stackoverflow 2](//stackoverflow.com/questions/54416996/gdb-on-macos-mojave-10-14-2)

Hangs:

* [Getting gdb to (semi) reliably work](//timnash.co.uk/getting-gdb-to-semi-reliably-work-on-mojave-macos/)

> #### Note
>
> Even properly signed I need `sudo DevToolsSecurity -enable` to debug.
>