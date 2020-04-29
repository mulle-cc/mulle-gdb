# mulle-gdb

##### 🦗 gdb debugger for mulle-objc

This is a development version of [gdb](//sourceware.org/gdb/) 10.0 adapted to
work with mulle-objc. Currently only x86_64 is supported.

Requires mulle-objc-runtime v0.18.


## Install

### Manual installation

Prerequisites   |
----------------|------------
autoconf        |
texinfo         |
flex            |
bison           |
build-essential | c++, make etc.


```
./configure-mulle-gdb --prefix /opt/mulle-gdb
make -j 8
make install
sudo ln -s /opt/mulle-gdb/bin/mulle-gdb /usr/local/bin/mulle-gdb
```

You may want to change the options to your liking. I prefer a minimal
debugger without python.
