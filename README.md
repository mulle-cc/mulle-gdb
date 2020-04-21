# mulle-gdb

##### 🦗 gdb debugger for mulle-objc

This is a development version of [gdb](//sourceware.org/gdb/) 10.0 adapted to
work with mulle-objc. Currently only x86_64 is supported.

Requires mulle-objc-runtime v0.18.


## Install

### Manual installation


```
./configure-mulle-gdb    # --prefix /usr/local
make -j 8
make install
```

You may want to change the options to your liking. I prefer a minimal
debugger without python.
