ocaml-prelude
=============

Includes the functions you need, that INRIA didn't.

This is an alternative standard library for OCaml based on the
[Haskell Prelude](https://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html)
that is more comprehensive with respect to the basic data types.

All of the fundamental data types are re-exported "batteries included"
in the `Data` library.

## Building the Library

Use the `Makefile` to build the library:
```bash
$ make
```
To build each individual target you can do
```bash
$ make control
```
or
```bash
$ make data
```
