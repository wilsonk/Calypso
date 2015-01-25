Special Calypso notes
------------

Calypso creates a bridge between DMD/LDC and Clang, both at the AST level (DMD <=> Clang's AST, Sema, ...) and at the code generation level (LDC <=> Clang's Codegen) to make D interface directly with the almost full set of C++ features, and without having to write bindings:

> http://forum.dlang.org/thread/nsjafpymezlqdknmnkhi@forum.dlang.org

In order to build it you need a LLVM + Clang 3.5 source tree, built libraries and the Clang binaries. Installing binary packages from your distribution isn't enough since the include/ files aren't exposing many symbols, so the source packages are needed as well. Or if you want or need to build Clang yourself, make sure to get the 3.5 branch:

    $ svn co http://llvm.org/svn/llvm-project/llvm/branches/release_35 llvm
    $ cd llvm/tool
    $ svn co http://llvm.org/svn/llvm-project/cfe/branches/release_35 clang
    $ cd ../..
    $ cd llvm/projects
    $ svn co http://llvm.org/svn/llvm-project/compiler-rt/branches/release_35 compiler-rt

Then build and install Clang as described in the rest of:
http://clang.llvm.org/get_started.html

Finally tell CMake where to find the LLVM source tree:

    $ cd build
    $ cmake .. -DLLVM_SOURCE_PATH="/path/to/llvm/source/tree"
    
The rest of the build process is identical to LDC.

Specific flags and building the [showcase](tests/calypso/showcase.d) example
-------

Calypso adds the -cpp-flags option to LDC to pass arguments to Clang during the PCH generation, e.g to enable C++11 required to build [tests/calypso/showcase.d](tests/calypso/showcase.d):

    $ clang++ -std=c++11 -c showcase.cpp -o showcase.cpp.o
    $ ar rcs libshowcase.a showcase.cpp.o
    $ ldc2 -cpp-args -std=c++11 -Llibshowcase.a -L-lstdc++ showcase.d

LDC – the LLVM-based D Compiler
===============================

[![Build Status](https://travis-ci.org/ldc-developers/ldc.png?branch=master)](https://travis-ci.org/ldc-developers/ldc) [![Bountysource](https://www.bountysource.com/badge/tracker?tracker_id=283332)](https://www.bountysource.com/trackers/283332-ldc?utm_source=283332&utm_medium=shield&utm_campaign=TRACKER_BADGE)

The LDC project aims to provide a portable D programming language
compiler with modern optimization and code generation capabilities.

The compiler uses the official DMD frontends to support the latest
version of D2, and relies on the LLVM Core libraries for code
generation.

LDC is fully Open Source; the parts of the code not taken/adapted from
other projects are BSD-licensed (see the LICENSE file for details).

Please consult the D wiki for further information:
http://wiki.dlang.org/LDC

D1 is no longer available; see the 'd1' Git branch for the last
version supporting it.


Installation
------------

In-depth material on building and installing LDC and the standard
libraries, including experimental instructions for running LDC on
Windows, is available on the project wiki, at
http://wiki.dlang.org/Building_LDC_from_source.

If you have a working C++ build environment, CMake, a current LLVM and
libconfig++ (http://hyperrealm.com/libconfig/libconfig.html)
available, there should be no big surprises, though.

Do not forget to make sure all the submodules are up to date:

    $ cd ldc
    $ git submodule update --init

Some Linux distributions are also packaging a recent version of LDC,
so building it manually might not be necessary.


Contact
-------

The best way to get in touch with the developers is either via the
digitalmars.D.ldc forum/newsgroup/mailing list
(http://forum.dlang.org) or the #ldc IRC channel on FreeNode.

For further documentation, contributor information, etc. please see
the D wiki: http://wiki.dlang.org/LDC

Feedback of any kind is very much appreciated!
