HOpenCL
=======

Haskell OpenCL Wrapper API

Overview
--------

HOpenCL is a Haskell wrapper for Open Compute Lanugage (OpenCL)
developed by Khronos (www.khronos.org/opencl).

Currently OpenCL 1.1 is supported but you can expect 1.2 support to be
added in the near future.

The wrapper is divided into three sub-components:

* OpenCL Language.OpenCL.Host.Core, which is a direct mapping of the OpenCL API into Haskell.
* OpenCL Language.OpenCL.Host, which is a high-level API built on top of the core components.
* OpenCL Language.OpenCL.Host.GLInterop, which adds OpenCL and OpenGL interopobility to the core components.

Installation
------------

To build the package you require [GHC](http://www.haskell.org/ghc/) 7.4 or later.

To compile the package an OpenCL SDK must be installed. There are a number of different choices here:

* Apple Mountain Lion, install XCode 4.4.
* Windows and Linux plaforms you can install
   AMD's [APP SDK](http://developer.amd.com/tools/hc/AMDAPPSDK/Pages/default.aspx),
   Intel's [OpenCL SDK](http://software.intel.com/en-us/articles/intel-opencl-sdk/),
   or NVIDIA's [Cuda SDK](http://developer.nvidia.com/cuda/cuda-downloads).

Optional Requisties
-------------------

OpenCL interoperability with OpenGL requires the Haskell library
[OpenGL](http://hackage.haskell.org/package/OpenGL).

Additionally, the OpenCL interoperability with OpenGL examples require
the Haskell package [GLUT](http://hackage.haskell.org/package/GLUT).

Building
---------

To build, try:

cabal install

in the root HOpenCL directory, which should track down any dependencies, configure, and build.  It
should install into local directories (%APPDATA%\Roaming\cabal, on Windows) so you probably don't
need to be admin.

EXAMPLES

The examples have been split into their own CABAL package.  To build them, try:

cd examples
cabal install

This will install the examples into some local directory.. (%APPDATA%\Roaming\cabal\bin, on
Windows).  If you'd prefer not to have two copies of the binaries running around, you could try:

cd examples
cabal configure
cabal build

The examples expect to find various _kernels.cl files in the current directory, so you probably to
run them from the examples directory.

License
-------

Copyright ©2012 Advanced Micro Devices, Inc. All rights reserved.

*   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
*   Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
********************************************************************


Authors
-------

J. Garrett Morris
Portland State Univeristy

Benedict R. Gaster
Advanced Micro Devices

Contributions and improvements are always welcome.
