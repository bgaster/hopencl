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
[OpenCL](http://hackage.haskell.org/package/OpenGL).

Additionally, the OpenCL interoperability with OpenGL examples require
the Haskell package [GLUT](http://hackage.haskell.org/package/GLUT).

Authors
-------

J. Garrett Morris
Portland State Univeristy

Benedict R. Gaster
Advanced Micro Devices

Contributions and improvements are always welcome.