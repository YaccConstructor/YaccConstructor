[![Issue Stats](http://issuestats.com/github/YaccConstructor/YaccConstructor/badge/issue)](http://issuestats.com/github/YaccConstructor/YaccConstructor)
[![Issue Stats](http://issuestats.com/github/YaccConstructor/YaccConstructor/badge/pr)](http://issuestats.com/github/YaccConstructor/YaccConstructor)
![Repository Size](https://reposs.herokuapp.com/?path=YaccConstructor/YaccConstructor)
YaccConstructor 
===============

Platform for parser generators and other grammarware research and development. GLL, RNGLR, graph parsing algorithms, and many others are included.

Build status:

| Branch | .NET | Mono |
|--------|------|------|
| Master | [![Master build status](https://ci.appveyor.com/api/projects/status/s8myouu45sunv2xh/branch/master?svg=true)](https://ci.appveyor.com/project/gsvgit/yaccconstructor/branch/master)| [![Build Status](https://travis-ci.org/YaccConstructor/YaccConstructor.svg?branch=master)](https://travis-ci.org/YaccConstructor/YaccConstructor)|
| Dev | [![Dev build status](https://ci.appveyor.com/api/projects/status/s8myouu45sunv2xh/branch/dev?svg=true)](https://ci.appveyor.com/project/gsvgit/yaccconstructor/branch/dev)| [![Build Status](https://travis-ci.org/YaccConstructor/YaccConstructor.svg?branch=dev)](https://travis-ci.org/YaccConstructor/YaccConstructor) |
| GLL-FSA | [![GLL-FSA build status](https://ci.appveyor.com/api/projects/status/s8myouu45sunv2xh/branch/GLL-FSA?svg=true)](https://ci.appveyor.com/project/gsvgit/yaccconstructor/branch/GLL-FSA)| [![Build Status](https://travis-ci.org/YaccConstructor/YaccConstructor.svg?branch=gll_fsa)](https://travis-ci.org/YaccConstructor/YaccConstructor)|


Packages:

| Package | |
|-------- |------|
| YC.SDK  | [![NuGet Status](http://img.shields.io/nuget/v/YC.SDK.svg?style=flat)](https://www.nuget.org/packages/YC.SDK/) |
| YC.SDK.Runtime| [![NuGet Status](http://img.shields.io/nuget/v/YC.SDK.Runtime.svg?style=flat)](https://www.nuget.org/packages/YC.SDK.Runtime/) |




Develop
==============

* Read [code style](https://docs.google.com/document/d/1Ta21jY09Z_kDFcWCPmKdd_LxfzrDOSZ_D0b9yFeBoZg/edit?usp=sharing)
* We use [git lfs](https://git-lfs.github.com/), so install it first.
* Get sources. We use submodules, so use ``git clone --recursive`` 
* Run ``build.cmd`` to perform initial build.
* Use MS VS 2015 to develop. Main solution is ``YaccConstructor.sln``. You can create your own solutions.

Maintainer(s)
==============
* [@gsvgit](https://github.com/gsvgit)
