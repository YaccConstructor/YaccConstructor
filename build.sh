#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet restore YaccConstructor.sln

mono ./packages/FSharp.Compiler.Tools.10.0.2/tools/fsi.exe ./src/YardFrontend/gen.fsx

msbuild YaccConstructor.sln
