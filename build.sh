#!/usr/bin/env bash

set -eu
set -o pipefail

mono ./packages/FSharp.Compiler.Tools.10.0.2/tools/fsi.exe --noframework ./src/YardFrontend/gen.fsx

msbuild YaccConstructor.sln
