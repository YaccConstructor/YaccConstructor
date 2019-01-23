@echo off
cls

nuget restore YaccConstructor.sln

cd packages/FSharp.Compiler.Tools.10.0.2/tools/ 
fsi.exe "../../../src/YardFrontend/gen.fsx"

cd ../../../
msbuild YaccConstructor.sln
