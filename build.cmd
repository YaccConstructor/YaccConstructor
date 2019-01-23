@echo on
cls

nuget restore YaccConstructor.sln

start \w "/packages/FSharp.Compiler.Tools.10.0.2/tools/fsi.exe " "/src/YardFrontend/gen.fsx"

msbuild YaccConstructor.sln
