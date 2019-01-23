@echo on
cls

nuget restore YaccConstructor.sln

"packages/FSharp.Compiler.Tools.10.0.2/tools/fsiAnyCpu.exe" "src/YardFrontend/gen.fsx"

msbuild YaccConstructor.sln
