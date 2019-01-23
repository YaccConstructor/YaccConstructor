@echo on
cls

nuget restore YaccConstructor.sln

"packages/FSharp.Compiler.Tools.10.0.2/tools/fsi.exe" -r "packages/FSharp.Compiler.Tools.10.0.2/tools/FSharp.Core.dll" "src/YardFrontend/gen.fsx"

msbuild YaccConstructor.sln
