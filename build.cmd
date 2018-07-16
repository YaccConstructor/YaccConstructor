@echo off
cls

mkdir tools
cd tools

dotnet new console
dotnet add tools.csproj package fslexyacc --package-directory .
xcopy fslexyacc\7.0.6\build fslexyacc

cd ../

dotnet build YC.StaticAnalysis.sln
dotnet test ./tests/Common.Tests/Common.Tests.fsproj 
dotnet test ./tests/Common.AST.Tests/Common.AST.Test.fsproj 
dotnet test ./tests/Conversions.Tests/Conversions.Tests.fsproj