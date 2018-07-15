@echo off
cls

dotnet build YC.StaticAnalysis.sln
dotnet test ./tests/Common.Tests/Common.Tests.fsproj 
dotnet test ./tests/Common.AST.Tests/Common.AST.Tests.fsproj 
dotnet test ./tests/Conversions.Tests/Conversions.Tests.fsproj