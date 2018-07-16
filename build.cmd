@echo off
cls

mkdir tools
cd tools

dotnet new console
dotnet add tools.csproj package fslexyacc --package-directory .
xcopy fslexyacc\7.0.6\build fslexyacc

cd ../

dotnet build YC.StaticAnalysis.sln