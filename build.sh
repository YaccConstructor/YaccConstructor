#!/usr/bin/env bash

dotnet build src/YC.FsYacc/YC.FsYacc.fsproj
dotnet build src/YaccConstructor/YaccConstructor.fsproj
dotnet build YaccConstructor.sln
dotnet test YaccConstructor.Tests.sln