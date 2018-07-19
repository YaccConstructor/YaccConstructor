#!/usr/bin/env bash

dotnet restore YaccConstructor.sln --configfile Nuget.Config
dotnet build src/YC.FsYacc/YC.FsYacc.fsproj -c Release
dotnet build src/YaccConstructor/YaccConstructor.fsproj -c Release
dotnet build YaccConstructor.sln -c Release
dotnet test YaccConstructor.Tests.sln -c Release
