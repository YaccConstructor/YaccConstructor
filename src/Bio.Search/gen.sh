#!/usr/bin/env bash

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\16s\R16S_1_18.yrd -g "GLLGenerator -token unit -module GLL.R16S_1_18 -o R16S_1_18.fs" >> generationLog.txt
mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\16s\R16S_19_27.yrd -g "GLLGenerator -token unit -module GLL.R16S_19_27 -o R16S_19_27.fs" >> generationLog.txt