#!/usr/bin/env bash

mono ../../Bin/Release/v40/YC.FsLex.exe --unicode Lexer.fsl
mono ../../Bin/Release/v40/YC.FsYacc.exe --module Yard.Frontends.FsYaccFrontend.Parser --open Yard.Core Parser.fsy
