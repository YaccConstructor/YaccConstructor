// ITester.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

open Yard.Core.IL

type TCompileStatus =
    | CSuccess
    | CFail of string

type TTestStatus =
    | TSuccess 
    | TFail of string
    | TNoIncluded

type TTestResult =
    {
        inputFile : string
        status    : TTestStatus
        result    : Option<string>
    }

type ITester = interface
    // BuidUserApplication testPath testName
    abstract BuildUserApplication : string -> string -> TCompileStatus
    abstract RunTests : List<string> -> List<TTestResult>
end

