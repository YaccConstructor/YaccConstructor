// ITester.fs
//
// Copyright 2009-2010 Semen Grigorev
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

