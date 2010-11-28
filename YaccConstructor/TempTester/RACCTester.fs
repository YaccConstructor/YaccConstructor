//  RACCTester.fs
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
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


module Yard.Testers._RACCTester

open Yard.Generators._RACCGenerator
open Yard.Core
open System.Configuration

type _RACCTester() =
    class
        interface ITester with
            member self.BuildUserApplication testPath testName = 
                let cll = 
                    "fsc -o:_RACCUserApplication.dll --debug:pdbonly --noframework --define:TRACE "
                    + "--doc:bin\\Release\\RACCUserApplication.XML --optimize+ -r:\"%FS%\\FSharp.Core.dll\" "
                    + "-r:\"%FS_POWERPACK%\\bin\\FSharp.PowerPack.Compatibility.dll\" -r:\"%FS_POWERPACK%\\bin\\FSharp.PowerPack.dll\" "
                    + "-r:\"%NET_4_0%\\mscorlib.dll\" -r:\"%NET_4_0%\\System.Core.dll\" -r:\"%NET_4_0%\\System.dll\" "
                    + "-r:\"%NET_4_0%\\System.Numerics.dll\" -r:..\\_RACCCommon\\bin\\Debug\\_RACCCommon.dll "
                    + "-r:..\\_RACCCore\\bin\\Debug\\_RACCCore.dll -r:..\\RACCFiniteAutomata\\bin\\Debug\\_RACCFiniteAutomata.dll "
                    + "--target:dll --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors "
                    + "\"C:\\Users\\gsv\\AppData\\Local\\Temp\\.NETFramework,Version=v4.0.AssemblyAttributes.fs\" "
                    + testPath + "\\" + testName + ".tables.fs Lexer.fs Driver.fs"
                CSuccess 
            member self.RunTests inputFiles = 
                [{
                    inputFile = "Test"
                    status    = TSuccess
                    result    = Some "res \n mk"
                 }]
    end

do ConfigurationManager.AppSettings.["testsPath"].ToString() |> m1.main |> ignore