// ComponentsLoader.fs contains helper functions for frontends/generators loading
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

module Yard.Core.ComponentsLoader

open System
open System.IO

let private loadByType (desiredType:Type) =
    let basePath = 
        AppDomain.CurrentDomain.BaseDirectory
    let assemblies =
        System.IO.Directory.GetFiles(basePath,"*.dll")
        |> Seq.map (fun x -> Path.GetFileNameWithoutExtension(x))
        |> Seq.choose (fun x -> try Reflection.Assembly.Load(x) |> Some with _ -> None)
   
    assemblies
    |> Seq.collect (fun assembly -> assembly.GetTypes())
    |> Seq.filter (fun _type -> desiredType.IsAssignableFrom(_type))

let private isRealClass (cls:Type) =
    not cls.IsAbstract
    && not cls.IsGenericTypeDefinition
    && not cls.IsInterface

let LoadComponents (desiredType:Type) =
    loadByType desiredType
    |> Seq.filter isRealClass
    |> Seq.map (fun x -> Activator.CreateInstance(x))
