//  Copyright 2009-2011 Konstantin Ulitin <ulitin.k@gmail.com>
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

module Yard.Core.ConvertionsManager

open Yard.Core
open Yard.Core.IL

let private convertionsCollection: ResizeArray<Convertion> = 
    new ResizeArray<Convertion>(ComponentsLoader.LoadComponents(typeof<Convertion>) |> Seq.map (fun x -> x :?> Convertion))

let AvailableConvertions = Seq.map (fun (x:Convertion) -> x.Name) convertionsCollection

let Convertion name = convertionsCollection.Find (fun convertion -> convertion.Name = name)

let ApplyConvertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>)  = 
    {   new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = 
            match Array.toList (convNameWithParams.Split(' ')) with
            | name::[] -> (Convertion name).ConvertList ilTree.grammar
            | name::parameters -> (Convertion name).ConvertList (ilTree.grammar, String.concat " " parameters)
            | _ -> failwith "You need to specify convertion name"
        and  foot = ilTree.foot
    }

