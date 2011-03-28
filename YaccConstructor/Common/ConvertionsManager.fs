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

let private convertionsCollection: ResizeArray<IConvertion> = 
    new ResizeArray<IConvertion>(ComponentsLoader.LoadComponents(typeof<IConvertion>) |> Seq.map (fun x -> x :?> IConvertion))

let AvailableConvertions = Seq.map (fun (x:IConvertion) -> x.Name) convertionsCollection

let Convertion name = convertionsCollection.Find (fun convertion -> convertion.Name = name)

let ApplyConvertion (ilTree:Definition.t<Source.t,Source.t>) (conv:IConvertion) = 
    {   new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = conv.ConvertList ilTree.grammar
        and  foot = ilTree.foot
    }

