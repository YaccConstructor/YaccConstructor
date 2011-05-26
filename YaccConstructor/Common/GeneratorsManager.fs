//  Copyright 2010-2011 by Konstantin Ulitin
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

module Yard.Core.GeneratorsManager

let private generatorsCollection: ResizeArray<IGenerator> = 
    new ResizeArray<IGenerator>(ComponentsLoader.LoadComponents(typeof<IGenerator>) |> Seq.map (fun x -> x :?> IGenerator))

let AvailableGenerators = Seq.map (fun (x:IGenerator) -> x.Name) generatorsCollection
let Generator name = generatorsCollection.Find (function gen -> gen.Name = name)
let Register (gen) = generatorsCollection.Add (gen)


