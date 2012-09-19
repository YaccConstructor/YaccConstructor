// Manager.fs contains common component's managers functionality
//
//  Copyright 2011, 2012 Alexander Deikin
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

module Yard.Core.Manager

type IComponent = interface
    abstract Name : string
end

type Manager<'T when 'T :> IComponent> () = 
    let collection = 
        new ResizeArray<'T>(ComponentsLoader.LoadComponents(typeof<'T>) |> Seq.cast)

    let available = ResizeArray.map (fun x -> (x:>IComponent).Name) collection
    let _component name = 
        collection |> ResizeArray.tryFind (function some_function -> some_function.Name = name) 
    member public self.Available = available
    member public self.Component name = _component name  