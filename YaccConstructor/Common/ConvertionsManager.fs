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

//Не понял еще сути этой функции, так что просто сделал что-бы работало
type ConvertionsManager () as this =
    inherit Yard.Core.Manager.Manager<Convertion>()
    let apply_convertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>)  = 
      {new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar =
                  let parameters = convNameWithParams.Split(' ')
                  //printfn "Convertion: %s" convNameWithParams
                  if parameters.Length = 0 then failwith "Missing convertion name"
                  else
                      match this.Component parameters.[0] with 
                      | Some conv -> conv.ConvertList (ilTree.grammar, Array.sub parameters 1 (parameters.Length - 1))
                      | None -> failwith <| "Convertion not found: " + parameters.[0]
        and  foot = ilTree.foot
        and options = ilTree.options
      }
    member  self.ApplyConvertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>) = apply_convertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>)
     