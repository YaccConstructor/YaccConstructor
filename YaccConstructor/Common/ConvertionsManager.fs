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
            match Array.toList (convNameWithParams.Split(' ')) with
            | name::[] -> 
                match (this.Component name) with 
                | Some conv -> conv.ConvertList ilTree.grammar
                | None -> failwith "Convertion is not found" 
            
            | name::parameters -> 
                match (this.Component name) with 
                | Some conv -> conv.ConvertList (ilTree.grammar, String.concat " " parameters)
                | None -> failwith "Convertion is not found"
            | _ -> failwith "You need to specify convertion name"
        and  foot = ilTree.foot
      }
    member  self.ApplyConvertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>) = apply_convertion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>)
     