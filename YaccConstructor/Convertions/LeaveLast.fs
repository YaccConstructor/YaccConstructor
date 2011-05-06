//  Module LeaveLast contains:
//  - function, which removes all rules with same name except last. It can be useful
//  for rule's overwriting when one file with gramamr includes other.
//
//  Copyright 2009-2011 Konstantin Ulitin
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

module LeaveLast

open Yard.Core
open Yard.Core.IL

open System.Collections.Generic

let leaveLast (ruleList: Rule.t<Source.t, Source.t> list) = 
    let findedRules = new HashSet<string>()
    List.rev ruleList |> List.filter (fun rule -> findedRules.Add(rule.name))
    

type LeaveLast() = 
    inherit Convertion()
        override this.Name = "LeaveLast"
        override this.ConvertList ruleList = leaveLast ruleList
        override this.EliminatedProductionTypes = [""]