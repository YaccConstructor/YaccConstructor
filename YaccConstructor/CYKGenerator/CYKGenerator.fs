//  CYKGenerator.fs
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.CYKGenerator

open Yard.Core
open System.Collections.Generic
open Yard.Core.IL

type CYKGeneartorImpl () =
    // Now we are not support action code. So skip it.
    let grammarFromIL (il:Yard.Core.IL.Definition.t<_,_>) =
        let ntermDict = new Dictionary<_,_>()
        let termDict = new Dictionary<_,_>()
        let lblDict = new Dictionary<_,_>()
        let processRule (r:Rule.t<_,_>) =
            let name = r.name
            let body = r.body
            match body with
            | Production.PSeq ([t],_) -> ""
            | Production.PSeq ([nt1;nt2],_) -> ""
            | _ -> failwith "CYK. Incorrect rule structure. Must be in CNF"
            
        il.grammar |> List.map processRule
        
    let print rule = 
        let rName, r1, r2, lblName, lblWeight = getRule rule
        [rName]

    static member Generate grammar = ()

type CYKGenerator() =    
    inherit Generator()
        override this.Name = "CYKGenerator"
        override this.Generate t = CYKGeneartorImpl.Generate t |> box
        override this.AcceptableProductionTypes = ["seq"]