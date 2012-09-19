//  Statistics.fs 
//  contains functions for IL statistics calculation.
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


module Yard.Core.Statistics

open Yard.Core.IL.Production
open Yard.Reports

let RuleLength (def:Yard.Core.IL.Definition.t<_,_>) =
    let rules = def.grammar
    let rec length body =
        match body with
        | PRef _ 
        | PMetaRef _
        | PLiteral _ 
        | PToken _  -> 1 
        | PSeq (exprList,_,_) -> List.length exprList 
        | PPerm _    
        | PRepet _
        | PMany _
        | PSome _
        | POpt  _ -> printfn "Unsupported body in rule length calculation."; -1
        | PAlt (lExpr,rExpr) -> 
            max (length lExpr) (length rExpr) //hack for toplevel alternatives
         
    let lengths = 
        rules
        |> List.map (fun r -> length r.body)

    let avg =
        lengths
        |> List.filter ((<>)(-1))
        |> List.averageBy(float)
    
    let max = List.max lengths

    let distribution =
        lengths
        |> Seq.groupBy id
        |> Seq.map (fun (gn,gv) -> seq [string gn;Seq.length gv |> string])

    let report = new Report("grammarReport.txt")

    let reportBody =
        ["Average: " + string avg |> RStr
        ; "Max: " + string avg |> RStr
        ; Table distribution |> RTable]
    report.Body <- reportBody
    report.AsWiki()