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
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open System.IO

type CYKGeneartorImpl () =
    let header =
        [
         "namespace Yard.Generators.CYK"
         ; ""
         ; "open Yard.Core"
        ]
        |> List.map wordL |> aboveListL

    let tokenStreamEncoder = 
        wordL "let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = "
        @@-- (wordL "stream |> Seq.map (fun t -> getTag t.Tag)")
        

    let tokenTypes (termDict:Dictionary<string,_>) =
        ("type cykToken = "|> wordL)
        @@-- ([for kvp in termDict -> [wordL "|"; wordL kvp.Key] |> spaceListL] |> aboveListL)
            
    let getTokenTypeTag (termDict:Dictionary<string,_>) =
        ("let getTag token = "|> wordL)
        @@-- (("match token with "|> wordL)
             @@ ([for kvp in termDict -> ["|"; kvp.Key; "->"; string kvp.Value]|> List.map wordL |> spaceListL] |> aboveListL))

    let rulesArray rules = 
        ("let rules = "|> wordL)
        @@-- (([wordL "["; [for rule in rules -> string rule + "u" |> wordL] |> semiListL; wordL "]"]|>spaceListL)
             @@ (wordL "|> Array.ofList"))

    let layoutToStr = 
        StructuredFormat.Display.layout_to_string 
          {StructuredFormat.FormatOptions.Default with PrintWidth=80}

    let code rules termDict =
        [ header
         ; tokenTypes termDict
         ; getTokenTypeTag termDict
         ; rulesArray rules 
         ; tokenStreamEncoder]
        |> aboveListL
        |> layoutToStr

    // Now we are not support action code. So skip it.
    let grammarFromIL (il:Yard.Core.IL.Definition.t<_,_>) =
        let ntermDict = new Dictionary<_,_>()
        let termDict = new Dictionary<_,_>()
        let lblDict = new Dictionary<_,_>()
        let ntermNum = ref 1
        let termNum = ref 1
        let lblNum = ref 1
        let processLbl lbl =
            match lbl with
            | Some (l:Production.DLabel) ->
                let lblId =
                    if lblDict.ContainsKey l.label
                    then lblDict.[l.label] 
                    else
                        let id = !lblNum 
                        lblDict.Add(l.label,id)
                        incr lblNum
                        id
                match l.weight with
                | Some i -> lblId, int i
                | None -> lblId,0
            | None -> 0,0

        let ntermId name = 
            if ntermDict.ContainsKey name 
            then ntermDict.[name] 
            else
                let id = !ntermNum 
                ntermDict.Add(name,id)
                incr ntermNum
                id

        let processNtermElem (elem:Production.elem<_,_>) = 
            match elem.rule with
            | Production.PRef ((n,_),_) -> ntermId n                
            | _ -> failwith "CYK. Incorrect rule structure. Expected PRef."

        let processRule (r:Rule.t<_,_>) =
            let name = r.name
            let body = r.body            
            match body with
            | Production.PSeq ([elem],_,lbl) ->
                match elem.rule with
                | Production.PToken(n,_) -> 
                    let tId =
                        if termDict.ContainsKey n
                        then termDict.[n]
                        else
                            let id = !termNum
                            termDict.Add(n,id)
                            incr termNum
                            id
                    let lN,lW = processLbl lbl

                    buildRule (ntermId name) tId 0 lN lW

                | _ -> failwith "CYK. Incorrect rule structure. Expected PToken."
            | Production.PSeq ([elem1; elem2],_,lbl) -> 
                let lN,lW = processLbl lbl
                buildRule (ntermId name) (processNtermElem elem1) (processNtermElem elem2) lN lW

            | _ -> failwith "CYK. Incorrect rule structure. Must be in CNF"
            
        il.grammar |> List.map processRule
        , termDict
        
    let print rule = 
        let rName, r1, r2, lblName, lblWeight = getRule rule
        [rName]

    member x.Generate grammar = 
        let rules, termDict = grammarFromIL grammar
        code rules termDict
    member x.GenRulesList grammar = grammarFromIL grammar |> fst

type CYKGenerator() =    
    inherit Generator()
        override this.Name = "CYKGenerator"
        override this.Generate t = 
            let g = new CYKGeneartorImpl()
            let code = g.Generate t 
            File.WriteAllText(Path.Combine(Path.GetFullPath(t.info.fileName),"CYK.fs"),code)
            code|> box
        override this.AcceptableProductionTypes = ["seq"]