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
open Constraints
open IL
open System.Collections.Generic
open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat
open System.IO
open Mono.Addins

type GrammarInfo =
    {        
        rules: uint64 array
        termDict : Dictionary<string,int>
        nTermDict : Dictionary<string,int>
        lblNames : string[]
    }

type CYKGeneartorImpl () =
    
    [<Literal>]
    let eof = "EOF"

    [<Literal>]
    let eofNum = 0

    let header =
        [
         "module Yard.Generators.CYK"
         ; ""
         ; "open Yard.Core"
         ; "open Yard.Generators.CYKGenerator"
        ]
        |> List.map wordL |> aboveListL

    let tokenStreamEncoder = 
        wordL "let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = "
        @@-- (wordL "stream"
              @@ ((wordL "|> Seq.choose (fun t ->"
                  @@-- ([wordL "let tag = getTag t.Tag"
                       ;"if tag <> " + string eofNum + "us then Some tag else None)" |> wordL]
                       |> aboveListL)))
              @@ (wordL "|> Array.ofSeq"))

    let tokenTypes (termDict:Dictionary<string,_>) =
        ("type cykToken = "|> wordL)
        @@-- ([for kvp in termDict -> [wordL "|"; wordL kvp.Key] |> spaceListL] |> aboveListL)
            
    let getTokenTypeTag (termDict:Dictionary<string,_>) =
        ("let getTag token = "|> wordL)
        @@-- (("match token with "|> wordL)
             @@ ([for kvp in termDict -> ["|"; kvp.Key; "->"; string kvp.Value + "us"]|> List.map wordL |> spaceListL] |> aboveListL))

    let rulesArray rules = 
        ("let rules = "|> wordL)
        @@-- (([wordL "[|"; [for rule in rules -> string rule + "UL" |> wordL] |> semiListL; wordL "|]"]|>spaceListL))

    let layoutToStr l =
        let res = ref ""
        let f () = res := print 80 l
        let thread = new System.Threading.Thread(f, 134217728)
        thread.Start()
        thread.Join()
        !res


    let genStartNTermID id =
        [wordL "let StartNTerm ="
        ; (string id |> wordL)]
        |> spaceListL
    
    let startNTerm (il:Yard.Core.IL.Definition.t<_,_>) (ntermDict:Dictionary<_,_>) =
        ntermDict.[(il.grammar.[0].rules |> List.find (fun r -> r.isStart)).name.text]
    
    let genlblArr (lblNames:string[])= 
        ("let lblName = " |> wordL)
        @@-- (("[|" |> wordL)
            @@ ([for lblInfo in lblNames -> ["\"" + lblInfo + "\";"]|> List.map wordL |> spaceListL] |> aboveListL))
            @@ ("|]" |> wordL)

    let code il grammarInfo =
        [
            header
            tokenTypes grammarInfo.termDict
            getTokenTypeTag grammarInfo.termDict
            rulesArray grammarInfo.rules
            genlblArr grammarInfo.lblNames
            startNTerm il grammarInfo.nTermDict |> genStartNTermID
            tokenStreamEncoder
        ]
        |> aboveListL
        |> layoutToStr    
    
    // Now we are not support action code. So skip it.
    let grammarFromIL (il:Yard.Core.IL.Definition.t<_,_>) =
        let ntermDict = new Dictionary<_,_>()
        let termDict = new Dictionary<_,_>()
        termDict.Add(eof,eofNum)
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

        let ntermId (name:Source.t) = 
            if ntermDict.ContainsKey name.text
            then ntermDict.[name.text] 
            else
                let id = !ntermNum 
                ntermDict.Add(name.text,id)
                incr ntermNum
                id

        let processNtermElem (elem:Production.elem<_,_>) = 
            match elem.rule with
            | Production.PRef (n,_) -> ntermId n
            | _ -> failwith "CYK. Incorrect rule structure. Expected PRef."

        let processRule (r:Rule.t<_,_>) =
            let name = r.name
            let body = r.body            
            match body with
            | Production.PSeq ([elem],_,lbl) ->
                match elem.rule with
                | Production.PToken n -> 
                    let tId =
                        if termDict.ContainsKey n.text
                        then termDict.[n.text]
                        else
                            let id = !termNum
                            termDict.Add(n.text,id)
                            incr termNum
                            id
                    let lN,lW = processLbl lbl

                    buildRule (ntermId name) tId 0 lN lW

                | x -> failwithf "CYK. Incorrect rule structure. Expected PToken, but get %A" x
            | Production.PSeq ([elem1; elem2],_,lbl) -> 
                let lN,lW = processLbl lbl
                buildRule (ntermId name) (processNtermElem elem1) (processNtermElem elem2) lN lW

            | _ -> failwith "CYK. Incorrect rule structure. Must be in CNF"
            
        {
            rules = il.grammar.[0].rules |> List.map processRule |> Array.ofList
            termDict = termDict
            nTermDict = ntermDict
            lblNames = Array.ofSeq (Dictionary.KeyCollection lblDict) 
        }
        
    let print rule = 
        let rName, r1, r2, lblName, lblWeight = getRuleCortege rule
        [rName]

    member x.Generate grammar = 
        let grammarInfo = grammarFromIL grammar
        code grammar grammarInfo
    member x.GenRulesList grammar = 
        let grammarInfo = grammarFromIL grammar
        grammarInfo.rules, startNTerm grammar grammarInfo.nTermDict, grammarInfo.lblNames

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type CYKGenerator() =    
    inherit Generator()
        override this.Name = "CYKGenerator"
        override this.Generate t = 
            let g = new CYKGeneartorImpl()
            let code = g.Generate t
            let fileName = Path.GetFileName t.info.fileName
            let fullName = Path.GetFullPath t.info.fileName + ".CYK.fs"
            fullName |> printfn "%s"
            if File.Exists fullName then File.Delete fullName
            (File.CreateText fullName).Close()
            File.WriteAllText(fullName, code)
            code|> box
        override this.Constraints = [|noMeta; noEbnf; noLiterals; noInnerAlt; noBrackets; inCNF; singleModule|]
