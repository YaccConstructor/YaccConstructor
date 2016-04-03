namespace Yard.Generators.RecDescGenerator

open Yard.Core
open Constraints
open Yard.Generators.Common
open IL
open System.IO
open System
open System.Collections.Generic

type GrammarInfo =
    {        
        rules: Dictionary<int, List<int * int> >
        terminals : Dictionary<string,int>
        nonTerminals : Dictionary<string,int>
        t : List<string>
        nT : List<string>
        startNt : int
    }

type RecDescGenerator() = 
    inherit Generator()

        override this.Name = "RecDescGenerator"
        override this.Constraints = [| noEbnf; noMeta; noLiterals; noInnerAlt; noAlt; inCNF|]
        override this.Generate g =      
            let grammarFromIL (il:Yard.Core.IL.Definition.t<_,_>) =
                let ntermDict = new Dictionary<_,_>()
                let termDict = new Dictionary<_,_>()
                let rulesDict = new Dictionary<int, List<int * int> >()
                let termList = new List<string>()
                let nonTermList = new List<string>()
                termDict.Add("EOF", 0)
                termList.Add("EOF")
                let ntermNum = ref 1
                let termNum = ref 1
                let startNterm = ref -1

                let ntermId (name:Source.t) = 
                    if ntermDict.ContainsKey name.text
                    then ntermDict.[name.text] 
                    else
                        let id = !ntermNum 
                        ntermDict.Add(name.text,id)
                        nonTermList.Add(name.text)
                        incr ntermNum
                        id

                let processNtermElem (elem:Production.elem<_,_>) = 
                    match elem.rule with
                    | Production.PRef (n,_) -> ntermId n
                    | _ -> failwith "RecDesc. Incorrect rule structure. Expected PRef."

                let processRule (r:Rule.t<_,_>) =
                    let name = r.name
                    let body = r.body        
                    if r.isStart && !startNterm = -1 then
                        startNterm := ntermId name
                    match body with
                    | Production.PSeq ([elem],_,lbl) ->
                        match elem.rule with
                        | Production.PToken n -> 
                            let tId =
                                if termDict.ContainsKey n.text
                                then termDict.[n.text]
                                else
                                    let id = !termNum
                                    termList.Add(n.text)
                                    termDict.Add(n.text,id)
                                    incr termNum
                                    id
                            // A -> a
                            let id = ntermId name
                            if not (rulesDict.ContainsKey(id)) then
                                rulesDict.Add(id, new List<int * int>())
                            rulesDict.[id].Add((tId, -1))
                            true
                        | _ -> failwith "RecDesc. Incorrect rule structure. Expected PToken."
                    | Production.PSeq ([elem1; elem2],_,lbl) -> 
                        // A -> BC
                        let id = ntermId name
                        if not (rulesDict.ContainsKey(id)) then
                            rulesDict.Add(id, new List<int * int>())
                        rulesDict.[id].Add((processNtermElem elem1), (processNtermElem elem2))
                        true

                    | _ -> failwith "RecDesc. Incorrect rule structure. Must be in CNF"
                il.grammar.[0].rules |> List.map processRule |> ignore
                {
                    rules = rulesDict
                    terminals = termDict
                    nonTerminals = ntermDict
                    t = termList
                    nT = nonTermList
                    startNt = !startNterm
                }
            let grammar = grammarFromIL g
            let termList = new List<string * string>()
            let output = Path.GetFullPath g.info.fileName + ".RecDesc.fs"
            let third (_, _, z) = z
            let header =
                [
                 "module RecursiveDescentParsers"; 
                 "open System.Collections.Generic";
                 "";
                 "let rulesDict = new Dictionary<string, List<string * string> >()";
                 "let rollback = new List<string * int * int>()";    // array containing rollback points(nT, index, rule number)
                 "";
                 "let fst (x, _, _) = x";
                 "let snd (_, y, _) = y";
                 "let third (_, _, z) = z";
                 "";
                 "let word = new List<string>()";
                 "let functions = new Dictionary<_, _>()"
                ]  
            //template for function for each nonterminal          
            let funTemplate = 
                "let {0} index = " + "\n" +
                "   let list = rulesDict.[\"{1}\"]" + "\n" +
                "   let toBreak = ref false" + "\n" +
                "   let ruleNum = ref -1" + "\n" +
                "   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = \"{0}\" then" + "\n" +
                "       ruleNum := third(rollback.[rollback.Count - 1])" + "\n" +
                "       index := snd(rollback.[rollback.Count - 1])" + "\n" +
                "       rollback.Remove(rollback.[rollback.Count - 1]) |> ignore" + "\n" +
                "   let ind = ref 0" + "\n" +
                "   ind := !index" + "\n" +
                "   for i in (max !ruleNum 0) .. list.Count - 1 do" + "\n" +
                "       let B, C = list.[i]" + "\n" +                                   // getting rule in CNF
                "       if not (!toBreak) then" + "\n" +
                "           ind := !index" + "\n" +
                "           if C = \"$\" then" + "\n" +
                "               if !ruleNum = -1 then" + "\n" +                          // if rule: A -> a
                "                   if !ind < word.Count && word.[!ind] = B then" + "\n" + 
                "                       if i <> list.Count - 1 then" + "\n" + 
                "                           rollback.Add((\"{0}\", !ind, i))" + "\n" + 
                "                       ind := !ind + 1" + "\n" + 
                "                       toBreak := true" + "\n" +
                "                   else" + "\n" + 
                "                       ind := -1" + "\n" +                             // terminal does not pass, try another rule 
                "           else" + "\n" +                                              // if rule: A -> BC
                "               if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = C then" + "\n" +
                "                   ind := snd(rollback.[rollback.Count - 1])" + "\n" + 
                "                   let fC = functions.[C]" + "\n" + 
                "                   let C_ind = ref(fC ind)" + "\n" + 
                "                   if !C_ind <> -1 then" + "\n" + 
                "                       toBreak := true" + "\n" + 
                "               if not (!toBreak) then" + "\n" +
                "                   if rollback.Count > 0 && fst(rollback.[rollback.Count - 1]) = B then" + "\n" +
                "                       ind := snd(rollback.[rollback.Count - 1])" + "\n" + 
                "                   let fB = functions.[B]" + "\n" + 
                "                   let B_ind = ref(fB ind)" + "\n" + 
                "                   if !B_ind = -1 then" + "\n" + 
                "                       ind := -1" + "\n" +                                 // B does not pass, try another rule   
                "                   else" + "\n" +    
                "                       let fC = functions.[C]" + "\n" + 
                "                       let C_ind = ref(fC B_ind)" + "\n" + 
                "                       if !C_ind = -1 then" + "\n" + 
                "                           while(not(rollback.Count = 0 || fst(rollback.[rollback.Count - 1]) <> B || !C_ind <> -1)) do" + "\n" + 
                "                               let new_ind = ref(snd(rollback.[rollback.Count - 1]))" + "\n" +
                "                               B_ind := fB new_ind" + "\n" +
                "                               if !B_ind <> -1 then" + "\n" + 
                "                                   C_ind := fC B_ind" + "\n" +    
                "                       ind := !C_ind" + "\n" +       
                "                       if !C_ind <> -1 then" + "\n" +                      // C finally passed 
                "                           if i <> list.Count - 1 then" + "\n" + 
                "                               rollback.Add((\"{0}\", !ind, i))" + "\n" + 
                "                           toBreak := true" + "\n" +
                "   !ind" + "\n" + 
                "functions.[\"{0}\"] <- {0}" + "\n\n\n" 
            let startFun = 
                "let start argv = " + "\n" +
                "   let i = ref 0" + "\n" +
                "   word.AddRange(argv)" + "\n" +
                "   let index = {0} i" + "\n" +
                "   if index <> -1 then" + "\n" +
                "       eprintfn \"true\"" + "\n" +
                "   else" + "\n" +
                "       eprintfn \"false\"" + "\n"
            use out = new System.IO.StreamWriter (output)
            // placing header
            out.Write(String.concat "\n" header)
            out.WriteLine()
            //placing terminals and nonterminals
            let placeSet name x = 
                out.Write("let " + name + " = Set.ofList [")
                let content = String.concat ";" x
                out.Write(content.Substring(0, content.Length) + "]\n")
            
            placeSet "terminals" [ for x in grammar.t do yield "\"" + x + "\""]
            placeSet "nonTerminals"  [ for x in grammar.nT do yield "\"" + x + "\""]
            out.WriteLine()
            out.WriteLine()
            //placing rules
            for nt in grammar.rules.Keys do
                out.WriteLine(String.Format("rulesDict.Add(\"{0}\", new List<string * string>())", grammar.nT.[nt - 1]))
                for a, b in grammar.rules.[nt] do
                    let s = String.Format("rulesDict.[\"{0}\"].Add((\"{1}\"), (\"{2}\"))\n", grammar.nT.[nt - 1], 
                    (if b <> -1 then grammar.nT.[a - 1] else grammar.t.[a]), (if b <> -1 then grammar.nT.[b - 1] else "$"))
                    out.Write(s)
            out.WriteLine()
            out.WriteLine()
            // generating functions

            for nt in grammar.nT do
                let s = String.Format(funTemplate, nt, nt, nt)
                out.Write((string)(s))

            out.Write(String.Format(startFun, grammar.nT.[grammar.startNt - 1]))

            out.Flush()
            out.Close()
            eprintfn "Finished"
            box ()
         //override this.Generate definition = this.Generate (definition, "")
//abcd
//
//A -> BC
//B -> 0:a | 1:DE
//C -> 0:d | 1:FG
//D -> a
//E -> b
//F -> c
//G -> d
//
//ind = 1
//rb: (B, 2, 1), (F, )
//
//A: BC
//B: a, 0
//C: -1
//A: B()
//B: DE, 0
//D: a, 0
//E: b, 1
//B: DE, 2
//C: FG, 2
//F: c, 3
//G: d, 4

