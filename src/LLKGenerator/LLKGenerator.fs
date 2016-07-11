namespace Yard.Generators.LLKGenerator

open Yard.Core
open Constraints
open IL
open System.Collections.Generic
open System.Collections
open System.IO
open LanguagePrimitives

open System
open System.Linq
open Mono.Addins

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Struct>]
type Symbol =
    val Name : string
    val IsTerminal : bool
    val isEpsilon : bool
    
    new (name: string, isTerminal : bool) =
        { Name = name; IsTerminal = isTerminal; isEpsilon = (name.Equals("{ None }"))}


type Chain(chain : System.Collections.Generic.List<int>) =
    member this.Chain = chain
    member this.isEpsilonChain = chain.Count = 0

    override x.GetHashCode() = 
        let mutable hashCode = 0
        for elem in x.Chain do
            hashCode <- elem.GetHashCode() + hashCode
        hashCode

    override x.Equals(yobj) =
        match yobj with
        | :? Chain as y -> x.Chain.SequenceEqual(y.Chain)            
        | _ -> false
    
    override x.ToString() =
        x.Chain.Aggregate("", fun acc el -> acc + el.ToString() + "$")



[<Struct>]
type Rule = 
    val RuleName : int 
    val R1 : int
    val R2 : int

    new (ruleName : int, r1 : int, r2 : int) =
        { RuleName = ruleName; R1 = r1; R2 = r2}

type GrammarInfo =
    {        
        rules: List<Rule>
        termDict : Dictionary<string, int>
        nTermDict : Dictionary<string, int>
        start: int
    }


type LLKGeneratorImpl () = 
    
    let epsilonChain = new Chain(new System.Collections.Generic.List<int>([]))

    let startNTerm (il:Yard.Core.IL.Definition.t<_,_>) (ntermDict:Dictionary<_,_>) =
        ntermDict.[(il.grammar.[0].rules |> List.find (fun r -> r.isStart)).name.text]

    let ILtoInternal (il:Yard.Core.IL.Definition.t<_,_>) =
        let nTermDict = new Dictionary<string, int>()
        let termDict = new Dictionary<string, int>()
        let num = ref 1

        let ntermId (name:Source.t) =
            if nTermDict.ContainsKey name.text
            then nTermDict.[name.text]
            else let id = !num
                 nTermDict.Add(name.text, id)
                 incr num
                 id

        let processNtermElem (elem:Production.elem<_,_>) = 
            match elem.rule with
            | Production.PRef (n,_) -> ntermId n
            | _ -> failwith "LLK. Incorrect rule structure. Expected PRef."




        let buildRule ruleName r1 r2 =
            new Rule(ruleName, r1, r2)

        let processRule (r:Rule.t<_,_>) =
            let name = r.name
            let body = r.body            
            match body with
            | Production.PSeq ([elem],_,lbl) ->
                match elem.rule with
                | Production.PToken n ->
                    let tId = if termDict.ContainsKey n.text
                              then termDict.[n.text]
                              else 
                                  let id = !num
                                  termDict.Add(n.text, id)
                                  incr num
                                  id
                    buildRule (ntermId name) tId 0

                | _ -> failwith "LLK. Incorrect rule structure. Expected PToken."
            | Production.PSeq ([elem1; elem2],_,lbl) -> 
                buildRule (ntermId name) (processNtermElem elem1) (processNtermElem elem2)
            | Production.PSeq([], _, _) -> 
                buildRule (ntermId name) 0 0
            | _ -> failwith "LLK. Incorrect rule structure. Must be in CNF"
          
        {
            rules = new List<Rule>(il.grammar.[0].rules |> List.map processRule)
            termDict = termDict
            nTermDict = nTermDict
            start = startNTerm il nTermDict
        }

    let createMutableList(ienumerable : IEnumerable<int>) =
        new System.Collections.Generic.List<int>(ienumerable)
            
    let trunc (c1 : Chain, c2: Chain, k : int) =
        let c3 = createMutableList([])
        for elem in c1.Chain do
            c3.Add(elem)
        for elem in c2.Chain do
            c3.Add(elem)
        new Chain(c3.GetRange(0, (min k c3.Count)))

    let calcFirst (grammarInfo:GrammarInfo, k: int) =
        let nt, t = grammarInfo.nTermDict, grammarInfo.termDict
        let first = new Dictionary<int, HashSet<Chain>>()

        for k in t.Values do
            first.Add(k, new HashSet<Chain>()) |> ignore
            first.[k].Add(new Chain(createMutableList([k]))) |> ignore

        for k in nt.Values do
            first.Add(k, new HashSet<Chain>())

        let mutable changed = true
        while changed do
            changed <- false
            for rule in grammarInfo.rules do
   
                let a, r1, r2 = rule.RuleName, rule.R1, rule.R2
                let sizeBefore = first.[a].Count
                let first_r1 = new HashSet<Chain>(first.[r1])
                if r1 = 0 then first.[a].Add(epsilonChain) |> ignore
                else if r2 = 0 
                then 
                    for i in first_r1 do 
                        first.[a].Add(i) |> ignore
                else let first_r2 = new HashSet<Chain>(first.[r2])
                     if first_r1.Count = 0 
                     then for i in first_r2 do
                            first.[a].Add(i) |> ignore
                     if first_r2.Count = 0 
                     then for i in first_r1 do
                            first.[a].Add(i) |> ignore
                     for i in first_r1 do
                        for j in first_r2 do
                            first.[a].Add(trunc(i, j, k)) |> ignore
                let sizeAfter = first.[a].Count
                changed <- changed || (sizeBefore < sizeAfter)
        first

    let calcFollow(grammarInfo:GrammarInfo, first: Dictionary<int, HashSet<Chain>>, k: int) = 
        let nt, t, s = grammarInfo.nTermDict, grammarInfo.termDict, grammarInfo.start
        let follow = new Dictionary<int, HashSet<Chain>>()
        for notTerm in nt.Values do
            follow.Add(notTerm, new HashSet<Chain>())
            if notTerm = grammarInfo.start
                then follow.[notTerm].Add(epsilonChain) |> ignore
        let mutable changed = true
        while changed do
            changed <- false
            for rule in grammarInfo.rules do
                let a, r1, r2 = rule.RuleName, rule.R1, rule.R2
                let f = new HashSet<Chain>(follow.[a])
                let beforSize = follow.Values.Aggregate(0, fun ac el -> ac + el.Count)
                if r1 = 0 then ()
                else if r2 = 0
                then ()
                else 
                    follow.[r2].UnionWith(f)
                    let f2 = new HashSet<Chain>()
                    if first.[r2].Count = 0
                    then for j in f do
                            f2.Add(j) |> ignore
                    if f.Count = 0
                    then for i in first.[r2] do
                            f2.Add(i) |> ignore
                    for i in first.[r2] do
                        for j in f do
                            f2.Add(trunc(i, j, k)) |> ignore
                    follow.[r1].UnionWith(f2)
                    let afterSize = follow.Values.Aggregate(0, fun ac el -> ac + el.Count)
                    changed <- changed || (beforSize < afterSize)
        follow
    let calcLookAhead(grammarInfo: GrammarInfo, first:  Dictionary<int, HashSet<Chain>>, follow:  Dictionary<int, HashSet<Chain>>, k: int) =
        let lookAhead = new Dictionary<int, HashSet<Chain>>()
        let nt = grammarInfo.nTermDict
        for notTerm in nt.Values do
            lookAhead.Add(notTerm, new HashSet<Chain>())
            if follow.[notTerm].Count = 0
            then for i in first.[notTerm] do
                    lookAhead.[notTerm].Add(i) |> ignore
            if first.[notTerm].Count = 0 
            then for j in follow.[notTerm] do
                    lookAhead.[notTerm].Add(j) |> ignore
            for i in first.[notTerm] do
                for j in follow.[notTerm] do
                    lookAhead.[notTerm].Add(trunc(i, j, k)) |> ignore          
        lookAhead
    let calcLookAheadForRules(grammarInfo: GrammarInfo, first:  Dictionary<int, HashSet<Chain>>, follow:  Dictionary<int, HashSet<Chain>>, k: int) = 
        let lookAhead = new Dictionary<int, HashSet<Chain>>() 
        let rules = grammarInfo.rules
        let ruleNum = ref 1
        for rule in rules do
            lookAhead.Add(!ruleNum, new HashSet<Chain>())
            let a, r1, r2 = rule.RuleName, rule.R1, rule.R2
            if r2 = 0
            then for i in first.[r1] do
                    if follow.[a].Count = 0
                    then
                        lookAhead.[!ruleNum].UnionWith(first.[r1])
                    else
                        for j in follow.[a] do
                            lookAhead.[!ruleNum].Add(trunc(i, j, k)) |> ignore
            else
                for i in first.[r1] do
                    for j in first.[r2] do
                        if follow.[a].Count = 0 
                        then
                            lookAhead.[!ruleNum].Add(trunc(i, j, k)) |> ignore 
                        else
                            for f in follow.[a] do
                                lookAhead.[!ruleNum].Add(trunc(trunc(i, j, k), f, k)) |> ignore
            incr ruleNum
        lookAhead  


    let enumerateChains(chains: HashSet<Chain>) =
        let num = ref 0
        let enumeratedChains = new Dictionary<Chain, int>()
        for chain in chains do
            enumeratedChains.Add(chain, !num)
            incr num
        enumeratedChains
    let buildTable(lookAheadNT: Dictionary<int, HashSet<Chain>>, lookAheadRules: Dictionary<int, HashSet<Chain>>, enumeratedChains: Dictionary<Chain, int>, grammarInfo: GrammarInfo, t: int) =
        let table = Array2D.zeroCreate (lookAheadNT.Count + t + 1) (enumeratedChains.Count + 1)
        for kv in lookAheadNT do
            let nt, nTermLa = kv.Key, kv.Value
            for kv in lookAheadRules do
                let rule, ruleLa = kv.Key, kv.Value
                let ruleName =  grammarInfo.rules.[rule - 1].RuleName
                if ruleName = nt
                then    let intersection = new HashSet<Chain>((new HashSet<Chain>(nTermLa)).Intersect(ruleLa))
                        if intersection.Count > 0
                        then 
                            for i in intersection do
                                if table.[nt, enumeratedChains.[i]] = 0
                                then table.[nt, enumeratedChains.[i]] <- rule
                                else raise (System.ArgumentException("It's not Strong LLk grammar"))
        table        

    member x.Generate grammar k output = 
        let grammarInfo = ILtoInternal grammar
        let output = Path.GetFullPath output
        let first = calcFirst(grammarInfo, k)
//        Console.WriteLine("first")
//        for k in first.Keys do
//            Console.WriteLine()
//            Console.WriteLine(k)
//            Console.WriteLine("----")
//            for el in first.[k] do
//                Console.WriteLine(el.ToString())
         
        let follow = calcFollow(grammarInfo, first, k)
//        Console.WriteLine("follow")
//        for k in follow.Keys do
//            Console.WriteLine()
//            Console.WriteLine(k)
//            Console.WriteLine("----")
//            for el in follow.[k] do
//                Console.WriteLine(el.ToString())
        let lookAhead = calcLookAhead(grammarInfo, first, follow, k)
        let lookAheadForRules = calcLookAheadForRules(grammarInfo, first, follow, k)
        let chains = new HashSet<Chain>()
        for v in lookAhead.Values do
            for c in v do
                chains.Add(c) |> ignore
        for v in lookAheadForRules.Values do
            for c in v do
                chains.Add(c) |> ignore

//        Console.WriteLine("lookAhead")
//        for kv in lookAhead do
//            let k, v = kv.Key, kv.Value
//            Console.WriteLine()
//            Console.WriteLine(k)
//            Console.WriteLine("---")
//            for v in v do
//                Console.WriteLine(v.ToString())


//        Console.WriteLine("lookAheadForRules")
//        for kv in lookAheadForRules do
//            let k, v = kv.Key, kv.Value
//            Console.WriteLine()
//            Console.WriteLine(k)
//            Console.WriteLine("---")
//            for v in v do
//                Console.WriteLine(v.ToString())
        let enumeratedChains = enumerateChains(chains)
        let table = buildTable(lookAhead, lookAheadForRules, enumeratedChains, grammarInfo, grammarInfo.termDict.Count)

        let header =
            "module LLk.test\n"+
            "open System.Collections.Generic\n" +
            "open Yard.Generators.LLK.Parser\n" +
            "open System\n"
            
        let tokenToNumber = 
             "let tokenToNumber = function \n"
             +
             grammarInfo.termDict.Aggregate("", 
                fun ac el -> ac + "    | \"" + el.Key.ToString() + "\" -> " + el.Value.ToString() + "\n"
             )
             +
             "    | _ -> failwith \"Unexpected token\"\n"
        
        let isTerminal =
            "let isTerminal = function \n"
            +
            grammarInfo.termDict.Aggregate("",
                fun ac el -> ac + "    | \"" + el.Key.ToString() + "\" -> true\n"
            )    
            +
            "    | _ -> false\n"

        let startRule = "let startNT = " + grammarInfo.start.ToString() + "\n"

        let _k = "let k = " + k.ToString() + "\n"
        let numIsTerminal =
            "let numIsTerminal = function\n" +
            grammarInfo.termDict.Aggregate("",
                fun ac el -> ac + "    | " + el.Value.ToString() + " -> true\n"
            )
            +
            "    | _ -> false\n"

        let chainToNum = 
            "let chainToNum = function\n" +
             enumeratedChains.Aggregate("",
                fun ac el -> ac + "    | \"" + el.Key.ToString() + "\" -> " + el.Value.ToString() + "\n"
             )  +
             "    | _ -> -1 \n"

        let rules = 
            "let rules = [|\n" + grammarInfo.rules.Aggregate("", fun ac el ->
                 ac + String.Format("    [|{0};{1};{2}|];\n", el.RuleName, el.R1, el.R2)) +
            "|]\n"

                 
        let h, w = Array2D.length1(table), Array2D.length2(table)
        let mutable strTable = ""
        for i = 0 to h - 1 do
            strTable <- strTable + "  [|"
            for j = 0 to w - 1 do
               strTable <- strTable + table.[i, j].ToString() + ";"
            strTable <- strTable + "|];\n"  
        strTable <- "let table = [|\n" + strTable + "|]\n"   

        let parser =
            "let parser = new Parser(tokenToNumber, isTerminal, table, " +
            "startNT, numIsTerminal, k, chainToNum, rules)"
        let code = 
            header + 
            tokenToNumber + 
            isTerminal + 
            startRule + 
            _k + 
            numIsTerminal + 
            chainToNum + 
            strTable + 
            rules + 
            parser
        
        
        
        use out = new System.IO.StreamWriter(Path.GetFullPath "LLKParser/mycooltest.fs")
        out.Write(code)
        code

[<Extension>]
type LLKGenerator() = 
    inherit Generator()
        override this.Name = "LLKGenerator"
        override this.Generate (t, args) =
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2

            for i = 0 to pairs.Length - 1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]

            let getOption name either f =
                match t.options.TryFind name with
                | Some v -> f v
                | None -> either
            let mutable k = 1
            let mutable output = "test.yrd.fs"
            for opt, value in pairs do
                match opt with
                | "-k" -> k <- Convert.ToInt32(value)
                | "-o" ->  if value.Trim() <> "" then output <- value
                | value -> failwithf "Unexpected %s option" value
            
            let gen = new LLKGeneratorImpl()
            let code = gen.Generate t k output
            "code" |> box
        override this.Generate t =
            this.Generate (t, "-k 1 -o LLkTest.yrd.fs")
        override this.Constraints = [| noEbnf; noMeta; noLiterals; noInnerAlt; noAlt; inCNF; singleModule|]

