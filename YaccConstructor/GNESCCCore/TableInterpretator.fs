// TableInterpretator.fs contains core functions of recursive-ascent algorithm:
//    parse and climb
//
//  Copyright 2009, 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.GNESCCGenerator


open Yard.Generators.GNESCCGenerator.AST

type Item<'state, 'symbol> =
    {
        state  : 'state
        symbol : 'symbol        
    }

type ParseStatus<'a,'b,'c when 'a: comparison and 'c: equality and 'c: comparison> = 
    | PSuccess of Set<AST<'a,'b,'c>>
    | PError   of int

type StackElt<'a,'b,'c when 'a : comparison and 'c: equality> =
    | State of int
    | Symbol of int * AST<'a,'b,'c>
    | Label of int

type  TableInterpreter (tables:Tables) = class
    
    let forest = ref []

    let maxCorrPos = ref 0

    let Lexer: Option<ILexer>  ref = ref None

    let CallCount = ref 0

//    let tables,setTables =
//        let Tables: Option<Tables> ref = ref None  
//        let getTables () =  (!Tables).Value
//        getTables , fun t -> Tables:=t

    let traceCache = new System.Collections.Generic.Dictionary<Set<FATrace list>,int>()

    let traceBuilderCache = new System.Collections.Generic.Dictionary<_, int>()

    let traceEnumerator = new Enumerator()

    let cache = new System.Collections.Generic.Dictionary<_,_>()    

    let memoize f =         
        fun parserState ->                    
            let key = parserState
            let flg,res = cache.TryGetValue key
            if flg then res
            else
                let calculated = f parserState
                let flg,stored = cache.TryGetValue key // value can be inserted in the cache by recursive call of f 
                if not flg then cache.Add(key,calculated)
                calculated

    let print ps =
        printfn "ParseState:\n     i = %A\n     symbol = %A\n     statesSet = [\n" ps.i ps.inpSymbol        
        Set.iter 
            (fun s -> 
                printfn 
                    "         State:\n             item = %A\n             forest = <<\n" 
                    s.item                
                List.iter PrintTree (List.map (fun x -> !x)(s.forest))
                printfn "             >>\n")
            ps.statesSet
        printfn "     ]\n" 

    let rAST =  RegExpAST()

    let getLabels s =
        List.init (Seq.fold (fun b x -> if x then b+1 else b) 0 (tables.IsStart.[s])) (fun _ -> Label s)

    let buildCorrectTrace trace =
        let id = hash trace        
        if traceBuilderCache.ContainsKey(id)
        then traceBuilderCache.[id]
        else                                                
            let key = traceEnumerator.Next()
            traceBuilderCache.Add(id, key)
            traceCache.Add(rAST.BuilCorrectTrace trace,key)
            key

    let rec step stack state i =
        let curLexeme = (!Lexer).Value.Get i
        let curLexemeTag = curLexeme.tag
        let reduce ps =
            let rec inner buf stack = 
                match stack with
                | Label _ :: tl -> buf,tl                
                | State _ ::(Symbol(_,t))::tl -> inner (t::buf) tl
                | State 0 :: _ -> buf,stack
                | [] -> buf,[]
                | _ -> failwith "Incorrect stack!"

            let rec clerStack s = 
                match s with
                | Label _ ::tl -> clerStack tl
                | _            ->  s
            clerStack stack |> inner [] 

        tables.ActionTable.[state].[tables.SymbolIdx.[curLexemeTag]]
        |> List.map (
            function
            | CommonTypes.Accept    -> if curLexemeTag = Constants.gnesccEndStreamTag then [stack] else []
            | CommonTypes.Error     -> 
                let xx = ref 1
                incr xx
                []//failwith "Parser error"
            | CommonTypes.Shift s   -> 
                let stk = State s :: Symbol (curLexemeTag,(Leaf(curLexemeTag,curLexeme))):: stack
                step (getLabels s @ stk) s (i+1)
            | CommonTypes.Reduce ps -> 
                let forest,nStack = reduce ps
                let rec state stack = 
                    match stack with
                    | State i :: tl -> i
                    | hd :: tl -> state tl
                    | [] -> failwith "incorrect stack"
                        
                let gt = (tables.GotoTable.[(state nStack)].[tables.ProdToNTerm.[ps]])
                
                if Option.isSome gt
                then
                    let stk = State gt.Value :: (Symbol (ps,Node(forest,ps,[]))) :: (nStack) 
                    step (getLabels ps @ stk) (gt.Value) i
                else [stack] )
        |> List.concat           

    let run lexer  = 
        Lexer := Some lexer
        //Some tables |> setTables
        let r = 
            let f = (step (getLabels 0 @ [State tables.StartIdx.Head]) tables.StartIdx.Head 1)
            f
            |> List.choose (function | _::hd::_ -> Some hd | _ -> None)
        let res = 
            r     
            |> List.map (
                function
                | Symbol (s,t) -> t)
                       
                
//            |> Set.fold
//                (fun buf r ->
//                    let forest = List.map (fun x -> !x) (r.rItem.forest)
//                    if List.length  forest = 1 && r.rItem.item = tables.StartIdx.Head
//                    then
//                        let getUserTree tree =
//                            match tree with
//                            | Node (childs,_,_) -> Some (List.head childs)
//                            | _                   -> None
//
//                        List.head forest
//                        |> fun x ->
//                            let y = getUserTree x
//                            if y.IsSome
//                            then Set.add !y.Value buf
//                            else buf
//                    else buf)
//                Set.empty
//#if DEBUG
//        Set.iter PrintTree res
//#endif
        Set.iter PrintTree (Set.ofList res)
        cache.Clear()
        traceBuilderCache.Clear()
        let trC =
            seq {for s in traceCache -> s.Value,s.Key}
            |> dict
        traceCache.Clear()
//        let res = 
//            if res.Count > 0
//            then PSuccess res
//            else PError (!maxCorrPos + 1)
//             ,trC,!CallCount
        CallCount := 0
        maxCorrPos := 0
        res
    member self.Run lexer = run lexer
end