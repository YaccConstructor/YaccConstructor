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
open System.Linq

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

    let rAST =  RegExpAST()

    let getLabels s =
        List.init (tables.IsStart.[s].Count(fun x -> x)) (fun _ -> Label s)
   
    let rec step stack state i =
        let curLexeme = (!Lexer).Value.Get i
        let curLexemeTag = curLexeme.tag
        let reduce ps =
            let rec inner buf stack = 
                match stack with
                | Label i :: tl when tables.IsStart.[i].[ps] -> buf,tl
                | Label _ :: tl -> inner buf tl
                | State _ ::(Symbol(_,t))::tl -> inner (t::buf) tl
                | State 0 :: _ -> buf,stack
                | [] -> buf,[]
                | _ -> failwith "Incorrect stack!"

            let rec clerStack s = 
                match s with
                | Label i :: tl when i = ps -> clerStack tl
                | _            ->  s
            clerStack stack |> inner [] 

        tables.ActionTable.[state].[tables.SymbolIdx.[curLexemeTag]]
        |> List.map (
            function
            | CommonTypes.Accept    -> if curLexemeTag = Constants.gnesccEndStreamTag then [stack] else []
            | CommonTypes.Error     -> 
                printfn "Possible error in symbol %A" i
                []
            | CommonTypes.Shift s   -> 
                let stk = State s :: Symbol (curLexemeTag,(Leaf(curLexemeTag,curLexeme))) :: stack
                step (getLabels s @ stk) s (i+1)
            | CommonTypes.Reduce ps -> 
                let forest,nStack = reduce ps
                let rec state stack = 
                    match stack with
                    | State i :: tl -> i
                    | hd :: tl -> state tl
                    | [] -> failwith "incorrect stack"
                        
                let gt = tables.GotoTable.[state nStack].[tables.ProdToNTerm.[ps]]
                
                if Option.isSome gt
                then
                    let stk = State gt.Value :: (Symbol (ps,Node(forest,ps,[]))) :: (nStack) 
                    step (getLabels gt.Value @ stk) (gt.Value) i
                else [stack] )
        |> List.concat           

    let run lexer  = 
        Lexer := Some lexer
        let r = 
            let f = (step (getLabels 0 @ [State tables.StartIdx.Head]) tables.StartIdx.Head 1)
            f
            |> List.choose (function | _::hd::_ -> Some hd | _ -> None)
        let res = 
            r     
            |> List.map (
                function
                | Symbol (s,t) -> t
                | x -> failwith <| "Incorrect stack: \n" + (List.map string r |> String.concat "\n"))

        cache.Clear()        
        CallCount := 0
        maxCorrPos := 0
        res
    member self.Run lexer = run lexer
end