//  Module ReplaceInline replace nonterminals, from which only
//    Pref, PToken or PLiteral can be infered, whit their right sides.
//
//  Copyright 2011, 2012 Avdyukhin Dmitry
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

module Yard.Core.Convertions.ToCNF

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic

//--Тестовые функции-------------------------------------------------------------------------------

let file = new FileStream(@"E:\out.txt", FileMode.OpenOrCreate, FileAccess.Write)
let cout = new StreamWriter(file)

let index = ref 0

let print (obj:'a) = 
    cout.WriteLine(obj)
    cout.Flush()

//--Функция которая удаляет правила в правиле------------------------------------------------------

let newRuleForRuleInRule (ruleList: Rule.t<_, _> list) = 

//--Функция для удаления эпсилон-правил------------------------------------------------------------

let deleteEpsRule (ruleList: Rule.t<_, _> list) = 
    
    //--Находим все эпсилон-правила----------------------------------------------------------------

    let epsList = 
        ruleList |> List.collect
            (fun rule -> 
                match rule.body with
                |PSeq(elements, actionCode, lbl) when elements.IsEmpty -> [rule.name]
                |x -> []
            )

    //--Функция для проверки вхождения эпсилон-правила---------------------------------------------

    let isEps (s:string) = 
        epsList |> List.collect
            (fun eps -> if s = eps then [eps] else [])
    
    //--Список эпсилон-правил входящих в данное правило--------------------------------------------     

    let rec epsInRule (elements: elem<_,_> list) = 
        elements |> List.collect
                    (fun elem ->
                        match elem.rule with
                        |PSeq(e,a,l) -> epsInRule e
                        |PRef(t,_) -> isEps (fst t)
                        |x -> []
                    )
                        
    //--Функция для добавления нового правила------------------------------------------------------

    let newRule (rule: Rule.t<_,_>) (epsRef: string list) = 
        incr index
        [{
            Rule.t.name="newEpsRule" + (!index).ToString()
            Rule.t.args=[]
            Rule.t._public=false
            Rule.t.metaArgs=[] 
            Rule.t.body=PSeq([],None,None)
        }]

    //--Добавляем новые правила--------------------------------------------------------------------

    ruleList |> List.collect
        (fun rule -> 
            print ("Rule: [" + rule.name + "]")
            match rule.body with
            |PSeq(elements, actionCode, lbl) when not elements.IsEmpty -> 
                print (epsInRule elements)
                newRule rule (epsInRule elements) @ [rule]
            |x -> []
        )

//--Функция для удаления цепных правил-------------------------------------------------------------
                
let deleteChainRule (ruleList: Rule.t<_, _> list) = ruleList

//--Преобразование в КНФ---------------------------------------------------------------------------

let toCNF (ruleList: Rule.t<_, _> list) = 
    let cnf (rules: Rule.t<_, _> list) = rules
    ruleList
    |> newRuleForRuleInRule
    |> deleteEpsRule
    |> deleteChainRule
    |> cnf

type ToCNF() = 
    inherit Convertion()
        override this.Name = "ToCNF"
        override this.ConvertList (ruleList,_) = toCNF ruleList
        override this.EliminatedProductionTypes = [""]

type DeleteEpsRule() = 
    inherit Convertion()
        override this.Name = "DeleteEpsRule"
        override this.ConvertList (ruleList,_) = deleteEpsRule ruleList
        override this.EliminatedProductionTypes = [""]

type DeleteChainRule() = 
    inherit Convertion()
        override this.Name = "DeleteChainRule"
        override this.ConvertList (ruleList,_) = deleteChainRule ruleList
        override this.EliminatedProductionTypes = [""]

