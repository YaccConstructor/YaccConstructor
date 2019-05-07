//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module YC.Generators.RNGLR.Helper

let _getLiteralName (lit:string) =    
    let replacementDict =
        [
            '.', "dot"
            ',', "comma"
            ':', "semi"
            ';', "colon"
            '+', "plus"
            '-', "minus"
            '*', "star"
            '<', "less"
            '>', "more"
            '=', "equal"
            '/', "slash"
            '&', "and"
            '|', "or"
            '?', "question"
            '$', "dollar"
            '[', "left_square_bracket"
            ']', "right_square_bracket"
            '(', "left_bracket"
            ')', "right_bracket"
            '!', "not"
            '~', "tilda"
            '#', "sharp"
            '%', "percent"
            '^', "hat"
            '{', "left_figure_bracket"
            '}', "right_figure_bracket"
            '\\', "reverse_slash"
            '`', "reverse_quate"
            ''', "quate"
            '№', "number"
        ]
        |> dict

    lit
    |> Seq.mapi  
        (fun i ch ->
            let exist,v = replacementDict.TryGetValue(ch)
            if exist
            then
                if i = 0 
                then v + "_"
                elif i = lit.Length - 1
                then "_" + v
                else "_" + v + "_"
            else string ch
        )
    |> String.concat ""

