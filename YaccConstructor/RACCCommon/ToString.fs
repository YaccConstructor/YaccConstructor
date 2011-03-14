// ToString.fs
//
//  Copyright 2010 Semen Grigorev <rsdpisuy@gmail.com>
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

module  Yard.Generators.RACCGenerator.ToString

open Yard.Generators.RACCGenerator

let rec toString (_val:obj) =
    match _val with
    | :? string  -> "\"" + (_val.ToString()) + "\""
    | :? char    -> "'" + (_val.ToString()) + "'" 
    | :? (string * int)    -> 
        let l = fst (_val :?> (string*int))
        let r = snd (_val :?> (string*int))
        "(" + toString l + "," + toString r + ")"

    | :? ((string * int * string)*(Set<string*int>))    ->
        match _val :?> ((string * int * string)*(Set<string*int>)) with
        | (l,r,m),s -> 
        
            "((" + toString l + "," + toString r + "," + toString m + ")," + __setToString s + ")"

    | :? (int*(string * int)) -> 
        let l = fst (_val :?> (int*(string * int)))
        let r = snd (_val :?> (int*(string * int)))
        "(" + toString l + "," + toString r + ")"
    | :? (((string*int)* 'a )* (string*int)) as l -> 
        match l with 
        |((p1, D), p2) -> 
            "(" + "(" + toString p1 + D.ToString() + ")," + toString p2 + ")"
    | :? List<'a> -> listToString (_val:?>List<'a>)
    | :? List<FATrace> -> _listToString (_val:?>List<FATrace>)
    | :? (int*(Set<string*int>)) -> 
        let l = fst (_val :?> (int*(Set<string*int>)))
        let r = snd (_val :?> (int*(Set<string*int>)))
        "(" + toString l + "," + __setToString r + ")"    
    | :? Set<List<FATrace>> -> _setToString (_val:?>(Set<List<FATrace>>))    
    | _          -> _val.ToString()

and listToString _list =
     "List.ofArray [|" + (String.concat ";" (List.map toString _list)) + "|]\n"    

and _listToString _list =
     "List.ofArray [|" + (String.concat ";" (List.map toString _list)) + "|]\n"    

and dictToString (_dict:System.Collections.Generic.IDictionary<_,_>) =
    "dict [|" + (String.concat ";" (Seq.map
                                     (fun k -> "("+ toString k + "," + (toString _dict.[k]) + ")")
                                     _dict.Keys)) + "|]" 
and setToString (_set:Set<'a>) =
    "Set.ofArray [|" + (String.concat ";" (Set.map toString _set)) + "|]"    

and _setToString _set =
    "Set.ofArray [|" + (String.concat ";" (Set.map toString _set)) + "|]"  

and __setToString _set =
    "Set.ofArray [|" + (String.concat ";" (Set.map toString _set)) + "|]"    

