//  InitialConvert.fs contains methods, which must be applied to grammar
//    to transform this to appliable for RNGLR form.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
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

module Yard.Generators.RNGLR.InitialConvert

open Yard.Core.IL
open Yard.Core.IL.Production

let initialConvert (def : Definition.t<_,_>) =
    let splitAlters ruleList =
        let rec splitRule (curRule : Rule.t<_,_>) res = function
            | PAlt (l, r) ->
                let leftRes = splitRule curRule res l
                splitRule curRule leftRes r
            |  x -> {curRule with body = x}::res
        List.fold (fun res rule -> splitRule rule res rule.body) [] ruleList
    {def with grammar = splitAlters def.grammar}