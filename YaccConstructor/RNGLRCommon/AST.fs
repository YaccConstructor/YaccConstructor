//  AST.fs contains description of derivation tree.
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

module Yard.Generators.RNGLR.AST

type AST<'TokenType> =
    | Epsilon
    /// Non-terminal expansion: production, family of children
    | Inner of int * MultiAST<'TokenType> []

/// Family of children - For one nonTerminal there can be a lot of dirivation trees
and MultiAST<'TokenType> =
    | NonTerm of AST<'TokenType> list ref
    | Term of 'TokenType

let inline getFamily node = match node with | NonTerm list -> list

let isEpsilon = function
    | Epsilon -> true
    | _ -> false

let isInner = function
    | Inner _ -> true
    | _ -> false

