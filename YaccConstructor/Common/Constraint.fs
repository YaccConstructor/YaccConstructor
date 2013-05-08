//  Copyright 2013 Avdyukhin Dmitry <dimonbv@gmail.com>
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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

namespace Yard.Core

open Yard.Core.IL

type Constraint(name : string, checker : Grammar.t<Source.t, Source.t> -> bool, conv : Conversion, ?args : string[]) =
    member this.Name = name
    member this.Conversion = conv
    member this.Fix grammar =
        match args with
        | None -> conv.ConvertGrammar grammar
        | Some args -> conv.ConvertGrammar (grammar, args)
    member this.Check grammar = checker grammar
    
