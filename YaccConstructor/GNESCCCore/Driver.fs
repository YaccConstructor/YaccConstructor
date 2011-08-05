// Driver.fs is main file GNESCCCore
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

namespace Yard.Generators.GNESCCGenerator

type CoreDriver() =
    class
        let parse (lexer: ILexer) tables =
            let tableInterpreter = new TableInterpreter(tables)
            tableInterpreter.Run lexer            

        let interprete inpForest = ()

        member self.Parse lexer = parse lexer
        member self.Interprete inpForest = interprete inpForest 
    end
