// TextWriter.fs
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators._RACCGenerator

open System.IO

type TextWriter(outPath: string) =
    class
        let outStrieam =         
            try
                let t = new FileInfo(outPath)
                let writer = t.CreateText()             
                writer     
            with e -> failwith ("Writer Exception:" + e.ToString())
                         
        let write (str:string) = outStrieam.WriteLine(str)

        let closeOutStream _ = outStrieam.Close()

        member self.Write str = write str
        member self.CloseOutStream () = closeOutStream ()
    end