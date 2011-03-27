// PrintCSV.fs contains simple CSV printer.
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

module PrintCSV

open System.IO

let print (outFile:string) sep data =
    let str = Seq.map (String.concat sep) data |> String.concat "\n"
    let outStrieam =         
        try            
            printf "\n%A\n" outFile
            let t = new FileInfo(outFile.Replace('/','\\'))
            let writer = t.CreateText()
            writer     
        with e -> failwith ("Writer Exception:" + e.ToString())
    outStrieam.Write str
    outStrieam.Close()    

