module AboutAction

open JetBrains.ActionManagement
open System.Windows.Forms

[<assembly: ActionsXml("YC.ReSharper.AbstractAnalysis.Languages.Calc.Actions.xml")>]
do ()

[<ActionHandler("Plugins.Calc.About")>]
type AboutAction () =
    interface IActionHandler with
        member this.Update (c, p, n) = true
        member this.Execute (c, n) =
            MessageBox.Show("Calc plugin") |> ignore