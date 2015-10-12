module AboutAction

//open JetBrains.ActionManagement
//open JetBrains.Application
//open JetBrains.UI.ActionsRevised
//open JetBrains.UI.Application
//open JetBrains.UI.ToolWindowManagement
//open System.Windows.Forms

//[<assembly: 
//("YC.ReSharper.AbstractAnalysis.Languages.Calc.Actions.xml")>]
//do ()
//
////[<("Plugins.Calc.About")>]
//[<ToolWindowDescriptor(Text = "Calc", 
//    VisibilityPersistenceScope = ToolWindowVisibilityPersistenceScope.Solution,
//    Type = ToolWindowType.SingleInstance,
//    InitialDocking = ToolWindowInitialDocking.NotSpecified)>]
//type WindowDescriptor (applicationHost : IApplicationHost) =
//    inherit ToolWindowDescriptor(applicationHost)
//
//type AboutAction () =
//    inherit ActivateToolWindowActionHandler<WindowDescriptor>()
//        member this.Update (context, presentation, nextUpdate) = true
//        member this.Execute (context, nextExecute) =
//            MessageBox.Show("Calc plugin") |> ignore