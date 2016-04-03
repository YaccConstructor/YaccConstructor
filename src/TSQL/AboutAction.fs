module AboutAction

//open JetBrains.ActionManagement
//open JetBrains.Application
//open JetBrains.UI.ActionsRevised
//open JetBrains.UI.Application
//open JetBrains.UI.ToolWindowManagement
//open System.Windows.Forms

//[<assembly: ActionsXml("YC.ReSharper.AbstractAnalysis.Languages.TSQL.Actions.xml")>]
//do ()

//[<ActionHandler("Plugins.TSQL.About")>]

//[<ToolWindowDescriptor(Text = "TSQL", 
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
//            MessageBox.Show("TSQL plugin") |> ignore