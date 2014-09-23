using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using GraphX.Controls;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;
using JetBrains.DataFlow;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.TextControl;
using JetBrains.UI.Application;
using JetBrains.UI.ToolWindowManagement;
using Microsoft.FSharp.Core;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow
{
    [ActionHandler("YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeToolWindow")]
    public class GraphCodeToolWindowAction : IActionHandler
    {
        public bool Update(IDataContext context, ActionPresentation presentation, DelegateUpdate nextUpdate)
        {
            return true;
        }

        public void Execute(IDataContext context, DelegateExecute nextExecute)
        {
            //textControl = context.GetData(JetBrains.TextControl.DataContext.DataConstants.TEXT_CONTROL);
            //GoToCodeEventHandler.InvokeGetCodeEvent += GetSourceCode;
            //var descriptor = DataConstantsExtensions.GetComponent<GraphCodeToolWindow>(context);
            //var manager = DataConstantsExtensions.GetComponent<ToolWindowManager>(context);
            //var lifetime = DataConstantsExtensions.GetComponent<Lifetime>(context);
            //var uiApplication = DataConstantsExtensions.GetComponent<UIApplication>(context);
            ////var registrar = new GraphCodeWindowRegistrar(lifetime, manager, descriptor, uiApplication);
            ////registrar.Show();
            //var graphs = (new GraphLoader()).Load();
            //var tabControl = new TabControl();
            //var zcontrols = new List<ZoomControl>();
            //foreach (var graph in graphs)
            //{
            //    var gArea = InitializeGraphArea.Initialize(graph);
            //    var zcontrol = new ZoomControl();
            //    zcontrol.Content = gArea;
            //    zcontrols.Add(zcontrol);
            //}
            //tabControl.ItemsSource = zcontrols;
            //Window w = new Window();
            //w.Content = tabControl;
            //w.Show();
        }

        public void GetSourceCode(object sender, EventArgs args)
        {
            try
            {
                //return source code
                GoToCodeEventHandler.OnEvent(textControl, new BackRefEventArgs((FSharpOption<ICSharpLiteralExpression>) sender));
            }
            catch (NullReferenceException e) { }
        }

        private ITextControl textControl;
    }

    public class BackRefEventArgs : EventArgs
    {
        public BackRefEventArgs(FSharpOption<ICSharpLiteralExpression> br)
        {
            BackRef = br;
        }
        public FSharpOption<ICSharpLiteralExpression> BackRef { get; private set; } 
    }
}
