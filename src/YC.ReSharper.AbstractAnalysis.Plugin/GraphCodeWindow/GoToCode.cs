using System;
using System.Linq;
using GraphX.Models;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Navigation.Navigation.NavigationExtensions;
using JetBrains.ReSharper.LiveTemplates;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.TextControl;
using JetBrains.TextControl.Coords;
using JetBrains.Util;
using JetBrains.Util.dataStructures.TypedIntrinsics;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow
{
    public static class GoToCodeEventHandler
    {
        /// <summary>
        /// Invokes if double click on edge control
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        public static void ClickHandler(object sender, EventArgs args)
        {            
            GetCodeEvent += GoToCode;
            InvokeGetCodeEvent.Invoke(sender, args);
        }

        /// <summary>
        /// Goes into souce code uses textControl
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public static void GoToCode(object sender, EventArgs e)
        {
            GetCodeEvent -= GoToCode;
            var textControl = (ITextControl)sender;
            var brEvent = (BackRefEventArgs) e;
            var offset = brEvent.BackRef.Value.GetTreeStartOffset().Offset;
            textControl.Caret.MoveTo(offset, new CaretVisualPlacement());         
        }

        public static event EventHandler GetCodeEvent;
        public static event EventHandler InvokeGetCodeEvent;

        /// <summary>
        /// Invokes event for load source code
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        public static void OnEvent(object sender, EventArgs eventArgs)
        {
            GetCodeEvent(sender, eventArgs);
        }
    }
}