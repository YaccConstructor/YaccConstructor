using System;
using System.Drawing;
using System.Linq;
using FSharpx;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Daemon.CSharp.Stages;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Feature.Services.LinqTools;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.TextControl;
using JetBrains.TextControl.DocumentMarkup;
using JetBrains.TextControl.Impl;
using JetBrains.UI.ActionSystem.Actions.CloseAll;
using JetBrains.UI.RichText;
using JetBrains.Util;
//using YS.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation;
//using ;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
    /// <summary>
    /// This is an example context action. The test project demonstrates tests for
    /// availability and execution of this action.
    /// </summary>
    [ContextAction(Name = "ReverseString", Description = "Reverses a string", Group = "C#")]
    public class ReverseStringAction : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;
        private ILiteralExpression _stringLiteral;

        public ReverseStringAction(ICSharpContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            var literal = _provider.GetSelectedElement<ILiteralExpression>(true, true);
            var processor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(_provider);
            var parserRes = processor.Process();
            //parserRes.
            ////var style = new TextStyle(FontStyle.Italic, Color.Aqua, Color.Bisque);
            ////var t =  CSharpHighlightingConsumerExtension.AddHighlighting()
            ////t.
            ////ITextControl
            ////var t = _provider.Document. // TextControl;
            ////t.Document.
            ////var h = new TextControlMarkup.HighlighterProcessor();
            ////var gg = HighlighterLayer.
            ////var tt = highli
            ////ITextControl.
            Console.WriteLine(parserRes);
            if (literal != null && literal.IsConstantValue() && literal.ConstantValue.IsString())
            {
                var s = literal.ConstantValue.Value as string;
                if (!string.IsNullOrEmpty(s))
                {
                    _stringLiteral = literal;
                    return true;
                }
            }
            return false;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            CSharpElementFactory factory = CSharpElementFactory.GetInstance(_provider.PsiModule);

            var stringValue = _stringLiteral.ConstantValue.Value as string;
            if (stringValue == null)
                return null;

            var chars = stringValue.ToCharArray();
            Array.Reverse(chars);
            ICSharpExpression newExpr = factory.CreateExpressionAsIs("\"" + new string(chars) + "\"");
            _stringLiteral.ReplaceBy(newExpr);
            return null;
        }

        public override string Text
        {
            get { return "Reverse string"; }
        }
    }
}
//Error	13	Argument 1: cannot convert from 'AbstractLexer.Common.LexerInputGraph<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>' to 'AbstractLexer.Common.LexerInputGraph<string>'	D:\projects\yc\recursive-ascent\YaccConstructor\YC.ReSharper.AbstractAnalysis\YC.ReSharper.AbstractAnalysis.Plugin.Core\ReverseStringAction.cs	46	103	YC.ReSharper.AbstractAnalysis.Plugin

