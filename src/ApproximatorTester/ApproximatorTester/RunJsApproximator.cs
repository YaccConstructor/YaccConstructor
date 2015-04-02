using System;
using JetBrains.Application;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.JavaScript.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.JavaScript.Services;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace ApproximatorTester
{
    [ContextAction(Name = "RunApproximatorJs", Description = "Run Approximator for Js", Group = "JS")]
    public class RunJsApproximator : ContextActionBase
    {
        private readonly IJavaScriptContextActionDataProvider _provider;

        public RunJsApproximator(IJavaScriptContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var jsCfg = _provider.GetControlFlowGraf();
            var cfg = ApproximationBuilderJs.BuildApproximation(jsCfg);
            OutputResult(Utils.GenericCfgStructureToDot(cfg));
            return null;
        }

        public override string Text
        {
            get { return "Run Approximator for Js"; }
        }

        private void OutputResult(string result)
        {
            var firstToken = _provider.TokenAfterCaret;
            var functionDecl = 
                firstToken == null ? null : 
                firstToken.Parent == null ? null :
                firstToken.Parent.Parent;
            if (functionDecl == null)
            {
                const string msg = @"No function declaration after caret in test file. 
                                    Possibly incorrect test input file format,
                                    it must start with ""{caret}function""";
                throw new Exception(msg);
            }

            var factory = CSharpElementFactory.GetInstance(functionDecl);
            var commentWithResult = factory.CreateComment("/*\n" + result + "\n*/");
            WriteLockCookie.Execute(() =>
            {
                ModificationUtil.ReplaceChild(functionDecl, commentWithResult);
            });
        }
    }
}