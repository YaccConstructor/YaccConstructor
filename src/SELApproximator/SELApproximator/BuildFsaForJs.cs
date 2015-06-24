using System;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.JavaScript.Bulbs;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Resources.Shell;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace SELApproximator
{
    [ContextAction(Name = "RunApproximatorJs", Description = "Run Approximator for Js", Group = "JS")]
    public class BuildFsaForJs : ContextActionBase
    {
        private readonly IJavaScriptContextActionDataProvider _provider;

        public BuildFsaForJs(IJavaScriptContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var jsCfg = _provider.GetControlFlowGraph();
            var fsa = ApproximateJs.BuildFsaForOneFunctionCfg(jsCfg);
            OutputResult(Utils.FsaToTestDot(fsa));
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