using System;
using System.IO;
using JetBrains.Application;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;


namespace ApproximatorTester
{
    [ContextAction(Name = "RunApproximator", Description = "Run Approximator", Group = "C#")]
    public class RunApproximatorAction : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;

        public RunApproximatorAction(ICSharpContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var inputFile = _provider.PsiFile;
            Approximator.BuildApproximation(inputFile);
            OutputResults();
            return null;
        }

        public override string Text
        {
            get { return "Run Approximator"; }
        }

        private void OutputResults()
        {
            var firstToken = _provider.TokenAfterCaret;
            var namespaceDecl = firstToken == null ? null : firstToken.Parent;
            if (namespaceDecl == null)
            {
                const string msg = @"No namespace declaration after caret in test file. 
                                    Possibly incorrect test input file format,
                                    it must start with ""{caret}namespace""";
                throw new Exception(msg);
            }

            var resultString = ResultToString();
            var factory = CSharpElementFactory.GetInstance(_provider.PsiModule);
            var commentWithResult = factory.CreateComment("/*\n" + resultString + "\n*/");
            WriteLockCookie.Execute(() =>
            {
                ModificationUtil.ReplaceChild(namespaceDecl, commentWithResult);
            });
        }

        private string ResultToString()
        {
            return "result";
        }
    }
}