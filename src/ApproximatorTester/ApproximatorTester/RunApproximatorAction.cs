using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.CSharp.Analyses.Bulbs;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Resources.Shell;
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
            var ddGraphs = Approximator.BuildApproximation(inputFile);
            OutputResult(ResultToString(ddGraphs.First()));
            return null;
        }

        public override string Text
        {
            get { return "Run Approximator"; }
        }

        private void OutputResult(string result)
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

            var factory = CSharpElementFactory.GetInstance(_provider.PsiModule);
            var commentWithResult = factory.CreateComment("/*\n" + result + "\n*/");
            WriteLockCookie.Execute(() =>
            {
                ModificationUtil.ReplaceChild(namespaceDecl, commentWithResult);
            });
        }

        private string ResultToString(DataDependencyGraph.DDGraph ddGraph)
        {
            var edgesList = ddGraph.Graph.Edges
                .Select(edge => edge.Source.ToString() + " -> " + edge.Target.ToString())
                .ToList();
            return String.Join("\n", edgesList);
        }

    }
}