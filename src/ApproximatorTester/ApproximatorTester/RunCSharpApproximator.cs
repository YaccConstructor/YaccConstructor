using System;
using System.Linq;
using JetBrains.Application;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation;


namespace ApproximatorTester
{
    [ContextAction(Name = "RunCSharpApproximator", Description = "Run Approximator for C#", Group = "C#")]
    public class RunCSharpApproximator : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;

        public RunCSharpApproximator(ICSharpContextActionDataProvider provider)
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
            get { return "Run Approximator for CSharp"; }
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

        private string ResultToString(GenericCFG.GenericCFG ddGraph)
        {
            var edgesList = ddGraph.Graph.Edges
                .Select(edge => edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString())
                .ToList();
            return String.Join("\n", edgesList);
        }
    }
}