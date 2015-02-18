using System;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Feature.Services.LinqTools;
using JetBrains.ReSharper.Intentions.Extensibility;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.TextControl;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation;


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
            
            CFA.buildControlFlowGraph(inputFile);

            return null;
        }

        public override string Text
        {
            get { return "Run Approximator"; }
        }
    }
}