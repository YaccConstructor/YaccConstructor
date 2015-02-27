using System;
using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.Bulbs;
using JetBrains.ReSharper.Feature.Services.CSharp.Bulbs;
using JetBrains.ReSharper.Intentions.Extensibility;
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
            return null;
        }

        public override string Text
        {
            get { return "Run Approximator"; }
        }
    }
}