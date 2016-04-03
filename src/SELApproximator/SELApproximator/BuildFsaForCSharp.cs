using System;

using Microsoft.FSharp.Collections;

using JetBrains.Application.Progress;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Feature.Services.ContextActions;
using JetBrains.ReSharper.Feature.Services.CSharp.Analyses.Bulbs;
using JetBrains.TextControl;
using JetBrains.Util;

using YC.ReSharper.AbstractAnalysis.LanguageApproximation;

namespace SELApproximator
{
    [ContextAction(Name = "BuildFSAForCSharp", Description = "BuildFSAForCSharp", Group = "C#")]
    public class BuildFsaForCSharp : ContextActionBase
    {
        private readonly ICSharpContextActionDataProvider _provider;
        private readonly FSharpList<Tuple<string, Hotspot.Hotspot>> _hotspotsList = CreateHotspotsList();

        public BuildFsaForCSharp(ICSharpContextActionDataProvider provider)
        {
            _provider = provider;
        }

        public override bool IsAvailable(IUserDataHolder cache)
        {
            return true;
        }

        public override string Text
        {
            get { return "BuildFSAForCSharp"; }
        }

        protected override Action<ITextControl> ExecutePsiTransaction(ISolution solution, IProgressIndicator progress)
        {
            var inputFile = _provider.PsiFile;
            const int recursionMaxLevel = 3; // recursive calls executed while recursionMaxLevel >= 0

            var automata = ApproximateCsharp.ApproximateFile(inputFile, recursionMaxLevel, _hotspotsList);
            if (automata.Count < 1)
                throw new ArgumentException("File under processing contains no hotspots, but at least one expected");
            var fsa = automata[0].Item2;
            Utils.OutputCSharpResult(Utils.FsaToTestDot(fsa), _provider);
            return null;
        }

        private static FSharpList<Tuple<string, Hotspot.Hotspot>> CreateHotspotsList()
        {
            var tSqlHotspot = new Hotspot.Hotspot("TSQL", "Program", "ExecuteImmediate", 0, "void");
            var calcHotspot = new Hotspot.Hotspot("calc", "Program", "Eval", 0, "int");
            var tSqlTuple = new Tuple<string, Hotspot.Hotspot>(tSqlHotspot.Language, tSqlHotspot);
            var calcTuple = new Tuple<string, Hotspot.Hotspot>(calcHotspot.Language, calcHotspot);
            return 
                FSharpList<Tuple<string, Hotspot.Hotspot>>.Cons(
                tSqlTuple,
                FSharpList<Tuple<string, Hotspot.Hotspot>>.Cons(
                calcTuple,
                FSharpList<Tuple<string, Hotspot.Hotspot>>.Empty
                ));
        }
    }
}