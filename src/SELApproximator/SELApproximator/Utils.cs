using System;
using System.Linq;
using JetBrains.ReSharper.Feature.Services.CSharp.Analyses.Bulbs;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Resources.Shell;
using QuickGraph.FSA;


namespace SELApproximator
{
    public class Utils
    {
        public static string DdgToTestDot<TLit, TOpInfo>(GenericGraphs.GraphWithSingleEnds<TLit, TOpInfo> ddg)
        {
            var edgesList = ddg.Graph.Edges
                .Select(edge => edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString())
                .ToList();
            return string.Join("\n", edgesList);
        }

        public static string FsaToTestDot<T>(GraphBasedFsa.FSA<Tuple<char, T>> fsa)
        {
            Func<GraphBasedFsa.Symb<Tuple<char, T>>, string> tagToStr =
                tag =>
                {
                    if (tag.IsEps) return "Eps";
                    var data = ((GraphBasedFsa.Symb<Tuple<char, T>>.Smbl) tag).Item;
                    return data.Item1.ToString();
                };
            var edgesList = fsa.Edges
                .Select(edge => string.Format(
                    "{0:D} -> {1:D} [label=\"{2}\"]; ", 
                    edge.Source, edge.Target, tagToStr(edge.Tag)))
                .ToList();
            return String.Join("\n", edgesList);
        }

        public static void OutputCSharpResult(string result, ICSharpContextActionDataProvider provider)
        {
            var firstToken = provider.TokenAfterCaret;
            var namespaceDecl = firstToken == null ? null : firstToken.Parent;
            if (namespaceDecl == null)
            {
                const string msg = @"No namespace declaration after caret in test file. 
                                    Possibly incorrect test input file format,
                                    it must start with ""{caret}namespace""";
                throw new Exception(msg);
            }

            var factory = CSharpElementFactory.GetInstance(provider.PsiModule);
            var commentWithResult = factory.CreateComment("/*\n" + result + "\n*/");
            WriteLockCookie.Execute(() =>
            {
                ModificationUtil.ReplaceChild(namespaceDecl, commentWithResult);
            });
        }
    }
}