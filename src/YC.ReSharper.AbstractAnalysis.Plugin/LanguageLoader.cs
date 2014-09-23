using System.Collections.Generic;
using JetBrains.Application;
using ReSharperExtension;
using YC.AbstractAnalysis;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
    [ShellComponent]
    public class LanguageLoader
    {
        public LanguageLoader(IEnumerable<IReSharperLanguage> providers)
        {
        }
    }
}
