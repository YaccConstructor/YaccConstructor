using System;
using System.Linq;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.GeneratedCode;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [FileStructureExplorer]
    public class MyFileStructureExplorer : IFileStructureExplorer
    {
        public IFileStructure Run(Predicate<DocumentRange> isRangeInvalidated, IPsiSourceFile psiSourceFile, IContextBoundSettingsStore settingsStore, IFile file)
        {
            return new MyFileStructure(file, settingsStore);
        }

        private class MyFileStructure : FileStructureWithRegionsBase
        {
            public MyFileStructure([NotNull] IFile file, IContextBoundSettingsStore settingsStore)
                : base(file, settingsStore.EnumEntryIndices(GeneratedCodeSettingsAccessor.GeneratedCodeRegions).ToHashSet())
            {
            }
        }
    }
}
