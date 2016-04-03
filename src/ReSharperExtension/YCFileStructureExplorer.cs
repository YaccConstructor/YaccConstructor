using System;
using System.Linq;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.GeneratedCode;
using JetBrains.ReSharper.Psi.Tree;

namespace ReSharperExtension
{
    [FileStructureExplorer]
    public class YCFileStructureExplorer : IFileStructureExplorer
    {
        #region IFileStructureExplorer members
        public IFileStructure Run(Predicate<DocumentRange> isRangeInvalidated, IPsiSourceFile psiSourceFile, IContextBoundSettingsStore settingsStore, IFile file)
        {
            return new YCFileStructure(file, settingsStore);
        }
        #endregion

        private class YCFileStructure : FileStructureWithRegionsBase
        {
            public YCFileStructure([NotNull] IFile file, IContextBoundSettingsStore settingsStore)
                : base(file, settingsStore.EnumEntryIndices(GeneratedCodeSettingsAccessor.GeneratedCodeRegions).ToHashSet())
            {
            }
        }
    }
}
