using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Psi.CodeInspections
{
    [FileStructureExplorer]
    public class MyFileStructureExplorer : IFileStructureExplorer
    {
        public IFileStructure Run(Predicate<DocumentRange> isRangeInvalidated, IPsiSourceFile psiSourceFile, IContextBoundSettingsStore settingsStore, IFile file)
        {
            //var psiFile = file as IMyFile;
            //if (psiFile == null)
            //    return null;

            return new MyFileStructure(file, settingsStore);
        }
    }
}
