using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.GeneratedCode;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Psi.CodeInspections
{
    public class MyFileStructure : FileStructureWithRegionsBase
    {
        private IFile myFile;

        public MyFileStructure([NotNull] IFile file, IContextBoundSettingsStore settingsStore)
            : base(file, settingsStore.EnumEntryIndices(GeneratedCodeSettingsAccessor.GeneratedCodeRegions).ToHashSet())
        {
            myFile = file;
            ProcessFile();
        }

        private void ProcessFile()
        {
            new RecursiveElementProcessor(node =>
            {
                
            }).Process(myFile);

            CloseAllRanges(myFile);
        }
    }
}
