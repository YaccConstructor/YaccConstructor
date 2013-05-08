using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using Microsoft.VisualStudio.Shell.Interop;   

namespace YC.VSYard.AutoCompletion
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("yardtype")]
    [Name("YardCompletion")]
    class YardCompletionSourceProvider : ICompletionSourceProvider
    {
        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
            DTE dte = Package.GetGlobalService(typeof(SDTE)) as DTE;
            return new VSYardNS.YardCompletionSource(textBuffer, dte);
        }
    }

}