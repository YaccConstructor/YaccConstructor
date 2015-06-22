using System.Collections.Generic;
using System.Linq;
using JetBrains.Application;
using JetBrains.ReSharper.Resources.Shell;
using JetBrains.Util;

namespace ReSharperExtension
{
    [ShellComponent]
    public class LanguageLoader
    {
        public LanguageLoader(IEnumerable<IReSharperLanguage> providers)
        {
            //At this point all languages must be found and loaded"
        }
    }
}
