using System.Collections.Generic;
using JetBrains.Application;

namespace ReSharperExtension
{
    [ShellComponent]
    public class LanguageLoader
    {
        public LanguageLoader(IEnumerable<ReSharperLanguage.IReSharperLanguage> providers)
        {
            //At this point all languages must be found and loaded"
        }
    }
}
