using System.Collections.Generic;
using System.Linq;
using JetBrains.Application;
using JetBrains.ReSharper.Resources.Shell;
using JetBrains.Util;
using ReSharperExtension;

namespace YC.ReSharper.AbstractAnalysis
{
    [ShellComponent]
    public class LanguageLoader
    {
        public LanguageLoader(IEnumerable<IReSharperLanguage> providers)
        {
            IEnumerable<IReSharperLanguage> components = Shell.Instance.GetComponents<IReSharperLanguage>();
            if (components.IsEmpty())
            {
            }
            else
            {
                IReSharperLanguage lang = components.FirstOrDefault();
                lang.ToString();
            }

            //At this point all languages must be found and loaded"
        }
    }
}
