using System.Collections.Generic;

using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.JavaScript.LanguageImpl;
using JetBrains.ReSharper.Psi.Tree;

namespace ReSharperExtension.Highlighting
{
    /// <summary>
    /// Class for manipulation with host languages. 
    /// </summary>
    static class HostLanguageHelper
    {
        /// <summary>
        /// List of current supported host languages. 
        /// If there is new host language (for example, C++) then 
        /// you'll need to add the appropriate instance to this list 
        /// </summary>
        private static readonly List<PsiLanguageType> SupportedLangs = new List<PsiLanguageType>
        {
            CSharpLanguage.Instance,
            JavaScriptLanguage.Instance
        };

        public static bool IsSupportedFile(IPsiSourceFile sourceFile)
        {
            IFile file = GetFile(sourceFile);
            return file != null;
        }

        public static IFile GetFile(IPsiSourceFile sourceFile)
        {
            if (sourceFile == null || !sourceFile.IsValid())
                return null;

            foreach (var lang in SupportedLangs)
            {
                IFile file = GetFile(sourceFile, lang);
                if (file != null)
                    return file;
            }
            return null;
        }

        private static IFile GetFile<T>(IPsiSourceFile sourceFile, T lang) where T : PsiLanguageType
        {
            IPsiServices psiServices = sourceFile.GetPsiServices();
            return psiServices.Files.GetDominantPsiFile<T>(sourceFile);
        }
    }
}
