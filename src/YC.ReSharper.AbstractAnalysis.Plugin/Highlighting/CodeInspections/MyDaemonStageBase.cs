using System.Collections.Generic;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.CodeInspections
{
    public abstract class MyDaemonStageBase : IDaemonStage
    {
        #region IDaemonStage Members

        public abstract IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind);

        public virtual ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile, IContextBoundSettingsStore settings)
        {
            //if (!IsSupported(sourceFile))
            //{
            //    return ErrorStripeRequest.NONE;
            //}

            //var properties = sourceFile.Properties;
            //if (!properties.ProvidesCodeModel || properties.IsNonUserFile)
            //{
            //    return ErrorStripeRequest.NONE;
            //}

            //return ErrorStripeRequest.STRIPE_AND_ERRORS;
            return ErrorStripeRequest.NONE;
        }

        #endregion

        [CanBeNull]
        public static IFile GetPsiFile(IPsiSourceFile sourceFile)
        {
            var psiServices = sourceFile.GetPsiServices();
			//psiServices.Files.AssertAllDocumentAreCommited();
            return psiServices.Files.GetDominantPsiFile<CSharpLanguage>(sourceFile);
        }

        protected bool IsSupported(IPsiSourceFile sourceFile)
        {
            if (sourceFile == null || !sourceFile.IsValid())
            {
                return false;
            }

            IFile psiFile = GetPsiFile(sourceFile);
            return psiFile != null && psiFile.Language.Is<CSharpLanguage>();
        }
    }
}
