using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.ProjectModel;
using JetBrains.ProjectModel.FileTypes;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public interface IMyInspectionsProcessFactory
    {
        IDaemonStageProcess CreateInspectionsProcess(IDaemonProcess process, IContextBoundSettingsStore settings);
    }

    [DaemonStage(StagesBefore = new[] { typeof(MySmartResolverStage), typeof(MyFileIndexStage) })]
    public class InspectionsStage : MyDaemonStageBase
    {
        private readonly ProjectFileTypeServices myServices;

        public InspectionsStage(ProjectFileTypeServices services)
        {
            myServices = services;
        }

        public override ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile, IContextBoundSettingsStore settings)
        {
            return ErrorStripeRequest.NONE;
        }

        public override IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            if (!IsSupported(process.SourceFile))
                return EmptyList<IDaemonStageProcess>.InstanceList;

            var factory = myServices.TryGetService<IMyInspectionsProcessFactory>(process.SourceFile.LanguageType);
            if (factory == null)
                return EmptyList<IDaemonStageProcess>.InstanceList;

            return new List<IDaemonStageProcess> { factory.CreateInspectionsProcess(process, settings) };
        }
    }

    [ProjectFileType(typeof(KnownProjectFileType))]
    public sealed class MyInspectionsProcessFactory : IMyInspectionsProcessFactory
    {
        #region IMyInspectionsProcessFactory Members

        public IDaemonStageProcess CreateInspectionsProcess(IDaemonProcess process, IContextBoundSettingsStore settings)
        {
            return new InspectionsProcess(process, settings);
        }

        #endregion
    }
}
