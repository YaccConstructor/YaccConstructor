using JetBrains.Application.BuildScript.Application.Zones;
using JetBrains.Application.Environment;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Tree;

namespace ReSharperExtension
{
    [ZoneDefinition(ZoneFlags.AutoEnable)]
    [ZoneDefinitionConfigurableFeature("ReSharperExtension", "Support for dynamic language loading", false)]
    public interface IMyZone : IZone
    {
    }

    [ZoneMarker]
    public class ZoneMarker : IRequire<IMyZone>
    {
    }

    public interface IReSharperLanguage : YC.SDK.CommonInterfaces.IInjectedLanguageModule<ICSharpLiteralExpression, DocumentRange, ITreeNode>
    {
    }
}
