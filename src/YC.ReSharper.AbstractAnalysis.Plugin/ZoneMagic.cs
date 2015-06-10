using JetBrains.Application.BuildScript.Application.Zones;

namespace YC.ReSharper.AbstractAnalysis
{
    [ZoneDefinition(ZoneFlags.AutoEnable)]
    [ZoneDefinitionConfigurableFeature("YC Plugins", "Support for string-embedded languages", false)]
    public interface IYcPluginZone : IZone
    {
    }

    [ZoneMarker]
    public class ZoneMarker : IRequire<IYcPluginZone>
    {
    }
}
