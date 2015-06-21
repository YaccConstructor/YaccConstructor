using JetBrains.Application.BuildScript.Application.Zones;

namespace ReSharperExtension
{
    [ZoneDefinition(ZoneFlags.AutoEnable)]
    [ZoneDefinitionConfigurableFeature("YCReSharperExtension", "YC Support for dynamic language loading", false)]
    public interface IMyZone : IZone
    {
    }

    [ZoneMarker]
    public class ZoneMarker : IRequire<IMyZone>
    {
    }
}
