using JetBrains.Application.Settings;
using JetBrains.ReSharper.Feature.Services.CodeCompletion.Settings;
using JetBrains.ReSharper.Resources.Settings;

namespace ReSharperExtension.Settings
{
    [SettingsKey(typeof(PatternsAndTemplatesSettingsKey), "Settings for the YaccConstructor plugin.")]
    public class YCSettingsRoot
    {
    }

    [SettingsKey(typeof (YCSettingsRoot), "Hotspot settings")]
    public class HotspotSettings
    {
        
    }

    [SettingsKey(typeof(YCSettingsRoot), "Highlihgting settings")]
    public class HighlihgtingSettings
    {

    }
}
