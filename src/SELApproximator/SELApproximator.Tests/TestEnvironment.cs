using JetBrains.Application.BuildScript.Application.Zones;
using JetBrains.ReSharper.TestFramework;
using JetBrains.TestFramework;
using JetBrains.TestFramework.Application.Zones;
using NUnit.Framework;

// See Zones below
[ZoneDefinition]
public interface ISELPluginTestZone : ITestsZone, IRequire<PsiFeatureTestZone>
{
}

[SetUpFixture]
public class TestEnvironment : ExtensionTestEnvironmentAssembly<ISELPluginTestZone>
{
}