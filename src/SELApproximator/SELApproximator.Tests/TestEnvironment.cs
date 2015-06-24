using JetBrains.TestFramework;
using JetBrains.TestFramework.Application.Zones;
using NUnit.Framework;

/// <summary>
/// Test environment. Must be in the global namespace.
/// </summary>
[SetUpFixture]
public class TestEnvironmentAssembly : ExtensionTestEnvironmentAssembly<ITestsZone>
{
}