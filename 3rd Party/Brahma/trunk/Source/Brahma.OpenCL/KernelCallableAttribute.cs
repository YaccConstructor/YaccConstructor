using System;

namespace Brahma.OpenCL
{
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
    internal sealed class KernelCallableAttribute: Attribute
    {
    }
}
