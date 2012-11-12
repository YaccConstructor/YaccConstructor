#region License and Copyright Notice
// Copyright (c) 2010 Ananth B.
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.
#endregion

using System;
using System.Linq;
using Brahma.Types;
using NUnit.Framework;
using OpenCL.Net;

namespace Brahma.OpenCL.Tests
{
    [TestFixture]
    public sealed class CompileQueryTests
    {
        public object[] GetPlatforms()
        {
            Cl.ErrorCode error;
            return (from platform in Cl.GetPlatformIDs(out error)
                    select (object)platform).ToArray();
        }

        [Test]
        [TestCaseSource("GetPlatforms")]
        [Category(Categories.Correctness)]
        [Description("Compiles the identity query for all platforms")]
        public void IdentityQuery(Cl.Platform platform)
        {
            Cl.ErrorCode error;

            Console.WriteLine("Creating compute provider for {0}",
                              Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Name, out error));

            using (var provider = new ComputeProvider(Cl.GetDeviceIDs(platform, Cl.DeviceType.All, out error)))
            {
                var query = provider.Compile<_1D, Buffer<int32>, Buffer<int32>>(
                    (range, input, output) => from r in range
                                              let index = r.GlobalID0
                                              select new[] { output[index] <= input[index] });
            }
        }
    }
}