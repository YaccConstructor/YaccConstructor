#region License and Copyright Notice

//OpenCL.Net: .NET bindings for OpenCL

//Copyright (c) 2010 Ananth B.
//All rights reserved.

//The contents of this file are made available under the terms of the
//Eclipse Public License v1.0 (the "License") which accompanies this
//distribution, and is available at the following URL:
//http://www.opensource.org/licenses/eclipse-1.0.php

//Software distributed under the License is distributed on an "AS IS" basis,
//WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
//the specific language governing rights and limitations under the License.

//By using this software in any fashion, you are agreeing to be bound by the
//terms of the License.

#endregion

using System;
using System.Linq;

using NUnit.Framework;
using System.Runtime.InteropServices;

namespace OpenCL.Net.Tests
{
    [TestFixture]
    public sealed class StatelessAPITests
    {
        [Test]
        public void PlatformQueries()
        {
            uint platformCount;
            Cl.ErrorCode result = Cl.GetPlatformIDs(0, null, out platformCount);
            Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform count");
            Console.WriteLine("{0} platforms found", platformCount);

            var platformIds = new Cl.Platform[platformCount];
            result = Cl.GetPlatformIDs(platformCount, platformIds, out platformCount);
            Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform ids");

            foreach (Cl.Platform platformId in platformIds)
            {
                IntPtr paramSize;
                result = Cl.GetPlatformInfo(platformId, Cl.PlatformInfo.Name, IntPtr.Zero, Cl.InfoBuffer.Empty, out paramSize);
                Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform name size");

                using (var buffer = new Cl.InfoBuffer(paramSize))
                {
                    result = Cl.GetPlatformInfo(platformIds[0], Cl.PlatformInfo.Name, paramSize, buffer, out paramSize);
                    Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform name string");

                    Console.WriteLine("Platform: {0}", buffer);
                }
            }
        }

        [Test]
        public void PlatformQueries2()
        {
            Cl.ErrorCode error;
            foreach (Cl.Platform platform in Cl.GetPlatformIDs(out error))
                Console.WriteLine("Platform Name: {0}, version {1}\nPlatform Vendor: {2}",
                                  Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Name, out error),
                                  Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Version, out error),
                                  Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Vendor, out error));
        }

        [Test]
        public void DeviceQueries()
        {
            uint platformCount;
            Cl.ErrorCode result = Cl.GetPlatformIDs(0, null, out platformCount);
            Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform count");
            Console.WriteLine("{0} platforms found", platformCount);

            var platformIds = new Cl.Platform[platformCount];
            result = Cl.GetPlatformIDs(platformCount, platformIds, out platformCount);
            Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform ids");

            foreach (Cl.Platform platformId in platformIds)
            {
                IntPtr paramSize;
                result = Cl.GetPlatformInfo(platformId, Cl.PlatformInfo.Name, IntPtr.Zero, Cl.InfoBuffer.Empty, out paramSize);
                Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform name size");

                using (var buffer = new Cl.InfoBuffer(paramSize))
                {
                    result = Cl.GetPlatformInfo(platformIds[0], Cl.PlatformInfo.Name, paramSize, buffer, out paramSize);
                    Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get platform name string");
                }

                uint deviceCount;
                result = Cl.GetDeviceIDs(platformIds[0], Cl.DeviceType.All, 0, null, out deviceCount);
                Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get device count");

                var deviceIds = new Cl.Device[deviceCount];
                result = Cl.GetDeviceIDs(platformIds[0], Cl.DeviceType.All, deviceCount, deviceIds, out deviceCount);
                Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get device ids");

                result = Cl.GetDeviceInfo(deviceIds[0], Cl.DeviceInfo.Vendor, IntPtr.Zero, Cl.InfoBuffer.Empty, out paramSize);
                Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get device vendor name size");
                using (var buf = new Cl.InfoBuffer(paramSize))
                {
                    result = Cl.GetDeviceInfo(deviceIds[0], Cl.DeviceInfo.Vendor, paramSize, buf, out paramSize);
                    Assert.AreEqual(result, Cl.ErrorCode.Success, "Could not get device vendor name string");
                    var deviceVendor = buf.ToString();
                }
            }
        }

        [Test]
        public void DeviceQueries2()
        {
            Cl.ErrorCode error;
            foreach (Cl.Platform platform in Cl.GetPlatformIDs(out error))
                foreach (Cl.Device device in Cl.GetDeviceIDs(platform, Cl.DeviceType.All, out error))
                    Console.WriteLine("Device name: {0}", Cl.GetDeviceInfo(device, Cl.DeviceInfo.Name, out error));
        }

        [Test]
        public void ContextCreation()
        {
            Cl.ErrorCode error;
            
            // Select the device we want
            var device = (from dev in Cl.GetDeviceIDs(
                            (from platform in Cl.GetPlatformIDs(out error)
                            select platform).First(), Cl.DeviceType.Gpu, out error)
                          select dev).First();

            uint refCount;
            using (Cl.Context context = Cl.CreateContext(null, 1, new[] { device }, null, IntPtr.Zero, out error))
                refCount = Cl.GetContextInfo(context, Cl.ContextInfo.ReferenceCount, out error).CastTo<uint>();
        }
    }

    [TestFixture]
    public sealed class APITests
    {
        private Cl.Context _context;
        private Cl.Device _device;

        [TestFixtureSetUp]
        public void Setup()
        {
            Cl.ErrorCode error;

            _device = (from device in
                           Cl.GetDeviceIDs(
                               (from platform in Cl.GetPlatformIDs(out error)
                                where Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Name, out error).ToString() == "NVIDIA CUDA"
                                select platform).First(), Cl.DeviceType.Gpu, out error)
                       select device).First();

            _context = Cl.CreateContext(null, 1, new[] { _device }, null, IntPtr.Zero, out error);
        }

        [TestFixtureTearDown]
        public void Teardown()
        {
            _context.Dispose();
        }

        [Test]
        public void SupportedImageFormats()
        {
            Cl.ErrorCode error;

            Console.WriteLine(Cl.MemObjectType.Image2D);
            foreach (Cl.ImageFormat imageFormat in Cl.GetSupportedImageFormats(_context, Cl.MemFlags.ReadOnly, Cl.MemObjectType.Image2D, out error))
                Console.WriteLine("{0} {1}", imageFormat.ChannelOrder, imageFormat.ChannelType);
            
            Console.WriteLine(Cl.MemObjectType.Image3D);
            foreach (Cl.ImageFormat imageFormat in Cl.GetSupportedImageFormats(_context, Cl.MemFlags.ReadOnly, Cl.MemObjectType.Image2D, out error))
                Console.WriteLine("{0} {1}", imageFormat.ChannelOrder, imageFormat.ChannelType);
        }

        [Test]
        public void MemBufferTests()
        {
            const int bufferSize = 100;
            
            Cl.ErrorCode error;
            Random random = new Random();

            float[] values = (from value in Enumerable.Range(0, bufferSize) select (float)random.NextDouble()).ToArray();
            Cl.Mem buffer = Cl.CreateBuffer(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.ReadOnly, (IntPtr)(sizeof (float) * bufferSize), values, out error);
            Assert.AreEqual(error, Cl.ErrorCode.Success);

            Assert.AreEqual(Cl.GetMemObjectInfo(buffer, Cl.MemInfo.Type, out error).CastTo<Cl.MemObjectType>(), Cl.MemObjectType.Buffer);
            Assert.AreEqual(Cl.GetMemObjectInfo(buffer, Cl.MemInfo.Size, out error).CastTo<uint>(), values.Length * sizeof (float));

            // TODO: Verify values
            //int index = 0;
            //foreach (float value in Cl.GetMemObjectInfo(buffer, Cl.MemInfo.HostPtr, out error).CastToEnumerable<float>(Enumerable.Range(0, 100)))
            //{
            //    Assert.AreEqual(values[index], value);
            //    index++;
            //}

            buffer.Dispose();
        }

        [Test]
        public void CreateImageTests()
        {
            Cl.ErrorCode error;

            if (Cl.GetDeviceInfo(_device, Cl.DeviceInfo.ImageSupport, out error).CastTo<Cl.Bool>() == Cl.Bool.False)
            {
                Console.WriteLine("No image support");
                return;
            }

            {
                var image2DData = new float[200 * 200 * sizeof(float)];
                Cl.Mem image2D = Cl.CreateImage2D(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.ReadOnly, new Cl.ImageFormat(Cl.ChannelOrder.RGBA, Cl.ChannelType.Float),
                                                  (IntPtr)200, (IntPtr)200, (IntPtr)0, image2DData, out error);
                Assert.AreEqual(error, Cl.ErrorCode.Success);

                Assert.AreEqual(Cl.GetImageInfo(image2D, Cl.ImageInfo.Width, out error).CastTo<uint>(), 200);
                Assert.AreEqual(Cl.GetImageInfo(image2D, Cl.ImageInfo.Height, out error).CastTo<uint>(), 200);

                image2D.Dispose();
            }

            {
                var image3DData = new float[200 * 200 * 200 * sizeof(float)];
                Cl.Mem image3D = Cl.CreateImage3D(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.ReadOnly, new Cl.ImageFormat(Cl.ChannelOrder.RGBA, Cl.ChannelType.Float),
                                                  (IntPtr)200, (IntPtr)200, (IntPtr)200, IntPtr.Zero, IntPtr.Zero, image3DData, out error);
                Assert.AreEqual(error, Cl.ErrorCode.Success);

                Assert.AreEqual(Cl.GetImageInfo(image3D, Cl.ImageInfo.Width, out error).CastTo<uint>(), 200);
                Assert.AreEqual(Cl.GetImageInfo(image3D, Cl.ImageInfo.Height, out error).CastTo<uint>(), 200);
                Assert.AreEqual(Cl.GetImageInfo(image3D, Cl.ImageInfo.Depth, out error).CastTo<uint>(), 200);

                image3D.Dispose();
            }
        }

        [Test]
        public void CommandQueueAPI()
        {
            Cl.ErrorCode error;
            using (Cl.CommandQueue commandQueue = Cl.CreateCommandQueue(_context, _device, Cl.CommandQueueProperties.OutOfOrderExecModeEnable, out error))
            {
                Assert.AreEqual(Cl.ErrorCode.Success, error);

                Assert.AreEqual(1, Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.ReferenceCount, out error).CastTo<uint>());

                Cl.RetainCommandQueue(commandQueue);
                Assert.AreEqual(2, Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.ReferenceCount, out error).CastTo<uint>());

                Cl.ReleaseCommandQueue(commandQueue);
                Assert.AreEqual(1, Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.ReferenceCount, out error).CastTo<uint>());

                Assert.AreEqual(Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.Context, out error).CastTo<Cl.Context>(), _context);
                Assert.AreEqual(Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.Device, out error).CastTo<Cl.Device>(), _device);
                Assert.AreEqual(Cl.GetCommandQueueInfo(commandQueue, Cl.CommandQueueInfo.Properties, out error).CastTo<Cl.CommandQueueProperties>(),
                    Cl.CommandQueueProperties.OutOfOrderExecModeEnable);
            }
        }

        [Test]
        public void ProgramAndKernelTests()
        {
            const string correctSource = @"
                // Simple test; c[i] = a[i] + b[i]

                __kernel void add_array(__global float *a, __global float *b, __global float *c)
                {
                    int xid = get_global_id(0);
                    c[xid] = a[xid] + b[xid];
                }
                
                __kernel void sub_array(__global float *a, __global float *b, __global float *c)
                {
                    int xid = get_global_id(0);
                    c[xid] = a[xid] - b[xid];
                }

                ";
            const string sourceWithErrors = @"
                // Erroneous kernel

                __kernel void add_array(__global float *a, __global float *b, __global float *c)
                {
                    foo(); // <-- Error right here!
                    int xid = get_global_id(0);
                    c[xid] = a[xid] + b[xid];
                }";

            Cl.ErrorCode error;


            using (Cl.Program program = Cl.CreateProgramWithSource(_context, 1, new[] { sourceWithErrors }, null, out error))
            {
                Assert.AreEqual(error, Cl.ErrorCode.Success);

                error = Cl.BuildProgram(program, 1, new[] { _device }, string.Empty, null, IntPtr.Zero);
                Assert.AreNotEqual(Cl.ErrorCode.Success, error);

                Assert.AreEqual(Cl.GetProgramBuildInfo(program, _device, Cl.ProgramBuildInfo.Status, out error).CastTo<Cl.BuildStatus>(), Cl.BuildStatus.Error);

                Console.WriteLine("There were error(s) compiling the provided kernel");
                Console.WriteLine(Cl.GetProgramBuildInfo(program, _device, Cl.ProgramBuildInfo.Log, out error));
            }

            using (Cl.Program program = Cl.CreateProgramWithSource(_context, 1, new[] { correctSource }, null, out error))
            {
                Assert.AreEqual(error, Cl.ErrorCode.Success);

                error = Cl.BuildProgram(program, 1, new[] { _device }, string.Empty, null, IntPtr.Zero);
                Assert.AreEqual(Cl.ErrorCode.Success, error);

                Assert.AreEqual(Cl.GetProgramBuildInfo(program, _device, Cl.ProgramBuildInfo.Status, out error).CastTo<Cl.BuildStatus>(), Cl.BuildStatus.Success);

                // Try to get information from the program
                Assert.AreEqual(Cl.GetProgramInfo(program, Cl.ProgramInfo.Context, out error).CastTo<Cl.Context>(), _context);
                Assert.AreEqual(Cl.GetProgramInfo(program, Cl.ProgramInfo.NumDevices, out error).CastTo<int>(), 1);
                Assert.AreEqual(Cl.GetProgramInfo(program, Cl.ProgramInfo.Devices, out error).CastTo<Cl.Device>(0), _device);

                Console.WriteLine("Program source was:");
                Console.WriteLine(Cl.GetProgramInfo(program, Cl.ProgramInfo.Source, out error));

                Cl.Kernel kernel = Cl.CreateKernel(program, "add_array", out error);
                Assert.AreEqual(error, Cl.ErrorCode.Success);

                kernel.Dispose();

                Cl.Kernel[] kernels = Cl.CreateKernelsInProgram(program, out error);
                Assert.AreEqual(error, Cl.ErrorCode.Success);
                Assert.AreEqual(kernels.Length, 2);
                Assert.AreEqual("add_array", Cl.GetKernelInfo(kernels[0], Cl.KernelInfo.FunctionName, out error).ToString());
                Assert.AreEqual("sub_array", Cl.GetKernelInfo(kernels[1], Cl.KernelInfo.FunctionName, out error).ToString());
            }
        }

        [Test]
        // Partially from OpenTK demo - Submitted by "mfagerlund"
        public void AddArrayAddsCorrectly()
        {
            const string correctSource = @"
                // Simple test; c[i] = a[i] + b[i]

                __kernel void add_array(__global float *a, __global float *b, __global float *c)
                {
                    int xid = get_global_id(0);
                    c[xid] = a[xid] + b[xid];
                }
                
                __kernel void sub_array(__global float *a, __global float *b, __global float *c)
                {
                    int xid = get_global_id(0);
                    c[xid] = a[xid] - b[xid];
                }

                ";

            Cl.ErrorCode error;

            using (Cl.Program program = Cl.CreateProgramWithSource(_context, 1, new[] { correctSource }, null, out error))
            {
                Assert.AreEqual(error, Cl.ErrorCode.Success);
                error = Cl.BuildProgram(program, 1, new[] { _device }, string.Empty, null, IntPtr.Zero);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                Assert.AreEqual(Cl.GetProgramBuildInfo(program, _device, Cl.ProgramBuildInfo.Status, out error).CastTo<Cl.BuildStatus>(), Cl.BuildStatus.Success);

                Cl.Kernel[] kernels = Cl.CreateKernelsInProgram(program, out error);
                Cl.Kernel kernel = kernels[0];

                const int cnBlockSize = 4;
                const int cnBlocks = 3;
                IntPtr cnDimension = new IntPtr(cnBlocks * cnBlockSize);

                // allocate host  vectors
                float[] A = new float[cnDimension.ToInt32()];
                float[] B = new float[cnDimension.ToInt32()];
                float[] C = new float[cnDimension.ToInt32()];

                // initialize host memory
                Random rand = new Random();
                for (int i = 0; i < A.Length; i++)
                {
                    A[i] = rand.Next() % 256;
                    B[i] = rand.Next() % 256;
                }

                Cl.Mem hDeviceMemA = Cl.CreateBuffer(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.ReadOnly, (IntPtr)(sizeof(float) * cnDimension.ToInt32()), A, out error);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                Cl.Mem hDeviceMemB = Cl.CreateBuffer(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.ReadOnly, (IntPtr)(sizeof(float) * cnDimension.ToInt32()), B, out error);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                Cl.Mem hDeviceMemC = Cl.CreateBuffer(_context, Cl.MemFlags.CopyHostPtr | Cl.MemFlags.WriteOnly, (IntPtr)(sizeof(float) * cnDimension.ToInt32()), IntPtr.Zero, out error);
                Assert.AreEqual(Cl.ErrorCode.Success, error);

                Cl.CommandQueue cmdQueue = Cl.CreateCommandQueue(_context, _device, (Cl.CommandQueueProperties)0, out error);

                Cl.Event clevent;

                int intPtrSize = 0;
                intPtrSize = Marshal.SizeOf(typeof(IntPtr));

                // setup parameter values
                error = Cl.SetKernelArg(kernel, 0, new IntPtr(intPtrSize), hDeviceMemA);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                error = Cl.SetKernelArg(kernel, 1, new IntPtr(intPtrSize), hDeviceMemB);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                error = Cl.SetKernelArg(kernel, 2, new IntPtr(intPtrSize), hDeviceMemC);
                Assert.AreEqual(Cl.ErrorCode.Success, error);

                // write data from host to device
                error = Cl.EnqueueWriteBuffer(cmdQueue, hDeviceMemA, Cl.Bool.True, IntPtr.Zero,
                    new IntPtr(cnDimension.ToInt32() * sizeof(float)),
                    A, 0, null, out clevent);
                Assert.AreEqual(Cl.ErrorCode.Success, error);
                error = Cl.EnqueueWriteBuffer(cmdQueue, hDeviceMemB, Cl.Bool.True, IntPtr.Zero,
                    new IntPtr(cnDimension.ToInt32() * sizeof(float)),
                    B, 0, null, out clevent);
                Assert.AreEqual(Cl.ErrorCode.Success, error);

                // execute kernel
                error = Cl.EnqueueNDRangeKernel(cmdQueue, kernel, 1, null, new IntPtr[] { cnDimension }, null, 0, null, out clevent);
                Assert.AreEqual(Cl.ErrorCode.Success, error, error.ToString());

                // copy results from device back to host
                IntPtr event_handle = IntPtr.Zero;

                error = Cl.EnqueueReadBuffer(
                    cmdQueue, hDeviceMemC, Cl.Bool.True, IntPtr.Zero,
                    new IntPtr(cnDimension.ToInt32() * sizeof(float)),
                    C, 0, null, out clevent);
                Assert.AreEqual(Cl.ErrorCode.Success, error, error.ToString());

                for (int i = 0; i < A.Length; i++)
                {
                    Assert.That(A[i] + B[i], Is.EqualTo(C[i]));
                }

                Cl.Finish(cmdQueue);

                Cl.ReleaseMemObject(hDeviceMemA);
                Cl.ReleaseMemObject(hDeviceMemB);
                Cl.ReleaseMemObject(hDeviceMemC);
            }
        }
    }
}