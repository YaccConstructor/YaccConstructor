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
using System.Runtime.InteropServices;

namespace OpenCL.Net
{
    #region Basic type aliases

    using cl_uint = UInt32;

    #endregion

    public static partial class Cl
    {
        public const string Library = "opencl.dll";
        
        #region Platform API

        [DllImport(Library)]
        private static extern ErrorCode clGetPlatformIDs(cl_uint numEntries,
                                                         [Out] [MarshalAs(UnmanagedType.LPArray)] Platform[] platforms,
                                                         out cl_uint numPlatforms);
        public static ErrorCode GetPlatformIDs(cl_uint numEntries,
                                               Platform[] platforms,
                                               out cl_uint numPlatforms)
        {
            return clGetPlatformIDs(numEntries, platforms, out numPlatforms);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetPlatformInfo(IntPtr platform,
                                                          PlatformInfo paramName,
                                                          IntPtr paramValueSize,
                                                          IntPtr paramValue,
                                                          out IntPtr paramValueSizeRet);
        public static ErrorCode GetPlatformInfo(Platform platformId,
                                                PlatformInfo paramName,
                                                IntPtr paramValueBufferSize,
                                                InfoBuffer paramValue,
                                                out IntPtr paramValueSize)
        {
            return clGetPlatformInfo((platformId as IHandle).Handle, paramName, paramValueBufferSize, paramValue.Address, out paramValueSize);
        }

        #endregion

        #region Device API

        [DllImport(Library)]
        private static extern ErrorCode clGetDeviceIDs(IntPtr platform,
                                                       DeviceType deviceType,
                                                       cl_uint numEntries,
                                                       [Out] [MarshalAs(UnmanagedType.LPArray)] Device[] devices,
                                                       out cl_uint numDevices);
        public static ErrorCode GetDeviceIDs(Platform platform,
                                             DeviceType deviceType,
                                             cl_uint numEntries,
                                             Device[] devices,
                                             out cl_uint numDevices)
        {
            return clGetDeviceIDs((platform as IHandle).Handle, deviceType, numEntries, devices, out numDevices);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetDeviceInfo(IntPtr device,
                                                        DeviceInfo paramName,
                                                        IntPtr paramValueSize,
                                                        IntPtr paramValue,
                                                        out IntPtr paramValueSizeRet);
        public static ErrorCode GetDeviceInfo(Device device,
                                              DeviceInfo paramName,
                                              IntPtr paramValueSize,
                                              InfoBuffer paramValue,
                                              out IntPtr paramValueSizeRet)
        {
            return clGetDeviceInfo((device as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Context API

        [DllImport(Library)]
        private static extern IntPtr clCreateContext([In] [MarshalAs(UnmanagedType.LPArray)] ContextProperty[] properties,
                                                     cl_uint numDevices,
                                                     [In] [MarshalAs(UnmanagedType.LPArray)] Device[] devices,
                                                     ContextNotify pfnNotify,
                                                     IntPtr userData,
                                                     out ErrorCode errcodeRet);
        public static Context CreateContext(ContextProperty[] properties,
                                               cl_uint numDevices,
                                               Device[] devices,
                                               ContextNotify pfnNotify,
                                               IntPtr userData,
                                               out ErrorCode errcodeRet)
        {
            return new Context(clCreateContext(properties, numDevices, devices, pfnNotify, userData, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern IntPtr clCreateContextFromType([In] [MarshalAs(UnmanagedType.LPArray)] ContextProperty[] properties,
                                                             DeviceType deviceType,
                                                             ContextNotify pfnNotify,
                                                             IntPtr userData,
                                                             [Out] [MarshalAs(UnmanagedType.I4)] out ErrorCode errcodeRet);
        public static Context CreateContextFromType(ContextProperty[] properties,
                                                       DeviceType deviceType,
                                                       ContextNotify pfnNotify,
                                                       IntPtr userData,
                                                       out ErrorCode errcodeRet)
        {
            return new Context(clCreateContextFromType(properties, deviceType, pfnNotify, userData, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainContext(IntPtr context);
        public static ErrorCode RetainContext(Context context)
        {
            return clRetainContext((context as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseContext(IntPtr context);
        public static ErrorCode ReleaseContext(Context context)
        { 
            return clReleaseContext((context as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetContextInfo(IntPtr context,
                                                         ContextInfo paramName,
                                                         IntPtr paramValueSize,
                                                         IntPtr paramValue,
                                                         out IntPtr paramValueSizeRet);
        public static ErrorCode GetContextInfo(Context context,
                                               ContextInfo paramName,
                                               IntPtr paramValueSize,
                                               InfoBuffer paramValue,
                                               out IntPtr paramValueSizeRet)
        {
            return clGetContextInfo((context as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Memory Object API

        [DllImport(Library)]
        private static extern IntPtr clCreateBuffer(IntPtr context, 
                                                    MemFlags flags, 
                                                    IntPtr size, IntPtr hostPtr,
                                                    [Out] [MarshalAs(UnmanagedType.I4)] out ErrorCode errcodeRet);
        public static Mem CreateBuffer(Context context, MemFlags flags, IntPtr size, object hostData, out ErrorCode errcodeRet)
        {
            using (var hostPtr = hostData.Pin())
                return new Mem(clCreateBuffer((context as IHandle).Handle, flags, size, hostPtr, out errcodeRet));
        }

        public static Mem CreateBuffer(Context context, MemFlags flags, IntPtr size, IntPtr hostPtr, out ErrorCode errcodeRet)
        {
            return new Mem(clCreateBuffer((context as IHandle).Handle, flags, size, hostPtr, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainMemObject(IntPtr memObj);
        public static ErrorCode RetainMemObject(Mem memObj)
        {
            return clRetainMemObject((memObj as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseMemObject(IntPtr memObj);
        public static ErrorCode ReleaseMemObject(Mem memObj)
        {
            return clReleaseMemObject((memObj as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetSupportedImageFormats(IntPtr context,
                                                                   MemFlags flags,
                                                                   MemObjectType imageType,
                                                                   cl_uint numEntries,
                                                                   [Out] [MarshalAs(UnmanagedType.LPArray)] ImageFormat[] imageFormats,
                                                                   out cl_uint numImageFormats);
        public static ErrorCode GetSupportedImageFormats(Context context, 
                                                         MemFlags flags,
                                                         MemObjectType imageType,
                                                         cl_uint numEntries,
                                                         ImageFormat[] imageFormats,
                                                         out cl_uint numImageFormats)
        {
            return clGetSupportedImageFormats((context as IHandle).Handle, flags, imageType, numEntries, imageFormats, out numImageFormats);
        }

        [DllImport(Library)]
        private static extern IntPtr clCreateImage2D(IntPtr context,
                                                     MemFlags flags,
                                                     IntPtr imageFormat,
                                                     IntPtr imageWidth,
                                                     IntPtr imageHeight,
                                                     IntPtr imageRowPitch,
                                                     IntPtr hostPtr,
                                                     out ErrorCode errcodeRet);
        public static Mem CreateImage2D(Context context,
                                        MemFlags flags,
                                        ImageFormat imageFormat,
                                        IntPtr imageWidth,
                                        IntPtr imageHeight,
                                        IntPtr imageRowPitch,
                                        object hostData,
                                        out ErrorCode errorcodeRet)
        {
            using (var hostPtr = hostData.Pin())
            using (var imageFormatPtr = imageFormat.Pin())
                return new Mem(clCreateImage2D((context as IHandle).Handle, flags, imageFormatPtr, imageWidth, imageHeight, imageRowPitch, hostPtr, out errorcodeRet));
        }
        public static Mem CreateImage2D(Context context,
                                        MemFlags flags,
                                        ImageFormat imageFormat,
                                        IntPtr imageWidth,
                                        IntPtr imageHeight,
                                        IntPtr imageRowPitch,
                                        IntPtr hostPtr,
                                        out ErrorCode errorcodeRet)
        {
            using (var imageFormatPtr = imageFormat.Pin())
                return new Mem(clCreateImage2D((context as IHandle).Handle, flags, imageFormatPtr, imageWidth, imageHeight, imageRowPitch, hostPtr, out errorcodeRet));
        }

        [DllImport(Library)]
        private static extern IntPtr clCreateImage3D(IntPtr context,
                                                     MemFlags flags,
                                                     IntPtr imageFormat,
                                                     IntPtr imageWidth,
                                                     IntPtr imageHeight,
                                                     IntPtr imageDepth,
                                                     IntPtr imageRowPitch,
                                                     IntPtr imageSlicePitch,
                                                     IntPtr hostPtr,
                                                     out ErrorCode errcodeRet);
        public static Mem CreateImage3D(Context context,
                                        MemFlags flags,
                                        ImageFormat imageFormat,
                                        IntPtr imageWidth,
                                        IntPtr imageHeight,
                                        IntPtr imageDepth,
                                        IntPtr imageRowPitch,
                                        IntPtr imageSlicePitch,
                                        object hostData,
                                        out ErrorCode errcodeRet)
        {
            using (var hostPtr = hostData.Pin())
            using (var imageFormatPtr = imageFormat.Pin())
                return new Mem(clCreateImage3D((context as IHandle).Handle, flags, imageFormatPtr, imageWidth, imageHeight, imageDepth, imageRowPitch, imageSlicePitch, hostPtr, out errcodeRet));
        }
        public static Mem CreateImage3D(Context context,
                                        MemFlags flags,
                                        ImageFormat imageFormat,
                                        IntPtr imageWidth,
                                        IntPtr imageHeight,
                                        IntPtr imageDepth,
                                        IntPtr imageRowPitch,
                                        IntPtr imageSlicePitch,
                                        IntPtr hostPtr,
                                        out ErrorCode errcodeRet)
        {
            using (var imageFormatPtr = imageFormat.Pin())
                return new Mem(clCreateImage3D((context as IHandle).Handle, flags, imageFormatPtr, imageWidth, imageHeight, imageDepth, imageRowPitch, imageSlicePitch, hostPtr, out errcodeRet));
        }


        [DllImport(Library)]
        private static extern ErrorCode clGetMemObjectInfo(IntPtr memObj, 
                                                           MemInfo paramName, 
                                                           IntPtr paramValueSize, 
                                                           IntPtr paramValue, 
                                                           out IntPtr paramValueSizeRet);
        public static ErrorCode GetMemObjectInfo(Mem memObj,
                                                 MemInfo paramName,
                                                 IntPtr paramValueSize,
                                                 InfoBuffer paramValue,
                                                 out IntPtr paramValueSizeRet)
        {
            return clGetMemObjectInfo((memObj as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetImageInfo(IntPtr image, 
                                                       ImageInfo paramName, 
                                                       IntPtr paramValueSize, 
                                                       IntPtr paramValue, 
                                                       out IntPtr paramValueSizeRet);
        public static ErrorCode GetImageInfo(Mem image,
                                             ImageInfo paramName,
                                             IntPtr paramValueSize,
                                             InfoBuffer paramValue,
                                             out IntPtr paramValueSizeRet)
        {
            return clGetImageInfo((image as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Program Object API

        [DllImport(Library)]
        private static extern ErrorCode clUnloadCompiler();
        public static ErrorCode UnloadCompiler()
        {
            return clUnloadCompiler();
        }

        [DllImport(Library)]
        private static extern IntPtr clCreateProgramWithSource(Context context,
                                                               cl_uint count,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1)] string[] strings,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1)] IntPtr[] lengths,
                                                               out ErrorCode errcodeRet);
        public static Program CreateProgramWithSource(Context context,
                                                         cl_uint count,
                                                         string[] strings,
                                                         IntPtr[] lengths,
                                                         out ErrorCode errcodeRet)
        {
            return new Program(clCreateProgramWithSource(context, count, strings, lengths, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern IntPtr clCreateProgramWithBinary(IntPtr context,
                                                               cl_uint numDevices,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1, ArraySubType = UnmanagedType.SysUInt)] Device[] deviceList,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1, ArraySubType = UnmanagedType.SysUInt)] IntPtr[] lengths,
                                                               [In] [MarshalAs(UnmanagedType.SysUInt)] IntPtr binaries,
                                                               [Out] [MarshalAs(UnmanagedType.SysUInt)] IntPtr binaryStatus,
                                                               out ErrorCode errcodeRet);
        public static Program CreateProgramWithBinary(Context context,
                                                         cl_uint numDevices,
                                                         Device[] deviceList,
                                                         IntPtr[] lengths,
                                                         InfoBufferArray binaries,
                                                         InfoBufferArray<ErrorCode> binariesStatus,
                                                         out ErrorCode errcodeRet)
        {
            using (var binariesPtr = binaries.Array.Pin())
            using (var binariesStatusPtr = binariesStatus.Array.Pin())
                return new Program(clCreateProgramWithBinary((context as IHandle).Handle, numDevices, deviceList, lengths, binariesPtr, binariesStatusPtr, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainProgram(IntPtr program);
        public static ErrorCode RetainProgram(Program program)
        {
            return clRetainProgram((program as IRefCountedHandle).Handle);
        }


        [DllImport(Library)]
        private static extern ErrorCode clReleaseProgram(IntPtr program);
        public static ErrorCode ReleaseProgram(Program program)
        {
            return clReleaseProgram((program as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clBuildProgram(IntPtr program,
                                                       cl_uint numDevices,
                                                       [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1)] Device[] deviceList,
                                                       [In] [MarshalAs(UnmanagedType.LPStr)] string options,
                                                       ProgramNotify pfnNotify,
                                                       IntPtr userData);
        public static ErrorCode BuildProgram(Program program,
                                             cl_uint numDevices,
                                             Device[] deviceList,
                                             string options,
                                             ProgramNotify pfnNotify,
                                             IntPtr userData)
        {
            return clBuildProgram((program as IHandle).Handle, numDevices, deviceList, options, pfnNotify, userData);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetProgramInfo(IntPtr program,
                                                         ProgramInfo paramName,
                                                         IntPtr paramValueSize,
                                                         IntPtr paramValue,
                                                         out IntPtr paramValueSizeRet);
        public static ErrorCode GetProgramInfo(Program program,
                                               ProgramInfo paramName,
                                               IntPtr paramValueSize,
                                               InfoBuffer paramValue,
                                               out IntPtr paramValueSizeRet)
        {
             return clGetProgramInfo((program as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        public static ErrorCode GetProgramInfo(Program program,
                                               ProgramInfo paramName,
                                               IntPtr paramValueSize,
                                               InfoBufferArray paramValues,
                                               out IntPtr paramValueSizeRet)
        {
            using (var paramValuesPtr = paramValues.Array.Pin())
                return clGetProgramInfo((program as IHandle).Handle, paramName, paramValueSize, paramValuesPtr,
                                        out paramValueSizeRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetProgramBuildInfo(IntPtr program,
                                                              IntPtr device,
                                                              ProgramBuildInfo paramName,
                                                              IntPtr paramValueSize,
                                                              IntPtr paramValue,
                                                              out IntPtr paramValueSizeRet);
        public static ErrorCode GetProgramBuildInfo(Program program,
                                                    Device device,
                                                    ProgramBuildInfo paramName,
                                                    IntPtr paramValueSize,
                                                    InfoBuffer paramValue,
                                                    out IntPtr paramValueSizeRet)
        {
            return clGetProgramBuildInfo((program as IHandle).Handle, (device as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Kernel Object API

        [DllImport(Library)]
        private static extern IntPtr clCreateKernel(IntPtr program,
                                                    [In] [MarshalAs(UnmanagedType.LPStr)] string kernelName,
                                                    out ErrorCode errcodeRet);
        public static Kernel CreateKernel(Program program, string kernelName, out ErrorCode errcodeRet)
        {
            return new Kernel(clCreateKernel((program as IHandle).Handle, kernelName, out errcodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clCreateKernelsInProgram(IntPtr program,
                                                                 uint numKernels,
                                                                 [Out] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1)] Kernel[] kernels,
                                                                 out uint numKernelsRet);
        public static ErrorCode CreateKernelsInProgram(Program program,
                                                       uint numKernels,
                                                       Kernel[] kernels,
                                                       out uint numKernelsRet)
        {
            return clCreateKernelsInProgram((program as IHandle).Handle, numKernels, kernels, out numKernelsRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainKernel(IntPtr kernel);
        public static ErrorCode RetainKernel(Kernel kernel)
        {
            return clRetainKernel((kernel as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseKernel(IntPtr kernel);
        public static ErrorCode ReleaseKernel(Kernel kernel)
        {
            return clReleaseKernel((kernel as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clSetKernelArg(IntPtr kernel, uint argIndex, IntPtr argSize, IntPtr argValue);
        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, IntPtr argSize, object argValue)
        {
            using (var argPtr = argValue.Pin())
                return clSetKernelArg((kernel as IHandle).Handle, argIndex, argSize, argPtr);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetKernelInfo(IntPtr kernel,
                                                        KernelInfo paramName,
                                                        IntPtr paramValueSize,
                                                        IntPtr paramValue,
                                                        out IntPtr paramValueSizeRet);
        public static ErrorCode GetKernelInfo(Kernel kernel,
                                              KernelInfo paramName,
                                              IntPtr paramValueSize,
                                              InfoBuffer paramValue,
                                              out IntPtr paramValueSizeRet)
        {
            return clGetKernelInfo((kernel as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetKernelWorkGroupInfo(IntPtr kernel, IntPtr device, KernelWorkGroupInfo paramName,
                                                                 IntPtr paramValueSize, IntPtr paramValue, out IntPtr paramValueSizeRet);
        public static ErrorCode GetKernelWorkGroupInfo(Kernel kernel, Device device, KernelWorkGroupInfo paramName,
                                                       IntPtr paramValueSize, InfoBuffer paramValue, out IntPtr paramValueSizeRet)
        {
            return clGetKernelWorkGroupInfo((kernel as IHandle).Handle, (device as IHandle).Handle,
                                            paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Command Queue API

        [DllImport(Library)]
        private static extern IntPtr clCreateCommandQueue(IntPtr context, IntPtr device,
                                                          [MarshalAs(UnmanagedType.U8)] CommandQueueProperties properties,
                                                          out ErrorCode error);
        public static CommandQueue CreateCommandQueue(Context context, Device device, CommandQueueProperties properties, out ErrorCode error)
        {
            return new CommandQueue(clCreateCommandQueue((context as IHandle).Handle, (device as IHandle).Handle, properties, out error));
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainCommandQueue(IntPtr commandQueue);
        public static ErrorCode RetainCommandQueue(CommandQueue commandQueue)
        {
            return clRetainCommandQueue((commandQueue as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseCommandQueue(IntPtr commandQueue);
        public static ErrorCode ReleaseCommandQueue(CommandQueue commandQueue)
        {
            return clReleaseCommandQueue((commandQueue as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetCommandQueueInfo(IntPtr commandQueue,
                                                              [MarshalAs(UnmanagedType.U4)] CommandQueueInfo paramName,
                                                              IntPtr paramValueSize,
                                                              IntPtr paramValue,
                                                              out IntPtr paramValueSizeRet);
        public static ErrorCode GetCommandQueueInfo(CommandQueue commandQueue,
                                                    CommandQueueInfo paramName,
                                                    IntPtr paramValueSize,
                                                    InfoBuffer paramValue,
                                                    out IntPtr paramValueSizeRet)
        {
            return clGetCommandQueueInfo((commandQueue as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clSetCommandQueueProperty(IntPtr commandQueue,
                                                                  [MarshalAs(UnmanagedType.U8)] CommandQueueProperties properties,
                                                                  bool enable, 
                                                                  [MarshalAs(UnmanagedType.U8)] out CommandQueueProperties oldProperties);
        public static ErrorCode SetCommandQueueProperty(CommandQueue commandQueue,
                                                        CommandQueueProperties properties,
                                                        bool enable, out CommandQueueProperties oldProperties)
        {
            return clSetCommandQueueProperty((commandQueue as IHandle).Handle, properties, enable, out oldProperties);
        }

        #endregion

        #region Queue Command API

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueReadBuffer(IntPtr commandQueue, 
                                                            IntPtr buffer,
                                                            Bool blockingRead,
                                                            IntPtr offset,
                                                            IntPtr cb,
                                                            IntPtr ptr,
                                                            uint numEventsInWaitList,
                                                            [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                            [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueReadBuffer(CommandQueue commandQueue,
                                                  Mem buffer,
                                                  Bool blockingRead,
                                                  IntPtr offset,
                                                  IntPtr cb,
                                                  object data,
                                                  uint numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e)
        {
            using (var dataPtr = data.Pin())
                return clEnqueueReadBuffer((commandQueue as IHandle).Handle, (buffer as IHandle).Handle, 
                                           blockingRead, offset, cb, dataPtr, numEventsInWaitList, eventWaitList, out e);
        }
        public static ErrorCode EnqueueReadBuffer(CommandQueue commandQueue,
                                                  Mem buffer,
                                                  Bool blockingRead,
                                                  IntPtr offset,
                                                  IntPtr cb,
                                                  IntPtr data,
                                                  uint numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e)
        {
                return clEnqueueReadBuffer((commandQueue as IHandle).Handle, (buffer as IHandle).Handle,
                                           blockingRead, offset, cb, data, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueWriteBuffer(IntPtr commandQueue,
                                                             IntPtr buffer,
                                                             Bool blockingWrite,
                                                             IntPtr offset,
                                                             IntPtr cb,
                                                             IntPtr ptr,
                                                             uint numEventsInWaitList,
                                                             [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                             [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueWriteBuffer(CommandQueue commandQueue,
                                                   Mem buffer,
                                                   Bool blockingWrite,
                                                   IntPtr offset,
                                                   IntPtr cb,
                                                   object data,
                                                   uint numEventsInWaitList,
                                                   Event[] eventWaitList,
                                                   out Event e)
        {
            using (var dataPtr = data.Pin())
                return clEnqueueWriteBuffer((commandQueue as IHandle).Handle, (buffer as IHandle).Handle, blockingWrite, offset, cb, dataPtr, numEventsInWaitList, eventWaitList, out e);
        }
        public static ErrorCode EnqueueWriteBuffer(CommandQueue commandQueue,
                                                   Mem buffer,
                                                   Bool blockingWrite,
                                                   IntPtr offset,
                                                   IntPtr cb,
                                                   IntPtr data,
                                                   uint numEventsInWaitList,
                                                   Event[] eventWaitList,
                                                   out Event e)
        {
            return clEnqueueWriteBuffer((commandQueue as IHandle).Handle, (buffer as IHandle).Handle, blockingWrite, offset, cb, data, numEventsInWaitList, eventWaitList, out e);
        }


        [DllImport(Library)]
        private static extern ErrorCode clEnqueueCopyBuffer(IntPtr commandQueue,
                                                            IntPtr srcBuffer,
                                                            IntPtr dstBuffer,
                                                            IntPtr srcOffset,
                                                            IntPtr dstOffset,
                                                            IntPtr cb,
                                                            uint numEventsInWaitList,
                                                            [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                            [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueCopyBuffer(CommandQueue commandQueue,
                                                  Mem srcBuffer,
                                                  Mem dstBuffer,
                                                  IntPtr srcOffset,
                                                  IntPtr dstOffset,
                                                  IntPtr cb,
                                                  uint numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e)
        {
            return clEnqueueCopyBuffer((commandQueue as IHandle).Handle, (srcBuffer as IHandle).Handle, (dstBuffer as IHandle).Handle,
                                       srcOffset, dstOffset, cb, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueReadImage(IntPtr commandQueue,
                                                           IntPtr image,
                                                           Bool blockingRead,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] origin,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                           IntPtr rowPitch,
                                                           IntPtr slicePitch,
                                                           IntPtr ptr,
                                                           uint numEventsIntWaitList,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 8)] Event[] eventWaitList,
                                                           [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueReadImage(CommandQueue commandQueue,
                                                 Mem image,
                                                 Bool blockingRead,
                                                 IntPtr[] origin,
                                                 IntPtr[] region,
                                                 IntPtr rowPitch,
                                                 IntPtr slicePitch,
                                                 object data,
                                                 uint numEventsInWaitList,
                                                 Event[] eventWaitList,
                                                 out Event e)
        {
            using (var dataPtr = data.Pin())
                return clEnqueueReadImage((commandQueue as IHandle).Handle, (image as IHandle).Handle, 
                                          blockingRead, origin, region, rowPitch, slicePitch, dataPtr, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueWriteImage(IntPtr commandQueue,
                                                            IntPtr image,
                                                            Bool blockingWrite,
                                                            [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] origin,
                                                            [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                            IntPtr rowPitch,
                                                            IntPtr slicePitch,
                                                            IntPtr ptr,
                                                            uint numEventsIntWaitList,
                                                            [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 8)] Event[] eventWaitList,
                                                            [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueWriteImage(CommandQueue commandQueue,
                                                  Mem image,
                                                  Bool blockingWrite,
                                                  IntPtr[] origin,
                                                  IntPtr[] region,
                                                  IntPtr rowPitch,
                                                  IntPtr slicePitch,
                                                  object data,
                                                  uint numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e)
        {
            using (var dataPtr = data.Pin())
                return clEnqueueWriteImage((commandQueue as IHandle).Handle, (image as IHandle).Handle,
                                          blockingWrite, origin, region, rowPitch, slicePitch, dataPtr, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueCopyImage(IntPtr commandQueue,
                                                           IntPtr srcImage,
                                                           IntPtr dstImage,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] srcOrigin,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] dstOrigin,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                           uint numEventsInWaitList,
                                                           [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                           [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueCopyImage(CommandQueue commandQueue,
                                                 Mem srcImage,
                                                 Mem dstImage,
                                                 IntPtr[] srcOrigin,
                                                 IntPtr[] dstOrigin,
                                                 IntPtr[] region,
                                                 uint numEventsInWaitList,
                                                 Event[] eventWaitList,
                                                 out Event e)
        {
            return clEnqueueCopyImage((commandQueue as IHandle).Handle, (srcImage as IHandle).Handle, (dstImage as IHandle).Handle,
                                      srcOrigin, dstOrigin, region, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueCopyImageToBuffer(IntPtr commandQueue,
                                                                   IntPtr srcImage,
                                                                   IntPtr dstBuffer,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] srcOrigin,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                                   IntPtr dstOffset,
                                                                   uint numEventsInWaitList,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                                   [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueCopyImageToBuffer(CommandQueue commandQueue,
                                                         Mem srcImage,
                                                         Mem dstBuffer,
                                                         IntPtr[] srcOrigin,
                                                         IntPtr[] region,
                                                         IntPtr dstOffset,
                                                         uint numEventsInWaitList,
                                                         Event[] eventWaitList,
                                                         out Event e)
        {
            return clEnqueueCopyImageToBuffer((commandQueue as IHandle).Handle, (srcImage as IHandle).Handle, (dstBuffer as IHandle).Handle,
                srcOrigin, region, dstOffset, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueCopyBufferToImage(IntPtr commandQueue,
                                                                   IntPtr srcBuffer,
                                                                   IntPtr dstImage,
                                                                   IntPtr srcOffset,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] dstOrigin,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                                   uint numEventsInWaitList,
                                                                   [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                                   [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueCopyBufferToImage(CommandQueue commandQueue,
                                                         Mem srcBuffer,
                                                         Mem dstImage,
                                                         IntPtr srcOffset,
                                                         IntPtr[] dstOrigin,
                                                         IntPtr[] region,
                                                         uint numEventsInWaitList,
                                                         Event[] eventWaitList,
                                                         out Event e)
        {
            return clEnqueueCopyBufferToImage((commandQueue as IHandle).Handle, (srcBuffer as IHandle).Handle, (dstImage as IHandle).Handle,
                srcOffset, dstOrigin, region, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern IntPtr clEnqueueMapBuffer(IntPtr commandQueue,
                                                        IntPtr buffer,
                                                        Bool blockingMap,
                                                        MapFlags mapFlags,
                                                        IntPtr offset,
                                                        IntPtr cb,
                                                        uint numEventsInWaitList,
                                                        [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                        [Out] [MarshalAs(UnmanagedType.Struct)] out Event e,
                                                        out ErrorCode errCodeRet);
        public static InfoBuffer EnqueueMapBuffer(CommandQueue commandQueue,
                                                  Mem buffer,
                                                  Bool blockingMap,
                                                  MapFlags mapFlags,
                                                  IntPtr offset,
                                                  IntPtr cb,
                                                  uint numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e,
                                                  out ErrorCode errCodeRet)
        {
            return new InfoBuffer(clEnqueueMapBuffer((commandQueue as IHandle).Handle, (buffer as IHandle).Handle, 
                                                     blockingMap, mapFlags, offset, cb, numEventsInWaitList, eventWaitList, out e, out errCodeRet));
        }

        [DllImport(Library)]
        private static extern IntPtr clEnqueueMapImage(IntPtr commandQueue,
                                                       IntPtr image,
                                                       Bool blockingMap,
                                                       MapFlags mapFlags,
                                                       [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] origin,
                                                       [In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] IntPtr[] region,
                                                       out IntPtr imageRowPitch,
                                                       out IntPtr imageSlicePitch,
                                                       uint numEventsInWaitList,
                                                       [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 8)] Event[] eventWaitList,
                                                       [Out] [MarshalAs(UnmanagedType.Struct)] out Event e,
                                                       out ErrorCode errCodeRet);
        public static InfoBuffer EnqueueMapImage(CommandQueue commandQueue,
                                                 Mem image,
                                                 Bool blockingMap,
                                                 MapFlags mapFlags,
                                                 IntPtr[] origin,
                                                 IntPtr[] region,
                                                 out IntPtr imageRowPitch,
                                                 out IntPtr imageSlicePitch,
                                                 uint numEventsInWaitList,
                                                 Event[] eventWaitList,
                                                 out Event e,
                                                 out ErrorCode errCodeRet)
        {
            return new InfoBuffer(clEnqueueMapImage((commandQueue as IHandle).Handle, (image as IHandle).Handle, 
                                                    blockingMap, mapFlags, origin, region, 
                                                    out imageRowPitch, out imageSlicePitch, numEventsInWaitList, 
                                                    eventWaitList, out e, out errCodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueUnmapMemObject(IntPtr commandQueue,
                                                                IntPtr memObj,
                                                                IntPtr mappedPtr,
                                                                uint numEventsInWaitList,
                                                                [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 3)] Event[] eventWaitList,
                                                                [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueUnmapObject(CommandQueue commandQueue,
                                                   Mem memObj,
                                                   InfoBuffer mappedObject,
                                                   uint numEventsInWaitList,
                                                   Event[] eventWaitList,
                                                   out Event e)
        {
            return clEnqueueUnmapMemObject((commandQueue as IHandle).Handle, (memObj as IHandle).Handle,
                mappedObject.Address, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueNDRangeKernel(IntPtr commandQueue,
                                                               IntPtr kernel,
                                                               uint workDim,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] IntPtr[] globalWorkOffset,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] IntPtr[] globalWorkSize,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] IntPtr[] localWorkSize,
                                                               uint numEventsInWaitList,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)] Event[] eventWaitList,
                                                               [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueNDRangeKernel(CommandQueue commandQueue,
                                                     Kernel kernel,
                                                     uint workDim,
                                                     IntPtr[] globalWorkOffset,
                                                     IntPtr[] globalWorkSize,
                                                     IntPtr[] localWorkSize,
                                                     uint numEventsInWaitList,
                                                     Event[] eventWaitList,
                                                     out Event e)
        {
            return clEnqueueNDRangeKernel((commandQueue as IHandle).Handle, (kernel as IHandle).Handle, 
                workDim, globalWorkOffset, globalWorkSize, localWorkSize, numEventsInWaitList, eventWaitList, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueTask(IntPtr commandQueue,
                                                      IntPtr kernel,
                                                      uint numEventsInWaitList,
                                                      [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 2)] Event[] eventWaitList,
                                                      [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueTask(CommandQueue commandQueue,
                                            Kernel kernel,
                                            uint numEventsInWaitList,
                                            Event[] eventWaitList,
                                            out Event e)
        {
            return clEnqueueTask((commandQueue as IHandle).Handle, (kernel as IHandle).Handle, numEventsInWaitList, eventWaitList, out e);
        }

        // TODO: Implement and test "clEnqueueNativeKernel" separately
           
        [DllImport(Library)]
        private static extern ErrorCode clEnqueueMarker(IntPtr commandQueue,
                                                        [Out] [MarshalAs(UnmanagedType.Struct)] out Event e);
        public static ErrorCode EnqueueMarker(CommandQueue commandQueue,
                                              out Event e)
        {
            return clEnqueueMarker((commandQueue as IHandle).Handle, out e);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueWaitForEvents(IntPtr commandQueue,
                                                               uint numEventsInWaitList,
                                                               [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1)] Event[] eventWaitList);
        public static ErrorCode EnqueueWaitForEvents(CommandQueue commandQueue,
            uint numEventsInWaitList,
            Event[] eventWaitList)
        {
            return clEnqueueWaitForEvents((commandQueue as IHandle).Handle, numEventsInWaitList, eventWaitList);
        }

        [DllImport(Library)]
        private static extern ErrorCode clEnqueueBarrier(IntPtr commandQueue);
        public static ErrorCode EnqueueBarrier(CommandQueue commandQueue)
        {
            return clEnqueueBarrier((commandQueue as IHandle).Handle);
        }

        #endregion

        #region Flush and Finish API

        [DllImport(Library)]
        private static extern ErrorCode clFlush(IntPtr commandQueue);
        public static ErrorCode Flush(CommandQueue commandQueue)
        {
            return clFlush((commandQueue as IHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clFinish(IntPtr commandQueue);
        public static ErrorCode Finish(CommandQueue commandQueue)
        {
            return clFinish((commandQueue as IHandle).Handle);
        }

        #endregion

        #region Event object API

        [DllImport(Library)]
        private static extern ErrorCode clWaitForEvents(uint numEvents,
                                                        [In] [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 0)] Event[] eventWaitList);
        public static ErrorCode WaitForEvents(uint numEvents,
            Event[] eventWaitList)
        {
            return clWaitForEvents(numEvents, eventWaitList);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetEventInfo(IntPtr e,
                                                       EventInfo paramName,
                                                       IntPtr paramValueSize,
                                                       IntPtr paramValue,
                                                       out IntPtr paramValueSizeRet);
        public static ErrorCode GetEventInfo(Event e,
                                             EventInfo paramName,
                                             IntPtr paramValueSize,
                                             InfoBuffer paramValue,
                                             out IntPtr paramValueSizeRet)
        {
            return clGetEventInfo((e as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainEvent(IntPtr e);
        public static ErrorCode RetainEvent(Event e)
        {
            return clRetainEvent((e as IRefCountedHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseEvent(IntPtr e);
        public static ErrorCode ReleaseEvent(Event e)
        {
            return clReleaseEvent((e as IRefCountedHandle).Handle);
        }

        #endregion

        #region Sampler API

        [DllImport(Library)]
        private static extern IntPtr clCreateSampler(IntPtr context,
                                                     bool normalizedCoords,
                                                     AddressingMode addressingMode,
                                                     FilterMode filterMode,
                                                     out ErrorCode errCodeRet);
        public static Sampler CreateSampler(Context context,
                                            bool normalizedCoords,
                                            AddressingMode addressingMode,
                                            FilterMode filterMode,
                                            out ErrorCode errCodeRet)
        {
            return new Sampler(clCreateSampler((context as IHandle).Handle, normalizedCoords, addressingMode, filterMode, out errCodeRet));
        }

        [DllImport(Library)]
        private static extern ErrorCode clRetainSampler(IntPtr sampler);
        public static ErrorCode RetainSampler(Sampler sampler)
        {
            return clRetainSampler((sampler as IHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clReleaseSampler(IntPtr sampler);
        public static ErrorCode ReleaseSampler(Sampler sampler)
        {
            return clReleaseSampler((sampler as IHandle).Handle);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetSamplerInfo(IntPtr sampler,
                                                         SamplerInfo paramName,
                                                         IntPtr paramValueSize,
                                                         IntPtr paramValue,
                                                         out IntPtr paramValueSizeRet);
        public static ErrorCode GetSamplerInfo(Sampler sampler,
                                               SamplerInfo paramName,
                                               IntPtr paramValueSize,
                                               InfoBuffer paramValue,
                                               out IntPtr paramValueSizeRet)
        {
            return clGetSamplerInfo((sampler as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }

        #endregion

        #region Miscellaneous
        [DllImport(Library)]
        private static extern IntPtr clGetExtensionFunctionAddress([MarshalAs(UnmanagedType.LPStr)] string funcName);
        public static IntPtr GetExtensionFunctionAddress(string funcName)
        {
            return clGetExtensionFunctionAddress(funcName);
        }

        [DllImport(Library)]
        private static extern ErrorCode clGetEventProfilingInfo(IntPtr e,
                                                                ProfilingInfo paramName,
                                                                IntPtr paramValueSize,
                                                                IntPtr paramValue,
                                                                out IntPtr paramValueSizeRet);
        public static ErrorCode GetEventProfilingInfo(Event e,
                                                      ProfilingInfo paramName,
                                                      IntPtr paramValueSize,
                                                      InfoBuffer paramValue,
                                                      out IntPtr paramValueSizeRet)
        {
            return clGetEventProfilingInfo((e as IHandle).Handle, paramName, paramValueSize, paramValue.Address, out paramValueSizeRet);
        }
        #endregion
    }
}