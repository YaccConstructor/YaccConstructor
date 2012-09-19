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

namespace OpenCL.Net
{
    public static partial class Cl
    {
        #region Platform API

        public static Platform[] GetPlatformIDs(out ErrorCode error)
        {
            uint platformCount;

            error = GetPlatformIDs(0, null, out platformCount);
            if (error != ErrorCode.Success)
                return new Platform[0];

            var platformIds = new Platform[platformCount] ;
            error = GetPlatformIDs(platformCount, platformIds, out platformCount);
            if (error != ErrorCode.Success)
                return new Platform[0];
            
            return platformIds;
        }

        public static InfoBuffer GetPlatformInfo(Platform platform, PlatformInfo paramName, out ErrorCode error)
        {
            return GetInfo(Cl.GetPlatformInfo, platform, paramName, out error);
        }

        #endregion

        #region Device API

        public static Device[] GetDeviceIDs(Platform platform, DeviceType deviceType, out ErrorCode error)
        {
            uint deviceCount;
            error = GetDeviceIDs(platform, deviceType, 0, null, out deviceCount);
            if (error != ErrorCode.Success)
                return new Device[0];

            var deviceIds = new Device[deviceCount];
            error = GetDeviceIDs(platform, deviceType, deviceCount, deviceIds, out deviceCount);
            if (error != ErrorCode.Success)
                return new Device[0];
            
            return deviceIds;
        }

        public static InfoBuffer GetDeviceInfo(Device device, DeviceInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetDeviceInfo, device, paramName, out error);
        }

        #endregion

        #region Context API

        public static InfoBuffer GetContextInfo(Context context, ContextInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetContextInfo, context, paramName, out error);
        }

        #endregion 

        #region Memory Object API

        public static InfoBuffer GetMemObjectInfo(Mem mem, MemInfo paramName, out ErrorCode error)
        {
            if (paramName == MemInfo.HostPtr) // Handle special case
            {
                IntPtr size = GetInfo(Cl.GetMemObjectInfo, mem, Cl.MemInfo.Size, out error).CastTo<IntPtr>();
                var buffer = new InfoBuffer(size);
                error = GetMemObjectInfo(mem, paramName, size, buffer, out size);
                if (error != ErrorCode.Success)
                    return InfoBuffer.Empty;

                return buffer;
            }

            return GetInfo(GetMemObjectInfo, mem, paramName, out error);
        }

        public static InfoBuffer GetImageInfo(Mem image, ImageInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetImageInfo, image, paramName, out error);
        }

        public static ImageFormat[] GetSupportedImageFormats(Context context, MemFlags flags, MemObjectType imageType, out ErrorCode error)
        {
            uint imageFormatCount;
            error = GetSupportedImageFormats(context, flags, imageType, 0, null, out imageFormatCount);
            if (error != ErrorCode.Success)
                return new ImageFormat[0];

            var imageFormats = new ImageFormat[imageFormatCount];
            error = GetSupportedImageFormats(context, flags, imageType, imageFormatCount, imageFormats, out imageFormatCount);
            if (error != ErrorCode.Success)
                return new ImageFormat[0];
            
            return imageFormats;
        }

        #endregion

        #region Program Object API

        public static InfoBuffer GetProgramInfo(Program program, ProgramInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetProgramInfo, program, paramName, out error);
        }

        public static InfoBuffer GetProgramBuildInfo(Program program, Device device, ProgramBuildInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetProgramBuildInfo, program, device, paramName, out error);
        }

        #endregion

        #region Command Queue API

        public static InfoBuffer GetCommandQueueInfo(CommandQueue commandQueue, CommandQueueInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetCommandQueueInfo, commandQueue, paramName, out error);
        }

        #endregion

        #region Kernel Object API

        public static Kernel[] CreateKernelsInProgram(Program program, out ErrorCode error)
        {
            uint numKernelsRet;
            error = CreateKernelsInProgram(program, 0, null, out numKernelsRet);
            if (error != Cl.ErrorCode.Success)
                return null;

            var result = new Kernel[numKernelsRet];
            error = CreateKernelsInProgram(program, numKernelsRet, result, out numKernelsRet);
            if (error != Cl.ErrorCode.Success)
                return null;

            return result;
        }

        public static InfoBuffer GetKernelInfo(Kernel kernel, KernelInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetKernelInfo, kernel, paramName, out error);
        }

        public static InfoBuffer GetKernelWorkGroupInfo(Kernel kernel, Device device, KernelWorkGroupInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetKernelWorkGroupInfo, kernel, device, paramName, out error);
        }

        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, Mem value)
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)IntPtr.Size, value);
        }

        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, float value)
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)sizeof(float), value);
        }

        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, int value)
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)sizeof(int), value);
        }

        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, bool value)
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)sizeof(bool), value);
        }

        #endregion

        #region Event object API

        public static InfoBuffer GetEventInfo(Event e, EventInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetEventInfo, e, paramName, out error);
        }

        #endregion

        #region Sampler API

        public static InfoBuffer GetSamplerInfo(Sampler sampler, SamplerInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetSamplerInfo, sampler, paramName, out error);
        }

        #endregion

        public static InfoBuffer GetEventProfilingInfo(Event e, ProfilingInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetEventProfilingInfo, e, paramName, out error);
        }
    }
}