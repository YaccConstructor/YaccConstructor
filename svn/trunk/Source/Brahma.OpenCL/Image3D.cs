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
using System.Collections.Generic;
using OpenCL.Net;

namespace Brahma.OpenCL
{
    public sealed class Image3D<T> : Brahma.Image3D<T> where T: struct, IImageFormat
    {
        private static readonly T _imageFormat = new T();
        
        private readonly Cl.Mem _image;
        private readonly int _width;
        private readonly int _height;
        private readonly int _depth;
        private readonly int _rowPitch = -1;

        public Image3D(ComputeProvider provider, Operations operations, bool hostAccessible, 
            int width, int height, int depth, int rowPitch = -1, int slicePitch = -1) // Create, no data
        {
            Cl.ErrorCode error = Cl.ErrorCode.Success;
            _image = Cl.CreateImage3D(provider.Context, (Cl.MemFlags)operations | (hostAccessible ? Cl.MemFlags.AllocHostPtr : 0),
                new Cl.ImageFormat(_imageFormat.ChannelOrder, _imageFormat.ChannelType.ChannelType), (IntPtr)width, (IntPtr)height, (IntPtr)depth,
                rowPitch == -1 ? (IntPtr)(width * _imageFormat.ComponentCount * _imageFormat.ChannelType.Size) : (IntPtr)rowPitch,
                slicePitch == -1 ? (IntPtr)(width * height * _imageFormat.ComponentCount * _imageFormat.ChannelType.Size) : (IntPtr)slicePitch,
                null, out error);

            if (error != Cl.ErrorCode.Success)
                throw new CLException(error);

            _width = width;
            _height = height;
            _depth = depth;
            _rowPitch = rowPitch;
        }

        public Image3D(ComputeProvider provider, Operations operations, Memory memory, int width, int height, int depth, T[] data, int rowPitch = -1, int slicePitch = -1) // Create and copy/use data from host
        {
            Cl.ErrorCode error;
            _image = Cl.CreateImage3D(provider.Context, (Cl.MemFlags)operations | (memory == Memory.Host ? Cl.MemFlags.UseHostPtr : (Cl.MemFlags)memory | Cl.MemFlags.CopyHostPtr),
                new Cl.ImageFormat(_imageFormat.ChannelOrder, _imageFormat.ChannelType.ChannelType), 
                (IntPtr)width, (IntPtr)height, (IntPtr)depth,
                rowPitch == -1 ? (IntPtr)(width * _imageFormat.ComponentCount * _imageFormat.ChannelType.Size) : (IntPtr)rowPitch,
                slicePitch == -1 ? (IntPtr)(width * height * _imageFormat.ComponentCount * _imageFormat.ChannelType.Size) : (IntPtr)slicePitch,
                data, out error);

            if (error != Cl.ErrorCode.Success)
                throw new CLException(error);

            _width = width;
            _height = height;
            _depth = depth;
            _rowPitch = rowPitch;
        }
        
        public int Width
        {
            get
            {
                return _width;
            }
        }

        public int Height
        {
            get
            {
                return _height;
            }
        }

        public int Depth
        {
            get
            {
                return _depth;
            }
        }

        public int RowPitch
        {
            get
            {
                return _rowPitch;
            }
        }
    }
}