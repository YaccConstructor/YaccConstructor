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

using System.Runtime.InteropServices;
using OpenCL.Net;

namespace Brahma.OpenCL
{
    public interface IImageComponentType
    {
        Cl.ChannelType ChannelType
        {
            get;
        }

        int Size
        {
            get;
        }
    }
    
    [StructLayout(LayoutKind.Sequential)] public struct Snorm_Int8 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 1;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Snorm_Int8;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Snorm_Int16 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Snorm_Int16;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unorm_Int8 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 1;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unorm_Int8;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unorm_Int16 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unorm_Int16;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unorm_Short565 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unorm_Short565;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unorm_Short555 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unorm_Short555;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unorm_Int101010 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 4;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unorm_Int101010;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Signed_Int8 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 1;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Signed_Int8;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Signed_Int16 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Signed_Int16;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Signed_Int32 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 4;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Signed_Int32;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unsigned_Int8 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 1;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unsigned_Int8;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unsigned_Int16 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unsigned_Int16;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Unsigned_Int32 : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 4;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Unsigned_Int32;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct HalfFloat : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 2;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.HalfFloat;
            }
        }
    }
    [StructLayout(LayoutKind.Sequential)] public struct Float : IImageComponentType 
    {
        public int Size
        {
            get 
            {
                return 4;
            }
        }

        public Cl.ChannelType ChannelType
        {
            get
            {
                return Cl.ChannelType.Float;
            }
        }
    }

    public interface IImageFormat: Brahma.IImageFormat
    {
        Cl.ChannelOrder ChannelOrder
        {
            get;
        }

        IImageComponentType ChannelType
        {
            get;
        }
    }
    
    [StructLayout(LayoutKind.Sequential)] public struct R<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T r;

        public int ComponentCount
        {
            get
            {
                return 1;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.R;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct A<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T a;

        public int ComponentCount
        {
            get
            {
                return 1;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get 
            {
                return Cl.ChannelOrder.A;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct RG<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T r;
        public T g;

        public int ComponentCount
        {
            get
            {
                return 2;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.RG;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct RA<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T r;
        public T a;

        public int ComponentCount
        {
            get
            {
                return 2;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.RA;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct RGB<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T r;
        public T g;
        public T b;

        public int ComponentCount
        {
            get
            {
                return 3;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.RGB;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct RGBA<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T r;
        public T g;
        public T b;
        public T a;

        public int ComponentCount
        {
            get
            {
                return 4;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.RGBA;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct BGRA<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T b;
        public T g;
        public T r;
        public T a;

        public int ComponentCount
        {
            get
            {
                return 4;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.RGBA;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct ARGB<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T a;
        public T r;
        public T g;
        public T b;

        public int ComponentCount
        {
            get
            {
                return 4;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.ARGB;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct Intensity<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T k;

        public int ComponentCount
        {
            get
            {
                return 1;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.Intensity;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)] public struct Luminance<T> : IImageFormat where T: struct, IImageComponentType
    {
        public T l;

        public int ComponentCount
        {
            get
            {
                return 1;
            }
        }

        public Cl.ChannelOrder ChannelOrder
        {
            get
            {
                return Cl.ChannelOrder.Luminance;
            }
        }

        private static readonly T _channelType = new T();

        public IImageComponentType ChannelType
        {
            get
            {
                return _channelType;
            }
        }
    }
}