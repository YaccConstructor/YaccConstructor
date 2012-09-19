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
using System.Runtime.Serialization;

namespace OpenCL.Net
{
    public static partial class Cl
    {
        internal interface IHandle
        {
            IntPtr Handle
            {
                get;
            }
        }

        internal interface IRefCountedHandle : IHandle, IDisposable
        {
            void Retain();
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Platform : IHandle
        {
            private readonly IntPtr _handle;

            internal Platform(IntPtr handle)
            {
                _handle = handle;
            }

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            public static implicit operator IntPtr(Platform platform)
            {
                return platform._handle;
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Device : IHandle
        {
            private readonly IntPtr _handle;

            internal Device(IntPtr handle)
            {
                _handle = handle;
            }

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential, Pack = 1)]
        public struct ImageFormat
        {
            [MarshalAs(UnmanagedType.U4)]
            private ChannelOrder _channelOrder;
            [MarshalAs(UnmanagedType.U4)]
            private ChannelType _channelType;

            public ImageFormat(ChannelOrder channelOrder, ChannelType channelType)
            {
                _channelOrder = channelOrder;
                _channelType = channelType;
            }

            public ChannelOrder ChannelOrder
            {
                get
                {
                    return _channelOrder;
                }
                set
                {
                    _channelOrder = value;
                }
            }

            public ChannelType ChannelType
            {
                get
                {
                    return _channelType;
                }
                set
                {
                    _channelType = value;
                }
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct ContextProperty
        {
            private static readonly ContextProperty _zero = new ContextProperty(0);

            private readonly uint _propertyName;
            private readonly IntPtr _propertyValue;

            public ContextProperty(ContextProperties property, IntPtr value)
            {
                _propertyName = (uint)property;
                _propertyValue = value;
            }

            public ContextProperty(ContextProperties property)
            {
                _propertyName = (uint)property;
                _propertyValue = IntPtr.Zero;
            }

            public static ContextProperty Zero
            {
                get
                {
                    return _zero;
                }
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Context : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Context(IntPtr handle)
            {
                _handle = handle;
            }

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainContext(this);
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseContext(this);
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Mem : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Mem(IntPtr handle)
            {
                _handle = handle;
            }

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainMemObject(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseMemObject(this);
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Program : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Program(IntPtr handle)
            {
                _handle = handle;
            }

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainProgram(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseProgram(this);
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct CommandQueue : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal CommandQueue(IntPtr handle)
            {
                _handle = handle;
            }
            
            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainCommandQueue(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseCommandQueue(this);
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Kernel : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Kernel(IntPtr handle)
            {
                _handle = handle;
            }

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainKernel(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseKernel(this);
            }

            #endregion
        }
        
        [StructLayout(LayoutKind.Sequential)]
        public struct Event : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Event(IntPtr handle)
            {
                _handle = handle;
            }

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainEvent(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseEvent(this);
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct Sampler : IRefCountedHandle
        {
            private readonly IntPtr _handle;

            internal Sampler(IntPtr handle)
            {
                _handle = handle;
            }

            #region IRefCountedHandle Members

            void IRefCountedHandle.Retain()
            {
                RetainSampler(this);
            }

            #endregion

            #region IHandle Members

            IntPtr IHandle.Handle
            {
                get
                {
                    return _handle;
                }
            }

            #endregion

            #region IDisposable Members

            public void Dispose()
            {
                ReleaseSampler(this);
            }

            #endregion
        }

        [Serializable]
        public class Exception : System.Exception
        {
            public Exception(ErrorCode error)
                : base(error.ToString())
            {
            }

            public Exception(ErrorCode error, string message)
                : base(string.Format("{0}: {1}", error, message))
            {
            }

            public Exception(ErrorCode error, string message, Exception inner)
                : base(string.Format("{0}: {1}", error, message), inner)
            {
            }

            protected Exception(
                SerializationInfo info,
                StreamingContext context)
                : base(info, context)
            {
            }
        }
    }
}
