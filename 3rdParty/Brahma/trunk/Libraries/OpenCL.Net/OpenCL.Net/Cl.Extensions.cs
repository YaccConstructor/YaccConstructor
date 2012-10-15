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
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace OpenCL.Net
{
    public static partial class Cl
    {
        internal static InfoBuffer GetInfo<THandleType, TEnumType>(
            GetInfoDelegate<THandleType, TEnumType> method, THandleType handle, TEnumType name, out ErrorCode error)
        {
            IntPtr paramSize;
            error = method(handle, name, IntPtr.Zero, InfoBuffer.Empty, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            var buffer = new InfoBuffer(paramSize);
            error = method(handle, name, paramSize, buffer, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            return buffer;
        }

        internal static InfoBuffer GetInfo<THandle1Type, THandle2Type, TEnumType>(
            GetInfoDelegate<THandle1Type, THandle2Type, TEnumType> method, THandle1Type handle1, THandle2Type handle2, TEnumType name, out ErrorCode error)
        {
            IntPtr paramSize;
            error = method(handle1, handle2, name, IntPtr.Zero, InfoBuffer.Empty, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            var buffer = new InfoBuffer(paramSize);
            error = method(handle1, handle2, name, paramSize, buffer, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            return buffer;
        }
        
        [StructLayout(LayoutKind.Sequential)]
        public struct InfoBuffer : IDisposable
        {
            [DllImport("msvcrt.dll", EntryPoint = "memcpy")]
            private static extern void CopyMemory(IntPtr pDest, IntPtr pSrc, int length);

            private static readonly InfoBuffer _empty = new InfoBuffer
            {
                _buffer = IntPtr.Zero
            };

            private IntPtr _buffer;

            public InfoBuffer(IntPtr size)
            {
                _buffer = Marshal.AllocHGlobal(size);
            }

            public InfoBuffer(byte[] array)
            {
                int length = array.Length;
                _buffer = Marshal.AllocHGlobal(length);
                using (var source = array.Pin())
                    CopyMemory(_buffer, source, length);
            }

            internal IntPtr Address
            {
                get
                {
                    return _buffer;
                }
                set
                {
                    _buffer = value;
                }
            }

            public T CastTo<T>() where T: struct
            {
                return _buffer.ElementAt<T>(0);
            }

            public T CastTo<T>(int index) where T: struct
            {
                return _buffer.ElementAt<T>(index);
            }

            public IEnumerable<T> CastToEnumerable<T>(IEnumerable<int> indices) where T : struct
            {
                foreach (int index in indices)
                    yield return _buffer.ElementAt<T>(index);
            }

            public T[] CastToArray<T>(int length) where T: struct
            {
                var result = new T[length];
                for (int i = 0; i < length; i++)
                    result[i] = _buffer.ElementAt<T>(i);

                return result;
            }

            public override string ToString()
            {
                return Marshal.PtrToStringAnsi(_buffer);
            }

            public static InfoBuffer Empty
            {
                get
                {
                    return _empty;
                }
            }

            #region IDisposable Members

            public void Dispose()
            {
                if (_buffer != IntPtr.Zero)
                {
                    Marshal.FreeHGlobal(_buffer);
                    _buffer = IntPtr.Zero;
                }
            }

            #endregion
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct InfoBufferArray : IDisposable
        {
            private readonly IntPtr[] _buffers;

            internal IntPtr[] Array
            {
                get
                {
                    return _buffers;
                }
            }

            public InfoBufferArray(params InfoBuffer[] buffers)
            {
                _buffers = new IntPtr[buffers.Length];
                for (int i = 0; i < buffers.Length; i++)
                    _buffers[i] = buffers[i].Address;
            }

            public InfoBuffer this[int index]
            {
                get
                {
                    return new InfoBuffer
                               {
                                   Address = _buffers[index]
                               };
                }
            }

            public int Length
            {
                get
                {
                    return _buffers.Length;
                }
            }

            public IntPtr Size
            {
                get
                {
                    return (IntPtr)(_buffers.Length * IntPtr.Size);
                }
            }

            public void Dispose()
            {
                for (int i = 0; i < _buffers.Length; i++)
                    new InfoBuffer
                        {
                            Address = _buffers[i]
                        }.Dispose();
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct InfoBufferArray<T>: IDisposable
            where T: struct
        {
            private static readonly IntPtr size = typeof(T).IsEnum ? (IntPtr)Marshal.SizeOf(Enum.GetUnderlyingType(typeof(T))) : (IntPtr)Marshal.SizeOf(typeof(T));
            
            private readonly IntPtr[] _buffers;

            internal IntPtr[] Array
            {
                get
                {
                    return _buffers;
                }
            }

            public InfoBufferArray(int length)
            {
                _buffers = new IntPtr[length];
                for (int i = 0; i < length; i++)
                    _buffers[i] = new InfoBuffer(size).Address;
            }

            public T this[int index]
            {
                get
                {
                    return new InfoBuffer
                    {
                        Address = _buffers[index]
                    }.CastTo<T>();
                }
            }

            public int Length
            {
                get
                {
                    return _buffers.Length;
                }
            }

            public IntPtr Size
            {
                get
                {
                    return (IntPtr)(_buffers.Length * IntPtr.Size);
                }
            }

            public void Dispose()
            {
                for (int i = 0; i < _buffers.Length; i++)
                    new InfoBuffer
                    {
                        Address = _buffers[i]
                    }.Dispose();
            }
        }

        // TODO: Figure out how to use segments of large arrays well ...
        // Or should we ditch this problem and let the user handle it?
        public sealed class ArraySegment<T>: IEnumerable<T>
        {
            private readonly T[] _array;
            private readonly int _index;
            private readonly int _count;
            
            internal ArraySegment(T[] array, int index, int count)
            {
                if (array == null)
                    throw new ArgumentNullException("array");
                if (index > array.Length)
                    throw new IndexOutOfRangeException("index");
                if (index + count > array.Length)
                    throw new ArgumentOutOfRangeException("count");

                _index = index;
                _count = count;
                
                _array = array;
            }

            public T this[int index]
            {
                get
                { 
                    if ((index < 0) || (index > _count) || (_index + index > _array.Length))
                        throw new IndexOutOfRangeException("index");

                    return _array[_index + index];
                }
            }

            public int Length
            {
                get
                {
                    return _count;
                }
            }

            public IEnumerator<T> GetEnumerator()
            {
                for (int i = _index; i < _count; i++)
                    yield return _array[i];
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            public static implicit operator T[](ArraySegment<T> segment)
            {
                return segment._array;
            }

            public static implicit operator ArraySegment<T>(T[] array)
            {
                return new ArraySegment<T>(array, 0, array.Length);
            }
        }

        public struct PinnedObject : IDisposable
        {
            private readonly GCHandle _handle;

            internal PinnedObject(object obj)
            {
                _handle = GCHandle.Alloc(obj, GCHandleType.Pinned);
            }

            public void Unpin()
            {
                Dispose();
            }

            #region IDisposable Members

            public void Dispose()
            {
                _handle.Free();
            }

            #endregion

            public static implicit operator IntPtr(PinnedObject pinned)
            {
                return pinned._handle.AddrOfPinnedObject();
            }
        }

        public static IntPtr Increment(this IntPtr ptr, int cbSize)
        {
            return new IntPtr(ptr.ToInt64() + cbSize);
        }

        public static IntPtr Increment<T>(this IntPtr ptr)
        {
            return ptr.Increment(Marshal.SizeOf(typeof(T)));
        }

        public static T ElementAt<T>(this IntPtr ptr, int index) where T: struct
        {
            Type resultType = typeof(T);
            resultType = resultType.IsEnum ? Enum.GetUnderlyingType(resultType) : resultType;
            
            var offset = Marshal.SizeOf(resultType) * index;
            var offsetPtr = ptr.Increment(offset);

            return (T)Marshal.PtrToStructure(offsetPtr, resultType);
        }

        public static ErrorCode OnError(this ErrorCode error, ErrorCode errorCode, Action<ErrorCode> action)
        {
            if (error == errorCode)
                action(error);

            return error;
        }

        public static ErrorCode OnAnyError(this ErrorCode error, Action<ErrorCode> action)
        {
            if (error != ErrorCode.Success)
                action(error);

            return error;
        }

        public static PinnedObject Pin(this object obj)
        {
            return new PinnedObject(obj);
        }

        public static T[] InitializeArray<T>(this T[] arr) where T : new()
        {
            for (int i = 0; i < arr.Length; i++)
                arr[i] = new T();

            return arr;
        }
    }
}
