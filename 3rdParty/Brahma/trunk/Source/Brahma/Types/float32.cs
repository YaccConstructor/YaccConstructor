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

namespace Brahma.Types
{
    [StructLayout(LayoutKind.Sequential)]
    public struct float32 : IPrimitiveType, IMem, IComparable<float32>
    {
        private static readonly IntPtr _size = (IntPtr)sizeof(float);

        internal float _value;

        public static implicit operator float32(float value)
        {
            return new float32
            {
                _value = value
            };
        }

        public static implicit operator float(float32 value)
        {
            return value._value;
        }

        #region Implicit conversions

        // http://msdn.microsoft.com/en-us/library/y5b434w4.aspx

        public static implicit operator float64(float32 value)
        {
            return new float64
                       {
                           _value = value
                       };
        }

        #endregion

        #region Explicit conversions

        // http://msdn.microsoft.com/en-us/library/yht2cx7b%28v=VS.100%29.aspx

        public static explicit operator int8(float32 value)
        {
            return new int8
                       {
                           _value = (sbyte)value._value
                       };
        }

        public static explicit operator uint8(float32 value)
        {
            return new uint8
                       {
                           _value = (byte)value._value
                       };
        }

        public static explicit operator int16(float32 value)
        {
            return new int16
                       {
                           _value = (short) value
                       };
        }

        public static explicit operator uint16(float32 value)
        {
            return new uint16
                       {
                           _value = (ushort)value._value
                       };
        }

        public static explicit operator int32(float32 value)
        {
            return new int32
                       {
                           _value = (int) value._value
                       };
        }

        public static explicit operator uint32(float32 value)
        {
            return new uint32
                       {
                           _value = (uint) value._value
                       };
        }

        public static explicit operator int64(float32 value)
        {
            return new int64
                       {
                           _value = (long) value._value
                       };
        }

        public static explicit operator uint64(float32 value)
        {
            return new uint64
                       {
                           _value = (ulong) value._value
                       };
        }

        #endregion

        public static Set<float32> operator <=(float32 lhs, float32 rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        public static Set<float32> operator <=(float32 lhs, float rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        public static Set<float32> operator <=(float lhs, float32 rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        public static Set<float32> operator >=(float32 lhs, float32 rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        public static Set<float32> operator >=(float32 lhs, float rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        public static Set<float32> operator >=(float lhs, float32 rhs)
        {
            return new Set<float32>(lhs, rhs);
        }

        #region IMem Members

        IntPtr IMem.Size
        {
            get
            {
                return _size;
            }
        }

        object IMem.Data
        {
            get
            {
                return _value;
            }
        }

        #endregion

        #region IComparable<float32> Members

        public int CompareTo(float32 other)
        {
            return System.Math.Sign(_value - other._value);
        }

        #endregion

        public override bool Equals(object obj)
        {
            return obj is float32 ? ((float32)obj)._value == _value : false;
        }

        public override int GetHashCode()
        {
            return _value.GetHashCode();
        }

        public override string ToString()
        {
            return _value.ToString();
        }
    }
}