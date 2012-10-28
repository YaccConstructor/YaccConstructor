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

namespace Brahma.Types
{
    public struct int8 : IPrimitiveType, IMem, IComparable<int8>
    {
        private static readonly IntPtr _size = (IntPtr)sizeof(sbyte);

        internal sbyte _value;

        public static implicit operator int8(sbyte value)
        {
            return new int8
            {
                _value = value
            };
        }

        public static implicit operator sbyte(int8 value)
        {
            return value._value;
        }

        #region Implicit conversions

        // http://msdn.microsoft.com/en-us/library/y5b434w4.aspx

        public static implicit operator int16(int8 value)
        {
            return new int16
                    {
                        _value = value._value
                    };
        }

        public static implicit operator int32(int8 value)
        {
            return new int32
                    {
                        _value = value._value
                    };
        }

        public static implicit operator int64(int8 value)
        {
            return new int64
                    {
                        _value = value._value
                    };
        }

        public static implicit operator float32(int8 value)
        {
            return new float32
                    {
                        _value = value._value
                    };
        }

        public static implicit operator float64(int8 value)
        {
            return new float64
                    {
                        _value = value._value
                    };
        }

        #endregion

        #region Explicit conversions

        // http://msdn.microsoft.com/en-us/library/yht2cx7b%28v=VS.100%29.aspx

        public static explicit operator uint8(int8 value)
        {
            return new uint8
                    {
                        _value = (byte)value._value
                    };
        }

        public static explicit operator uint16(int8 value)
        {
            return new uint16
                    {
                        _value = (ushort)value._value
                    };
        }

        public static explicit operator uint32(int8 value)
        {
            return new uint32
                    {
                        _value = (uint)value._value
                    };
        }

        public static explicit operator uint64(int8 value)
        {
            return new uint64
                    {
                        _value = (ulong)value._value
                    };
        }

        #endregion

        public static Set<int8> operator <=(int8 lhs, int8 rhs)
        {
            return new Set<int8>(lhs, rhs);
        }

        public static Set<int8> operator >=(int8 lhs, int8 rhs)
        {
            throw new NotSupportedException();
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

        #region IComparable<int8> Members

        public int CompareTo(int8 other)
        {
            return System.Math.Sign(_value - other._value);
        }

        #endregion

        public override bool Equals(object obj)
        {
            return obj is int8 ? ((int8)obj)._value == _value : false;
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