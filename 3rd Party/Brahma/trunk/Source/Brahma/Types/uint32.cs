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
    public struct uint32 : IPrimitiveType, IMem, IComparable<uint32>
    {
        private static readonly IntPtr _size = (IntPtr)sizeof(uint);

        internal uint _value;

        public static implicit operator uint32(uint value)
        {
            return new uint32
            {
                _value = value
            };
        }

        public static implicit operator uint(uint32 value)
        {
            return value._value;
        }

        #region Implicit conversions

        // http://msdn.microsoft.com/en-us/library/y5b434w4.aspx

        public static implicit operator int64(uint32 value)
        {
            return new int64
                    {
                        _value = value._value
                    };
        }

        public static implicit operator uint64(uint32 value)
        {
            return new uint64
                    {
                        _value = value._value
                    };
        }

        public static implicit operator float32(uint32 value)
        {
            return new float32
                    {
                        _value = value._value
                    };
        }

        public static implicit operator float64(uint32 value)
        {
            return new float64
                    {
                        _value = value._value
                    };
        }

        #endregion

        #region Explicit conversions

        // http://msdn.microsoft.com/en-us/library/yht2cx7b%28v=VS.100%29.aspx

        public static explicit operator int8(uint32 value)
        {
            return new int8
                    {
                        _value = (sbyte)value._value
                    };
        }

        public static explicit operator uint8(uint32 value)
        {
            return new uint8
                    {
                        _value = (byte)value._value
                    };
        }

        public static explicit operator int16(uint32 value)
        {
            return new int16
                    {
                        _value = (short)value._value
                    };
        }

        public static explicit operator uint16(uint32 value)
        {
            return new uint16
                    {
                        _value = (ushort)value._value
                    };
        }

        public static explicit operator int32(uint32 value)
        {
            return new int32
                    {
                        _value = (int)value._value
                    };
        }

        #endregion

        public static Set<uint32> operator <=(uint32 lhs, uint32 rhs)
        {
            return new Set<uint32>(lhs, rhs);
        }

        public static Set<uint32> operator >=(uint32 lhs, uint32 rhs)
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

        #region IComparable<uint32> Members

        public int CompareTo(uint32 other)
        {
            return System.Math.Sign(_value - other._value);
        }

        #endregion

        public override bool Equals(object obj)
        {
            return obj is uint32 ? ((uint32)obj)._value == _value : false;
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