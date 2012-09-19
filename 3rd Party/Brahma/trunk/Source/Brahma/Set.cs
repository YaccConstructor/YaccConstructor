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

namespace Brahma
{
    public abstract class Set
    {
        public static IEnumerable<Set[]> Void
        {
            get
            {
                yield return new Set[] { };
            }
        }
    }

    public enum Comparison
    {
        GreaterThanEquals,
        LessThanEquals
    }

    public sealed class Set<T> : Set where T : struct, IComparable<T>
    {
        private readonly T _lhs;
        private readonly T _rhs;
        private readonly Comparison _comparison;

        public Set(T lhs, T rhs, Comparison comparison = Comparison.LessThanEquals)
        {
            _lhs = lhs;
            _rhs = rhs;

            _comparison = comparison;
        }

        public T Lhs
        {
            get
            {
                return _lhs;
            }
        }

        public T Rhs
        {
            get
            {
                return _rhs;
            }
        }

        public Comparison Comparison
        {
            get
            {
                return _comparison;
            }
        }

        public static implicit operator bool (Set<T> set)
        {
            switch (set._comparison)
            {
                case Comparison.LessThanEquals:
                    return set._lhs.CompareTo(set._rhs) <= 0;

                case Comparison.GreaterThanEquals:
                    return set._lhs.CompareTo(set._rhs) >= 0;

                default:
                    throw new NotSupportedException();
            }
        }
    }
}