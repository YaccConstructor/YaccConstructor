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
using Brahma.Types;

namespace Brahma
{
    public static class Math
    {
        public const float PI = 3.1415926535897931f;
        
        [KernelCallable]
        public static T Powr<T>(T x, T y)
            where T: struct, IPrimitiveType, IMem
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static float32 Fabs(float32 value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Log10<T>(T value)
            where T: struct, IPrimitiveType, IMem
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Log2<T>(T value)
            where T : struct, IPrimitiveType, IMem
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Min<T>(T value1, T value2)
            where T : struct, IPrimitiveType, IMem
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Max<T>(T value1, T value2)
            where T : struct, IPrimitiveType, IMem
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static float32 reinterpretAsFloat32(int32 value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static int32 Floor(float32 value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static float32 Sin(float32 value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static float32 Cos(float32 value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Exp<T>(T value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }
        
        [KernelCallable]
        public static T Sqrt<T>(T value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }

        [KernelCallable]
        public static T Log<T>(T value)
        {
            throw new NotSupportedException("Can only call this method from inside a kernel");
        }
    }
}
