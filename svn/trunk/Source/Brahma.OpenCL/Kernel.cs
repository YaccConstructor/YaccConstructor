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

using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;
using Brahma.OpenCL.Commands;
using Brahma.Types;
using OpenCL.Net;

namespace Brahma.OpenCL
{
    internal interface ICLKernel: IKernel
    {
        StringBuilder Source
        {
            get;
        }

        Cl.Kernel ClKernel
        {
            get;
            set;
        }

        int32 WorkDim
        {
            get;
        }

        void SetClosures(IEnumerable<MemberExpression> closures);
        void SetParameters(IEnumerable<ParameterExpression> parameters);
    }

    public abstract class KernelBase<TRange>: ICLKernel
        where TRange: struct, Brahma.INDRangeDimension
    {
        private static readonly TRange _range; // Default value will do

        private readonly StringBuilder _source = new StringBuilder();
        private IEnumerable<MemberExpression> _closures;
        private IEnumerable<ParameterExpression> _parameters;

        #region ICLKernel Members

        StringBuilder ICLKernel.Source
        {
            get { return _source; }
        }

        Cl.Kernel ICLKernel.ClKernel
        {
            get; set;
        }

        int32 ICLKernel.WorkDim
        {
            get { return ((INDRangeDimension)_range).Dimensions; }
        }

        void ICLKernel.SetClosures(IEnumerable<MemberExpression> closures)
        {
            _closures = closures;
        }

        void ICLKernel.SetParameters(IEnumerable<ParameterExpression> parameters)
        {
            _parameters = parameters;
        }

        #endregion

        #region IKernel Members

        IEnumerable<MemberExpression> IKernel.Closures
        {
            get { return _closures; }
        }

        IEnumerable<ParameterExpression> IKernel.Parameters
        {
            get { return _parameters; }
        }

        #endregion
    }

    public sealed class Kernel<TRange>: KernelBase<TRange>, IKernel<TRange>
        where TRange: struct, Brahma.INDRangeDimension
    {
    }

    public sealed class Kernel<TRange, T>: KernelBase<TRange>, IKernel<TRange, T>
        where TRange: struct, Brahma.INDRangeDimension
        where T: IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2>: KernelBase<TRange>, IKernel<TRange, T1, T2>
        where TRange: struct, Brahma.INDRangeDimension
        where T1: IMem 
        where T2: IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3: IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3, T4> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3, T4>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3, T4, T5> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3, T4, T5>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3, T4, T5, T6> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3, T4, T5, T6>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3, T4, T5, T6, T7> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3, T4, T5, T6, T7>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where T7 : IMem
    {
    }

    public sealed class Kernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> : KernelBase<TRange>, IKernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8>
        where TRange : struct, Brahma.INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where T7 : IMem
        where T8 : IMem
    {
    }

    public static class KernelExtensions
    {
        public static Run<TRange> Run<TRange>(this Kernel<TRange> kernel, TRange range)
            where TRange: struct, INDRangeDimension
        {
            return new Run<TRange>(kernel, range);
        }
        
        public static Run<TRange, T> Run<TRange, T>(this Kernel<TRange, T> kernel, TRange range, T data) 
            where TRange: struct, INDRangeDimension
            where T: IMem
        {
            return new Run<TRange, T>(kernel, range) { D1 = data };
        }
        
        public static Run<TRange, T1, T2> Run<TRange, T1, T2>(this Kernel<TRange, T1, T2> kernel, TRange range, T1 d1, T2 d2) 
            where TRange: struct, INDRangeDimension
            where T1: IMem 
            where T2: IMem
        {
            return new Run<TRange, T1, T2>(kernel, range) { D1 = d1, D2 = d2 };
        }
        
        public static Run<TRange, T1, T2, T3> Run<TRange, T1, T2, T3>(this Kernel<TRange, T1, T2, T3> kernel, TRange range, T1 d1, T2 d2, T3 d3)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
        {
            return new Run<TRange, T1, T2, T3>(kernel, range) { D1 = d1, D2 = d2, D3 = d3 };
        }

        public static Run<TRange, T1, T2, T3, T4> Run<TRange, T1, T2, T3, T4>(this Kernel<TRange, T1, T2, T3, T4> kernel, TRange range, T1 d1, T2 d2, T3 d3, T4 d4)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
        {
            return new Run<TRange, T1, T2, T3, T4>(kernel, range) { D1 = d1, D2 = d2, D3 = d3, D4 = d4 };
        }
        
        public static Run<TRange, T1, T2, T3, T4, T5> Run<TRange, T1, T2, T3, T4, T5>(this Kernel<TRange, T1, T2, T3, T4, T5> kernel, TRange range, T1 d1, T2 d2, T3 d3, T4 d4, T5 d5)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
        {
            return new Run<TRange, T1, T2, T3, T4, T5>(kernel, range) { D1 = d1, D2 = d2, D3 = d3, D4 = d4, D5 = d5 };
        }
        
        public static Run<TRange, T1, T2, T3, T4, T5, T6> Run<TRange, T1, T2, T3, T4, T5, T6>(this Kernel<TRange, T1, T2, T3, T4, T5, T6> kernel, TRange range, T1 d1, T2 d2, T3 d3, T4 d4, T5 d5, T6 d6)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
        {
            return new Run<TRange, T1, T2, T3, T4, T5, T6>(kernel, range) { D1 = d1, D2 = d2, D3 = d3, D4 = d4, D5 = d5, D6 = d6 };
        }

        public static Run<TRange, T1, T2, T3, T4, T5, T6, T7> Run<TRange, T1, T2, T3, T4, T5, T6, T7>(this Kernel<TRange, T1, T2, T3, T4, T5, T6, T7> kernel, TRange range, T1 d1, T2 d2, T3 d3, T4 d4, T5 d5, T6 d6, T7 d7)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
            where T7 : IMem
        {
            return new Run<TRange, T1, T2, T3, T4, T5, T6, T7>(kernel, range) { D1 = d1, D2 = d2, D3 = d3, D4 = d4, D5 = d5, D6 = d6, D7 = d7 };
        }

        public static Run<TRange, T1, T2, T3, T4, T5, T6, T7, T8> Run<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(this Kernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> kernel, TRange range, T1 d1, T2 d2, T3 d3, T4 d4, T5 d5, T6 d6, T7 d7, T8 d8)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
            where T7 : IMem
            where T8 : IMem
        {
            return new Run<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(kernel, range) { D1 = d1, D2 = d2, D3 = d3, D4 = d4, D5 = d5, D6 = d6, D7 = d7, D8 = d8 };
        }
    }
}