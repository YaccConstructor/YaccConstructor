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

namespace Brahma
{
    public interface IKernel
    {
        IEnumerable<MemberExpression> Closures
        {
            get;
        }

        IEnumerable<ParameterExpression> Parameters
        {
            get;
        }
    }

    public interface IKernel<TRange>: IKernel
        where TRange : struct, INDRangeDimension
    {
    }
    
    public interface IKernel<TRange, T>: IKernel
        where T: IMem 
        where TRange: struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2>: IKernel
        where T1: IMem 
        where T2: IMem 
        where TRange: struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3>: IKernel
        where T1: IMem 
        where T2: IMem 
        where T3: IMem
        where TRange: struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3, T4>: IKernel
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4: IMem
        where TRange : struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3, T4, T5>: IKernel
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where TRange : struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3, T4, T5, T6>: IKernel
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where TRange : struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3, T4, T5, T6, T7>: IKernel
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where T7 : IMem
        where TRange : struct, INDRangeDimension
    {
    }

    public interface IKernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8>: IKernel
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where T7 : IMem
        where T8 : IMem
        where TRange : struct, INDRangeDimension
    {
    }
}