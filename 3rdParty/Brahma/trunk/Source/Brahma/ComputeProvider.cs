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
using System.Linq.Expressions;

namespace Brahma
{
    public abstract class ComputeProvider: IDisposable
    {
        protected abstract IKernel<TRange> CompileQuery<TRange>(Expression<Func<NDRange<TRange>, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension;

        protected abstract IKernel<TRange, T> CompileQuery<TRange, T>(Expression<Func<NDRange<TRange>, T, IEnumerable<Set[]>>> query) 
            where TRange: struct, INDRangeDimension
            where T : IMem;

        protected abstract IKernel<TRange, T1, T2> CompileQuery<TRange, T1, T2>(Expression<Func<NDRange<TRange>, T1, T2, IEnumerable<Set[]>>> query)
            where TRange: struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem;

        protected abstract IKernel<TRange, T1, T2, T3> CompileQuery<TRange, T1, T2, T3>(Expression<Func<NDRange<TRange>, T1, T2, T3, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem;
        
        protected abstract IKernel<TRange, T1, T2, T3, T4> CompileQuery<TRange, T1, T2, T3, T4>(Expression<Func<NDRange<TRange>, T1, T2, T3, T4, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4: IMem;

        protected abstract IKernel<TRange, T1, T2, T3, T4, T5> CompileQuery<TRange, T1, T2, T3, T4, T5>(Expression<Func<NDRange<TRange>, T1, T2, T3, T4, T5, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem;

        protected abstract IKernel<TRange, T1, T2, T3, T4, T5, T6> CompileQuery<TRange, T1, T2, T3, T4, T5, T6>(Expression<Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem;

        protected abstract IKernel<TRange, T1, T2, T3, T4, T5, T6, T7> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7>(Expression<Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
            where T7 : IMem;

        protected abstract IKernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(Expression<Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, T8, IEnumerable<Set[]>>> query)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
            where T7 : IMem
            where T8 : IMem;

        public abstract void Dispose();
    }
}