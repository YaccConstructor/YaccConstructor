using System.CodeDom.Compiler;
using Microsoft.CSharp;

namespace Brahma.Reference
{
    public sealed class ComputeProvider: Brahma.ComputeProvider
    {
        private readonly CSharpCodeProvider _codeProvider;

        protected override IKernel<TRange> CompileQuery<TRange>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T> CompileQuery<TRange, T>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2> CompileQuery<TRange, T1, T2>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3> CompileQuery<TRange, T1, T2, T3>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3, T4> CompileQuery<TRange, T1, T2, T3, T4>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, T4, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5> CompileQuery<TRange, T1, T2, T3, T4, T5>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, T4, T5, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6> CompileQuery<TRange, T1, T2, T3, T4, T5, T6>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6, T7> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(System.Linq.Expressions.Expression<System.Func<NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, T8, System.Collections.Generic.IEnumerable<Set[]>>> query)
        {
            throw new System.NotImplementedException();
        }

        public ComputeProvider()
        {
            _codeProvider = new CSharpCodeProvider();
        }

        public override void Dispose()
        {
            _codeProvider.Dispose();
        }
    }
}
