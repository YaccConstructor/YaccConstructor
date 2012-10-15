using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace Brahma.Reference
{
    internal static class CSCodeGenerator
    {
        private static readonly MemberExpressionComparer _memberExpressionComparer = new MemberExpressionComparer();

        private sealed class CodeGenerator: ExpressionVisitor
        {
            private readonly ComputeProvider _provider;
            private readonly LambdaExpression _lambda;

            private readonly List<MemberExpression> _closures = new List<MemberExpression>();

            public CodeGenerator(ComputeProvider provider, LambdaExpression lambda)
            {
                _provider = provider;
                _lambda = lambda;
            }

            public string Generate()
            {
                throw new NotImplementedException();
            }

            public IEnumerable<MemberExpression> Closures
            {
                get { return _closures; }
            }
        }

        public static void GenerateKernel(this LambdaExpression lambda, ComputeProvider provider, ICSKernel kernel)
        {
            var codeGenerator = new CodeGenerator(provider, lambda);
            kernel.Source.Append(codeGenerator.Generate());
            kernel.SetClosures(codeGenerator.Closures);
            kernel.SetParameters(lambda.Parameters);
        }
    }
}
