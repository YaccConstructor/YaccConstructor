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
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Brahma
{
    public static class ExpressionExtensions
    {
        // This class derives from ExpressionVisitor and flattens the tree by visiting each node and adding them to a "flattened" list
        private sealed class TreeFlattener : ExpressionVisitor
        {
            // A list to hold the flattened expression tree
            private readonly List<Expression> _flattened = new List<Expression>();

            // No one can create an instance of this class
            private TreeFlattener()
            {
            }

            // Override the main Visit method, since we don't care what what node we see, we need them all
            protected override Expression Visit(Expression exp)
            {
                if (!_flattened.Contains(exp))
                    _flattened.Add(exp); // Add to a list

                return base.Visit(exp); // Call base to process the rest of the tree
            }

            // Static method to flatten an expression since no one can make an instance of this class
            public static IEnumerable<Expression> Flatten(Expression expression)
            {
                var f = new TreeFlattener();
                f.Visit(expression); // Start flattening
                return f._flattened; // Return the "flattened" list
            }
        }

        // Extension method to flatten an expression
        public static IEnumerable<Expression> Flatten(this Expression expression)
        {
            return TreeFlattener.Flatten(expression);
        }

        // Extension method to flatten a bunch of expressions
        public static IEnumerable<Expression> Flatten(this IEnumerable<Expression> expressions)
        {
            var flattenedExpressions = new List<Expression>();

            foreach (Expression expression in expressions)
                flattenedExpressions.AddRange(expression.Flatten());

            return flattenedExpressions;
        }

        public static IEnumerable<LambdaExpression> Lambdas(this IEnumerable<Expression> expressions)
        {
            return from Expression node in expressions
                   where node is LambdaExpression
                   select node as LambdaExpression;
        }

        public static LambdaExpression FindLambda(this IEnumerable<LambdaExpression> lambdas, Type returnType)
        {
            return (from lambda in lambdas
                   where lambda.Body.Type == returnType
                   select lambda).First();
        }

        public static Type ReturnType(this LambdaExpression lambda)
        {
            return lambda.Body.Type;
        }

        public static IEnumerable<Type> AnonymousTypes(this IEnumerable<LambdaExpression> lambdas)
        {
            return (from lambda in lambdas
                    from parameter in lambda.Parameters
                    where parameter.IsTransparentIdentifier()
                    select parameter.Type).Distinct();
        }

        // Extension methods to flatten a bunch of lambdas
        public static IEnumerable<Expression> Flatten(this IEnumerable<LambdaExpression> expressions)
        {
            var flattenedExpressions = new List<Expression>();

            foreach (LambdaExpression expression in expressions)
                flattenedExpressions.AddRange(expression.Flatten());

            return flattenedExpressions;
        }

        // Extension method to find out if a given (parameter) expression is a transparent identifier
        public static bool IsTransparentIdentifier(this ParameterExpression parameter)
        {
            return parameter.Name.StartsWith("<>h__TransparentIdentifier");
        }

        public static bool IsConstantAccess(this MemberExpression member)
        {
            if (!(member.Member is FieldInfo))
                return true;

            if (member.Expression == null)
                return true;

            if (member.Expression.NodeType == ExpressionType.Constant)
                return true;

            if (member.Expression is MemberExpression)
                return IsConstantAccess(member.Expression as MemberExpression);

            return false;
        }

        public static bool IsClosureAccess(this MemberExpression expression)
        {
            return expression.ToString().Contains("<>c__DisplayClass");
        }

        public static IEnumerable<MemberExpression> Closures(this IEnumerable<Expression> flattened)
        {
            return (from expression in flattened
                    let memberExp = expression as MemberExpression
                    where (memberExp != null) &&
                    (memberExp.Type.Name.Contains("<>c__DisplayClass") ||
                    ((expression.NodeType == ExpressionType.MemberAccess) &&
                    (memberExp.Expression.NodeType == ExpressionType.Constant)))
                    select memberExp).Distinct(new MemberExpressionComparer());
        }
    }

    internal static class MemberExpressionExtensions
    {
        private static object UnwindClosureAccess(Expression expression, Stack<MemberExpression> access)
        {
            if (expression is ConstantExpression)
                return (expression as ConstantExpression).Value;

            if (expression.NodeType == ExpressionType.MemberAccess)
            {
                var member = expression as MemberExpression;
                if (!(member.Member is FieldInfo))
                    throw new InvalidOperationException("Cannot access methods/properties inside a kernel!");

                access.Push(member);

                if (member.Expression == null)
                    return null;

                return UnwindClosureAccess(member.Expression, access);
            }

            throw new InvalidOperationException(string.Format("Unknown/Invalid expression in closure access: {0}", expression.NodeType));
        }

        public static object GetClosureValue(this MemberExpression expression)
        {
            var accessStack = new Stack<MemberExpression>();
            var constant = UnwindClosureAccess(expression, accessStack);

            foreach (var member in accessStack)
                constant = ((FieldInfo)member.Member).GetValue(constant);

            return constant;
        }
    }
}