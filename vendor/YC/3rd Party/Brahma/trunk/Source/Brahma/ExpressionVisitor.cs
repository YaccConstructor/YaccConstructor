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
using System.Collections.ObjectModel;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace Brahma
{
    public abstract class ExpressionVisitor
    {
        protected virtual Expression Visit(Expression exp)
        {
            if (exp == null)
                return exp;
            switch (exp.NodeType)
            {
                case ExpressionType.Negate:
                case ExpressionType.NegateChecked:
                case ExpressionType.Not:
                case ExpressionType.Convert:
                case ExpressionType.ConvertChecked:
                case ExpressionType.ArrayLength:
                case ExpressionType.Quote:
                case ExpressionType.TypeAs:
                    return VisitUnary((UnaryExpression)exp);
                case ExpressionType.Add:
                case ExpressionType.AddChecked:
                case ExpressionType.Subtract:
                case ExpressionType.SubtractChecked:
                case ExpressionType.Multiply:
                case ExpressionType.MultiplyChecked:
                case ExpressionType.Divide:
                case ExpressionType.Modulo:
                case ExpressionType.And:
                case ExpressionType.AndAlso:
                case ExpressionType.Or:
                case ExpressionType.OrElse:
                case ExpressionType.LessThan:
                case ExpressionType.LessThanOrEqual:
                case ExpressionType.GreaterThan:
                case ExpressionType.GreaterThanOrEqual:
                case ExpressionType.Equal:
                case ExpressionType.NotEqual:
                case ExpressionType.Coalesce:
                case ExpressionType.ArrayIndex:
                case ExpressionType.RightShift:
                case ExpressionType.LeftShift:
                case ExpressionType.ExclusiveOr:
                    return VisitBinary((BinaryExpression)exp);
                case ExpressionType.TypeIs:
                    return VisitTypeIs((TypeBinaryExpression)exp);
                case ExpressionType.Conditional:
                    return VisitConditional((ConditionalExpression)exp);
                case ExpressionType.Constant:
                    return VisitConstant((ConstantExpression)exp);
                case ExpressionType.Parameter:
                    return VisitParameter((ParameterExpression)exp);
                case ExpressionType.MemberAccess:
                    return VisitMemberAccess((MemberExpression)exp);
                case ExpressionType.Call:
                    return VisitMethodCall((MethodCallExpression)exp);
                case ExpressionType.Lambda:
                    return VisitLambda((LambdaExpression)exp);
                case ExpressionType.New:
                    return VisitNew((NewExpression)exp);
                case ExpressionType.NewArrayInit:
                case ExpressionType.NewArrayBounds:
                    return VisitNewArray((NewArrayExpression)exp);
                case ExpressionType.Invoke:
                    return VisitInvocation((InvocationExpression)exp);
                case ExpressionType.MemberInit:
                    return VisitMemberInit((MemberInitExpression)exp);
                case ExpressionType.ListInit:
                    return VisitListInit((ListInitExpression)exp);
                default:
                    throw new NotSupportedException(string.Format("Expressions of type {0} are not supported", exp.NodeType));
            }
        }

        protected virtual MemberBinding VisitBinding(MemberBinding binding)
        {
            switch (binding.BindingType)
            {
                case MemberBindingType.Assignment:
                    return VisitMemberAssignment((MemberAssignment)binding);
                case MemberBindingType.MemberBinding:
                    return VisitMemberMemberBinding((MemberMemberBinding)binding);
                case MemberBindingType.ListBinding:
                    return VisitMemberListBinding((MemberListBinding)binding);
                default:
                    throw new NotSupportedException(string.Format("Binding type '{0}' is not supported", binding.BindingType));
            }
        }

        protected virtual ElementInit VisitElementInitializer(ElementInit initializer)
        {
            ReadOnlyCollection<Expression> arguments = VisitExpressionList(initializer.Arguments);
            return arguments != initializer.Arguments
                       ? Expression.ElementInit(initializer.AddMethod, arguments)
                       : initializer;
        }

        protected virtual Expression VisitUnary(UnaryExpression unary)
        {
            Expression operand = Visit(unary.Operand);
            return operand != unary.Operand
                       ? Expression.MakeUnary(unary.NodeType, operand, unary.Type, unary.Method)
                       : unary;
        }

        protected virtual Expression VisitBinary(BinaryExpression binary)
        {
            Expression left = Visit(binary.Left);
            Expression right = Visit(binary.Right);
            Expression conversion = Visit(binary.Conversion);
            if (left != binary.Left || right != binary.Right || conversion != binary.Conversion)
            {
                if (binary.NodeType == ExpressionType.Coalesce && binary.Conversion != null)
                    return Expression.Coalesce(left, right, conversion as LambdaExpression);

                return Expression.MakeBinary(binary.NodeType, left, right, binary.IsLiftedToNull, binary.Method);
            }

            return binary;
        }

        protected virtual Expression VisitTypeIs(TypeBinaryExpression typeBinary)
        {
            Expression expr = Visit(typeBinary.Expression);
            return expr != typeBinary.Expression
                       ? Expression.TypeIs(expr, typeBinary.TypeOperand)
                       : typeBinary;
        }

        protected virtual Expression VisitConstant(ConstantExpression constant)
        {
            return constant;
        }

        protected virtual Expression VisitConditional(ConditionalExpression conditional)
        {
            Expression test = Visit(conditional.Test);
            Expression ifTrue = Visit(conditional.IfTrue);
            Expression ifFalse = Visit(conditional.IfFalse);
            if (test != conditional.Test || ifTrue != conditional.IfTrue || ifFalse != conditional.IfFalse)
                return Expression.Condition(test, ifTrue, ifFalse);
            return conditional;
        }

        protected virtual Expression VisitParameter(ParameterExpression parameter)
        {
            return parameter;
        }

        protected virtual Expression VisitMemberAccess(MemberExpression member)
        {
            Expression exp = Visit(member.Expression);
            return exp != member.Expression
                       ? Expression.MakeMemberAccess(exp, member.Member)
                       : member;
        }

        protected virtual Expression VisitMethodCall(MethodCallExpression method)
        {
            Expression obj = Visit(method.Object);
            IEnumerable<Expression> args = VisitExpressionList(method.Arguments);
            if (obj != method.Object || args != method.Arguments)
                return Expression.Call(obj, method.Method, args);
            return method;
        }

        protected virtual ReadOnlyCollection<Expression> VisitExpressionList(ReadOnlyCollection<Expression> original)
        {
            List<Expression> list = null;
            for (int i = 0,
                     n = original.Count; i < n; i++)
            {
                Expression p = Visit(original[i]);
                if (list != null)
                    list.Add(p);
                else if (p != original[i])
                {
                    list = new List<Expression>(n);
                    for (int j = 0; j < i; j++)
                        list.Add(original[j]);
                    list.Add(p);
                }
            }
            return list != null
                       ? list.AsReadOnly()
                       : original;
        }

        protected virtual MemberAssignment VisitMemberAssignment(MemberAssignment assignment)
        {
            Expression e = Visit(assignment.Expression);
            return e != assignment.Expression
                       ? Expression.Bind(assignment.Member, e)
                       : assignment;
        }

        protected virtual MemberMemberBinding VisitMemberMemberBinding(MemberMemberBinding binding)
        {
            IEnumerable<MemberBinding> bindings = VisitBindingList(binding.Bindings);
            return bindings != binding.Bindings
                       ? Expression.MemberBind(binding.Member, bindings)
                       : binding;
        }

        protected virtual MemberListBinding VisitMemberListBinding(MemberListBinding binding)
        {
            IEnumerable<ElementInit> initializers = VisitElementInitializerList(binding.Initializers);
            return initializers != binding.Initializers
                       ? Expression.ListBind(binding.Member, initializers)
                       : binding;
        }

        protected virtual IEnumerable<MemberBinding> VisitBindingList(ReadOnlyCollection<MemberBinding> original)
        {
            List<MemberBinding> list = null;
            for (int i = 0,
                     n = original.Count; i < n; i++)
            {
                MemberBinding b = VisitBinding(original[i]);
                if (list != null)
                    list.Add(b);
                else if (b != original[i])
                {
                    list = new List<MemberBinding>(n);
                    for (int j = 0; j < i; j++)
                        list.Add(original[j]);
                    list.Add(b);
                }
            }
            if (list != null)
                return list;
            return original;
        }

        protected virtual IEnumerable<ElementInit> VisitElementInitializerList(ReadOnlyCollection<ElementInit> original)
        {
            List<ElementInit> list = null;
            for (int i = 0,
                     n = original.Count; i < n; i++)
            {
                ElementInit init = VisitElementInitializer(original[i]);
                if (list != null)
                    list.Add(init);
                else if (init != original[i])
                {
                    list = new List<ElementInit>(n);
                    for (int j = 0; j < i; j++)
                        list.Add(original[j]);
                    list.Add(init);
                }
            }
            if (list != null)
                return list;
            return original;
        }

        protected virtual Expression VisitLambda(LambdaExpression lambda)
        {
            Expression body = Visit(lambda.Body);
            return body != lambda.Body
                       ? Expression.Lambda(lambda.Type, body, lambda.Parameters)
                       : lambda;
        }

        protected virtual NewExpression VisitNew(NewExpression newExpression)
        {
            IEnumerable<Expression> args = VisitExpressionList(newExpression.Arguments);
            if (args != newExpression.Arguments)
            {
                return newExpression.Members != null
                           ? Expression.New(newExpression.Constructor, args, newExpression.Members)
                           : Expression.New(newExpression.Constructor, args);
            }
            return newExpression;
        }

        protected virtual Expression VisitMemberInit(MemberInitExpression memberInit)
        {
            NewExpression n = VisitNew(memberInit.NewExpression);
            IEnumerable<MemberBinding> bindings = VisitBindingList(memberInit.Bindings);
            if (n != memberInit.NewExpression || bindings != memberInit.Bindings)
                return Expression.MemberInit(n, bindings);
            return memberInit;
        }

        protected virtual Expression VisitListInit(ListInitExpression init)
        {
            NewExpression n = VisitNew(init.NewExpression);
            IEnumerable<ElementInit> initializers = VisitElementInitializerList(init.Initializers);
            if (n != init.NewExpression || initializers != init.Initializers)
                return Expression.ListInit(n, initializers);
            return init;
        }

        protected virtual Expression VisitNewArray(NewArrayExpression newArray)
        {
            IEnumerable<Expression> exprs = VisitExpressionList(newArray.Expressions);
            if (exprs != newArray.Expressions)
            {
                return newArray.NodeType == ExpressionType.NewArrayInit
                           ? Expression.NewArrayInit(newArray.Type.GetElementType(), exprs)
                           : Expression.NewArrayBounds(newArray.Type.GetElementType(), exprs);
            }
            return newArray;
        }

        protected virtual Expression VisitInvocation(InvocationExpression invocation)
        {
            IEnumerable<Expression> args = VisitExpressionList(invocation.Arguments);
            Expression expr = Visit(invocation.Expression);
            if (args != invocation.Arguments || expr != invocation.Expression)
                return Expression.Invoke(expr, args);
            return invocation;
        }
    }
}
