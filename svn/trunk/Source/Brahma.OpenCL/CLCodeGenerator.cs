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
using System.Text;
using Brahma.Types;

namespace Brahma.OpenCL
{
    internal static class CLCodeGenerator
    {
        private static readonly MemberExpressionComparer _memberExpressionComparer = new MemberExpressionComparer();
        
        public const string KernelName = "brahmaKernel";

        private sealed class CodeGenerator : ExpressionVisitor
        {
            private readonly ComputeProvider _provider;
            private readonly LambdaExpression _lambda;

            private readonly List<MemberExpression> _closures = new List<MemberExpression>();

            private string _functionName = string.Empty;

            private struct FunctionDescriptor
            {
                public string Code;
                public Type ReturnType;
                public Type[] ParameterTypes;
            }

            private readonly Dictionary<string, FunctionDescriptor> _functions = new Dictionary<string, FunctionDescriptor>();

            private StringBuilder _code = new StringBuilder();

            private readonly List<string> _declaredMembers = new List<string>();

            private static string TranslateType(Type type)
            {
                if ((type == typeof(int32)) || (type == typeof(int)))
                    return "int";
                if ((type == typeof(uint)))
                    return "uint";
                if ((type == typeof(float32)) || (type == typeof(float)))
                    return "float";
                
                if ((type.IsConcreteGenericOf(typeof(Brahma.Buffer<>))) ||
                    (type.IsConcreteGenericOf(typeof(Buffer<>))))
                {
                    string genericParameterType = TranslateType(type.GetGenericArguments()[0]);
                    return "__global " + genericParameterType + "*"; // TODO: Take into account different kinds of variables later
                }

                throw new InvalidOperationException(string.Format("Cannot use the type {0} inside a kernel", type));
            }

            private static IEnumerable<Type> GetAllButLast(Type[] types)
            {
                for (int i = 0; i < types.Length; i++)
                    if (i < types.Length - 1)
                        yield return types[i];
            }

            private static void UnwindMemberAccess(Expression expression, StringBuilder builder)
            {
                if ((expression == null) || (expression.NodeType == ExpressionType.Constant))
                    return;

                if (expression.NodeType == ExpressionType.MemberAccess)
                {
                    var member = expression as MemberExpression;
                    if (!(member.Member is FieldInfo))
                        throw new InvalidOperationException("Cannot access methods/properties inside a kernel!");

                    builder.Append(member.Member.Name);

                    UnwindMemberAccess(member.Expression, builder);
                }
            }

            private static ParameterExpression GetLoopVar(LambdaExpression expression)
            {
                ParameterExpression loopRange = expression.Parameters[0];
                var call = expression.Body as MethodCallExpression;
                do
                {
                    if (call.Arguments[0] == loopRange)
                        return ((LambdaExpression)call.Arguments[1]).Parameters[0];

                    call = call.Arguments[0] as MethodCallExpression;
                } while (call != null);

                // TODO: Will this ever happen?
                throw new InvalidExpressionTreeException("Don't know how to get the loop variable from the given expression", expression);
            }

            private static string GetClosureAccessName(MemberExpression member)
            {
                var result = new StringBuilder();
                UnwindMemberAccess(member, result);

                return result.ToString();
            }

            private string NativeMethodPrefix
            {
                get 
                {
                   return (_provider.CompileOptions & CompileOptions.UseNativeFunctions) == CompileOptions.UseNativeFunctions ? "native_" : string.Empty;
                }
            }

            protected override Expression VisitConstant(ConstantExpression constant)
            {
                _code.Append("((" + TranslateType(constant.Type) + ")");
                _code.Append(constant + ")");

                return constant;
            }

            protected override Expression VisitMemberAccess(MemberExpression member)
            {
                switch (member.Member.Name)
                {
                    case "GlobalID0":
                        _code.Append("get_global_id(0)");
                        return member;
                    case "GlobalID1":
                        _code.Append("get_global_id(1)");
                        return member;
                    case "GlobalID2":
                        _code.Append("get_global_id(2)");
                        return member;
                    case "LocalID0":
                        _code.Append("get_local_id(0)");
                        return member;
                    case "LocalID1":
                        _code.Append("get_local_id(1)");
                        return member;
                    case "LocalID2":
                        _code.Append("get_local_id(2)");
                        return member;
                }

                if (member.Member.DeclaringType.IsAnonymous())
                {
                    _code.Append(member.Member.Name);

                    return member;
                }

                if (member.IsClosureAccess() || member.IsConstantAccess())
                {
                    _code.Append(GetClosureAccessName(member));

                    if (!_closures.Contains(member, _memberExpressionComparer))
                        _closures.Add(member);

                    return member;
                }

                return member;
            }

            protected override Expression VisitUnary(UnaryExpression unary)
            {
                switch (unary.NodeType)
                {
                    case ExpressionType.Convert:
                        var srcType = TranslateType(unary.Operand.Type);
                        var destType = TranslateType(unary.Type);

                        if (srcType == destType)
                            Visit(unary.Operand);
                        else
                        {
                            _code.AppendFormat("convert_{0}(", destType);
                            Visit(unary.Operand);
                            _code.Append(")");
                        }

                        break;

                    case ExpressionType.Negate:
                        _code.Append("-(");
                        Visit(unary.Operand);
                        _code.Append(")");

                        break;
                }

                return unary;
            }

            protected override Expression VisitConditional(ConditionalExpression conditional)
            {
                _code.Append("(");
                Visit(conditional.Test);
                _code.Append(" ? ");
                Visit(conditional.IfTrue);
                _code.Append(" : ");
                Visit(conditional.IfFalse);
                
                _code.Append(")");
                
                return conditional;
            }

            protected override Expression VisitParameter(ParameterExpression parameter)
            {
                _code.Append(parameter.Name);

                return parameter;
            }

            protected override NewExpression VisitNew(NewExpression newExpression)
            {
                if (newExpression.Type.IsAnonymous())
                {
                    for (int i = 0; i < newExpression.Arguments.Count; i++)
                    {
                        if (newExpression.Members[i].Name == newExpression.Arguments[i].ToString())
                            continue; // Trivial assignment

                        // TODO: Verify with method name, too
                        // Loop
                        if ((newExpression.Arguments[i].Type == typeof(Func<int, IEnumerable<Set[]>>)) ||
                            (newExpression.Arguments[i].Type == typeof(Func<IEnumerable<int>, IEnumerable<Set[]>>)))
                        {
                            Visit(newExpression.Arguments[i]);
                            continue;
                        }

                        // TODO: Verify with method name, too
                        // CompileFunction
                        if (newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,,,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,,,,,>)) ||
                            newExpression.Arguments[i].Type.IsConcreteGenericOf(typeof(Func<,,,,,,,,>)))
                        {
                            _functionName = newExpression.Members[i].Name;
                            Visit(newExpression.Arguments[i]);
                            continue;
                        }

                        if (!_declaredMembers.Contains(newExpression.Members[i].Name))
                        {
                            _code.Append(TranslateType(((PropertyInfo) newExpression.Members[i]).PropertyType) + " ");
                            _declaredMembers.Add(newExpression.Members[i].Name);
                        }
                        _code.Append(newExpression.Members[i].Name + " = ");

                        Visit(newExpression.Arguments[i]);
                    }
                }

                return newExpression;
            }

            protected override Expression VisitBinary(BinaryExpression binary)
            {
                bool isResultAssignment = false; // TODO: Remove this and move it one level up
                
                _code.Append("(");
                Visit(binary.Left);

                switch (binary.NodeType)
                {
                    case ExpressionType.LessThanOrEqual:
                        if (binary.Type.IsConcreteGenericOf(typeof(Set<>)))
                        {
                            _code.Append(" = ");
                            isResultAssignment = true;
                        }
                        else
                            _code.Append(" <= ");
                        break;

                    case ExpressionType.Add:
                        _code.Append(" + ");
                        break;
                    case ExpressionType.Subtract:
                        _code.Append(" - ");
                        break;
                    case ExpressionType.Multiply:
                        _code.Append(" * ");
                        break;
                    case ExpressionType.Divide:
                        _code.Append(" / ");
                        break;
                    case ExpressionType.Modulo:
                        _code.Append(" % ");
                        break;

                    case ExpressionType.NotEqual:
                        _code.Append(" != ");
                        break;

                    case ExpressionType.Equal:
                        _code.Append(" == ");
                        break;

                    case ExpressionType.LessThan:
                        _code.Append(" < ");
                        break;
                    
                    case ExpressionType.GreaterThan:
                        _code.Append(" > ");
                        break;

                    case ExpressionType.And:
                        _code.Append(" & ");
                        break;
                    
                    case ExpressionType.Or:
                        _code.Append(" | ");
                        break;

                    case ExpressionType.LeftShift:
                        _code.Append(" << ");
                        break;

                    case ExpressionType.RightShift:
                        _code.Append(" >> ");
                        break;
                }

                Visit(binary.Right);
                _code.Append(")");

                if (isResultAssignment)
                    _code.Append(";");

                return binary;
            }

            protected override Expression VisitMethodCall(MethodCallExpression method)
            {
                switch (method.Method.Name)
                {
                    case "get_Item":
                        Visit(method.Object);
                        _code.Append("[");
                        Visit(method.Arguments[0]);
                        for (int i = 1; i < method.Arguments.Count; i++)
                        {
                            _code.Append(", ");
                            Visit(method.Arguments[i]);
                        }
                        _code.Append("]");

                        break;

                    case "Loop":
                        ParameterExpression loopVar = null;

                        // Figure out what kind of loop body this is
                        var loopBody = method.Arguments[2] as LambdaExpression;

                        if (loopBody.Parameters[0].Type == typeof(IEnumerable<int32>))
                            loopVar = GetLoopVar(loopBody);
                        if (loopBody.Parameters[0].Type == typeof(int32))
                            loopVar = loopBody.Parameters[0];

                        _code.AppendFormat("for (int {0} = ", loopVar.Name);
                        Visit(method.Arguments[0]);
                        _code.AppendFormat("; {0} < ", loopVar.Name);
                        Visit(method.Arguments[1]);
                        _code.AppendFormat("; {0}++) {{", loopVar.Name);

                        Visit(method.Arguments[2]);
                        _code.AppendLine(";");

                        _code.AppendLine("}");

                        break;

                    case "CompileFunction":

                        if (string.IsNullOrEmpty(_functionName))
                            throw new InvalidExpressionTreeException("Anonymous functions are not supported, try declaring it using a let first", method);
                        if ((method.Arguments[0] as LambdaExpression) == null)
                            throw new InvalidExpressionTreeException("Inline functions in Brahma have to be lambda expressions", method.Arguments[0]);

                        var functionCode = new StringBuilder();

                        var functionReturnType = method.Type.GetGenericArguments().Last();
                        var functionParameterTypes = GetAllButLast(method.Type.GetGenericArguments()).ToArray();

                        functionCode.AppendFormat("{0} {1}(", TranslateType(functionReturnType), _functionName);

                        for (int i = 0; i < functionParameterTypes.Length; i++)
                            functionCode.Append(string.Format("{0} {1},", TranslateType(functionParameterTypes[i]), (method.Arguments[0] as LambdaExpression).Parameters[i]));
                        if (functionCode[functionCode.Length - 1] == ',')
                            functionCode.Remove(functionCode.Length - 1, 1);

                        functionCode.AppendLine(") {");
                        functionCode.Append("return (");

                        var saveCurrentStringBuilder = _code;
                        _code = functionCode;
                        Visit(method.Arguments[0]);
                        _code = saveCurrentStringBuilder;

                        functionCode.Append("); }");

                        _functions.Add(_functionName, new FunctionDescriptor
                                                          {
                                                              Code = functionCode.ToString(),
                                                              ReturnType = functionReturnType,
                                                              ParameterTypes = functionParameterTypes
                                                          });

                        _functionName = string.Empty;
                        break;

                    case "Select":
                        if (!(method.Arguments[0] is ParameterExpression))
                        {
                            Visit(method.Arguments[0]);
                        }
                        Visit(method.Arguments[1]);
                        _code.AppendLine(";");

                        break;
                    
                    case "Fabs":
                        _code.Append("fabs(");
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;

                    case "Log10":
                        _code.AppendFormat("{0}log10(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;

                    case "Log2":
                        _code.AppendFormat("{0}log2(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;

                    case "Powr":
                        _code.AppendFormat("{0}powr(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(", ");
                        Visit(method.Arguments[1]);
                        _code.Append(")");

                        break;

                    case "Min":
                        _code.Append("min(");
                        Visit(method.Arguments[0]);
                        _code.Append(", ");
                        Visit(method.Arguments[1]);
                        _code.Append(")");

                        break;
                    
                    case "Max":
                        _code.Append("max(");
                        Visit(method.Arguments[0]);
                        _code.Append(", ");
                        Visit(method.Arguments[1]);
                        _code.Append(")");

                        break;

                    case "reinterpretAsFloat32":
                        _code.Append("as_float(");
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;

                    case "Floor":
                        _code.Append("floor(");
                        Visit(method.Arguments[0]);
                        _code.Append(")");
                        
                        break;

                    case "Sin":
                        _code.AppendFormat("{0}sin(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");
                        
                        break;

                    case "Cos":
                        _code.AppendFormat("{0}cos(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;

                    case "Exp":
                        _code.AppendFormat("{0}exp(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;
                    
                    case "Sqrt":
                        _code.AppendFormat("{0}sqrt(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;
                    
                    case "Log":
                        _code.AppendFormat("{0}log(", NativeMethodPrefix);
                        Visit(method.Arguments[0]);
                        _code.Append(")");

                        break;
                }

                return method;
            }

            protected override Expression VisitInvocation(InvocationExpression invocation)
            {
                FunctionDescriptor localFunction;
                var member = (invocation.Expression as MemberExpression);
                if (member == null)
                    return invocation;

                if (_functions.TryGetValue(member.Member.Name, out localFunction))
                {
                    var functionType = (member.Member as PropertyInfo).PropertyType;
                    var functionReturnType = functionType.GetGenericArguments().Last();
                    var functionParameterTypes = GetAllButLast(functionType.GetGenericArguments()).ToArray();

                    if ((localFunction.ReturnType == functionReturnType) &&
                        (localFunction.ParameterTypes.SequenceEqual(functionParameterTypes)))
                    {
                        _code.AppendFormat("{0}(", member.Member.Name);

                        if (invocation.Arguments.Count > 0)
                        {
                            Visit(invocation.Arguments[0]);
                            for (int i = 1; i < invocation.Arguments.Count; i++)
                            {
                                _code.Append(", ");
                                Visit(invocation.Arguments[i]);
                            }
                        }
                        _code.Append(")");
                    }
                }
                
                return invocation;
            }

            public CodeGenerator(ComputeProvider provider, LambdaExpression lambda)
            {
                _provider = provider;
                _lambda = lambda;
            }

            public string Generate()
            {
                Visit(_lambda.Body);

                var parameters = new StringBuilder();
                foreach (var parameter in _lambda.Parameters)
                {
                    if (parameter.Type.DerivesFrom(typeof(NDRange)))
                        continue;

                    parameters.Append(string.Format("{0} {1},", TranslateType(parameter.Type), parameter.Name));
                }

                foreach (var closure in _closures)
                    parameters.Append(string.Format("{0} {1},", TranslateType(closure.Type), GetClosureAccessName(closure)));

                if (parameters[parameters.Length - 1] == ',')
                    parameters.Remove(parameters.Length - 1, 1);

                var kernelSource = new StringBuilder();

                // Add any functions we generated
                foreach (var func in _functions)
                    kernelSource.AppendLine(func.Value.Code);

                kernelSource.AppendLine(string.Format("__kernel void {0}({1}) {{", KernelName, parameters));
                kernelSource.Append(_code.ToString());
                kernelSource.AppendLine("}");

                return kernelSource.ToString();
            }

            public IEnumerable<MemberExpression> Closures
            {
                get { return _closures; }
            }
        }

        public static void GenerateKernel(this LambdaExpression lambda, ComputeProvider provider, ICLKernel kernel)
        {
            var codeGenerator = new CodeGenerator(provider, lambda);
            kernel.Source.Append(codeGenerator.Generate());
            kernel.SetClosures(codeGenerator.Closures);
            kernel.SetParameters(lambda.Parameters);
        }
    }
}