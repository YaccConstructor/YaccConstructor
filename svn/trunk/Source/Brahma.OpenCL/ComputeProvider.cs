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
using System.Text;
using System.Text.RegularExpressions;

using OpenCL.Net;

using Brahma.Types;

namespace Brahma.OpenCL
{
    [Flags]
    public enum CompileOptions
    {
        UseNativeFunctions = 1 << 0,
        FastRelaxedMath = 1 << 1,
        FusedMultiplyAdd = 1 << 2,
        DisableOptimizations = 1 << 3,
        StrictAliasing = 1 << 4,
        NoSignedZeros = 1 << 5,
        UnsafeMathOptimizations = 1 << 6,
        FiniteMathOnly = 1 << 7
    }
    
    public sealed class ComputeProvider: Brahma.ComputeProvider
    {
        private const CompileOptions DefaultOptions = CompileOptions.UseNativeFunctions | CompileOptions.FusedMultiplyAdd | CompileOptions.FastRelaxedMath;
        
        private readonly Cl.Context _context;
        private readonly Cl.Device[] _devices;
        private bool _disposed;
        private string _compileOptions = string.Empty;

        private void SetCompileOptions(CompileOptions options)
        {
            CompileOptions = options;
            
            _compileOptions = string.Empty;

            // UseNativeFunctions = ((options & CompileOptions.UseNativeFunctions) == CompileOptions.UseNativeFunctions);
            _compileOptions += ((options & CompileOptions.FastRelaxedMath) == CompileOptions.FastRelaxedMath ? " -cl-fast-relaxed-math " : string.Empty);
            _compileOptions += ((options & CompileOptions.FusedMultiplyAdd) == CompileOptions.FusedMultiplyAdd ? " -cl-mad-enable " : string.Empty);
            _compileOptions += ((options & CompileOptions.DisableOptimizations) == CompileOptions.DisableOptimizations ? " -cl-opt-disable " : string.Empty);
            _compileOptions += ((options & CompileOptions.StrictAliasing) == CompileOptions.StrictAliasing ? " -cl-strict-aliasing " : string.Empty);
            _compileOptions += ((options & CompileOptions.NoSignedZeros) == CompileOptions.NoSignedZeros ? " -cl-no-signed-zeros " : string.Empty);
            _compileOptions += ((options & CompileOptions.UnsafeMathOptimizations) == CompileOptions.UnsafeMathOptimizations ? " -cl-unsafe-math-optimizations " : string.Empty);
            _compileOptions += ((options & CompileOptions.FiniteMathOnly) == CompileOptions.FiniteMathOnly ? " -cl-finite-math-only " : string.Empty);
        }

        internal CompileOptions CompileOptions
        {
            get;
            private set;
        }

        internal Cl.Context Context
        {
            get 
            {
                return _context;
            }
        }
        
        public ComputeProvider(params Cl.Device[] devices)
        {
            if (devices == null)
                throw new ArgumentNullException("devices");
            if (devices.Length == 0)
                throw new ArgumentException("Need at least one device!");
            
            _devices = devices;
            
            Cl.ErrorCode error;
            _context = Cl.CreateContext(null, (uint)devices.Length, _devices, null, IntPtr.Zero, out error);
            
            if (error != Cl.ErrorCode.Success)
                throw new CLException(error);
        }

        private T CompileQuery<T>(LambdaExpression lambda) where T: IKernel, ICLKernel, new()
        {
            var kernel = new T();
            lambda.GenerateKernel(this, kernel);

            Cl.ErrorCode error;
            using (Cl.Program program = Cl.CreateProgramWithSource(_context, 1, new[] { (kernel as ICLKernel).Source.ToString() }, null, out error))
            {
                error = Cl.BuildProgram(program, (uint)_devices.Length, _devices, _compileOptions, null, IntPtr.Zero);
                if (error != Cl.ErrorCode.Success)
                    throw new Exception(string.Join("\n", from device in _devices
                                                          select Cl.GetProgramBuildInfo(program, device, Cl.ProgramBuildInfo.Log, out error).ToString()));
                (kernel as ICLKernel).ClKernel = Cl.CreateKernel(program, CLCodeGenerator.KernelName, out error);
            }

            return kernel;
        }

        protected override IKernel<TRange> CompileQuery<TRange>(Expression<Func<Brahma.NDRange<TRange>,IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange>>(query);
        }

        protected override IKernel<TRange, T> CompileQuery<TRange, T>(Expression<Func<Brahma.NDRange<TRange>,T,IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T>>(query);
        }

        protected override IKernel<TRange, T1, T2> CompileQuery<TRange,T1,T2>(Expression<Func<Brahma.NDRange<TRange>, T1, T2,IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3> CompileQuery<TRange, T1, T2, T3>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3, T4> CompileQuery<TRange, T1, T2, T3, T4>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3, T4>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5> CompileQuery<TRange, T1, T2, T3, T4, T5>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3, T4, T5>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6> CompileQuery<TRange, T1, T2, T3, T4, T5, T6>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3, T4, T5, T6>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6, T7> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3, T4, T5, T6, T7>>(query);
        }

        protected override IKernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> CompileQuery<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, T8, IEnumerable<Set[]>>> query)
        {
            return CompileQuery<Kernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8>>(query);
        }

        public Kernel<TRange> Compile<TRange>(Expression<Func<Brahma.NDRange<TRange>, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange>;
        }

        public Kernel<TRange, T> Compile<TRange, T>(Expression<Func<Brahma.NDRange<TRange>, T, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange: struct, INDRangeDimension
            where T : IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T>;
        }

        public Kernel<TRange, T1, T2> Compile<TRange, T1, T2>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange: struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2>;
        }

        public Kernel<TRange, T1, T2, T3> Compile<TRange, T1, T2, T3>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3: IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3>;
        }

        public Kernel<TRange, T1, T2, T3, T4> Compile<TRange, T1, T2, T3, T4>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4: IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3, T4>;
        }

        public Kernel<TRange, T1, T2, T3, T4, T5> Compile<TRange, T1, T2, T3, T4, T5>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3, T4, T5>;
        }

        public Kernel<TRange, T1, T2, T3, T4, T5, T6> Compile<TRange, T1, T2, T3, T4, T5, T6>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3, T4, T5, T6>;
        }

        public Kernel<TRange, T1, T2, T3, T4, T5, T6, T7> Compile<TRange, T1, T2, T3, T4, T5, T6, T7>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
            where TRange : struct, INDRangeDimension
            where T1 : IMem
            where T2 : IMem
            where T3 : IMem
            where T4 : IMem
            where T5 : IMem
            where T6 : IMem
            where T7 : IMem
        {
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3, T4, T5, T6, T7>;
        }

        public Kernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8> Compile<TRange, T1, T2, T3, T4, T5, T6, T7, T8>(Expression<Func<Brahma.NDRange<TRange>, T1, T2, T3, T4, T5, T6, T7, T8, IEnumerable<Set[]>>> query, CompileOptions options = DefaultOptions)
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
            SetCompileOptions(options);
            return CompileQuery(query) as Kernel<TRange, T1, T2, T3, T4, T5, T6, T7, T8>;
        }

        [KernelCallable]
        public Func<int, IEnumerable<Set[]>> Loop(int32 startValue, int32 count, Func<IEnumerable<int32>, IEnumerable<Set[]>> body)
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<int, IEnumerable<Set[]>> Loop(int32 startValue, int32 count, Func<int32, IEnumerable<Set>> body)
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        // TODO: Using a range variable inside the body of a function does not carry over to OpenCL (that variable is not in scope)
        [KernelCallable]
        public Func<T> CompileFunction<T>(Func<T> function)
            where T: IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T, TResult> CompileFunction<T, TResult>(Func<T, TResult> function)
            where T : IMem, IPrimitiveType
            where TResult: IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, TResult> CompileFunction<T1, T2, TResult>(Func<T1, T2, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, TResult> CompileFunction<T1, T2, T3, TResult>(Func<T1, T2, T3, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, T4, TResult> CompileFunction<T1, T2, T3, T4, TResult>(Func<T1, T2, T3, T4, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where T4 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, T4, T5, TResult> CompileFunction<T1, T2, T3, T4, T5, TResult>(Func<T1, T2, T3, T4, T5, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where T4 : IMem, IPrimitiveType
            where T5 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, T4, T5, T6, TResult> CompileFunction<T1, T2, T3, T4, T5, T6, TResult>(Func<T1, T2, T3, T4, T5, T6, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where T4 : IMem, IPrimitiveType
            where T5 : IMem, IPrimitiveType
            where T6 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, T4, T5, T6, T7, TResult> CompileFunction<T1, T2, T3, T4, T5, T6, T7, TResult>(Func<T1, T2, T3, T4, T5, T6, T7, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where T4 : IMem, IPrimitiveType
            where T5 : IMem, IPrimitiveType
            where T6 : IMem, IPrimitiveType
            where T7 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        [KernelCallable]
        public Func<T1, T2, T3, T4, T5, T6, T7, T8, TResult> CompileFunction<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(Func<T1, T2, T3, T4, T5, T6, T7, T8, TResult> function)
            where T1 : IMem, IPrimitiveType
            where T2 : IMem, IPrimitiveType
            where T3 : IMem, IPrimitiveType
            where T4 : IMem, IPrimitiveType
            where T5 : IMem, IPrimitiveType
            where T6 : IMem, IPrimitiveType
            where T7 : IMem, IPrimitiveType
            where T8 : IMem, IPrimitiveType
            where TResult : IMem, IPrimitiveType
        {
            throw new NotSupportedException("Cannot call this method from code, only inside a kernel");
        }

        public override void Dispose()
        {
            if (!_disposed)
            {
                _context.Dispose();
                _disposed = true;
            }
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            foreach (var device in _devices)
            {
                Cl.ErrorCode error;
                var platform =
                    Cl.GetDeviceInfo(device, Cl.DeviceInfo.Platform, out error).CastTo<Cl.Platform>();
                var deviceType =
                    Cl.GetDeviceInfo(device, Cl.DeviceInfo.Type, out error).CastTo<Cl.DeviceType>();

                builder.AppendFormat("[Platform: {0}, device type:{1}]\n",
                                     Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Name, out error), deviceType);
            }

            return builder.ToString();
        }

        public IEnumerable<Cl.Device> Devices
        {
            get
            {
                return _devices;
            }
        }

#if DEBUG
        public string GetBufferValues<T>(Buffer<T> buffer, CommandQueue commandQueue = null)
            where T: struct, IMem
        {
            bool disposeCommandQueue = commandQueue == null;
            commandQueue = commandQueue ?? new CommandQueue(this, _devices.First());

            var resultData = new T[buffer.Length];
            commandQueue
                .Add(buffer.Read(0, buffer.Length, resultData))
                .Finish();

            var result = new StringBuilder();
            for (int i = 0; i < resultData.Length; i++)
            {
                result.Append(resultData[i].ToString());
                if (i < resultData.Length - 1)
                    result.Append(", ");
            }

            if (disposeCommandQueue)
                commandQueue.Dispose();

            return result.ToString();
        }
#endif

        private static string WildcardToRegex(string pattern)
        {
            return "^" + Regex.Escape(pattern).
            Replace("\\*", ".*").
            Replace("\\?", ".") + "$";
        }

        public static ComputeProvider Create(string platformName = "*", Cl.DeviceType deviceType = Cl.DeviceType.Default)
        {
            var platformNameRegex = new Regex(WildcardToRegex(platformName), RegexOptions.IgnoreCase);
            Cl.Platform? currentPlatform = null;
            Cl.ErrorCode error;
            foreach (Cl.Platform platform in Cl.GetPlatformIDs(out error))
                if (platformNameRegex.Match(Cl.GetPlatformInfo(platform, Cl.PlatformInfo.Name, out error).ToString()).Success)
                {
                    currentPlatform = platform;
                    break;
                }

            if (currentPlatform == null)
                throw new PlatformNotSupportedException(string.Format("Could not find a platform that matches {0}", platformName));

            var compatibleDevices = from device in Cl.GetDeviceIDs(currentPlatform.Value, deviceType, out error)
                                    select device;
            if (compatibleDevices.Count() == 0)
                throw new PlatformNotSupportedException(string.Format("Could not find a device with type {0} on platform {1}",
                    deviceType, Cl.GetPlatformInfo(currentPlatform.Value, Cl.PlatformInfo.Name, out error)));

            return new ComputeProvider(compatibleDevices.ToArray().First());
        }
    }
}