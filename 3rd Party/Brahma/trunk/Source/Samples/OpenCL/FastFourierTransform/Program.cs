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
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

using Brahma.Types;
using Brahma.Samples;

using OpenCL.Net;

namespace Brahma.OpenCL.Samples.FastFourierTransform
{
    [DebuggerDisplay("{ToString()}")]
    public struct Complex<T>
        where T : IComparable
    {
        public readonly T A;
        public readonly T B;

        public Complex(T a, T b)
        {
            A = a;
            B = b;
        }

        public override string ToString()
        {
            return A + (B.CompareTo(0.0) < 0.0 ? " - i" : " + i") + B;
        }
    }
    
    class Program
    {
        private const float Eps = 0.001f;
        
        private static bool PowerOfTwo(int x)
        {
            return ((x > 0) && ((x & (x - 1)) == 0));
        }

        private static void Swap<T>(ref T a, ref T b)
        {
            var temp = a;
            a = b;
            b = temp;
        }

        private static Complex<float> FFT(int x, int fftSize, Complex<float>[] input, int size)
        {
            int b = (((int) System.Math.Floor((float) x / fftSize)) * (fftSize / 2));
            int offset = x % (fftSize / 2);
            int x0 = b + offset;
            int x1 = x0 + size / 2;

            Complex<float> val0 = input[x0];
            Complex<float> val1 = input[x1];
            double angle = -2 * System.Math.PI * ((float) x / fftSize);
            var t = new Complex<float>((float) System.Math.Cos(angle), (float) System.Math.Sin(angle));

            return new Complex<float>(val0.A + t.A * val1.A - t.B * val1.B, val0.B + t.B * val1.A + t.A * val1.B);
        }

        static void Main(string[] args)
        {
            string platformName = "*";
            int32 size = 1024;
            int localWorkSize = 32;
            int iterations = 100;
            Cl.DeviceType deviceType = Cl.DeviceType.Default;

            args.Process(() => Console.WriteLine("Usage is {0} device=<Cpu/Gpu/Default> localWorkSize=<local work size (32)> iterations=<Number of iterations to run (100)> size=<Size of input - must be a power of two (1024)>", 
                Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().CodeBase)), 
                new CommandLine.Switch("platform", v => platformName = v.First()),
                new CommandLine.Switch("device", v => deviceType = (Cl.DeviceType)Enum.Parse(typeof(Cl.DeviceType), v.First())),
                new CommandLine.Switch("localWorkSize", v => localWorkSize = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("iterations", v => iterations = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("size", v => size = int.Parse(v.First(), CultureInfo.CurrentCulture)));

            if (!PowerOfTwo(size))
            {
                Console.WriteLine("{0} isn't a power of two, please specify a power of two as the size", size);
                return;
            }

            ComputeProvider provider;
            try
            {
                provider = ComputeProvider.Create(platformName, deviceType);
            }
            catch (PlatformNotSupportedException ex)
            {
                Console.WriteLine(ex.Message);
                return;
            }
            Console.WriteLine("Using " + provider);
            
            var commandQueue = new CommandQueue(provider, provider.Devices.First());

            var random = new Random();

            var inputRealData = (from index in Enumerable.Range(0, size)
                                select (float32) random.NextDouble()).ToArray();
            var zeros = (from index in Enumerable.Range(0, size)
                          select (float32)0.0f).ToArray();
            var inputReal = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, inputRealData);
            var inputImaginary = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, zeros);

            var outputReal = new Buffer<float32>(provider, Operations.ReadWrite, Memory.Device, zeros);
            var outputImaginary = new Buffer<float32>(provider, Operations.ReadWrite, Memory.Device, zeros);

            var fft = provider.Compile<_1D, int32, Buffer<float32>, Buffer<float32>, Buffer<float32>, Buffer<float32>>(
                (range, fftSize, a, ib, c, id) => from r in range
                                               let x = r.GlobalID0
                                               let b = Math.Floor(x / fftSize) * fftSize / 2
                                               let offset = x % (fftSize/2)
                                               let x0 = b + offset
                                               let x1 = x0 + size / 2
                                               let val0A = a[x0]
                                               let val0B = ib[x0]
                                               let val1A = a[x1]
                                               let val1B = ib[x1]

                                               let angle = -2 * Math.PI * ((float32)x / fftSize)
                                               let tA = Math.Cos(angle)
                                               let tB = Math.Sin(angle)

                                               select new[]
                                                          {
                                                              c[x] <= val0A + tA * val1A - tB * val1B,
                                                              id[x] <= val0B + tB * val1A + tA * val1B
                                                          });

            var conjugate = provider.Compile<_1D, Buffer<float32>, Buffer<float32>>(
                (range, a, ib) => from r in range
                                  let x = r.GlobalID0

                                  select new[]
                                             {
                                                 ib[x] <= -ib[x]
                                             }
                );

            var conjugateAndScale = provider.Compile<_1D, float32, Buffer<float32>, Buffer<float32>>(
                (range, scale, a, ib) => from r in range
                                         let x = r.GlobalID0

                                         select new[]
                                                    {
                                                        a[x] <= a[x] * scale,
                                                        ib[x] <= -ib[x] * scale
                                                    }
                );

            Console.Write("Calculating FFT and inverse FFT using .NET {0} times...", iterations);

            var output = new Complex<float>[size];

            for (int iteration = 0; iteration < iterations; iteration++)
            {
                Complex<float>[] input = (from x in inputRealData
                                          select new Complex<float>(x, 0.0f)).ToArray();

                Timer<string>.Global.Start();

                // Forward FFT
                Complex<float>[] temp;
                {
                    int fftSize = 2;
                    for (int i = 0; i < System.Math.Log(size, 2.0); i++)
                    {
                        for (int x = 0; x < size; x++)
                            output[x] = FFT(x, fftSize, input, size);

                        fftSize *= 2;

                        temp = input;
                        input = output;
                        output = temp;
                    }

                }

                // Inverse FFT
                {
                    int fftSize = 2;

                    for (int i = 0; i < size; i++)
                        input[i] = new Complex<float>(input[i].A, -input[i].B);

                    for (int i = 0; i < System.Math.Log(size, 2.0); i++)
                    {
                        for (int x = 0; x < size; x++)
                            output[x] = FFT(x, fftSize, input, size);

                        fftSize *= 2;

                        temp = input;
                        input = output;
                        output = temp;
                    }

                    temp = input;
                    // input = output;
                    output = temp;

                    for (int i = 0; i < size; i++)
                        output[i] = new Complex<float>(output[i].A / size, -output[i].B / size);
                }

                Timer<string>.Global.Lap(".NET", true);
            }

            Console.WriteLine("done.");


            Console.Write("Verifying results...");
            for (int i = 0; i < size; i++)
            {
                if (System.Math.Abs(inputRealData[i] - output[i].A) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", inputRealData[i], output[i].A,
                                                                    System.Math.Abs(inputRealData[i] - output[i].A)));

                if (System.Math.Abs(output[i].B) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", 0.0f, output[i].B,
                                                                    System.Math.Abs(output[i].B)));
            }
            Console.WriteLine("done");

            Console.Write("Calculating FFT and inverse FFT using OpenCL {0} times...", iterations);

            for (int iteration = 0; iteration < iterations; iteration++)
            {
                commandQueue
                    .Add(inputReal.Write(0, size, inputRealData))
                    .Add(inputImaginary.Write(0, size, zeros))
                    .Finish();

                Timer<string>.Global.Start();
                
                // Forward FFT
                {
                    int fftSize = 2;
                    for (int i = 0; i < System.Math.Log(size, 2.0f); i++)
                    {
                        commandQueue.Add(fft.Run(new _1D(size, localWorkSize), fftSize, inputReal, inputImaginary,
                                                 outputReal, outputImaginary));

                        fftSize *= 2;

                        Swap(ref inputReal, ref outputReal);
                        Swap(ref inputImaginary, ref outputImaginary);
                    }
                }

                // If we were returning the output, we'd have to swap again because output of the last iteration is in the input
                // but this is fine for now because the output of the forward FFT is the input of the inverse FFT anyway

                // Inverse FFT
                {
                    commandQueue.Add(conjugate.Run(new _1D(size, localWorkSize), inputReal, inputImaginary));

                    int fftSize = 2;
                    for (int i = 0; i < System.Math.Log(size, 2.0f); i++)
                    {
                        commandQueue.Add(fft.Run(new _1D(size, localWorkSize), fftSize, inputReal, inputImaginary,
                                                 outputReal, outputImaginary));

                        fftSize *= 2;

                        Swap(ref inputReal, ref outputReal);
                        Swap(ref inputImaginary, ref outputImaginary);
                    }

                    Swap(ref inputReal, ref outputReal);
                    Swap(ref inputImaginary, ref outputImaginary);

                    commandQueue.Add(conjugateAndScale.Run(new _1D(size, localWorkSize), 1.0f/size, outputReal, outputImaginary));
                }

                Timer<string>.Global.Lap("OpenCL", true);
            }
            
            Console.WriteLine("done.");

            var outputRealData = new float32[size];
            var outputImaginaryData = new float32[size];

            commandQueue
                .Add(outputReal.Read(0, size, outputRealData))
                .Add(outputImaginary.Read(0, size, outputImaginaryData))
                .Finish();

            Console.Write("Verifying results...");
            for (int i = 0; i < size; i++)
            {
                if (System.Math.Abs(inputRealData[i] - outputRealData[i]) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", inputRealData[i], outputRealData[i],
                                                                    System.Math.Abs(inputRealData[i] - outputRealData[i])));

                if (System.Math.Abs(outputImaginaryData[i]) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", 0.0f, outputImaginaryData[i],
                                                                    System.Math.Abs(outputImaginaryData[i])));
            }
            Console.WriteLine("done");

            Console.WriteLine("Avg. time, C#: {0}", Timer<string>.Global.Average(".NET"));
            Console.WriteLine("Avg. time, OpenCL: {0}", Timer<string>.Global.Average("OpenCL"));

            inputReal.Dispose();
            inputImaginary.Dispose();
            outputReal.Dispose();
            outputImaginary.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}