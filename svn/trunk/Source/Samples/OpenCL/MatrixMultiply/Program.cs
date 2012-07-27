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
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

using Brahma.Types;
using Brahma.Samples;

using OpenCL.Net;

namespace Brahma.OpenCL.Samples.MatrixMultiply
{
    class Program
    {
        private static readonly Random Random = new Random();
        
        private static float32[] MakeMatrix(int rows, int cols)
        {
            var result = new float32[rows * cols];
            for (int i = 0; i < rows * cols; i++)
                result[i] = (float32)Random.NextDouble();

            return result;
        }

        private static void GetOutputMatrixDimensions(int aRows, int aCols, int bRows, int bCols, out int cRows, out int cCols)
        {
            if (aCols != bRows)
                throw new InvalidOperationException("Cannot multiply these two matrices");

            cRows = aRows;
            cCols = bCols;
        }

        private static void Multiply(float32[] a, int aRows, int aCols, float32[] b, int bRows, int bCols, ref float32[] c)
        {
            int cRows;
            int cCols;
            GetOutputMatrixDimensions(aRows, aCols, bRows, bCols, out cRows, out cCols);

            for (int i = 0; i < cRows; i++)
                for (int j = 0; j < cCols; j++)
                {
                    float32 tmp = 0;
                    for (int k = 0; k < aCols; k++)
                        tmp += a[i * aCols + k] * b[k * bCols + j];

                    c[i * cCols + j] = tmp;
                }
        }

        static void Main(string[] args)
        {
            string platformName = "*";

            int32 rows = 100;
            int32 columns = 100;
            int32 localWorkSize = 10;
            int32 iterations = 100;
            Cl.DeviceType deviceType = Cl.DeviceType.Default;

            args.Process(() => Console.WriteLine("Usage is {0} platform=<platform name with wildcards (*)> device=<Cpu/Gpu/Default> localWorkSize=<local work size (10)> iterations=<Number of iterations to run (100)> (Default)> rows=<rows (100)> cols=<columns (100)>",
                Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().CodeBase)),
                new CommandLine.Switch("platform", v => platformName = v.First()),
                new CommandLine.Switch("device", v => deviceType = (Cl.DeviceType)Enum.Parse(typeof(Cl.DeviceType), v.First())),
                new CommandLine.Switch("rows", v => rows = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("cols", v => columns = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("localWorkSize", v => localWorkSize = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("iterations", v => iterations = int.Parse(v.First(), CultureInfo.CurrentCulture)));

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

            var aValues = MakeMatrix(rows, columns);
            var bValues = MakeMatrix(rows, columns);
            var cParallel = new float32[rows * columns];

            var aBuffer = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, aValues);
            var bBuffer = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, bValues);
            var cBuffer = new Buffer<float32>(provider, Operations.ReadWrite, Memory.Device, cParallel);

            var matrixMult = provider.Compile<_2D, Buffer<float32>, Buffer<float32>, Buffer<float32>>(
                (range, a, b, c) => from r in range
                                    let tx = r.GlobalID0
                                    let ty = r.GlobalID1

                                    let value = default(float32)
                                    let sum = provider.Loop(0, columns, kIndices => from k in kIndices
                                                                                    let elementA = a[ty * columns + k]
                                                                                    let elementB = b[k * columns + tx]
                                                                                    select new[]
                                                                                               {
                                                                                                   value <= value + (elementA * elementB)
                                                                                               })
                                    select new[]
                                               {
                                                   c[ty * columns + tx] <= value
                                               });

            Console.Write("Multiplying two {0}x{1} matrices {2} times using .NET...", rows, columns, iterations);
            var cNormal = new float32[rows * columns];
            for (int i = 0; i < iterations; i++)
            {
                Timer<string>.Global.Start();
                Multiply(aValues, rows, columns, bValues, rows, columns, ref cNormal);
                Timer<string>.Global.Lap(".NET");
            }
            Console.WriteLine("done.");

            Console.Write("Multiplying two {0}x{1} matrices {2} times using Brahma.OpenCL and selected platform/device...", rows, columns, iterations);
            for (int i = 0; i < iterations; i++)
            {
                Timer<string>.Global.Start();
                commandQueue.Add(matrixMult.Run(new _2D(rows, columns, localWorkSize, localWorkSize), aBuffer, bBuffer, cBuffer))
                    .Finish();
                Timer<string>.Global.Lap("OpenCL");
            }
            Console.WriteLine("done.");

            Console.Write("Verifying results...");
            commandQueue.Add(cBuffer.Read(0, rows * columns, cParallel))
                .Finish();
            for (int i = 0; i < rows * columns; i++)
                if (System.Math.Abs(cParallel[i] - cNormal[i]) > 0.00001f)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", cNormal[i], cParallel[i],
                                                                    System.Math.Abs(cParallel[i] - cNormal[i])));
            Console.WriteLine("done.");

            Console.WriteLine("Avg. time, C#: {0}", Timer<string>.Global.Average(".NET"));
            Console.WriteLine("Avg. time, OpenCL: {0}", Timer<string>.Global.Average("OpenCL"));

            aBuffer.Dispose();
            bBuffer.Dispose();
            cBuffer.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}