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
using Brahma.OpenCL;
using Brahma.Samples;
using Brahma.Types;
using OpenCL.Net;

namespace BlackScholes
{
    class Program
    {
        private const float Eps = 0.001f;
        
        private const float RiskFreeInterestRate = 0.02f;
        private const float Volatility = 0.30f;

        private const float A1 = 0.31938153f;
        private const float A2 = -0.356563782f;
        private const float A3 = 1.781477937f;
        private const float A4 = -1.821255978f;
        private const float A5 = 1.330274429f;

        private static float CumulativeNormalDistribution(float x)
        {
            var l = Math.Abs(x);
            var k = 1f / (1f + 0.2316419f * l);
            var cnd = 1f - 1f / (float)Math.Sqrt(2f * Math.PI) *
                (float)Math.Exp(-l * l / 2f) * (A1 * k + A2 * k * k + A3 * (float)Math.Pow(k, 3f) +
                A4 * (float)Math.Pow(k, 4f) + A5 * (float)Math.Pow(k, 5f));
            return x < 0f ? 1f - cnd : cnd;
        }

        public static float BlackScholesCallOption(float stockPrice, float strikePrice, float timeToExpirationYears, float riskFreeInterestRate, float volatility)
        {
            var d1 = (float)(Math.Log(stockPrice / strikePrice) + (riskFreeInterestRate + volatility * volatility / 2.0) * timeToExpirationYears) / (float)(volatility * Math.Sqrt(timeToExpirationYears));
            var d2 = d1 - volatility * (float)Math.Sqrt(timeToExpirationYears);
            return stockPrice * CumulativeNormalDistribution(d1) -
                   strikePrice * (float)Math.Exp(-riskFreeInterestRate * timeToExpirationYears) * CumulativeNormalDistribution(d2);
        }

        public static float BlackScholesPutOption(float stockPrice, float strikePrice, float timeToExpirationYears, float riskFreeInterestRate, float volatility)
        {
            var d1 = (float)(Math.Log(stockPrice / strikePrice) + (riskFreeInterestRate + volatility * volatility / 2.0) * timeToExpirationYears) / (float)(volatility * Math.Sqrt(timeToExpirationYears));
            var d2 = d1 - volatility * (float)Math.Sqrt(timeToExpirationYears);
            return strikePrice * (float)Math.Exp(-riskFreeInterestRate * timeToExpirationYears) * CumulativeNormalDistribution(-d2) - stockPrice * CumulativeNormalDistribution(-d1);
        }
        
        static void Main(string[] args)
        {
            string platformName = "*";
            int iterations = 16;
            int32 optionCount = 4000000;
            int localWorkSize = 32;
            Cl.DeviceType deviceType = Cl.DeviceType.Default;

            args.Process(() => Console.WriteLine("Usage is {0} device=<Cpu/Gpu/Default> localWorkSize=<local work size (32)> iterations=<Number of iterations to run (16)> optionCount=<Number of options to evaluate (4000000)>",
                Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().CodeBase)),
                new CommandLine.Switch("platform", v => platformName = v.First()),
                new CommandLine.Switch("device", v => deviceType = (Cl.DeviceType)Enum.Parse(typeof(Cl.DeviceType), v.First())),
                new CommandLine.Switch("localWorkSize", v => localWorkSize = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("iterations", v => iterations = int.Parse(v.First(), CultureInfo.CurrentCulture)),
                new CommandLine.Switch("optionCount", v => optionCount = int.Parse(v.First(), CultureInfo.CurrentCulture)));

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

            var random = new Random(2009);

            var putCpu = (from value in Enumerable.Range(0, optionCount) select -1f).ToArray();
            var callCpu = (from value in Enumerable.Range(0, optionCount) select -1f).ToArray();
            
            var putParallel = (from value in putCpu select value).ToArray();
            var callParallel = (from value in callCpu select value).ToArray();

            var stockPrices = (from idx in Enumerable.Range(0, optionCount) select random.Random(5f, 30f)).ToArray();
            var strikePrices = (from idx in Enumerable.Range(0, optionCount) select random.Random(1f, 100f)).ToArray();
            var timesToExpiration = (from idx in Enumerable.Range(0, optionCount) select random.Random(0.25f, 10f)).ToArray();

            #region Compute call and put on the CPU using .NET

            Console.Write("Running {0} iterations on {1} options using .NET...", iterations, optionCount);
            for (int iteration = 0; iteration < iterations; iteration++)
            {
                Timer<string>.Global.Start();

                for (int i = 0; i < optionCount; i++)
                {
                    putCpu[i] = BlackScholesPutOption(stockPrices[i], strikePrices[i], timesToExpiration[i],
                                                      RiskFreeInterestRate, Volatility);
                    callCpu[i] = BlackScholesCallOption(stockPrices[i], strikePrices[i], timesToExpiration[i],
                                                        RiskFreeInterestRate, Volatility);
                }

                Timer<string>.Global.Lap(".NET", true);
            }
            
            Console.WriteLine("done");
            
            #endregion

            #region Compute call and put using OpenCL

            var putBuffer = new Buffer<float32>(provider, Operations.WriteOnly, false, optionCount);
            var callBuffer = new Buffer<float32>(provider, Operations.WriteOnly, false, optionCount);

            var stockPriceBuffer = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, stockPrices);
            var strikePriceBuffer = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, strikePrices);
            var timeToExpirationBuffer = new Buffer<float32>(provider, Operations.ReadOnly, Memory.Device, timesToExpiration);

            Console.Write("Compiling kernel(s)...");
            var blackScholes = provider.Compile
                <_1D, Buffer<float32>, Buffer<float32>, Buffer<float32>, Buffer<float32>, Buffer<float32>>(
                    (range, stocks, strikes, times, call, put) =>
                    from r in range

                    let index = r.GlobalID0

                    let l = provider.CompileFunction<float32, float32>(l => Brahma.Math.Fabs(l))

                    let k = provider.CompileFunction<float32, float32>(k => 1f / (1f + 0.2316419f * l(k)))

                    let cnd = provider.CompileFunction<float32, float32>(x => 1f - 1f / Brahma.Math.Sqrt(2f * Brahma.Math.PI) *
                                                                              Brahma.Math.Exp(-l(x) * l(x) / 2f) *
                                                                              (A1 * k(x) + A2 * k(x) * k(x) + A3 * Brahma.Math.Powr<float32>(k(x), 3f) +
                                                                               A4 * Brahma.Math.Powr<float32>(k(x), 4f) + A5 * Brahma.Math.Powr<float32>(k(x), 5f)))

                    let cumulativeNormalDistribution = provider.CompileFunction((float32 x) => x < 0f ? (float32) (1f - cnd(x)) : cnd(x))

                    let d1 = provider.CompileFunction((float32 stockPrice, float32 strikePrice, float32 timeToExpiration) =>
                                                                                          (float32)(Brahma.Math.Log(stockPrice / strikePrice) + (RiskFreeInterestRate + Volatility * Volatility / 2f) * timeToExpiration / (float) (Volatility * Brahma.Math.Sqrt(timeToExpiration))))
                    let d2 = provider.CompileFunction((float32 d, float32 timeToExpiration) => (float32)(d - Volatility * Brahma.Math.Sqrt(timeToExpiration)))

                    let callOption = provider.CompileFunction(
                        (float32 stockPrice, float32 strikePrice, float32 timeToExpiration) => (float32)(stockPrice * cumulativeNormalDistribution(d1(stockPrice, strikePrice, timeToExpiration)) -
                                                                       strikePrice * Brahma.Math.Exp(-RiskFreeInterestRate * timeToExpiration) * cumulativeNormalDistribution(d2(d1(stockPrice, strikePrice, timeToExpiration), timeToExpiration))))

                    let putOption = provider.CompileFunction(
                        (float32 stockPrice, float32 strikePrice, float32 timeToExpiration) => (float32)(strikePrice * Brahma.Math.Exp(-RiskFreeInterestRate * timeToExpiration) * cumulativeNormalDistribution(-d2(d1(stockPrice, strikePrice, timeToExpiration), timeToExpiration)) - 
                            stockPrice * cumulativeNormalDistribution(-d1(stockPrice, strikePrice, timeToExpiration))))

                    select new Brahma.Set[]
                               {
                                   call[index] <= callOption(stocks[index], strikes[index], times[index]),
                                   put[index] <= putOption(stocks[index], strikes[index], times[index])
                               });
            Console.WriteLine("done.");

            Console.Write("Running {0} iterations on {1} options using OpenCL...", iterations, optionCount);

            Timer<string>.Global.Start(); // We're going to time the OpenCL version a little differently
            for (int iteration = 0; iteration < iterations; iteration++)
            {
                commandQueue.Add(blackScholes.Run(new _1D(optionCount, localWorkSize), stockPriceBuffer,
                                                  strikePriceBuffer, timeToExpirationBuffer, callBuffer, putBuffer))
                            .Finish();
            }

            commandQueue.Finish();
            Timer<string>.Global.Lap("OpenCL", true);

            Console.WriteLine("done");

            Console.Write("Verifying results...");
            commandQueue
                .Add(putBuffer.Read(0, optionCount, putParallel))
                .Add(callBuffer.Read(0, optionCount, callParallel))
                .Finish();

            for (int i = 0; i < optionCount; i++)
            {
                if (Math.Abs(putCpu[i] - putParallel[i]) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", putCpu[i], putParallel[i],
                                                                    Math.Abs(putCpu[i] - putParallel[i])));
                if (Math.Abs(callCpu[i] - callParallel[i]) > Eps)
                    throw new InvalidProgramException(string.Format("Expected: {0} Actual: {1} Error = {2}", callCpu[i], callParallel[i],
                                                                    Math.Abs(callCpu[i] - callParallel[i])));
            }
            Console.WriteLine("done.");

            #endregion

            Console.WriteLine("Avg. time, C#: {0}", Timer<string>.Global.Average(".NET"));
            Console.WriteLine("Avg. time, OpenCL: {0}", Timer<string>.Global.Total("OpenCL") / iterations);
        }
    }
}
