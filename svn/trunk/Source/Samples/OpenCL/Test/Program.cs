using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Brahma.Types;
using Brahma.OpenCL;

using OpenCL.Net;

using BT = Brahma.Types;

namespace Test
{
    struct SingleCellData
    {
        public BT.uint16 rId;
        public BT.uint32 k;
        public BT.uint8 lblNum;
        public BT.uint8 lblWeight;
        public BT.uint8 lblState;

        public SingleCellData(BT.uint16 _rId, BT.uint32 _k, BT.uint8 _lblNum, BT.uint8 _lblWeight, BT.uint8 _lblState)
        {
            rId = _rId;
            k = _k;
            lblNum = _lblNum;
            lblWeight = _lblWeight;
            lblState = _lblState;
        }

        public SingleCellData(BT.uint16 _rId, Rule rule)
        {
            rId = _rId;
            k = 0;
            lblNum = rule.lblNum;
            lblWeight = rule.lblWeight;
            lblState = (BT.uint8)((rule.lblNum == 0) ? 1 : 0);
        }
    }

    struct Rule
    {
        public BT.uint16 a;
        public BT.uint16 b;
        public BT.uint16 c;
        public BT.uint8 lblNum;
        public BT.uint8 lblWeight;

        public Rule(BT.uint16 _a, BT.uint16 _b, BT.uint16 _c, BT.uint8 _lblNum, BT.uint8 _lblWeight)
        {
            a = _a;
            b = _b;
            c = _c;
            lblNum = _lblNum;
            lblWeight = _lblWeight;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var start = System.DateTime.Now;
            Do();
            System.Console.WriteLine("Time DX9=" + (System.DateTime.Now - start));
        }

        static void Do()
        {
            var inArr = new uint32[] { 1, 2 };
            int32 size = inArr.Length;

            var rules = new Rule[] {new Rule(1,2,3,0,0),new Rule(2,1,0,0,0),new Rule(3,2,0,0,0)
            };

            int32 nTerms = 3;

            string platformName = "*";

            Cl.DeviceType deviceType = Cl.DeviceType.Default;

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

            var commandQueue = new CommandQueue(provider, provider.Devices.First());

            var bArr = new float32[size * size * nTerms * 5];

            for (int i = 0; i < size; i++)
            {

                for (int j = 0; j < rules.Length; j++)
                {
                    if (inArr[i] == (rules[j]).b && (rules[j]).c == 0)
                    {
                        bArr[i * nTerms * 5] = (BT.float32)j;
                        bArr[i * nTerms * 5 + 1] = (BT.float32)0;
                        bArr[i * nTerms * 5 + 2] = (BT.float32)(rules[j].lblNum == 0 ? 1 : 0);
                        bArr[i * nTerms * 5 + 3] = (BT.float32)rules[j].lblNum;
                        bArr[i * nTerms * 5 + 4] = (BT.float32)rules[j].lblWeight;
                    }
                }
            }

            var buffer = new Buffer<float32>(provider, Operations.ReadWrite, Memory.Device, bArr);

            var recognize = provider.Compile<_2D, Buffer<float32>>(
                (range, a) => from r in range
                              let i = r.GlobalID0
                              let k = r.GlobalID1
                              let sum =
                                provider.Loop(1, size,
                                    kIndices =>
                                        from l in kIndices
                                        let _base = provider.CompileFunction((int32 _l, int32 _size, int32 _nTerms) => (int32)(_l * _size * 5 * _nTerms))
                                        let v = (float32)(i < size - l && k < l ? ((a[_base(l - k - 1, size, nTerms) + i*nTerms*5] + a[_base(l - k - 1, size, nTerms) + (i+ 1)*nTerms*5 ])) : 0)
                                        select new[] { a[_base(l, size, nTerms) + i * nTerms * 5] <= v })
                              select new[] { a[0] <= a[0] });

            commandQueue.Add(recognize.Run(new _2D(size - 1, size), buffer))
                    .Finish();
            commandQueue.Add(buffer.Read(0, size * size * nTerms * 5, bArr))
                .Finish();
            foreach (var x in bArr)
            {
                System.Console.WriteLine(x);
            }
            buffer.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}