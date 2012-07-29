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

        static void toMatrix(int32[] arr, int size)
        {
            for (int i = 0; i < arr.Length / size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    //System.Console.Write(arr[i * size + j]);
                    //if (j > 0 && (j + 1) % 5 == 0)
                    //{
                    //    System.Console.Write("|");
                    //}
                    
                    if ((j) % 5 == 0)
                    {
                        System.Console.Write(arr[i * size + j]);
                        System.Console.Write("|");
                    }
                }
                System.Console.WriteLine();
            }
        }

        static void Do()
        {
            var inArr = new uint32[] {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2 };
            int32 size = inArr.Length;

            var rules = new Rule[] {new Rule(1,2,3,0,0),new Rule(2,1,0,0,0),new Rule(3,2,0,0,0),new Rule(2,3,2,0,0)
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

            var bArr = new int32[size * size * nTerms * 5];
            var rulesArr = new int32[rules.Length * 5];

            for (int i = 0; i < size; i++)
            {

                for (int j = 0; j < rules.Length; j++)
                {
                    if (inArr[i] == (rules[j]).b && (rules[j]).c == 0)
                    {
                        var _base = i * nTerms * 5 + (int) (rules[j].a -1) * 5;
                        bArr[_base] = (BT.int32)rules[j].a;
                        bArr[_base + 1] = (BT.int32)0;
                        bArr[_base + 2] = (BT.int32)(rules[j].lblNum == 0 ? 1 : 0);
                        bArr[_base + 3] = (BT.int32)rules[j].lblNum;
                        bArr[_base + 4] = (BT.int32)rules[j].lblWeight;
                    }
                }
            }

            for (int i = 0; i < rules.Length; i++)
            {
                rulesArr[i * 5] = rules[i].a;
                rulesArr[i * 5 + 1] = rules[i].b;
                rulesArr[i * 5 + 2] = rules[i].c;
                rulesArr[i * 5 + 3] = rules[i].lblNum;
                rulesArr[i * 5 + 4] = rules[i].lblWeight;
            }

            var buffer = new Buffer<int32>(provider, Operations.ReadWrite, Memory.Device, bArr);
            var rulesBuf = new Buffer<int32>(provider, Operations.ReadOnly, Memory.Device, rulesArr);

            var processRow =
                provider.Compile<_2D, int32, int32, Buffer<int32>, Buffer<int32>>(
                (range, l, rule_id, a, _rulesBuf) => 
                    from r in range
                    let i = r.GlobalID0
                    let k = r.GlobalID1
                    let _base = provider.CompileFunction((int32 _l, int32 _size, int32 _nTerms) => (int32)(_l * _size * 5 * _nTerms))
                    let left_base_idx = _base(k, size, nTerms) + i * nTerms * 5
                    let right_base_id = _base((l - k - 1), size, nTerms) + (k + i + 1) * nTerms * 5
                    let rule_a = _rulesBuf[(rule_id * 5)]
                    let rule_b = _rulesBuf[(rule_id * 5) + 1]
                    let rule_c = _rulesBuf[(rule_id * 5) + 2]
                    let left = a[left_base_idx + (rule_b - 1) * 5]
                    let right = a[right_base_id + (rule_c - 1) * 5]
                    let v = (rule_b == left && rule_c == right && rule_c != 0)
                            ? rule_a
                            : a[_base(l, size, nTerms) + i * nTerms * 5 + (rule_a - 1) * 5]
                    select new[] { a[_base(l, size, nTerms) + i * nTerms * 5 + (rule_a - 1) * 5] <= v });

            for (int l = 1; l < size; l++)
            {
                for (int rId = 0; rId < rules.Length; rId++)
                {
                    commandQueue.Add(processRow.Run(new _2D(size - l, l), l, rId, buffer, rulesBuf)).Barrier();
                }
            }
            commandQueue.Finish();
            commandQueue.Add(buffer.Read(0, size * size * nTerms * 5, bArr)).Finish();
            toMatrix(bArr, (int) (size * nTerms * 5));
            buffer.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}