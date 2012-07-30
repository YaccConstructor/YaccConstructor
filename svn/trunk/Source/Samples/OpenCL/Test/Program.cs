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
        static int32 magicConst = 3;
        static int32 magicConst_c = 1;        
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
                    System.Console.Write(arr[i * size + j]);
                    if (j > 0 && (j + 1) % magicConst_c == 0)
                    {
                        System.Console.Write("|");
                    }

                    //if ((j) % magicConst == 0)
                    //{
                    //    System.Console.Write(arr[i * size + j]);
                    //    System.Console.Write("|");
                    //}
                }
                System.Console.WriteLine();
            }
        }

        static void Do()
        {

            var inArr = new int32
                             //[]
                             [3002] 
                             //{ 2, 1, 2 }
                             //{2, 2, 2, 2, 2, 2, 2, 1, 2 }
                            //{2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2 }
                            //{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2 }
                            ;
            for (int _i = 0; _i < 3002; _i++) { inArr[_i] = 2; }
            inArr[3001] = 1;
            int32 size = inArr.Length;

            var rules = new Rule[] {new Rule(1,2,3,0,0),new Rule(2,3,2,0,0),new Rule(2,1,0,0,0),new Rule(3,2,0,0,0)
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

            var bArr = new int32[size * size * nTerms * magicConst_c];
            var rulesArr = new int32[rules.Length * magicConst];

            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < rules.Length; j++)
                {
                    if (inArr[i] == (rules[j]).b && (rules[j]).c == 0)
                    {
                        var _base = i * nTerms * magicConst_c + (int)(rules[j].a - 1) * magicConst_c;
                        bArr[_base] = rules[j].a;
                        //bArr[_base + 1] = 0;
                        //bArr[_base + 2] = (rules[j].lblNum == 0 ? 1 : 0);
                        ///bArr[_base + 3] = rules[j].lblNum;
                        //bArr[_base + 4] = rules[j].lblWeight;
                    }
                }
            }

            for (int i = 0; i < rules.Length; i++)
            {
                var _base =  i * magicConst;
                rulesArr[_base] = rules[i].a;
                rulesArr[_base + 1] = rules[i].b;
                rulesArr[_base + 2] = rules[i].c;
                //rulesArr[_base + 3] = rules[i].lblNum;
                //rulesArr[_base + 4] = rules[i].lblWeight;
            }            

            var buffer = new Buffer<int32>(provider, Operations.ReadWrite, Memory.Device, bArr);            
            var db = new Buffer<int32>(provider, Operations.ReadWrite, Memory.Device, new int32[1]);

            var processRow =
                provider.Compile<_1D, int32, int32, int32, int32, Buffer<int32>>(
                (range, l, rule_a, rule_b, rule_c, a) =>
                    from r in range
                    let i = r.GlobalID0
                    let nT = nTerms
                    let _base = nT * size
                    let iter = provider.Loop(0, l, kIdx=>from k in kIdx 
                        let left_base_idx = (k * _base) + i * nT
                        let right_base_idx = ((l - k - 1) * _base) + (k + i + 1) * nT
                        let left = a[left_base_idx + (rule_b - 1)]
                        let right = a[right_base_idx + (rule_c - 1)]
                        let res_id = (l * _base) + i * nT + (rule_a - 1)
                        let v = (rule_c != 0 & rule_c == right & rule_b == left)
                                ? rule_a
                                : a[res_id]
                        select new[] {a[res_id] <= v})
                    select new[] { a[0] <= a[0] });

            for (int l = 1; l < size; l++)
            {
                for (int rId = 0; rId < rules.Length; rId++)
                {
                        var rule_base = rId * magicConst;
                        var rule_a = rulesArr[rule_base];
                        var rule_b = rulesArr[rule_base + 1];
                        var rule_c = rulesArr[rule_base + 2];
                        commandQueue.Add(processRow.Run(new _1D(size - l), l, rule_a, rule_b, rule_c, buffer)).Finish();
                }
                //commandQueue.Finish();
            }
            //commandQueue.Finish();
            commandQueue.Add(buffer.Read(0, size * size * nTerms * magicConst_c, bArr)).Finish();
            //toMatrix(bArr, (int)(size * nTerms * magicConst_c));
            buffer.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}