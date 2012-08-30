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
        static int32 ruleRepresentationLength = 3;
        static int32 cellDataRepresentationLength = 1;        
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
                    if (j > 0 && (j + 1) % cellDataRepresentationLength == 0)
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
            var c = 4;
            var inArr = new int32 [c];
            for (int _i = 0; _i < c; _i++) { inArr[_i] = 2; }
            inArr[c-2] = 1;
            int32 size = inArr.Length;

            var rules = new Rule[] {new Rule(1,2,3,0,0),new Rule(2,3,2,0,0),new Rule(2,1,0,0,0),new Rule(3,2,0,0,0)};

            var nTermRules = (from rule in rules where rule.c > 0 select rule).ToArray();

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

            var bArr = new int32[size * size * nTerms * cellDataRepresentationLength];
            var rulesArr = new int32[nTermRules.Length * ruleRepresentationLength];

            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < rules.Length; j++)
                {
                    if (inArr[i] == (rules[j]).b && (rules[j]).c == 0)
                    {
                        var _base = (i * nTerms  + (int)(rules[j].a - 1)) * cellDataRepresentationLength;
                        bArr[_base] = rules[j].a;
                        //bArr[_base + 1] = 0;
                        //bArr[_base + 2] = (rules[j].lblNum == 0 ? 1 : 0);
                        ///bArr[_base + 3] = rules[j].lblNum;
                        //bArr[_base + 4] = rules[j].lblWeight;
                    }
                }
            }

            for (int i = 0; i < nTermRules.Length; i++)
            {
                var _base =  i * ruleRepresentationLength;
                rulesArr[_base] = nTermRules[i].a;
                rulesArr[_base + 1] = nTermRules[i].b;
                rulesArr[_base + 2] = nTermRules[i].c;
                //rulesArr[_base + 3] = rules[i].lblNum;
                //rulesArr[_base + 4] = rules[i].lblWeight;
            }            

            var buffer = new Buffer<int32>(provider, Operations.ReadWrite, Memory.Device, bArr);
            var rulesBuffer = new Buffer<int32>(provider, Operations.ReadOnly, Memory.Device, rulesArr);

            int32 rLength = nTermRules.Length;

            var processRow = provider.Compile<_1D, int32, Buffer<int32>, Buffer<int32>>(
                (range, l, a, _rules) =>
                    from r in range
                    let i = r.GlobalID0
                    let nT = nTerms
                    let _base = nT * size
                    let res_id_base = (l * _base) + i * nT
                    let iter = provider.Loop(0, l, kIdx=>
                        from k in kIdx
                        let left_base_idx = (k * _base) + i * nTerms
                        let right_base_idx = ((l - k - 1) * _base) + (k + i + 1) * nTerms
                        let iter2 = provider.Loop(0, rLength, rIdxs =>
                            from rId in rIdxs
                            let rule_base = rId * ruleRepresentationLength
                            let rule_a = _rules[rule_base]
                            let rule_b = _rules[rule_base + 1]
                            let rule_c = _rules[rule_base + 2]
                            let res_id = res_id_base + (rule_a - 1)
                            select new[]{(rule_c != 0
                                          & rule_c == a[right_base_idx + (rule_c - 1)] 
                                          & rule_b == a[left_base_idx + (rule_b - 1)])
                                          ? a[res_id] <= rule_a
                                          : ((Brahma.Set<int32>)null)})
                        select (Brahma.Set<int32>[])null)
                    select (Brahma.Set<int32>[])null);

            for (int l = 1; l < size; l++)
            {
                commandQueue.Add(processRow.Run(new _1D(size - l), l, buffer, rulesBuffer)).Finish();
            }
            
            commandQueue.Add(buffer.Read(0, size * size * nTerms * cellDataRepresentationLength, bArr)).Finish();
            toMatrix(bArr, (int)(size * nTerms * cellDataRepresentationLength));
            buffer.Dispose();

            commandQueue.Dispose();
            provider.Dispose();
        }
    }
}