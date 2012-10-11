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

using System.Collections.Generic;
using System.Linq;
using Brahma.Commands;
using Brahma.Types;
using OpenCL.Net;

namespace Brahma.OpenCL.Commands
{
    public abstract class RunBase<TRange> : Brahma.Commands.Run<TRange>
        where TRange: struct, INDRangeDimension
    {
        protected override void SetupArgument(object sender, int index, IMem argument)
        {
            var kernel = Kernel as ICLKernel;
            
            Cl.ErrorCode error = Cl.SetKernelArg(kernel.ClKernel, (uint)index, argument.Size, argument.Data);
            if (error != Cl.ErrorCode.Success)
                throw new CLException(error);
        }
        
        protected RunBase(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public override void Execute(object sender)
        {
            base.Execute(sender);

            var queue = sender as CommandQueue;
            var kernel = Kernel as ICLKernel;
            var range = Range as INDRangeDimension;
            var waitList = (from name in WaitList
                            let ev = CommandQueue.FindEvent(name)
                            where ev != null
                            select ev.Value).ToArray();

            Cl.Event eventID;
            Cl.ErrorCode error = Cl.EnqueueNDRangeKernel(queue.Queue, kernel.ClKernel, (uint32)kernel.WorkDim, null,
                range.GlobalWorkSize, range.LocalWorkSize, (uint)waitList.Length, waitList.Length == 0 ? null : waitList.ToArray(), out eventID);
            if (error != Cl.ErrorCode.Success)
                throw new CLException(error);

            if (Name == string.Empty)
                eventID.Dispose();
            else
                CommandQueue.AddEvent(Name, eventID);
        }
    }

    public sealed class Run<TRange>: RunBase<TRange>, ICommand<TRange>
        where TRange: struct, INDRangeDimension
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield break;
            }
        }
        
        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }
    }

    public sealed class Run<TRange, T1> : RunBase<TRange>, ICommand<TRange, T1>
        where TRange : struct, INDRangeDimension
        where T1: IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2> : RunBase<TRange>, ICommand<TRange, T1, T2>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3> : RunBase<TRange>, ICommand<TRange, T1, T2, T3>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3, T4> : RunBase<TRange>, ICommand<TRange, T1, T2, T3, T4>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
                yield return D4;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }

        public T4 D4
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3, T4, T5> : RunBase<TRange>, ICommand<TRange, T1, T2, T3, T4, T5>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
                yield return D4;
                yield return D5;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }

        public T4 D4
        {
            get;
            internal set;
        }

        public T5 D5
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3, T4, T5, T6> : RunBase<TRange>, ICommand<TRange, T1, T2, T3, T4, T5, T6>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
                yield return D4;
                yield return D5;
                yield return D6;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }

        public T4 D4
        {
            get;
            internal set;
        }

        public T5 D5
        {
            get;
            internal set;
        }

        public T6 D6
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3, T4, T5, T6, T7> : RunBase<TRange>, ICommand<TRange, T1, T2, T3, T4, T5, T6, T7>
        where TRange : struct, INDRangeDimension
        where T1 : IMem
        where T2 : IMem
        where T3 : IMem
        where T4 : IMem
        where T5 : IMem
        where T6 : IMem
        where T7 : IMem
    {
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
                yield return D4;
                yield return D5;
                yield return D6;
                yield return D7;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }

        public T4 D4
        {
            get;
            internal set;
        }

        public T5 D5
        {
            get;
            internal set;
        }

        public T6 D6
        {
            get;
            internal set;
        }

        public T7 D7
        {
            get;
            internal set;
        }
    }

    public sealed class Run<TRange, T1, T2, T3, T4, T5, T6, T7, T8> : RunBase<TRange>, ICommand<TRange, T1, T2, T3, T4, T5, T6, T7, T8>
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
        protected override IEnumerable<IMem> Arguments
        {
            get
            {
                yield return D1;
                yield return D2;
                yield return D3;
                yield return D4;
                yield return D5;
                yield return D6;
                yield return D7;
                yield return D8;
            }
        }

        internal Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public T1 D1
        {
            get;
            internal set;
        }

        public T2 D2
        {
            get;
            internal set;
        }

        public T3 D3
        {
            get;
            internal set;
        }

        public T4 D4
        {
            get;
            internal set;
        }

        public T5 D5
        {
            get;
            internal set;
        }

        public T6 D6
        {
            get;
            internal set;
        }

        public T7 D7
        {
            get;
            internal set;
        }

        public T8 D8
        {
            get;
            internal set;
        }
    }
}