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
using Brahma.Types;

namespace Brahma.OpenCL
{
    public interface INDRangeDimension : Brahma.INDRangeDimension
    {
        IntPtr[] GlobalWorkSize
        {
            get;
        }

        IntPtr[] LocalWorkSize
        {
            get;
        }
    }
    
    public struct _1D : INDRangeDimension
    {
        public struct IDs_1D
        {
            private int32 _x;
            
            internal IDs_1D(int32 x)
            {
                _x = x;
            }
            
            [KernelCallable]
            public int32 x
            {
                get
                {
                    return _x;
                }
            }
        }

        private IDs_1D _globalIDs;
        private IDs_1D _localIDs;

        public _1D(int globalWorkSize, int localWorkSize = 1)
        {
            _globalIDs = new IDs_1D(globalWorkSize);
            _localIDs = new IDs_1D(localWorkSize);
        }

        [KernelCallable]
        public int32 GlobalID0
        {
            get
            {
                return _globalIDs.x;
            }
        }

        [KernelCallable]
        public int32 LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }

        int32 Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 1;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_globalIDs.x };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_localIDs.x };
            }
        }
    }

    public struct _2D : INDRangeDimension
    {
        public struct IDs_2D
        {
            private int32 _x;
            private int32 _y;
            
            internal IDs_2D(int32 x, int32 y)
            {
                _x = x;
                _y = y;
            }
            
            [KernelCallable]
            public int32 x
            {
                get
                {
                    return _x;
                }
            }

            [KernelCallable]
            public int32 y
            {
                get
                {
                    return _y;
                }
            }
        }

        private IDs_2D _globalIDs;
        private IDs_2D _localIDs;

        public _2D(int globalWorkSizeX, int globalWorkSizeY,
            int localWorkSizeX = 1, int localWorkSizeY = 1)
        {
            _globalIDs = new IDs_2D(globalWorkSizeX, globalWorkSizeY);
            _localIDs = new IDs_2D(localWorkSizeX, localWorkSizeY);
        }

        [KernelCallable]
        public int32 GlobalID0
        {
            get
            {
                return _globalIDs.x;
            }
        }

        [KernelCallable]
        public int32 GlobalID1
        {
            get
            {
                return _globalIDs.y;
            }
        }

        [KernelCallable]
        public int32 LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }

        [KernelCallable]
        public int32 LocalID1
        {
            get
            {
                return _localIDs.y;
            }
        }

        int32 Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 2;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_globalIDs.x, (IntPtr)(int)_globalIDs.y };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_localIDs.x, (IntPtr)(int)_localIDs.y };
            }
        }
    }

    public struct _3D : INDRangeDimension
    {
        public struct IDs_3D
        {
            private int32 _x;
            private int32 _y;
            private int32 _z;

            internal IDs_3D(int32 x, int32 y, int32 z)
            {
                _x = x;
                _y = y;
                _z = z;
            }

            [KernelCallable]
            public int32 x
            {
                get
                {
                    return _x;
                }
            }

            [KernelCallable]
            public int32 y
            {
                get
                {
                    return _y;
                }
            }

            [KernelCallable]
            public int32 z
            {
                get
                {
                    return _z;
                }
            }
        }

        private IDs_3D _globalIDs;
        private IDs_3D _localIDs;

        public _3D(int globalSizeX, int globalSizeY, int globalSizeZ,
            int localSizeX = 1, int localSizeY = 1, int localSizeZ = 1)
        {
            _globalIDs = new IDs_3D(globalSizeX, globalSizeY, globalSizeZ);
            _localIDs = new IDs_3D(localSizeX, localSizeY, localSizeZ);
        }

        [KernelCallable]
        public int32 GlobalID0
        {
            get
            {
                return _globalIDs.x;
            }
        }

        [KernelCallable]
        public int32 GlobalID1
        {
            get
            {
                return _globalIDs.y;
            }
        }
        
        [KernelCallable]
        public int32 GlobalID2
        {
            get
            {
                return _globalIDs.z;
            }
        }

        [KernelCallable]
        public int32 LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }

        [KernelCallable]
        public int32 LocalID1
        {
            get
            {
                return _localIDs.y;
            }
        }

        [KernelCallable]
        public int32 LocalID2
        {
            get
            {
                return _localIDs.z;
            }
        }

        int32 Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 3;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_globalIDs.x, (IntPtr)(int)_globalIDs.y, (IntPtr)(int)_globalIDs.z };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)(int)_localIDs.x, (IntPtr)(int)_localIDs.y, (IntPtr)(int)_localIDs.z };
            }
        }
    }
}
