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

namespace Brahma.Commands
{
    public abstract class Command
    {
        private readonly List<string> _waitList = new List<string>();
        private string _name = string.Empty;
        protected internal List<string> WaitList
        {
            get
            {
                return _waitList;
            }
        }

        public string Name
        {
            get 
            {
                return _name;
            }
            protected set
            {
                _name = value;
            }
        }

        public abstract void Execute(object sender);

        public static Command operator <=(string name, Command command)
        {
            command._name = name;
            return command;
        }

        public static Command operator >=(string name, Command command)
        {
            throw new NotSupportedException();
        }
    }

    public interface ICommand
    {
        
    }

    public interface ICommand<TRange>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2, T3>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2, T3, T4>: ICommand
        where TRange: INDRangeDimension

    {
    }

    public interface ICommand<TRange, T1, T2, T3, T4, T5>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2, T3, T4, T5, T6>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2, T3, T4, T5, T6, T7>: ICommand
        where TRange: INDRangeDimension
    {
    }

    public interface ICommand<TRange, T1, T2, T3, T4, T5, T6, T7, T8>: ICommand
        where TRange: INDRangeDimension
    {
    }
}