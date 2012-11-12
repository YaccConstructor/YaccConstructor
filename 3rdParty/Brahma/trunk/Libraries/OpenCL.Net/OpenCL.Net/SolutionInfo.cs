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
using System.Reflection;
using System.Runtime.InteropServices;

//General assembly information

[assembly: AssemblyCompany("Ananth B.")]
[assembly: AssemblyProduct("OpenCL.Net: .NET bindings for OpenCL")]
[assembly: AssemblyCopyright("Copyright © Ananth B. 2010")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

//Assembly version information

[assembly: AssemblyVersion("0.1.*")]

//CLS compliance. False. OpenCL needs access to signed and unsigned types.

[assembly: CLSCompliant(false)]

//Visibility to COM. False by default. Turn on specifically for code that needs to be
//visible to COM.

[assembly: ComVisible(false)]