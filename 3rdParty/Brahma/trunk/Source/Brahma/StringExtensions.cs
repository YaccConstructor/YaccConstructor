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

namespace Brahma
{
    public static class StringExtensions
    {
        private static readonly char[] InvalidCharacters = new[] 
        { '<', '>', '`', '.' };
        private const char PlaceHolderCharacter = '_';
        
        public static string MakeValidIdentifier(this string name)
        {
            return new string((from c in name
                   select InvalidCharacters.Contains(c) ? PlaceHolderCharacter : c).ToArray());
        }

        public static string Join(this IEnumerable<string> strings, string separator, bool noTrailingSeparator = true)
        {
            int count = strings.Count();
            if (count == 0)
                return noTrailingSeparator ? string.Empty : string.Empty + separator;
            string result = string.Empty;
            int index = 0;
            foreach (string s in strings)
            {
                result += s;
                result += (index == count - 1)
                    ?
                        noTrailingSeparator ? string.Empty : separator 
                    : separator;
                index++;
            }
            return result;
        }
    }
}
