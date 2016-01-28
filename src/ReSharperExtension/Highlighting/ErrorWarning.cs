/*
 * Copyright 2007-2011 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;

namespace ReSharperExtension.Highlighting
{
    /// <summary>
    /// The highlighting that warns about high complexity
    /// </summary>
    [StaticSeverityHighlighting(Severity.ERROR, "CSharpInfo")]
    public class ErrorWarning : IHighlighting
    {
        private readonly string myTooltip;
        private DocumentRange range;

        public ErrorWarning(DocumentRange range, string toolTip)
        {
            myTooltip = toolTip;
        }

        #region IHighlighting members
        public string ToolTip
        {
            get { return myTooltip; }
        }

        public string ErrorStripeToolTip
        {
            get { return myTooltip; }
        }

        public int NavigationOffsetPatch
        {
            get { return 0; }
        }

        public bool IsValid()
        {
            return true;
        }

        public DocumentRange CalculateRange()
        {
            return range;
        }
        #endregion
    }
}