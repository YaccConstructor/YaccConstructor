//using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.ComponentModel.Composition;
//using Microsoft.VisualStudio.Text;
//using Microsoft.VisualStudio.Text.Classification;
//using Microsoft.VisualStudio.Text.Editor;
//using Microsoft.VisualStudio.Text.Tagging;
//using Microsoft.VisualStudio.Utilities;

////It is simple tags 
//namespace MyCompany.VSYard.Classifiers
//{

//    [Export(typeof(ITaggerProvider))]
//    [ContentType("yard")]
//    [TagType(typeof(VSYardNS.TokenTag))]
//    internal sealed class OokTokenTagProvider : ITaggerProvider
//    {

//        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
//        {
//            return null;//new VSYardNS.TokenTagger(buffer) as ITagger<T>;
//        }
//    }
//}
