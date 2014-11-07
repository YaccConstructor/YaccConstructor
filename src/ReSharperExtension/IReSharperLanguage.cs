using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Tree;

namespace ReSharperExtension
{
    public interface IReSharperLanguage : YC.SDK.CommonInterfaces.IInjectedLanguageModule<ICSharpLiteralExpression, DocumentRange, ITreeNode>
    {
    }


}
