using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Tree;

using YC.SDK;

namespace ReSharperExtension
{
    public interface IReSharperLanguage : CommonInterfaces.IInjectedLanguageModule<ICSharpLiteralExpression, DocumentRange, ITreeNode>
    {
    }
}