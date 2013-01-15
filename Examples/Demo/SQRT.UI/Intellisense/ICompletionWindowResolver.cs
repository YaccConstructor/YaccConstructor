using ICSharpCode.AvalonEdit.CodeCompletion;

namespace SQRT.UI.Intellisense
{
	public interface ICompletionWindowResolver
	{
		CompletionWindow Resolve();
	}
}