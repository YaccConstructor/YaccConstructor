using ICSharpCode.AvalonEdit.CodeCompletion;

namespace DotNetNotepad.UI.Intellisense
{
	public interface ICompletionWindowResolver
	{
		CompletionWindow Resolve();
	}
}