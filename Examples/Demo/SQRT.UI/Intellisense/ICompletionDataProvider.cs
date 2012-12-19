using System.Collections.Generic;
using ICSharpCode.AvalonEdit.CodeCompletion;

namespace SQRT.UI.Intellisense
{
	public interface ICompletionDataProvider
	{
		IEnumerable<ICompletionData> GetData(string text, int position, string input, string highlightingName); 
	}
}