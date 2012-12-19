using System;
using System.Windows.Media;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Editing;

namespace SQRT.UI.Intellisense
{
	public class TextCompletionData : ICompletionData
	{
		private readonly double _priority;
		private readonly string _header;
		private readonly string _text;

		public TextCompletionData(string header, string text, double priority = 0)
		{
			_header = header;
			_text = text;
			_priority = priority;
		}

		public ImageSource Image
		{
			get { return null; }
		}

		public string Text
		{
			get { return _text; }
		}

		public object Content
		{
			get { return _header; }
		}

		public object Description
		{
			get { return Text; }
		}

		public double Priority
		{
			get { return _priority; }
		}

		public void Complete(TextArea textArea, ISegment completionSegment, EventArgs insertionRequestEventArgs)
		{
			textArea.Document.Replace(completionSegment, Text);
		}
	}
}