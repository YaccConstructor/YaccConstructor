using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Forms;
using System.Windows.Input;
using SQRT.UI.Intellisense;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Folding;
using ICSharpCode.AvalonEdit.Highlighting;

namespace SQRT.UI
{
	/// <summary>
	/// Interaction logic for Document.xaml
	/// </summary>
	public partial class Document
	{

        public string FileName
        {
            get;
            set;
        }

		public Document(string fileName)
		{
			InitializeComponent();

            FileName = fileName;
			
			Title = Path.GetFileName(fileName);
			
			var content = File.ReadAllText(fileName);
			TextContent = content;

			var ext = Path.GetExtension(fileName);
			if (ext != null)
			{
				var def = HighlightingManager.Instance.GetDefinitionByExtension(ext);
				if (def != null)
				{
					textEditor.SyntaxHighlighting = def;
				}
			}
			Init();
		}

		public Document()
		{
			InitializeComponent();
			Init();
		}
		

		void Init()
		{
			DataContext = this;
			textEditor.TextArea.TextEntering += TextEditorTextAreaTextEntering;
			textEditor.TextArea.TextEntered += TextEditorTextAreaTextEntered;
		}

		#region TextContent

		/// <summary>
		/// TextContent Dependency Property
		/// </summary>
		public static readonly DependencyProperty TextContentProperty =
			DependencyProperty.Register("TextContent", typeof(string), typeof(Document),
				new FrameworkPropertyMetadata(string.Empty,
					OnTextContentChanged));

		/// <summary>
		/// Gets or sets the TextContent property.  This dependency property 
		/// indicates document text.
		/// </summary>
		public string TextContent
		{
			get { return textEditor.Text; }
			set { textEditor.Text = value; }
		}

		/// <summary>
		/// Handles changes to the TextContent property.
		/// </summary>
		private static void OnTextContentChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
		{
			((Document)d).OnTextContentChanged(e);
		}

		/// <summary>
		/// Provides derived classes an opportunity to handle changes to the TextContent property.
		/// </summary>
		protected virtual void OnTextContentChanged(DependencyPropertyChangedEventArgs e)
		{
			if (TextContentChanged != null)
				TextContentChanged(this, EventArgs.Empty);
		}

		/// <summary>
		/// event raised when text changes
		/// </summary>
		public event EventHandler TextContentChanged;
		#endregion

		private void SaveFile(object sender, RoutedEventArgs e)
		{
			var sfd = new SaveFileDialog { Filter = "All files (*.*)|*.*" };
			var result = sfd.ShowDialog();
			if (result == DialogResult.OK)
			{
				File.WriteAllText(sfd.FileName, textEditor.Text, Encoding.Unicode);
			}
		}

		/// <summary>
		/// Позволяет устанавливать стратегии разбиения текста на части
		/// </summary>
		FoldingManager _foldingManager;
		/// <summary>
		/// Сама стратегия разбиения
		/// </summary>
		AbstractFoldingStrategy _foldingStrategy;

		/// <summary>
		/// Обработчик меняет стратегию разбиения текста на части. Подсветка синтаксиса меняется автоматом.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		void HighlightingComboBoxSelectionChanged(object sender, SelectionChangedEventArgs e)
		{
			// SyntaxHighlighting - это свойство, определяющее текущее правило подсветки синтаксиса
			if (textEditor.SyntaxHighlighting == null)
			{
				_foldingStrategy = null;
			}
			else
			{
				switch (textEditor.SyntaxHighlighting.Name)
				{
					case "XML":
						_foldingStrategy = new XmlFoldingStrategy();
						textEditor.TextArea.IndentationStrategy = new ICSharpCode.AvalonEdit.Indentation.DefaultIndentationStrategy();
						break;
					case "C#":
					case "C++":
					case "PHP":
					case "Java":
						textEditor.TextArea.IndentationStrategy = new ICSharpCode.AvalonEdit.Indentation.CSharp.CSharpIndentationStrategy(textEditor.Options);
						_foldingStrategy = null;
						break;
					default:
						textEditor.TextArea.IndentationStrategy = new ICSharpCode.AvalonEdit.Indentation.DefaultIndentationStrategy();
						_foldingStrategy = null;
						break;
				}
			}
			if (_foldingStrategy != null)
			{
				if (_foldingManager == null)
					_foldingManager = FoldingManager.Install(textEditor.TextArea);
				_foldingStrategy.UpdateFoldings(_foldingManager, textEditor.Document);
			}
			else
			{
				if (_foldingManager != null)
				{
					FoldingManager.Uninstall(_foldingManager);
					_foldingManager = null;
				}
			}
		}


		private CompletionWindow _completionWindow;
		void TextEditorTextAreaTextEntered(object sender, TextCompositionEventArgs e)
		{
			ICompletionWindowResolver resolver = new CompletionWindowResolver(textEditor.Text, textEditor.CaretOffset, e.Text, textEditor);
			_completionWindow = resolver.Resolve();
		}

		void TextEditorTextAreaTextEntering(object sender, TextCompositionEventArgs e)
		{
			if (e.Text.Length > 0 && _completionWindow != null)
			{
				if (!char.IsLetterOrDigit(e.Text[0]))
				{
					_completionWindow.CompletionList.RequestInsertion(e);
				}
			}
		}

	}

	
}
