using System;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using AvalonDock;
using Microsoft.Win32;

namespace DotNetNotepad.UI
{
	public partial class MainWindow
	{
		public MainWindow()
		{
			InitializeComponent();

			var args = Environment.GetCommandLineArgs();
			var created = false;
			foreach (var s in args.Skip(1))
			{
				if (File.Exists(s))
				{
					NewDocument(s);
					created = true;
				}
			}
			if (!created)
			NewDocument();
		}

		private void DockManagerDocumentClosing(object sender, System.ComponentModel.CancelEventArgs e)
		{
			if (MessageBox.Show("Close this tab?", ".Net Notepad", MessageBoxButton.YesNo) == MessageBoxResult.No)
			{
				e.Cancel = true;
			}
		}


		private void ExitClick(object sender, RoutedEventArgs e)
		{
			Application.Current.Shutdown();
		}

		private void OpenFileClick(object sender, RoutedEventArgs e)
		{
			var dlg = new OpenFileDialog {Filter = "All files (*.*)|*.*"};

			if (dlg.ShowDialog().GetValueOrDefault())
			{
				NewDocument(dlg.FileName);
			}
		}

		private void NewClick(object sender, RoutedEventArgs e)
		{
			NewDocument();
		} 


		private void NewDocument(string filename)
		{
			var doc = new Document(filename);
			doc.Show(DockManager);
			doc.Activate();
		}

		private void NewDocument()
		{
			string title = "New1";
			int i = 2;
			while (DockManager.Documents.Any(d => d.Title == title))
			{
				title = "New" + i;
				i++;
			}

			var doc = new Document { Title = title };
			doc.Show(DockManager);
			doc.Activate();
		}

		private void WindowDrop(object sender, DragEventArgs e)
		{
			if (e.Data is DataObject && ((DataObject)e.Data).ContainsFileDropList())
			{
				foreach (string filePath in ((DataObject) e.Data).GetFileDropList())
				{
					if (File.Exists(filePath))
						NewDocument(filePath);
				}
			}
		}

		#region Themes

		private void SetDefaultTheme(object sender, RoutedEventArgs e)
		{
			ThemeFactory.ResetTheme();
		}

		private void ChangeCustomTheme(object sender, RoutedEventArgs e)
		{
			var uri = (string)((MenuItem)sender).Tag;
			ThemeFactory.ChangeTheme(new Uri(uri, UriKind.RelativeOrAbsolute));
		}

		private void ChangeStandardTheme(object sender, RoutedEventArgs e)
		{
			var name = (string)((MenuItem)sender).Tag;
			ThemeFactory.ChangeTheme(name);
		}

		#endregion
	}
}
