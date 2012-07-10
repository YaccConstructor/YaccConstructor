using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio;
using System.Windows;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;//added
using VSYardNS;


namespace VSYard.AutoCompletion
{
    #region Command Filter

    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("yardtype")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    internal sealed class VsTextViewCreationListener : IVsTextViewCreationListener
    {
        [Export]
        [Name("yardtype")]
        [BaseDefinition("plaintext")]
        internal static ContentTypeDefinition YARDContentType = null;

        [Export]
        [FileExtension(".yrd")]
        [ContentType("yardtype")]
        internal static FileExtensionToContentTypeDefinition YARDFileType = null;
        [Import]
        IVsEditorAdaptersFactoryService AdaptersFactory = null;

        [Import]
        ICompletionBroker CompletionBroker = null;
        //added
        //[Import]
        //internal SVsServiceProvider ServiceProvider { get; set; }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            IWpfTextView view = AdaptersFactory.GetWpfTextView(textViewAdapter);
            Debug.Assert(view != null);

            CommandFilter filter = new CommandFilter(view, CompletionBroker);

            IOleCommandTarget next;
            textViewAdapter.AddCommandFilter(filter, out next);
            filter.Next = next;
        }
    }

    internal sealed class CommandFilter : IOleCommandTarget
    {
        ICompletionSession _currentSession;
        bool isSessionRunning = false;

        public CommandFilter(IWpfTextView textView, ICompletionBroker broker)
        {
            _currentSession = null;
             
            TextView = textView;
            Broker = broker;
        }

        public IWpfTextView TextView { get; private set; }
        public ICompletionBroker Broker { get; private set; }
        public IOleCommandTarget Next { get; set; }

        private char GetTypeChar(IntPtr pvaIn)
        {
            return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
        }

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            int hresult = VSConstants.S_OK;
            bool handled = false;


            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)nCmdID)
                {
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                        break;
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD: // Нажатие Ctrl+Space
                        handled = StartSession();
                        break;
                    case VSConstants.VSStd2KCmdID.RETURN: // Нажатие Enter
                        handled = Complete(false);
                        break;
                    case VSConstants.VSStd2KCmdID.TAB:    // Нажатие Tab
                        handled = Complete(true);
                        break;
                    case VSConstants.VSStd2KCmdID.CANCEL: // Нажатие Esc
                        handled = Cancel();
                        break;
                    //default: Cancel(); break;
                }
            }

            if (!handled)
                hresult = Next.Exec(pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);

            if (ErrorHandler.Succeeded(hresult))
            {
                if (pguidCmdGroup == VSConstants.VSStd2K)
                {
                    if (isSessionRunning)
                    {
                        //Cancel();
                        //isSessionRunning = false;
                    }
                    else
                    {
                        StartSession();
                        isSessionRunning = true;
                        if (_currentSession != null)
                            _currentSession.Recalculate();
                    }

                    switch ((VSConstants.VSStd2KCmdID)nCmdID)
                    {
                        case VSConstants.VSStd2KCmdID.TYPECHAR:
                            char ch = GetTypeChar(pvaIn);
                            if (ch == ' ')
                                if (isSessionRunning)
                                {
                                    Cancel();
                                    isSessionRunning = false;
                                }
                                else
                                {
                                    StartSession();
                                    isSessionRunning = true;
                                    if (_currentSession != null)
                                        _currentSession.Recalculate(); 
                                }
                            else if (_currentSession != null)
                                _currentSession.Filter();
                            break;
                        case VSConstants.VSStd2KCmdID.BACKSPACE:
                            if (_currentSession != null)
                            {
                                _currentSession.Filter();
                            }
                            break;
                    } 
                }
            }

            //else handled = StartSession();

            return hresult;
        }

        bool Cancel()
        {
            if (_currentSession == null)
                return false;

            _currentSession.Dismiss();

            return true;
        }

        bool Complete(bool force)
        {
            if (_currentSession == null)
                return false;

            if (!_currentSession.SelectedCompletionSet.SelectionStatus.IsSelected && !force)
            {
                _currentSession.Dismiss();
                return false;
            }
            else
            {
                _currentSession.Commit();
                return true;
            }
        }

        bool StartSession()
        {
            if (_currentSession != null)
                return false;

            SnapshotPoint caret = TextView.Caret.Position.BufferPosition; // Расположение каретки
            ITextSnapshot snapshot = caret.Snapshot;

            if (!Broker.IsCompletionActive(TextView))
            {
                _currentSession = Broker.CreateCompletionSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _currentSession = Broker.GetSessions(TextView)[0];
            }
            _currentSession.Dismissed += (sender, args) => _currentSession = null;

            _currentSession.Start();


            return true;
        }

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                        prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                        return VSConstants.S_OK;
                }
            }
            return Next.QueryStatus(pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }

    }

    #endregion
}