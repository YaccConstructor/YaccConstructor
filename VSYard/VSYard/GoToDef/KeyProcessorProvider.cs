using System;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Input;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;

namespace YC.VSYard.GoToDef
{
    [Export(typeof(IKeyProcessorProvider))]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    [ContentType("yardtype")]
    [Name("GotoDef")]
    [Order(Before = "VisualStudioKeyboardProcessor")]
    internal sealed class GoToDefKeyProcessorProvider : IKeyProcessorProvider
    {
        [Import]
        internal ITextStructureNavigatorSelectorService TextStructureNavigatorSelector { get; set; }
        public KeyProcessor GetAssociatedProcessor(IWpfTextView view)
        {

            ITextStructureNavigator textStructureNavigator =
                TextStructureNavigatorSelector.GetTextStructureNavigator(view.TextBuffer);

            return view.Properties.GetOrCreateSingletonProperty(typeof(VSYardNS.GoToDefKeyProcessor),
                                                                () => new VSYardNS.GoToDefKeyProcessor(textStructureNavigator, view));
        }
    }

    /// <summary>
    /// The state of the control key for a given view, which is kept up-to-date by a combination of the
    /// key processor and the mouse process
    /// </summary>
    //internal sealed class CtrlKeyState
    //{
    //    internal static CtrlKeyState GetStateForView(ITextView view)
    //    {
    //        return view.Properties.GetOrCreateSingletonProperty(typeof(CtrlKeyState), () => new CtrlKeyState());
    //    }

    //    bool _enabled = false;

    //    internal bool Enabled
    //    {
    //        get
    //        {
    //            // Check and see if ctrl is down but we missed it somehow.
    //            bool F12Down = (Keyboard.Modifiers & ModifierKeys.Control) != 0;
    //            if (F12Down != _enabled)
    //                Enabled = F12Down;

    //            return _enabled;
    //        }
    //        set
    //        {
    //            bool oldVal = _enabled;
    //            _enabled = value;
    //            if (oldVal != _enabled)
    //            {
    //                var temp = CtrlKeyStateChanged;
    //                if (temp != null)
    //                    temp(this, new EventArgs());
    //            }
    //        }
    //    }

    //    internal event EventHandler<EventArgs> CtrlKeyStateChanged;
    //}

    /// <summary>
    /// Listen for the control key being pressed or released to update the CtrlKeyStateChanged for a view.
    /// </summary>
    //internal sealed class GoToDefKeyProcessor : KeyProcessor
    //{
    //    CtrlKeyState _state;

    //    public GoToDefKeyProcessor(CtrlKeyState state)
    //    {
    //        _state = state;
    //    }

    //    void UpdateState(KeyEventArgs args)
    //    {
    //        _state.Enabled = (args.KeyboardDevice.Modifiers & ModifierKeys.Control) != 0;
    //    }

    //    public override void PreviewKeyDown(KeyEventArgs args)
    //    {
    //        UpdateState(args);
    //    }

    //    public override void PreviewKeyUp(KeyEventArgs args)
    //    {
    //        UpdateState(args);
    //    }
    //}
}
