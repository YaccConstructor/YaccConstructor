using System;
using System.Linq;
using GraphX;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.GraphSharp.Algorithms.OverlapRemoval;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;
using JetBrains.DocumentModel;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.UI.Application;
using JetBrains.UI.CrossFramework;
using Plugin.ToolWindow;
using MessageBox1 = JetBrains.Util.MessageBox;

using System.Windows.Forms;
using System.Drawing;
using JetBrains.TextControl;
using JetBrains.Application;
using JetBrains.DataFlow;
using JetBrains.ReSharper.Features.Altering.Resources;
using JetBrains.UI.Controls;
using JetBrains.UI.Extensions;
using JetBrains.UI.RichText;
using JetBrains.UI.ToolWindowManagement;
using Microsoft.VisualStudio.Shell.Interop;

namespace Plugin.ToolWindow
{
    #region Descriptor
    [ToolWindowDescriptor(
    ProductNeutralId = "ClassName",
    Text = "Graph of code",
    VisibilityPersistenceScope = ToolWindowVisibilityPersistenceScope.Solution,
    Icon = typeof(AlteringFeatuThemedIcons.GeneratedMembers),
    Type = ToolWindowType.SingleInstance,
    InitialDocking = ToolWindowInitialDocking.NotSpecified)]
    public class WindowDescriptor : ToolWindowDescriptor
    {
        public WindowDescriptor(IApplicationDescriptor applicationDescriptor)
            : base(applicationDescriptor)
        {
        }
    }
    #endregion
    [ActionHandler("Plugin.ToolWindow.About")]
    public class WindowAction : IActionHandler
    {
        private WindowDescriptor descriptor;
        private ToolWindowManager manager;
        private Lifetime lifetime;
        private UIApplication uiApplication;
        private IVsUIShell shell;
        private WindowRegistrar registrar;
        private ISolution solution;
        private ITextControl codefile;
        public IDocument document;
        public static ITextControl textControl;
        public static IDeclaredElement element;
        
        public bool Update(IDataContext context, ActionPresentation presentation, DelegateUpdate nextUpdate)
        {
            return true;
        }
        public static void GoToCode(ITextControl t, Edge e)
        {
            var p = t.Caret.PositionValue;
            t.Caret.MoveTo(new DocOffsetAndVirtual(736), new CaretVisualPlacement());
        }

        public static void GoToGraph(object sender, EventArgs args)
        {
            MessageBox.Show("asdf");
        }

        public void Execute(IDataContext context, DelegateExecute nextExecute)
        {
            //TODO same method
            try
            {
                solution = DataConstantsExtensions.GetComponent<ISolution>(context);
                textControl = context.GetData(JetBrains.TextControl.DataContext.DataConstants.TEXT_CONTROL);
                //TODO mere beayty analiser
                //TextControl.Caret.CaretMoved += GoToGraph;
                if (descriptor == null || manager == null || lifetime == null || uiApplication == null || shell == null ||
                registrar == null)
                {
                    descriptor = DataConstantsExtensions.GetComponent<WindowDescriptor>(context);
                    manager = DataConstantsExtensions.GetComponent<ToolWindowManager>(context);
                    lifetime = DataConstantsExtensions.GetComponent<Lifetime>(context);
                    uiApplication = DataConstantsExtensions.GetComponent<UIApplication>(context);
                    shell = DataConstantsExtensions.GetComponent<IVsUIShell>(context);
                    registrar = new WindowRegistrar(lifetime, manager, shell, descriptor, uiApplication);
                }

                registrar.Show();
            }
            catch (Exception)
            {
                MessageBox.Show("Please, open project");
            }
        }
    }

    public class WindowRegistrar
    {
        private AreaControl arcont;
        private readonly Lifetime lifetime;
        private readonly ToolWindowClass toolWindowClass;
        private readonly UIApplication environment;
        private ToolWindowInstance instance;

        public WindowRegistrar(Lifetime lifetime, ToolWindowManager toolWindowManager, IVsUIShell shell,
            WindowDescriptor descriptor, UIApplication environment)
        {
            this.environment = environment;
            this.lifetime = lifetime;
            this.toolWindowClass = toolWindowManager.Classes[descriptor];
            instance = this.toolWindowClass.RegisterInstance(
            this.lifetime,
            "", // title of your window; tip: StringUtil.MakeTitle
            null, // return a System.Drawing.Image to be displayed
            (lt, twi) =>
            {
                arcont = new AreaControl();
                arcont.GraphArea_Setup();
                //arcont.Graph_Setup();
                arcont.Loaded += arcont.MainWindow_Loaded;
                return arcont;
            });
            instance.EnsureControlCreated().Show();
        }

        public void Show()
        {
            instance.EnsureControlCreated().Show();
        }
    }
}