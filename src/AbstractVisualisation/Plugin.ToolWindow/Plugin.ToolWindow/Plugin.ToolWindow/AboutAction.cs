using System;
using System.Linq;
using GraphX;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.GraphSharp.Algorithms.OverlapRemoval;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;
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
    [ToolWindowDescriptor(
    ProductNeutralId = "ClassName",
    Text = "Show graph of code",
    VisibilityPersistenceScope = ToolWindowVisibilityPersistenceScope.Solution,
    Icon = typeof(AlteringFeatuThemedIcons.GeneratedMembers),
    Type = ToolWindowType.SingleInstance,
    InitialDocking = ToolWindowInitialDocking.NotSpecified)]
    public class GraphCodeToolWindow : ToolWindowDescriptor
    {
        public GraphCodeToolWindow(IApplicationDescriptor applicationDescriptor)
            : base(applicationDescriptor)
        {
        }
    }

    [ActionHandler("Plugin.ToolWindow.About")]
    public class GraphCodeToolWindowAction : IActionHandler
    {
        private GraphCodeToolWindow descriptor;
        private ToolWindowManager manager;
        private Lifetime lifetime;
        private UIApplication uiApplication;
        private IVsUIShell shell;
        private ToolWindowRegistrar registrar;
        private ITextControl textControl;
        public bool Update(IDataContext context, ActionPresentation presentation, DelegateUpdate nextUpdate)
        {
            return true;
        }

        public void Execute(IDataContext context, DelegateExecute nextExecute)
        {
            if (descriptor == null || manager == null || lifetime == null || uiApplication == null || shell == null ||
                registrar == null)
            {
                descriptor = DataConstantsExtensions.GetComponent<GraphCodeToolWindow>(context);
                manager = DataConstantsExtensions.GetComponent<ToolWindowManager>(context);
                lifetime = DataConstantsExtensions.GetComponent<Lifetime>(context);
                uiApplication = DataConstantsExtensions.GetComponent<UIApplication>(context);
                shell = DataConstantsExtensions.GetComponent<IVsUIShell>(context);
                registrar = new ToolWindowRegistrar(lifetime, manager, shell, descriptor, uiApplication);
            }
            registrar.Show();
        }
    }

    public class ToolWindowRegistrar
    {
        private AreaControl arcont;
        private readonly Lifetime lifetime;
        private readonly ToolWindowClass toolWindowClass;
        private readonly UIApplication environment;
        private ToolWindowInstance instance;

        public ToolWindowRegistrar(Lifetime lifetime, ToolWindowManager toolWindowManager, IVsUIShell shell,
            GraphCodeToolWindow descriptor, UIApplication environment)
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
                arcont.Graph_Setup();
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