using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using GraphX.Controls;
using JetBrains.Application;
using JetBrains.DataFlow;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Features.Altering.Resources;
using JetBrains.UI.Application;
using JetBrains.UI.Controls;
using JetBrains.UI.CrossFramework;
using JetBrains.UI.Extensions;
using JetBrains.UI.RichText;
using JetBrains.UI.ToolWindowManagement;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow
{
    [ToolWindowDescriptor(
      ProductNeutralId = "ClassName",
      Text = "Show graph of code",
      VisibilityPersistenceScope = ToolWindowVisibilityPersistenceScope.Solution,
      Icon = typeof(AlteringFeatuThemedIcons.GeneratedMembers),
      Type = ToolWindowType.MultiInstance,
      InitialDocking = ToolWindowInitialDocking.NotSpecified)]
    public class GraphCodeToolWindow : ToolWindowDescriptor
    {
        public GraphCodeToolWindow(IApplicationDescriptor applicationDescriptor)
            : base(applicationDescriptor)
        {
        }
    }

    public class GraphCodeWindowRegistrar
    {
        private readonly Lifetime _lifetime;
        private readonly ToolWindowClass _toolWindowClass;
        private readonly UIApplication _environment;

        public GraphCodeWindowRegistrar(Lifetime lifetime, ToolWindowManager toolWindowManager,
                                            GraphCodeToolWindow descriptor, UIApplication environment)
        {
            _environment = environment;
            _lifetime = lifetime;
            
            _toolWindowClass = toolWindowManager.Classes[descriptor];
            _toolWindowClass.RegisterEmptyContent(
              lifetime,
              lt =>
              {
                  var graphs = (new GraphLoader()).Load();
                  var tabControl = new System.Windows.Controls.TabControl();
                  var zcontrols = new List<ZoomControl>();
                  foreach (var graph in graphs)
                  {
                      var gArea = InitializeGraphArea.Initialize(graph);
                      var zcontrol = new ZoomControl();
                      zcontrol.Content = gArea;
                      zcontrols.Add(zcontrol);
                  }
                  tabControl.ItemsSource = zcontrols;
                  return (new EitherControl(tabControl)).BindToLifetime(lt);
              });
        }

        public void Show()
        {
            ToolWindowInstance instance = _toolWindowClass.RegisterInstance(
              _lifetime,
              "Graph of code", // title of your window; tip: StringUtil.MakeTitle
              null, // return a System.Drawing.Image to be displayed
              (lt, twi) =>
              {

                  var graphs = (new GraphLoader()).Load();
                  var tabControl = new System.Windows.Controls.TabControl();
                  var zcontrols = new List<ZoomControl>();
                  foreach (var graph in graphs)
                  {
                      var gArea = InitializeGraphArea.Initialize(graph);
                      var zcontrol = new ZoomControl();
                      zcontrol.Content = gArea;
                      zcontrols.Add(zcontrol);
                  }
                  tabControl.ItemsSource = zcontrols;
                  return (new EitherControl(tabControl)).BindToLifetime(lt);
              });
            instance.EnsureControlCreated().Show();
        }
    }
}
