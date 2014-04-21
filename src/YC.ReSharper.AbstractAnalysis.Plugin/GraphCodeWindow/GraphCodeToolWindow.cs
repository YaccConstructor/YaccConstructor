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

    [SolutionComponent]
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
                  var graph = (new GraphLoader()).Load();
                  var gArea = InitializeGraphArea.Initialize(graph);
                  var zcontrol = new ZoomControl();
                  zcontrol.Content = gArea;
                  var control = new EitherControl(zcontrol);
                  return control.BindToLifetime(lt);
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
                  var graph = (new GraphLoader()).Load();
                  var gArea = InitializeGraphArea.Initialize(graph);
                  var zcontrol = new ZoomControl();
                  zcontrol.Content = gArea;
                  var control = new EitherControl(zcontrol);
                  return control.BindToLifetime(lt);
              });
            instance.EnsureControlCreated().Show();
        }
    }
}
