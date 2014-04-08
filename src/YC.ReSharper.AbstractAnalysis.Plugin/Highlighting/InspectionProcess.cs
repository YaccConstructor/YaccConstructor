using System;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class InspectionsProcess : MyDaemonStageProcessBase
    {
        //private readonly IDictionary<string, List<IDeclaration>> myDeclarations;

        public InspectionsProcess(IDaemonProcess process, IContextBoundSettingsStore settings)
            : base(process, settings)
        {
            process.SourceFile.PrimaryPsiLanguage.Is<KnownLanguage>();
            process.GetStageProcess<MyFileIndexProcess>();

            //myDeclarations = new Dictionary<string, List<IDeclaration>>();
            //VisitFile(process.SourceFile.GetPsiFile<KnownLanguage>(new DocumentRange(process.SourceFile.Document, 0)) as IFile);
        }

        public override void Execute(Action<DaemonStageResult> commiter)
        {
            HighlightInFile((file, consumer) => file.ProcessDescendants(this, consumer), commiter);
        }

        #region commented code
        //public override void VisitRuleDeclaredName(IRuleDeclaredName ruleDeclaredName, IHighlightingConsumer consumer)
        //{
        //    //var name = ruleDeclaredName.GetText();
        //    //if (myDeclarations.ContainsKey(name))
        //    //{
        //    //    List<IDeclaration> list = myDeclarations.GetValue(name);
        //    //    if (list.Count > 1)
        //    //    {
        //    //        consumer.AddHighlighting(new DuplicatingLocalDeclarationError(ruleDeclaredName), File);
        //    //    }
        //    //}
        //    base.VisitRuleDeclaredName(ruleDeclaredName, consumer);
        //}

        //public override void VisitRuleDeclaration(IRuleDeclaration ruleDeclaration, IHighlightingConsumer consumer)
        //{
        //    IRuleBody body = ruleDeclaration.Body;
        //    ITreeNode child = MyTreeUtil.GetFirstChild<IRuleName>(body);
        //    var ruleName = child as IRuleName;
        //    if (ruleName != null)
        //    {
        //        if (ruleName.GetText().Equals(ruleDeclaration.DeclaredName))
        //        {
        //            if (!IsCustomImpl(ruleDeclaration))
        //            {
        //                consumer.AddHighlighting(new LeftRecursionWarning(ruleName), File);
        //            }
        //        }
        //    }
        //    base.VisitRuleDeclaration(ruleDeclaration, consumer);
        //}

        //private bool IsCustomImpl(IRuleDeclaration ruleDeclaration)
        //{
        //    var options = ruleDeclaration.Options;
        //    if (options == null)
        //    {
        //        return false;
        //    }
        //    var child = options.FirstChild;
        //    while (child != null)
        //    {
        //        var optionDefinition = child as IOptionDefinition;
        //        if (optionDefinition != null)
        //        {
        //            if (optionDefinition.OptionName.GetText() == "customParseFunction")
        //            {
        //                return true;
        //            }
        //        }
        //        child = child.NextSibling;
        //    }
        //    return false;
        //}

        //public override void VisitPsiExpression(IPsiExpression psiExpression, IHighlightingConsumer consumer)
        //{
        //    ITreeNode child = psiExpression.FirstChild;
        //    IList<ISequence> list = new List<ISequence>();
        //    while (child != null)
        //    {
        //        if (child is ISequence)
        //        {
        //            list.Add(child as ISequence);
        //        }
        //        if (child is IChoiceTail)
        //        {
        //            list.Add((child as IChoiceTail).Sequence);
        //        }
        //        child = child.NextSibling;
        //    }

        //    if (list.Count > 1)
        //    {
        //        ISequence[] sequences = list.ToArray();
        //        var isRepeated = new bool[sequences.Count()];
        //        for (int i = 0; i < sequences.Count() - 1; ++i)
        //        {
        //            if (!isRepeated[i])
        //            {
        //                ISequence sequence1 = sequences[i];
        //                for (int j = i + 1; j < sequences.Count(); ++j)
        //                {
        //                    ISequence sequence2 = sequences[j];
        //                    if (PsiTreeUtil.EqualsElements(sequence1, sequence2))
        //                    {
        //                        if (!isRepeated[i])
        //                        {
        //                            consumer.AddHighlighting(new RepeatedChoiceWarning(sequence1), File);
        //                            isRepeated[i] = true;
        //                        }
        //                        consumer.AddHighlighting(new RepeatedChoiceWarning(sequence2), File);
        //                        isRepeated[j] = true;
        //                    }
        //                }
        //            }
        //        }
        //    }
        //    base.VisitPsiExpression(psiExpression, consumer);
        //}

        //private void VisitFile(IFile element)
        //{
        //    ITreeNode child = element.FirstChild;
        //    while (child != null)
        //    {
        //        if (child is IRuleDeclaration)
        //        {
        //            var declaration = child as IRuleDeclaration;
        //            {
        //                string name = declaration.DeclaredName;
        //                if (myDeclarations.ContainsKey(name))
        //                {
        //                    List<IDeclaration> list = myDeclarations.GetValue(name);
        //                    list.Add(declaration);
        //                }
        //                else
        //                {
        //                    var list = new List<IDeclaration> { declaration };
        //                    myDeclarations.Add(name, list);
        //                }
        //            }
        //        }
        //        child = child.NextSibling;
        //    }

        //    child = element.Interfaces;
        //    if (child != null)
        //    {
        //        child = child.FirstChild;
        //        while (child != null)
        //        {
        //            var declaration = child as IRuleDeclaration;
        //            if (declaration != null)
        //            {
        //                string name = declaration.DeclaredName;
        //                if (myDeclarations.ContainsKey(name))
        //                {
        //                    List<IDeclaration> list = myDeclarations.GetValue(name);
        //                    list.Add(declaration);
        //                }
        //                else
        //                {
        //                    var list = new List<IDeclaration> { declaration };
        //                    myDeclarations.Add(name, list);
        //                }
        //            }
        //            child = child.NextSibling;
        //        }
        //    }
        //}
        #endregion
    }
}
