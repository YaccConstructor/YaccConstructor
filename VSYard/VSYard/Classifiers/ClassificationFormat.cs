using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace OokLanguage
{
    #region Format definition
    /// <summary>
    /// Defines an editor format for the OrdinaryClassification type that has a purple background
    /// and is underlined.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "yterminal")]
    [Name("yterminal")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class YardTerm : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public YardTerm()
        {
            this.DisplayName = "yterminal"; //human readable version of the name
            this.ForegroundColor = Colors.BlueViolet;
        }
    }

    /// <summary>
    /// Defines an editor format for the OrdinaryClassification type that has a purple background
    /// and is underlined.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "ycomment")]
    [Name("ycomment")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class YardComment : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public YardComment()
        {
            this.DisplayName = "ycomment"; //human readable version of the name
            this.ForegroundColor = Colors.Green;
        }
    }

    /// <summary>
    /// Defines an editor format for the OrdinaryClassification type that has a purple background
    /// and is underlined.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "yliteral")]
    [Name("yliteral")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class YardLiteral : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public YardLiteral()
        {
            this.DisplayName = "yliteral"; //human readable version of the name
            this.ForegroundColor = Colors.Orange;
        }
    }

    /// <summary>
    /// Defines an editor format for the OrdinaryClassification type that has a purple background
    /// and is underlined.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "ynterm")]
    [Name("ynterm")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class YardNterm : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public YardNterm()
        {
            this.DisplayName = "ynterm"; //human readable version of the name
            this.ForegroundColor = Colors.Blue;            
        }
    }

    /// <summary>
    /// Defines an editor format for the OrdinaryClassification type that has a purple background
    /// and is underlined.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "yother")]
    [Name("yother")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class YardOther : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public YardOther()
        {
            this.DisplayName = "yother"; //human readable version of the name
            this.ForegroundColor = Colors.Black;
        }
    }
    #endregion //Format definition
}
