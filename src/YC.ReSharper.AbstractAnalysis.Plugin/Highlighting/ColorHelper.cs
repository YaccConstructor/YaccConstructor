using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Schema;
using Highlighting.Core;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi.Tree;
using QuickGraph.Collections;
using YC.ReSharper.AbstractAnalysis.Plugin.Dynamic;
using MatcherHelper = YC.ReSharper.AbstractAnalysis.Plugin.Dynamic.MatcherHelper;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class ColorHelper
    {
        public static string DefaultColor = HighlightingAttributeIds.UNRESOLVED_ERROR_ATTRIBUTE;

        private static Dictionary<string, string> myTokenToColor;
        public static Dictionary<string, string> TokenToColor
        {
            get { return myTokenToColor; }
        }

        private static Dictionary<string, string> mapping = new Dictionary<string, string>()
        {
            {"ANALYSIS_ERROR_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_ERROR_ERRORSTRIPE},
            {"ANALYSIS_WARNING_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_WARNING_ERRORSTRIPE},
            {"ANALYSIS_SUGGESTION_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_SUGGESTION_ERRORSTRIPE},
            {"CLASS_IS_INHERITED_ATTRIBUTE", HighlightingAttributeIds.CLASS_IS_INHERITED_ATTRIBUTE},
            {"CONSTANT_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE},
            {"DEADCODE_ATTRIBUTE", HighlightingAttributeIds.DEADCODE_ATTRIBUTE},
            {"ERROR_ATTRIBUTE",HighlightingAttributeIds.ERROR_ATTRIBUTE},
            {"EVENT_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.EVENT_IDENTIFIER_ATTRIBUTE},
            {"EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE},
            {"FIELD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.FIELD_IDENTIFIER_ATTRIBUTE},
            {"FORMAT_STRING_ITEM", HighlightingAttributeIds.FORMAT_STRING_ITEM},
            {"HIDES_ATTRIBUTE", HighlightingAttributeIds.HIDES_ATTRIBUTE},
            {"HINT_ATTRIBUTE", HighlightingAttributeIds.HINT_ATTRIBUTE},
            {"IMPLEMENTS_AND_HIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_HIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE},
            {"IMPLEMENTS_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_ATTRIBUTE},
            {"INTERFACE_IS_IMPLEMENTED_ATTRIBUTE", HighlightingAttributeIds.INTERFACE_IS_IMPLEMENTED_ATTRIBUTE},
            {"JAVA_SCRIPT_XML_DOC_TAG", HighlightingAttributeIds.JAVA_SCRIPT_XML_DOC_TAG},
            {"LATE_BOUND_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.LATE_BOUND_IDENTIFIER_ATTRIBUTE},
            {"LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"MATCHED_BRACE", HighlightingAttributeIds.MATCHED_BRACE},
            {"MATCHED_FORMAT_STRING_ITEM", HighlightingAttributeIds.MATCHED_FORMAT_STRING_ITEM},
            {"METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.METHOD_IDENTIFIER_ATTRIBUTE},
            {"MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"NAMESPACE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.NAMESPACE_IDENTIFIER_ATTRIBUTE},
            {"OPERATOR_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.OPERATOR_IDENTIFIER_ATTRIBUTE},
            {"OUTLINE_BRACE", HighlightingAttributeIds.OUTLINE_BRACE},
            {"OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.OVERRIDES_ATTRIBUTE},
            {"PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"PATH_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PATH_IDENTIFIER_ATTRIBUTE},
            {"PUBLIC_DEADCODE_ATTRIBUTE", HighlightingAttributeIds.PUBLIC_DEADCODE_ATTRIBUTE},
            {"RECURSION_ATTRIBUTE", HighlightingAttributeIds.RECURSION_ATTRIBUTE},
            {"TODOITEM_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ATTRIBUTE},
            {"TODOITEM_ERRORSTRIPE_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ERRORSTRIPE_ATTRIBUTE},
            {"SUGGESTION_ATTRIBUTE", HighlightingAttributeIds.SUGGESTION_ATTRIBUTE},
            {"TYPE_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_CLASS_ATTRIBUTE},
            {"TYPE_STATIC_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_STATIC_CLASS_ATTRIBUTE},
            {"TYPE_DELEGATE_ATTRIBUTE", HighlightingAttributeIds.TYPE_DELEGATE_ATTRIBUTE},
            {"TYPE_ENUM_ATTRIBUTE", HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_INTERFACE_ATTRIBUTE", HighlightingAttributeIds.TYPE_INTERFACE_ATTRIBUTE},
            {"TYPE_PARAMETER_ATTRIBUTE", HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_STRUCT_ATTRIBUTE", HighlightingAttributeIds.TYPE_STRUCT_ATTRIBUTE},
            {"UNMATCHED_BRACE", HighlightingAttributeIds.UNMATCHED_BRACE},
            {"UNRESOLVED_ERROR_ATTRIBUTE", HighlightingAttributeIds.UNRESOLVED_ERROR_ATTRIBUTE},
            {"WARNING_ATTRIBUTE", HighlightingAttributeIds.WARNING_ATTRIBUTE},
        };

        public static void ParseFile(string fileName)
        {
            try
            {
                MatcherHelper.ReCreate();
                using (XmlReader reader = new XmlTextReader(new StreamReader(fileName)))
                {
                    reader.MoveToContent();
                    var xmlReader = GetValidatingReader(reader, new XmlSchemaSet());
                    xmlReader.Read();
                    ParseDefinition(xmlReader);
                }
            }
            catch (Exception)
            {
                return;
            }
        }

        private static void ParseDefinition(XmlReader xmlReader)
        {
            myTokenToColor = new Dictionary<string, string>();

            while (xmlReader.Read() && xmlReader.NodeType != XmlNodeType.EndElement)
            {
                if (xmlReader.Name == "Tokens")
                {
                    ParseTokensGroup(xmlReader, xmlReader.GetAttribute("color"));
                }
                else if (xmlReader.Name == "Token")
                {
                    ParseToken(xmlReader, xmlReader.GetAttribute("color"));
                }
                else if (xmlReader.Name == "Matched")
                {
                    ParseMatching(xmlReader);
                }
                else
                {
                    throw new Exception(string.Format("Unexpected element"));
                }
            }
        }

        private static void ParseToken(XmlReader xmlReader, string color)
        {
            xmlReader.Read();
            var content = xmlReader.ReadContentAsString().Trim();

            if (myTokenToColor.ContainsKey(content))
                return;

            if (!string.IsNullOrEmpty(color) && mapping.ContainsKey(color))
                myTokenToColor.Add(content, mapping[color]);
            else
                myTokenToColor.Add(content, DefaultColor);
        }

        private static void ParseTokensGroup(XmlReader xmlReader, string color)
        {
            while (xmlReader.Read() && xmlReader.NodeType != XmlNodeType.EndElement)
            {
                ParseToken(xmlReader, color);
            }
        }

        private static void ParseMatching(XmlReader xmlReader)
        {
            while (xmlReader.Read() && xmlReader.NodeType != XmlNodeType.EndElement)
            {
                xmlReader.Read();

                var left = ParseLeftMatcher(xmlReader);
                var right = ParseRightMatcher(xmlReader);

                MatcherHelper.AddMatch(left, right);
            }
        }

        private static string ParseLeftMatcher(XmlReader xmlReader)
        {
            if (xmlReader.Name == "Left")
            {
                xmlReader.Read();
                string res = xmlReader.ReadContentAsString().Trim();
                xmlReader.Read();
                return res;
            }
            else throw new Exception("Unexpected left element in matching");
        }

        private static string ParseRightMatcher(XmlReader xmlReader)
        {
            if (xmlReader.Name == "Right")
            {
                xmlReader.Read();
                string res = xmlReader.ReadContentAsString().Trim();
                xmlReader.Read();
                return res;
            }
            else throw new Exception("Unexpected right element in matching");
        }

        private static XmlReader GetValidatingReader(XmlReader input, XmlSchemaSet schemaSet)
        {
            var settings = new XmlReaderSettings
            {
                CloseInput = true, 
                IgnoreComments = true, 
                IgnoreWhitespace = true
            };

            if (schemaSet != null)
            {
                settings.Schemas = schemaSet;
                settings.ValidationType = ValidationType.Schema;
            }
            return XmlReader.Create(input, settings);
        }
    }
}
