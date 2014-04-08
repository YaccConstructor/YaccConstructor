using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using JetBrains.ReSharper.Daemon;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Inspections
{
    public static class XMLParser
    {
        private static readonly Dictionary<string, string> mapping = new Dictionary<string, string>()
        {
            {"UNRESOLVED_ERROR_ATTRIBUTE", HighlightingAttributeIds.UNRESOLVED_ERROR_ATTRIBUTE},
            {"ERROR_ATTRIBUTE", HighlightingAttributeIds.ERROR_ATTRIBUTE},
            {"WARNING_ATTRIBUTE", HighlightingAttributeIds.WARNING_ATTRIBUTE},
            {"DEADCODE_ATTRIBUTE", HighlightingAttributeIds.DEADCODE_ATTRIBUTE},
            {"JAVA_SCRIPT_XML_DOC_TAG", HighlightingAttributeIds.JAVA_SCRIPT_XML_DOC_TAG},
            {"PUBLIC_DEADCODE_ATTRIBUTE", HighlightingAttributeIds.PUBLIC_DEADCODE_ATTRIBUTE},
            {"SUGGESTION_ATTRIBUTE", HighlightingAttributeIds.SUGGESTION_ATTRIBUTE},
            {"HINT_ATTRIBUTE", HighlightingAttributeIds.HINT_ATTRIBUTE},
            {"CONSTANT_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE},
            {"EVENT_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.EVENT_IDENTIFIER_ATTRIBUTE},
            {"FIELD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.FIELD_IDENTIFIER_ATTRIBUTE},
            {"LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.METHOD_IDENTIFIER_ATTRIBUTE},
            {"EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE},
            {"OPERATOR_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.OPERATOR_IDENTIFIER_ATTRIBUTE},
            {"TYPE_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_CLASS_ATTRIBUTE},
            {"TYPE_STATIC_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_STATIC_CLASS_ATTRIBUTE},
            {"TYPE_INTERFACE_ATTRIBUTE", HighlightingAttributeIds.TYPE_INTERFACE_ATTRIBUTE},
            {"TYPE_DELEGATE_ATTRIBUTE", HighlightingAttributeIds.TYPE_DELEGATE_ATTRIBUTE},
            {"TYPE_STRUCT_ATTRIBUTE", HighlightingAttributeIds.TYPE_STRUCT_ATTRIBUTE},
            {"TYPE_ENUM_ATTRIBUTE", HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_PARAMETER_ATTRIBUTE", HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"NAMESPACE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.NAMESPACE_IDENTIFIER_ATTRIBUTE},
            {"PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"LATE_BOUND_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.LATE_BOUND_IDENTIFIER_ATTRIBUTE},
            {"PATH_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PATH_IDENTIFIER_ATTRIBUTE},
            {"IMPLEMENTS_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_ATTRIBUTE},
            {"OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.OVERRIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE},
            {"HIDES_ATTRIBUTE", HighlightingAttributeIds.HIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_HIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE},
            {"INTERFACE_IS_IMPLEMENTED_ATTRIBUTE", HighlightingAttributeIds.INTERFACE_IS_IMPLEMENTED_ATTRIBUTE},
            {"CLASS_IS_INHERITED_ATTRIBUTE", HighlightingAttributeIds.CLASS_IS_INHERITED_ATTRIBUTE},
            {"RECURSION_ATTRIBUTE", HighlightingAttributeIds.RECURSION_ATTRIBUTE},
            {"TODOITEM_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ATTRIBUTE},
            {"TODOITEM_ERRORSTRIPE_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ERRORSTRIPE_ATTRIBUTE},
            {"FORMAT_STRING_ITEM", HighlightingAttributeIds.FORMAT_STRING_ITEM},
            {"MATCHED_FORMAT_STRING_ITEM", HighlightingAttributeIds.MATCHED_FORMAT_STRING_ITEM},
            {"MATCHED_BRACE", HighlightingAttributeIds.MATCHED_BRACE},
            {"UNMATCHED_BRACE", HighlightingAttributeIds.UNMATCHED_BRACE},
            {"OUTLINE_BRACE", HighlightingAttributeIds.OUTLINE_BRACE},
            {"ANALYSIS_ERROR_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_ERROR_ERRORSTRIPE},
            {"ANALYSIS_WARNING_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_WARNING_ERRORSTRIPE},
            {"ANALYSIS_SUGGESTION_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_SUGGESTION_ERRORSTRIPE}
        };

        public static Dictionary<string, string> ParseFile(string fileName)
        {
            using (XmlReader reader = new XmlTextReader(new StreamReader(fileName)))
            {
                reader.MoveToContent();
                var xmlReader = GetValidatingReader(reader, new XmlSchemaSet());
                xmlReader.Read();
                return ParseDefinition(xmlReader);
            }
        }

        private static Dictionary<string, string> ParseDefinition(XmlReader xmlReader)
        {
            var dict = new Dictionary<string, string>();

            while (xmlReader.Read() && xmlReader.NodeType != XmlNodeType.EndElement)
            {
                if (xmlReader.Name == "Tokens")
                {
                    ParseTokensGroup(xmlReader, dict, xmlReader.GetAttribute("color"));
                }
                else if (xmlReader.Name == "Token")
                {
                    ParseToken(xmlReader, dict, xmlReader.GetAttribute("color"));
                }
                else
                {
                    throw new Exception(string.Format("Unexpected element"));
                }
            }
            return dict;
        }

        private static void ParseToken(XmlReader xmlReader, Dictionary<string, string> dict, string color)
        {
            xmlReader.Read();
            var content = xmlReader.ReadContentAsString();
            if (mapping.ContainsKey(content))
                dict.Add(mapping[content], color);
        }

        private static void ParseTokensGroup(XmlReader xmlReader, Dictionary<string, string> dict, string color)
        {
            while (xmlReader.Read() && xmlReader.NodeType != XmlNodeType.EndElement)
            {
                ParseToken(xmlReader, dict, color);
            }
        }

        private static XmlReader GetValidatingReader(XmlReader input, XmlSchemaSet schemaSet)
        {
            var settings = new XmlReaderSettings {CloseInput = true, IgnoreComments = true, IgnoreWhitespace = true};
            if (schemaSet != null)
            {
                settings.Schemas = schemaSet;
                settings.ValidationType = ValidationType.Schema;
            }
            return XmlReader.Create(input, settings);
        }
    }
}
