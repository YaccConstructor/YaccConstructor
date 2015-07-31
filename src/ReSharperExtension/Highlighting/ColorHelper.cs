using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;

using JetBrains.ReSharper.Feature.Services.Daemon;

namespace ReSharperExtension.Highlighting
{
    public class ColorHelper
    {
        public static string DefaultColor = HighlightingAttributeIds.UNRESOLVED_ERROR_ATTRIBUTE;

        private static List<string> parsedFiles = new List<string>();

        #region mapping dictionary
        private static Dictionary<string, string> mapping = new Dictionary<string, string>()
        {
            {"ANALYSIS_ERROR_ERRORSTRIPE",                  HighlightingAttributeIds.ANALYSIS_ERROR_ERRORSTRIPE},
            {"ANALYSIS_SUGGESTION_ERRORSTRIPE",             HighlightingAttributeIds.ANALYSIS_SUGGESTION_ERRORSTRIPE},
            {"ANALYSIS_WARNING_ERRORSTRIPE",                HighlightingAttributeIds.ANALYSIS_WARNING_ERRORSTRIPE},
            {"CLASS_IS_INHERITED_ATTRIBUTE",                HighlightingAttributeIds.CLASS_IS_INHERITED_ATTRIBUTE},
            {"CONSTANT_IDENTIFIER_ATTRIBUTE",               HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE},
            {"DEADCODE_ATTRIBUTE",                          HighlightingAttributeIds.DEADCODE_ATTRIBUTE},
            {"ERROR_ATTRIBUTE",                             HighlightingAttributeIds.ERROR_ATTRIBUTE},
            {"EVENT_IDENTIFIER_ATTRIBUTE",                  HighlightingAttributeIds.EVENT_IDENTIFIER_ATTRIBUTE},
            {"EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE",       HighlightingAttributeIds.EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE},
            {"FIELD_IDENTIFIER_ATTRIBUTE",                  HighlightingAttributeIds.FIELD_IDENTIFIER_ATTRIBUTE},
            {"FORMAT_STRING_ITEM",                          HighlightingAttributeIds.FORMAT_STRING_ITEM},
            {"HIDES_ATTRIBUTE",                             HighlightingAttributeIds.HIDES_ATTRIBUTE},
            {"HINT_ATTRIBUTE",                              HighlightingAttributeIds.HINT_ATTRIBUTE},
            {"IMPLEMENTS_AND_HIDES_ATTRIBUTE",              HighlightingAttributeIds.IMPLEMENTS_AND_HIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE",          HighlightingAttributeIds.IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE},
            {"IMPLEMENTS_ATTRIBUTE",                        HighlightingAttributeIds.IMPLEMENTS_ATTRIBUTE},
            {"INTERFACE_IS_IMPLEMENTED_ATTRIBUTE",          HighlightingAttributeIds.INTERFACE_IS_IMPLEMENTED_ATTRIBUTE},
            {"JAVA_SCRIPT_XML_DOC_TAG",                     HighlightingAttributeIds.JAVA_SCRIPT_XML_DOC_TAG},
            {"JS_FUNCTION_IDENTIFIER_ATTRIBUTE",            HighlightingAttributeIds.JS_FUNCTION_IDENTIFIER_ATTRIBUTE},
            {"JS_LATEBOUND_IDENTIFIER_ATTRIBUTE",           HighlightingAttributeIds.JS_LATEBOUND_IDENTIFIER_ATTRIBUTE},
            {"JS_LOCAL_IDENTIFIER_ATTRIBUTE",               HighlightingAttributeIds.JS_LOCAL_IDENTIFIER_ATTRIBUTE},
            {"JS_PARAMETER_IDENTIFIER_ATTRIBUTE",           HighlightingAttributeIds.JS_PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"JS_PROPERTY_IDENTIFIER_ATTRIBUTE",            HighlightingAttributeIds.JS_PROPERTY_IDENTIFIER_ATTRIBUTE},
            {"LATE_BOUND_IDENTIFIER_ATTRIBUTE",             HighlightingAttributeIds.LATE_BOUND_IDENTIFIER_ATTRIBUTE},
            {"LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE",         HighlightingAttributeIds.LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"MATCHED_BRACE",                               HighlightingAttributeIds.MATCHED_BRACE},
            {"MATCHED_FORMAT_STRING_ITEM",                  HighlightingAttributeIds.MATCHED_FORMAT_STRING_ITEM},
            {"METHOD_IDENTIFIER_ATTRIBUTE",                 HighlightingAttributeIds.METHOD_IDENTIFIER_ATTRIBUTE},
            {"MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE},
            {"NAMESPACE_IDENTIFIER_ATTRIBUTE",              HighlightingAttributeIds.NAMESPACE_IDENTIFIER_ATTRIBUTE},
            {"OPERATOR_IDENTIFIER_ATTRIBUTE",               HighlightingAttributeIds.OPERATOR_IDENTIFIER_ATTRIBUTE},
            {"OUTLINE_BRACE",                               HighlightingAttributeIds.OUTLINE_BRACE},
            {"OVERRIDES_ATTRIBUTE",                         HighlightingAttributeIds.OVERRIDES_ATTRIBUTE},
            {"PARAMETER_IDENTIFIER_ATTRIBUTE",              HighlightingAttributeIds.PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"PATH_IDENTIFIER_ATTRIBUTE",                   HighlightingAttributeIds.PATH_IDENTIFIER_ATTRIBUTE},
            {"PUBLIC_DEADCODE_ATTRIBUTE",                   HighlightingAttributeIds.PUBLIC_DEADCODE_ATTRIBUTE},
            {"RECURSION_ATTRIBUTE",                         HighlightingAttributeIds.RECURSION_ATTRIBUTE},
            {"TODOITEM_ATTRIBUTE",                          HighlightingAttributeIds.TODOITEM_ATTRIBUTE},
            {"TODOITEM_ERRORSTRIPE_ATTRIBUTE",              HighlightingAttributeIds.TODOITEM_ERRORSTRIPE_ATTRIBUTE},
            {"SUGGESTION_ATTRIBUTE",                        HighlightingAttributeIds.SUGGESTION_ATTRIBUTE},
            {"TS_CLASS_IDENTIFIER_ATTRIBUTE",               HighlightingAttributeIds.TS_CLASS_IDENTIFIER_ATTRIBUTE},
            {"TS_ENUM_IDENTIFIER_ATTRIBUTE",                HighlightingAttributeIds.TS_ENUM_IDENTIFIER_ATTRIBUTE},
            {"TS_INTERFACE_IDENTIFIER_ATTRIBUTE",           HighlightingAttributeIds.TS_INTERFACE_IDENTIFIER_ATTRIBUTE},
            {"TS_MODULE_IDENTIFIER_ATTRIBUTE",              HighlightingAttributeIds.TS_MODULE_IDENTIFIER_ATTRIBUTE},
            {"TS_TYPE_PARAMETER_IDENTIFIER_ATTRIBUTE",      HighlightingAttributeIds.TS_TYPE_PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"TYPE_CLASS_ATTRIBUTE",                        HighlightingAttributeIds.TYPE_CLASS_ATTRIBUTE},
            {"TYPE_STATIC_CLASS_ATTRIBUTE",                 HighlightingAttributeIds.TYPE_STATIC_CLASS_ATTRIBUTE},
            {"TYPE_DELEGATE_ATTRIBUTE",                     HighlightingAttributeIds.TYPE_DELEGATE_ATTRIBUTE},
            {"TYPE_ENUM_ATTRIBUTE",                         HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_INTERFACE_ATTRIBUTE",                    HighlightingAttributeIds.TYPE_INTERFACE_ATTRIBUTE},
            {"TYPE_PARAMETER_ATTRIBUTE",                    HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_STRUCT_ATTRIBUTE",                       HighlightingAttributeIds.TYPE_STRUCT_ATTRIBUTE},
            {"UNMATCHED_BRACE",                             HighlightingAttributeIds.UNMATCHED_BRACE},
            {"UNRESOLVED_ERROR_ATTRIBUTE",                  HighlightingAttributeIds.UNRESOLVED_ERROR_ATTRIBUTE},
            {"WARNING_ATTRIBUTE",                           HighlightingAttributeIds.WARNING_ATTRIBUTE},
        };
        #endregion

        public static void ParseFile(string fileName, string lang)
        {
            if (parsedFiles.Contains(fileName))
                return;

            string path = GetFullPath(fileName);

            if (String.IsNullOrEmpty(path))
                throw new Exception(String.Format("File {0} doesn't exist", fileName));

            var xmlDocument = new XmlDocument();
            xmlDocument.Load(fileName);

            Dictionary<string, TokenInfo> res = ParseDefinition(xmlDocument.DocumentElement);

            parsedFiles.Add(fileName);
            LanguageHelper.Update(lang, res);

        }

        private static string GetFullPath(string fileName)
        {
            var res = Directory.GetFiles(@"..\..\..", fileName, SearchOption.AllDirectories);
            if (res.Length > 0)
                return res[0];

            return string.Empty;
        }

        private static Dictionary<string, TokenInfo> ParseDefinition(XmlNode root)
        {
            XmlNode currentNode = root.FirstChild;
            Dictionary<string, TokenInfo> tokenToColor = ParseColors(currentNode);

            currentNode = currentNode.NextSibling;
            if (currentNode != null)
            {
                ParseParenthesis(currentNode, tokenToColor);
            }
            return tokenToColor;
        }

        private static void ParseParenthesis(XmlNode element, Dictionary<string, TokenInfo> tokenToColor)
        {
            foreach (XmlNode item in element)
            {
                if (item.Name.ToLowerInvariant() != "pair")
                    continue;

                XmlNode first = item.FirstChild;

                if (first.Name.ToLowerInvariant() != "left")
                    continue;

                string left = first.InnerText.Trim().ToLowerInvariant();

                XmlNode snd = first.NextSibling;
                if (snd.Name.ToLowerInvariant() != "right")
                    continue;

                string right = snd.InnerText.Trim().ToLowerInvariant();

                tokenToColor[left].RightPair = right;
                tokenToColor[right].LeftPair = left;
            }
        }

        private static Dictionary<string, TokenInfo> ParseColors(XmlNode element)
        {
            var dict = new Dictionary<string, TokenInfo>();
            foreach (XmlNode item in element.ChildNodes)
            {
                if (item.Name.ToLowerInvariant() != "tokens")
                    continue;

                string colorText = item.Attributes.GetNamedItem("color").Value.Trim().ToUpperInvariant();
                string resharperColor = mapping.ContainsKey(colorText)
                    ? mapping[colorText]
                    : DefaultColor;

                foreach (XmlNode token in item.ChildNodes)
                {
                    if (token.Name.ToLowerInvariant() != "token")
                        continue;

                    string tokenName = token.InnerText.Trim().ToLowerInvariant();
                    dict.Add(tokenName, new TokenInfo() {Color = resharperColor});
                }
            }
            
            return dict;
        }
    }
}
