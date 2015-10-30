using System.Collections.Generic;
using System.Collections.ObjectModel;
using JetBrains.ReSharper.Feature.Services.Daemon;

namespace ReSharperExtension.Settings
{
    internal static class ColorHelper
    {
        internal static string DefaultColor = HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE;

        #region Mapping dictionary

        public static readonly Dictionary<string, string> Mapping = new Dictionary<string, string>
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
            {
                "MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE",
                HighlightingAttributeIds.MUTABLE_LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE
            },
            {"METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.METHOD_IDENTIFIER_ATTRIBUTE},
            {"EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.EXTENSION_METHOD_IDENTIFIER_ATTRIBUTE},
            {"OPERATOR_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.OPERATOR_IDENTIFIER_ATTRIBUTE},
            {"TYPE_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_CLASS_ATTRIBUTE},
            {"TYPE_STATIC_CLASS_ATTRIBUTE", HighlightingAttributeIds.TYPE_STATIC_CLASS_ATTRIBUTE},
            {"TYPE_INTERFACE_ATTRIBUTE", HighlightingAttributeIds.TYPE_INTERFACE_ATTRIBUTE},
            {"TYPE_DELEGATE_ATTRIBUTE", HighlightingAttributeIds.TYPE_DELEGATE_ATTRIBUTE},
            {"TYPE_STRUCT_ATTRIBUTE", HighlightingAttributeIds.TYPE_STRUCT_ATTRIBUTE},
            {"TYPE_ENUM_ATTRIBUTE", HighlightingAttributeIds.TYPE_ENUM_ATTRIBUTE},
            {"TYPE_PARAMETER_ATTRIBUTE", HighlightingAttributeIds.TYPE_PARAMETER_ATTRIBUTE},
            {"NAMESPACE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.NAMESPACE_IDENTIFIER_ATTRIBUTE},
            {"PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"LATE_BOUND_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.LATE_BOUND_IDENTIFIER_ATTRIBUTE},
            {"PATH_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.PATH_IDENTIFIER_ATTRIBUTE},
            {"IMPLEMENTS_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_ATTRIBUTE},
            {"OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.OVERRIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_OVERRIDES_ATTRIBUTE},
            {"HIDES_ATTRIBUTE", HighlightingAttributeIds.HIDES_ATTRIBUTE},
            {"IMPLEMENTS_AND_HIDES_ATTRIBUTE", HighlightingAttributeIds.IMPLEMENTS_AND_HIDES_ATTRIBUTE},
            {"INTERFACE_IS_IMPLEMENTED_ATTRIBUTE", HighlightingAttributeIds.INTERFACE_IS_IMPLEMENTED_ATTRIBUTE},
            {"CLASS_IS_INHERITED_ATTRIBUTE", HighlightingAttributeIds.CLASS_IS_INHERITED_ATTRIBUTE},
            {"RECURSION_ATTRIBUTE", HighlightingAttributeIds.RECURSION_ATTRIBUTE},
            {"TODOITEM_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ATTRIBUTE},
            {"TODOITEM_ERRORSTRIPE_ATTRIBUTE", HighlightingAttributeIds.TODOITEM_ERRORSTRIPE_ATTRIBUTE},
            {"FORMAT_STRING_ITEM", HighlightingAttributeIds.FORMAT_STRING_ITEM},
            {"MATCHED_FORMAT_STRING_ITEM", HighlightingAttributeIds.MATCHED_FORMAT_STRING_ITEM},
            {"STRING_ESCAPE_CHARACTER_PRIMARY", HighlightingAttributeIds.STRING_ESCAPE_CHARACTER_PRIMARY},
            {"STRING_ESCAPE_CHARACTER_SECONDARY", HighlightingAttributeIds.STRING_ESCAPE_CHARACTER_SECONDARY},
            {"MATCHED_BRACE", HighlightingAttributeIds.MATCHED_BRACE},
            {"UNMATCHED_BRACE", HighlightingAttributeIds.UNMATCHED_BRACE},
            {"OUTLINE_BRACE", HighlightingAttributeIds.OUTLINE_BRACE},
            {"ANALYSIS_ERROR_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_ERROR_ERRORSTRIPE},
            {"ANALYSIS_WARNING_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_WARNING_ERRORSTRIPE},
            {"ANALYSIS_SUGGESTION_ERRORSTRIPE", HighlightingAttributeIds.ANALYSIS_SUGGESTION_ERRORSTRIPE},
            {"REGEXP_GROUP", HighlightingAttributeIds.REGEXP_GROUP},
            {"REGEXP_SET", HighlightingAttributeIds.REGEXP_SET},
            {"REGEXP_IDENTIFIER", HighlightingAttributeIds.REGEXP_IDENTIFIER},
            {"REGEXP_COMMENT", HighlightingAttributeIds.REGEXP_COMMENT},
            {"REGEXP_MATCHED_VALUE", HighlightingAttributeIds.REGEXP_MATCHED_VALUE},
            {"REGEXP_MATCHED_SELECTION", HighlightingAttributeIds.REGEXP_MATCHED_SELECTION},
            {"JS_PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"JS_LOCAL_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_LOCAL_IDENTIFIER_ATTRIBUTE},
            {"JS_CONSTANT_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_CONSTANT_IDENTIFIER_ATTRIBUTE},
            {"JS_BLOCK_LOCAL_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_BLOCK_LOCAL_IDENTIFIER_ATTRIBUTE},
            {"JS_FUNCTION_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_FUNCTION_IDENTIFIER_ATTRIBUTE},
            {"JS_PROPERTY_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_PROPERTY_IDENTIFIER_ATTRIBUTE},
            {"JS_LATEBOUND_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JS_LATEBOUND_IDENTIFIER_ATTRIBUTE},
            {"JSDOC_PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.JSDOC_PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"JSDOC_KEYWORD_ATTRIBUTE", HighlightingAttributeIds.JSDOC_KEYWORD_ATTRIBUTE},
            {"TS_INTERFACE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_INTERFACE_IDENTIFIER_ATTRIBUTE},
            {"TS_CLASS_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_CLASS_IDENTIFIER_ATTRIBUTE},
            {"TS_ENUM_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_ENUM_IDENTIFIER_ATTRIBUTE},
            {"TS_MODULE_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_MODULE_IDENTIFIER_ATTRIBUTE},
            {"TS_ALIAS_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_ALIAS_IDENTIFIER_ATTRIBUTE},
            {"TS_TYPE_PARAMETER_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_TYPE_PARAMETER_IDENTIFIER_ATTRIBUTE},
            {"TS_CONSTRAINED_IDENTIFIER_ATTRIBUTE", HighlightingAttributeIds.TS_CONSTRAINED_IDENTIFIER_ATTRIBUTE},
            {"CPP_NAMESPACE_ATTRIBUTE", HighlightingAttributeIds.CPP_NAMESPACE_ATTRIBUTE},
            {"CPP_CLASS_ATTRIBUTE", HighlightingAttributeIds.CPP_CLASS_ATTRIBUTE},
            {"CPP_STRUCT_ATTRIBUTE", HighlightingAttributeIds.CPP_STRUCT_ATTRIBUTE},
            {"CPP_ENUM_ATTRIBUTE", HighlightingAttributeIds.CPP_ENUM_ATTRIBUTE},
            {"CPP_UNION_ATTRIBUTE", HighlightingAttributeIds.CPP_UNION_ATTRIBUTE},
            {"CPP_LOCAL_VARIABLE_ATTRIBUTE", HighlightingAttributeIds.CPP_LOCAL_VARIABLE_ATTRIBUTE},
            {"CPP_PARAMETER_VARIABLE_ATTRIBUTE", HighlightingAttributeIds.CPP_PARAMETER_VARIABLE_ATTRIBUTE},
            {"CPP_GLOBAL_VARIABLE_ATTRIBUTE", HighlightingAttributeIds.CPP_GLOBAL_VARIABLE_ATTRIBUTE},
            {"CPP_STRUCT_FIELD_ATTRIBUTE", HighlightingAttributeIds.CPP_STRUCT_FIELD_ATTRIBUTE},
            {"CPP_CLASS_FIELD_ATTRIBUTE", HighlightingAttributeIds.CPP_CLASS_FIELD_ATTRIBUTE},
            {"CPP_UNION_MEMBER_ATTRIBUTE", HighlightingAttributeIds.CPP_UNION_MEMBER_ATTRIBUTE},
            {"CPP_ENUMERATOR_ATTRIBUTE", HighlightingAttributeIds.CPP_ENUMERATOR_ATTRIBUTE},
            {"CPP_LOCAL_TYPEDEF_ATTRIBUTE", HighlightingAttributeIds.CPP_LOCAL_TYPEDEF_ATTRIBUTE},
            {"CPP_TYPEDEF_ATTRIBUTE", HighlightingAttributeIds.CPP_TYPEDEF_ATTRIBUTE},
            {"CPP_GLOBAL_FUNCTION_ATTRIBUTE", HighlightingAttributeIds.CPP_GLOBAL_FUNCTION_ATTRIBUTE},
            {"CPP_MEMBER_FUNCTION_ATTRIBUTE", HighlightingAttributeIds.CPP_MEMBER_FUNCTION_ATTRIBUTE},
            {"CPP_OVERLOADED_OPERATOR_ATTRIBUTE", HighlightingAttributeIds.CPP_OVERLOADED_OPERATOR_ATTRIBUTE},
            {"CPP_TEMPLATE_PARAMETER_ATTRIBUTE", HighlightingAttributeIds.CPP_TEMPLATE_PARAMETER_ATTRIBUTE},
            {"CPP_DEPENDENT_NAME_ATTRIBUTE", HighlightingAttributeIds.CPP_DEPENDENT_NAME_ATTRIBUTE},
            {"CPP_MACRO_NAME_ATTRIBUTE", HighlightingAttributeIds.CPP_MACRO_NAME_ATTRIBUTE},
            {"CPP_CLASS_IS_INHERITED_ATTRIBUTE", HighlightingAttributeIds.CPP_CLASS_IS_INHERITED_ATTRIBUTE},
            {"CPP_DECLARATOR_IS_INHERITED_ATTRIBUTE", HighlightingAttributeIds.CPP_DECLARATOR_IS_INHERITED_ATTRIBUTE},
            {"CPP_DECLARATOR_OVERRIDES_ATTRIBUTE", HighlightingAttributeIds.CPP_DECLARATOR_OVERRIDES_ATTRIBUTE},
            {"CPP_DECLARATOR_HIDES_ATTRIBUTE", HighlightingAttributeIds.CPP_DECLARATOR_HIDES_ATTRIBUTE},
            {"CPP_DECLARATOR_IMPLEMENTS_ATTRIBUTE", HighlightingAttributeIds.CPP_DECLARATOR_IMPLEMENTS_ATTRIBUTE},
            {"CPP_DECLARATION_DEFINITION_ATTRIBUTE", HighlightingAttributeIds.CPP_DECLARATION_DEFINITION_ATTRIBUTE},
            {"CPP_SPECIALIZATIONS_ATTRIBUTE", HighlightingAttributeIds.CPP_SPECIALIZATIONS_ATTRIBUTE}
        };

        #endregion

        public static ObservableCollection<ColorModelView> GetColors()
        {
            var resCollection = new ObservableCollection<ColorModelView>();

            foreach (KeyValuePair<string, string> pair in Mapping)
            {
                var color = new ColorModelView
                {
                    ColorId = pair.Value,
                    ViewName = pair.Key
                };
                resCollection.Add(color);
            }

            return resCollection;
        }
    }
}
