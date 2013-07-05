create or replace
PACKAGE PK_JRXML2PDF_APEX2JRXML IS
/* *****************************************************************************

Copyright (C) 2012-2013 by Andreas Weiden

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

****************************************************************************** */

/**

  $name    PK_JRXML2PDF_APEX2JRXML

  $created 01.04.2013

  $author  Andreas Weiden

  $desc    package for the creation of JRXML-report-definitions out of the definition
           of a given APEX-pages

  $version 0.5.0.0 01.04.2013 Weiden
           initial draft-version

*/

/**

  $name    PR_INIT_COLLECTION

  $created 01.04.2013

  $author  Andreas Weiden

  $desc    initializes the APEX-collection JRXML with the data from
           the regions of the given APEX-page

  $param   i_nApplicationId APEX-application-id

  $param   i_nPageId APEX-page-id

  $version 1.0.0.0 01.04.2013 Weiden
           initial draft-version

*/
  PROCEDURE PR_INIT_COLLECTION(i_nApplicationId IN NUMBER,
                               i_nPageid        IN NUMBER);

/**

  $name    PR_GENERATE_JRXML

  $created 01.04.2013

  $author  Andreas Weiden

  $desc    takes the given data along with the defintions in the APEX-collection
           JRXML and generates a JRXML-rport-definition out of it.
           The result is stored JRXML_REPORt_DEFINITIONS

  $param   i_nApplicationId        APEX-application-id

  $param   i_nPageId               APEX-page-id

  $param   i_vcReportName          The name used to store the resulting JRXML
  
  $param   i_nReportTemplateId     The overall template
  
  $param   i_nBackgroundTemplateId additional template for the background-band
  
  $param   i_nTitelTemplateId      additional template for the title-band
  
  $param   i_nSummaryTemplateId    additional template for the summary-band
  
  $param   i_nPageheaderTemplateId additional template for the pageheader-band
  
  $param   i_nPagefooterTemplateId additional template for the pagefooter-band

  $version 1.0.0.0 01.04.2013 Weiden
           initial draft-version

*/
  PROCEDURE PR_GENERATE_JRXML(i_nApplicationId        IN NUMBER,
                              i_nPageId               IN NUMBER,
                              i_vcReportName          IN VARCHAR2,
                              i_nReportTemplateId     IN NUMBER,
                              i_nBackgroundTemplateId IN NUMBER,
                              i_nTitelTemplateId      IN NUMBER,
                              i_nSummaryTemplateId    IN NUMBER,
                              i_nPageheaderTemplateId IN NUMBER,
                              i_nPagefooterTemplateId IN NUMBER);

END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_APEX2JRXML IS

  TYPE tRegionData IS RECORD (
    nRegionId         NUMBER,
    vcDisplayPosition PK_JRXML2PDF_TYPES.tMaxVarchar2,
    nColumn           NUMBER,
    nRegionTemplateId NUMBER,
    nColumnTemplateId NUMBER,
    nLabelTemplateId  NUMBER,
    nItemTemplateId   NUMBER,
    nImageTemplateId  NUMBER
  );

  TYPE tRegionDataList IS TABLE OF tRegionData INDEX BY BINARY_INTEGER;

  TYPE tColumn IS RECORD (
    nWidth NUMBER
  );
  
  TYPE tColumnList IS TABLE OF tColumn INDEX BY PK_JRXML2PDF_TYPES.tIndex;
  TYPE tColumnListList IS TABLE OF tColumnList INDEX BY PK_JRXML2PDF_TYPES.tIndex;
 
  TYPE tTemplate IS RECORD (
    clTemplate    CLOB
  );

  TYPE tParamList IS TABLE OF PK_JRXML2PDF_TYPES.tIndex INDEX BY PK_JRXML2PDF_TYPES.tIndex;

  DELIMITERS   CONSTANT VARCHAR2(20):='),;|&+-*&(!=<> ' || CHR(9) || CHR(10) || CHR(13);

  TABLEHEADER1TEMPLATE CONSTANT VARCHAR2(32000):=
  '                 <datasetRun subDataset="#REGIONID#">' || CHR(10);
  
  TABLEPARAMTEMPLATE CONSTANT VARCHAR2(32000):=  
  '                        <datasetParameter name="#PARAMETER#">' || CHR(10) || 
  '                            <datasetParameterExpression><![CDATA[$P{#PARAMETER#}]]></datasetParameterExpression>' || CHR(10) || 
  '                        </datasetParameter>' || CHR(10);
  
  TABLEHEADER2TEMPLATE CONSTANT VARCHAR2(32000):=
  '                     <connectionExpression><![CDATA[$P{REPORT_CONNECTION}]]></connectionExpression>' || CHR(10) || 
  '                 </datasetRun>' || CHR(10);

  PROCEDURE PR_INIT_COLLECTION(i_nApplicationId IN NUMBER,
                               i_nPageid        IN NUMBER) IS
                               
    CURSOR crRegions IS
      SELECT REGION_ID        C001, 
             REGION_NAME      C002,
             SOURCE_TYPE      C003,
             'Y'              C004,
             DISPLAY_POSITION_CODE C005,
             DISPLAY_COLUMN   C006,
             DISPLAY_SEQUENCE N001
        FROM APEX_APPLICATION_PAGE_REGIONS
       WHERE APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageid
         AND SOURCE_TYPE IN ('Report', 'Tabular Form', 'HTML/Text', 'Interactive Report')
       ORDER BY DISPLAY_SEQUENCE;
      
    vcQuery PK_JRXML2PDF_TYPES.tMaxVarchar2;

    nTemplateTabRegion  NUMBER;
    nTemplateTabColumn  NUMBER;
    nTemplateTabItem    NUMBER;
    nTemplateTabLabel   NUMBER;
    nTemplateTabImage   NUMBER;
    nTemplateFormRegion NUMBER;
    nTemplateFormItem   NUMBER;
    nTemplateFormLabel  NUMBER;
    nTemplateFormImage  NUMBER;
    
    FUNCTION FK_GET_TEMPLATE_ID(i_vcType IN VARCHAR2)
    RETURN NUMBER IS
      CURSOR crTemplate IS
        SELECT JAT_ID
          FROM JRXML_APEX2JRXML_TEMPLATES
         WHERE JAT_TYPE=i_vcType
           AND JAT_STANDARD='Y';
      nResult NUMBER;
    BEGIN
      OPEN crTemplate;
      FETCh crTemplate INTO nResult;
      CLOSE crTemplate;
      RETURN nResult;
    END;
  BEGIN
    -- Create new Collection
    APEX_COLLECTION.CREATE_OR_TRUNCATE_COLLECTION('JRXML');
    -- initialize default-templates
    nTemplateTabRegion :=FK_GET_TEMPLATE_ID('TR');
    nTemplateTabColumn :=FK_GET_TEMPLATE_ID('TC');
    nTemplateTabItem   :=FK_GET_TEMPLATE_ID('TI');
    nTemplateTabLabel  :=FK_GET_TEMPLATE_ID('TH');
    nTemplateTabImage  :=FK_GET_TEMPLATE_ID('IT');
    nTemplateFormRegion:=FK_GET_TEMPLATE_ID('FR');
    nTemplateFormItem  :=FK_GET_TEMPLATE_ID('FI');
    nTemplateFormLabel :=FK_GET_TEMPLATE_ID('FL');
    nTemplateFormImage :=FK_GET_TEMPLATE_ID('IF');
    
    FOR rec IN crRegions LOOP
      IF rec.C003='HTML/Text' THEN
        APEX_COLLECTION.ADD_MEMBER (p_collection_name =>'JRXML',
                                    p_c001            =>rec.C001,
                                    p_c002            =>rec.C002,
                                    p_c003            =>rec.C003,
                                    p_c004            =>rec.C004,
                                    p_c005            =>rec.C005,
                                    p_c006            =>rec.C006,
                                    p_c011            =>nTemplateFormRegion,
                                    p_c012            =>NULL,
                                    p_c013            =>nTemplateFormLabel,
                                    p_c014            =>nTemplateFormItem,
                                    p_c015            =>nTemplateFormImage,
                                    p_n001            =>rec.N001);
      ELSE
        APEX_COLLECTION.ADD_MEMBER (p_collection_name =>'JRXML',
                                    p_c001            =>rec.C001,
                                    p_c002            =>rec.C002,
                                    p_c003            =>rec.C003,
                                    p_c004            =>rec.C004,
                                    p_c005            =>rec.C005,
                                    p_c006            =>rec.C006,
                                    p_c011            =>nTemplateTabRegion,
                                    p_c012            =>nTemplateTabColumn,
                                    p_c013            =>nTemplateTabLabel,
                                    p_c014            =>nTemplateTabItem,
                                    p_c015            =>nTemplateTabImage,
                                    p_n001            =>rec.N001);
      END IF;
    END LOOP;
    
  END;

  
  FUNCTION FK_GET_TEMPLATE(i_nId IN NUMBER)
  RETURN CLOB IS
    CURSOR crTemplate IS
      SELECT JAT_TEMPLATE_START
        FROM JRXML_APEX2JRXML_TEMPLATES
       WHERE JAT_ID=i_nId;
    clTemplate CLOB;
  BEGIN
    OPEN crTemplate;
    FETCh crTemplate INTO clTemplate;
    CLOSE crTemplate;

    RETURN clTemplate;
  END;

  FUNCTION FK_REVERSE_PATTERN(i_vcPattern IN VARCHAR2)
  RETURN VARCHAR2 IS
    CURRENCYPATTERN     CONSTANT VARCHAR2(4) :=UNISTR('\00A4');
    ISOCURRENCYPATTERN  CONSTANT VARCHAR2(8) :=UNISTR('\00A4\00A4');

    vcResult PK_JRXML2PDF_TYPES.tPattern;
  BEGIN
    IF    INSTR(i_vcPattern, '9')>0 
       OR INSTR(i_vcPattern, '0')>0 THEN
      -- Num-Pattern
      vcResult:=TRANSLATE(i_vcPattern, '#0.,', '90DG');
      vcResult:=REPLACE(vcResult, 'C', ISOCURRENCYPATTERN);
      vcResult:=REPLACE(vcResult, 'L', CURRENCYPATTERN);
      vcResult:=REPLACE(vcResult, 'FM', '');
    ELSIF i_vcPattern IS NOT NULL THEN
      -- date-pattern
      vcResult:=PK_JRXML2PDF_UTIL.FK_JAVA_DATE_PATTERN(i_vcPattern);
    END IF;
    RETURN vcResult;
  END;


  PROCEDURE PR_GET_TEMPLATES(i_nId            IN  NUMBER,
                             o_clStart        OUT CLOB,
                             o_clEnd          OUT CLOB,
                             o_nContentWidth  OUT NUMBER,
                             o_nHeightOffset  OUT NUMBER)
  IS
    CURSOR crTemplate IS
      SELECT JAT_TEMPLATE_START,
             JAT_TEMPLATE_END,
             JAT_CONTENT_WIDTH,
             JAT_HEIGHT_OFFSET
        FROM JRXML_APEX2JRXML_TEMPLATES
       WHERE JAT_ID=i_nId;
  BEGIN
    OPEN crTemplate;
    FETCh crTemplate INTO o_clStart,
                          o_clEnd,
                          o_nContentWidth,
                          o_nHeightOffset;
    CLOSE crTemplate;
  END;

  FUNCTION FK_REPLACE_IN_CLOB(i_clData IN CLOB, i_vcReplace IN VARCHAR2, i_vcReplaceWith IN CLOB)
  RETURN CLOB IS
    clTemp   CLOB;
    clTemp2  CLOB;
    iPos     PLS_INTEGER;
    clResult CLOB;
  BEGIN
    DBMS_LOB.CREATETEMPORARY(clTemp, TRUE, DBMS_LOB.SESSION);
    DBMS_LOB.APPEND(clTemp, i_clData);
    LOOP
      iPos:=DBMS_LOB.INSTR(clTemp, i_vcReplace);
      IF iPos>0 THEN
        DBMS_LOB.CREATETEMPORARY(clTemp2, TRUE, DBMS_LOB.SESSION);
        DBMS_LOB.COPY(clTemp2, 
                      clTemp, 
                      iPos-1,
                      1,
                      1);
        DBMS_LOB.APPEND(clTemp2, i_vcReplaceWith);
        DBMS_LOB.COPY(clTemp2, 
                      clTemp, 
                      DBMS_LOB.GETLENGTH(clTemp)-iPos-LENGTH(i_vcReplace)+1,
                      DBMS_LOB.GETLENGTH(clTemp2)+1,
                      iPos+LENGTH(i_vcReplace));
        DBMS_LOB.FREETEMPORARY(clTemp);
        DBMS_LOB.CREATETEMPORARY(clTemp, TRUE, DBMS_LOB.SESSION);
        DBMS_LOB.APPEND(clTemp, clTemp2);
        DBMS_LOB.FREETEMPORARY(clTemp2);
      ELSE
        EXIT;
      END IF;
    END LOOP;
    clResult:=clTemp;
    DBMS_LOB.FREETEMPORARY(clTemp);
    RETURN clResult;
  END;
  
  -- ---------------------------------------------------------------------------
  
  PROCEDURE PR_REPLACE(io_clData IN OUT NOCOPY CLOB,
                       i_vcToken IN VARCHAR2,
                       i_vcValue IN VARCHAR2) IS
    iPos PLS_INTEGER;
    clTemp CLOB;
  BEGIN
    IF DBMS_LOB.INSTR (io_clData, i_vcToken)>0 THEN
      clTemp:=FK_REPLACE_IN_CLOB(io_clData, i_vcToken, i_vcValue);
      io_clData:=clTemp;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_COLUMNS_FROM_LOOKUP_QUERY(i_vcLovQuery IN VARCHAR2,
                                         o_vcDisplay  OUT VARCHAR2,
                                         o_vcValue    OUT VARCHAR2
                                        ) IS
    rec          DBMS_SQL.DESC_REC;
    iColCount    PLS_INTEGER:=0;
    rQuery       PK_JRXML2PDF_TYPES.tQuery;
  BEGIN
    -- parse teh resulting query
    rQuery.iCursor:=DBMS_SQL.OPEN_CURSOR;
    rQuery.bEOF:=FALSE;
    rQuery.nRecordPosition:=0;
    rQuery.nRecordRead:=0;
    rQuery.bTreatLastAsCurrent:=FALSE;

    DBMS_SQL.PARSE(rQuery.iCursor, i_vcLovQuery, DBMS_SQL.NATIVE);

    DBMS_SQL.DESCRIBE_COLUMNS (rQuery.iCursor, iColCount, rQuery.lDescTab);
    IF rQuery.lDescTab.COUNT>=2 THEN
      o_vcDisplay:=rQuery.lDescTab(1).col_name;
      o_vcValue:=rQuery.lDescTab(2).col_name;
    END IF;
  END;

  -- ---------------------------------------------------------------------------
   
  FUNCTION FK_BUILD_LOV(i_nApplicationId IN NUMBER,
                        i_vcName         IN VARCHAR2,
                        i_vcInlineQuery  IN VARCHAR2,
                        i_vcColname      IN VARCHAR2)
  RETURN VARCHAR2 IS
    CURSOR crLov IS
      SELECT LOV_ID,
             LOV_TYPE, 
             LIST_OF_VALUES_QUERY
        FROM APEX_APPLICATION_LOVS
       WHERE APPLICATION_ID=i_nApplicationId
         AND LIST_OF_VALUES_NAME=i_vcName;
    recLov       crLov%ROWTYPE;

    CURSOR crLovCols (i_nLovId IN NUMBER) IS
      SELECT DISPLAY_VALUE, RETURN_VALUE
        FROM APEX_APPLICATION_LOV_ENTRIES
       WHERE APPLICATION_ID=i_nApplicationId
         AND LOV_ID=i_nLovId;
    
    vcLookupCode    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcDecodeOptions PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcDecodePiece   PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcDisplayValue  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcReturnValue   PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcDisplay       PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcValue         PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    IF i_vcName IS NOT NULL THEN
      OPEN crLov;
      FETCH crLov INTO recLov;
      CLOSE crLov;
      IF recLov.LOV_TYPE='Static' THEN
        -- Read all values
        vcLookupCode:='DECODE(' || i_vcColName || ',' || CHR(10);
        FOR recLovCols IN crLovCols(recLov.LOV_ID) LOOP
          vcLookupCode:=vcLookupCode || '       '''|| recLovCols.RETURN_VALUE || ''', ''' || recLovCols.DISPLAY_VALUE || ''',' || CHR(10);
        END LOOP;
        vcLookupCode:=vcLookupCode || '       '|| i_vcColName || ') ';
      ELSE
        PR_COLUMNS_FROM_LOOKUP_QUERY(i_vcLovQuery=>recLov.LIST_OF_VALUES_QUERY, 
                                     o_vcDisplay =>vcDisplay,
                                     o_vcValue   =>vcValue
                                    );
        -- wrap up query
        vcLookupCode:='(SELECT ' || vcDisplay || CHR(10) ||
                      '   FROM (' || CHR(10) ||
                      recLov.LIST_OF_VALUES_QUERY ||
                      '        ) INNER' || CHR(10) ||
                      '  WHERE INNER.' || vcValue || '=' || i_vcColname || CHR(10) ||
                      ')';
      END IF;
    ELSE
      -- inline query
      IF UPPER(i_vcInlineQuery) LIKE 'STATIC%' THEN
        -- build up decode from options
        -- Read all values
        vcLookupCode:='DECODE(' || i_vcColName || ',' || CHR(10);
        vcDecodeOptions:=i_vcInlineQuery;
        vcDecodePiece:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDecodeOptions, ':');
        vcDecodePiece:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDecodeOptions, ',');
        WHILE vcDecodePiece IS NOT NULL LOOP
          vcDisplayValue:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDecodePiece, ';');
          vcReturnValue:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDecodePiece, ';');
          vcLookupCode:=vcLookupCode || '       '''|| vcReturnValue || ''', ''' || vcDisplayValue || ''',' || CHR(10);
        END LOOP;
        vcLookupCode:=vcLookupCode || '       '|| i_vcColName || ') ';
      ELSE
        PR_COLUMNS_FROM_LOOKUP_QUERY(i_vcLovQuery=>i_vcInlineQuery, 
                                     o_vcDisplay =>vcDisplay,
                                     o_vcValue   =>vcValue
                                    );
        -- wrap up query
        vcLookupCode:='(SELECT ' || vcDisplay || CHR(10) ||
                      '   FROM (' || CHR(10) ||
                      i_vcInlineQuery ||
                      '        ) INNER' || CHR(10) ||
                      '  WHERE INNER.' || vcValue || '=' || i_vcColname || CHR(10) ||
                      ')';
      END IF;
    END IF;
    RETURN vcLookupCode;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_HANDLE_QUERY(i_nApplicationId IN  NUMBER,
                            i_nPageId        IN  NUMBER,
                            i_vcSource       IN  VARCHAR2,
                            o_clQuery        OUT CLOB,
                            o_clFields       OUT CLOB,
                            o_lVars          OUT tParamList,
                            o_lColumns       OUT tColumnList) IS
    vcVar        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    iColCount    PLS_INTEGER;
    rec          DBMS_SQL.DESC_REC;
    iRecord      PLS_INTEGER:=0;
    nRows        NUMBER;
    lQueryResult PK_JRXML2PDF_TYPES.tQueryresult;
    rEntry       PK_JRXML2PDF_TYPES.tDataEntry;
    rQuery       PK_JRXML2PDF_TYPES.tQuery;
    iQuotePos    PLS_INTEGER;
    iColonPos    PLS_INTEGER;
    iColonEndPos PLS_INTEGER;
    iPos         PLS_INTEGER:=1;
    clQuery      CLOB;
    
    CURSOR crOwner IS
      SELECT OWNER
        FROM APEX_APPLICATIONS 
      WHERE APPLICATION_ID=i_nApplicationId;

    CURSOR crCols IS
      SELECT ITEM_SOURCE
        FROM APEX_APPLICATION_PAGE_ITEMS
       WHERE APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageId
         AND ITEM_SOURCE_TYPE='Database Column'
         ORDER BY DISPLAY_SEQUENCE;

    CURSOR crLookupColumns IS
      SELECT ITEM_SOURCE,
             LOV_NAMED_LOV LOV_NAME,
             LOV_DEFINITION LOV_TEXT
        FROM APEX_APPLICATION_PAGE_ITEMS
       WHERE DISPLAY_AS_CODE IN ('NATIVE_SELECT_LIST',
                                 'NATIVE_POPUP_LOV',
                                 'NATIVE_RADIOGROUP'
                                )
         AND APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageId;
    TYPE tLookupColumns IS TABLE OF crLookupColumns%ROWTYPE;
    
    lLookupCols  tLookupColumns;
   
    TYPE tCols IS TABLE OF crCols%ROWTYPE;
    lCols        tCols;
    bIsLookup    BOOLEAN;
    vcDatasource PK_JRXML2PDF_TYPES.tMaxVarchar2:=i_vcSource;
    vcDummy      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcOwner      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcTable      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem1      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColumn1    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem2      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColumn2    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem3      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColumn3    PK_JRXML2PDF_TYPES.tMaxVarchar2;


  BEGIN
    -- Extract values from datasource
    IF INSTR(vcDatasource, '|')>0 THEN
      vcDummy  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, '|');
    END IF;
    vcOwner  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcTable  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcItem1  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcColumn1:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcItem2  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcColumn2:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcItem3  :=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    vcColumn3:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcDatasource, ':');
    IF vcOwner='#OWNER#' THEN
      OPEN crOwner;
      FETCH crOwner INTO vcOwner;
      CLOSE crOwner;
    END IF;
    --get database columns from regions
    OPEN crCols;
    FETCh crCols
    BULK COLLECT INTO lCols;
    CLOSE crCols;
    DBMS_LOB.CREATETEMPORARY(clQuery, TRUE, DBMS_LOB.SESSION);

    -- build query
    IF lCols.COUNT>0 THEN
      DBMS_LOB.APPEND(clQuery, 'SELECT ');
      FOR i IN 1..lCOls.COUNT LOOP
        DBMS_LOB.APPEND(clQuery, lCols(i).ITEM_SOURCE);
        IF i<lCols.COUNT THEN
          DBMS_LOB.APPEND(clQuery, ',');
        END IF;
        DBMS_LOB.APPEND(clQuery, CHR(10));
      END LOOP;
      DBMS_LOB.APPEND(clQuery, '  FROM ' || vcOwner || '.' || vcTable || CHR(10));
      -- WHERE-condition
      IF vcItem1 IS NOT NULL THEN
        DBMS_LOB.APPEND(clQuery, ' WHERE ' || vcColumn1 || '=:' || vcItem1 || CHR(10));
        o_lVars(vcItem1):=vcItem1;
      END IF;
      IF vcItem2 IS NOT NULL THEN
        DBMS_LOB.APPEND(clQuery, '   AND ' || vcColumn2 || '=:' || vcItem2 || CHR(10));
        o_lVars(vcItem2):=vcItem2;
      END IF;
      IF vcItem3 IS NOT NULL THEN
        DBMS_LOB.APPEND(clQuery, '   AND ' || vcColumn3 || '=:' || vcItem3 || CHR(10));
        o_lVars(vcItem3):=vcItem3;
      END IF;
    ELSE
      clQuery:='SELECT 1 X FROM DUAL';
    END IF;
    -- parse teh resulting query
    rQuery.iCursor:=DBMS_SQL.OPEN_CURSOR;
    rQuery.bEOF:=FALSE;
    rQuery.nRecordPosition:=0;
    rQuery.nRecordRead:=0;
    rQuery.bTreatLastAsCurrent:=FALSE;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Parse query' || SUBSTR(clQuery,1,4000));
    END IF;

    DBMS_SQL.PARSE(rQuery.iCursor, clQuery , DBMS_SQL.NATIVE);

    DBMS_SQL.DESCRIBE_COLUMNS (rQuery.iCursor, iColCount, rQuery.lDescTab);
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Column is (' || rQuery.lDescTab(i).col_type || '):' || rQuery.lDescTab(i).col_name);
      END IF;
      IF rQuery.lDescTab(i).col_type IN     (PK_JRXML2PDF_TYPES.DBMS_SQL_VARCHAR2_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_CHAR_TYPE
                                            ) THEN
        o_clFields:=o_clFields|| '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_max_len;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Varchar:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type =      PK_JRXML2PDF_TYPES.DBMS_SQL_CLOB_TYPE THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Clob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_NUMBER_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_FLOAT_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_BOUBLE_TYPE
                                            ) THEN
        IF rQuery.lDescTab(i).col_precision>0 THEN
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_precision;
        ELSE
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        END IF;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Number:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.math.BigDecimal"/>' || CHR(10);
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_DATE_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TIMESTAMP_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TSTMP_WITH_TZ_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_YEAR_TO_MONTH_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_DAY_TO_SECOND_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.sql.Timestamp"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=10;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Date:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_BLOB_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="oracle.sql.BLOB"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Blob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSE
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=0;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Whatever(' || rQuery.lDescTab(i).col_type || '):' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      END IF;
      IF NVL(o_lColumns(rQuery.lDescTab(i).col_name).nWidth,1)>1 THEN
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=o_lColumns(rQuery.lDescTab(i).col_name).nWidth*LOG(o_lColumns(rQuery.lDescTab(i).col_name).nWidth, 10);
      END IF;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Start building result-query');
    END IF;

    DBMS_LOB.CREATETEMPORARY(o_clQuery, TRUE, DBMS_LOB.SESSION);
    -- Build up SELECT-List from column-descriptions
    -- Lookup-Columns
    OPEN crLookupColumns;
    FETCH crLookupColumns
    BULK COLLECT INTO lLookupCols;
    CLOSE crLookupColumns;
    DBMS_LOB.APPEND(o_clQuery, 'SELECT ');
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      bIsLookup:=FALSE;
      FOR j IN 1..lLookupCols.COUNT LOOP
        IF lLookupCols(j).ITEM_SOURCE=rQuery.lDescTab(i).col_name THEN
          bIsLookup:=TRUE;
          DBMS_LOB.APPEND(o_clQuery, FK_BUILD_LOV(i_nApplicationId,
                                                  lLookupCols(j).LOV_NAME,
                                                  lLookupCols(j).LOV_TEXT,
                                                  rQuery.lDescTab(i).col_name) || ' ' || rQuery.lDescTab(i).col_name);
        END IF;
      END LOOP;
      IF NOT bIsLookup THEN
        DBMS_LOB.APPEND(o_clQuery, '       ' || rQuery.lDescTab(i).col_name);
      END IF;
      IF i<rQuery.lDescTab.COUNT THEN
        DBMS_LOB.APPEND(o_clQuery, ',');
      END IF;
      DBMS_LOB.APPEND(o_clQuery, CHR(10));
    END LOOP;

    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('building inner query');
    END IF;
    
    DBMS_LOB.APPEND(o_clQuery, '  FROM ( ');
    DBMS_LOB.APPEND(o_clQuery, clQuery);
    LOOP
      iQuotePos:=DBMS_LOB.INSTR(o_clQuery, '''', iPos);
      iColonPos:=DBMS_LOB.INSTR(o_clQuery, ':', iPos);
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('replacing quotes' || iQuotePos || '-' || iColonPos);
      END IF;
      IF iQuotePos>0 AND iQuotePos<iColonPos THEN
        iQuotePos:=DBMS_LOB.INSTR(o_clQuery, '''', iQuotePos+1);
        iPos:=iQuotePos+1;
      ELSIF iColonPos>iQuotePos THEN
        -- found match, find end
        iColonEndPos:=DBMS_LOB.GETLENGTH(o_clQuery);
        FOR i IN 1..LENGTH(DELIMITERS) LOOP
          iPos:=INSTR(o_clQuery, SUBSTR(DELIMITERS, i, 1), iColonPos+1);
          IF iPos>0 THEN
            iColonEndPos:=LEAST(iColonEndPos, iPos);
          END IF;
        END LOOP;
        -- replace 
        vcVar:=DBMS_LOB.SUBSTR(o_clQuery, iColonEndPos-iColonPos-1, iColonPos+1);
        o_lVars(vcVar):=vcVar;
        o_clQuery:=FK_REPLACE_IN_CLOB(o_clQuery, ':' || vcVar, '$P{' || vcVar || '}');
        iPos:=iColonPos+1;
      END IF;
      EXIT WHEN iColonPos=0;
    END LOOP;
    DBMS_LOB.APPEND(o_clQuery, '       )');
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('finish query');
    END IF;

    o_clQuery:='<queryString>' || CHR(10) || '      <![CDATA[' || o_clQuery || ']]>' || CHR(10) || '    </queryString>' || CHR(10);
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_HANDLE_REPORT_QUERY(i_nApplicationId IN NUMBER,
                                   i_nRegionId IN NUMBER,
                                   i_clQuery   IN CLOB,
                                   o_clQuery   OUT CLOB,
                                   o_clFields  OUT CLOB,
                                   o_lVars     OUT tParamList,
                                   o_lColumns  OUT tColumnList) IS
    CURSOR crSortCols IS
      SELECT COLUMN_ALIAS || ' ' || NVL(DEFAULT_SORT_DIRECTION, 'ASC') SORTCOL
        FROM APEX_APPLICATION_PAGE_RPT_COLS
       WHERE REGION_ID=i_nRegionId
         AND DEFAULT_SORT_SEQUENCE>0
    ORDER BY DEFAULT_SORT_SEQUENCE;
    
    CURSOR crLookupColumns IS
      SELECT COLUMN_ALIAS,
             NAMED_LIST_OF_VALUES LOV_NAME,
             INLINE_LIST_OF_VALUES LOV_TEXT
        FROM APEX_APPLICATION_PAGE_RPT_COLS
       WHERE DISPLAY_AS_CODE IN ('TEXT_FROM_LOV',
                                 'SELECT_LIST',
                                 'SELECT_LIST_FROM_LOV',
                                 'SELECT_LIST_FROM_QUERY',
                                 'RADIOGROUP',
                                 'RADIOGROUP_FROM_LOV',
                                 'RADIOGROUP_FROM_QUERY',
                                 'POPUP',
                                 'POPUP_FROM_LOV',
                                 'POPUP_FROM_QUERY',
                                 'POPUPKEY_FROM_LOV',
                                 'POPUPKEY_FROM_QUERY'
                                )
         AND REGION_ID=i_nRegionId;
         
    TYPE tLookupColumns IS TABLE OF crLookupColumns%ROWTYPE;
    
    lLookupCols  tLookupColumns;
    vcVar        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    iColCount    PLS_INTEGER;
    rec          DBMS_SQL.DESC_REC;
    iRecord      PLS_INTEGER:=0;
    nRows        NUMBER;
    lQueryResult PK_JRXML2PDF_TYPES.tQueryresult;
    rEntry       PK_JRXML2PDF_TYPES.tDataEntry;
    rQuery       PK_JRXML2PDF_TYPES.tQuery;
    iQuotePos    PLS_INTEGER;
    iColonPos    PLS_INTEGER;
    iColonEndPos PLS_INTEGER;
    iPos         PLS_INTEGER:=1;
    clQuery      CLOB;
    bIsLookup    BOOLEAN;
    vcSort       PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    rQuery.iCursor:=DBMS_SQL.OPEN_CURSOR;
    rQuery.bEOF:=FALSE;
    rQuery.nRecordPosition:=0;
    rQuery.nRecordRead:=0;
    rQuery.bTreatLastAsCurrent:=FALSE;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Parse query ' || i_clQuery);
    END IF;

    DBMS_SQL.PARSE(rQuery.iCursor, i_clQuery , DBMS_SQL.NATIVE);

    DBMS_SQL.DESCRIBE_COLUMNS (rQuery.iCursor, iColCount, rQuery.lDescTab);
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Column ' || rQuery.lDescTab(i).col_name);
      END IF;
      IF rQuery.lDescTab(i).col_type IN     (PK_JRXML2PDF_TYPES.DBMS_SQL_VARCHAR2_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_CHAR_TYPE
                                            ) THEN
        o_clFields:=o_clFields|| '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_max_len;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Varchar:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type =      PK_JRXML2PDF_TYPES.DBMS_SQL_CLOB_TYPE THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Clob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_NUMBER_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_FLOAT_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_BOUBLE_TYPE
                                            ) THEN
        IF rQuery.lDescTab(i).col_precision>0 THEN
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_precision;
        ELSE
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=15;
        END IF;
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.math.BigDecimal"/>' || CHR(10);
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Number:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_DATE_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TIMESTAMP_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TSTMP_WITH_TZ_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_YEAR_TO_MONTH_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_DAY_TO_SECOND_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.sql.Timestamp"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=10;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Date:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_BLOB_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="oracle.sql.BLOB"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Blob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSE
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        -- default to small width
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=5;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Whatever(' || rQuery.lDescTab(i).col_type || '):' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      END IF;
      IF NVL(o_lColumns(rQuery.lDescTab(i).col_name).nWidth,1)>1 THEN
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=o_lColumns(rQuery.lDescTab(i).col_name).nWidth*LOG(o_lColumns(rQuery.lDescTab(i).col_name).nWidth, 10);
      END IF;
    END LOOP;
    -- replace bind-variables by $P{}-JRXML-style fields
    DBMS_LOB.CREATETEMPORARY(clQuery, TRUE, DBMS_LOB.SESSION);
    -- Build up SELECT-List from column-descriptions
    -- Lookup-Columns
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Build up outer query');
    END IF;
    OPEN crLookupColumns;
    FETCH crLookupColumns
    BULK COLLECT INTO lLookupCols;
    CLOSE crLookupColumns;
    DBMS_LOB.APPEND(clQuery, 'SELECT ');
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      bIsLookup:=FALSE;
      FOR j IN 1..lLookupCols.COUNT LOOP
        IF lLookupCols(j).COLUMN_ALIAS=rQuery.lDescTab(i).col_name THEN
          bIsLookup:=TRUE;
          DBMS_LOB.APPEND(clQuery, FK_BUILD_LOV(i_nApplicationId,
                                                lLookupCols(j).LOV_NAME,
                                                lLookupCols(j).LOV_TEXT,
                                                rQuery.lDescTab(i).col_name) || ' ' || rQuery.lDescTab(i).col_name);
        END IF;
      END LOOP;
      IF NOT bIsLookup THEN
        DBMS_LOB.APPEND(clQuery, '       ' || rQuery.lDescTab(i).col_name);
      END IF;
      IF i<rQuery.lDescTab.COUNT THEN
        DBMS_LOB.APPEND(clQuery, ',');
      END IF;
      DBMS_LOB.APPEND(clQuery, CHR(10));
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Build up inner query');
    END IF;
    
    DBMS_LOB.APPEND(clQuery, '  FROM ( ');
    DBMS_LOB.APPEND(clQuery, i_clQuery);
    DBMS_LOB.APPEND(clQuery, ' ');
    LOOP
      iQuotePos:=DBMS_LOB.INSTR(clQuery, '''', iPos);
      iColonPos:=DBMS_LOB.INSTR(clQuery, ':', iPos);
      IF iQuotePos>0 AND iQuotePos<iColonPos THEN
        iQuotePos:=DBMS_LOB.INSTR(clQuery, '''', iQuotePos+1);
        iPos:=iQuotePos+1;
      ELSIF iColonPos>iQuotePos THEN
        -- found match, find end
        iColonEndPos:=DBMS_LOB.GETLENGTH(clQuery);
        FOR i IN 1..LENGTH(DELIMITERS) LOOP
          iPos:=INSTR(clQuery, SUBSTR(DELIMITERS, i, 1), iColonPos+1);
          IF iPos>0 THEN
            iColonEndPos:=LEAST(iColonEndPos, iPos);
          END IF;
        END LOOP;
        -- replace 
        vcVar:=DBMS_LOB.SUBSTR(clQuery, iColonEndPos-iColonPos-1, iColonPos+1);
        o_lVars(vcVar):=vcVar;
        clQuery:=FK_REPLACE_IN_CLOB(clQuery, ':' || vcVar, '$P{' || vcVar || '}');
        iPos:=iColonPos+1;
      END IF;
      EXIT WHEN iColonPos=0;
    END LOOP;
    DBMS_LOB.APPEND(clQuery, '       )');
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('build order by');
    END IF;
    FOR rec IN crSortCols LOOP
      vcSort:=vcSort || rec.SORTCOL || ',';
    END LOOP;
    IF vcSort IS NOT NULL THEN
      DBMS_LOB.APPEND(clQuery, CHR(10) || ' ORDER BY ' || SUBSTR(vcSort, 1, LENGTH(vcSort)-1));
    END IF;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('query done');
    END IF;
    o_clQuery:='<queryString>' || CHR(10) || '      <![CDATA[' || clQuery || ']]>' || CHR(10) || '    </queryString>' || CHR(10);
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL(o_clQuery);
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_HANDLE_IR_QUERY(i_nApplicationId IN NUMBER,
                               i_nRegionId IN NUMBER,
                               i_clQuery   IN CLOB,
                               o_clQuery   OUT CLOB,
                               o_clFields  OUT CLOB,
                               o_lVars     OUT tParamList,
                               o_lColumns  OUT tColumnList) IS

    CURSOR crSortCol IS
      SELECT CASE WHEN SORT_COLUMN_1 IS NOT NULL THEN
               'ORDER BY ' || SORT_COLUMN_1 || ' ' || NVL(SORT_DIRECTION_1, 'ASC')
             END || 
             CASE WHEN NVL(SORT_COLUMN_2,'0')!='0' THEN
               ',' || SORT_COLUMN_2 || ' ' || NVL(SORT_DIRECTION_2, 'ASC')
             END || 
             CASE WHEN NVL(SORT_COLUMN_3,'0')!='0' THEN
               ',' || SORT_COLUMN_3 || ' ' || NVL(SORT_DIRECTION_3, 'ASC')
             END || 
             CASE WHEN NVL(SORT_COLUMN_4,'0')!='0' THEN
               ',' || SORT_COLUMN_4 || ' ' || NVL(SORT_DIRECTION_4, 'ASC')
             END || 
             CASE WHEN NVL(SORT_COLUMN_5,'0')!='0' THEN
               ',' || SORT_COLUMN_5 || ' ' || NVL(SORT_DIRECTION_5, 'ASC')
             END || 
             CASE WHEN NVL(SORT_COLUMN_6,'0')!='0' THEN
               ',' || SORT_COLUMN_6 || ' ' || NVL(SORT_DIRECTION_6, 'ASC')
             END SORTCOL
        FROM APEX_APPLICATION_PAGE_IR A,
             APEX_APPLICATION_PAGE_IR_RPT B
       WHERE A.REGION_ID=i_nRegionId
         AND A.INTERACTIVE_REPORT_ID=B.INTERACTIVE_REPORT_ID
         AND B.REPORT_TYPE='PRIMARY_DEFAULT';


    CURSOR crLookupColumns IS
      SELECT COLUMN_ALIAS,
             NAMED_LOV LOV_NAME,
             NULL LOV_TEXT
        FROM APEX_APPLICATION_PAGE_IR_COL
       WHERE DISPLAY_TEXT_AS IN ('LOV_ESCAPE_SC'
                                )
         AND REGION_ID=i_nRegionId;
         
    TYPE tLookupColumns IS TABLE OF crLookupColumns%ROWTYPE;
    
    CURSOR crConditions IS
      SELECT B.CONDITION_SQL, 
             B.CONDITION_EXPRESSION,
             B.CONDITION_EXPRESSION2
        FROM APEX_APPLICATION_PAGE_IR A,
             APEX_APPLICATION_PAGE_IR_COND B,
             APEX_APPLICATION_PAGE_IR_RPT C
       WHERE A.REGION_ID=i_nRegionId
         AND A.INTERACTIVE_REPORT_ID=B.INTERACTIVE_REPORT_ID
         AND B.INTERACTIVE_REPORT_ID=C.INTERACTIVE_REPORT_ID
         AND B.REPORT_ID=C.REPORT_ID
         AND B.CONDITION_TYPE='Filter'
         AND C.REPORT_TYPE='PRIMARY_DEFAULT'
;
    
    lLookupCols  tLookupColumns;
    vcVar        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    iColCount    PLS_INTEGER;
    rec          DBMS_SQL.DESC_REC;
    iRecord      PLS_INTEGER:=0;
    nRows        NUMBER;
    lQueryResult PK_JRXML2PDF_TYPES.tQueryresult;
    rEntry       PK_JRXML2PDF_TYPES.tDataEntry;
    rQuery       PK_JRXML2PDF_TYPES.tQuery;
    iQuotePos    PLS_INTEGER;
    iColonPos    PLS_INTEGER;
    iColonEndPos PLS_INTEGER;
    iPos         PLS_INTEGER:=1;
    clQuery      CLOB;
    bIsLookup    BOOLEAN;
    vcSort       PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcToAdd      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    
    FUNCTION FK_BUILD_CONDITION(i_vcConditionSql IN VARCHAR2,
                                i_vcExpression1  IN VARCHAR2,
                                i_vcExpression2  IN VARCHAR2)
    RETURN VARCHAR2 IS
      vcResult        PK_JRXML2PDF_TYPES.tMaxVarchar2:=i_vcConditionSql;
      iPos            PLS_INTEGER:=1;
      vcExpressions PK_JRXML2PDF_TYPES.tMaxVarchar2:=i_vcExpression1;
      vcOneValue    PK_JRXML2PDF_TYPES.tMaxVarchar2;
      
      FUNCTION FK_VALUE(i_vcValue IN VARCHAR2)
      RETURN VARCHAR2 IS
      BEGIN
        RETURN '''' || REPLACE(i_vcValue, '''', '''''') || '''';
      END;
    BEGIN
      IF INSTR(i_vcConditionSql, '#APXWS_EXPR#')>0 THEN
        -- complete expression
        vcResult:=REPLACE(vcResult, '#APXWS_EXPR#', FK_VALUE(i_vcExpression1));
        vcResult:=REPLACE(vcResult, '#APXWS_EXPR2#', FK_VALUE(i_vcExpression2));
      ELSE
        WHILE INSTR(i_vcConditionSql, '#APXWS_EXPR_VAL' || TO_CHAR(iPos) || '#')>0 LOOP
          vcOneValue:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcExpressions, ',');
          vcResult:=REPLACE(vcResult, '#APXWS_EXPR_VAL' || TO_CHAR(iPos) || '#', FK_VALUE(vcOneValue));
          iPos:=iPos+1;
        END LOOP;
      END IF;
      RETURN vcResult;
    END;
  BEGIN
    rQuery.iCursor:=DBMS_SQL.OPEN_CURSOR;
    rQuery.bEOF:=FALSE;
    rQuery.nRecordPosition:=0;
    rQuery.nRecordRead:=0;
    rQuery.bTreatLastAsCurrent:=FALSE;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Parse query ' || i_clQuery);
    END IF;

    DBMS_SQL.PARSE(rQuery.iCursor, i_clQuery , DBMS_SQL.NATIVE);

    DBMS_SQL.DESCRIBE_COLUMNS (rQuery.iCursor, iColCount, rQuery.lDescTab);
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Column ' || rQuery.lDescTab(i).col_name);
      END IF;
      IF rQuery.lDescTab(i).col_type IN     (PK_JRXML2PDF_TYPES.DBMS_SQL_VARCHAR2_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_CHAR_TYPE
                                            ) THEN
        o_clFields:=o_clFields|| '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_max_len;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Varchar:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type =      PK_JRXML2PDF_TYPES.DBMS_SQL_CLOB_TYPE THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Clob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_NUMBER_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_FLOAT_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_BOUBLE_TYPE
                                            ) THEN
        IF rQuery.lDescTab(i).col_precision>0 THEN
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=rQuery.lDescTab(i).col_precision;
        ELSE
          o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=15;
        END IF;
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.math.BigDecimal"/>' || CHR(10);
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Number:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_DATE_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TIMESTAMP_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TSTMP_WITH_TZ_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_YEAR_TO_MONTH_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_DAY_TO_SECOND_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.sql.Timestamp"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=10;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Date:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_BLOB_TYPE
                                            ) THEN
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="oracle.sql.BLOB"/>' || CHR(10);
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=30;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Blob:' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      ELSE
        o_clFields:=o_clFields || '    <field name="' || rQuery.lDescTab(i).col_name || '" class="java.lang.String"/>' || CHR(10);
        -- default to small width
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=5;
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Whatever(' || rQuery.lDescTab(i).col_type || '):' || o_lColumns(rQuery.lDescTab(i).col_name).nWidth);
        END IF;
      END IF;
      IF NVL(o_lColumns(rQuery.lDescTab(i).col_name).nWidth,1)>1 THEN
        o_lColumns(rQuery.lDescTab(i).col_name).nWidth:=o_lColumns(rQuery.lDescTab(i).col_name).nWidth*LOG(o_lColumns(rQuery.lDescTab(i).col_name).nWidth, 10);
      END IF;
    END LOOP;
    -- replace bind-variables by $P{}-JRXML-style fields
    DBMS_LOB.CREATETEMPORARY(clQuery, TRUE, DBMS_LOB.SESSION);
    -- Build up SELECT-List from column-descriptions
    -- Lookup-Columns
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Build up outer query');
    END IF;
    OPEN crLookupColumns;
    FETCH crLookupColumns
    BULK COLLECT INTO lLookupCols;
    CLOSE crLookupColumns;
    DBMS_LOB.APPEND(clQuery, 'SELECT ');
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      bIsLookup:=FALSE;
      FOR j IN 1..lLookupCols.COUNT LOOP
        IF lLookupCols(j).COLUMN_ALIAS=rQuery.lDescTab(i).col_name THEN
          bIsLookup:=TRUE;
          DBMS_LOB.APPEND(clQuery, FK_BUILD_LOV(i_nApplicationId,
                                                lLookupCols(j).LOV_NAME,
                                                lLookupCols(j).LOV_TEXT,
                                                rQuery.lDescTab(i).col_name) || ' ' || rQuery.lDescTab(i).col_name);
        END IF;
      END LOOP;
      IF NOT bIsLookup THEN
        DBMS_LOB.APPEND(clQuery, '       ' || rQuery.lDescTab(i).col_name);
      END IF;
      IF i<rQuery.lDescTab.COUNT THEN
        DBMS_LOB.APPEND(clQuery, ',');
      END IF;
      DBMS_LOB.APPEND(clQuery, CHR(10));
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Build up inner query');
    END IF;
    
    DBMS_LOB.APPEND(clQuery, '  FROM ( ');
    DBMS_LOB.APPEND(clQuery, i_clQuery);
    LOOP
      iQuotePos:=DBMS_LOB.INSTR(clQuery, '''', iPos);
      iColonPos:=DBMS_LOB.INSTR(clQuery, ':', iPos);
      IF iQuotePos>0 AND iQuotePos<iColonPos THEN
        iQuotePos:=DBMS_LOB.INSTR(clQuery, '''', iQuotePos+1);
        iPos:=iQuotePos+1;
      ELSIF iColonPos>iQuotePos THEN
        -- found match, find end
        iColonEndPos:=DBMS_LOB.GETLENGTH(clQuery);
        FOR i IN 1..LENGTH(DELIMITERS) LOOP
          iPos:=INSTR(clQuery, SUBSTR(DELIMITERS, i, 1), iColonPos+1);
          IF iPos>0 THEN
            iColonEndPos:=LEAST(iColonEndPos, iPos);
          END IF;
        END LOOP;
        -- replace 
        vcVar:=DBMS_LOB.SUBSTR(clQuery, iColonEndPos-iColonPos-1, iColonPos+1);
        o_lVars(vcVar):=vcVar;
        clQuery:=FK_REPLACE_IN_CLOB(clQuery, ':' || vcVar, '$P{' || vcVar || '}');
        iPos:=iColonPos+1;
      END IF;
      EXIT WHEN iColonPos=0;
    END LOOP;
    DBMS_LOB.APPEND(clQuery, '       )');
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('apply IR-conditions');
    END IF;
    vcToAdd:=' WHERE ';
    FOR recConditions IN crConditions LOOP
      DBMS_LOB.APPEND(clQuery, vcToAdd || FK_BUILD_CONDITION(recConditions.CONDITION_SQL, 
                                                             recConditions.CONDITION_EXPRESSION,
                                                             recConditions.CONDITION_EXPRESSION2) || CHR(10));
      vcToAdd:='   AND ';
    END LOOP;
    
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('build order by ');
    END IF;
    OPEN crSortCol;
    FETCH crSortCol INTO vcSort;
    CLOSE crSortCol;
    IF vcSort IS NOT NULL THEN
      DBMS_LOB.APPEND(clQuery, CHR(10) || vcSort);
    END IF;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('query done');
    END IF;
    o_clQuery:='<queryString>' || CHR(10) || '      <![CDATA[' || clQuery || ']]>' || CHR(10) || '    </queryString>' || CHR(10);
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL(o_clQuery);
    END IF;
  END;

  -- ----------------------------------------------------------------------------

  PROCEDURE PR_CREATE_REPORT_TABLE(i_nRegionId    IN NUMBER, 
                                   i_vcRegionName IN VARCHAR2,
                                   i_rRegion      IN tRegionData,
                                   i_lVars        IN tParamList,
                                   i_lColumns     IN tColumnList,
                                   i_nXOffset     IN NUMBER,
                                   o_clData       OUT CLOB,
                                   o_nHeight      OUT NUMBER,
                                   o_nWidth       OUT NUMBER) IS
    CURSOR crCols IS
      SELECT COLUMN_ALIAS, 
             DISPLAY_AS_CODE, 
             DISPLAY_SEQUENCE, 
             HEADING, 
             DECODE(COLUMN_ALIGNMENT, 'LEFT', 'Left',
                                      'RIGHT', 'Right',
                                      'CENTER', 'Center',
                                      'Left') COLUMN_ALIGNMENT, 
             DECODE(HEADING_ALIGNMENT, 'LEFT', 'Left',
                                       'RIGHT', 'Right',
                                       'CENTER', 'Center',
                                       'Left') HEADING_ALIGNMENT, 
             DEFAULT_SORT_SEQUENCE, 
             DEFAULT_SORT_DIRECTION, 
             FORMAT_MASK, 
             REPORT_COLUMN_WIDTH, 
             REFERENCE_TABLE_NAME, 
             REFERENCE_COLUMN_NAME
        FROM APEX_APPLICATION_PAGE_RPT_COLS
       WHERE REGION_ID=i_nRegionId
       ORDER BY DISPLAY_SEQUENCE;
    TYPE tCols IS TABLE OF crCols%ROWTYPE;
    
    lCols tCols;
    
    clTable       CLOB;
    clTabStart    CLOB;
    clTabEnd      CLOB;
    clColStart    CLOB;
    clColEnd      CLOB;
    clHeader      CLOB;
    clItem        CLOB;
    clDummy       CLOB;
    vcColStart    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItemHeader  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColEnd      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nWidth        NUMBER;
    nXPos         NUMBER:=0;
    vcVar         PK_JRXML2PDF_TYPES.tIndex;
    nMaxWidth     NUMBER;
    nDummy        NUMBER;
    nTotalWidth   NUMBER;
    nUnitPerWidth NUMBER;
    nRowHeight    NUMBER;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Load templates');
    END IF;
  
    -- load templates
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nRegionTemplateId,
                     o_clStart        =>clTabStart,
                     o_clEnd          =>clTabEnd,
                     o_nContentWidth  =>nMaxWidth,
                     o_nHeightOffset  =>o_nHeight);

    -- load templates
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nColumnTemplateId,
                     o_clStart        =>clColStart,
                     o_clEnd          =>clColEnd,
                     o_nContentWidth  =>nDummy,
                     o_nHeightOffset  =>nDummy);
  
    clHeader:=FK_GET_TEMPLATE(i_nId=>i_rRegion.nLabelTemplateId);
    
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nItemTemplateId,
                     o_clStart        =>clItem,
                     o_clEnd          =>clDummy,
                     o_nContentWidth  =>nDummy,
                     o_nHeightOffset  =>nRowHeight);

    DBMS_LOB.CREATETEMPORARY(clTable, TRUE, DBMS_LOB.SESSION);
    DBMS_LOB.APPEND(clTable, clTabStart);

    DBMS_LOB.APPEND(clTable, TABLEHEADER1TEMPLATE);

    -- Parametermappings
    vcVar:=i_lVars.FIRST;
    LOOP
      EXIT WHEN vcVar IS NULL;
      DBMS_LOB.APPEND(clTable, REPLACE(TABLEPARAMTEMPLATE, '#PARAMETER#', vcVar));
      vcVar:=i_lVars.NEXT(vcVar);
    END LOOP;
    DBMS_LOB.APPEND(clTable, TABLEHEADER2TEMPLATE);

    PR_REPLACE(clTable, '#XPOSITION#', i_nXOffset);
    PR_REPLACE(clTable, '#YPOSITION#', '0');
    PR_REPLACE(clTable, '#WIDTH#', nMaxWidth);
    PR_REPLACE(clTable, '#HEIGHT#', '10');
    PR_REPLACE(clTable, '#REGIONID#', i_nRegionId);
    OPEN crCols;
    FETCH crCols
    BULK COLLECT INTO lCols;
    CLOSE crCols;
    nTotalWidth:=0;
    FOR i IN 1..lCols.COUNT LOOP
      IF NVL(lCols(i).REPORT_COLUMN_WIDTH,0)>0 THEN
        nTotalWidth:=nTotalWidth+lCols(i).REPORT_COLUMN_WIDTH;
      ELSIF i_lColumns.EXISTS(lCols(i).COLUMN_ALIAS) THEN
        nTotalWidth:=nTotalWidth+i_lColumns(lCols(i).COLUMN_ALIAS).nWidth;
      ELSE
        nTotalWidth:=nTotalWidth+5;
      END IF;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Total width for report' || nTotalWidth);
    END IF;
    -- Units per with
    nUnitPerWidth:=nMaxWidth/nTotalWidth;
    FOR i IN 1..lCols.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('Column ' || i);
      END IF;
      IF NVL(lCols(i).REPORT_COLUMN_WIDTH,0)>0 THEN
        nWidth:=TRUNC(lCols(i).REPORT_COLUMN_WIDTH*nUnitPerWidth);
      ELSIF i_lColumns.EXISTS(lCols(i).COLUMN_ALIAS) THEN
        nWidth:=TRUNC(i_lColumns(lCols(i).COLUMN_ALIAS).nWidth*nUnitPerWidth);
      ELSE
        nWidth:=TRUNC(5*nUnitPerWidth);
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('- Width is ' || nWidth);
      END IF;
    
      vcColStart:=clColStart;
      vcColStart:=REPLACE(vcColStart, '#COLUMNWIDTH#', nWidth);
    
      vcItemHeader:=clHeader;
      vcItemHeader:=REPLACE(vcItemHeader, '#XPOSITION#', nXPos);
      vcItemHeader:=REPLACE(vcItemHeader, '#COLUMNWIDTH#', nWidth);
      vcItemHeader:=REPLACE(vcItemHeader, '#HEADERTEXT#', lCols(i).HEADING);
      vcItemHeader:=REPLACE(vcItemHeader, '#ALIGNMENT#', lCols(i).HEADING_ALIGNMENT);

      vcItem:=clItem;
      vcItem:=REPLACE(vcItem, '#XPOSITION#', nXPos);
      vcItem:=REPLACE(vcItem, '#COLUMNWIDTH#', nWidth);
      vcItem:=REPLACE(vcItem, '#FIELD#', lCols(i).COLUMN_ALIAS);
      vcItem:=REPLACE(vcItem, '#ALIGNMENT#', lCols(i).COLUMN_ALIGNMENT);
      vcItem:=REPLACE(vcItem, '#PATTERN#', FK_REVERSE_PATTERN(lCols(i).FORMAT_MASK));
      vcColEnd:=clColEnd;

      clTable:=clTable || vcColStart || vcItemHeader || vcItem || vcColEnd;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('- Go to next column');
      END IF;
      
      nXPos:=nXPos+nWidth;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Report is done');
    END IF;
    DBMS_LOB.APPEND(clTable, clTabEnd);
    -- replace region-name
    IF i_vcRegionName IS NOT NULL THEN
      PR_REPLACE(clTable, '#REGION_TITLE#', i_vcRegionName);
    ELSE
      PR_REPLACE(clTable, '#REGION_TITLE#', ' ');
    END IF;
    -- set output data
    o_nWidth:=nMaxWidth;
    o_cldata:=clTable;
  END;

  -- ----------------------------------------------------------------------------

  PROCEDURE PR_CREATE_IR_TABLE(i_nRegionId    IN NUMBER, 
                               i_vcRegionName IN VARCHAR2,
                               i_rRegion      IN tRegionData,
                               i_lVars        IN tParamList,
                               i_lColumns     IN tColumnList,
                               i_nXOffset     IN NUMBER,
                               o_clData       OUT CLOB,
                               o_nHeight      OUT NUMBER,
                               o_nWidth       OUT NUMBER) IS

    CURSOR crColList IS
      SELECT REPORT_COLUMNS
        FROM APEX_APPLICATION_PAGE_IR A,
             APEX_APPLICATION_PAGE_IR_RPT B
       WHERE A.REGION_ID=i_nRegionId
         AND A.INTERACTIVE_REPORT_ID=B.INTERACTIVE_REPORT_ID
         AND B.REPORT_TYPE='PRIMARY_DEFAULT';
  
    CURSOR crOneColumn(i_vcColumn IN VARCHAR2) IS
      SELECT COLUMN_ALIAS,
             REPORT_LABEL, 
             DECODE(COLUMN_ALIGNMENT, 'LEFT', 'Left',
                                      'RIGHT', 'Right',
                                      'CENTER', 'Center',
                                      'Left') COLUMN_ALIGNMENT, 
             DECODE(HEADING_ALIGNMENT, 'LEFT', 'Left',
                                       'RIGHT', 'Right',
                                       'CENTER', 'Center',
                                       'Left') HEADING_ALIGNMENT, 
             FORMAT_MASK
        FROM APEX_APPLICATION_PAGE_IR_COL
       WHERE REGION_ID=i_nRegionId
         AND COLUMN_ALIAS=i_vcColumn;
    
    TYPE tOneColumn IS TABLE OF crOneColumn%ROWTYPE INDEX BY BINARY_INTEGER;
    
    lCols           tOneColumn;
    
    clTable         CLOB;
    clTabStart      CLOB;
    clTabEnd        CLOB;
    clColStart      CLOB;
    clColEnd        CLOB;
    clHeader        CLOB;
    clItem          CLOB;
    
    vcColStart      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItemHeader    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem          PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColEnd        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nWidth          NUMBER;
    nXPos           NUMBER:=0;
    vcVar           PK_JRXML2PDF_TYPES.tIndex;
    nMaxWidth       NUMBER;
    nDummy          NUMBER;
    nTotalWidth     NUMBER;
    nUnitPerWidth   NUMBER;
    vcReportColumns PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcColumn        PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Load templates');
    END IF;
  
    -- load templates
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nRegionTemplateId,
                     o_clStart        =>clTabStart,
                     o_clEnd          =>clTabEnd,
                     o_nContentWidth  =>nMaxWidth,
                     o_nHeightOffset  =>o_nHeight);

    -- load templates
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nColumnTemplateId,
                     o_clStart        =>clColStart,
                     o_clEnd          =>clColEnd,
                     o_nContentWidth  =>nDummy,
                     o_nHeightOffset  =>nDummy);
  
    clHeader:=FK_GET_TEMPLATE(i_nId=>i_rRegion.nLabelTemplateId);
    clItem  :=FK_GET_TEMPLATE(i_nId=>i_rRegion.nItemTemplateId);

    DBMS_LOB.CREATETEMPORARY(clTable, TRUE, DBMS_LOB.SESSION);
    DBMS_LOB.APPEND(clTable, clTabStart);

    DBMS_LOB.APPEND(clTable, TABLEHEADER1TEMPLATE);

    -- Parametermappings
    vcVar:=i_lVars.FIRST;
    LOOP
      EXIT WHEN vcVar IS NULL;
      DBMS_LOB.APPEND(clTable, REPLACE(TABLEPARAMTEMPLATE, '#PARAMETER#', vcVar));
      vcVar:=i_lVars.NEXT(vcVar);
    END LOOP;
    DBMS_LOB.APPEND(clTable, TABLEHEADER2TEMPLATE);

    PR_REPLACE(clTable, '#XPOSITION#', i_nXOffset);
    PR_REPLACE(clTable, '#YPOSITION#', '0');
    PR_REPLACE(clTable, '#WIDTH#', nMaxWidth);
    PR_REPLACE(clTable, '#HEIGHT#', '10');
    PR_REPLACE(clTable, '#REGIONID#', i_nRegionId);
    
    -- Get Columns for report
    OPEN crColList;
    FETCH crColList INTO vcReportColumns;
    CLOSE crColList;
    
    vcColumn:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcReportColumns, ':');
    WHILE vcColumn IS NOT NULL LOOP
      lCols(lCols.COUNT+1).COLUMN_ALIAS:=vcColumn;
      OPEN crOneColumn(vcColumn);
      FETCH crOneColumn INTO lCols(lCols.COUNT);
      CLOSE crOneColumn;
      vcColumn:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcReportColumns, ':');
    END LOOP;
    nTotalWidth:=0;
    FOR i IN 1..lCols.COUNT LOOP
      IF i_lColumns.EXISTS(lCols(i).COLUMN_ALIAS) THEN
        nTotalWidth:=nTotalWidth+i_lColumns(lCols(i).COLUMN_ALIAS).nWidth;
      ELSE
        nTotalWidth:=nTotalWidth+5;
      END IF;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Total width for report' || nTotalWidth);
    END IF;
    -- Units per with
    nUnitPerWidth:=nMaxWidth/nTotalWidth;
    FOR i IN 1..lCols.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('Column ' || i);
      END IF;
      IF i_lColumns.EXISTS(lCols(i).COLUMN_ALIAS) THEN
        nWidth:=TRUNC(i_lColumns(lCols(i).COLUMN_ALIAS).nWidth*nUnitPerWidth);
      ELSE
        nWidth:=TRUNC(5*nUnitPerWidth);
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('- Width is ' || nWidth);
      END IF;
    
      vcColStart:=clColStart;
      vcColStart:=REPLACE(vcColStart, '#COLUMNWIDTH#', nWidth);
    
      vcItemHeader:=clHeader;
      vcItemHeader:=REPLACE(vcItemHeader, '#XPOSITION#', nXPos);
      vcItemHeader:=REPLACE(vcItemHeader, '#COLUMNWIDTH#', nWidth);
      vcItemHeader:=REPLACE(vcItemHeader, '#HEADERTEXT#', lCols(i).REPORT_LABEL);
      vcItemHeader:=REPLACE(vcItemHeader, '#ALIGNMENT#', lCols(i).HEADING_ALIGNMENT);

      vcItem:=clItem;
      vcItem:=REPLACE(vcItem, '#XPOSITION#', nXPos);
      vcItem:=REPLACE(vcItem, '#COLUMNWIDTH#', nWidth);
      vcItem:=REPLACE(vcItem, '#FIELD#', lCols(i).COLUMN_ALIAS);
      vcItem:=REPLACE(vcItem, '#ALIGNMENT#', lCols(i).COLUMN_ALIGNMENT);
      vcItem:=REPLACE(vcItem, '#PATTERN#', FK_REVERSE_PATTERN(lCols(i).FORMAT_MASK));
      vcColEnd:=clColEnd;

      clTable:=clTable || vcColStart || vcItemHeader || vcItem || vcColEnd;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('- Go to next column');
      END IF;
      
      nXPos:=nXPos+nWidth;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Report is done');
    END IF;
    DBMS_LOB.APPEND(clTable, clTabEnd);
    -- replace region-name
    IF i_vcRegionName IS NOT NULL THEN
      PR_REPLACE(clTable, '#REGION_TITLE#', i_vcRegionName);
    ELSE
      PR_REPLACE(clTable, '#REGION_TITLE#', ' ');
    END IF;
    -- set output data
    o_nWidth:=nMaxWidth;
    o_clData:=clTable;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CREATE_FORM_REGION(i_nRegionId   IN NUMBER,
                                 i_vcRegionName IN VARCHAR2,
                                 i_rRegion      IN tRegionData,
                                 i_nXOffset     IN NUMBER,
                                 o_clData       OUT CLOB,
                                 o_nHeight      OUT NUMBER,
                                 o_nWidth       OUT NUMBER) IS
    CURSOR crItems IS
      SELECT ITEM_NAME, 
           ITEM_SOURCE_TYPE, 
           ITEM_SOURCE, 
           LABEL, 
           FORMAT_MASK, 
           REGION_ID, 
           DISPLAY_AS_CODE,
           LOV_NAMED_LOV,
           LOV_DEFINITION,
           ITEM_ELEMENT_WIDTH,
           ITEM_ELEMENT_HEIGHT,
           BEGINS_ON_NEW_ROW,
           LABEL_ALIGNMENT,
           ITEM_ALIGNMENT,
           0 REGION_ROW,
           0 REGION_COL,
           0 LABEL_WIDTH,
           0 ITEM_WIDTH
      FROM APEX_APPLICATION_PAGE_ITEMS
     WHERE REGION_ID=i_nRegionId
       AND DISPLAY_AS_CODE!='NATIVE_HIDDEN'
       ORDER BY DISPLAY_SEQUENCE;
       
    TYPE tItems IS TABLE OF crItems%ROWTYPE;
    
    lItems      tItems;

    
    clForm        CLOB;
    clFormStart   CLOB;
    clFormEnd     CLOB;
    clLabel       CLOB;
    clItem        CLOB;
    clImage       CLOB;
    clDummy       CLOB;
    nMaxWidth     NUMBER;
    nRow          NUMBER;
    nCol          NUMBER;
    nWidth        NUMBER;
    nHeight       NUMBER;
    nHeightOffset NUMBER;
    nItemHeight   NUMBER;
    nDummy        NUMBER;
    vcItemLabel   PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcItem        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nXPos         NUMBER;
    lLabelWidth   PK_JRXML2PDF_TYPES.tNumList;
    lItemWidth    PK_JRXML2PDF_TYPES.tNumList;
    nUnitPerWidth NUMBER;
    nTotalWidth   NUMBER;
    nImageWidth   NUMBER;
    nImageHeight  NUMBER;
    nRowHeight    NUMBER;
  BEGIN
    -- load templates
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nRegionTemplateId,
                     o_clStart        =>clFormStart,
                     o_clEnd          =>clFormEnd,
                     o_nContentWidth  =>nMaxWidth,
                     o_nHeightOffset  =>nHeightOffset);

    clLabel:=FK_GET_TEMPLATE(i_nId=>i_rRegion.nLabelTemplateId);
    
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nItemTemplateId,
                     o_clStart        =>clItem,
                     o_clEnd          =>clDummy,
                     o_nContentWidth  =>nDummy,
                     o_nHeightOffset  =>nItemHeight);
  
    PR_GET_TEMPLATES(i_nId            =>i_rRegion.nImageTemplateId,
                     o_clStart        =>clImage,
                     o_clEnd          =>clDummy,
                     o_nContentWidth  =>nImageWidth,
                     o_nHeightOffset  =>nImageHeight);

    DBMS_LOB.CREATETEMPORARY(clForm, TRUE, DBMS_LOB.SESSION);
    DBMS_LOB.APPEND(clForm, clFormStart);
    PR_REPLACE(clForm, '#XPOSITION#', i_nXOffset);
    PR_REPLACE(clForm, '#YPOSITION#', '0');
    PR_REPLACE(clForm, '#WIDTH#', nMaxWidth);
    PR_REPLACE(clForm, '#REGIONID#', i_nRegionId);
    OPEN crItems;
    FETCH crItems
    BULK COLLECT INTO lItems;
    CLOSE crItems;

    -- Sort items into grid
    -- Units per with
    nRow:=0;
    nCol:=1;
    FOR i IN 1..lItems.COUNT LOOP
      IF lItems(i).BEGINS_ON_NEW_ROW='Yes' THEN
        nRow:=nRow+1;
        nCol:=1;
      END IF;
      lItems(i).REGION_ROW:=nRow;
      lItems(i).REGION_COL:=nCol;
      nCol:=nCol+1;
    END LOOP;

    -- max with per column
    
    FOR i IN 1..lItems.COUNT LOOP
      IF lLabelWidth.EXISTS(lItems(i).REGION_COL) THEN
        lLabelWidth(lItems(i).REGION_COL):=GREATEST(lLabelWidth(lItems(i).REGION_COL), NVL(LENGTH(lItems(i).LABEL),1)*5);
      ELSE
        lLabelWidth(lItems(i).REGION_COL):=NVL(LENGTH(lItems(i).LABEL),1)*5;
      END IF;
      IF lItems(i).DISPLAY_AS_CODE='NATIVE_DISPLAY_IMAGE' THEN
        nWidth:=nImageWidth;
      ELSE
        nWidth:=NVL(lItems(i).ITEM_ELEMENT_WIDTH,1)*5;
      END IF;
      
      IF lItemWidth.EXISTS(lItems(i).REGION_COL) THEN
        lItemWidth(lItems(i).REGION_COL):=GREATEST(lItemWidth(lItems(i).REGION_COL), nWidth);
      ELSE
        lItemWidth(lItems(i).REGION_COL):=nWidth;
      END IF;
    END LOOP;
    FOR i IN 1..lItems.COUNT LOOP
      lItems(i).LABEL_WIDTH:=lLabelWidth(lItems(i).REGION_COL);
      lItems(i).ITEM_ELEMENT_WIDTH:=lItemWidth(lItems(i).REGION_COL);
    END LOOP;

    nTotalWidth:=0;
    FOR i IN 1..lLabelWidth.COUNT LOOP
      nTotalWidth:=nTotalWidth+lLabelWidth(i)+lItemWidth(i);
    END LOOP;
    IF nTotalWidth>nMaxWidth THEN
      -- Units per with
      nUnitPerWidth:=nMaxWidth/nTotalWidth;
    ELSE
      nUnitPerWidth:=1;
    END IF;

    nHeight:=0;
    nXPos:=0;
    nRowHeight:=0;
    FOR i IN 1..lItems.COUNT LOOP
      IF lItems(i).BEGINS_ON_NEW_ROW='Yes' THEN
        IF i>1 THEN
          nHeight:=nHeight+nRowHeight;
          nRowHeight:=0;
        END IF;
        nXPos:=0;
      END IF;
      nRowHeight:=GREATEST(nRowHeight, CASE WHEN lItems(i).DISPLAY_AS_CODE='NATIVE_DISPLAY_IMAGE' THEN
                                         nImageHeight
                                       ELSE
                                         nItemHeight
                                       END
                          );
      vcItemLabel:=clLabel;
      vcItemLabel:=REPLACE(vcItemLabel, '#XPOSITION#', TRUNC(nXPos*nUnitPerWidth));
      vcItemLabel:=REPLACE(vcItemLabel, '#YPOSITION#', nHeight);
      vcItemLabel:=REPLACE(vcItemLabel, '#WIDTH#', TRUNC(lItems(i).LABEL_WIDTH*nUnitPerWidth));
      vcItemLabel:=REPLACE(vcItemLabel, '#HEIGHT#', nItemHeight);
      vcItemLabel:=REPLACE(vcItemLabel, '#LABEL#', lItems(i).LABEL);
      vcItemLabel:=REPLACE(vcItemLabel, '#ALIGNMENT#', lItems(i).LABEL_ALIGNMENT);
  
      IF lItems(i).DISPLAY_AS_CODE='NATIVE_DISPLAY_IMAGE' THEN
        vcItem:=clImage;
        vcItem:=REPLACE(vcItem, '#XPOSITION#', TRUNC((nXPos+lItems(i).LABEL_WIDTH)*nUnitPerWidth));
        vcItem:=REPLACE(vcItem, '#YPOSITION#', nHeight);
        IF lItems(i).ITEM_SOURCE_TYPE='Database Column' THEN
          vcItem:=REPLACE(vcItem, '#FIELD#', '$F{' || lItems(i).ITEM_SOURCE || '}');
        ELSE
          vcItem:=REPLACE(vcItem, '#FIELD#', '');
        END IF;
      ELSE
        vcItem:=clItem;
        vcItem:=REPLACE(vcItem, '#XPOSITION#', TRUNC((nXPos+lItems(i).LABEL_WIDTH)*nUnitPerWidth));
        vcItem:=REPLACE(vcItem, '#YPOSITION#', nHeight);
        vcItem:=REPLACE(vcItem, '#WIDTH#', TRUNC(lItems(i).ITEM_ELEMENT_WIDTH*nUnitPerWidth));
        vcItem:=REPLACE(vcItem, '#HEIGHT#', nItemHeight);
        IF lItems(i).ITEM_SOURCE_TYPE='Database Column' THEN
          vcItem:=REPLACE(vcItem, '#FIELD#', '$F{' || lItems(i).ITEM_SOURCE || '}');
        ELSE
          vcItem:=REPLACE(vcItem, '#FIELD#', '');
        END IF;
        vcItem:=REPLACE(vcItem, '#ALIGNMENT#', lItems(i).ITEM_ALIGNMENT);
        
        vcItem:=REPLACE(vcItem, '#PATTERN#', FK_REVERSE_PATTERN(lItems(i).FORMAT_MASK));
      END IF;
      clForm:=clForm || vcItemLabel || vcItem;
      nXPos:=nXPos+lItems(i).LABEL_WIDTH+lItems(i).ITEM_ELEMENT_WIDTH;
    END LOOP;
    nHeight:=nHeight+nRowHeight;
    
    PR_REPLACE(clForm, '#INNERHEIGHT#', nHeight);
    DBMS_LOB.APPEND(clForm, clFormEnd);
        -- replace region-name
    IF i_vcRegionName IS NOT NULL THEN
      PR_REPLACE(clForm, '#REGION_TITLE#', i_vcRegionName);
    ELSE
      PR_REPLACE(clForm, '#REGION_TITLE#', ' ');
    END IF;

    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Region height (inner/offset) ' || nHeight || '/' || nHeightOffset);
    END IF;

    o_nHeight:=nHeight+nHeightOffset;
    o_nWidth:=nMaxWidth;
    o_clData:=clForm;
  END;
  
  -- --------------------------------------------------------------------------
  
  PROCEDURE PR_GENERATE_JRXML(i_nApplicationId        IN NUMBER,
                              i_nPageId               IN NUMBER,
                              i_vcReportName          IN VARCHAR2,
                              i_nReportTemplateId     IN NUMBER,
                              i_nBackgroundTemplateId IN NUMBER,
                              i_nTitelTemplateId      IN NUMBER,
                              i_nSummaryTemplateId    IN NUMBER,
                              i_nPageheaderTemplateId IN NUMBER,
                              i_nPagefooterTemplateId IN NUMBER,
                              i_lRegions              IN tRegionDataList) IS
                              
    TYPE tClobList IS TABLE OF CLOB INDEX BY BINARY_INTEGER;
    TYPE tVarList  IS TABLE OF tParamList INDEX BY PK_JRXML2PDF_TYPES.tIndex;

    CURSOR crOwner IS
      SELECT OWNER
        FROM APEX_APPLICATIONS 
      WHERE APPLICATION_ID=i_nApplicationId;
    

    CURSOR crPage IS
      SELECT PAGE_TITLE
        FROM APEX_APPLICATION_PAGES
       WHERE APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageId;

    recPage crPage%ROWTYPE;

    CURSOR crRegion(i_nRegionId IN NUMBER) IS
      SELECT REGION_ID, 
             REGION_NAME, 
             REGION_SOURCE,
             DECODE(SOURCE_TYPE_CODE, 'SQL_QUERY', 'REPORT',
                                      'UPDATABLE_SQL_QUERY', 'REPORT',
                                      'DYNAMIC_QUERY', 'IR',
                                 SOURCE_TYPE
                   ) SOURCE_TYPE
        FROM APEX_APPLICATION_PAGE_REGIONS
       WHERE APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageId
         AND REGION_ID=i_nRegionId;

    recRegion crRegion%ROWTYPE;
         
    CURSOR crProcess IS
      SELECT PROCESS_SOURCE
        FROM APEX_APPLICATION_PAGE_PROC
       WHERE APPLICATION_ID=i_nApplicationId
         AND PAGE_ID=i_nPageId
         AND PROCESS_TYPE_CODE='DML_FETCH_ROW';
    
    recProcess       crProcess%ROWTYPE;

    clBackgroundTemplate CLOB;
    clTitleTemplate      CLOB;
    clSummaryTemplate    CLOB;
    clPageHeaderTemplate CLOB;
    clPageFooterTemplate CLOB;
    clReportTemplate     CLOB;
    
    clReport     CLOB;                              
    rTemplate tTemplate;
    clQuery          CLOB;
    clFields         CLOB;
    clSubdatasets    CLOB;
    clDetailSections CLOB;
    lTempVars        tParamList;
    lVars            tParamList;
    lVarsByRegion    tVarList;
    vcVar            PK_JRXML2PDF_TYPES.tIndex;
    clParams         CLOB;
    iIdx             PLS_INTEGER:=0;
    lColumns         tColumnList;
    lColsByRegion    tColumnListList;
    vcOwner          PK_JRXML2PDF_TYPES.tMaxVarchar2;

    PROCEDURE PR_LOAD_TEMPLATES IS
    BEGIN
      clReportTemplate:=FK_GET_TEMPLATE(i_nReportTemplateId);
      clBackgroundTemplate:=FK_GET_TEMPLATE(i_nBackgroundTemplateId);
      clTitleTemplate:=FK_GET_TEMPLATE(i_nTitelTemplateId);
      clSummaryTemplate:=FK_GET_TEMPLATE(i_nSummaryTemplateId);
      clPageHeaderTemplate:=FK_GET_TEMPLATE(i_nPageheaderTemplateId);
      clPageFooterTemplate:=FK_GET_TEMPLATE(i_nPagefooterTemplateId);
    END;
    
    PROCEDURE PR_HANDLE_QUERIES IS
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('handle main query');
      END IF;
      
      OPEN crOwner;
      FETCH crOwner INTO vcOwner;
      CLOSE crOwner;
      
      OPEN crProcess;
      FETCH crProcess INTO recProcess;
      CLOSE crProcess;
      -- dummy-query for later Regions
      PR_HANDLE_QUERY(i_nApplicationId =>i_nApplicationId,
                      i_nPageId        =>i_nPageId,
                      i_vcSource       =>recProcess.PROCESS_SOURCE,
                      o_clQuery        =>clQuery,
                      o_clFields       =>clFields,
                      o_lVars          =>lTempVars,
                      o_lColumns       =>lColumns);
  
      vcVar:=lTempVars.FIRST;
      LOOP
        EXIT WHEN vcVar IS NULL;
        lVars(vcVar):=vcVar;
        vcVar:=lTempVars.NEXT(vcVar);
      END LOOP;
      PR_REPLACE(clReport, '#MAINQUERY#', clQuery || clFields);
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
       PK_JRXML2PDF_LOG.PR_LOG_FINE('handle region queries');
      END IF;
      
      -- Queries for reports as subdatasets
      FOR i IN 1..i_lRegions.COUNT LOOP
        OPEN crRegion(i_lRegions(i).nRegionId);
        FETCh crRegion INTO recRegion;
        CLOSE crRegion;
        IF recRegion.SOURCE_TYPE IN ('REPORT', 'IR') THEN
          PR_REPLACE(recRegion.REGION_SOURCE, '#OWNER#', vcOwner);
          IF recRegion.SOURCE_TYPE ='REPORT' THEN
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
             PK_JRXML2PDF_LOG.PR_LOG_FINE('tab region/query' || recRegion.REGION_ID || ':' || recRegion.REGION_SOURCE);
            END IF;
            PR_HANDLE_REPORT_QUERY(i_nApplicationId, 
                                   recRegion.REGION_ID, 
                                   recRegion.REGION_SOURCE,
                                   clQuery, 
                                   clFields, 
                                   lTempVars, 
                                   lColumns);
          ELSE
            PR_HANDLE_IR_QUERY(i_nApplicationId, 
                               recRegion.REGION_ID, 
                               recRegion.REGION_SOURCE,
                               clQuery, 
                               clFields, 
                               lTempVars, 
                               lColumns);
          END IF;
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('process vars');
          END IF;
          vcVar:=lTempVars.FIRST;
          LOOP
            EXIT WHEN vcVar IS NULL;
            lVars(vcVar):=vcVar;
            vcVar:=lTempVars.NEXT(vcVar);
          END LOOP;
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('build subdataset');
          END IF;
          lVarsByRegion(TO_CHAR(recRegion.REGION_ID)):=lTempVars;
          lColsByRegion(TO_CHAR(recRegion.REGION_ID)):=lColumns;
          clSubDataSets:=clSubDataSets || '    <subDataset name="' || recRegion.REGION_ID || '">' || CHR(10);
          -- parameters
          vcVar:=lTempVars.FIRST;
          LOOP
            EXIT WHEN vcVar IS NULL;
            clSubDataSets:=clSubDataSets  || '    <parameter name="' || vcVar || '" class="java.lang.String"/>' || CHR(10);
            vcVar:=lTempVars.NEXT(vcVar);
          END LOOP;
          -- Query and fields
          clSubDataSets:=clSubDataSets || clQuery || clFields;
          clSubDataSets:=clSubDataSets || '    </subDataset>' || CHR(10);
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('done with subdataset');
          END IF;
        END IF;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('put subdatasets into report');
      END IF;
      IF clSubDataSets IS NULL THEN
        PR_REPLACE(clReport, '#SUBDATASETS#', ' ');
      ELSE
        PR_REPLACE(clReport, '#SUBDATASETS#', clSubDataSets);
      END IF;
  
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('generate main parameters');
      END IF;
  
      vcVar:=lVars.FIRST;
      LOOP
        EXIT WHEN vcVar IS NULL;
        clParams:=clParams || '    <parameter name="' || vcVar || '" class="java.lang.String"/>' || CHR(10);
        vcVar:=lVars.NEXT(vcVar);
      END LOOP;
      
      IF clParams IS NOT NULL THEN
        PR_REPLACE(clReport, '#PARAMETERS#', clParams);
      ELSE
        PR_REPLACE(clReport, '#PARAMETERS#', ' ');
      END IF;
    END;
    
    PROCEDURE PR_CREATE_DETAIL_LAYOUT IS
      clLocalData  CLOB;
      nLocalHeight NUMBER;
      nHeight      NUMBER;
      nXOffset     NUMBER;
      nLocalX      NUMBER;
      PROCEDURE PR_START_BAND IS
      BEGIN
        nHeight:=0;
        nXOffset:=0;
        -- put band start into clob
        clDetailSections:=clDetailSections || '  <band height="#HEIGHT#">';
      END;
      
      PROCEDURE PR_END_BAND IS
      BEGIN
        clDetailSections:=clDetailSections || '  </band>';
        PR_REPLACE(clDetailSections,'#HEIGHT#', nHeight);
      END;

    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('process detail-sections');
      END IF;
      
      -- for each report, create a detail-section
      clDetailSections:=clDetailSections || '    <detail>' || CHR(10);
  
      FOR i IN 1..i_lRegions.COUNT LOOP
        -- check for band change
        IF i=1 THEN
          PR_START_BAND;
        ELSIF     i>1
              AND (   i_lRegions(i).vcDisplayPosition!=i_lRegions(i-1).vcDisplayPosition
                   OR i_lRegions(i).nColumn=1
                  ) THEN
          PR_START_BAND;
        END IF;

        OPEN crRegion(i_lRegions(i).nRegionId);
        FETCh crRegion INTO recRegion;
        CLOSE crRegion;
        IF recRegion.SOURCE_TYPE='REPORT' THEN
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
           PK_JRXML2PDF_LOG.PR_LOG_FINE('Create report for region ' || i_lRegions(i).nRegionId);
          END IF;
          PR_CREATE_REPORT_TABLE(recRegion.REGION_ID,
                                 recRegion.REGION_NAME,
                                 i_lRegions(i),
                                 lVarsByRegion(recRegion.REGION_ID),
                                 lColsByRegion(recRegion.REGION_ID),
                                 nXOffset,
                                 clLocalData,
                                 nLocalHeight,
                                 nLocalX);
        ELSIF recRegion.SOURCE_TYPE='IR' THEN
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
           PK_JRXML2PDF_LOG.PR_LOG_FINE('Create Interactive-Report for region ' || i_lRegions(i).nRegionId);
          END IF;
          PR_CREATE_IR_TABLE(recRegion.REGION_ID,
                             recRegion.REGION_NAME,
                             i_lRegions(i),
                             lVarsByRegion(recRegion.REGION_ID),
                             lColsByRegion(recRegion.REGION_ID),
                             nXOffset,
                             clLocalData,
                             nLocalHeight,
                             nLocalX);
        ELSE
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
           PK_JRXML2PDF_LOG.PR_LOG_FINE('Create formular layout for region ' || i_lRegions(i).nRegionId);
          END IF;
          PR_CREATE_FORM_REGION(recRegion.REGION_ID,
                                recRegion.REGION_NAME,
                                i_lRegions(i),
                                nXOffset,
                                clLocalData,
                                nLocalHeight,
                                nLocalX);
        END IF;
        clDetailSections:=clDetailSections || clLocalData;
        nHeight:=GREATEST(nHeight, nLocalHeight);
        nXOffset:=nXOffset+nLocalX;
        IF i=i_lRegions.COUNT THEN
          PR_END_BAND;
        ELSIF     i<i_lRegions.COUNT
              AND (   i_lRegions(i).vcDisplayPosition!=i_lRegions(i+1).vcDisplayPosition
                   OR i_lRegions(i+1).nColumn=1
                  ) THEN
          PR_END_BAND;
        END IF;

      END LOOP;
      clDetailSections:=clDetailSections || '    </detail>' || CHR(10);
  
  
      
      PR_REPLACE(clReport, '#DETAIL#', clDetailSections);
    END;
    
    PROCEDURE PR_CREATE_OTHER_LAYOUT IS
    BEGIN
      IF clBackgroundTemplate IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Background set');
        END IF;
        PR_REPLACE(clReport, '#BACKGROUNDREGION#', clBackgroundTemplate);
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('no Background');
        END IF;
        PR_REPLACE(clReport, '#BACKGROUNDREGION#', ' ');
      END IF;
      
      IF clTitleTemplate IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('title set');
        END IF;
        PR_REPLACE(clReport, '#TITLEREGION#', clTitleTemplate);
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('no title');
        END IF;
        PR_REPLACE(clReport, '#TITLEREGION#', ' ');
      END IF;
  
      clPageFooterTemplate:=FK_GET_TEMPLATE(i_nPagefooterTemplateId);
  
      IF clPageHeaderTemplate IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Pageheader set');
        END IF;
        PR_REPLACE(clReport, '#PAGEHEADERREGION#', clPageHeaderTemplate);
      ELSE  
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('no pageheader');
        END IF;
        PR_REPLACE(clReport, '#PAGEHEADERREGION#', ' ');
      END IF;
      
      IF clPageFooterTemplate IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Pagefooter set');
        END IF;
        PR_REPLACE(clReport, '#PAGEFOOTERREGION#', clPageFooterTemplate );
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('no pagefooter');
        END IF;
        PR_REPLACE(clReport, '#PAGEFOOTERREGION#', ' ');
      END IF;
      
      IF clSummaryTemplate IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('summary set');
        END IF;
        PR_REPLACE(clReport, '#SUMMARYREGION#', clSummaryTemplate );
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('no summary');
        END IF;
        PR_REPLACE(clReport, '#SUMMARYREGION#', ' ');
      END IF;
      -- replace Page-name
      IF recPage.PAGE_TITLE IS NOT NULL THEN
        PR_REPLACE(clReport, '#REPORT_TITLE#', recPage.PAGE_TITLE);
      ELSE
        PR_REPLACE(clReport, '#REPORT_TITLE#', ' ');
      END IF;
    END;
  BEGIN
    PK_JRXML2PDF_LOG.PR_SET_LEVEL(i_nLevel  =>PK_JRXML2PDF_LOG.LEVEL_FINE,
                                  i_nLogMode=>PK_JRXML2PDF_LOG.LOGMODE_TABLE);

    OPEN crPage;
    FETCH crPage INTO recPage;
    CLOSE crPage;

    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
     PK_JRXML2PDF_LOG.PR_LOG_FINE('Load templates');
    END IF;

    DBMS_LOB.CREATETEMPORARY(clReport, TRUE, DBMS_LOB.SESSION);
    
    -- load all templates
    PR_LOAD_TEMPLATES;

    DBMS_LOB.APPEND(clReport, clReportTemplate);

    PR_HANDLE_QUERIES;

    PR_CREATE_DETAIL_LAYOUT;

    PR_CREATE_OTHER_LAYOUT;

    MERGE INTO JRXML_REPORT_DEFINITIONS I
    USING (SELECT i_vcReportName JRD_NAME
             FROM DUAL
          ) O
    ON (I.JRD_NAME=O.JRD_NAME)
    WHEN MATCHED THEN
      UPDATE SET JRD_XML=clReport
    WHEN NOT MATCHED THEN
      INSERT (
        JRD_ID, 
        JRD_NAME, 
        JRD_XML
      ) VALUES (
        JRXML_SEQ.NEXTVAL,
        i_vcReportName,
        clReport
      );
    COMMIT;
    DBMS_LOB.FREETEMPORARY(clReport);
  END;

  PROCEDURE PR_GENERATE_JRXML(i_nApplicationId        IN NUMBER,
                              i_nPageId               IN NUMBER,
                              i_vcReportName          IN VARCHAR2,
                              i_nReportTemplateId     IN NUMBER,
                              i_nBackgroundTemplateId IN NUMBER,
                              i_nTitelTemplateId      IN NUMBER,
                              i_nSummaryTemplateId    IN NUMBER,
                              i_nPageheaderTemplateId IN NUMBER,
                              i_nPagefooterTemplateId IN NUMBER) IS
    lRegions PK_JRXML2PDF_APEX2JRXML.tRegionDataList;
    CURSOR crRegions IS
      SELECT C001,
             C005,
             C006,
             C011,
             C012,
             C013,
             C014,
             C015
        FROM APEX_COLLECTIONS
       WHERE COLLECTION_NAME='JRXML'
         AND C004='Y';
    
  BEGIN
    -- Transfer collection-Data INTO Region-array
    OPEN crRegions;
    FETCH crRegions
    BULK COLLECT INTO lRegions;
    CLOSE crRegions;
    
    -- call generator
    PR_GENERATE_JRXML(i_nApplicationId        =>i_nApplicationId,
                      i_nPageId               =>i_nPageId,
                      i_vcReportName          =>i_vcReportName,
                      i_nReportTemplateId     =>i_nReportTemplateId,
                      i_nBackgroundTemplateId =>i_nBackgroundTemplateId,
                      i_nTitelTemplateId      =>i_nTitelTemplateId,
                      i_nSummaryTemplateId    =>i_nSummaryTemplateId,
                      i_nPageheaderTemplateId =>i_nPageheaderTemplateId,
                      i_nPagefooterTemplateId =>i_nPagefooterTemplateId,
                      i_lRegions              =>lRegions);
  END;

END;
/

