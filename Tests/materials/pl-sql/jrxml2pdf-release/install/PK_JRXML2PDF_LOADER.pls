create or replace
PACKAGE PK_JRXML2PDF_LOADER AUTHID CURRENT_USER IS
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

  $name    PK_JRXML2PDF_LOADER

  $created 16.10.2012

  $author  Andreas Weiden

  $desc    Package with procedures aand functions to load a report-definition from
           the table JRXML_REPORT_DEFINITIONS into the in-momory-structure of PL-jrxml2pdf
           for further processing

  $version 1.0.0.0 16.10.2012 Weiden
           Out-sourced from PK_JRXML2PDF_REPGEN

  $version 1.1.0.0 31.10.2012 Weiden
           Added support for barcharts and piecharts

  $version 1.1.0.1 10.12.2012 Weiden
           Added support for category-linecharts

  $version 1.1.1.1 23.12.2012 Weiden
           Enhancements and bugfixes for tables:
           - groups in subdatasets are now loaded
           - groups in tables are not loaded
           - corrected handling of columngGroups without columns
           - corrected handling for footer objects

  $version 1.1.1.1 04.01.2013 Weiden
           Added support for stacked barcharts (2D and 3D)

  $version 1.1.1.2 05.02.2013 Weiden
           Bugfixes for ReportLocale in Table-subreports

  $version 1.1.1.3 15.02.2013 Weiden
           Bugfix: Set opaque to No for rects when Mode=Transparent

  $version 1.1.2.3 11.03.2013 Weiden
           Enhance: Add loading of HeaderCell for crosstabs
*/

/**

  $name    FK_LOAD_REPORT

  $created 16.10.2012

  $author  Andreas Weiden

  $desc    Loads the report with the given name into memory and returns the result as record

  $param   i_vcReportName Name of the report to be loaded, must match a record in JRXML_REPORT_DEFINITIONS

  $param   i_lParamList   List of parameters and values to pass to the report

  $return  record with report-definition

  $version 1.0.0.0 16.10.2012 Weiden
           initial version

*/
  FUNCTION FK_LOAD_REPORT (i_vcReportName IN VARCHAR2,
                           i_lParamList   IN PK_JRXML2PDF_TYPES.tParamList)
  RETURN PK_JRXML2PDF_TYPES.tReport;

/**

  $name    PR_CLEAR_CACHE

  $created 16.10.2012

  $author  Andreas Weiden

  $desc    Clears the internal report-cache

  $version 1.0.0.0 16.10.2012 Weiden
           initial version

*/
  PROCEDURE PR_CLEAR_CACHE;

/**

  $name    PR_EXECUTE_QUERY

  $created 16.10.2012

  $author  Andreas Weiden

  $desc    Parses and executes the query of the given report

  $param   io_rReport     report-record with the report-definition

  $param   i_lParams      List of parameters and values to pass to the report

  $version 1.0.0.0 16.10.2012 Weiden
           initial version

*/
  PROCEDURE PR_EXECUTE_QUERY(io_rReport IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                             i_lParams  IN PK_JRXML2PDF_TYPES.tParamList);

END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_LOADER IS

  lReportCache         PK_JRXML2PDF_TYPES.tReportCache;

  FUNCTION FK_LOAD_BAND(io_rReport              IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                        i_oBand                 IN      XMLTYPE,
                        i_vcTyp                 IN      PK_JRXML2PDF_TYPES.tAttribute,
                        i_nHeight               IN      NUMBER,
                        i_vcPrintWhenExpression IN      PK_JRXML2PDF_TYPES.tExpression,
                        i_vcSplitType           IN      PK_JRXML2PDF_TYPES.tSplitType)
  RETURN PK_JRXML2PDF_TYPES.tBand;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_MAKE_NUM(i_vcValue IN VARCHAR2)
  RETURN NUMBER IS
  BEGIN
    RETURN TO_NUMBER(i_vcValue, 'FM9999999D99', 'NLS_NUMERIC_CHARACTERS=.,');
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_DATATYPE_FROM_CLASS(i_vcClass IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tDatatype IS
    vcResult PK_JRXML2PDF_TYPES.tDatatype;
  BEGIN
    IF i_vcClass IN ('java.lang.Integer',
                     'java.lang.Byte',
                     'java.lang.Double',
                     'java.lang.Float',
                     'java.lang.Long',
                     'java.lang.Short',
                     'java.lang.Number',
                     'java.math.BigDecimal') THEN
      vcResult:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    ELSIF i_vcClass IN ('java.sql.Timestamp',
                        'java.sql.Time',
                        'java.util.Date') THEN
      vcResult:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
    ELSE
      vcResult:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
    END IF;
    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_PARAMLIST(i_xmlParams IN XMLTYPE)
  RETURN PK_JRXML2PDF_TYPES.tParamList IS
    CURSOR crParams IS
      SELECT PARAM_NAME,
             EXPRESSION
        FROM XMLTABLE('/subreport/subreportParameter' PASSING i_xmlParams
                                                      COLUMNS PARAM_NAME VARCHAR2(255) PATH './@name',
                                                              EXPRESSION VARCHAR2(255) PATH './subreportParameterExpression'
                     );
    lParams PK_JRXML2PDF_TYPES.tParamList;
    iPos    PLS_INTEGER:=0;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Create paramlist');
    END IF;
    FOR rec IN crParams LOOP
      iPos:=iPos+1;
      lParams(iPos).vcName:=rec.PARAM_NAME;
      lParams(iPos).vcValue:=rec.EXPRESSION;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Added Parameter ' || lParams(iPos).vcName ||':' || lParams(iPos).vcValue);
      END IF;
    END LOOP;
    RETURN lParams;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_TABLE_PARAMLIST(i_xmlParams IN XMLTYPE)
  RETURN PK_JRXML2PDF_TYPES.tParamList IS
    CURSOR crParams IS
      SELECT PARAM_NAME,
             EXPRESSION
        FROM XMLTABLE('/datasetRun/datasetParameter' PASSING i_xmlParams
                                                     COLUMNS PARAM_NAME VARCHAR2(255) PATH './@name',
                                                             EXPRESSION VARCHAR2(255) PATH './datasetParameterExpression'
                     );
    lParams PK_JRXML2PDF_TYPES.tParamList;
    iPos    PLS_INTEGER:=0;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Create paramlist for table');
    END IF;
    FOR rec IN crParams LOOP
      iPos:=iPos+1;
      lParams(iPos).vcName:=rec.PARAM_NAME;
      lParams(iPos).vcValue:=rec.EXPRESSION;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Added Parameter ' || lParams(iPos).vcName ||':' || lParams(iPos).vcValue);
      END IF;
    END LOOP;
    RETURN lParams;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_EXECUTE_QUERY(io_rReport IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                             i_lParams  IN PK_JRXML2PDF_TYPES.tParamList) IS
   iColCount    PLS_INTEGER;
   rec          DBMS_SQL.DESC_REC;
   iRecord      PLS_INTEGER:=0;
   nRows        NUMBER;
   lQueryResult PK_JRXML2PDF_TYPES.tQueryresult;
   rEntry       PK_JRXML2PDF_TYPES.tDataEntry;
   vcQuery      PK_JRXML2PDF_TYPES.tMaxVarchar2:=REPLACE(REPLACE(io_rReport.vcQuery, '$P{', ':'), '}', '');

   nNumberDummy NUMBER;
   rQuery       PK_JRXML2PDF_TYPES.tQuery;
  BEGIN
    rQuery.iCursor:=DBMS_SQL.OPEN_CURSOR;
    rQuery.bEOF:=FALSE;
    rQuery.nRecordPosition:=0;
    rQuery.nRecordRead:=0;
    rQuery.bTreatLastAsCurrent:=FALSE;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Parse query');
    END IF;

    DBMS_SQL.PARSE(rQuery.iCursor, vcQuery, DBMS_SQL.NATIVE);

    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Query:' || vcQuery);
    END IF;

    -- Bind Variables from arameters
    FOR i IN 1..i_lParams.COUNT LOOP
      IF INSTR(vcQuery, ':' || i_lParams(i).vcName)>0 THEN
        DBMS_SQL.BIND_VARIABLE(rQuery.iCursor, i_lParams(i).vcName, i_lParams(i).vcValue);
      END IF;
    END LOOP;

    DBMS_SQL.DESCRIBE_COLUMNS (rQuery.iCursor, iColCount, rQuery.lDescTab);
    FOR i IN 1..rQuery.lDescTab.COUNT LOOP
      IF rQuery.lDescTab(i).col_type IN     (PK_JRXML2PDF_TYPES.DBMS_SQL_VARCHAR2_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_CHAR_TYPE
                                            ) THEN
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, 'X', rQuery.lDescTab(i).COL_MAX_LEN);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is varchar: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type =      PK_JRXML2PDF_TYPES.DBMS_SQL_CLOB_TYPE THEN
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, EMPTY_CLOB);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is clob: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_NUMBER_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_FLOAT_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_BOUBLE_TYPE
                                            ) THEN
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, TO_NUMBER(NULL));
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is number: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_DATE_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TIMESTAMP_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TSTMP_WITH_TZ_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_YEAR_TO_MONTH_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_IV_DAY_TO_SECOND_TYPE,
                                             PK_JRXML2PDF_TYPES.DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE
                                            ) THEN
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, TO_DATE(NULL));
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is date: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      ELSIF rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_BLOB_TYPE
                                            ) THEN
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, EMPTY_BLOB);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is blob: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      ELSE
        DBMS_SQL.DEFINE_COLUMN(rQuery.iCursor, i, rQuery.lDescTab(i).COL_NAME, rQuery.lDescTab(i).COL_MAX_LEN);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Column is undefined: ' || rQuery.lDescTab(i).COL_NAME);
        END IF;
      END IF;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Execute query');
    END IF;

    nRows := DBMS_SQL.EXECUTE(rQuery.iCursor);

    io_rReport.rQuery:=rQuery;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LOAD_CROSSTAB(io_rReport           IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                            i_oCrosstab          IN XMLTYPE,
                            i_vcSubdataset       IN PK_JRXML2PDF_TYPES.tName,
                            i_vcRepeatRowHeaders IN PK_JRXML2PDF_TYPES.tYesNo,
                            i_vcRepeatColHeaders IN PK_JRXML2PDF_TYPES.tYesNo,
                            i_nColBreakOffset    IN NUMBER
                           )
  RETURN PK_JRXML2PDF_TYPES.tCrossTab IS
    CURSOR crDetails IS
      SELECT CASE WHEN ROWGROUP IS NOT NULL THEN
               'ROWGROUP'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             WHEN MEASURE IS NOT NULL THEN
               'MEASURE'
             WHEN CELL IS NOT NULL THEN
               'CELL'
             WHEN HEADERCELL IS NOT NULL THEN
               'HEADERCELL'
             END ROWTYPE,
             ROWGROUP,
             COLGROUP,
             MEASURE,
             CELL,
             HEADERCELL
        FROM XMLTABLE('/crosstab/*' PASSING i_oCrosstab
                           COLUMNS ROWGROUP            XMLTYPE        PATH './../rowGroup',
                                   COLGROUP            XMLTYPE        PATH './../columnGroup',
                                   MEASURE             XMLTYPE        PATH './../measure',
                                   CELL                XMLTYPE        PATH './../crosstabCell',
                                   HEADERCELL          XMLTYPE        PATH './../crosstabHeaderCell'
                     );
    nColStart          NUMBER:=0;
    rCrosstab          PK_JRXML2PDF_TYPES.tCrosstab;
    iPos               PLS_INTEGER;
    iBandPos           PLS_INTEGER;
    vcStyle            PK_JRXML2PDF_TYPES.tStyleName;
    rCell              PK_JRXML2PDF_TYPES.tCrossTabCell;
    vcIndex            PK_JRXML2PDF_TYPES.tCrosstabCellName;

    FUNCTION FK_MAKE_BAND_FROM_CELL(i_oParts           IN XMLTYPE,
                                    i_nWidth           IN NUMBER,
                                    i_nHeight          IN NUMBER,
                                    i_vcBoxTop         IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                                    i_vcBoxBottom      IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                                    i_vcBoxLeft        IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                                    i_vcBoxRight       IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                                    i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                                    i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                                    i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                                    i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                                    i_vcBackColor      IN PK_JRXML2PDF_TYPES.tColor,
                                    i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo,
                                    i_vcStyle          IN PK_JRXML2PDF_TYPES.tStyleName
                                   )
    RETURN NUMBER IS
      iBandPos PLS_INTEGER;
      iPos     PLS_INTEGER;
      rObject  PK_JRXML2PDF_TYPES.tObject;
    BEGIN
      -- create new band-entry
      iBandPos:=io_rReport.lBands.COUNT+1;
      io_rReport.lBands(iBandPos).nX:=NULL;

      iPos:=io_rReport.lBands.COUNT+1;
      -- Mark record as used because of recursion
      io_rReport.lBands(iPos).nX:=NULL;
      -- load the band
      io_rReport.lBands(iPos):=FK_LOAD_BAND(io_rReport             =>io_rReport,
                                            i_oBand                =>i_oParts,
                                            i_vcTyp                =>'X-RowReg',
                                            i_nHeight              =>i_nHeight,
                                            i_vcPrintWhenExpression=>NULL,
                                            i_vcSplitType          =>PK_JRXML2PDF_TYPES.PREVENT
                                           );
      -- set band-properties
      io_rReport.lBands(iPos).nX               :=0;
      io_rReport.lBands(iPos).nY               :=0;
      io_rReport.lBands(iPos).nWidth           :=i_nWidth;
      io_rReport.lBands(iPos).nHeight          :=i_nHeight;
      io_rReport.lBands(iPos).nBoxTop          :=FK_MAKE_NUM(i_vcboxTop);
      io_rReport.lBands(iPos).nBoxLeft         :=FK_MAKE_NUM(i_vcBoxLeft);
      io_rReport.lBands(iPos).nBoxBottom       :=FK_MAKE_NUM(i_vcBoxBottom);
      io_rReport.lBands(iPos).nBoxRight        :=FK_MAKE_NUM(i_vcBoxRight);
      io_rReport.lBands(iPos).vcBoxTopColor    :=i_vcBoxTopColor;
      io_rReport.lBands(iPos).vcBoxLeftColor   :=i_vcBoxLeftColor;
      io_rReport.lBands(iPos).vcBoxBottomColor :=i_vcBoxBottomColor;
      io_rReport.lBands(iPos).vcBoxRightColor  :=i_vcBoxRightColor;
      io_rReport.lBands(iPos).vcStyle          :=i_vcStyle;
      io_rReport.lBands(iPos).vcBGColor        :=i_vcBackColor;
      io_rReport.lBands(iPos).vcOpaque         :=i_vcOpaque;
      io_rReport.lBands(iPos).vcWhenExpression :=NULL;

      rObject.nType:=PK_JRXML2PDF_TYPES.SUBFRAME;
      rObject.nPosition:=iPos;
      io_rReport.lBands(iBandPos).lObjects(io_rReport.lBands(iBandPos).lObjects.COUNT+1):=rObject;
      RETURN iPos;
    END;

    FUNCTION FK_LOAD_ROW(i_oXml IN XMLTYPE)
    RETURN PK_JRXML2PDF_TYPES.tCrosstabRow IS
      CURSOR crRow IS
        SELECT NAME,
               WIDTH,
               TOTAL_POSITION,
               HEADER_POSITION,
               EXPRESSION,
               ORDERBY,
               CLASS,
               XMLELEMENT("element", HEADER)    HEADER,
               NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
               NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
               NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
               NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
               NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
               NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
               NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
               NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
               BACKCOLOR,
               CASE WHEN MODUS='Opaque' THEN
                 'Y'
               END OPAQUE,
               STYLE,
               XMLELEMENT("element", T_HEADER)      T_HEADER,
               NVL(T_BOX_TOP_COLOR, T_BOX_COLOR)    T_BOX_TOP_COLOR,
               NVL(T_BOX_BOTTOM_COLOR, T_BOX_COLOR) T_BOX_BOTTOM_COLOR,
               NVL(T_BOX_LEFT_COLOR, T_BOX_COLOR)   T_BOX_LEFT_COLOR,
               NVL(T_BOX_RIGHT_COLOR, T_BOX_COLOR)  T_BOX_RIGHT_COLOR,
               NVL(T_BOX_TOP, T_BOX_LINEWIDTH)      T_BOX_TOP,
               NVL(T_BOX_BOTTOM, T_BOX_LINEWIDTH)   T_BOX_BOTTOM,
               NVL(T_BOX_LEFT, T_BOX_LINEWIDTH)     T_BOX_LEFT,
               NVL(T_BOX_RIGHT, T_BOX_LINEWIDTH)    T_BOX_RIGHT,
               T_BACKCOLOR,
               CASE WHEN T_MODUS='Opaque' THEN
                 'Y'
               END T_OPAQUE,
               T_STYLE
          FROM XMLTABLE('/rowGroup' PASSING i_oXml
                                    COLUMNS NAME               VARCHAR2(255)  PATH './@name',
                                            WIDTH              VARCHAR2(20)   PATH './@width',
                                            TOTAL_POSITION     VARCHAR2(20)   PATH './@totalPosition',
                                            HEADER_POSITION    VARCHAR2(20)   PATH './@headerPosition',
                                            EXPRESSION         VARCHAR2(4000) PATH './bucket/bucketExpression',
                                            ORDERBY            VARCHAR2(20)   PATH './bucket/@order',
                                            CLASS              VARCHAR2(20)   PATH './bucket/@class',
                                            HEADER             XMLTYPE        PATH './crosstabRowHeader/cellContents/*',
                                            BOX_TOP_COLOR      VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/topPen/@lineColor',
                                            BOX_BOTTOM_COLOR   VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/bottomPen/@lineColor',
                                            BOX_LEFT_COLOR     VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/leftPen/@lineColor',
                                            BOX_RIGHT_COLOR    VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/rightPen/@lineColor',
                                            BOX_TOP            VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/topPen/@lineWidth',
                                            BOX_BOTTOM         VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/bottomPen/@lineWidth',
                                            BOX_LEFT           VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/leftPen/@lineWidth',
                                            BOX_RIGHT          VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/rightPen/@lineWidth',
                                            BOX_LINEWIDTH      VARCHAR2(10)   PATH './crosstabRowHeader/cellContents/box/pen/@lineWidth',
                                            BOX_COLOR          VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/box/pen/@lineColor',
                                            BACKCOLOR          VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/@backcolor',
                                            MODUS              VARCHAR2(20)   PATH './crosstabRowHeader/cellContents/@mode',
                                            STYLE              VARCHAR2(255)  PATH './crosstabRowHeader/cellContents/@style',
                                            T_HEADER           XMLTYPE        PATH './crosstabTotalRowHeader/cellContents/*',
                                            T_BOX_TOP_COLOR    VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/topPen/@lineColor',
                                            T_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/bottomPen/@lineColor',
                                            T_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/leftPen/@lineColor',
                                            T_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/rightPen/@lineColor',
                                            T_BOX_TOP          VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/topPen/@lineWidth',
                                            T_BOX_BOTTOM       VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/bottomPen/@lineWidth',
                                            T_BOX_LEFT         VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/leftPen/@lineWidth',
                                            T_BOX_RIGHT        VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/rightPen/@lineWidth',
                                            T_BOX_LINEWIDTH    VARCHAR2(10)   PATH './crosstabTotalRowHeader/cellContents/box/pen/@lineWidth',
                                            T_BOX_COLOR        VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/box/pen/@lineColor',
                                            T_BACKCOLOR        VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/@backcolor',
                                            T_MODUS            VARCHAR2(20)   PATH './crosstabTotalRowHeader/cellContents/@mode',
                                            T_STYLE            VARCHAR2(255)  PATH './crosstabTotalRowHeader/cellContents/@style'
                       );
      rec  crRow%ROWTYPE;
      rRow PK_JRXML2PDF_TYPES.tCrosstabRow;
    BEGIN
      -- extract row-data
      OPEN crRow;
      FETCh crRow INTO rec;
      CLOSE crRow;

      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Load row ' || rec.NAME);
      END IF;

      -- set properties
      rRow.vcName            :=rec.NAME;
      rRow.nWidth            :=FK_MAKE_NUM(rec.WIDTH);
      rRow.vcBucketExpression:=rec.EXPRESSION;
      rRow.vcOrderBy         :=NVL(rec.ORDERBY, PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING);
      rRow.vcTotalPosition   :=rec.TOTAL_POSITION;
      rRow.vcDatatype        :=FK_DATATYPE_FROM_CLASS(rec.CLASS);
      rRow.vcHeaderPosition  :=LOWER(NVL(rec.HEADER_POSITION, PK_JRXML2PDF_TYPES.TOP_ALIGN));
      -- Special handling of Stretch
      IF rRow.vcHeaderPosition=PK_JRXML2PDF_TYPES.STRETCH_ALIGN THEN
        rRow.vcHeaderPosition:=PK_JRXML2PDF_TYPES.STRETCH_Y_ALIGN;
      END IF;
      IF rec.HEADER IS NOT NULL THEN
        -- load header-band
        rRow.nHeaderPos     :=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.HEADER,
                                                     i_nWidth           =>rec.WIDTH,
                                                     i_nHeight          =>20,
                                                     i_vcBoxTop         =>rec.BOX_TOP,
                                                     i_vcBoxBottom      =>rec.BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.BOX_LEFT,
                                                     i_vcBoxRight       =>rec.BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.BACKCOLOR,
                                                     i_vcOpaque         =>rec.OPAQUE,
                                                     i_vcStyle          =>rec.STYLE
                                                    );
      END IF;
      IF rec.T_HEADER IS NOT NULL THEN
        -- load total-band
        rRow.nTotalHeaderPos:=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.T_HEADER,
                                                     i_nWidth           =>rec.WIDTH,
                                                     i_nHeight          =>20,
                                                     i_vcBoxTop         =>rec.T_BOX_TOP,
                                                     i_vcBoxBottom      =>rec.T_BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.T_BOX_LEFT,
                                                     i_vcBoxRight       =>rec.T_BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.T_BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.T_BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.T_BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.T_BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.T_BACKCOLOR,
                                                     i_vcOpaque         =>rec.T_OPAQUE,
                                                     i_vcStyle          =>rec.T_STYLE
                                                    );
      END IF;
      RETURN rRow;
    END;

    FUNCTION FK_LOAD_COLUMN(i_oXml IN XMLTYPE)
    RETURN PK_JRXML2PDF_TYPES.tCrosstabRow IS
      CURSOR crColumn IS
        SELECT NAME,
               HEIGHT,
               EXPRESSION,
               ORDERBY,
               CLASS,
               TOTAL_POSITION,
               HEADER_POSITION,
               XMLELEMENT("element", HEADER)    HEADER,
               NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
               NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
               NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
               NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
               NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
               NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
               NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
               NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
               BACKCOLOR,
               CASE WHEN MODUS='Opaque' THEN
                 'Y'
               END OPAQUE,
               STYLE,
               XMLELEMENT("element", T_HEADER)      T_HEADER,
               NVL(T_BOX_TOP_COLOR, T_BOX_COLOR)    T_BOX_TOP_COLOR,
               NVL(T_BOX_BOTTOM_COLOR, T_BOX_COLOR) T_BOX_BOTTOM_COLOR,
               NVL(T_BOX_LEFT_COLOR, T_BOX_COLOR)   T_BOX_LEFT_COLOR,
               NVL(T_BOX_RIGHT_COLOR, T_BOX_COLOR)  T_BOX_RIGHT_COLOR,
               NVL(T_BOX_TOP, T_BOX_LINEWIDTH)      T_BOX_TOP,
               NVL(T_BOX_BOTTOM, T_BOX_LINEWIDTH)   T_BOX_BOTTOM,
               NVL(T_BOX_LEFT, T_BOX_LINEWIDTH)     T_BOX_LEFT,
               NVL(T_BOX_RIGHT, T_BOX_LINEWIDTH)    T_BOX_RIGHT,
               T_BACKCOLOR,
               CASE WHEN T_MODUS='Opaque' THEN
                 'Y'
               END T_OPAQUE,
               T_STYLE
          FROM XMLTABLE('/columnGroup'  PASSING i_oXml
                                        COLUMNS NAME               VARCHAR2(255)  PATH './@name',
                                                HEIGHT             VARCHAR2(20)   PATH './@height',
                                                TOTAL_POSITION     VARCHAR2(20)   PATH './@totalPosition',
                                                HEADER_POSITION    VARCHAR2(20)   PATH './@headerPosition',
                                                EXPRESSION         VARCHAR2(4000) PATH './bucket/bucketExpression',
                                                CLASS              VARCHAR2(20)   PATH './bucket/@class',
                                                ORDERBY            VARCHAR2(20)   PATH './bucket/@order',
                                                HEADER             XMLTYPE        PATH './crosstabColumnHeader/cellContents/*',
                                                BOX_TOP_COLOR      VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/topPen/@lineColor',
                                                BOX_BOTTOM_COLOR   VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/bottomPen/@lineColor',
                                                BOX_LEFT_COLOR     VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/leftPen/@lineColor',
                                                BOX_RIGHT_COLOR    VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/rightPen/@lineColor',
                                                BOX_TOP            VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/topPen/@lineWidth',
                                                BOX_BOTTOM         VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/bottomPen/@lineWidth',
                                                BOX_LEFT           VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/leftPen/@lineWidth',
                                                BOX_RIGHT          VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/rightPen/@lineWidth',
                                                BOX_LINEWIDTH      VARCHAR2(10)   PATH './crosstabColumnHeader/cellContents/box/pen/@lineWidth',
                                                BOX_COLOR          VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/box/pen/@lineColor',
                                                BACKCOLOR          VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/@backcolor',
                                                MODUS              VARCHAR2(20)   PATH './crosstabColumnHeader/cellContents/@mode',
                                                STYLE              VARCHAR2(255)  PATH './crosstabColumnHeader/cellContents/@style',
                                                T_HEADER           XMLTYPE        PATH './crosstabTotalColumnHeader/cellContents/*',
                                                T_BOX_TOP_COLOR    VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/topPen/@lineColor',
                                                T_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/bottomPen/@lineColor',
                                                T_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/leftPen/@lineColor',
                                                T_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/rightPen/@lineColor',
                                                T_BOX_TOP          VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/topPen/@lineWidth',
                                                T_BOX_BOTTOM       VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/bottomPen/@lineWidth',
                                                T_BOX_LEFT         VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/leftPen/@lineWidth',
                                                T_BOX_RIGHT        VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/rightPen/@lineWidth',
                                                T_BOX_LINEWIDTH    VARCHAR2(10)   PATH './crosstabTotalColumnHeader/cellContents/box/pen/@lineWidth',
                                                T_BOX_COLOR        VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/box/pen/@lineColor',
                                                T_BACKCOLOR        VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/@backcolor',
                                                T_MODUS            VARCHAR2(20)   PATH './crosstabTotalColumnHeader/cellContents/@mode',
                                                T_STYLE            VARCHAR2(255)  PATH './crosstabTotalColumnHeader/cellContents/@style'
                       );
      rec  crColumn%ROWTYPE;
      rRow PK_JRXML2PDF_TYPES.tCrosstabRow;
    BEGIN
      -- extract column-data
      OPEN crColumn;
      FETCh crColumn INTO rec;
      CLOSE crColumn;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Load column ' || rec.NAME);
      END IF;
      -- set properties
      rRow.vcName            :=rec.NAME;
      rRow.nHeight           :=FK_MAKE_NUM(rec.HEIGHT);
      rRow.vcBucketExpression:=rec.EXPRESSION;
      rRow.vcOrderBy         :=NVL(rec.ORDERBY, PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING);
      rRow.vcTotalPosition   :=rec.TOTAL_POSITION;
      rRow.vcDatatype        :=FK_DATATYPE_FROM_CLASS(rec.CLASS);
      rRow.vcHeaderPosition  :=LOWER(NVL(rec.HEADER_POSITION, PK_JRXML2PDF_TYPES.CENTER_ALIGN));
      IF rec.HEADER IS NOT NULL THEN
        -- load header-band
        rRow.nHeaderPos     :=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.HEADER,
                                                     i_nWidth           =>20,
                                                     i_nHeight          =>rec.HEIGHT,
                                                     i_vcBoxTop         =>rec.BOX_TOP,
                                                     i_vcBoxBottom      =>rec.BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.BOX_LEFT,
                                                     i_vcBoxRight       =>rec.BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.BACKCOLOR,
                                                     i_vcOpaque         =>rec.OPAQUE,
                                                     i_vcStyle          =>rec.STYLE
                                                    );
      END IF;
      IF rec.T_HEADER IS NOT NULL THEN
        -- load total-band
        rRow.nTotalHeaderPos:=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.T_HEADER,
                                                     i_nWidth           =>20,
                                                     i_nHeight          =>rec.HEIGHT,
                                                     i_vcBoxTop         =>rec.T_BOX_TOP,
                                                     i_vcBoxBottom      =>rec.T_BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.T_BOX_LEFT,
                                                     i_vcBoxRight       =>rec.T_BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.T_BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.T_BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.T_BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.T_BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.T_BACKCOLOR,
                                                     i_vcOpaque         =>rec.T_OPAQUE,
                                                     i_vcStyle          =>rec.T_STYLE
                                                    );
      END IF;
      RETURN rRow;
    END;

    FUNCTION FK_LOAD_MEASURE(i_oXml IN XMLTYPE)
    RETURN PK_JRXML2PDF_TYPES.tCrossTabMeasure IS
      CURSOR crMeasure IS
        SELECT NAME,
               CALCULATION,
               MEASURE_EXPRESSION,
               CLASS
          FROM XMLTABLE('/measure' PASSING i_oXml
                                        COLUMNS NAME               VARCHAR2(255)  PATH './@name',
                                                CALCULATION        VARCHAR2(20)   PATH './@calculation',
                                                MEASURE_EXPRESSION VARCHAR2(4000) PATH './measureExpression',
                                                CLASS              VARCHAR2(255)  PATH './@class'
                       );
      rec      crMeasure%ROWTYPE;
      rMeasure PK_JRXML2PDF_TYPES.tCrossTabMeasure;
    BEGIN
      -- extract measure-data
      OPEN crMeasure;
      FETCh crMeasure INTO rec;
      CLOSE crMeasure;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Load measure ' || rec.NAME);
      END IF;
      -- set properties
      rMeasure.vcName          :=rec.NAME;
      rMeasure.vcCalculation   :=NVL(rec.CALCULATION, PK_JRXML2PDF_TYPES.CALCULATION_NOTHING);
      rMeasure.vcExpression    :=rec.MEASURE_EXPRESSION;
      rMeasure.vcDatatype      :=FK_DATATYPE_FROM_CLASS(rec.CLASS);
      RETURN rMeasure;
    END;

    FUNCTION FK_LOAD_CELL(i_oXml IN XMLTYPE)
    RETURN PK_JRXML2PDF_TYPES.tCrossTabCell IS
      CURSOR crCell IS
        SELECT ROW_TOTAL_GROUP,
               COL_TOTAL_GROUP,
               WIDTH,
               HEIGHT,
               XMLELEMENT("element", CELL) CELL,
               NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
               NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
               NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
               NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
               NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
               NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
               NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
               NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
               BACKCOLOR,
               CASE WHEN MODUS='Opaque' THEN
                 'Y'
               END OPAQUE,
               STYLE
          FROM XMLTABLE('/crosstabCell' PASSING i_oXml
                                        COLUMNS NAME               VARCHAR2(255)  PATH './@name',
                                                WIDTH              VARCHAR2(20)   PATH './@width',
                                                HEIGHT             VARCHAR2(20)   PATH './@height',
                                                CELL               XMLTYPE        PATH './cellContents/*',
                                                BOX_TOP_COLOR      VARCHAR2(20)   PATH './cellContents/box/topPen/@lineColor',
                                                BOX_BOTTOM_COLOR   VARCHAR2(20)   PATH './cellContents/box/bottomPen/@lineColor',
                                                BOX_LEFT_COLOR     VARCHAR2(20)   PATH './cellContents/box/leftPen/@lineColor',
                                                BOX_RIGHT_COLOR    VARCHAR2(20)   PATH './cellContents/box/rightPen/@lineColor',
                                                BOX_TOP            VARCHAR2(20)   PATH './cellContents/box/topPen/@lineWidth',
                                                BOX_BOTTOM         VARCHAR2(20)   PATH './cellContents/box/bottomPen/@lineWidth',
                                                BOX_LEFT           VARCHAR2(20)   PATH './cellContents/box/leftPen/@lineWidth',
                                                BOX_RIGHT          VARCHAR2(20)   PATH './cellContents/box/rightPen/@lineWidth',
                                                BOX_LINEWIDTH      VARCHAR2(10)   PATH './cellContents/box/pen/@lineWidth',
                                                BOX_COLOR          VARCHAR2(20)   PATH './cellContents/box/pen/@lineColor',
                                                BACKCOLOR          VARCHAR2(20)   PATH './cellContents/@backcolor',
                                                MODUS              VARCHAR2(20)   PATH './cellContents/@mode',
                                                STYLE              VARCHAR2(255)  PATH './cellContents/@style',
                                                ROW_TOTAL_GROUP    VARCHAR2(255)  PATH './@rowTotalGroup',
                                                COL_TOTAL_GROUP    VARCHAR2(255)  PATH './@columnTotalGroup'
                       );
      rec  crCell%ROWTYPE;
      rCell PK_JRXML2PDF_TYPES.tCrossTabCell;
    BEGIN
      -- extract cell-data
      OPEN crCell;
      FETCh crCell INTO rec;
      CLOSE crCell;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Load cell ' || rec.ROW_TOTAL_GROUP ||'_' || rec.COL_TOTAL_GROUP);
      END IF;
      -- set properties
      rCell.vcName            :=rec.ROW_TOTAL_GROUP ||'_' || rec.COL_TOTAL_GROUP;
      rCell.vcRowGroup        :=rec.ROW_TOTAL_GROUP;
      rCell.vcColgroup        :=rec.COL_TOTAL_GROUP;
      rCell.nWidth            :=FK_MAKE_NUM(rec.WIDTH);
      rCell.nHeight           :=FK_MAKE_NUM(rec.HEIGHT);
      IF rec.CELL IS NOT NULL THEN
        -- load cell-band
        rCell.nCellPos      :=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.CELL,
                                                     i_nWidth           =>rCell.nWidth,
                                                     i_nHeight          =>rCell.nHeight,
                                                     i_vcBoxTop         =>rec.BOX_TOP,
                                                     i_vcBoxBottom      =>rec.BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.BOX_LEFT,
                                                     i_vcBoxRight       =>rec.BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.BACKCOLOR,
                                                     i_vcOpaque         =>rec.OPAQUE,
                                                     i_vcStyle          =>rec.STYLE
                                                    );
      END IF;
      RETURN rCell;
    END;

    FUNCTION FK_LOAD_HEADERCELL(i_oXml IN XMLTYPE)
    RETURN PK_JRXML2PDF_TYPES.tCrossTabCell IS
      CURSOR crCell IS
        SELECT WIDTH,
               HEIGHT,
               XMLELEMENT("element", CELL) CELL,
               NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
               NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
               NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
               NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
               NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
               NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
               NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
               NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
               BACKCOLOR,
               CASE WHEN MODUS='Opaque' THEN
                 'Y'
               END OPAQUE,
               STYLE
          FROM XMLTABLE('/crosstabHeaderCell' PASSING i_oXml
                                        COLUMNS NAME               VARCHAR2(255)  PATH './@name',
                                                WIDTH              VARCHAR2(20)   PATH './@width',
                                                HEIGHT             VARCHAR2(20)   PATH './@height',
                                                CELL               XMLTYPE        PATH './cellContents/*',
                                                BOX_TOP_COLOR      VARCHAR2(20)   PATH './cellContents/box/topPen/@lineColor',
                                                BOX_BOTTOM_COLOR   VARCHAR2(20)   PATH './cellContents/box/bottomPen/@lineColor',
                                                BOX_LEFT_COLOR     VARCHAR2(20)   PATH './cellContents/box/leftPen/@lineColor',
                                                BOX_RIGHT_COLOR    VARCHAR2(20)   PATH './cellContents/box/rightPen/@lineColor',
                                                BOX_TOP            VARCHAR2(20)   PATH './cellContents/box/topPen/@lineWidth',
                                                BOX_BOTTOM         VARCHAR2(20)   PATH './cellContents/box/bottomPen/@lineWidth',
                                                BOX_LEFT           VARCHAR2(20)   PATH './cellContents/box/leftPen/@lineWidth',
                                                BOX_RIGHT          VARCHAR2(20)   PATH './cellContents/box/rightPen/@lineWidth',
                                                BOX_LINEWIDTH      VARCHAR2(10)   PATH './cellContents/box/pen/@lineWidth',
                                                BOX_COLOR          VARCHAR2(20)   PATH './cellContents/box/pen/@lineColor',
                                                BACKCOLOR          VARCHAR2(20)   PATH './cellContents/@backcolor',
                                                MODUS              VARCHAR2(20)   PATH './cellContents/@mode',
                                                STYLE              VARCHAR2(255)  PATH './cellContents/@style'
                       );
      rec  crCell%ROWTYPE;
      rCell PK_JRXML2PDF_TYPES.tCrossTabCell;
    BEGIN
      -- extract cell-data
      OPEN crCell;
      FETCh crCell INTO rec;
      CLOSE crCell;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Load headercell ');
      END IF;
      -- set properties
      rCell.vcName            :='HEADER';
      rCell.nWidth            :=FK_MAKE_NUM(rec.WIDTH);
      rCell.nHeight           :=FK_MAKE_NUM(rec.HEIGHT);
      IF rec.CELL IS NOT NULL THEN
        -- load cell-band
        rCell.nCellPos      :=FK_MAKE_BAND_FROM_CELL(i_oParts           =>rec.CELL,
                                                     i_nWidth           =>rCell.nWidth,
                                                     i_nHeight          =>rCell.nHeight,
                                                     i_vcBoxTop         =>rec.BOX_TOP,
                                                     i_vcBoxBottom      =>rec.BOX_BOTTOM,
                                                     i_vcBoxLeft        =>rec.BOX_LEFT,
                                                     i_vcBoxRight       =>rec.BOX_RIGHT,
                                                     i_vcBoxTopColor    =>rec.BOX_TOP_COLOR,
                                                     i_vcBoxBottomColor =>rec.BOX_BOTTOM_COLOR,
                                                     i_vcBoxLeftColor   =>rec.BOX_LEFT_COLOR,
                                                     i_vcBoxRightColor  =>rec.BOX_RIGHT_COLOR,
                                                     i_vcBackColor      =>rec.BACKCOLOR,
                                                     i_vcOpaque         =>rec.OPAQUE,
                                                     i_vcStyle          =>rec.STYLE
                                                    );
      END IF;
      RETURN rCell;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Load crosstab');
    END IF;

    -- find subdataset
    IF io_rReport.lDatasets.EXISTS(i_vcSubdataset) THEN
      rCrossTab.vcQuery:=io_rReport.lDatasets(i_vcSubdataset).vcQuery;
    ELSE
      rCrossTab.vcQuery:=io_rReport.vcQuery;
    END IF;
    -- set attributes
    rCrossTab.vcRepeatRowHeaders:=i_vcRepeatRowHeaders;
    rCrossTab.vcRepeatColHeaders:=i_vcRepeatColHeaders;
    rCrossTab.nColBreakOffset:=i_nColBreakOffset;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Load crosstab-details');
    END IF;
    -- read all crosstab-detail-information
    FOR rec IN crDetails LOOP
      IF rec.ROWTYPE='ROWGROUP' THEN
        rCrossTab.lRows(rCrossTab.lRows.COUNT+1):=FK_LOAD_ROW(rec.ROWGROUP);
      ELSIF rec.ROWTYPE='COLGROUP' THEN
        rCrossTab.lColumns(rCrossTab.lColumns.COUNT+1):=FK_LOAD_COLUMN(rec.COLGROUP);
      ELSIF rec.ROWTYPE='MEASURE' THEN
        rCrossTab.lMeasures(rCrossTab.lMeasures.COUNT+1):=FK_LOAD_MEASURE(rec.MEASURE);
      ELSIF rec.ROWTYPE='CELL' THEN
        rCell:=FK_LOAD_CELL(rec.CELL);
        rCrossTab.lCells(rCell.vcName):=rCell;
      ELSIF rec.ROWTYPE='HEADERCELL' THEN
        rCrossTab.rHeaderCell:=FK_LOAD_HEADERCELL(rec.HEADERCELL);
      END IF;
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Calculating missing width and height for rows, columns and cells');
    END IF;
    -- Set the width for all cols froms the according cells
    FOR i IN 1..rCrossTab.lColumns.COUNT LOOP
      rCell:=rCrossTab.lCells('_');
      IF rCrossTab.lColumns(i).nWidth IS NULL THEN
        rCrossTab.lColumns(i).nWidth:=rCell.nWidth;
      END IF;
      IF rCrossTab.lColumns(i).nHeaderPos IS NOT NULL THEN
        rCell:=rCrossTab.lCells('_');
        io_rReport.lBands(rCrossTab.lColumns(i).nHeaderPos).nWidth:=NVL(rCrossTab.lColumns(i).nWidth, rCell.nWidth);
      END IF;
      IF rCrossTab.lColumns(i).nTotalHeaderPos IS NOT NULL THEN
        rCell:=rCrossTab.lCells('_' || rCrossTab.lColumns(i).vcName);
        IF rCell.nWidth IS NULL THEN
          rCell:=rCrossTab.lCells('_');
        END IF;
        io_rReport.lBands(rCrossTab.lColumns(i).nTotalHeaderPos).nWidth:=rCell.nWidth;
      END IF;
    END LOOP;
    -- set the width for all cols froms the according cells
    FOR i IN 1..rCrossTab.lRows.COUNT LOOP
      rCell:=rCrossTab.lCells('_');
      IF rCrossTab.lRows(i).nHeight IS NULL THEN
        rCrossTab.lRows(i).nHeight:=rCell.nHeight;
      END IF;
      IF rCrossTab.lRows(i).nHeaderPos IS NOT NULL THEN
        rCell:=rCrossTab.lCells('_');
        io_rReport.lBands(rCrossTab.lRows(i).nHeaderPos).nHeight:=NVL(rCrossTab.lRows(i).nHeight, rCell.nHeight);
      END IF;
      IF rCrossTab.lRows(i).nTotalHeaderPos IS NOT NULL THEN
        rCell:=rCrossTab.lCells(rCrossTab.lRows(i).vcName || '_');
        IF rCell.nHeight IS NULL THEN
          rCell:=rCrossTab.lCells('_');
        END IF;
        io_rReport.lBands(rCrossTab.lRows(i).nTotalHeaderPos).nHeight:=rCell.nHeight;
      END IF;
    END LOOP;
    -- Now set the width and height for all cells, if not already set

    rCell:=rCrossTab.lCells('_');

    vcIndex:=rCrosstab.lCells.FIRST;
    WHILE vcIndex IS NOT NULL LOOP
      IF rCrossTab.lCells(vcIndex).nWidth IS NULL THEN
        -- take width from according column
        FOR i IN 1..rCrossTab.lColumns.COUNT LOOP
          IF rCrossTab.lColumns(i).vcName=rCrossTab.lCells(vcIndex).vcColGroup THEN
            IF rCrossTab.lCells(vcIndex).nCellPos IS NOT NULL THEN
              rCrossTab.lCells(vcIndex).nWidth:=io_rReport.lBands(rCrossTab.lColumns(i).nTotalHeaderPos).nWidth;
              io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nWidth:=io_rReport.lBands(rCrossTab.lColumns(i).nTotalHeaderPos).nWidth;
              EXIT;
            END IF;
          END IF;
        END LOOP;
        IF io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nWidth IS NULL THEN
          rCrossTab.lCells(vcIndex).nWidth:=rCell.nWidth;
          IF rCrossTab.lCells(vcIndex).nCellPos IS NOT NULL THEN
            io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nWidth:=rCrossTab.lCells(vcIndex).nWidth;
          END IF;
        END IF;
      END IF;
      IF rCrossTab.lCells(vcIndex).nheight IS NULL THEN
        -- take height from according row
        FOR i IN 1..rCrossTab.lRows.COUNT LOOP
          IF rCrossTab.lRows(i).vcName=rCrossTab.lCells(vcIndex).vcRowGroup THEN
            IF rCrossTab.lCells(vcIndex).nCellPos IS NOT NULL THEN
              rCrossTab.lCells(vcIndex).nHeight:=io_rReport.lBands(rCrossTab.lRows(i).nTotalHeaderPos).nHeight;
              io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nHeight:=io_rReport.lBands(rCrossTab.lRows(i).nTotalHeaderPos).nHeight;
              EXIT;
            END IF;
          END IF;
        END LOOP;
        IF io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nHeight IS NULL THEN
          rCrossTab.lCells(vcIndex).nHeight:=rCell.nHeight;
          IF rCrossTab.lCells(vcIndex).nCellPos IS NOT NULL THEN
            io_rReport.lBands(rCrossTab.lCells(vcIndex).nCellPos).nHeight:=rCrossTab.lCells(vcIndex).nHeight;
          END IF;
        END IF;
      END IF;
      vcIndex:=rCrossTab.lCells.NEXT(vcIndex);
    END LOOP;
    RETURN rCrossTab;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_LOAD_TABLE_AS_SUBREPORT(i_rReport      IN PK_JRXML2PDF_TYPES.tReport,
                                       i_vcReportName IN VARCHAR2,
                                       i_oTable       IN XMLTYPE,
                                       i_vcSubdataset IN VARCHAR2) IS

    TYPE tColStartGroup IS TABLE OF NUMBER INDEX BY PK_JRXML2PDF_TYPES.tName;
    TYPE_TABLE_HEADER  CONSTANT NUMBER:=1;
    TYPE_COLUMN_HEADER CONSTANT NUMBER:=2;
    TYPE_DETAIL_CELL   CONSTANT NUMBER:=3;
    TYPE_TABLE_FOOTER  CONSTANT NUMBER:=4;
    TYPE_COLUMN_FOOTER CONSTANT NUMBER:=5;
    TYPE_GROUP_HEADER  CONSTANT NUMBER:=6;
    TYPE_GROUP_FOOTER  CONSTANT NUMBER:=7;

    CURSOR crColumnsTH IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             TH_HEIGHT,
             CH_HEIGHT,
             DC_HEIGHT,
             CF_HEIGHT,
             TF_HEIGHT,
             NVL(TH_BOX_TOP_COLOR, TH_BOX_COLOR)    TH_BOX_TOP_COLOR,
             NVL(TH_BOX_BOTTOM_COLOR, TH_BOX_COLOR) TH_BOX_BOTTOM_COLOR,
             NVL(TH_BOX_LEFT_COLOR, TH_BOX_COLOR)   TH_BOX_LEFT_COLOR,
             NVL(TH_BOX_RIGHT_COLOR, TH_BOX_COLOR)  TH_BOX_RIGHT_COLOR,
             NVL(TH_BOX_TOP, TH_BOX_LINEWIDTH)    TH_BOX_TOP,
             NVL(TH_BOX_BOTTOM, TH_BOX_LINEWIDTH) TH_BOX_BOTTOM,
             NVL(TH_BOX_LEFT, TH_BOX_LINEWIDTH)   TH_BOX_LEFT,
             NVL(TH_BOX_RIGHT, TH_BOX_LINEWIDTH)  TH_BOX_RIGHT,
             TH_STYLE,
             XMLELEMENT("element", TH_DETAILS) TH_DETAILS,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                TH_BOX_TOP_COLOR    VARCHAR2(20)   PATH './tableHeader/box/topPen/@lineColor',
                                                TH_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './tableHeader/box/bottomPen/@lineColor',
                                                TH_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './tableHeader/box/leftPen/@lineColor',
                                                TH_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './tableHeader/box/rightPen/@lineColor',
                                                TH_BOX_TOP          VARCHAR2(20)   PATH './tableHeader/box/topPen/@lineWidth',
                                                TH_BOX_BOTTOM       VARCHAR2(20)   PATH './tableHeader/box/bottomPen/@lineWidth',
                                                TH_BOX_LEFT         VARCHAR2(20)   PATH './tableHeader/box/leftPen/@lineWidth',
                                                TH_BOX_RIGHT        VARCHAR2(20)   PATH './tableHeader/box/rightPen/@lineWidth',
                                                TH_BOX_LINEWIDTH    VARCHAR2(10)   PATH './tableHeader/box/pen/@lineWidth',
                                                TH_BOX_COLOR        VARCHAR2(20)   PATH './tableHeader/box/pen/@lineColor',
                                                TH_STYLE            VARCHAR2(255)  PATH './tableHeader/@style',
                                                TH_DETAILS          XMLTYPE        PATH './tableHeader/*'
                     );

    CURSOR crColumnsCH IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             TH_HEIGHT,
             CH_HEIGHT,
             DC_HEIGHT,
             CF_HEIGHT,
             TF_HEIGHT,
             NVL(CH_BOX_TOP_COLOR, CH_BOX_COLOR)    CH_BOX_TOP_COLOR,
             NVL(CH_BOX_BOTTOM_COLOR, CH_BOX_COLOR) CH_BOX_BOTTOM_COLOR,
             NVL(CH_BOX_LEFT_COLOR, CH_BOX_COLOR)   CH_BOX_LEFT_COLOR,
             NVL(CH_BOX_RIGHT_COLOR, CH_BOX_COLOR)  CH_BOX_RIGHT_COLOR,
             NVL(CH_BOX_TOP, CH_BOX_LINEWIDTH)    CH_BOX_TOP,
             NVL(CH_BOX_BOTTOM, CH_BOX_LINEWIDTH) CH_BOX_BOTTOM,
             NVL(CH_BOX_LEFT, CH_BOX_LINEWIDTH)   CH_BOX_LEFT,
             NVL(CH_BOX_RIGHT, CH_BOX_LINEWIDTH)  CH_BOX_RIGHT,
             CH_STYLE,
             XMLELEMENT("element", CH_DETAILS) CH_DETAILS,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                CH_BOX_TOP_COLOR    VARCHAR2(20)   PATH './columnHeader/box/topPen/@lineColor',
                                                CH_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './columnHeader/box/bottomPen/@lineColor',
                                                CH_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './columnHeader/box/leftPen/@lineColor',
                                                CH_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './columnHeader/box/rightPen/@lineColor',
                                                CH_BOX_TOP          VARCHAR2(20)   PATH './columnHeader/box/topPen/@lineWidth',
                                                CH_BOX_BOTTOM       VARCHAR2(20)   PATH './columnHeader/box/bottomPen/@lineWidth',
                                                CH_BOX_LEFT         VARCHAR2(20)   PATH './columnHeader/box/leftPen/@lineWidth',
                                                CH_BOX_RIGHT        VARCHAR2(20)   PATH './columnHeader/box/rightPen/@lineWidth',
                                                CH_BOX_LINEWIDTH    VARCHAR2(10)   PATH './columnHeader/box/pen/@lineWidth',
                                                CH_BOX_COLOR        VARCHAR2(20)   PATH './columnHeader/box/pen/@lineColor',
                                                CH_STYLE            VARCHAR2(255)  PATH './columnHeader/@style',
                                                CH_DETAILS          XMLTYPE        PATH './columnHeader/*'
                     );
    CURSOR crColumnsDC IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             TH_HEIGHT,
             CH_HEIGHT,
             DC_HEIGHT,
             CF_HEIGHT,
             TF_HEIGHT,
             NVL(DC_BOX_TOP_COLOR, DC_BOX_COLOR)    DC_BOX_TOP_COLOR,
             NVL(DC_BOX_BOTTOM_COLOR, DC_BOX_COLOR) DC_BOX_BOTTOM_COLOR,
             NVL(DC_BOX_LEFT_COLOR, DC_BOX_COLOR)   DC_BOX_LEFT_COLOR,
             NVL(DC_BOX_RIGHT_COLOR, DC_BOX_COLOR)  DC_BOX_RIGHT_COLOR,
             NVL(DC_BOX_TOP, DC_BOX_LINEWIDTH)    DC_BOX_TOP,
             NVL(DC_BOX_BOTTOM, DC_BOX_LINEWIDTH) DC_BOX_BOTTOM,
             NVL(DC_BOX_LEFT, DC_BOX_LINEWIDTH)   DC_BOX_LEFT,
             NVL(DC_BOX_RIGHT, DC_BOX_LINEWIDTH)  DC_BOX_RIGHT,
             DC_STYLE,
             XMLELEMENT("element", DC_DETAILS) DC_DETAILS,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                DC_BOX_TOP_COLOR    VARCHAR2(20)   PATH './detailCell/box/topPen/@lineColor',
                                                DC_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './detailCell/box/bottomPen/@lineColor',
                                                DC_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './detailCell/box/leftPen/@lineColor',
                                                DC_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './detailCell/box/rightPen/@lineColor',
                                                DC_BOX_TOP          VARCHAR2(20)   PATH './detailCell/box/topPen/@lineWidth',
                                                DC_BOX_BOTTOM       VARCHAR2(20)   PATH './detailCell/box/bottomPen/@lineWidth',
                                                DC_BOX_LEFT         VARCHAR2(20)   PATH './detailCell/box/leftPen/@lineWidth',
                                                DC_BOX_RIGHT        VARCHAR2(20)   PATH './detailCell/box/rightPen/@lineWidth',
                                                DC_BOX_LINEWIDTH    VARCHAR2(10)   PATH './detailCell/box/pen/@lineWidth',
                                                DC_BOX_COLOR        VARCHAR2(20)   PATH './detailCell/box/pen/@lineColor',
                                                DC_STYLE            VARCHAR2(255)  PATH './detailCell/@style',
                                                DC_DETAILS          XMLTYPE        PATH './detailCell/*'
                     );
    CURSOR crColumnsCF IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             TH_HEIGHT,
             CH_HEIGHT,
             DC_HEIGHT,
             CF_HEIGHT,
             TF_HEIGHT,
             NVL(CF_BOX_TOP_COLOR, CF_BOX_COLOR)    CF_BOX_TOP_COLOR,
             NVL(CF_BOX_BOTTOM_COLOR, CF_BOX_COLOR) CF_BOX_BOTTOM_COLOR,
             NVL(CF_BOX_LEFT_COLOR, CF_BOX_COLOR)   CF_BOX_LEFT_COLOR,
             NVL(CF_BOX_RIGHT_COLOR, CF_BOX_COLOR)  CF_BOX_RIGHT_COLOR,
             NVL(CF_BOX_TOP, CF_BOX_LINEWIDTH)    CF_BOX_TOP,
             NVL(CF_BOX_BOTTOM, CF_BOX_LINEWIDTH) CF_BOX_BOTTOM,
             NVL(CF_BOX_LEFT, CF_BOX_LINEWIDTH)   CF_BOX_LEFT,
             NVL(CF_BOX_RIGHT, CF_BOX_LINEWIDTH)  CF_BOX_RIGHT,
             CF_STYLE,
             XMLELEMENT("element", CF_DETAILS) CF_DETAILS,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                CF_BOX_TOP_COLOR    VARCHAR2(20)   PATH './columnFooter/box/topPen/@lineColor',
                                                CF_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './columnFooter/box/bottomPen/@lineColor',
                                                CF_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './columnFooter/box/leftPen/@lineColor',
                                                CF_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './columnFooter/box/rightPen/@lineColor',
                                                CF_BOX_TOP          VARCHAR2(20)   PATH './columnFooter/box/topPen/@lineWidth',
                                                CF_BOX_BOTTOM       VARCHAR2(20)   PATH './columnFooter/box/bottomPen/@lineWidth',
                                                CF_BOX_LEFT         VARCHAR2(20)   PATH './columnFooter/box/leftPen/@lineWidth',
                                                CF_BOX_RIGHT        VARCHAR2(20)   PATH './columnFooter/box/rightPen/@lineWidth',
                                                CF_BOX_LINEWIDTH    VARCHAR2(10)   PATH './columnFooter/box/pen/@lineWidth',
                                                CF_BOX_COLOR        VARCHAR2(20)   PATH './columnFooter/box/pen/@lineColor',
                                                CF_STYLE            VARCHAR2(255)  PATH './columnFooter/@style',
                                                CF_DETAILS          XMLTYPE        PATH './columnFooter/*'
                     );

    CURSOR crColumnsTF IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             TH_HEIGHT,
             CH_HEIGHT,
             DC_HEIGHT,
             CF_HEIGHT,
             TF_HEIGHT,
             NVL(TF_BOX_TOP_COLOR, TF_BOX_COLOR)    TF_BOX_TOP_COLOR,
             NVL(TF_BOX_BOTTOM_COLOR, TF_BOX_COLOR) TF_BOX_BOTTOM_COLOR,
             NVL(TF_BOX_LEFT_COLOR, TF_BOX_COLOR)   TF_BOX_LEFT_COLOR,
             NVL(TF_BOX_RIGHT_COLOR, TF_BOX_COLOR)  TF_BOX_RIGHT_COLOR,
             NVL(TF_BOX_TOP, TF_BOX_LINEWIDTH)    TF_BOX_TOP,
             NVL(TF_BOX_BOTTOM, TF_BOX_LINEWIDTH) TF_BOX_BOTTOM,
             NVL(TF_BOX_LEFT, TF_BOX_LINEWIDTH)   TF_BOX_LEFT,
             NVL(TF_BOX_RIGHT, TF_BOX_LINEWIDTH)  TF_BOX_RIGHT,
             TF_STYLE,
             XMLELEMENT("element", TF_DETAILS) TF_DETAILS,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                TF_BOX_TOP_COLOR    VARCHAR2(20)   PATH './tableFooter/box/topPen/@lineColor',
                                                TF_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './tableFooter/box/bottomPen/@lineColor',
                                                TF_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './tableFooter/box/leftPen/@lineColor',
                                                TF_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './tableFooter/box/rightPen/@lineColor',
                                                TF_BOX_TOP          VARCHAR2(20)   PATH './tableFooter/box/topPen/@lineWidth',
                                                TF_BOX_BOTTOM       VARCHAR2(20)   PATH './tableFooter/box/bottomPen/@lineWidth',
                                                TF_BOX_LEFT         VARCHAR2(20)   PATH './tableFooter/box/leftPen/@lineWidth',
                                                TF_BOX_RIGHT        VARCHAR2(20)   PATH './tableFooter/box/rightPen/@lineWidth',
                                                TF_BOX_LINEWIDTH    VARCHAR2(10)   PATH './tableFooter/box/pen/@lineWidth',
                                                TF_BOX_COLOR        VARCHAR2(20)   PATH './tableFooter/box/pen/@lineColor',
                                                TF_STYLE            VARCHAR2(255)  PATH './tableFooter/@style',
                                                TF_DETAILS          XMLTYPE        PATH './tableFooter/*'
                     );

    CURSOR crColumnsGH IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE,
             XMLELEMENT("element", COL_GH) COL_GH,
             XMLELEMENT("element", COLGROUP_GH) COLGROUP_GH
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                COL_GH              XMLTYPE        PATH './../column/groupHeader',
                                                COLGROUP_GH         XMLTYPE        PATH './../columnGroup/groupHeader'
                     );

    CURSOR crColumnsGF IS
      SELECT WIDTH,
             WHEN_EXPRESSION,
             COL,
             COLGROUP,
             CASE WHEN COL IS NOT NULL THEN
               'COL'
             WHEN COLGROUP IS NOT NULL THEN
               'COLGROUP'
             END ROWTYPE,
             XMLELEMENT("element", COL_GF) COL_GF,
             XMLELEMENT("element", COLGROUP_GF) COLGROUP_GF
        FROM XMLTABLE('/table/*'        PASSING i_oTable
                                        COLUMNS COL                 XMLTYPE        PATH './../column',
                                                COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                WIDTH               NUMBER         PATH './@width',
                                                WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                COL_GF              XMLTYPE        PATH './../column/groupFooter',
                                                COLGROUP_GF         XMLTYPE        PATH './../columnGroup/groupFooter'
                     );

    CURSOR crColumnsGroup(i_oXmlHeaders IN XMLTYPE) IS
      SELECT NAME,
             HEIGHT,
             NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
             NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
             NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
             NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
             STYLE,
             XMLELEMENT("element", DETAILS)   DETAILS
        FROM XMLTABLE('/element/*'  PASSING i_oXmlHeaders
                                        COLUMNS NAME             VARCHAR2(255)  PATH './@groupName',
                                                HEIGHT           NUMBER         PATH './cell/@height',
                                                BOX_TOP_COLOR    VARCHAR2(20)   PATH './cell/box/topPen/@lineColor',
                                                BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './cell/box/bottomPen/@lineColor',
                                                BOX_LEFT_COLOR   VARCHAR2(20)   PATH './cell/box/leftPen/@lineColor',
                                                BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './cell/box/rightPen/@lineColor',
                                                BOX_TOP          VARCHAR2(20)   PATH './cell/box/topPen/@lineWidth',
                                                BOX_BOTTOM       VARCHAR2(20)   PATH './cell/box/bottomPen/@lineWidth',
                                                BOX_LEFT         VARCHAR2(20)   PATH './cell/box/leftPen/@lineWidth',
                                                BOX_RIGHT        VARCHAR2(20)   PATH './cell/box/rightPen/@lineWidth',
                                                BOX_LINEWIDTH    VARCHAR2(10)   PATH './cell/box/pen/@lineWidth',
                                                BOX_COLOR        VARCHAR2(20)   PATH './cell/box/pen/@lineColor',
                                                STYLE            VARCHAR2(255)  PATH './cell/@style',
                                                DETAILS          XMLTYPE        PATH './cell/*'
                     );

    nColStart          NUMBER:=0;
    nColStartForGroup  tColStartGroup;
    nTempColStart      NUMBER;
    rReport            PK_JRXML2PDF_TYPES.tReport;
    iPos               PLS_INTEGER;
    iBandPos           PLS_INTEGER;
    vcStyle            PK_JRXML2PDF_TYPES.tStyleName;
    rGroup             PK_JRXML2PDF_TYPES.tGroup;
    iMatchGroup        PLS_INTEGER;

    PROCEDURE PR_PROCESS_PART(io_rRegion         IN OUT NOCOPY PK_JRXML2PDF_TYPES.tRegion,
                              i_vcWhenExpression IN PK_JRXML2PDF_TYPES.tExpression,
                              i_nColStart        IN NUMBER,
                              i_nColWidth        IN NUMBER,
                              i_nYOffset         IN NUMBER,
                              i_nHeight          IN NUMBER,
                              i_vcBoxTop         IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                              i_vcBoxBottom      IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                              i_vcBoxLeft        IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                              i_vcBoxRight       IN PK_JRXML2PDF_TYPES.tNumAsVarchar2,
                              i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                              i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                              i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                              i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                              i_vcStyle          IN PK_JRXML2PDF_TYPES.tStyleName,
                              i_oParts           IN XMLTYPE
                             ) IS
      rObject PK_JRXML2PDF_TYPES.tObject;
    BEGIN
      IF i_nHeight IS NOT NULL THEN
        -- Initialize band in region
        IF io_rRegion.lObjects.COUNT=0 THEN
          iBandPos:=rReport.lBands.COUNT+1;
          -- tick record to reserve array-index
          rReport.lBands(iBandPos).nX:=0;
          rReport.lBands(iBandPos).nHeight:=NVL(i_nHeight,0);
          rReport.lBands(iBandPos).bHasBreaks:=FALSE;
          rObject.nType:=PK_JRXML2PDF_TYPES.BAND;
          rObject.nPosition:=iBandPos;
          io_rRegion.lObjects(io_rRegion.lObjects.COUNT+1):=rObject;
        ELSE
          -- read band-position
          iBandPos:=io_rRegion.lObjects(1).nPosition;
          -- calculate new height for the band
          rReport.lBands(iBandPos).nHeight:=GREATEST(rReport.lBands(iBandPos).nHeight, i_nHeight+i_nYOffset);
        END IF;
        iPos:=rReport.lBands.COUNT+1;
        -- Mark record as used because of recursion
        rReport.lBands(iPos).nX:=NULL;
        rReport.lBands(iPos):=FK_LOAD_BAND(io_rReport             =>rReport,
                                           i_oBand                =>i_oParts,
                                           i_vcTyp                =>'colRegion',
                                           i_nHeight              =>i_nHeight,
                                           i_vcPrintWhenExpression=>NULL,
                                           i_vcSplitType          =>PK_JRXML2PDF_TYPES.PREVENT
                                          );
        rReport.lBands(iPos).nX               :=i_nColStart;
        rReport.lBands(iPos).nY               :=i_nYOffset;
        rReport.lBands(iPos).nWidth           :=i_nColWidth;
        rReport.lBands(iPos).nHeight          :=NVL(i_nHeight,0);
        rReport.lBands(iPos).nBoxTop          :=FK_MAKE_NUM(i_vcboxTop);
        rReport.lBands(iPos).nBoxLeft         :=FK_MAKE_NUM(i_vcBoxLeft);
        rReport.lBands(iPos).nBoxBottom       :=FK_MAKE_NUM(i_vcBoxBottom);
        rReport.lBands(iPos).nBoxRight        :=FK_MAKE_NUM(i_vcBoxRight);
        rReport.lBands(iPos).vcBoxTopColor    :=i_vcBoxTopColor;
        rReport.lBands(iPos).vcBoxLeftColor   :=i_vcBoxLeftColor;
        rReport.lBands(iPos).vcBoxBottomColor :=i_vcBoxBottomColor;
        rReport.lBands(iPos).vcBoxRightColor  :=i_vcBoxRightColor;
        rReport.lBands(iPos).vcStyle          :=i_vcStyle;
        rReport.lBands(iPos).vcWhenExpression :=i_vcWhenExpression;
        rReport.lBands(iPos).vcStretch        :=PK_JRXML2PDF_TYPES.YES;
        rObject.nType:=PK_JRXML2PDF_TYPES.SUBFRAME;
        rObject.nPosition:=iPos;
        rReport.lBands(iBandPos).lObjects(rReport.lBands(iBandPos).lObjects.COUNT+1):=rObject;
      END IF;
    END;

    FUNCTION FK_PROCESS_GROUP(i_oXml             IN XMLTYPE,
                              i_nProcessType     IN NUMBER,
                              i_vcWhenExpression IN PK_JRXML2PDF_TYPES.tExpression,
                              i_nHeight          IN NUMBER,
                              i_nLastLevelheight IN NUMBER,
                              i_vcGroupName      IN VARCHAR2 DEFAULT NULL
                             )
    RETURN NUMBER IS
      CURSOR crColumnsTH IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               TH_HEIGHT,
               NVL(TH_BOX_TOP_COLOR, TH_BOX_COLOR)    TH_BOX_TOP_COLOR,
               NVL(TH_BOX_BOTTOM_COLOR, TH_BOX_COLOR) TH_BOX_BOTTOM_COLOR,
               NVL(TH_BOX_LEFT_COLOR, TH_BOX_COLOR)   TH_BOX_LEFT_COLOR,
               NVL(TH_BOX_RIGHT_COLOR, TH_BOX_COLOR)  TH_BOX_RIGHT_COLOR,
               NVL(TH_BOX_TOP, TH_BOX_LINEWIDTH)    TH_BOX_TOP,
               NVL(TH_BOX_BOTTOM, TH_BOX_LINEWIDTH) TH_BOX_BOTTOM,
               NVL(TH_BOX_LEFT, TH_BOX_LINEWIDTH)   TH_BOX_LEFT,
               NVL(TH_BOX_RIGHT, TH_BOX_LINEWIDTH)  TH_BOX_RIGHT,
               TH_STYLE,
               XMLELEMENT("element", TH_DETAILS) TH_DETAILS,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXML
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                  TH_BOX_TOP_COLOR    VARCHAR2(20)   PATH './tableHeader/box/topPen/@lineColor',
                                                  TH_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './tableHeader/box/bottomPen/@lineColor',
                                                  TH_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './tableHeader/box/leftPen/@lineColor',
                                                  TH_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './tableHeader/box/rightPen/@lineColor',
                                                  TH_BOX_TOP          VARCHAR2(20)   PATH './tableHeader/box/topPen/@lineWidth',
                                                  TH_BOX_BOTTOM       VARCHAR2(20)   PATH './tableHeader/box/bottomPen/@lineWidth',
                                                  TH_BOX_LEFT         VARCHAR2(20)   PATH './tableHeader/box/leftPen/@lineWidth',
                                                  TH_BOX_RIGHT        VARCHAR2(20)   PATH './tableHeader/box/rightPen/@lineWidth',
                                                  TH_BOX_LINEWIDTH    VARCHAR2(10)   PATH './tableHeader/box/pen/@lineWidth',
                                                  TH_BOX_COLOR        VARCHAR2(20)   PATH './tableHeader/box/pen/@lineColor',
                                                  TH_STYLE            VARCHAR2(255)  PATH './tableHeader/@style',
                                                  TH_DETAILS          XMLTYPE        PATH './tableHeader/*'
                       );

      CURSOR crColumnsCH IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               CH_HEIGHT,
               NVL(CH_BOX_TOP_COLOR, CH_BOX_COLOR)    CH_BOX_TOP_COLOR,
               NVL(CH_BOX_BOTTOM_COLOR, CH_BOX_COLOR) CH_BOX_BOTTOM_COLOR,
               NVL(CH_BOX_LEFT_COLOR, CH_BOX_COLOR)   CH_BOX_LEFT_COLOR,
               NVL(CH_BOX_RIGHT_COLOR, CH_BOX_COLOR)  CH_BOX_RIGHT_COLOR,
               NVL(CH_BOX_TOP, CH_BOX_LINEWIDTH)    CH_BOX_TOP,
               NVL(CH_BOX_BOTTOM, CH_BOX_LINEWIDTH) CH_BOX_BOTTOM,
               NVL(CH_BOX_LEFT, CH_BOX_LINEWIDTH)   CH_BOX_LEFT,
               NVL(CH_BOX_RIGHT, CH_BOX_LINEWIDTH)  CH_BOX_RIGHT,
               CH_STYLE,
               XMLELEMENT("element", CH_DETAILS) CH_DETAILS,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXML
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  TH_DETAILS          XMLTYPE        PATH './tableHeader/*',
                                                  CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                  CH_BOX_TOP_COLOR    VARCHAR2(20)   PATH './columnHeader/box/topPen/@lineColor',
                                                  CH_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './columnHeader/box/bottomPen/@lineColor',
                                                  CH_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './columnHeader/box/leftPen/@lineColor',
                                                  CH_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './columnHeader/box/rightPen/@lineColor',
                                                  CH_BOX_TOP          VARCHAR2(20)   PATH './columnHeader/box/topPen/@lineWidth',
                                                  CH_BOX_BOTTOM       VARCHAR2(20)   PATH './columnHeader/box/bottomPen/@lineWidth',
                                                  CH_BOX_LEFT         VARCHAR2(20)   PATH './columnHeader/box/leftPen/@lineWidth',
                                                  CH_BOX_RIGHT        VARCHAR2(20)   PATH './columnHeader/box/rightPen/@lineWidth',
                                                  CH_BOX_LINEWIDTH    VARCHAR2(10)   PATH './columnHeader/box/pen/@lineWidth',
                                                  CH_BOX_COLOR        VARCHAR2(20)   PATH './columnHeader/box/pen/@lineColor',
                                                  CH_STYLE            VARCHAR2(255)  PATH './columnHeader/@style',
                                                  CH_DETAILS          XMLTYPE        PATH './columnHeader/*'
                       );

      CURSOR crColumnsDC IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               DC_HEIGHT,
               TH_HEIGHT,
               CH_HEIGHT,
               CF_HEIGHT,
               TF_HEIGHT,
               NVL(DC_BOX_TOP_COLOR, DC_BOX_COLOR)    DC_BOX_TOP_COLOR,
               NVL(DC_BOX_BOTTOM_COLOR, DC_BOX_COLOR) DC_BOX_BOTTOM_COLOR,
               NVL(DC_BOX_LEFT_COLOR, DC_BOX_COLOR)   DC_BOX_LEFT_COLOR,
               NVL(DC_BOX_RIGHT_COLOR, DC_BOX_COLOR)  DC_BOX_RIGHT_COLOR,
               NVL(DC_BOX_TOP, DC_BOX_LINEWIDTH)    DC_BOX_TOP,
               NVL(DC_BOX_BOTTOM, DC_BOX_LINEWIDTH) DC_BOX_BOTTOM,
               NVL(DC_BOX_LEFT, DC_BOX_LINEWIDTH)   DC_BOX_LEFT,
               NVL(DC_BOX_RIGHT, DC_BOX_LINEWIDTH)  DC_BOX_RIGHT,
               DC_STYLE,
               XMLELEMENT("element", DC_DETAILS) DC_DETAILS,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXML
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  DC_HEIGHT           NUMBER         PATH './detailCell/@height',
                                                  TH_HEIGHT           NUMBER         PATH './tableHeader/@height',
                                                  CH_HEIGHT           NUMBER         PATH './columnHeader/@height',
                                                  CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                  TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                  DC_BOX_TOP_COLOR    VARCHAR2(20)   PATH './detailCell/box/topPen/@lineColor',
                                                  DC_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './detailCell/box/bottomPen/@lineColor',
                                                  DC_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './detailCell/box/leftPen/@lineColor',
                                                  DC_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './detailCell/box/rightPen/@lineColor',
                                                  DC_BOX_TOP          VARCHAR2(20)   PATH './detailCell/box/topPen/@lineWidth',
                                                  DC_BOX_BOTTOM       VARCHAR2(20)   PATH './detailCell/box/bottomPen/@lineWidth',
                                                  DC_BOX_LEFT         VARCHAR2(20)   PATH './detailCell/box/leftPen/@lineWidth',
                                                  DC_BOX_RIGHT        VARCHAR2(20)   PATH './detailCell/box/rightPen/@lineWidth',
                                                  DC_BOX_LINEWIDTH    VARCHAR2(10)   PATH './detailCell/box/pen/@lineWidth',
                                                  DC_BOX_COLOR        VARCHAR2(20)   PATH './detailCell/box/pen/@lineColor',
                                                  DC_STYLE            VARCHAR2(255)  PATH './detailCell/@style',
                                                  DC_DETAILS          XMLTYPE        PATH './detailCell/*'
                       );

      CURSOR crColumnsCF IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               CF_HEIGHT,
               NVL(CF_BOX_TOP_COLOR, CF_BOX_COLOR)    CF_BOX_TOP_COLOR,
               NVL(CF_BOX_BOTTOM_COLOR, CF_BOX_COLOR) CF_BOX_BOTTOM_COLOR,
               NVL(CF_BOX_LEFT_COLOR, CF_BOX_COLOR)   CF_BOX_LEFT_COLOR,
               NVL(CF_BOX_RIGHT_COLOR, CF_BOX_COLOR)  CF_BOX_RIGHT_COLOR,
               NVL(CF_BOX_TOP, CF_BOX_LINEWIDTH)    CF_BOX_TOP,
               NVL(CF_BOX_BOTTOM, CF_BOX_LINEWIDTH) CF_BOX_BOTTOM,
               NVL(CF_BOX_LEFT, CF_BOX_LINEWIDTH)   CF_BOX_LEFT,
               NVL(CF_BOX_RIGHT, CF_BOX_LINEWIDTH)  CF_BOX_RIGHT,
               CF_STYLE,
               XMLELEMENT("element", CF_DETAILS) CF_DETAILS,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXML
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  CF_HEIGHT           NUMBER         PATH './columnFooter/@height',
                                                  CF_BOX_TOP_COLOR    VARCHAR2(20)   PATH './columnFooter/box/topPen/@lineColor',
                                                  CF_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './columnFooter/box/bottomPen/@lineColor',
                                                  CF_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './columnFooter/box/leftPen/@lineColor',
                                                  CF_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './columnFooter/box/rightPen/@lineColor',
                                                  CF_BOX_TOP          VARCHAR2(20)   PATH './columnFooter/box/topPen/@lineWidth',
                                                  CF_BOX_BOTTOM       VARCHAR2(20)   PATH './columnFooter/box/bottomPen/@lineWidth',
                                                  CF_BOX_LEFT         VARCHAR2(20)   PATH './columnFooter/box/leftPen/@lineWidth',
                                                  CF_BOX_RIGHT        VARCHAR2(20)   PATH './columnFooter/box/rightPen/@lineWidth',
                                                  CF_BOX_LINEWIDTH    VARCHAR2(10)   PATH './columnFooter/box/pen/@lineWidth',
                                                  CF_BOX_COLOR        VARCHAR2(20)   PATH './columnFooter/box/pen/@lineColor',
                                                  CF_STYLE            VARCHAR2(255)  PATH './columnFooter/@style',
                                                  CF_DETAILS          XMLTYPE        PATH './columnFooter/*'
                       );


      CURSOR crColumnsTF IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               TF_HEIGHT,
               NVL(TF_BOX_TOP_COLOR, TF_BOX_COLOR)    TF_BOX_TOP_COLOR,
               NVL(TF_BOX_BOTTOM_COLOR, TF_BOX_COLOR) TF_BOX_BOTTOM_COLOR,
               NVL(TF_BOX_LEFT_COLOR, TF_BOX_COLOR)   TF_BOX_LEFT_COLOR,
               NVL(TF_BOX_RIGHT_COLOR, TF_BOX_COLOR)  TF_BOX_RIGHT_COLOR,
               NVL(TF_BOX_TOP, TF_BOX_LINEWIDTH)    TF_BOX_TOP,
               NVL(TF_BOX_BOTTOM, TF_BOX_LINEWIDTH) TF_BOX_BOTTOM,
               NVL(TF_BOX_LEFT, TF_BOX_LINEWIDTH)   TF_BOX_LEFT,
               NVL(TF_BOX_RIGHT, TF_BOX_LINEWIDTH)  TF_BOX_RIGHT,
               TF_STYLE,
               XMLELEMENT("element", TF_DETAILS) TF_DETAILS,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXML
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  TF_HEIGHT           NUMBER         PATH './tableFooter/@height',
                                                  TF_BOX_TOP_COLOR    VARCHAR2(20)   PATH './tableFooter/box/topPen/@lineColor',
                                                  TF_BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './tableFooter/box/bottomPen/@lineColor',
                                                  TF_BOX_LEFT_COLOR   VARCHAR2(20)   PATH './tableFooter/box/leftPen/@lineColor',
                                                  TF_BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './tableFooter/box/rightPen/@lineColor',
                                                  TF_BOX_TOP          VARCHAR2(20)   PATH './tableFooter/box/topPen/@lineWidth',
                                                  TF_BOX_BOTTOM       VARCHAR2(20)   PATH './tableFooter/box/bottomPen/@lineWidth',
                                                  TF_BOX_LEFT         VARCHAR2(20)   PATH './tableFooter/box/leftPen/@lineWidth',
                                                  TF_BOX_RIGHT        VARCHAR2(20)   PATH './tableFooter/box/rightPen/@lineWidth',
                                                  TF_BOX_LINEWIDTH    VARCHAR2(10)   PATH './tableFooter/box/pen/@lineWidth',
                                                  TF_BOX_COLOR        VARCHAR2(20)   PATH './tableFooter/box/pen/@lineColor',
                                                  TF_STYLE            VARCHAR2(255)  PATH './tableFooter/@style',
                                                  TF_DETAILS          XMLTYPE        PATH './tableFooter/*'
                       );

      CURSOR crColumnsGH IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE,
               XMLELEMENT("element", COL_GH) COL_GH,
               XMLELEMENT("element", COLGROUP_GH) COLGROUP_GH
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXml
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  COL_GH              XMLTYPE        PATH './../column/groupHeader',
                                                  COLGROUP_GH         XMLTYPE        PATH './../columnGroup/groupHeader'
                       );

      CURSOR crColumnsGF IS
        SELECT WIDTH,
               WHEN_EXPRESSION,
               COL,
               COLGROUP,
               CASE WHEN COL IS NOT NULL THEN
                 'COL'
               WHEN COLGROUP IS NOT NULL THEN
                 'COLGROUP'
               END ROWTYPE,
               XMLELEMENT("element", COL_GF) COL_GF,
               XMLELEMENT("element", COLGROUP_GF) COLGROUP_GF
          FROM XMLTABLE('/columnGroup/*'  PASSING i_oXml
                                          COLUMNS COL                 XMLTYPE        PATH './../column',
                                                  COLGROUP            XMLTYPE        PATH './../columnGroup',
                                                  WIDTH               NUMBER         PATH './@width',
                                                  WHEN_EXPRESSION     VARCHAR2(4000) PATH './printWhenExpression',
                                                  COL_GF              XMLTYPE        PATH './../column/groupFooter',
                                                  COLGROUP_GF         XMLTYPE        PATH './../columnGroup/groupFooter'
                       );

      CURSOR crColumnsGroup(i_oXmlHeaders IN XMLTYPE) IS
        SELECT NAME,
               HEIGHT,
               NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
               NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
               NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
               NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
               NVL(BOX_TOP, BOX_LINEWIDTH)      BOX_TOP,
               NVL(BOX_BOTTOM, BOX_LINEWIDTH)   BOX_BOTTOM,
               NVL(BOX_LEFT, BOX_LINEWIDTH)     BOX_LEFT,
               NVL(BOX_RIGHT, BOX_LINEWIDTH)    BOX_RIGHT,
               STYLE,
               XMLELEMENT("element", DETAILS)   DETAILS
          FROM XMLTABLE('/element/*'  PASSING i_oXmlHeaders
                                          COLUMNS NAME             VARCHAR2(255)  PATH './@groupName',
                                                  HEIGHT           NUMBER         PATH './cell/@height',
                                                  BOX_TOP_COLOR    VARCHAR2(20)   PATH './cell/box/topPen/@lineColor',
                                                  BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './cell/box/bottomPen/@lineColor',
                                                  BOX_LEFT_COLOR   VARCHAR2(20)   PATH './cell/box/leftPen/@lineColor',
                                                  BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './cell/box/rightPen/@lineColor',
                                                  BOX_TOP          VARCHAR2(20)   PATH './cell/box/topPen/@lineWidth',
                                                  BOX_BOTTOM       VARCHAR2(20)   PATH './cell/box/bottomPen/@lineWidth',
                                                  BOX_LEFT         VARCHAR2(20)   PATH './cell/box/leftPen/@lineWidth',
                                                  BOX_RIGHT        VARCHAR2(20)   PATH './cell/box/rightPen/@lineWidth',
                                                  BOX_LINEWIDTH    VARCHAR2(10)   PATH './cell/box/pen/@lineWidth',
                                                  BOX_COLOR        VARCHAR2(20)   PATH './cell/box/pen/@lineColor',
                                                  STYLE            VARCHAR2(255)  PATH './cell/@style',
                                                  DETAILS          XMLTYPE        PATH './cell/*'
                       );

      nLastColStart NUMBER:=nColStart;
      nInnerheight  NUMBER:=0;

      PROCEDURE PR_PROCESS_TABLE_HEADER IS
        nMasterColStart NUMBER;
      BEGIN
        FOR rec IN crColumnsTH LOOP
          IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
            -- table-header in column,
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Table-Header');
            END IF;

            PR_PROCESS_PART(rReport.rTitleRegion,
                            i_vcWhenExpression,
                            nColStart,
                            rec.Width,
                            i_nHeight,
                            rec.TH_HEIGHT,
                            rec.TH_BOX_TOP,
                            rec.TH_BOX_BOTTOM,
                            rec.TH_BOX_LEFT,
                            rec.TH_BOX_RIGHT,
                            rec.TH_BOX_TOP_COLOR,
                            rec.TH_BOX_BOTTOM_COLOR,
                            rec.TH_BOX_LEFT_COLOR,
                            rec.TH_BOX_RIGHT_COLOR,
                            rec.TH_STYLE,
                            rec.TH_DETAILS
                           );
          END IF;
          IF rec.ROWTYPE='COLGROUP' THEN
            -- recursively process columngroup
            nMasterColStart:=nColStart;
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           i_nProcessType,
                                           i_vcWhenExpression,
                                           i_nHeight+NVL(rec.TH_HEIGHT,0),
                                           NVL(rec.TH_HEIGHT,0)
                                          );
          END IF;
          IF rec.ROWTYPE='COL' THEN
            -- Startposition of next column
            nColStart:=nColStart+NVL(rec.WIDTH,0);
          ELSIF rec.ROWTYPE='COLGROUP' THEN
            -- Startposition of next column
            nColStart:=nMasterColStart+NVL(rec.WIDTH,0);
          END IF;
        END LOOP;
      END;

      PROCEDURE PR_PROCESS_COLUMN_HEADER IS
        nMasterColStart NUMBER;
      BEGIN
        FOR rec IN crColumnsCH LOOP
          IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
            -- column-Header in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Column-Header');
            END IF;

            PR_PROCESS_PART(rReport.rColumnHeaderRegion,
                            i_vcWhenExpression,
                            nColStart,
                            rec.Width,
                            i_nHeight,
                            rec.CH_HEIGHT,
                            rec.CH_BOX_TOP,
                            rec.CH_BOX_BOTTOM,
                            rec.CH_BOX_LEFT,
                            rec.CH_BOX_RIGHT,
                            rec.CH_BOX_TOP_COLOR,
                            rec.CH_BOX_BOTTOM_COLOR,
                            rec.CH_BOX_LEFT_COLOR,
                            rec.CH_BOX_RIGHT_COLOR,
                            rec.CH_STYLE,
                            rec.CH_DETAILS);
          END IF;
          IF rec.ROWTYPE='COLGROUP' THEN
            nMasterColStart:=nColStart;
            -- recursively process columngroup
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           i_nProcessType,
                                           i_vcWhenExpression,
                                           i_nHeight+NVL(rec.CH_HEIGHT,0),
                                           NVL(rec.CH_HEIGHT,0)
                                          );
          END IF;
          IF rec.ROWTYPE='COL' THEN
            -- Startposition of next column
            nColStart:=nColStart+NVL(rec.WIDTH,0);
          ELSIF rec.ROWTYPE='COLGROUP' THEN
            -- Startposition of next column
            nColStart:=nMasterColStart+NVL(rec.WIDTH,0);
          END IF;
        END LOOP;
      END;

      PROCEDURE PR_PROCESS_DETAIL_CELL IS
      BEGIN
        FOR rec IN crColumnsDC LOOP
          IF rec.ROWTYPE IN ('COL') THEN
            -- detail in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Detail-Cells');
            END IF;
            PR_PROCESS_PART(rReport.rDetailRegion,
                            i_vcWhenExpression,
                            nColStart,
                            rec.Width,
                            0,
                            rec.DC_HEIGHT,
                            rec.DC_BOX_TOP,
                            rec.DC_BOX_BOTTOM,
                            rec.DC_BOX_LEFT,
                            rec.DC_BOX_RIGHT,
                            rec.DC_BOX_TOP_COLOR,
                            rec.DC_BOX_BOTTOM_COLOR,
                            rec.DC_BOX_LEFT_COLOR,
                            rec.DC_BOX_RIGHT_COLOR,
                            rec.DC_STYLE,
                            rec.DC_DETAILS);
            -- Startposition of next column
            nColStart:=nColStart+NVL(rec.WIDTH, 0);
          ELSIF rec.ROWTYPE='COLGROUP' THEN
            -- recursively process columngroup
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           i_nProcessType,
                                           i_vcWhenExpression,
                                           i_nHeight+NVL(rec.DC_HEIGHT,0),
                                           NVL(rec.DC_HEIGHT,0)
                                          );
          END IF;
        END LOOP;
      END;

      PROCEDURE PR_PROCESS_COLUMN_FOOTER IS
        nMasterColStart NUMBER;
        nDetailColStart NUMBER;
        bFound          BOOLEAN:=FALSE;
      BEGIN
        FOR rec IN crColumnsCF LOOP
          IF rec.ROWTYPE='COLGROUP' THEN
            -- recursively process columngroup
            nMasterColStart:=nColStart;
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           i_nProcessType,
                                           i_vcWhenExpression,
                                           NVL(rec.CF_HEIGHT,0),
                                           NVL(rec.CF_HEIGHT,0)
                                          );
            nDetailColStart:=nColStart;
            nColStart:=nMasterColStart;
          ELSIF rec.ROWTYPE='COL' THEN
            nInnerheight:=i_nHeight-i_nLastLevelheight;
          END IF;

          IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
            IF rec.CF_HEIGHT IS NOT NULL THEN
              bFound:=TRUE;
            END IF;
            -- column-footer in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Column-Footer');
            END IF;
            PR_PROCESS_PART(rReport.rColumnFooterRegion,
                            i_vcWhenExpression,
                            nColStart,
                            rec.Width,
                            nInnerheight,
                            rec.CF_HEIGHT,
                            rec.CF_BOX_TOP,
                            rec.CF_BOX_BOTTOM,
                            rec.CF_BOX_LEFT,
                            rec.CF_BOX_RIGHT,
                            rec.CF_BOX_TOP_COLOR,
                            rec.CF_BOX_BOTTOM_COLOR,
                            rec.CF_BOX_LEFT_COLOR,
                            rec.CF_BOX_RIGHT_COLOR,
                            rec.CF_STYLE,
                            rec.CF_DETAILS);
          END IF;
          IF rec.ROWTYPE='COL' THEN
            -- Startposition of next column
            nColStart:=nColStart+NVL(rec.WIDTH,0);
          ELSIF rec.ROWTYPE IN ('COLGROUP') THEN
            nColStart:=nDetailColStart;
          END IF;
        END LOOP;
        IF bFound THEN
          nInnerHeight:=nInnerHeight+i_nHeight;
        END IF;
      END;

      PROCEDURE PR_PROCESS_TABLE_FOOTER IS
        nMasterColStart NUMBER;
        nDetailColStart NUMBER;
        bFound          BOOLEAN:=FALSE;
      BEGIN
        FOR rec IN crColumnsTF LOOP
          IF rec.ROWTYPE='COLGROUP' THEN
            nMasterColStart:=nColStart;
            -- recursively process columngroup
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           i_nProcessType,
                                           i_vcWhenExpression,
                                           NVL(rec.TF_HEIGHT,0),
                                           NVL(rec.TF_HEIGHT,0)
                                          );
            nDetailColStart:=nColStart;
            nColStart:=nMasterColStart;
          ELSIF rec.ROWTYPE='COL' THEN
            nInnerheight:=i_nHeight-i_nLastLevelheight;
          END IF;
          IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
            IF rec.TF_HEIGHT IS NOT NULL THEN
              bFound:=TRUE;
            END IF;
            -- table-footer in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Table-Footer');
            END IF;
            PR_PROCESS_PART(rReport.rSummaryRegion,
                            i_vcWhenExpression,
                            nColStart,
                            rec.Width,
                            nInnerheight,
                            rec.TF_HEIGHT,
                            rec.TF_BOX_TOP,
                            rec.TF_BOX_BOTTOM,
                            rec.TF_BOX_LEFT,
                            rec.TF_BOX_RIGHT,
                            rec.TF_BOX_TOP_COLOR,
                            rec.TF_BOX_BOTTOM_COLOR,
                            rec.TF_BOX_LEFT_COLOR,
                            rec.TF_BOX_RIGHT_COLOR,
                            rec.TF_STYLE,
                            rec.TF_DETAILS);
          END IF;
          IF rec.ROWTYPE='COL' THEN
            -- Startposition of next column
            nColStart:=nColStart+NVL(rec.WIDTH,0);
          END IF;
        END LOOP;
        IF bFound THEN
          nInnerHeight:=nInnerHeight+i_nHeight;
        END IF;
      END;

      PROCEDURE PR_PROCESS_GROUP_HEADER IS
        iMatchGroup PLS_INTEGER;
      BEGIN
        FOR rec IN crColumnsGH    LOOP
          IF rec.ROWTYPE IN ('COL') THEN
            FOR rec2 IN crColumnsGroup(rec.COL_GH) LOOP
              IF rec2.NAME=i_vcGroupName THEN
                IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
                  nColStartForGroup(rec2.NAME):=nTempColStart;
                END IF;
                -- detail in column
                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-Headers for group ' || rec2.NAME);
                END IF;
                iMatchGroup:=NULL;
                FOR i IN 1..rReport.lGroups.COUNT LOOP
                  IF rReport.lGroups(i).vcName=rec2.NAME THEN
                    iMatchGroup:=i;
                    EXIT;
                  END IF;
                END LOOP;
                IF iMatchGroup IS NOT NULL THEN
                  -- find the matching region
                  PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupHeader,
                                  rec.WHEN_EXPRESSION,
                                  nColStartForGroup(rec2.NAME),
                                  rec.WIDTH,
                                  i_nHeight,
                                  rec2.HEIGHT,
                                  rec2.BOX_TOP,
                                  rec2.BOX_BOTTOM,
                                  rec2.BOX_LEFT,
                                  rec2.BOX_RIGHT,
                                  rec2.BOX_TOP_COLOR,
                                  rec2.BOX_BOTTOM_COLOR,
                                  rec2.BOX_LEFT_COLOR,
                                  rec2.BOX_RIGHT_COLOR,
                                  rec2.STYLE,
                                  rec2.DETAILS);
                END IF;
                nColStartForGroup(rec2.NAME):=nColStartForGroup(rec2.NAME)+rec.WIDTH;
              END IF;
            END LOOP;
         ELSIF rec.ROWTYPE='COLGROUP' THEN
            FOR rec2 IN crColumnsGroup(rec.COLGROUP_GH) LOOP
              IF rec2.NAME=i_vcGroupName THEN
                IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
                  nColStartForGroup(rec2.NAME):=nTempColStart;
                END IF;
                -- detail in column
                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-Headers for group ' || rec2.NAME);
                END IF;
                iMatchGroup:=NULL;
                FOR i IN 1..rReport.lGroups.COUNT LOOP
                  IF rReport.lGroups(i).vcName=rec2.NAME THEN
                    iMatchGroup:=i;
                    EXIT;
                  END IF;
                END LOOP;
                IF iMatchGroup IS NOT NULL THEN
                  -- find the matching region
                  PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupHeader,
                                  rec.WHEN_EXPRESSION,
                                  nColStartForGroup(rec2.NAME),
                                  rec.WIDTH,
                                  i_nHeight,
                                  rec2.HEIGHT,
                                  rec2.BOX_TOP,
                                  rec2.BOX_BOTTOM,
                                  rec2.BOX_LEFT,
                                  rec2.BOX_RIGHT,
                                  rec2.BOX_TOP_COLOR,
                                  rec2.BOX_BOTTOM_COLOR,
                                  rec2.BOX_LEFT_COLOR,
                                  rec2.BOX_RIGHT_COLOR,
                                  rec2.STYLE,
                                  rec2.DETAILS);
                END IF;
                nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                               i_nProcessType,
                                               rec.WHEN_EXPRESSION,
                                               NVL(rec2.HEIGHT,0),
                                               NVL(rec2.HEIGHT,0),
                                               rec2.NAME
                                              );
              END IF;
            END LOOP;
          END IF;
        END LOOP;
      END;

      PROCEDURE PR_PROCESS_GROUP_FOOTER IS
        iMatchGroup     PLS_INTEGER;
        nMasterColStart NUMBER;
        nDetailColStart NUMBER;
        bFound          BOOLEAN:=FALSE;
      BEGIN
        FOR rec IN crColumnsGF    LOOP
          IF rec.ROWTYPE IN ('COL') THEN
            FOR rec2 IN crColumnsGroup(rec.COL_GF) LOOP
              IF rec2.NAME=i_vcGroupName THEN
                nInnerheight:=i_nHeight-i_nLastLevelheight;
                IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
                  nColStartForGroup(rec2.NAME):=nTempColStart;
                END IF;
                -- detail in column
                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-footers for group ' || rec2.NAME);
                END IF;
                iMatchGroup:=NULL;
                FOR i IN 1..rReport.lGroups.COUNT LOOP
                  IF rReport.lGroups(i).vcName=rec2.NAME THEN
                    iMatchGroup:=i;
                    EXIT;
                  END IF;
                END LOOP;
                IF iMatchGroup IS NOT NULL THEN
                  IF rec2.HEIGHT IS NOT NULL THEN
                    bFound:=TRUE;
                  END IF;
                  -- find the matching region
                  PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupFooter,
                                  rec.WHEN_EXPRESSION,
                                  nColStartForGroup(rec2.NAME),
                                  rec.WIDTH,
                                  nInnerHeight,
                                  rec2.HEIGHT,
                                  rec2.BOX_TOP,
                                  rec2.BOX_BOTTOM,
                                  rec2.BOX_LEFT,
                                  rec2.BOX_RIGHT,
                                  rec2.BOX_TOP_COLOR,
                                  rec2.BOX_BOTTOM_COLOR,
                                  rec2.BOX_LEFT_COLOR,
                                  rec2.BOX_RIGHT_COLOR,
                                  rec2.STYLE,
                                  rec2.DETAILS);
                END IF;
                nColStartForGroup(rec2.NAME):=nColStartForGroup(rec2.NAME)+rec.WIDTH;
              END IF;
            END LOOP;
          ELSIF rec.ROWTYPE='COLGROUP' THEN
            FOR rec2 IN crColumnsGroup(rec.COLGROUP_GF) LOOP
              IF rec2.NAME=i_vcGroupName THEN
                IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
                  nColStartForGroup(rec2.NAME):=nTempColStart;
                END IF;
                nMasterColStart:=nColStartForGroup(rec2.NAME);

                nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                               i_nProcessType,
                                               rec.WHEN_EXPRESSION,
                                               NVL(rec2.HEIGHT,0),
                                               NVL(rec2.HEIGHT,0),
                                               rec2.NAME
                                              );
                nDetailColStart:=nColStartForGroup(rec2.NAME);
                nColStartForGroup(rec2.NAME):=nMasterColStart;

                -- detail in column
                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-footers for group ' || rec2.NAME);
                END IF;
                iMatchGroup:=NULL;
                FOR i IN 1..rReport.lGroups.COUNT LOOP
                  IF rReport.lGroups(i).vcName=rec2.NAME THEN
                    iMatchGroup:=i;
                    EXIT;
                  END IF;
                END LOOP;
                IF iMatchGroup IS NOT NULL THEN
                  IF rec2.HEIGHT IS NOT NULL THEN
                    bFound:=TRUE;
                  END IF;
                  -- find the matching region
                  PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupFooter,
                                  rec.WHEN_EXPRESSION,
                                  nColStartForGroup(rec2.NAME),
                                  rec.WIDTH,
                                  nInnerheight,
                                  rec2.HEIGHT,
                                  rec2.BOX_TOP,
                                  rec2.BOX_BOTTOM,
                                  rec2.BOX_LEFT,
                                  rec2.BOX_RIGHT,
                                  rec2.BOX_TOP_COLOR,
                                  rec2.BOX_BOTTOM_COLOR,
                                  rec2.BOX_LEFT_COLOR,
                                  rec2.BOX_RIGHT_COLOR,
                                  rec2.STYLE,
                                  rec2.DETAILS);
                END IF;
                nColStartForGroup(rec2.NAME):=nDetailColStart;
              END IF;
            END LOOP;
          END IF;
        END LOOP;
        IF bFound THEN
          nInnerHeight:=nInnerHeight+i_nHeight;
        END IF;
      END;

    BEGIN
      IF i_nProcessType=TYPE_TABLE_HEADER THEN
        PR_PROCESS_TABLE_HEADER;
      ELSIF i_nProcessType=TYPE_COLUMN_HEADER THEN
        PR_PROCESS_COLUMN_HEADER;
      ELSIF i_nProcessType=TYPE_DETAIL_CELL THEN
        PR_PROCESS_DETAIL_CELL;
      ELSIF i_nProcessType=TYPE_COLUMN_FOOTER THEN
        PR_PROCESS_COLUMN_FOOTER;
      ELSIF i_nProcessType=TYPE_TABLE_FOOTER THEN
        PR_PROCESS_TABLE_FOOTER;
      ELSIF i_nProcessType=TYPE_GROUP_HEADER THEN
        PR_PROCESS_GROUP_HEADER;
      ELSIF i_nProcessType=TYPE_GROUP_FOOTER THEN
        PR_PROCESS_GROUP_FOOTER;
      END IF;
      RETURN nInnerHeight;
    END;

    PROCEDURE PR_INITIALIZE_REPORT IS
    BEGIN
      -- copy styles
      vcStyle:=i_rReport.lStyles.FIRST;
      LOOP
        EXIT WHEN vcStyle IS NULL;
        rReport.lStyles(vcStyle):=i_rReport.lStyles(vcStyle);
        vcStyle:=i_rReport.lStyles.NEXT(vcStyle);
      END LOOP;
      
      -- find subdataset
      IF i_rReport.lDatasets.EXISTS(i_vcSubdataset) THEN
        rReport.vcQuery:=i_rReport.lDatasets(i_vcSubdataset).vcQuery;
        -- take over the groups from the subdataset
        rReport.lGroups:=i_rReport.lDatasets(i_vcSubdataset).lGroups;
        FOR i IN 1..rReport.lGroups.COUNT LOOP
          -- set layouting options
          rReport.lGroups(i).vcStartOnNewPage:=PK_JRXML2PDF_TYPES.NO;
          rReport.lGroups(i).vcResetPageNumber:=PK_JRXML2PDF_TYPES.NO;
          rReport.lGroups(i).vcReprintHeader:=PK_JRXML2PDF_TYPES.NO;
          rReport.lGroups(i).nMinHeightForPage:=NULL;
          rReport.lGroupExpressions(i):=NULL;

        END LOOP;
      END IF;
      -- take over resources and locale-data
      rReport.rLocaleData:=i_rReport.rLocaleData;
      rReport.lResources:=i_rReport.lResources;

    END;

    PROCEDURE PR_PROCESS_TH_COLUMNS IS
      nInnerheight    NUMBER;
      nMasterColStart NUMBER;
    BEGIN
      nTempColStart:=nColStart;

      FOR rec IN crColumnsTH LOOP
        IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
          -- table-header in column,
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Table-Header');
          END IF;
          PR_PROCESS_PART(rReport.rTitleRegion,
                          rec.WHEN_EXPRESSION,
                          nColStart,
                          rec.Width,
                          0,
                          rec.TH_HEIGHT,
                          rec.TH_BOX_TOP,
                          rec.TH_BOX_BOTTOM,
                          rec.TH_BOX_LEFT,
                          rec.TH_BOX_RIGHT,
                          rec.TH_BOX_TOP_COLOR,
                          rec.TH_BOX_BOTTOM_COLOR,
                          rec.TH_BOX_LEFT_COLOR,
                          rec.TH_BOX_RIGHT_COLOR,
                          rec.TH_STYLE,
                          rec.TH_DETAILS
                         );
        END IF;
        -- groups
        IF rec.ROWTYPE='COLGROUP' THEN
          nMasterColStart:=nColStart;
          -- recursively process columngroup
          nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                         TYPE_TABLE_HEADER,
                                         rec.WHEN_EXPRESSION,
                                         NVL(rec.TH_HEIGHT,0),
                                         NVL(rec.TH_HEIGHT,0)
                                        );
        END IF;
        IF rec.ROWTYPE IN ('COL') THEN
          -- Startposition of next column
          nColStart:=nColStart+rec.WIDTH;
        ELSIF rec.ROWTYPE IN ('COLGROUP') THEN
          nColStart:=nMasterColStart+rec.WIDTH;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_CH_COLUMNS IS
      nInnerheight   NUMBER;
      nMasterColStart NUMBER;
    BEGIN
      nColStart:=nTempColStart;
      FOR rec IN crColumnsCH LOOP
        IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
          -- column-Header in column
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Column-Header');
          END IF;
          PR_PROCESS_PART(rReport.rColumnHeaderRegion,
                          rec.WHEN_EXPRESSION,
                          nColStart,
                          rec.Width,
                          0,
                          rec.CH_HEIGHT,
                          rec.CH_BOX_TOP,
                          rec.CH_BOX_BOTTOM,
                          rec.CH_BOX_LEFT,
                          rec.CH_BOX_RIGHT,
                          rec.CH_BOX_TOP_COLOR,
                          rec.CH_BOX_BOTTOM_COLOR,
                          rec.CH_BOX_LEFT_COLOR,
                          rec.CH_BOX_RIGHT_COLOR,
                          rec.CH_STYLE,
                          rec.CH_DETAILS);
        END IF;
        -- groups
        IF rec.ROWTYPE='COLGROUP' THEN
          nMasterColStart:=nColStart;
          nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                         TYPE_COLUMN_HEADER,
                                         rec.WHEN_EXPRESSION,
                                         NVL(rec.CH_HEIGHT,0),
                                         NVL(rec.CH_HEIGHT,0)
                                        );
        END IF;
        IF rec.ROWTYPE IN ('COL') THEN
          -- Startposition of next column
          nColStart:=nColStart+rec.WIDTH;
        ELSIF rec.ROWTYPE IN ('COLGROUP') THEN
          -- Startposition of next column
          nColStart:=nMasterColStart+rec.WIDTH;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_DC_COLUMNS IS
      nInnerheight NUMBER;
    BEGIN
      nColStart:=nTempColStart;
      FOR rec IN crColumnsDC LOOP
        IF rec.ROWTYPE IN ('COL') THEN
          -- detail in column
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Detail-Cells');
          END IF;
          PR_PROCESS_PART(rReport.rDetailRegion,
                          rec.WHEN_EXPRESSION,
                          nColStart,
                          rec.Width,
                          0,
                          rec.DC_HEIGHT,
                          rec.DC_BOX_TOP,
                          rec.DC_BOX_BOTTOM,
                          rec.DC_BOX_LEFT,
                          rec.DC_BOX_RIGHT,
                          rec.DC_BOX_TOP_COLOR,
                          rec.DC_BOX_BOTTOM_COLOR,
                          rec.DC_BOX_LEFT_COLOR,
                          rec.DC_BOX_RIGHT_COLOR,
                          rec.DC_STYLE,
                          rec.DC_DETAILS);
        ELSIF rec.ROWTYPE='COLGROUP' THEN
          nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                         TYPE_DETAIL_CELL,
                                         rec.WHEN_EXPRESSION,
                                         0,
                                         0
                                        );
        END IF;
        IF rec.ROWTYPE IN ('COL') THEN
          -- Startposition of next column
          nColStart:=nColStart+rec.WIDTH;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_CF_COLUMNS IS
      nMasterColStart NUMBER;
      nDetailColStart NUMBER;
      nInnerheight    NUMBER:=0;
    BEGIN
      nColStart:=nTempColStart;
      FOR rec IN crColumnsCF LOOP
        IF rec.ROWTYPE='COLGROUP' THEN
          nMasterColStart:=nColStart;
          nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                         TYPE_COLUMN_FOOTER,
                                         rec.WHEN_EXPRESSION,
                                         NVL(rec.CF_HEIGHT,0),
                                         NVL(rec.CF_HEIGHT,0)
                                        );
          nDetailColStart:=nColStart;
          nColStart:=nMasterColStart;
        ELSIF rec.ROWTYPE ='COL' THEN
          nInnerheight:=0;
        END IF;
        IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
          -- column-footer in column
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Column-Footer');
          END IF;
          PR_PROCESS_PART(rReport.rColumnFooterRegion,
                          rec.WHEN_EXPRESSION,
                          nColStart,
                          rec.Width,
                          nInnerheight,
                          rec.CF_HEIGHT,
                          rec.CF_BOX_TOP,
                          rec.CF_BOX_BOTTOM,
                          rec.CF_BOX_LEFT,
                          rec.CF_BOX_RIGHT,
                          rec.CF_BOX_TOP_COLOR,
                          rec.CF_BOX_BOTTOM_COLOR,
                          rec.CF_BOX_LEFT_COLOR,
                          rec.CF_BOX_RIGHT_COLOR,
                          rec.CF_STYLE,
                          rec.CF_DETAILS);
        END IF;
        IF rec.ROWTYPE IN ('COL') THEN
          -- Startposition of next column
          nColStart:=nColStart+rec.WIDTH;
        ELSIF rec.ROWTYPE IN ('COLGROUP') THEN
          nColStart:=nMasterColStart+rec.WIDTH;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_TF_COLUMNS IS
      nMasterColStart NUMBER;
      nDetailColStart NUMBER;
      nInnerheight    NUMBER:=0;
    BEGIN
      nColStart:=nTempColStart;
      FOR rec IN crColumnsTF LOOP
        IF rec.ROWTYPE='COLGROUP' THEN
          nMasterColStart:=nColStart;
          nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                         TYPE_TABLE_FOOTER,
                                         rec.WHEN_EXPRESSION,
                                         NVL(rec.TF_HEIGHT,0),
                                         NVL(rec.TF_HEIGHT,0)
                                        );
          nDetailColStart:=nColStart;
          nColStart:=nMasterColStart;
        ELSIF rec.ROWTYPE ='COL' THEN
          nInnerheight:=0;
        END IF;
        IF rec.ROWTYPE IN ('COL', 'COLGROUP') THEN
          -- table-footer in column
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Table-Footer');
          END IF;
          PR_PROCESS_PART(rReport.rSummaryRegion,
                          rec.WHEN_EXPRESSION,
                          nColStart,
                          rec.Width,
                          nInnerheight,
                          rec.TF_HEIGHT,
                          rec.TF_BOX_TOP,
                          rec.TF_BOX_BOTTOM,
                          rec.TF_BOX_LEFT,
                          rec.TF_BOX_RIGHT,
                          rec.TF_BOX_TOP_COLOR,
                          rec.TF_BOX_BOTTOM_COLOR,
                          rec.TF_BOX_LEFT_COLOR,
                          rec.TF_BOX_RIGHT_COLOR,
                          rec.TF_STYLE,
                          rec.TF_DETAILS);
        END IF;
        IF rec.ROWTYPE IN ('COL') THEN
          -- Startposition of next column
          nColStart:=nColStart+rec.WIDTH;
        ELSIF rec.ROWTYPE IN ('COLGROUP') THEN
          nColStart:=nMasterColStart+rec.WIDTH;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_GH_COLUMNS IS
      nInnerheight NUMBER;
    BEGIN
      FOR rec IN crColumnsGH LOOP
        IF rec.ROWTYPE IN ('COL') THEN
          FOR rec2 IN crColumnsGroup(rec.COL_GH) LOOP
            IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
              nColStartForGroup(rec2.NAME):=nTempColStart;
            END IF;
            -- detail in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-Headers for group ' || rec2.NAME);
            END IF;
            iMatchGroup:=NULL;
            FOR i IN 1..rReport.lGroups.COUNT LOOP
              IF rReport.lGroups(i).vcName=rec2.NAME THEN
                iMatchGroup:=i;
                EXIT;
              END IF;
            END LOOP;
            IF iMatchGroup IS NOT NULL THEN
              -- find the matching region
              PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupHeader,
                              rec.WHEN_EXPRESSION,
                              nColStartForGroup(rec2.NAME),
                              rec.WIDTH,
                              0,
                              rec2.HEIGHT,
                              rec2.BOX_TOP,
                              rec2.BOX_BOTTOM,
                              rec2.BOX_LEFT,
                              rec2.BOX_RIGHT,
                              rec2.BOX_TOP_COLOR,
                              rec2.BOX_BOTTOM_COLOR,
                              rec2.BOX_LEFT_COLOR,
                              rec2.BOX_RIGHT_COLOR,
                              rec2.STYLE,
                              rec2.DETAILS);
            END IF;
            nColStartForGroup(rec2.NAME):=nColStartForGroup(rec2.NAME)+rec.WIDTH;
          END LOOP;
        ELSIF rec.ROWTYPE='COLGROUP' THEN
          FOR rec2 IN crColumnsGroup(rec.COLGROUP_GH) LOOP
            IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
              nColStartForGroup(rec2.NAME):=nTempColStart;
            END IF;
            -- detail in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-Headers for group ' || rec2.NAME);
            END IF;
            iMatchGroup:=NULL;
            FOR i IN 1..rReport.lGroups.COUNT LOOP
              IF rReport.lGroups(i).vcName=rec2.NAME THEN
                iMatchGroup:=i;
                EXIT;
              END IF;
            END LOOP;
            IF iMatchGroup IS NOT NULL THEN
              -- find the matching region
              PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupHeader,
                              rec.WHEN_EXPRESSION,
                              nColStartForGroup(rec2.NAME),
                              rec.WIDTH,
                              0,
                              rec2.HEIGHT,
                              rec2.BOX_TOP,
                              rec2.BOX_BOTTOM,
                              rec2.BOX_LEFT,
                              rec2.BOX_RIGHT,
                              rec2.BOX_TOP_COLOR,
                              rec2.BOX_BOTTOM_COLOR,
                              rec2.BOX_LEFT_COLOR,
                              rec2.BOX_RIGHT_COLOR,
                              rec2.STYLE,
                              rec2.DETAILS);
            END IF;
            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           TYPE_GROUP_HEADER,
                                           rec.WHEN_EXPRESSION,
                                           NVL(rec2.HEIGHT,0),
                                           NVL(rec2.HEIGHT,0),
                                           rec2.NAME
                                          );
          END LOOP;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_PROCESS_GF_COLUMNS IS
      nMasterColStart NUMBER;
      nDetailColStart NUMBER;
      nInnerheight    NUMBER:=0;
    BEGIN
      nColStartForGroup.DELETE;
      FOR rec IN crColumnsGF LOOP
        IF rec.ROWTYPE IN ('COL') THEN
          FOR rec2 IN crColumnsGroup(rec.COL_GF) LOOP
            nInnerheight:=0;
            IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
              nColStartForGroup(rec2.NAME):=nTempColStart;
            END IF;
            -- detail in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-footers for group ' || rec2.NAME);
            END IF;
            iMatchGroup:=NULL;
            FOR i IN 1..rReport.lGroups.COUNT LOOP
              IF rReport.lGroups(i).vcName=rec2.NAME THEN
                iMatchGroup:=i;
                EXIT;
              END IF;
            END LOOP;
            IF iMatchGroup IS NOT NULL THEN
              -- find the matching region
              PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupFooter,
                              rec.WHEN_EXPRESSION,
                              nColStartForGroup(rec2.NAME),
                              rec.WIDTH,
                              nInnerheight,
                              rec2.HEIGHT,
                              rec2.BOX_TOP,
                              rec2.BOX_BOTTOM,
                              rec2.BOX_LEFT,
                              rec2.BOX_RIGHT,
                              rec2.BOX_TOP_COLOR,
                              rec2.BOX_BOTTOM_COLOR,
                              rec2.BOX_LEFT_COLOR,
                              rec2.BOX_RIGHT_COLOR,
                              rec2.STYLE,
                              rec2.DETAILS);
            END IF;
            nColStartForGroup(rec2.NAME):=nColStartForGroup(rec2.NAME)+rec.WIDTH;
          END LOOP;
        ELSIF rec.ROWTYPE='COLGROUP' THEN
          FOR rec2 IN crColumnsGroup(rec.COLGROUP_GF) LOOP
            IF NOT nColStartForGroup.EXISTS(rec2.NAME) THEN
              nColStartForGroup(rec2.NAME):=nTempColStart;
            END IF;
            nMasterColStart:=nColStartForGroup(rec2.NAME);

            nInnerheight:=FK_PROCESS_GROUP(rec.COLGROUP,
                                           TYPE_GROUP_FOOTER,
                                           rec.WHEN_EXPRESSION,
                                           NVL(rec2.HEIGHT,0),
                                           NVL(rec2.HEIGHT,0),
                                           rec2.NAME
                                          );
            nDetailColStart:=nColStartForGroup(rec2.NAME);
            nColStartForGroup(rec2.NAME):=nMasterColStart;
            -- detail in column
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group-footers for group ' || rec2.NAME);
            END IF;
            iMatchGroup:=NULL;
            FOR i IN 1..rReport.lGroups.COUNT LOOP
              IF rReport.lGroups(i).vcName=rec2.NAME THEN
                iMatchGroup:=i;
                EXIT;
              END IF;
            END LOOP;
            IF iMatchGroup IS NOT NULL THEN
              -- find the matching region
              PR_PROCESS_PART(rReport.lGroups(iMatchGroup).rGroupFooter,
                              rec.WHEN_EXPRESSION,
                              nColStartForGroup(rec2.NAME),
                              rec.WIDTH,
                              nInnerheight,
                              rec2.HEIGHT,
                              rec2.BOX_TOP,
                              rec2.BOX_BOTTOM,
                              rec2.BOX_LEFT,
                              rec2.BOX_RIGHT,
                              rec2.BOX_TOP_COLOR,
                              rec2.BOX_BOTTOM_COLOR,
                              rec2.BOX_LEFT_COLOR,
                              rec2.BOX_RIGHT_COLOR,
                              rec2.STYLE,
                              rec2.DETAILS);
            END IF;
            nColStartForGroup(rec2.NAME):=nMasterColStart+rec.WIDTH;
          END LOOP;
        END IF;
      END LOOP;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Load table as subreport' || i_rReport.lStyles.COUNT);
    END IF;

    PR_INITIALIZE_REPORT;

    PR_PROCESS_TH_COLUMNS;

    PR_PROCESS_CH_COLUMNS;

    PR_PROCESS_DC_COLUMNS;

    PR_PROCESS_CF_COLUMNS;

    PR_PROCESS_TF_COLUMNS;

    PR_PROCESS_GH_COLUMNS;

    PR_PROCESS_GF_COLUMNS;

    -- dummy-data
    rReport.nPageWidth          :=nColStart;
    rReport.nPageHeight         :=9999;
    rReport.nLeftMargin         :=0;
    rReport.nRightMargin        :=0;
    rReport.nTopMargin          :=0;
    rReport.nBottomMargin       :=0;
    rReport.vcFloatColumnFooter :=PK_JRXML2PDF_TYPES.YES;
    rReport.nTyp                :=PK_JRXML2PDF_TYPES.REPORT_TYP_TABLE;
    -- push to cache, used later
    lReportCache(i_vcReportName):=rReport;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LOAD_BAND(io_rReport              IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                        i_oBand                 IN      XMLTYPE,
                        i_vcTyp                 IN      PK_JRXML2PDF_TYPES.tAttribute,
                        i_nHeight               IN      NUMBER,
                        i_vcPrintWhenExpression IN      PK_JRXML2PDF_TYPES.tExpression,
                        i_vcSplitType           IN      PK_JRXML2PDF_TYPES.tSplitType)
  RETURN PK_JRXML2PDF_TYPES.tBand IS
    CURSOR crSubFrames IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             BGCOLOR,
             FGCOLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             XMLELEMENT("element", FRAME) FRAME,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             END OPAQUE,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE
        FROM XMLTABLE('/element/frame' PASSING i_oBand
                                       COLUMNS X                NUMBER         PATH './reportElement/@x',
                                               Y                NUMBER         PATH './reportElement/@y',
                                               WIDTH            NUMBER         PATH './reportElement/@width',
                                               HEIGHT           NUMBER         PATH './reportElement/@height',
                                               BGCOLOR          VARCHAR2(20)   PATH './reportElement/@backcolor',
                                               FGCOLOR          VARCHAR2(20)   PATH './reportElement/@forecolor',
                                               STYLE            VARCHAR2(255)  PATH './reportElement/@style',
                                               MODUS            VARCHAR2(20)   PATH './reportElement/@mode',
                                               WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                               POSITION_TYPE    VARCHAR2(20)   PATH './reportElement/@positionType',
                                               BOX_TOP          VARCHAR2(20)   PATH './box/topPen/@lineWidth',
                                               BOX_BOTTOM       VARCHAR2(20)   PATH './box/bottomPen/@lineWidth',
                                               BOX_LEFT         VARCHAR2(20)   PATH './box/leftPen/@lineWidth',
                                               BOX_RIGHT        VARCHAR2(20)   PATH './box/rightPen/@lineWidth',
                                               BOX_TOP_COLOR    VARCHAR2(20)   PATH './box/topPen/@lineColor',
                                               BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './box/bottomPen/@lineColor',
                                               BOX_LEFT_COLOR   VARCHAR2(20)   PATH './box/leftPen/@lineColor',
                                               BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './box/rightPen/@lineColor',
                                               BOX_LINEWIDTH    VARCHAR2(10)   PATH './box/pen/@lineWidth',
                                               BOX_COLOR        VARCHAR2(20)   PATH './box/pen/@lineColor',
                                               FRAME            XMLTYPE        PATH  './*'
                     );

    CURSOR crLines IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(FGCOLOR2, FGCOLOR) FGCOLOR,
             LINEWIDTH,
             CASE WHEN     STRETCH_TYPE='RelativeToBandHeight'
                       AND WIDTH='1' THEN
               'Y'
             ELSE
               'N'
             END STRETCH,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE
        FROM XMLTABLE('/element/line' PASSING i_oBand
                                      COLUMNS X               NUMBER         PATH './reportElement/@x',
                                              Y               NUMBER         PATH './reportElement/@y',
                                              WIDTH           NUMBER         PATH './reportElement/@width',
                                              HEIGHT          NUMBER         PATH './reportElement/@height',
                                              FGCOLOR         VARCHAR2(20)   PATH './reportElement/@forecolor',
                                              STYLE           VARCHAR2(255)  PATH './reportElement/@style',
                                              WHEN_EXPRESSION VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                              POSITION_TYPE  VARCHAR2(20)   PATH './reportElement/@positionType',
                                              LINEWIDTH       VARCHAR2(20)   PATH './graphicElement/pen/@lineWidth',
                                              FGCOLOR2        VARCHAR2(20)   PATH './graphicElement/pen/@lineColor',
                                              STRETCH_TYPE    VARCHAR2(255)  PATH './reportElement/@stretchType'
                     );
    CURSOR crRectangles IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(FGCOLOR2, FGCOLOR) FGCOLOR,
             BGCOLOR,
             LINEWIDTH,
             CASE WHEN STRETCH_TYPE='RelativeToBandHeight' THEN
               'Y'
             ELSE
               'N'
             END STRETCH,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'             
             END OPAQUE,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE
        FROM XMLTABLE('/element/rectangle' PASSING i_oBand
                                           COLUMNS X               NUMBER         PATH './reportElement/@x',
                                                   Y               NUMBER         PATH './reportElement/@y',
                                                   WIDTH           NUMBER         PATH './reportElement/@width',
                                                   HEIGHT          NUMBER         PATH './reportElement/@height',
                                                   STYLE           VARCHAR2(255)  PATH './reportElement/@style',
                                                   MODUS           VARCHAR2(20)   PATH './reportElement/@mode',
                                                   FGCOLOR         VARCHAR2(20)   PATH './reportElement/@forecolor',
                                                   BGCOLOR         VARCHAR2(20)   PATH './reportElement/@backcolor',
                                                   WHEN_EXPRESSION VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                   POSITION_TYPE  VARCHAR2(20)   PATH './reportElement/@positionType',
                                                   LINEWIDTH       VARCHAR2(20)   PATH './graphicElement/pen/@lineWidth',
                                                   FGCOLOR2        VARCHAR2(20)   PATH './graphicElement/pen/@lineColor',
                                                   STRETCH_TYPE    VARCHAR2(255)  PATH './reportElement/@stretchType'
                     );

    CURSOR crStaticText IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             BGCOLOR,
             FGCOLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             TOP_PADDING,
             BOTTOM_PADDING,
             LEFT_PADDING,
             RIGHT_PADDING,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             FONT,
             FONT_SIZE,
             ALIGNMENT,
             VERTICAL_ALIGN,
             TEXT,
             CASE WHEN STRETCH_TYPE='RelativeToBandHeight' THEN
               'Y'
             ELSE
               'N'
             END STRETCH,
             STYLE,
             CASE WHEN FONT_BOLD='true' THEN
               'Y'
             WHEN FONT_BOLD='false' THEN
               'N'
             END FONT_BOLD,
             CASE WHEN FONT_ITALIC='true' THEN
               'Y'
             WHEN FONT_ITALIC='false' THEN
               'N'
             END FONT_ITALIC,
             WHEN_EXPRESSION,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             ROTATION
        FROM XMLTABLE('/element/staticText'  PASSING i_oBand
                                            COLUMNS X                NUMBER         PATH './reportElement/@x',
                                                    Y                NUMBER         PATH './reportElement/@y',
                                                    WIDTH            NUMBER         PATH './reportElement/@width',
                                                    HEIGHT           NUMBER         PATH './reportElement/@height',
                                                    BGCOLOR          VARCHAR2(20)   PATH './reportElement/@backcolor',
                                                    FGCOLOR          VARCHAR2(20)   PATH './reportElement/@forecolor',
                                                    STYLE            VARCHAR2(255)  PATH './reportElement/@style',
                                                    MODUS            VARCHAR2(20)   PATH './reportElement/@mode',
                                                    WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                    POSITION_TYPE    VARCHAR2(20)   PATH './reportElement/@positionType',
                                                    BOX_TOP          VARCHAR2(20)   PATH './box/topPen/@lineWidth',
                                                    BOX_BOTTOM       VARCHAR2(20)   PATH './box/bottomPen/@lineWidth',
                                                    BOX_LEFT         VARCHAR2(20)   PATH './box/leftPen/@lineWidth',
                                                    BOX_RIGHT        VARCHAR2(20)   PATH './box/rightPen/@lineWidth',
                                                    TOP_PADDING      VARCHAR2(20)   PATH './box/@topPadding',
                                                    BOTTOM_PADDING   VARCHAR2(20)   PATH './box/@bottomPadding',
                                                    LEFT_PADDING     VARCHAR2(20)   PATH './box/@leftPadding',
                                                    RIGHT_PADDING    VARCHAR2(20)   PATH './box/@rightPadding',
                                                    BOX_TOP_COLOR    VARCHAR2(20)   PATH './box/topPen/@lineColor',
                                                    BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './box/bottomPen/@lineColor',
                                                    BOX_LEFT_COLOR   VARCHAR2(20)   PATH './box/leftPen/@lineColor',
                                                    BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './box/rightPen/@lineColor',
                                                    BOX_LINEWIDTH    VARCHAR2(10)   PATH './box/pen/@lineWidth',
                                                    BOX_COLOR        VARCHAR2(20)   PATH './box/pen/@lineColor',
                                                    FONT             VARCHAR2(255)  PATH './textElement/font/@fontName',
                                                    FONT_SIZE        VARCHAR2(255)  PATH './textElement/font/@size',
                                                    FONT_BOLD        VARCHAR2(5)    PATH './textElement/font/@isBold',
                                                    FONT_ITALIC      VARCHAR2(5)    PATH './textElement/font/@isItalic',
                                                    ALIGNMENT        VARCHAR2(255)  PATH './textElement/@textAlignment',
                                                    VERTICAL_ALIGN   VARCHAR2(255)  PATH './textElement/@verticalAlignment',
                                                    ROTATION         VARCHAR2(20)   PATH './textElement/@rotation',
                                                    TEXT             VARCHAR2(4000) PATH './text',
                                                    STRETCH_TYPE     VARCHAR2(255)  PATH './reportElement/@stretchType'
                      );
    CURSOR crImages IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             IMAGE,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE
        FROM XMLTABLE('/element/image' PASSING i_oBand
                                       COLUMNS X                NUMBER         PATH './reportElement/@x',
                                               Y                NUMBER         PATH './reportElement/@y',
                                               WIDTH            NUMBER         PATH './reportElement/@width',
                                               HEIGHT           NUMBER         PATH './reportElement/@height',
                                               BGCOLOR          VARCHAR2(20)   PATH './reportElement/@backcolor',
                                               FGCOLOR          VARCHAR2(20)   PATH './reportElement/@forecolor',
                                               STYLE            VARCHAR2(255)  PATH './reportElement/@style',
                                               WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                               POSITION_TYPE    VARCHAR2(20)   PATH './reportElement/@positionType',
                                               BOX_TOP          VARCHAR2(20)   PATH './box/topPen/@lineWidth',
                                               BOX_BOTTOM       VARCHAR2(20)   PATH './box/bottomPen/@lineWidth',
                                               BOX_LEFT         VARCHAR2(20)   PATH './box/leftPen/@lineWidth',
                                               BOX_RIGHT        VARCHAR2(20)   PATH './box/rightPen/@lineWidth',
                                               BOX_TOP_COLOR    VARCHAR2(20)   PATH './box/topPen/@lineColor',
                                               BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './box/bottomPen/@lineColor',
                                               BOX_LEFT_COLOR   VARCHAR2(20)   PATH './box/leftPen/@lineColor',
                                               BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './box/rightPen/@lineColor',
                                               BOX_LINEWIDTH    VARCHAR2(10)   PATH './box/pen/@lineWidth',
                                               BOX_COLOR        VARCHAR2(20)   PATH './box/pen/@lineColor',
                                               IMAGE            VARCHAR2(2000) PATH './imageExpression'
                     );


    CURSOR crTextFields IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             BGCOLOR,
             FGCOLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             TOP_PADDING,
             BOTTOM_PADDING,
             LEFT_PADDING,
             RIGHT_PADDING,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             FONT,
             FONT_SIZE,
             ALIGNMENT,
             VERTICAL_ALIGN,
             EXPRESSION,
             PATTERN,
             PATTERN_EXPRESSION,
             CASE WHEN    STRETCH_OVERFLOW='true'
                       OR STRETCH_TYPE='RelativeToBandHeight' THEN
               'Y'
             ELSE
               'N'
            END STRETCH,
            CASE WHEN REPEATED_VALUES='false' THEN
              'N'
            ELSE
              'Y'
            END REPEATED_VALUES,
            CASE WHEN FONT_BOLD='true' THEN
              'Y'
            WHEN FONT_BOLD='false' THEN
              'N'
            END FONT_BOLD,
            CASE WHEN FONT_ITALIC='true' THEN
              'Y'
            WHEN FONT_ITALIC='false' THEN
              'N'
            END FONT_ITALIC,
            STYLE,
            CASE WHEN     STRETCH_OVERFLOW='true'
                      AND ROTATION IS NULL THEN
              'Y'
            ELSE
              'N'
            END STRETCH_OVERFLOW,
            EVALUATION_TIME,
            PK_JRXML2PDF_TYPES.EVALUATION_GROUP,
            WHEN_EXPRESSION,
            CASE WHEN LINE_SPACE='Single' THEN
              '1.17'
            WHEN LINE_SPACE='1_1_2' THEN
              '1.52'
            WHEN LINE_SPACE='Double' THEN
              '1.85'
            WHEN LINE_SPACE='Proportional' THEN
              NVL(LINE_SPACE_SIZE, '1.17')
            END LINE_SPACING,
            CASE WHEN MODUS='Opaque' THEN
              'Y'
            WHEN MODUS='Transparent' THEN
              'N'
            END OPAQUE,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             MARKUP,
             KEY,
             ROTATION
        FROM XMLTABLE('/element/textField' PASSING i_oBand
                                           COLUMNS X                NUMBER           PATH './reportElement/@x',
                                                   Y                NUMBER           PATH './reportElement/@y',
                                                   WIDTH            NUMBER           PATH './reportElement/@width',
                                                   HEIGHT           NUMBER           PATH './reportElement/@height',
                                                   BGCOLOR          VARCHAR2(20)     PATH './reportElement/@backcolor',
                                                   FGCOLOR          VARCHAR2(20)     PATH './reportElement/@forecolor',
                                                   STYLE            VARCHAR2(255)    PATH './reportElement/@style',
                                                   MODUS            VARCHAR2(20)     PATH './reportElement/@mode',
                                                   KEY              VARCHAr2(80)     PATH './reportElement/@key',
                                                   WHEN_EXPRESSION  VARCHAR2(4000)   PATH './reportElement/printWhenExpression',
                                                   POSITION_TYPE    VARCHAR2(20)     PATH './reportElement/@positionType',
                                                   BOX_TOP          VARCHAR2(20)     PATH './box/topPen/@lineWidth',
                                                   BOX_BOTTOM       VARCHAR2(20)     PATH './box/bottomPen/@lineWidth',
                                                   BOX_LEFT         VARCHAR2(20)     PATH './box/leftPen/@lineWidth',
                                                   BOX_RIGHT        VARCHAR2(20)     PATH './box/rightPen/@lineWidth',
                                                   TOP_PADDING      VARCHAR2(20)     PATH './box/@topPadding',
                                                   BOTTOM_PADDING   VARCHAR2(20)     PATH './box/@bottomPadding',
                                                   LEFT_PADDING     VARCHAR2(20)     PATH './box/@leftPadding',
                                                   RIGHT_PADDING    VARCHAR2(20)     PATH './box/@rightPadding',
                                                   BOX_TOP_COLOR    VARCHAR2(20)     PATH './box/topPen/@lineColor',
                                                   BOX_BOTTOM_COLOR VARCHAR2(20)     PATH './box/bottomPen/@lineColor',
                                                   BOX_LEFT_COLOR   VARCHAR2(20)     PATH './box/leftPen/@lineColor',
                                                   BOX_RIGHT_COLOR  VARCHAR2(20)     PATH './box/rightPen/@lineColor',
                                                   BOX_LINEWIDTH    VARCHAR2(10)     PATH './box/pen/@lineWidth',
                                                   BOX_COLOR        VARCHAR2(20)     PATH './box/pen/@lineColor',
                                                   FONT             VARCHAR2(255)    PATH './textElement/font/@fontName',
                                                   FONT_SIZE        VARCHAR2(255)    PATH './textElement/font/@size',
                                                   FONT_BOLD        VARCHAR2(5)      PATH './textElement/font/@isBold',
                                                   FONT_ITALIC      VARCHAR2(5)      PATH './textElement/font/@isItalic',
                                                   ALIGNMENT        VARCHAR2(255)    PATH './textElement/@textAlignment',
                                                   VERTICAL_ALIGN   VARCHAR2(255)    PATH './textElement/@verticalAlignment',
                                                   MARKUP           VARCHAR2(20)     PATH './textElement/@markup',
                                                   ROTATION         VARCHAR2(20)     PATH './textElement/@rotation',
                                                   LINE_SPACE       VARCHAR2(20)     PATH './textElement/paragraph/@lineSpacing',
                                                   LINE_SPACE_SIZE  VARCHAR2(20)     PATH './textElement/paragraph/@lineSpacingSize',
                                                   EXPRESSION       VARCHAR2(4000)   PATH './textFieldExpression',
                                                   PATTERN_EXPRESSION VARCHAR2(4000) PATH './patternExpression',
                                                   PATTERN          VARCHAR2(2000)   PATH './@pattern',
                                                   STRETCH_TYPE     VARCHAR2(255)    PATH './reportElement/@stretchType',
                                                   STRETCH_OVERFLOW VARCHAR2(5)      PATH './@isStretchWithOverflow',
                                                   EVALUATION_TIME  VARCHAR2(80)     PATH './@evaluationTime',
                                                   EVALUATION_GROUP VARCHAR2(80)     PATH './@evaluationGroup',
                                                   REPEATED_VALUES  VARCHAR2(5)      PATH './reportElement/@isPrintRepeatedValues'
                      );

    CURSOR crSubReports IS
     SELECT X,
            Y,
            WIDTH,
            HEIGHT,
            REPORT_NAME,
            DATA
        FROM XMLTABLE('/element/subreport'  PASSING i_oBand
                                            COLUMNS X           NUMBER         PATH './reportElement/@x',
                                                    Y           NUMBER         PATH './reportElement/@y',
                                                    WIDTH       NUMBER         PATH './reportElement/@width',
                                                    HEIGHT      NUMBER         PATH './reportElement/@height',
                                                    REPORT_NAME VARCHAR2(255)  PATH './subreportExpression',
                                                    DATA        XMLTYPE        PATH '.'
                     );

    CURSOR crBreaks IS
     SELECT X,
            Y,
            WIDTH,
            HEIGHT,
            TYPE,
            WHEN_EXPRESSION
        FROM XMLTABLE('/element/break'   PASSING i_oBand
                                         COLUMNS X                NUMBER         PATH './reportElement/@x',
                                                 Y                NUMBER         PATH './reportElement/@y',
                                                 WIDTH            NUMBER         PATH './reportElement/@width',
                                                 HEIGHT           NUMBER         PATH './reportElement/@height',
                                                 WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                 TYPE             VARCHAR2(255)  PATH './@type'
                     );

    CURSOR crTables IS
     SELECT X,
            Y,
            WIDTH,
            HEIGHT,
            WHEN_EXPRESSION,
            STYLE,
            TABLE_ELEMENT,
            DATA,
            SUBDATASET
        FROM XMLTABLE('/element/componentElement'   PASSING i_oBand
                                                    COLUMNS X                NUMBER         PATH './reportElement/@x',
                                                            Y                NUMBER         PATH './reportElement/@y',
                                                            WIDTH            NUMBER         PATH './reportElement/@width',
                                                            HEIGHT           NUMBER         PATH './reportElement/@height',
                                                            WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                            STYLE            VARCHAR2(255)  PATH './reportElement/@style',
                                                            TABLE_ELEMENT    XMLTYPE        PATH './table',
                                                            DATA             XMLTYPE        PATH './table/datasetRun',
                                                            SUBDATASET       VARCHAR2(255)  PATH './table/datasetRun/@subDataset'
                     );

    CURSOR crCrosstabs IS
     SELECT X,
            Y,
            WIDTH,
            HEIGHT,
            WHEN_EXPRESSION,
            CASE WHEN REPEAT_ROWHDR='false' THEN
              'N'
            ELSE
              'Y'
            END REPEAT_ROWHDR,
            CASE WHEN REPEAT_COLHDR='false' THEN
              'N'
            ELSE
              'Y'
            END REPEAT_COLHDR,
            NVL(COLBREAKOFFSET, '10') COLBREAKOFFSET,
            STYLE,
            CROSSTAB_ELEMENT,
            DATA,
            SUBDATASET
        FROM XMLTABLE('/element/crosstab'   PASSING i_oBand
                                                    COLUMNS X                NUMBER         PATH './reportElement/@x',
                                                            Y                NUMBER         PATH './reportElement/@y',
                                                            WIDTH            NUMBER         PATH './reportElement/@width',
                                                            HEIGHT           NUMBER         PATH './reportElement/@height',
                                                            WHEN_EXPRESSION  VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                            STYLE            VARCHAR2(255)  PATH './reportElement/@style',
                                                            REPEAT_ROWHDR    VARCHAR2(20)   PATH './@isRepeatRowHeaders',
                                                            REPEAT_COLHDR    VARCHAR2(20)   PATH './@isRepeatColumnHeaders',
                                                            COLBREAKOFFSET   VARCHAR2(20)   PATH './@columnBreakOffset',
                                                            CROSSTAB_ELEMENT XMLTYPE        PATH '.',
                                                            DATA             XMLTYPE        PATH './crosstabDataset/dataset/datasetRun',
                                                            SUBDATASET       VARCHAR2(255)  PATH './crosstabDataset/dataset/datasetRun/@subDataset'
                     );

    CURSOR crMaps IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             LATITUDE_EXPRESSION,
             LONGITUDE_EXPRESSION,
             ZOOM_EXPRESSION,
             MAP_ELEMENT
        FROM XMLTABLE('/element/componentElement'  PASSING i_oBand
                                                   COLUMNS X                    NUMBER         PATH './reportElement/@x',
                                                           Y                    NUMBER         PATH './reportElement/@y',
                                                           WIDTH                NUMBER         PATH './reportElement/@width',
                                                           HEIGHT               NUMBER         PATH './reportElement/@height',
                                                           BGCOLOR              VARCHAR2(20)   PATH './reportElement/@backcolor',
                                                           FGCOLOR              VARCHAR2(20)   PATH './reportElement/@forecolor',
                                                           STYLE                VARCHAR2(255)  PATH './reportElement/@style',
                                                           WHEN_EXPRESSION      VARCHAR2(4000) PATH './reportElement/printWhenExpression',
                                                           POSITION_TYPE        VARCHAR2(20)   PATH './reportElement/@positionType',
                                                           BOX_TOP              VARCHAR2(20)   PATH './box/topPen/@lineWidth',
                                                           BOX_BOTTOM           VARCHAR2(20)   PATH './box/bottomPen/@lineWidth',
                                                           BOX_LEFT             VARCHAR2(20)   PATH './box/leftPen/@lineWidth',
                                                           BOX_RIGHT            VARCHAR2(20)   PATH './box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR        VARCHAR2(20)   PATH './box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR     VARCHAR2(20)   PATH './box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR       VARCHAR2(20)   PATH './box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR      VARCHAR2(20)   PATH './box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH        VARCHAR2(10)   PATH './box/pen/@lineWidth',
                                                           BOX_COLOR            VARCHAR2(20)   PATH './box/pen/@lineColor',
                                                           MAP_ELEMENT          XMLTYPE        PATH './map',
                                                           LATITUDE_EXPRESSION  VARCHAR2(2000) PATH './map/latitudeExpression',
                                                           LONGITUDE_EXPRESSION VARCHAR2(2000) PATH './map/longitudeExpression',
                                                           ZOOM_EXPRESSION      VARCHAR2(2000) PATH './map/zoomExpression'
                     );

    CURSOR crBarCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP,    BOX_LINEWIDTH) BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT,   BOX_LINEWIDTH) BOX_LEFT,
             NVL(BOX_RIGHT,  BOX_LINEWIDTH) BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/barChart'          PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './barPlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './barPlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './barPlot/@isShowTickMarks',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './barPlot/plot/@labelRotation',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './barPlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './barPlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './barPlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './barPlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './barPlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './barPlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './barPlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/rangeAxisMaxValueExpression',
                                                           DATA                        XMLTYPE        PATH './categoryDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './categoryDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './categoryDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './barPlot/plot'
                     );

    CURSOR crBar3DCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/bar3DChart'        PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './bar3DPlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './bar3DPlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './bar3DPlot/@isShowTickMarks',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './bar3DPlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './bar3DPlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './bar3DPlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './bar3DPlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './bar3DPlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './bar3DPlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './bar3DPlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/rangeAxisMaxValueExpression',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './bar3DPlot/plot/@labelRotation',
                                                           DATA                        XMLTYPE        PATH './categoryDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './categoryDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './categoryDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './bar3DPlot/plot'
                     );

    CURSOR crStackedBarCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP,    BOX_LINEWIDTH) BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT,   BOX_LINEWIDTH) BOX_LEFT,
             NVL(BOX_RIGHT,  BOX_LINEWIDTH) BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/stackedBarChart'   PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './barPlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './barPlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './barPlot/@isShowTickMarks',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './barPlot/plot/@labelRotation',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './barPlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './barPlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './barPlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './barPlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './barPlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './barPlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './barPlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './barPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './barPlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './barPlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './barPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './barPlot/rangeAxisMaxValueExpression',
                                                           DATA                        XMLTYPE        PATH './categoryDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './categoryDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './categoryDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './barPlot/plot'
                     );

    CURSOR crStackedBar3DCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/stackedBar3DChart' PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './bar3DPlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './bar3DPlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './bar3DPlot/@isShowTickMarks',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './bar3DPlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './bar3DPlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './bar3DPlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './bar3DPlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './bar3DPlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './bar3DPlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './bar3DPlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './bar3DPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './bar3DPlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './bar3DPlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './bar3DPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './bar3DPlot/rangeAxisMaxValueExpression',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './bar3DPlot/plot/@labelRotation',
                                                           DATA                        XMLTYPE        PATH './categoryDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './categoryDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './categoryDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './bar3DPlot/plot'
                     );

    CURSOR crBarchartSeries(i_oXml IN XMLTYPE) IS
      SELECT SERIES_EXPRESSION,
             VALUE_EXPRESSION,
             CATEGORY_EXPRESSION
        FROM XMLTABLE('/categoryDataset/categorySeries' PASSING i_oXml
                                                        COLUMNS SERIES_EXPRESSION   VARCHAR2(4000) PATH './seriesExpression',
                                                                VALUE_EXPRESSION    VARCHAR2(4000) PATH './valueExpression',
                                                                CATEGORY_EXPRESSION VARCHAR2(4000) PATH './categoryExpression'
                     );

    CURSOR crChartSeriesColors(i_oXml IN XMLTYPE) IS
      SELECT SERIES_ORDER,
             COLOR
        FROM XMLTABLE('/plot/seriesColor' PASSING i_oXml
                                          COLUMNS SERIES_ORDER   VARCHAR2(20) PATH './@seriesOrder',
                                                  COLOR          VARCHAR2(20) PATH './@color'
                     );

    CURSOR crPieCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             KEY_EXPRESSION,
             VALUE_EXPRESSION,
             LABEL_FORMAT,
             LEGEND_LABEL_FORMAT,
             CUSTOMIZER_CLASS
        FROM XMLTABLE('/element/pieChart'          PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './piePlot/@isShowLabels',
                                                           LABEL_FORMAT                VARCHAR2(2000) PATH './piePlot/@labelFormat',
                                                           LEGEND_LABEL_FORMAT         VARCHAR2(2000) PATH './piePlot/@legendLabelFormat',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './piePlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './piePlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './piePlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './piePlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './piePlot/itemLabel/font/@isItalic',
                                                           DATA                        XMLTYPE        PATH './pieDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './pieDataset/dataset/datasetRun/@subDataset',
                                                           KEY_EXPRESSION              VARCHAR2(2000) PATH './pieDataset/keyExpression',
                                                           VALUE_EXPRESSION            VARCHAR2(2000) PATH './pieDataset/valueExpression',
                                                           SERIESCOLORS                XMLTYPE        PATH './piePlot/plot'
                     );

    CURSOR crCategoryLineCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP,    BOX_LINEWIDTH) BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT,   BOX_LINEWIDTH) BOX_LEFT,
             NVL(BOX_RIGHT,  BOX_LINEWIDTH) BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_LINES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LINES,
             CASE WHEN SHOW_SHAPES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_SHAPES,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/lineChart'         PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LINES                  VARCHAR2(5)    PATH './linePlot/@isShowLines',
                                                           SHOW_SHAPES                 VARCHAR2(5)    PATH './linePlot/@isShowShapes',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './linePlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './linePlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './linePlot/@isShowTickMarks',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './linePlot/plot/@labelRotation',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './linePlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './linePlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './linePlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './linePlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './linePlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './linePlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './linePlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/rangeAxisMaxValueExpression',
                                                           DATA                        XMLTYPE        PATH './categoryDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './categoryDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './categoryDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './linePlot/plot'
                     );

    CURSOR crXYLineCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP,    BOX_LINEWIDTH) BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT,   BOX_LINEWIDTH) BOX_LEFT,
             NVL(BOX_RIGHT,  BOX_LINEWIDTH) BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_LINES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LINES,
             CASE WHEN SHOW_SHAPES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_SHAPES,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/xyLineChart'       PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LINES                  VARCHAR2(5)    PATH './linePlot/@isShowLines',
                                                           SHOW_SHAPES                 VARCHAR2(5)    PATH './linePlot/@isShowShapes',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './linePlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './linePlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './linePlot/@isShowTickMarks',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './linePlot/plot/@labelRotation',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './linePlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './linePlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './linePlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './linePlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './linePlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './linePlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './linePlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './linePlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './linePlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './linePlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './linePlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './linePlot/rangeAxisMaxValueExpression',
                                                           DATA                        XMLTYPE        PATH './xyDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './xyDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './xyDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './linePlot/plot'
                     );

    CURSOR crXYSeries(i_oXml IN XMLTYPE) IS
      SELECT SERIES_EXPRESSION,
             XVALUE_EXPRESSION,
             YVALUE_EXPRESSION
        FROM XMLTABLE('/xyDataset/xySeries' PASSING i_oXml
                                                        COLUMNS SERIES_EXPRESSION   VARCHAR2(4000) PATH './seriesExpression',
                                                                XVALUE_EXPRESSION   VARCHAR2(4000) PATH './xValueExpression',
                                                                YVALUE_EXPRESSION   VARCHAR2(4000) PATH './yValueExpression'
                     );

    CURSOR crTimeseriesLineCharts IS
      SELECT X,
             Y,
             WIDTH,
             HEIGHT,
             NVL(BOX_TOP,    BOX_LINEWIDTH) BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT,   BOX_LINEWIDTH) BOX_LEFT,
             NVL(BOX_RIGHT,  BOX_LINEWIDTH) BOX_RIGHT,
             BGCOLOR,
             FGCOLOR,
             COALESCE(BOX_TOP_COLOR, BOX_COLOR, FGCOLOR)    BOX_TOP_COLOR,
             COALESCE(BOX_BOTTOM_COLOR, BOX_COLOR, FGCOLOR) BOX_BOTTOM_COLOR,
             COALESCE(BOX_LEFT_COLOR, BOX_COLOR, FGCOLOR)   BOX_LEFT_COLOR,
             COALESCE(BOX_RIGHT_COLOR, BOX_COLOR, FGCOLOR)  BOX_RIGHT_COLOR,
             BOX_TOP_PADDING,
             BOX_BOTTOM_PADDING,
             BOX_LEFT_PADDING,
             BOX_RIGHT_PADDING,
             STYLE,
             WHEN_EXPRESSION,
             CASE WHEN POSITION_TYPE='Float' THEN
               'FixRelativeToTop'
             ELSE
               POSITION_TYPE
             END POSITION_TYPE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE,
             CASE WHEN SHOW_LEGEND='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LEGEND,
             TITLE_POSITION,
             TITLE_EXPRESSION,
             TITLE_FONT,
             TITLE_FONT_SIZE,
             CASE WHEN TITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_BOLD,
             CASE WHEN TITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END TITLE_FONT_ITALIC,
             NVL(TITLE_COLOR, FGCOLOR) TITLE_COLOR,
             SUBTITLE_EXPRESSION,
             SUBTITLE_FONT,
             SUBTITLE_FONT_SIZE,
             CASE WHEN SUBTITLE_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_BOLD,
             CASE WHEN SUBTITLE_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END SUBTITLE_FONT_ITALIC,
             NVL(SUBTITLE_COLOR, FGCOLOR) SUBTITLE_COLOR,
             NVL(LEGEND_FG_COLOR, FGCOLOR) LEGEND_FG_COLOR,
             CASE WHEN MODUS='Opaque' THEN
               NVL(LEGEND_BG_COLOR, BGCOLOR)
             END LEGEND_BG_COLOR,
             LEGEND_POSITION,
             LEGEND_FONT,
             LEGEND_FONT_SIZE,
             CASE WHEN LEGEND_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_BOLD,
             CASE WHEN LEGEND_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LEGEND_FONT_ITALIC,
             DATA,
             SERIES,
             SUBDATASET,
             SERIESCOLORS,
             CASE WHEN SHOW_LABELS='true' THEN
               'Y'
             ELSE
               'N'
             END SHOW_LABELS,
             CASE WHEN SHOW_LINES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_LINES,
             CASE WHEN SHOW_SHAPES='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_SHAPES,
             CASE WHEN SHOW_TICK_LABELS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_LABELS,
             CASE WHEN SHOW_TICK_MARKS='false' THEN
               'N'
             ELSE
               'Y'
             END SHOW_TICK_MARKS,
             CAT_AXIS_EXPRESSION,
             NVL(CAT_AXIS_LABEL_COLOR, FGCOLOR) CAT_AXIS_LABEL_COLOR,
             NVL(CAT_AXIS_TICKLABEL_COLOR, FGCOLOR) CAT_AXIS_TICKLABEL_COLOR,
             CAT_AXIS_TICKLABEL_PATTERN,
             CAT_AXIS_LINE_COLOR,
             CAT_AXIS_LABEL_FONT,
             CAT_AXIS_LABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_LABEL_FONTITAL,
             CAT_AXIS_TICKLABEL_FONT,
             CAT_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN CAT_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END CAT_AXIS_TICKLABEL_FONTITAL,
             VAL_AXIS_EXPRESSION,
             NVL(VAL_AXIS_LABEL_COLOR, FGCOLOR) VAL_AXIS_LABEL_COLOR,
             NVL(VAL_AXIS_TICKLABEL_COLOR, FGCOLOR) VAL_AXIS_TICKLABEL_COLOR,
             VAL_AXIS_TICKLABEL_PATTERN,
             VAL_AXIS_LINE_COLOR,
             VAL_AXIS_LABEL_FONT,
             VAL_AXIS_LABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_LABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_LABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_LABEL_FONTITAL,
             VAL_AXIS_TICKLABEL_FONT,
             VAL_AXIS_TICKLABEL_FONTSIZE,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTBOLD='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTBOLD,
             CASE WHEN VAL_AXIS_TICKLABEL_FONTITAL='true' THEN
               'Y'
             ELSE
               'N'
             END VAL_AXIS_TICKLABEL_FONTITAL,
             NVL(LABEL_COLOR, FGCOLOR) LABEL_COLOR,
             LABEL_FONT,
             LABEL_FONT_SIZE,
             CASE WHEN LABEL_FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_BOLD,
             CASE WHEN LABEL_FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END LABEL_FONT_ITALIC,
             CAT_AXIS_MIN_VAL_EXPRESSION,
             CAT_AXIS_MAX_VAL_EXPRESSION,
             VAL_AXIS_MIN_VAL_EXPRESSION,
             VAL_AXIS_MAX_VAL_EXPRESSION,
             CUSTOMIZER_CLASS,
             LABEL_ROTATION
        FROM XMLTABLE('/element/timeSeriesChart'   PASSING i_oBand
                                                   COLUMNS X                           NUMBER         PATH './chart/reportElement/@x',
                                                           Y                           NUMBER         PATH './chart/reportElement/@y',
                                                           WIDTH                       NUMBER         PATH './chart/reportElement/@width',
                                                           HEIGHT                      NUMBER         PATH './chart/reportElement/@height',
                                                           BGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@backcolor',
                                                           FGCOLOR                     VARCHAR2(20)   PATH './chart/reportElement/@forecolor',
                                                           STYLE                       VARCHAR2(255)  PATH './chart/reportElement/@style',
                                                           MODUS                       VARCHAR2(20)   PATH './chart/reportElement/@mode',
                                                           WHEN_EXPRESSION             VARCHAR2(4000) PATH './chart/reportElement/printWhenExpression',
                                                           POSITION_TYPE               VARCHAR2(20)   PATH './chart/reportElement/@positionType',
                                                           BOX_TOP                     VARCHAR2(20)   PATH './chart/box/topPen/@lineWidth',
                                                           BOX_BOTTOM                  VARCHAR2(20)   PATH './chart/box/bottomPen/@lineWidth',
                                                           BOX_LEFT                    VARCHAR2(20)   PATH './chart/box/leftPen/@lineWidth',
                                                           BOX_RIGHT                   VARCHAR2(20)   PATH './chart/box/rightPen/@lineWidth',
                                                           BOX_TOP_COLOR               VARCHAR2(20)   PATH './chart/box/topPen/@lineColor',
                                                           BOX_BOTTOM_COLOR            VARCHAR2(20)   PATH './chart/box/bottomPen/@lineColor',
                                                           BOX_LEFT_COLOR              VARCHAR2(20)   PATH './chart/box/leftPen/@lineColor',
                                                           BOX_RIGHT_COLOR             VARCHAR2(20)   PATH './chart/box/rightPen/@lineColor',
                                                           BOX_LINEWIDTH               VARCHAR2(10)   PATH './chart/box/pen/@lineWidth',
                                                           BOX_COLOR                   VARCHAR2(20)   PATH './chart/box/pen/@lineColor',
                                                           BOX_TOP_PADDING             VARCHAR2(20)   PATH './chart/box/@topPadding',
                                                           BOX_BOTTOM_PADDING          VARCHAR2(20)   PATH './chart/box/@bottomPadding',
                                                           BOX_LEFT_PADDING            VARCHAR2(20)   PATH './chart/box/@leftPadding',
                                                           BOX_RIGHT_PADDING           VARCHAR2(20)   PATH './chart/box/@rightPadding',
                                                           SHOW_LEGEND                 VARCHAR2(20)   PATH './chart/@isShowLegend',
                                                           CUSTOMIZER_CLASS            VARCHAR2(4000) PATH './chart/@customizerClass',
                                                           TITLE_POSITION              VARCHAR2(20)   PATH './chart/chartTitle/@position',
                                                           TITLE_EXPRESSION            VARCHAR2(4000) PATH './chart/chartTitle/titleExpression',
                                                           TITLE_FONT                  VARCHAR2(255)  PATH './chart/chartTitle/font/@fontName',
                                                           TITLE_FONT_SIZE             VARCHAR2(255)  PATH './chart/chartTitle/font/@size',
                                                           TITLE_FONT_BOLD             VARCHAR2(5)    PATH './chart/chartTitle/font/@isBold',
                                                           TITLE_FONT_ITALIC           VARCHAR2(5)    PATH './chart/chartTitle/font/@isItalic',
                                                           TITLE_COLOR                 VARCHAR2(20)   PATH './chart/chartTitle/@color',
                                                           SUBTITLE_EXPRESSION         VARCHAR2(4000) PATH './chart/chartSubtitle/subtitleExpression',
                                                           SUBTITLE_FONT               VARCHAR2(255)  PATH './chart/chartSubtitle/font/@fontName',
                                                           SUBTITLE_FONT_SIZE          VARCHAR2(255)  PATH './chart/chartSubtitle/font/@size',
                                                           SUBTITLE_FONT_BOLD          VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isBold',
                                                           SUBTITLE_FONT_ITALIC        VARCHAR2(5)    PATH './chart/chartSubtitle/font/@isItalic',
                                                           SUBTITLE_COLOR              VARCHAR2(20)   PATH './chart/chartSubtitle/@color',
                                                           LEGEND_FG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@textColor',
                                                           LEGEND_BG_COLOR             VARCHAR2(20)   PATH './chart/chartLegend/@backgroundColor',
                                                           LEGEND_POSITION             VARCHAR2(20)   PATH './chart/chartLegend/@position',
                                                           LEGEND_FONT                 VARCHAR2(255)  PATH './chart/chartLegend/font/@fontName',
                                                           LEGEND_FONT_SIZE            VARCHAR2(255)  PATH './chart/chartLegend/font/@size',
                                                           LEGEND_FONT_BOLD            VARCHAR2(5)    PATH './chart/chartLegend/font/@isBold',
                                                           LEGEND_FONT_ITALIC          VARCHAR2(5)    PATH './chart/chartLegend/font/@isItalic',
                                                           SHOW_LINES                  VARCHAR2(5)    PATH './timeSeriesPlot/@isShowLines',
                                                           SHOW_SHAPES                 VARCHAR2(5)    PATH './timeSeriesPlot/@isShowShapes',
                                                           SHOW_LABELS                 VARCHAR2(5)    PATH './timeSeriesPlot/@isShowLabels',
                                                           SHOW_TICK_LABELS            VARCHAR2(5)    PATH './timeSeriesPlot/@isShowTickLabels',
                                                           SHOW_TICK_MARKS             VARCHAR2(5)    PATH './timeSeriesPlot/@isShowTickMarks',
                                                           LABEL_ROTATION              VARCHAR2(20)   PATH './timeSeriesPlot/plot/@labelRotation',
                                                           LABEL_COLOR                 VARCHAR2(255)  PATH './timeSeriesPlot/itemLabel/@color',
                                                           LABEL_FONT                  VARCHAR2(255)  PATH './timeSeriesPlot/itemLabel/font/@fontName',
                                                           LABEL_FONT_SIZE             VARCHAR2(255)  PATH './timeSeriesPlot/itemLabel/font/@size',
                                                           LABEL_FONT_BOLD             VARCHAR2(5)    PATH './timeSeriesPlot/itemLabel/font/@isBold',
                                                           LABEL_FONT_ITALIC           VARCHAR2(5)    PATH './timeSeriesPlot/itemLabel/font/@isItalic',
                                                           CAT_AXIS_EXPRESSION         VARCHAR2(4000) PATH './timeSeriesPlot/categoryAxisLabelExpression',
                                                           CAT_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/@labelColor',
                                                           CAT_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/@tickLabelColor',
                                                           CAT_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/@tickLabelMask',
                                                           CAT_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/@axisLineColor',
                                                           CAT_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           CAT_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/labelFont/font/@size',
                                                           CAT_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           CAT_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           CAT_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           CAT_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           CAT_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           CAT_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './timeSeriesPlot/categoryAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           VAL_AXIS_EXPRESSION         VARCHAR2(4000) PATH './timeSeriesPlot/valueAxisLabelExpression',
                                                           VAL_AXIS_LABEL_COLOR        VARCHAR2(20)   PATH './timeSeriesPlot/valueAxisFormat/axisFormat/@labelColor',
                                                           VAL_AXIS_TICKLABEL_COLOR    VARCHAR2(20)   PATH './timeSeriesPlot/valueAxisFormat/axisFormat/@tickLabelColor',
                                                           VAL_AXIS_TICKLABEL_PATTERN  VARCHAR2(20)   PATH './timeSeriesPlot/valueAxisFormat/axisFormat/@tickLabelMask',
                                                           VAL_AXIS_LINE_COLOR         VARCHAR2(20)   PATH './timeSeriesPlot/valueAxisFormat/axisFormat/@axisLineColor',
                                                           VAL_AXIS_LABEL_FONT         VARCHAR2(255)  PATH './timeSeriesPlot/valueAxisFormat/axisFormat/labelFont/font/@fontName',
                                                           VAL_AXIS_LABEL_FONTSIZE     VARCHAR2(255)  PATH './timeSeriesPlot/valueAxisFormat/axisFormat/labelFont/font/@size',
                                                           VAL_AXIS_LABEL_FONTBOLD     VARCHAR2(5)    PATH './timeSeriesPlot/valueAxisFormat/axisFormat/labelFont/font/@isBold',
                                                           VAL_AXIS_LABEL_FONTITAL     VARCHAR2(5)    PATH './timeSeriesPlot/valueAxisFormat/axisFormat/labelFont/font/@isItalic',
                                                           VAL_AXIS_TICKLABEL_FONT     VARCHAR2(255)  PATH './timeSeriesPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@fontName',
                                                           VAL_AXIS_TICKLABEL_FONTSIZE VARCHAR2(255)  PATH './timeSeriesPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@size',
                                                           VAL_AXIS_TICKLABEL_FONTBOLD VARCHAR2(5)    PATH './timeSeriesPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isBold',
                                                           VAL_AXIS_TICKLABEL_FONTITAL VARCHAR2(5)    PATH './timeSeriesPlot/valueAxisFormat/axisFormat/tickLabelFont/font/@isItalic',
                                                           CAT_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './timeSeriesPlot/domainAxisMinValueExpression',
                                                           CAT_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './timeSeriesPlot/domainAxisMaxValueExpression',
                                                           VAL_AXIS_MIN_VAL_EXPRESSION VARCHAR2(4000) PATH './timeSeriesPlot/rangeAxisMinValueExpression',
                                                           VAL_AXIS_MAX_VAL_EXPRESSION VARCHAR2(4000) PATH './timeSeriesPlot/rangeAxisMaxValueExpression',
                                                           DATA                        XMLTYPE        PATH './timeSeriesDataset/dataset/datasetRun',
                                                           SUBDATASET                  VARCHAR2(255)  PATH './timeSeriesDataset/dataset/datasetRun/@subDataset',
                                                           SERIES                      XMLTYPE        PATH './timeSeriesDataset',
                                                           SERIESCOLORS                XMLTYPE        PATH './timeSeriesPlot/plot'
                     );

    CURSOR crTimeSeries(i_oXml IN XMLTYPE) IS
      SELECT SERIES_EXPRESSION,
             TIME_EXPRESSION,
             VALUE_EXPRESSION
        FROM XMLTABLE('/timeSeriesDataset/timeSeries'   PASSING i_oXml
                                                        COLUMNS SERIES_EXPRESSION   VARCHAR2(4000) PATH './seriesExpression',
                                                                TIME_EXPRESSION     VARCHAR2(4000) PATH './timePeriodExpression',
                                                                VALUE_EXPRESSION    VARCHAR2(4000) PATH './valueExpression'
                     );

    rBand   PK_JRXML2PDF_TYPES.tBand;
    rObject PK_JRXML2PDF_TYPES.tObject;
    iPos    PLS_INTEGER;

    PROCEDURE PR_STORE_TOP_BOTTOM_BOUNDS(i_nY             IN NUMBER,
                                         i_nHeight        IN NUMBER,
                                         i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
    BEGIN
      IF NVL(i_vcPositionType,PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP)!=PK_JRXML2PDF_TYPES.RELATIVE_TO_BOTTOM THEN
        rBand.nMaxPosTop:=GREATEST(NVL(rBand.nMaxPosTop, -99999), i_nY+i_nHeight);
        rBand.bHasTopPos:=TRUE;
      ELSE
        rBand.nMinPosBottom:=LEAST(NVL(rBand.nMinPosBottom, 99999), i_nY);
        rBand.bHasBottomPos:=TRUE;
      END IF;
    END;

    PROCEDURE PR_LOAD_SUBFRAMES IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec  IN crSubFrames LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lBands.COUNT+1;
        -- Mark record as used because of recursion
        io_rReport.lBands(iPos).nX:=NULL;
        io_rReport.lBands(iPos):=FK_LOAD_BAND(io_rReport             =>io_rReport,
                                              i_oBand                =>rec.FRAME,
                                              i_vcTyp                =>'frame',
                                              i_nHeight              =>rec.HEIGHT,
                                              i_vcPrintWhenExpression=>rec.WHEN_EXPRESSION,
                                              i_vcSplitType          =>PK_JRXML2PDF_TYPES.PREVENT
                                             );
        io_rReport.lBands(iPos).nX               :=rec.X;
        io_rReport.lBands(iPos).nY               :=rec.Y;
        io_rReport.lBands(iPos).nWidth           :=rec.WIDTH;
        io_rReport.lBands(iPos).nHeight          :=rec.HEIGHT;
        io_rReport.lBands(iPos).vcBGColor        :=rec.BGCOLOR;
        io_rReport.lBands(iPos).vcFGColor        :=rec.FGCOLOR;
        io_rReport.lBands(iPos).nBoxTop          :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lBands(iPos).nBoxLeft         :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lBands(iPos).nBoxBottom       :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lBands(iPos).nBoxRight        :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lBands(iPos).vcBoxTopColor    :=rec.BOX_TOP_COLOR;
        io_rReport.lBands(iPos).vcBoxLeftColor   :=rec.BOX_LEFT_COLOR;
        io_rReport.lBands(iPos).vcBoxBottomColor :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lBands(iPos).vcBoxRightColor  :=rec.BOX_RIGHT_COLOR;
        io_rReport.lBands(iPos).vcStyle          :=rec.STYLE;
        io_rReport.lBands(iPos).vcPositionType   :=rec.POSITION_TYPE;
        io_rReport.lBands(iPos).vcOpaque         :=rec.OPAQUE;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lBands(iPos).nY,
                                   i_nHeight       =>io_rReport.lBands(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lBands(iPos).vcPositionType
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.SUBFRAME;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load subframes ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_LINES IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crLines LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lLines.COUNT+1;
        io_rReport.lLines(iPos).nX              :=rec.X;
        io_rReport.lLines(iPos).nY              :=rec.Y;
        io_rReport.lLines(iPos).nWidth          :=rec.WIDTH;
        io_rReport.lLines(iPos).nHeight         :=rec.HEIGHT;
        io_rReport.lLines(iPos).vcLineColor     :=rec.FGCOLOR;
        io_rReport.lLines(iPos).nLineWidth      :=FK_MAKE_NUM(rec.LINEWIDTH);
        io_rReport.lLines(iPos).vcStyle         :=rec.STYLE;
        io_rReport.lLines(iPos).vcStretch       :=rec.STRETCH;
        io_rReport.lLines(iPos).vcWhenExpression:=rec.WHEN_EXPRESSION;
        io_rReport.lLines(iPos).vcPositionType  :=rec.POSITION_TYPE;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lLines(iPos).nY,
                                   i_nHeight       =>io_rReport.lLines(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lLines(iPos).vcPositionType
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.LINE;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load lines ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_RECTANGLES IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crRectangles LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lRectangles.COUNT+1;
        io_rReport.lRectangles(iPos).nX              :=rec.X;
        io_rReport.lRectangles(iPos).nY              :=rec.Y;
        io_rReport.lRectangles(iPos).nWidth          :=rec.WIDTH;
        io_rReport.lRectangles(iPos).nHeight         :=rec.HEIGHT;
        io_rReport.lRectangles(iPos).vcLineColor     :=rec.FGCOLOR;
        io_rReport.lRectangles(iPos).vcFillColor     :=rec.BGCOLOR;
        io_rReport.lRectangles(iPos).nLineWidth      :=FK_MAKE_NUM(rec.LINEWIDTH);
        io_rReport.lRectangles(iPos).vcStyle         :=rec.STYLE;
        io_rReport.lRectangles(iPos).vcStretch       :=rec.STRETCH;
        io_rReport.lRectangles(iPos).vcWhenExpression:=rec.WHEN_EXPRESSION;
        io_rReport.lRectangles(iPos).vcOpaque        :=rec.OPAQUE;
        io_rReport.lRectangles(iPos).vcPositionType  :=rec.POSITION_TYPE;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lRectangles(iPos).nY,
                                   i_nHeight       =>io_rReport.lRectangles(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lRectangles(iPos).vcPositionType
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.RECTANGLE;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;

      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load rectangles ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_IMAGES IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crImages LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lImages.COUNT+1;
        io_rReport.lImages(iPos).vcImageName     :=rec.IMAGE;
        io_rReport.lImages(iPos).nX              :=rec.X;
        io_rReport.lImages(iPos).nY              :=rec.Y;
        io_rReport.lImages(iPos).nWidth          :=rec.WIDTH;
        io_rReport.lImages(iPos).nHeight         :=rec.HEIGHT;
        io_rReport.lImages(iPos).vcLineColor     :=rec.FGCOLOR;
        io_rReport.lImages(iPos).vcFillColor     :=rec.BGCOLOR;
        io_rReport.lImages(iPos).vcStyle         :=rec.STYLE;
        io_rReport.lImages(iPos).nBoxTop         :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lImages(iPos).nBoxLeft        :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lImages(iPos).nBoxBottom      :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lImages(iPos).nBoxRight       :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lImages(iPos).vcBoxTopColor   :=rec.BOX_TOP_COLOR;
        io_rReport.lImages(iPos).vcBoxLeftColor  :=rec.BOX_LEFT_COLOR;
        io_rReport.lImages(iPos).vcBoxBottomColor:=rec.BOX_BOTTOM_COLOR;
        io_rReport.lImages(iPos).vcBoxRightColor :=rec.BOX_RIGHT_COLOR;
        io_rReport.lImages(iPos).vcWhenExpression:=rec.WHEN_EXPRESSION;
        io_rReport.lImages(iPos).vcPositionType  :=rec.POSITION_TYPE;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lImages(iPos).nY,
                                   i_nHeight       =>io_rReport.lImages(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lImages(iPos).vcPositionType
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.IMAGE;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;

      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load images ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_STATIC_TEXT IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crStaticText LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lTexts.COUNT+1;
        io_rReport.lTexts(iPos).vcText           :=rec.TEXT;
        io_rReport.lTexts(iPos).nX               :=rec.X;
        io_rReport.lTexts(iPos).nY               :=rec.Y;
        io_rReport.lTexts(iPos).nWidth           :=rec.WIDTH;
        io_rReport.lTexts(iPos).nHeight          :=rec.HEIGHT;
        io_rReport.lTexts(iPos).vcBGColor        :=rec.BGCOLOR;
        io_rReport.lTexts(iPos).vcFGColor        :=rec.FGCOLOR;
        io_rReport.lTexts(iPos).vcFont           :=rec.FONT;
        io_rReport.lTexts(iPos).vcFontStyle      :=CASE WHEN     rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                             AND rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                     'BI'
                                                   WHEN rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                     'B'
                                                   WHEN rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                     'I'
                                                   WHEN rec.FONT_BOLD=PK_JRXML2PDF_TYPES.NO THEN
                                                     'N'
                                                   END;
        io_rReport.lTexts(iPos).vcAlignment      :=rec.ALIGNMENT;
        io_rReport.lTexts(iPos).vcVerticalAlign  :=rec.VERTICAL_ALIGN;
        io_rReport.lTexts(iPos).nFontSize        :=FK_MAKE_NUM(rec.FONT_SIZE);
        io_rReport.lTexts(iPos).nBoxTop          :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lTexts(iPos).nBoxLeft         :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lTexts(iPos).nBoxBottom       :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lTexts(iPos).nBoxRight        :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lTexts(iPos).nTopPadding      :=FK_MAKE_NUM(rec.TOP_PADDING);
        io_rReport.lTexts(iPos).nLeftPadding     :=FK_MAKE_NUM(rec.LEFT_PADDING);
        io_rReport.lTexts(iPos).nBottomPadding   :=FK_MAKE_NUM(rec.BOTTOM_PADDING);
        io_rReport.lTexts(iPos).nRightPadding    :=FK_MAKE_NUM(rec.RIGHT_PADDING);
        io_rReport.lTexts(iPos).vcBoxTopColor    :=rec.BOX_TOP_COLOR;
        io_rReport.lTexts(iPos).vcBoxLeftColor   :=rec.BOX_LEFT_COLOR;
        io_rReport.lTexts(iPos).vcBoxBottomColor :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lTexts(iPos).vcBoxRightColor  :=rec.BOX_RIGHT_COLOR;
        io_rReport.lTexts(iPos).vcStretch        :=rec.STRETCH;
        io_rReport.lTexts(iPos).vcStyle          :=rec.STYLE;
        io_rReport.lTexts(iPos).vcWhenExpression :=rec.WHEN_EXPRESSION;
        io_rReport.lTexts(iPos).vcOpaque         :=rec.OPAQUE;
        io_rReport.lTexts(iPos).vcPositionType   :=rec.POSITION_TYPE;
        io_rReport.lTexts(iPos).vcRotation       :=rec.ROTATION;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lTexts(iPos).nY,
                                   i_nHeight       =>io_rReport.lTexts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lTexts(iPos).vcPositionType
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.TEXT;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load static text ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_TEXTFIELDS IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crTextFields LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lFields.COUNT+1;

        io_rReport.lFields(iPos).vcExpression       :=rec.EXPRESSION;
        io_rReport.lFields(iPos).vcPattern          :=rec.PATTERN;
        io_rReport.lFields(iPos).vcPatternExpression:=rec.PATTERN_EXPRESSION;
        io_rReport.lFields(iPos).vcPrintRepeated    :=rec.REPEATED_VALUES;
        io_rReport.lFields(iPos).nX                 :=rec.X;
        io_rReport.lFields(iPos).nY                 :=rec.Y;
        io_rReport.lFields(iPos).nWidth             :=rec.WIDTH;
        io_rReport.lFields(iPos).nHeight            :=rec.HEIGHT;
        io_rReport.lFields(iPos).vcBGColor          :=rec.BGCOLOR;
        io_rReport.lFields(iPos).vcFGColor          :=rec.FGCOLOR;
        io_rReport.lFields(iPos).vcFont             :=rec.FONT;
        io_rReport.lFields(iPos).vcFontStyle        :=CASE WHEN    rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                               AND rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                       'BI'
                                                     WHEN rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                       'B'
                                                     WHEN rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                       'I'
                                                     WHEN rec.FONT_BOLD=PK_JRXML2PDF_TYPES.NO THEN
                                                       'N'
                                                     END;
        io_rReport.lFields(iPos).vcAlignment        :=rec.ALIGNMENT;
        io_rReport.lFields(iPos).vcVerticalAlign    :=rec.VERTICAL_ALIGN;
        io_rReport.lFields(iPos).nFontSize          :=FK_MAKE_NUM(rec.FONT_SIZE);
        io_rReport.lFields(iPos).nBoxTop            :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lFields(iPos).nBoxLeft           :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lFields(iPos).nBoxBottom         :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lFields(iPos).nBoxRight          :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lFields(iPos).nTopPadding        :=FK_MAKE_NUM(rec.TOP_PADDING);
        io_rReport.lFields(iPos).nLeftPadding       :=FK_MAKE_NUM(rec.LEFT_PADDING);
        io_rReport.lFields(iPos).nBottomPadding     :=FK_MAKE_NUM(rec.BOTTOM_PADDING);
        io_rReport.lFields(iPos).nRightPadding      :=FK_MAKE_NUM(rec.RIGHT_PADDING);
        io_rReport.lFields(iPos).vcBoxTopColor      :=rec.BOX_TOP_COLOR;
        io_rReport.lFields(iPos).vcBoxLeftColor     :=rec.BOX_LEFT_COLOR;
        io_rReport.lFields(iPos).vcBoxBottomColor   :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lFields(iPos).vcBoxRightColor    :=rec.BOX_RIGHT_COLOR;
        io_rReport.lFields(iPos).vcStretch          :=rec.STRETCH;
        io_rReport.lFields(iPos).vcStretchOverflow  :=rec.STRETCH_OVERFLOW;
        io_rReport.lFields(iPos).vcStyle            :=rec.STYLE;
        io_rReport.lFields(iPos).vcEvaluationTime   :=rec.EVALUATION_TIME;
        io_rReport.lFields(iPos).vcEvaluationGroup  :=rec.EVALUATION_GROUP;
        io_rReport.lFields(iPos).vcWhenExpression   :=rec.WHEN_EXPRESSION;
        io_rReport.lFields(iPos).nLineSpacing       :=GREATEST(FK_MAKE_NUM(rec.LINE_SPACING), PK_JRXML2PDF_TYPES.MIN_LINE_SPACING);
        io_rReport.lFields(iPos).vcPositionType     :=rec.POSITION_TYPE;
        io_rReport.lFields(iPos).vcMarkup           :=rec.MARKUP;
        io_rReport.lFields(iPos).vcKey              :=rec.KEY;
        io_rReport.lFields(iPos).vcRotation         :=rec.ROTATION;

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lFields(iPos).nY,
                                   i_nHeight       =>io_rReport.lFields(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lFields(iPos).vcPositionType
                                  );

        io_rReport.lFields(iPos).vcOpaque         :=rec.OPAQUE;
        rObject.nType:=PK_JRXML2PDF_TYPES.FIELD;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load textfields ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_SUBREPORTS IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crSubReports LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lSubreports.COUNT+1;
        io_rReport.lSubreports(iPos).vcReportname     :=rec.REPORT_NAME;
        io_rReport.lSubreports(iPos).nX               :=rec.X;
        io_rReport.lSubreports(iPos).nY               :=rec.Y;
        io_rReport.lSubreports(iPos).nWidth           :=rec.WIDTH;
        io_rReport.lSubreports(iPos).nHeight          :=rec.HEIGHT;
        io_rReport.lSubreports(iPos).lParamList       :=FK_CREATE_PARAMLIST(rec.DATA);

        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lSubreports(iPos).nY,
                                   i_nHeight       =>io_rReport.lSubreports(iPos).nHeight,
                                   i_vcPositionType=>PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP
                                  );

        rObject.nType:=PK_JRXML2PDF_TYPES.SUBREPORT;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load subreports ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_BREAKS IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crBreaks LOOP
        IF NVL(rec.TYPE, 'page')='page' THEN
          iCount:=iCount+1;
          iPos:=io_rReport.lBreaks.COUNT+1;
          io_rReport.lBreaks(iPos).vcType          :='page';
          io_rReport.lBreaks(iPos).nX              :=rec.X;
          io_rReport.lBreaks(iPos).nY              :=rec.Y;
          io_rReport.lBreaks(iPos).nWidth          :=rec.WIDTH;
          io_rReport.lBreaks(iPos).nHeight         :=rec.HEIGHT;
          io_rReport.lBreaks(iPos).vcWhenExpression:=rec.WHEN_EXPRESSION;

          rObject.nType:=PK_JRXML2PDF_TYPES.BREAK;
          rObject.nPosition:=iPos;
          rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
          rBand.bHasBreaks:=TRUE;
        END IF;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load breaks ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_TABLES IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crTables LOOP
        IF rec.TABLE_ELEMENT IS NOT NULL THEN
          iCount:=iCount+1;
          -- Create a new table, tables are processed the same way as subreports, so we "fake"
          -- a subreport for the table, and transform all table-elements to subreport-elementa
          iPos:=io_rReport.lSubreports.COUNT+1;
          -- Name must be unique, take SYSTIMESTAMP
          io_rReport.lSubreports(iPos).vcReportname     :=TO_CHAR(SYSTIMESTAMP);
          io_rReport.lSubreports(iPos).nX               :=rec.X;
          io_rReport.lSubreports(iPos).nY               :=rec.Y;
          io_rReport.lSubreports(iPos).nWidth           :=rec.WIDTH;
          io_rReport.lSubreports(iPos).nHeight          :=rec.HEIGHT;
          io_rReport.lSubreports(iPos).vcStyle          :=rec.STYLE;
          io_rReport.lSubreports(iPos).lParamList       :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);

          PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lSubreports(iPos).nY,
                                     i_nHeight       =>io_rReport.lSubreports(iPos).nHeight,
                                     i_vcPositionType=>PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP
                                    );

          rObject.nType:=PK_JRXML2PDF_TYPES.SUBREPORT;
          rObject.nPosition:=iPos;
          rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
          PR_LOAD_TABLE_AS_SUBREPORT(i_rReport      =>io_rReport,
                                     i_vcReportName =>io_rReport.lSubreports(iPos).vcReportname,
                                     i_oTable       =>rec.TABLE_ELEMENT,
                                     i_vcSubdataset =>rec.SUBDATASET
                                    );
          -- report-name is evaluated after loading has finished, so enclose it to be a string
          io_rReport.lSubreports(iPos).vcReportname     :='"' || io_rReport.lSubreports(iPos).vcReportname || '"';
        END IF;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load tables ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_CROSSTABS IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crCrosstabs LOOP
        IF rec.CROSSTAB_ELEMENT IS NOT NULL THEN
          iCount:=iCount+1;
          -- Create a new table, tables are processed the same way as subreports, so we "fake"
          -- a subreport for the table, and transform all table-elements to subreport-elementa
          iPos:=io_rReport.lCrosstabs.COUNT+1;
          rObject.nType:=PK_JRXML2PDF_TYPES.CROSSTAB;
          rObject.nPosition:=iPos;
          rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
          io_rReport.lCrosstabs(iPos):=FK_LOAD_CROSSTAB(io_rReport    =>io_rReport,
                                                        i_oCrosstab   =>rec.CROSSTAB_ELEMENT,
                                                        i_vcSubdataset=>rec.SUBDATASET,
                                                        i_vcRepeatRowHeaders=>rec.REPEAT_ROWHDR,
                                                        i_vcRepeatColHeaders=>rec.REPEAT_COLHDR,
                                                        i_nColBreakOffset=>FK_MAKE_NUM(rec.COLBREAKOFFSET)
                                                        );
          io_rReport.lCrosstabs(iPos).nX               :=rec.X;
          io_rReport.lCrosstabs(iPos).nY               :=rec.Y;
          io_rReport.lCrosstabs(iPos).nWidth           :=rec.WIDTH;
          io_rReport.lCrosstabs(iPos).nHeight          :=rec.HEIGHT;
          io_rReport.lCrosstabs(iPos).vcStyle          :=rec.STYLE;
          io_rReport.lCrosstabs(iPos).vcWhenExpression :=rec.WHEN_EXPRESSION;
          io_rReport.lCrosstabs(iPos).lParams          :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);

          PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lCrosstabs(iPos).nY,
                                     i_nHeight       =>io_rReport.lCrosstabs(iPos).nHeight,
                                     i_vcPositionType=>PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP
                                    );

        END IF;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load Crosstabs ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_MAPS IS
      iCount PLS_INTEGER:=0;
    BEGIN
      FOR rec IN crMaps LOOP
        IF rec.MAP_ELEMENT IS NOT NULL THEN
          iCount:=iCount+1;
          iPos:=io_rReport.lMaps.COUNT+1;
          io_rReport.lMaps(iPos).nX                    :=rec.X;
          io_rReport.lMaps(iPos).nY                    :=rec.Y;
          io_rReport.lMaps(iPos).nWidth                :=rec.WIDTH;
          io_rReport.lMaps(iPos).nHeight               :=rec.HEIGHT;
          io_rReport.lMaps(iPos).vcLineColor           :=rec.FGCOLOR;
          io_rReport.lMaps(iPos).vcLineColor           :=rec.BGCOLOR;
          io_rReport.lMaps(iPos).vcStyle               :=rec.STYLE;
          io_rReport.lMaps(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
          io_rReport.lMaps(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
          io_rReport.lMaps(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
          io_rReport.lMaps(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
          io_rReport.lMaps(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
          io_rReport.lMaps(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
          io_rReport.lMaps(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
          io_rReport.lMaps(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
          io_rReport.lMaps(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
          io_rReport.lMaps(iPos).vcPositionType        :=rec.POSITION_TYPE;
          io_rReport.lMaps(iPos).vcLongitudeExpression :=rec.LONGITUDE_EXPRESSION;
          io_rReport.lMaps(iPos).vcLatitudeExpression  :=rec.LATITUDE_EXPRESSION;
          io_rReport.lMaps(iPos).vcZoomExpression      :=rec.ZOOM_EXPRESSION;

          PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lMaps(iPos).nY,
                                     i_nHeight       =>io_rReport.lMaps(iPos).nHeight,
                                     i_vcPositionType=>io_rReport.lMaps(iPos).vcPositionType
                                    );

          rObject.nType:=PK_JRXML2PDF_TYPES.MAP;
          rObject.nPosition:=iPos;
          rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
        END IF;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load maps ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_BARCHARTS IS
      iCount  PLS_INTEGER:=0;
      iSeries PLS_INTEGER;
    BEGIN
      FOR rec IN crBarcharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lBarcharts.COUNT+1;
        io_rReport.lBarcharts(iPos).nX                    :=rec.X;
        io_rReport.lBarcharts(iPos).nY                    :=rec.Y;
        io_rReport.lBarcharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lBarcharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lBarcharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lBarcharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lBarcharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lBarcharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lBarcharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lBarcharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lBarcharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lBarcharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lBarcharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lBarcharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lBarcharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lBarcharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lBarcharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lBarcharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lBarcharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.NO;
        io_rReport.lBarcharts(iPos).vcIsStacked           :=PK_JRXML2PDF_TYPES.NO;
        io_rReport.lBarcharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lBarcharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lBarcharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lBarcharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lBarcharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lBarcharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lBarcharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lBarcharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lBarcharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lBarcharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lBarcharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lBarcharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lBarcharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crBarchartSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lBarcharts(iPos).lSeries.COUNT+1;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.CATEGORY_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lBarcharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lBarcharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lBarcharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lBarcharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.BARCHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;

      FOR rec IN crBar3Dcharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lBarcharts.COUNT+1;
        io_rReport.lBarcharts(iPos).nX                    :=rec.X;
        io_rReport.lBarcharts(iPos).nY                    :=rec.Y;
        io_rReport.lBarcharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lBarcharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lBarcharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lBarcharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lBarcharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lBarcharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lBarcharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lBarcharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lBarcharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lBarcharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lBarcharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lBarcharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lBarcharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lBarcharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lBarcharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lBarcharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lBarcharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.YES;
        io_rReport.lBarcharts(iPos).vcIsStacked           :=PK_JRXML2PDF_TYPES.NO;
        io_rReport.lBarcharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lBarcharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lBarcharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lBarcharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lBarcharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lBarcharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lBarcharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lBarcharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lBarcharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lBarcharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lBarcharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lBarcharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        io_rReport.lBarcharts(iPos).lParams               :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crBarchartSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lBarcharts(iPos).lSeries.COUNT+1;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.CATEGORY_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lBarcharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lBarcharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lBarcharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lBarcharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.BARCHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;

      FOR rec IN crStackedBarcharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lBarcharts.COUNT+1;
        io_rReport.lBarcharts(iPos).nX                    :=rec.X;
        io_rReport.lBarcharts(iPos).nY                    :=rec.Y;
        io_rReport.lBarcharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lBarcharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lBarcharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lBarcharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lBarcharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lBarcharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lBarcharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lBarcharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lBarcharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lBarcharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lBarcharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lBarcharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lBarcharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lBarcharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lBarcharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lBarcharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lBarcharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.NO;
        io_rReport.lBarcharts(iPos).vcIsStacked           :=PK_JRXML2PDF_TYPES.YES;
        io_rReport.lBarcharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lBarcharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lBarcharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lBarcharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lBarcharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lBarcharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lBarcharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lBarcharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lBarcharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lBarcharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lBarcharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lBarcharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lBarcharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crBarchartSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lBarcharts(iPos).lSeries.COUNT+1;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.CATEGORY_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lBarcharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lBarcharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lBarcharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lBarcharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.BARCHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;

      FOR rec IN crStackedBar3Dcharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lBarcharts.COUNT+1;
        io_rReport.lBarcharts(iPos).nX                    :=rec.X;
        io_rReport.lBarcharts(iPos).nY                    :=rec.Y;
        io_rReport.lBarcharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lBarcharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lBarcharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lBarcharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lBarcharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lBarcharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lBarcharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lBarcharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lBarcharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lBarcharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lBarcharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lBarcharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lBarcharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lBarcharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lBarcharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lBarcharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lBarcharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lBarcharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.YES;
        io_rReport.lBarcharts(iPos).vcIsStacked           :=PK_JRXML2PDF_TYPES.YES;
        io_rReport.lBarcharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lBarcharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lBarcharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lBarcharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lBarcharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lBarcharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lBarcharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lBarcharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lBarcharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lBarcharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lBarcharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lBarcharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lBarcharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lBarcharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lBarcharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lBarcharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lBarcharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lBarcharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lBarcharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lBarcharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lBarcharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        io_rReport.lBarcharts(iPos).lParams               :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lBarcharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crBarchartSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lBarcharts(iPos).lSeries.COUNT+1;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lBarcharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.CATEGORY_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lBarcharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lBarcharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lBarcharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lBarcharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.BARCHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;

      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load barcharts ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_PIECHARTS IS
      iCount  PLS_INTEGER:=0;
      iSeries PLS_INTEGER;
    BEGIN
      FOR rec IN crPiecharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lPieCharts.COUNT+1;
        io_rReport.lPieCharts(iPos).nX                    :=rec.X;
        io_rReport.lPieCharts(iPos).nY                    :=rec.Y;
        io_rReport.lPieCharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lPieCharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lPieCharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lPieCharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lPieCharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lPieCharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lPieCharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lPieCharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lPieCharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lPieCharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lPieCharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lPieCharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lPieCharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lPieCharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lPiecharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lPiecharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lPiecharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lPiecharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lPieCharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lPieCharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lPieCharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.BOOL_NO;
        io_rReport.lPieCharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lPieCharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lPieCharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lPieCharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lPieCharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lPieCharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lPieCharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lPieCharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lPieCharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lPieCharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lPieCharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lPieCharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lPieCharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lPieCharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lPieCharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lPieCharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                     AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'BI'
                                                            WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                              'B'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                              'I'
                                                            WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                              'N'
                                                            END;
        io_rReport.lPieCharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lPieCharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lPieCharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lPieCharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lPieCharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                            AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'BI'
                                                                   WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'B'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                     'I'
                                                                   WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                     'N'
                                                                   END;
        io_rReport.lPieCharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lPieCharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lPieCharts(iPos).vcLabelFormat                :=rec.LABEL_FORMAT;
        io_rReport.lPieCharts(iPos).vcLegendLabelFormat          :=rec.LEGEND_LABEL_FORMAT;
        io_rReport.lPieCharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lPieCharts(iPos).vcKeyExpression              :=rec.KEY_EXPRESSION;
        io_rReport.lPieCharts(iPos).vcValueExpression            :=rec.VALUE_EXPRESSION;
        io_rReport.lPieCharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lPieCharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lPieCharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lPieCharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lPieCharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lPieCharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lPieCharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.PIECHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;


      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load piecharts ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_CATEGORY_LINECHARTS IS
      iCount  PLS_INTEGER:=0;
      iSeries PLS_INTEGER;
    BEGIN
      FOR rec IN crCategoryLinecharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lCategoryLinecharts.COUNT+1;
        io_rReport.lCategoryLinecharts(iPos).nX                    :=rec.X;
        io_rReport.lCategoryLinecharts(iPos).nY                    :=rec.Y;
        io_rReport.lCategoryLinecharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lCategoryLinecharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lCategoryLinecharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lCategoryLinecharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lCategoryLinecharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lCategoryLinecharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lCategoryLinecharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lCategoryLinecharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lCategoryLinecharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lCategoryLinecharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lCategoryLinecharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lCategoryLinecharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lCategoryLinecharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lCategoryLinecharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lCategoryLinecharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lCategoryLinecharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lCategoryLinecharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.BOOL_NO;
        io_rReport.lCategoryLinecharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lCategoryLinecharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                              AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'BI'
                                                                     WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'B'
                                                                     WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'I'
                                                                     WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                       'N'
                                                                     END;
        io_rReport.lCategoryLinecharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lCategoryLinecharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                              AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'BI'
                                                                     WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'B'
                                                                     WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'I'
                                                                     WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                       'N'
                                                                     END;
        io_rReport.lCategoryLinecharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lCategoryLinecharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lCategoryLinecharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                              AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'BI'
                                                                     WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'B'
                                                                     WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                       'I'
                                                                     WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                       'N'
                                                                     END;
        io_rReport.lCategoryLinecharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lCategoryLinecharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lCategoryLinecharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lCategoryLinecharts(iPos).vcShowLines                  :=rec.SHOW_LINES;
        io_rReport.lCategoryLinecharts(iPos).vcShowShapes                 :=rec.SHOW_SHAPES;
        io_rReport.lCategoryLinecharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lCategoryLinecharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                                     AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'BI'
                                                                            WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'B'
                                                                            WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'I'
                                                                            WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                              'N'
                                                                            END;
        io_rReport.lCategoryLinecharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lCategoryLinecharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lCategoryLinecharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lCategoryLinecharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                                     AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'BI'
                                                                            WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'B'
                                                                            WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'I'
                                                                            WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                              'N'
                                                                            END;
        io_rReport.lCategoryLinecharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                                     AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'BI'
                                                                            WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'B'
                                                                            WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'I'
                                                                            WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                              'N'
                                                                            END;
        io_rReport.lCategoryLinecharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                                     AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'BI'
                                                                            WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'B'
                                                                            WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'I'
                                                                            WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                              'N'
                                                                            END;
        io_rReport.lCategoryLinecharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                                     AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'BI'
                                                                            WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'B'
                                                                            WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                              'I'
                                                                            WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                              'N'
                                                                            END;
        io_rReport.lCategoryLinecharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lCategoryLinecharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lCategoryLinecharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lCategoryLinecharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lCategoryLinecharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lCategoryLinecharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crBarchartSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lCategoryLinecharts(iPos).lSeries.COUNT+1;
          io_rReport.lCategoryLinecharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lCategoryLinecharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lCategoryLinecharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.CATEGORY_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lCategoryLinecharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lCategoryLinecharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lCategoryLinecharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lCategoryLinecharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.CATEGORYLINECHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load category linecharts ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_XY_LINECHARTS IS
      iCount  PLS_INTEGER:=0;
      iSeries PLS_INTEGER;
    BEGIN
      FOR rec IN crXYLinecharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lXYLineCharts.COUNT+1;
        io_rReport.lXYLineCharts(iPos).nX                    :=rec.X;
        io_rReport.lXYLineCharts(iPos).nY                    :=rec.Y;
        io_rReport.lXYLineCharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lXYLineCharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lXYLineCharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lXYLineCharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lXYLineCharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lXYLineCharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lXYLineCharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lXYLineCharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lXYLineCharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lXYLineCharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lXYLineCharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lXYLineCharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lXYLineCharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lXYLineCharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lXYLineCharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lXYLineCharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lXYLineCharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lXYLineCharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lXYLineCharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lXYLineCharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.BOOL_NO;
        io_rReport.lXYLineCharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lXYLineCharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lXYLineCharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lXYLineCharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lXYLineCharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lXYLineCharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lXYLineCharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lXYLineCharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lXYLineCharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lXYLineCharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lXYLineCharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lXYLineCharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lXYLineCharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lXYLineCharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lXYLineCharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lXYLineCharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lXYLineCharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lXYLineCharts(iPos).vcShowLines                  :=rec.SHOW_LINES;
        io_rReport.lXYLineCharts(iPos).vcShowShapes                 :=rec.SHOW_SHAPES;
        io_rReport.lXYLineCharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lXYLineCharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lXYLineCharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lXYLineCharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lXYLineCharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lXYLineCharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lXYLineCharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lXYLineCharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lXYLineCharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lXYLineCharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lXYLineCharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lXYLineCharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lXYLineCharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lXYLineCharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lXYLineCharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lXYLineCharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lXYLineCharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lXYLineCharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lXYLineCharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lXYLineCharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lXYLineCharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lXYLineCharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lXYLineCharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lXYLineCharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lXYLineCharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lXYLineCharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lXYLineCharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lXYLineCharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lXYLineCharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lXYLineCharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lXYLineCharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lXYLineCharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crXYSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lXYLineCharts(iPos).lSeries.COUNT+1;
          io_rReport.lXYLineCharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lXYLineCharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.YVALUE_EXPRESSION;
          io_rReport.lXYLineCharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.XVALUE_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lXYLineCharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lXYLineCharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lXYLineCharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lXYLineCharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.XYLINECHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load category linecharts ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

    PROCEDURE PR_LOAD_time_LINECHARTS IS
      iCount  PLS_INTEGER:=0;
      iSeries PLS_INTEGER;
    BEGIN
      FOR rec IN crTimeseriesLinecharts LOOP
        iCount:=iCount+1;
        iPos:=io_rReport.lTimeLineCharts.COUNT+1;
        io_rReport.lTimeLineCharts(iPos).nX                    :=rec.X;
        io_rReport.lTimeLineCharts(iPos).nY                    :=rec.Y;
        io_rReport.lTimeLineCharts(iPos).nWidth                :=rec.WIDTH;
        io_rReport.lTimeLineCharts(iPos).nHeight               :=rec.HEIGHT;
        io_rReport.lTimeLineCharts(iPos).vcFillColor           :=rec.BGCOLOR;
        io_rReport.lTimeLineCharts(iPos).vcLineColor           :=rec.FGCOLOR;
        io_rReport.lTimeLineCharts(iPos).vcStyle               :=rec.STYLE;
        io_rReport.lTimeLineCharts(iPos).vcOpaque              :=rec.OPAQUE;
        io_rReport.lTimeLineCharts(iPos).nBoxTop               :=FK_MAKE_NUM(rec.BOX_TOP);
        io_rReport.lTimeLineCharts(iPos).nBoxLeft              :=FK_MAKE_NUM(rec.BOX_LEFT);
        io_rReport.lTimeLineCharts(iPos).nBoxBottom            :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        io_rReport.lTimeLineCharts(iPos).nBoxRight             :=FK_MAKE_NUM(rec.BOX_RIGHT);
        io_rReport.lTimeLineCharts(iPos).vcBoxTopColor         :=rec.BOX_TOP_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcBoxLeftColor        :=rec.BOX_LEFT_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcBoxBottomColor      :=rec.BOX_BOTTOM_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcBoxRightColor       :=rec.BOX_RIGHT_COLOR;
        io_rReport.lTimeLineCharts(iPos).nTopPadding           :=FK_MAKE_NUM(rec.BOX_TOP_PADDING);
        io_rReport.lTimeLineCharts(iPos).nLeftPadding          :=FK_MAKE_NUM(rec.BOX_LEFT_PADDING);
        io_rReport.lTimeLineCharts(iPos).nBottomPadding        :=FK_MAKE_NUM(rec.BOX_BOTTOM_PADDING);
        io_rReport.lTimeLineCharts(iPos).nRightPadding         :=FK_MAKE_NUM(rec.BOX_RIGHT_PADDING);
        io_rReport.lTimeLineCharts(iPos).vcWhenExpression      :=rec.WHEN_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcPositionType        :=rec.POSITION_TYPE;
        io_rReport.lTimeLineCharts(iPos).vcIs3D                :=PK_JRXML2PDF_TYPES.BOOL_NO;
        io_rReport.lTimeLineCharts(iPos).vcTitleExpression     :=rec.TITLE_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcTitleColor          :=rec.TITLE_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcTitlePosition       :=NVL(rec.TITLE_POSITION, PK_JRXML2PDF_CHARTS.POSITION_TOP);
        io_rReport.lTimeLineCharts(iPos).vcTitleFont           :=rec.TITLE_FONT;
        io_rReport.lTimeLineCharts(iPos).vcTitleFontStyle      :=CASE WHEN    rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.TITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.TITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lTimeLineCharts(iPos).nTitleFontSize        :=FK_MAKE_NUM(rec.TITLE_FONT_SIZE);
        io_rReport.lTimeLineCharts(iPos).vcSubTitleExpression  :=rec.SUBTITLE_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcSubTitleColor       :=rec.SUBTITLE_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcSubTitleFont        :=rec.SUBTITLE_FONT;
        io_rReport.lTimeLineCharts(iPos).vcSubTitleFontStyle   :=CASE WHEN    rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.SUBTITLE_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.SUBTITLE_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lTimeLineCharts(iPos).nSubTitleFontSize     :=FK_MAKE_NUM(rec.SUBTITLE_FONT_SIZE);
        io_rReport.lTimeLineCharts(iPos).vcShowLegend          :=rec.SHOW_LEGEND;
        io_rReport.lTimeLineCharts(iPos).vcLegendTextColor     :=rec.LEGEND_FG_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcLegendBgColor       :=rec.LEGEND_BG_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcLegendFont          :=rec.LEGEND_FONT;
        io_rReport.lTimeLineCharts(iPos).vcLegendFontStyle     :=CASE WHEN    rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                        AND rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'BI'
                                                               WHEN rec.LEGEND_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'B'
                                                               WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                 'I'
                                                               WHEN rec.LEGEND_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                 'N'
                                                               END;
        io_rReport.lTimeLineCharts(iPos).nLegendFontSize              :=FK_MAKE_NUM(rec.LEGEND_FONT_SIZE);
        io_rReport.lTimeLineCharts(iPos).vcLegendPosition             :=NVL(rec.LEGEND_POSITION, PK_JRXML2PDF_CHARTS.POSITION_BOTTOM);
        io_rReport.lTimeLineCharts(iPos).vcShowLabels                 :=rec.SHOW_LABELS;
        io_rReport.lTimeLineCharts(iPos).vcShowLines                  :=rec.SHOW_LINES;
        io_rReport.lTimeLineCharts(iPos).vcShowShapes                 :=rec.SHOW_SHAPES;
        io_rReport.lTimeLineCharts(iPos).vcLabelFont                  :=rec.LABEL_FONT;
        io_rReport.lTimeLineCharts(iPos).vclabelFontStyle             :=CASE WHEN    rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.LABEL_FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.LABEL_FONT_ITALIC=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lTimeLineCharts(iPos).vcLabelColor                 :=rec.LABEL_COLOR;
        io_rReport.lTimeLineCharts(iPos).nLabelFontSize               :=FK_MAKE_NUM(rec.LABEL_FONT_SIZE);
        io_rReport.lTimeLineCharts(iPos).vcShowTickLabels             :=rec.SHOW_TICK_LABELS;
        io_rReport.lTimeLineCharts(iPos).vcShowTickMarks              :=rec.SHOW_TICK_MARKS;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisLabelExpression     :=rec.CAT_AXIS_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisLabelColor          :=rec.CAT_AXIS_LABEL_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisTickLabelColor      :=rec.CAT_AXIS_TICKLABEL_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisTickLabelFont       :=rec.CAT_AXIS_TICKLABEL_FONT;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisTickLabelFontStyle  :=CASE WHEN    rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.CAT_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lTimeLineCharts(iPos).nCatAxisTickLabelFontSize    :=FK_MAKE_NUM(rec.CAT_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lTimeLineCharts(iPos).vcCatAxisTickLabelPattern    :=rec.CAT_AXIS_TICKLABEL_PATTERN;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisLineColor           :=rec.CAT_AXIS_LINE_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisLabelFont           :=rec.CAT_AXIS_LABEL_FONT;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisLabelFontStyle      :=CASE WHEN    rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.CAT_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lTimeLineCharts(iPos).nCatAxisLabelFontSize        :=FK_MAKE_NUM(rec.CAT_AXIS_LABEL_FONTSIZE);
        io_rReport.lTimeLineCharts(iPos).vcValueAxisLabelExpression   :=rec.VAL_AXIS_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisLabelColor        :=rec.VAL_AXIS_LABEL_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisTickLabelColor    :=rec.VAL_AXIS_TICKLABEL_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisTickLabelFont     :=rec.VAL_AXIS_TICKLABEL_FONT;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisTickLabelFontStyle:=CASE WHEN    rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.VAL_AXIS_TICKLABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lTimeLineCharts(iPos).nAxisTickLabelFontSize       :=FK_MAKE_NUM(rec.VAL_AXIS_TICKLABEL_FONTSIZE);
        io_rReport.lTimeLineCharts(iPos).vcValueAxisTickLabelPattern  :=rec.VAL_AXIS_TICKLABEL_PATTERN;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisLineColor         :=rec.VAL_AXIS_LINE_COLOR;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisLabelFont         :=rec.VAL_AXIS_LABEL_FONT;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisLabelFontStyle    :=CASE WHEN    rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES
                                                                               AND rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'BI'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTBOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'B'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.YES THEN
                                                                        'I'
                                                                      WHEN rec.VAL_AXIS_LABEL_FONTITAL=PK_JRXML2PDF_TYPES.NO THEN
                                                                        'N'
                                                                      END;
        io_rReport.lTimeLineCharts(iPos).nValueAxisLabelFontSize      :=FK_MAKE_NUM(rec.VAL_AXIS_LABEL_FONTSIZE);
        io_rReport.lTimeLineCharts(iPos).vcValueAxisMinValExpression  :=rec.VAL_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcValueAxisMaxValExpression  :=rec.VAL_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisMinValExpression    :=rec.CAT_AXIS_MIN_VAL_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).vcCatAxisMaxValExpression    :=rec.CAT_AXIS_MAX_VAL_EXPRESSION;
        io_rReport.lTimeLineCharts(iPos).lParams                      :=FK_CREATE_TABLE_PARAMLIST(rec.DATA);
        io_rReport.lTimeLineCharts(iPos).vcCustomizerClass            :=rec.CUSTOMIZER_CLASS;
        io_rReport.lTimeLineCharts(iPos).nLabelRotation               :=FK_MAKE_NUM(rec.LABEL_ROTATION);

        -- find subdataset
        IF io_rReport.lDatasets.EXISTS(rec.SUBDATASET) THEN
          io_rReport.lTimeLineCharts(iPos).vcQuery:=io_rReport.lDatasets(rec.SUBDATASET).vcQuery;
        ELSE
          io_rReport.lTimeLineCharts(iPos).vcQuery:=io_rReport.vcQuery;
        END IF;
        FOR recSeries IN crTimeSeries(rec.SERIES) LOOP
          iSeries:=io_rReport.lTimeLineCharts(iPos).lSeries.COUNT+1;
          io_rReport.lTimeLineCharts(iPos).lSeries(iSeries).vcSeriesExpression  :=recSeries.SERIES_EXPRESSION;
          io_rReport.lTimeLineCharts(iPos).lSeries(iSeries).vcValueExpression   :=recSeries.VALUE_EXPRESSION;
          io_rReport.lTimeLineCharts(iPos).lSeries(iSeries).vcCategoryExpression:=recSeries.TIME_EXPRESSION;
        END LOOP;
        FOR recColor IN crChartSeriesColors(rec.SERIESCOLORS) LOOP
          io_rReport.lTimeLineCharts(iPos).lSeriesColors(FK_MAKE_NUM(recColor.SERIES_ORDER)+1):=recColor.COLOR;
        END LOOP;
        PR_STORE_TOP_BOTTOM_BOUNDS(i_nY            =>io_rReport.lTimeLineCharts(iPos).nY,
                                   i_nHeight       =>io_rReport.lTimeLineCharts(iPos).nHeight,
                                   i_vcPositionType=>io_rReport.lTimeLineCharts(iPos).vcPositionType
                                  );


        rObject.nType:=PK_JRXML2PDF_TYPES.TIMELINECHART;
        rObject.nPosition:=iPos;
        rBand.lObjects(rBand.lObjects.COUNT+1):=rObject;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        IF iCount>0 THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load timeseries linecharts ' || TO_CHAR(iCount));
        END IF;
      END IF;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Load band, type:' || i_vcTyp);
    END IF;
    rBand.vcTyp:=i_vcTyp;
    rBand.bHasBreaks:=FALSE;
    rBand.bHasTopPos:=FALSE;
    rBand.bHasBottomPos:=FALSE;

    rBand.nHeight:=i_nHeight;
    rBand.vcWhenExpression:=i_vcPrintWhenExpression;
    rBand.vcSplitType:=i_vcSplitType;

    PR_LOAD_SUBFRAMES;

    PR_LOAD_LINES;

    PR_LOAD_RECTANGLES;

    PR_LOAD_IMAGES;

    PR_LOAD_STATIC_TEXT;

    PR_LOAD_TEXTFIELDS;

    PR_LOAD_SUBREPORTS;

    PR_LOAD_BREAKS;

    PR_LOAD_TABLES;

    PR_LOAD_CROSSTABS;

    PR_LOAD_MAPS;

    PR_LOAD_BARCHARTS;

    PR_LOAD_PIECHARTS;

    PR_LOAD_CATEGORY_LINECHARTS;

    PR_LOAD_XY_LINECHARTS;

    PR_LOAD_TIME_LINECHARTS;

    RETURN rBand;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LOAD_REGION(io_rReport   IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport,
                          i_oRegion    IN      XMLTYPE)
  RETURN PK_JRXML2PDF_TYPES.tRegion IS
    CURSOR crBand IS
      SELECT XMLELEMENT("element", BAND) BAND,
             HEIGHT,
             WHEN_EXPRESSION,
             SPLIT_TYPE
        FROM XMLTABLE('/region/band' PASSING i_oRegion
                                     COLUMNS BAND            XMLTYPE        PATH './*',
                                             HEIGHT          NUMBER         PATH '@height',
                                             SPLIT_TYPE      VARCHAR2(80)   PATH '@splitType',
                                             WHEN_EXPRESSION VARCHAR2(4000) PATH 'printWhenExpression'
                     );
    rRegion PK_JRXML2PDF_TYPES.tRegion;
    rBand   PK_JRXML2PDF_TYPES.tBand;
    rObject PK_JRXML2PDF_TYPES.tObject;
    iPos    PLS_INTEGER;
  BEGIN
    IF i_oRegion IS NOT NULL THEN
      FOR rec IN crBand LOOP
        rBand:=FK_LOAD_BAND(io_rReport              =>io_rReport,
                            i_oBand                 =>rec.BAND,
                            i_vcTyp                 =>'band',
                            i_nHeight               =>rec.HEIGHT,
                            i_vcPrintWhenExpression =>rec.WHEN_EXPRESSION,
                            i_vcSplitType           =>NVL(rec.SPLIT_TYPE, PK_JRXML2PDF_TYPES.STRETCH)
                           );
        iPos:=io_rReport.lBands.COUNT+1;
        io_rReport.lBands(iPos):=rBand;
        rObject.nType:=PK_JRXML2PDF_TYPES.BAND;
        rObject.nPosition:=iPos;
        rRegion.lObjects(rRegion.lObjects.COUNT+1):=rObject;
      END LOOP;
    END IF;
    RETURN rRegion;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LOAD_REPORT (i_vcReportName IN VARCHAR2,
                           i_lParamList   IN PK_JRXML2PDF_TYPES.tParamList)
  RETURN PK_JRXML2PDF_TYPES.tReport IS
    CURSOR crReport IS
      SELECT JRD_ID,
             EXTRACTVALUE(XML, '/jasperReport/@pageWidth'                    )  PAGE_WIDTH,
             EXTRACTVALUE(XML, '/jasperReport/@pageHeight'                   )  PAGE_HEIGHT,
             EXTRACTVALUE(XML, '/jasperReport/@leftMargin'                   )  LEFT_MARGIN,
             EXTRACTVALUE(XML, '/jasperReport/@rightMargin'                  )  RIGHT_MARGIN,
             EXTRACTVALUE(XML, '/jasperReport/@topMargin'                    )  TOP_MARGIN,
             EXTRACTVALUE(XML, '/jasperReport/@bottomMargin'                 )  BOTTOM_MARGIN,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/title/*'       )) TITLE_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/pageHeader/*'  )) PAGEHEADER_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/columnHeader/*')) COLUMNHEADER_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/detail/*'      )) DETAIL_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/columnFooter/*')) COLUMNFOOTER_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/pageFooter/*'  )) PAGEFOOTER_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/summary/*'     )) SUMMARY_XML,
             XMLELEMENT("region", EXTRACT(XML, '/jasperReport/background/*'  )) BACKGROUND_XML,
             EXTRACTVALUE(XML, '/jasperReport/queryString'                   )  QUERY_STRING,
             CASE WHEN EXTRACTVALUE(XML, '/jasperReport/@isFloatColumnFooter')='true' THEN
               'Y'
             ELSE
               'N'
             END                                                                FLOAT_COLUMN_FOOTER,
             CASE WHEN EXTRACTVALUE(XML, '/jasperReport/@isTitleNewPage')='true' THEN
               'Y'
             ELSE
               'N'
             END                                                                TITLE_ON_NEW_PAGE,
             CASE WHEN EXTRACTVALUE(XML, '/jasperReport/@isSummaryNewPage')='true' THEN
               'Y'
             ELSE
               'N'
             END                                                                SUMMARY_ON_NEW_PAGE,
             CASE WHEN EXTRACTVALUE(XML, '/jasperReport/@isSummaryWithPageHeaderAndFooter')='true' THEN
               'Y'
             ELSE
               'N'
             END                                                                SUMMARY_WITH_HDR_FTR,
             EXTRACTVALUE(XML, '/jasperReport/@resourceBundle')                 RESOURCE_BUNDLE,
             XML
        FROM (SELECT JRD_ID,
                     XMLTYPE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(JRD_XML,
                                                                                             'xmlns="http://jasperreports.sourceforge.net/jasperreports"',
                                                                                             ''
                                                                                            ),
                                                                                     'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                                                                                     ''
                                                                                    ),
                                                                             'xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd"',
                                                                             ''
                                                                            ),
                                                                     'xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components"',
                                                                     ''
                                                                    ),
                                                             'xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd"',
                                                             ''
                                                            ),
                                                     '<jr:',
                                                     '<'
                                                    ),
                                             '</jr:',
                                             '</'
                                            ),
                                     '<mp:',
                                     '<'
                                    ),
                                 '</mp:',
                                 '</'
                                )
                            ) XML
                FROM JRXML_REPORT_DEFINITIONS
               WHERE JRD_NAME =i_vcReportName
             );

    CURSOR crStyles(i_oXml IN XMLTYPE) IS
      SELECT BGCOLOR,
             FGCOLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             TOP_PADDING,
             BOTTOM_PADDING,
             LEFT_PADDING,
             RIGHT_PADDING,
             NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
             NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
             NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
             NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
             FONT,
             FONT_SIZE,
             NVL(ALIGNMENT, H_ALIGN) ALIGNMENT,
             NVL(VERTICAL_ALIGN, V_ALIGN) VERTICAL_ALIGN,
             LINE_WIDTH,
             STYLE_NAME,
             CASE WHEN FONT_BOLD='true' THEN
               'Y'
             ELSE
               'N'
             END FONT_BOLD,
             CASE WHEN FONT_ITALIC='true' THEN
               'Y'
             ELSE
               'N'
             END FONT_ITALIC,
             CASE WHEN LINE_SPACE IS NULL THEN
               '1.17'
             WHEN LINE_SPACE='Single' THEN
               '1.17'
             WHEN LINE_SPACE='1_1_2' THEN
               '1.52'
             WHEN LINE_SPACE='Double' THEN
               '1.85'
             WHEN LINE_SPACE='Proportional' THEN
               NVL(LINE_SPACE_SIZE, '1.17')
             ELSE
               '1.17'
             END LINE_SPACING,
             STYLE,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             END OPAQUE
        FROM XMLTABLE('/jasperReport/style' PASSING i_oXml
                                            COLUMNS BGCOLOR          VARCHAR2(20)   PATH './@backcolor',
                                                    FGCOLOR          VARCHAR2(20)   PATH './@forecolor',
                                                    MODUS            VARCHAR2(20)   PATH './@mode',
                                                    BOX_TOP          VARCHAR2(20)   PATH './box/topPen/@lineWidth',
                                                    BOX_BOTTOM       VARCHAR2(20)   PATH './box/bottomPen/@lineWidth',
                                                    BOX_LEFT         VARCHAR2(20)   PATH './box/leftPen/@lineWidth',
                                                    BOX_RIGHT        VARCHAR2(20)   PATH './box/rightPen/@lineWidth',
                                                    BOX_TOP_COLOR    VARCHAR2(20)   PATH './box/topPen/@lineColor',
                                                    BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './box/bottomPen/@lineColor',
                                                    BOX_LEFT_COLOR   VARCHAR2(20)   PATH './box/leftPen/@lineColor',
                                                    BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './box/rightPen/@lineColor',
                                                    TOP_PADDING      VARCHAR2(20)   PATH './box/@topPadding',
                                                    BOTTOM_PADDING   VARCHAR2(20)   PATH './box/@bottomPadding',
                                                    LEFT_PADDING     VARCHAR2(20)   PATH './box/@leftPadding',
                                                    RIGHT_PADDING    VARCHAR2(20)   PATH './box/@rightPadding',
                                                    FONT             VARCHAR2(255)  PATH './@fontName',
                                                    FONT_SIZE        VARCHAR2(255)  PATH './@fontSize',
                                                    LINE_WIDTH       VARCHAR2(10)   PATH './pen/@lineWidth',
                                                    BOX_LINEWIDTH    VARCHAR2(10)   PATH './box/pen/@lineWidth',
                                                    BOX_COLOR        VARCHAR2(20)   PATH './box/pen/@lineColor',
                                                    ALIGNMENT        VARCHAR2(255)  PATH './@textAlignment',
                                                    VERTICAL_ALIGN   VARCHAR2(255)  PATH './@verticalAlignment',
                                                    H_ALIGN          VARCHAR2(255)  PATH './@hAlign',
                                                    V_ALIGN          VARCHAR2(255)  PATH './@vAlign',
                                                    STYLE_NAME       VARCHAR2(255)  PATH './@name',
                                                    FONT_BOLD        VARCHAR2(5)    PATH './@isBold',
                                                    FONT_ITALIC      VARCHAR2(5)    PATH './@isItalic',
                                                    LINE_SPACE       VARCHAR2(20)   PATH './paragraph/@lineSpacing',
                                                    LINE_SPACE_SIZE  VARCHAR2(20)   PATH './paragraph/@lineSpacingSize',
                                                    STYLE            XMLTYPE        PATH '.'
                      );

    CURSOR crConditionalStyle(i_xmlStyle IN XMLTYPE) IS
      SELECT WHEN_EXPRESSION,
             BGCOLOR,
             FGCOLOR,
             NVL(BOX_TOP, BOX_LINEWIDTH)    BOX_TOP,
             NVL(BOX_BOTTOM, BOX_LINEWIDTH) BOX_BOTTOM,
             NVL(BOX_LEFT, BOX_LINEWIDTH)   BOX_LEFT,
             NVL(BOX_RIGHT, BOX_LINEWIDTH)  BOX_RIGHT,
             TOP_PADDING,
             BOTTOM_PADDING,
             LEFT_PADDING,
             RIGHT_PADDING,
             NVL(BOX_TOP_COLOR, BOX_COLOR)    BOX_TOP_COLOR,
             NVL(BOX_BOTTOM_COLOR, BOX_COLOR) BOX_BOTTOM_COLOR,
             NVL(BOX_LEFT_COLOR, BOX_COLOR)   BOX_LEFT_COLOR,
             NVL(BOX_RIGHT_COLOR, BOX_COLOR)  BOX_RIGHT_COLOR,
             FONT,
             FONT_SIZE,
             NVL(ALIGNMENT, H_ALIGN) ALIGNMENT,
             NVL(VERTICAL_ALIGN, V_ALIGN) VERTICAL_ALIGN,
             LINE_WIDTH,
             STYLE_NAME,
             CASE WHEN FONT_BOLD='true' THEN
               'Y'
             WHEN FONT_BOLD='false' THEN
               'N'
             END FONT_BOLD,
             CASE WHEN FONT_ITALIC='true' THEN
               'Y'
             WHEN FONT_ITALIC='false' THEN
               'N'
             END FONT_ITALIC,
             CASE WHEN LINE_SPACE='Single' THEN
               '1.17'
             WHEN LINE_SPACE='1_1_2' THEN
               '1.52'
             WHEN LINE_SPACE='Double' THEN
               '1.85'
             WHEN LINE_SPACE='Proportional' THEN
               NVL(LINE_SPACE_SIZE, '1.17')
             END LINE_SPACING,
             CASE WHEN MODUS='Opaque' THEN
               'Y'
             WHEN MODUS='Transparent' THEN
               'N'
             END OPAQUE
        FROM XMLTABLE('/style/conditionalStyle' PASSING i_xmlStyle
                                            COLUMNS WHEN_EXPRESSION  VARCHAR2(4000) PATH './conditionExpression',
                                                    BGCOLOR          VARCHAR2(20)   PATH './style/@backcolor',
                                                    FGCOLOR          VARCHAR2(20)   PATH './style/@forecolor',
                                                    MODUS            VARCHAR2(20)   PATH './style/@mode',
                                                    BOX_TOP          VARCHAR2(20)   PATH './style/box/topPen/@lineWidth',
                                                    BOX_BOTTOM       VARCHAR2(20)   PATH './style/box/bottomPen/@lineWidth',
                                                    BOX_LEFT         VARCHAR2(20)   PATH './style/box/leftPen/@lineWidth',
                                                    BOX_RIGHT        VARCHAR2(20)   PATH './style/box/rightPen/@lineWidth',
                                                    BOX_TOP_COLOR    VARCHAR2(20)   PATH './style/box/topPen/@lineColor',
                                                    BOX_BOTTOM_COLOR VARCHAR2(20)   PATH './style/box/bottomPen/@lineColor',
                                                    BOX_LEFT_COLOR   VARCHAR2(20)   PATH './style/box/leftPen/@lineColor',
                                                    BOX_RIGHT_COLOR  VARCHAR2(20)   PATH './style/box/rightPen/@lineColor',
                                                    TOP_PADDING      VARCHAR2(20)   PATH './box/@topPadding',
                                                    BOTTOM_PADDING   VARCHAR2(20)   PATH './box/@bottomPadding',
                                                    LEFT_PADDING     VARCHAR2(20)   PATH './box/@leftPadding',
                                                    RIGHT_PADDING    VARCHAR2(20)   PATH './box/@rightPadding',
                                                    FONT             VARCHAR2(255)  PATH './style/@fontName',
                                                    FONT_SIZE        VARCHAR2(255)  PATH './style/@fontSize',
                                                    LINE_WIDTH       VARCHAR2(10)   PATH './style/pen/@lineWidth',
                                                    BOX_LINEWIDTH    VARCHAR2(10)   PATH './style/box/pen/@lineWidth',
                                                    BOX_COLOR        VARCHAR2(20)   PATH './style/box/pen/@lineColor',
                                                    ALIGNMENT        VARCHAR2(255)  PATH './style/@textAlignment',
                                                    VERTICAL_ALIGN   VARCHAR2(255)  PATH './style/@verticalAlignment',
                                                    H_ALIGN          VARCHAR2(255)  PATH './style/@hAlign',
                                                    V_ALIGN          VARCHAR2(255)  PATH './style/@vAlign',
                                                    STYLE_NAME       VARCHAR2(255)  PATH './style/@name',
                                                    FONT_BOLD        VARCHAR2(5)    PATH './style/@isBold',
                                                    FONT_ITALIC      VARCHAR2(5)    PATH './style/@isItalic',
                                                    LINE_SPACE       VARCHAR2(20)   PATH './style/paragraph/@lineSpacing',
                                                    LINE_SPACE_SIZE  VARCHAR2(20)   PATH './style/paragraph/@lineSpacingSize'
                      );

    CURSOR crGroups(i_oXml IN XMLTYPE) IS
      SELECT NAME,
             EXPRESSION,
             XMLELEMENT("region", GROUPHEADER) GROUPHEADER,
             XMLELEMENT("region", GROUPFOOTER) GROUPFOOTER,
             CASE WHEN START_ON_NEW_PAGE='true' THEN
               'Y'
             ELSE
               'N'
             END START_ON_NEW_PAGE,
             CASE WHEN RESET_PAGE_NUMBER='true' THEN
               'Y'
             ELSE
               'N'
             END RESET_PAGE_NUMBER,
             CASE WHEN REPRINT_HEADER='true' THEN
               'Y'
             ELSE
               'N'
             END REPRINT_HEADER,
             MIN_HEIGHT
        FROM XMLTABLE('/jasperReport/group' PASSING i_oXml
                                            COLUMNS NAME              VARCHAR2(255)  PATH './@name',
                                                    EXPRESSION        VARCHAR2(4000) PATH './groupExpression',
                                                    START_ON_NEW_PAGE VARCHAR2(20)   PATH './@isStartNewPage',
                                                    RESET_PAGE_NUMBER VARCHAR2(20)   PATH './@isResetPageNumber',
                                                    REPRINT_HEADER    VARCHAR2(20)   PATH './@isReprintHeaderOnEachPage',
                                                    MIN_HEIGHT        VARCHAR2(20)   PATH './@minHeightToStartNewPage',
                                                    GROUPHEADER       XMLTYPE        PATH './groupHeader/*',
                                                    GROUPFOOTER       XMLTYPE        PATH './groupFooter/*'
                      );

    CURSOR crDatasets(i_oXml IN XMLTYPE) IS
      SELECT NAME,
             QUERY,
             XMLELEMENT("element", GROUPS) GROUPS
        FROM XMLTABLE('/jasperReport/subDataset' PASSING i_oXml
                                                 COLUMNS NAME         VARCHAR2(255)  PATH './@name',
                                                         QUERY        VARCHAR2(4000) PATH './queryString',
                                                         GROUPS       XMLTYPE        PATH './group'
                      );

    CURSOR crDatasetGroups(i_oXml IN XMLTYPE) IS
      SELECT NAME,
             EXPRESSION
        FROM XMLTABLE('/element/group' PASSING i_oXml
                                       COLUMNS NAME        VARCHAR2(255)  PATH './@name',
                                               EXPRESSION  VARCHAR2(4000) PATH './groupExpression'
                      );

    recReport   crReport%ROWTYPE;
    rReport     PK_JRXML2PDF_TYPES.tReport;
    rStyle      PK_JRXML2PDF_TYPES.tStyle;
    rEmptyStyle PK_JRXML2PDF_TYPES.tStyle;
    rGroup      PK_JRXML2PDF_TYPES.tGroup;
    iPos        PLS_INTEGER;

    PROCEDURE PR_LOAD_STYLES(i_oXml IN XMLTYPE) IS
    BEGIN
      -- Read styles
      FOR rec IN crStyles(i_oXml) LOOP
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load style:' || rec.STYLE_NAME);
        END IF;
        rStyle:=rEmptyStyle;
        rStyle.rStyle.vcFGColor        :=rec.FGCOLOR;
        rStyle.rStyle.vcBGColor        :=rec.BGCOLOR;
        rStyle.rStyle.vcFont           :=rec.FONT;
        rStyle.rStyle.vcFontStyle      :=CASE WHEN     rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                   AND rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                            'BI'
                                          WHEN rec.FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                            'B'
                                          WHEN rec.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                            'I'
                                          ELSE
                                            'N'
                                          END;
        rStyle.rStyle.nFontSize        :=rec.FONT_SIZE;
        rStyle.rStyle.nBoxTop          :=FK_MAKE_NUM(rec.BOX_TOP);
        rStyle.rStyle.nBoxLeft         :=FK_MAKE_NUM(rec.BOX_LEFT);
        rStyle.rStyle.nBoxBottom       :=FK_MAKE_NUM(rec.BOX_BOTTOM);
        rStyle.rStyle.nBoxRight        :=FK_MAKE_NUM(rec.BOX_RIGHT);
        rStyle.rStyle.nTopPadding      :=FK_MAKE_NUM(rec.TOP_PADDING);
        rStyle.rStyle.nLeftPadding     :=FK_MAKE_NUM(rec.LEFT_PADDING);
        rStyle.rStyle.nBottomPadding   :=FK_MAKE_NUM(rec.BOTTOM_PADDING);
        rStyle.rStyle.nRightPadding    :=FK_MAKE_NUM(rec.RIGHT_PADDING);
        rStyle.rStyle.vcBoxTopColor    :=rec.BOX_TOP_COLOR;
        rStyle.rStyle.vcBoxLeftColor   :=rec.BOX_LEFT_COLOR;
        rStyle.rStyle.vcBoxBottomColor :=rec.BOX_BOTTOM_COLOR;
        rStyle.rStyle.vcBoxRightColor  :=rec.BOX_RIGHT_COLOR;
        rStyle.rStyle.nLineWidth       :=FK_MAKE_NUM(rec.LINE_WIDTH);
        rStyle.rStyle.vcAlignment      :=rec.ALIGNMENT;
        rStyle.rStyle.vcVerticalAlign  :=rec.VERTICAL_ALIGN;
        rStyle.rStyle.nLineSpacing     :=GREATEST(FK_MAKE_NUM(rec.LINE_SPACING), PK_JRXML2PDF_TYPES.MIN_LINE_SPACING);
        rStyle.rStyle.vcOpaque         :=rec.OPAQUE;
        FOR recCond IN crConditionalStyle(rec.STYLE) LOOP
          iPos:=rStyle.lConditionalStyles.COUNT+1;
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Load conditional-style:' || recCond.WHEN_EXPRESSION);
          END IF;
          rStyle.lConditionalStyles(iPos).vcWhenExpression :=recCond.WHEN_EXPRESSION;
          rStyle.lConditionalStyles(iPos).vcFGColor        :=recCond.FGCOLOR;
          rStyle.lConditionalStyles(iPos).vcBGColor        :=recCond.BGCOLOR;
          rStyle.lConditionalStyles(iPos).vcFont           :=recCond.FONT;
          rStyle.lConditionalStyles(iPos).vcFontStyle      :=CASE WHEN     recCond.FONT_BOLD=PK_JRXML2PDF_TYPES.YES
                                                                       AND recCond.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                'BI'
                                                              WHEN recCond.FONT_BOLD=PK_JRXML2PDF_TYPES.YES THEN
                                                                'B'
                                                              WHEN recCond.FONT_ITALIC=PK_JRXML2PDF_TYPES.YES THEN
                                                                'I'
                                                              WHEN recCond.FONT_BOLD=PK_JRXML2PDF_TYPES.NO THEN
                                                                'N'
                                                              END;
          rStyle.lConditionalStyles(iPos).nFontSize        :=recCond.FONT_SIZE;
          rStyle.lConditionalStyles(iPos).nBoxTop          :=FK_MAKE_NUM(recCond.BOX_TOP);
          rStyle.lConditionalStyles(iPos).nBoxLeft         :=FK_MAKE_NUM(recCond.BOX_LEFT);
          rStyle.lConditionalStyles(iPos).nBoxBottom       :=FK_MAKE_NUM(recCond.BOX_BOTTOM);
          rStyle.lConditionalStyles(iPos).nBoxRight        :=FK_MAKE_NUM(recCond.BOX_RIGHT);
          rStyle.lConditionalStyles(iPos).nTopPadding      :=FK_MAKE_NUM(rec.TOP_PADDING);
          rStyle.lConditionalStyles(iPos).nLeftPadding     :=FK_MAKE_NUM(rec.LEFT_PADDING);
          rStyle.lConditionalStyles(iPos).nBottomPadding   :=FK_MAKE_NUM(rec.BOTTOM_PADDING);
          rStyle.lConditionalStyles(iPos).nRightPadding    :=FK_MAKE_NUM(rec.RIGHT_PADDING);
          rStyle.lConditionalStyles(iPos).vcBoxTopColor    :=recCond.BOX_TOP_COLOR;
          rStyle.lConditionalStyles(iPos).vcBoxLeftColor   :=recCond.BOX_LEFT_COLOR;
          rStyle.lConditionalStyles(iPos).vcBoxBottomColor :=recCond.BOX_BOTTOM_COLOR;
          rStyle.lConditionalStyles(iPos).vcBoxRightColor  :=recCond.BOX_RIGHT_COLOR;
          rStyle.lConditionalStyles(iPos).nLineWidth       :=FK_MAKE_NUM(recCond.LINE_WIDTH);
          rStyle.lConditionalStyles(iPos).vcAlignment      :=recCond.ALIGNMENT;
          rStyle.lConditionalStyles(iPos).vcVerticalAlign  :=recCond.VERTICAL_ALIGN;
          rStyle.lConditionalStyles(iPos).nLineSpacing     :=GREATEST(FK_MAKE_NUM(recCond.LINE_SPACING), PK_JRXML2PDF_TYPES.MIN_LINE_SPACING);
          rStyle.lConditionalStyles(iPos).vcOpaque         :=recCond.OPAQUE;
        END LOOP;

        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Conditional style count=' || rStyle.lConditionalStyles.COUNT);
        END IF;

        rReport.lStyles(rec.STYLE_NAME):=rStyle;
      END LOOP;
    END;

    PROCEDURE PR_LOAD_GROUPS(i_oXml IN XMLTYPE) IS
    BEGIN
      -- load groups
      FOR rec IN crGroups(i_oXml) LOOP
        rGroup.vcName:=rec.NAME;
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load group:' || rGroup.vcName);
        END IF;

        rGroup.vcExpression:=rec.EXPRESSION;
        rGroup.vcStartOnNewPage:=rec.START_ON_NEW_PAGE;
        rGroup.vcResetPageNumber:=rec.RESET_PAGE_NUMBER;
        rGroup.vcReprintHeader:=rec.REPRINT_HEADER;
        rGroup.nMinHeightForPage:=FK_MAKE_NUM(rec.MIN_HEIGHT);
        rGroup.rGroupHeader:=FK_LOAD_REGION(rReport, rec.GROUPHEADER);
        rGroup.rGroupFooter:=FK_LOAD_REGION(rReport, rec.GROUPFOOTER);
        rReport.lGroups(rReport.lGroups.COUNT+1):=rGroup;
        rReport.lGroupExpressions(rReport.lGroupExpressions.COUNT+1):=NULL;
      END LOOP;
    END;

    PROCEDURE PR_LOAD_DATASETS(i_oXml IN XMLTYPE) IS
      rGroup PK_JRXML2PDF_TYPES.tGroup;
    BEGIN
      -- load groups
      FOR rec IN crDatasets(i_oXml) LOOP
        rReport.lDatasets(rec.NAME).vcQuery:=rec.QUERY;
        IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
          PK_JRXML2PDF_LOG.PR_LOG_FINE('Load dataset:' || rec.NAME);
        END IF;
        FOR recGroup IN crDatasetGroups(rec.GROUPS) LOOP
          IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
            PK_JRXML2PDF_LOG.PR_LOG_FINE('Add group to dataset:' || recGroup.NAME);
          END IF;
          rGroup.vcName:=recGroup.NAME;
          rGroup.vcExpression:=recGroup.EXPRESSION;
          rReport.lDatasets(rec.NAME).lGroups(rReport.lDatasets(rec.NAME).lGroups.COUNT+1):=rGroup;
        END LOOP;
      END LOOP;
    END;

    PROCEDURE PR_LOAD_RESOURCES IS
      vcLocale PK_JRXML2PDF_TYPES.tName;
    BEGIN
      -- clear resources
      rReport.lResources.DELETE;

      -- check for locale to use
      -- this is done always, because of format to be used when using the
      -- predefined patterns default, short, medium, long
      FOR i iN 1..i_lParamList.COUNT LOOP
        IF i_lParamList(i).vcName=PK_JRXML2PDF_TYPES.REPORT_LOCALE THEN
          vcLocale:=i_lParamList(i).vcValue;
          EXIT;
        END IF;
      END LOOP;
      rReport.rLocaleData:=PK_JRXML2PDF_UTIL.FK_GET_LOCALE_DATA(vcLocale);

      IF rReport.vcResourceBundle IS NOT NULL THEN
        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Loading Resources');
        END IF;
        -- initialize the resources used for the report
        rReport.lResources:=PK_JRXML2PDF_UTIL.FK_GET_RESOURCE(i_vcResourceName=>rReport.vcResourceBundle,
                                                              i_vcLocale      =>rReport.rLocaleData.vcLocale
                                                             );
      END IF;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Loading report ' || i_vcReportName);
    END IF;

    IF lReportCache.EXISTS(i_vcReportName) THEN

      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Taking report from cache');
      END IF;

      rReport:=lReportCache(i_vcReportName);
      rReport.lParams:=i_lParamList;
    ELSE
      rReport.lParams:=i_lParamList;

      OPEN crReport;
      FETCh crReport INTO recReport;
      IF crReport%FOUND THEN
        rReport.nId                 :=recReport.JRD_ID;
        rReport.nPageWidth          :=recReport.PAGE_WIDTH;
        rReport.nPageHeight         :=recReport.PAGE_HEIGHT;
        rReport.nLeftMargin         :=recReport.LEFT_MARGIN;
        rReport.nRightMargin        :=recReport.RIGHT_MARGIN;
        rReport.nTopMargin          :=recReport.TOP_MARGIN;
        rReport.nBottomMargin       :=recReport.BOTTOM_MARGIN;
        rReport.vcFloatColumnFooter :=recReport.FLOAT_COLUMN_FOOTER;
        rReport.vcTitleOnNewPage    :=recReport.TITLE_ON_NEW_PAGE;
        rReport.vcSummaryOnNewPage  :=recReport.SUMMARY_ON_NEW_PAGE;
        rReport.vcSummaryWithHdrFtr :=recReport.SUMMARY_WITH_HDR_FTR;
        rReport.vcResourceBundle    :=recReport.RESOURCE_BUNDLE;
        rReport.vcQuery             :=recReport.QUERY_STRING;
        rReport.nTyp                :=PK_JRXML2PDF_TYPES.REPORT_TYP_REPORT;
        -- Styles must be loadced before regions because of tables,
        -- which inherit styles
        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Loading styles');
        END IF;
        PR_LOAD_STYLES(recReport.XML);

        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Loading resources');
        END IF;
        PR_LOAD_RESOURCES;


        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Loading groups');
        END IF;
        PR_LOAD_GROUPS(recReport.XML);

        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Loading datasets');
        END IF;
        PR_LOAD_DATASETS(recReport.XML);

        rReport.rTitleRegion        :=FK_LOAD_REGION(rReport, recReport.TITLE_XML);
        rReport.rPageHeaderRegion   :=FK_LOAD_REGION(rReport, recReport.PAGEHEADER_XML);
        rReport.rColumnHeaderRegion :=FK_LOAD_REGION(rReport, recReport.COLUMNHEADER_XML);
        rReport.rDetailRegion       :=FK_LOAD_REGION(rReport, recReport.DETAIL_XML);
        rReport.rColumnFooterRegion :=FK_LOAD_REGION(rReport, recReport.COLUMNFOOTER_XML);
        rReport.rPageFooterRegion   :=FK_LOAD_REGION(rReport, recReport.PAGEFOOTER_XML);
        rReport.rSummaryRegion      :=FK_LOAD_REGION(rReport, recReport.SUMMARY_XML);
        rReport.rBackgroundRegion   :=FK_LOAD_REGION(rReport, recReport.BACKGROUND_XML);
        CLOSE crReport;

      ELSE
        CLOSE crReport;
        RAISE NO_DATA_FOUND;
      END IF;
      -- Cache
      lReportCache(i_vcReportName):=rReport;

    END IF;

    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Parse and execute query');
    END IF;

    PR_EXECUTE_QUERY(rReport, i_lParamList);
    -- empty variables
    rReport.lVariableCache.DELETE;
    rReport.lPageNumbers.DELETE;
    rReport.lLogicalPageNumbers.DELETE;
    rReport.lStartOfPageDone.DELETE;
    rReport.lEndOfPageDone.DELETE;
    rReport.lPageFrameMarkers.DELETE;
    rReport.lPageNumbers(1):=1;
    rReport.lLogicalPageNumbers(1):=1;
    rReport.nCurrentPagePointer:=1;
    RETURN rReport;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CLEAR_CACHE IS
  BEGIN
    lReportCache.DELETE;
  END;

END;
/
