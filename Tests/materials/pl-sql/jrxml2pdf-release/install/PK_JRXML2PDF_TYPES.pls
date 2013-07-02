create or replace
PACKAGE PK_JRXML2PDF_TYPES AUTHID CURRENT_USER IS
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

  $name    PK_JRXML2PDF_TYPES

  $created 16.10.2012

  $author  Andreas Weiden

  $desc    Package with types and constants used by PL-jrxml2pdf

  $version 1.0.0.0 16.10.2012 Weiden
           Out-sourced from PK_JRXML2PDF_REPGEN

  $version 1.1.0.0 31.10.2012 Weiden
           New types for barchart

  $version 1.1.0.1 10.12.2012 Weiden
           New types for category-linechart

  $version 1.1.1.1 23.12.2012 Weiden
           adjusted type for subdatasets

  $version 1.1.2.1 24.12.2012 Weiden
           New types for timeseries-linechart

  $version 1.1.1.0 04.01.2013 Weiden
           adjusted type for barchart

  $version 1.1.2.0 11.03.2013 Weiden
           Enhance: Add rHeaderCell to crosstab-type
*/

  SUBTYPE tColor             IS VARCHAR2(60);
  SUBTYPE tYesNo             IS VARCHAR2(1);
  SUBTYPE tFont              IS VARCHAR2(255);
  SUBTYPE tAlignment         IS VARCHAR2(20);
  SUBTYPE tAttribute         IS VARCHAR2(80);
  SUBTYPE tExpression        IS VARCHAR2(4000);
  SUBTYPE tGroupname         IS VARCHAR2(255);
  SUBTYPE tStyleName         IS VARCHAR2(255);
  SUBTYPE tPattern           IS VARCHAR2(4000);
  SUBTYPE tName              IS VARCHAR2(255);
  SUBTYPE tCalculation       IS VARCHAR2(255);
  SUBTYPE tPosition          IS VARCHAR2(20);
  SUBTYPE tHeaderPosition    IS VARCHAR2(20);
  SUBTYPE tCrosstabCellName  IS VARCHAR2(512);
  SUBTYPE tDatatype          IS VARCHAR2(1);
  SUBTYPE tPositionType      IS VARCHAR2(20);
  SUBTYPE tSplitType         IS VARCHAR2(20);
  SUBTYPE tFontStyle         IS VARCHAR2(2);
  SUBTYPE tMaxVarchar2       IS VARCHAR2(32767);
  SUBTYPE tIndex             IS VARCHAR2(255);
  SUBTYPE tDatePattern       IS VARCHAR2(255);
  SUBTYPE tChar              IS VARCHAR2(1);
  SUBTYPE tNumAsVarchar2     IS VARCHAR2(255);
  SUBTYPE tSmallVarchar2     IS VARCHAR2(10);

  $IF DBMS_DB_VERSION.VER_LE_10 $THEN
    -- DBMS-SQL-constants for 10G
    DBMS_SQL_VARCHAR2_TYPE         CONSTANT NUMBER := 1;
    DBMS_SQL_CHAR_TYPE             CONSTANT NUMBER := 96;
    DBMS_SQL_CLOB_TYPE             CONSTANT NUMBER := 112;
    DBMS_SQL_NUMBER_TYPE           CONSTANT NUMBER := 2;
    DBMS_SQL_BINARY_FLOAT_TYPE     CONSTANT NUMBER := 100;
    DBMS_SQL_BINARY_BOUBLE_TYPE    CONSTANT NUMBER := 101;
    DBMS_SQL_DATE_TYPE             CONSTANT NUMBER := 12;
    DBMS_SQL_TIMESTAMP_TYPE        CONSTANT NUMBER := 180;
    DBMS_SQL_TSTMP_WITH_TZ_TYPE    CONSTANT NUMBER := 181;
    DBMS_SQL_IV_YEAR_TO_MONTH_TYPE CONSTANT NUMBER := 182;
    DBMS_SQL_IV_DAY_TO_SECOND_TYPE CONSTANT NUMBER := 183;
    DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE  CONSTANT NUMBER := 231;
    DBMS_SQL_BLOB_TYPE             CONSTANT NUMBER := 113;
  $ELSE
    DBMS_SQL_VARCHAR2_TYPE         CONSTANT NUMBER :=DBMS_SQL.VARCHAR2_TYPE;
    DBMS_SQL_CHAR_TYPE             CONSTANT NUMBER :=DBMS_SQL.CHAR_TYPE;
    DBMS_SQL_CLOB_TYPE             CONSTANT NUMBER :=DBMS_SQL.CLOB_TYPE;
    DBMS_SQL_NUMBER_TYPE           CONSTANT NUMBER :=DBMS_SQL.NUMBER_TYPE;
    DBMS_SQL_BINARY_FLOAT_TYPE     CONSTANT NUMBER :=DBMS_SQL.BINARY_FLOAT_TYPE;
    DBMS_SQL_BINARY_BOUBLE_TYPE    CONSTANT NUMBER :=DBMS_SQL.BINARY_BOUBLE_TYPE;
    DBMS_SQL_DATE_TYPE             CONSTANT NUMBER :=DBMS_SQL.DATE_TYPE;
    DBMS_SQL_TIMESTAMP_TYPE        CONSTANT NUMBER :=DBMS_SQL.TIMESTAMP_TYPE;
    DBMS_SQL_TSTMP_WITH_TZ_TYPE    CONSTANT NUMBER :=DBMS_SQL.TIMESTAMP_WITH_TZ_TYPE;
    DBMS_SQL_IV_YEAR_TO_MONTH_TYPE CONSTANT NUMBER :=DBMS_SQL.INTERVAL_YEAR_TO_MONTH_TYPE;
    DBMS_SQL_IV_DAY_TO_SECOND_TYPE CONSTANT NUMBER :=DBMS_SQL.INTERVAL_DAY_TO_SECOND_TYPE;
    DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE  CONSTANT NUMBER :=DBMS_SQL.TIMESTAMP_WITH_LOCAL_TZ_TYPE;
    DBMS_SQL_BLOB_TYPE             CONSTANT NUMBER :=DBMS_SQL.BLOB_TYPE;
  $END

  MAP_URL               CONSTANT tMaxVarchar2:='http://maps.google.com/maps/api/staticmap?center=#LATITUDE#,#LONGITUDE#&zoom=#ZOOM#&size=#WIDTH#x#HEIGHT#&maptype=roadmap&sensor=false';
  NIL                   CONSTANT VARCHAR2(10):='$#§#$';
  DEFAULT_INT_FORMAT    CONSTANT tAttribute:='FM9999999999999999999999999999999999990';
  DEFAULT_FLOAT_FORMAT  CONSTANT tAttribute:='FM9999999999999999999999999999999999990D09999999999999999';

  REGION            CONSTANT NUMBER:=0;
  BAND              CONSTANT NUMBER:=1;
  SUBFRAME          CONSTANT NUMBER:=2;
  LINE              CONSTANT NUMBER:=3;
  RECTANGLE         CONSTANT NUMBER:=4;
  IMAGE             CONSTANT NUMBER:=5;
  TEXT              CONSTANT NUMBER:=6;
  FIELD             CONSTANT NUMBER:=7;
  SUBREPORT         CONSTANT NUMBER:=8;
  BREAK             CONSTANT NUMBER:=9;
  CROSSTAB          CONSTANT NUMBER:=10;
  MAP               CONSTANT NUMBER:=11;
  BARCHART          CONSTANT NUMBER:=12;
  PIECHART          CONSTANT NUMBER:=13;
  CATEGORYLINECHART CONSTANT NUMBER:=14;
  XYLINECHART       CONSTANT NUMBER:=15;
  TIMELINECHART     CONSTANT NUMBER:=16;

  NO_RECORD         CONSTANT NUMBER:=-1;
  BLACK             CONSTANT tColor:='000000';
  WHITE             CONSTANT tColor:='FFFFFF';
  THIS_IS_A_BLOB    CONSTANT VARCHAR2(10):='$$BLOB$$';
  THIN              CONSTANT NUMBER:=0.25;

  NONE_ALIGN      CONSTANT tAttribute:='none';
  LEFT_ALIGN      CONSTANT tAttribute:='left';
  CENTER_ALIGN    CONSTANT tAttribute:='center';
  RIGHT_ALIGN     CONSTANT tAttribute:='right';
  END_ALIGN       CONSTANT tAttribute:='end';
  TOP_ALIGN       CONSTANT tAttribute:='top';
  MIDDLE_ALIGN    CONSTANT tAttribute:='middle';
  BOTTOM_ALIGN    CONSTANT tAttribute:='bottom';
  STRETCH_ALIGN   CONSTANT tAttribute:='stretch';
  STRETCH_Y_ALIGN CONSTANT tAttribute:='stretchY';

  HTML_H1         CONSTANT tSmallVarchar2:='h1';
  HTML_H2         CONSTANT tSmallVarchar2:='h2';
  HTML_H3         CONSTANT tSmallVarchar2:='h3';
  HTML_H4         CONSTANT tSmallVarchar2:='h4';
  HTML_P          CONSTANT tSmallVarchar2:='p';
  HTML_B          CONSTANT tSmallVarchar2:='b';
  HTML_BR         CONSTANT tSmallVarchar2:='br';
  HTML_LI         CONSTANT tSmallVarchar2:='li';
  HTML_UL         CONSTANT tSmallVarchar2:='ul';
  HTML_OL         CONSTANT tSmallVarchar2:='ol';
  HTML_LT         CONSTANT tSmallVarchar2:='<';
  HTML_GT         CONSTANT tSmallVarchar2:='>';
  HTML_PRE        CONSTANT tSmallVarchar2:='pre';
  HTML_STRONG     CONSTANT tSmallVarchar2:='strong';
  HTML_TEXT       CONSTANT tSmallVarchar2:='text';
  HTML_CODE       CONSTANT tSmallVarchar2:='code';
  HTML_SPAN       CONSTANT tSmallVarchar2:='span';
  HTML_P_START    CONSTANT tSmallVarchar2:='<p>';
  HTML_P_END      CONSTANT tSmallVarchar2:='</p>';

  MIN_LINE_SPACING  CONSTANT NUMBER:=1.17;

  EVALUATION_NOW    CONSTANT tAttribute:='Now';
  EVALUATION_GROUP  CONSTANT tAttribute:='Group';
  EVALUATION_REPORT CONSTANT tAttribute:='Report';

  RENDER_FRAME_NONE   CONSTANT NUMBER:=1;
  RENDER_FRAME_BEFORE CONSTANT NUMBER:=2;
  RENDER_FRAME_AFTER  CONSTANT NUMBER:=3;

  BOOL_NO             CONSTANT tYesNo:='N';
  BOOL_YES            CONSTANT tYesNo:='Y';

  REPORT_LOCALE       CONSTANT tAttribute:='REPORT_LOCALE';

  REPORT_TYP_REPORT CONSTANT NUMBER:=1;
  REPORT_TYP_TABLE  CONSTANT NUMBER:=2;

  NUM_FORMAT_MASK   CONSTANT tAttribute:='FM999999999999990D099999';
  DATE_FORMAT_MASK  CONSTANT tAttribute:='DD.MM.YYYY HH24:MI:SS';

  OFFSET_NONE       CONSTANT NUMBER:=0;
  OFFSET_PREVIOUS   CONSTANT NUMBER:=1;
  OFFSET_NEXT       CONSTANT NUMBER:=2;

  HELVETICA         CONSTANT tFont:='helvetica';
  ARIAL             CONSTANT tFont:='Arial';
  WINGDINGS         CONSTANT tFont:='WingDings';
  COURIER_NEW       CONSTANT tFont:='Courier New';

  FONT_NORMAL       CONSTANT tFontStyle:='N';
  FONT_BOLD         CONSTANT tFontStyle:='B';
  FONT_ITALIC       CONSTANT tFontStyle:='I';
  FONT_BOLD_ITALIC  CONSTANT tFontStyle:='BI';

  YES               CONSTANT tYesNo:='Y';
  NO                CONSTANT tYesNo:='N';

  DATATYPE_VARCHAR  CONSTANT tDatatype:='V';
  DATATYPE_NUMBER   CONSTANT tDatatype:='N';
  DATATYPE_DATE     CONSTANT tDatatype:='D';
  DATATYPE_BLOB     CONSTANT tDatatype:='B';

  CALCULATION_HIGHEST CONSTANT tAttribute:='Highest';
  CALCULATION_LOWEST  CONSTANT tAttribute:='Lowest';
  CALCULATION_COUNT   CONSTANT tAttribute:='Count';
  CALCULATION_SUM     CONSTANT tAttribute:='Sum';
  CALCULATION_AVERAGE CONSTANT tAttribute:='Average';
  CALCULATION_FIRST   CONSTANT tAttribute:='First';
  CALCULATION_NOTHING CONSTANT tAttribute:='#';

  ORDERBY_ASCENDING   CONSTANT tAttribute:='Ascending';
  ORDERBY_DESCENDING  CONSTANT tAttribute:='Descending';

  RELATIVE_TO_TOP     CONSTANT tAttribute:='FixRelativeToTop';
  RELATIVE_TO_BOTTOM  CONSTANT tAttribute:='FixRelativeToBottom';

  IMMEDIATE           CONSTANT tAttribute:='Immediate';
  STRETCH             CONSTANT tAttribute:='Stretch';
  PREVENT             CONSTANT tAttribute:='Prevent';

  PIXEL               CONSTANT tSmallVarchar2:='px';

  TYPE tNumList        IS TABLE OF NUMBER            INDEX BY BINARY_INTEGER;
  TYPE tMaxVarcharList IS TABLE OF tMaxVarchar2      INDEX BY BINARY_INTEGER;
  TYPE tIndexList      IS TABLE OF tIndex            INDEX BY BINARY_INTEGER;
  TYPE tColorList      IS TABLE OF tColor            INDEX BY BINARY_INTEGER;
  TYPE tReportNameList IS TABLE OF tName             INDEX BY BINARY_INTEGER;

  TYPE tPatternList    IS TABLE OF tDatePattern      INDEX BY tIndex;
  TYPE tAttributeList  IS TABLE OF tAttribute        INDEX BY tAttribute;


  TYPE tArea IS RECORD (
    nX      NUMBER,
    nY      NUMBER,
    nPage   NUMBER
  );

  TYPE tAreaList IS TABLE OF tArea INDEX BY BINARY_INTEGER;

  TYPE tToken IS RECORD (
    vcStartTag tAttribute,
    vcEndTag   tAttribute
  );

  TYPE tTokenList IS TABLE OF tToken INDEX BY BINARY_INTEGER;

  TYPE tTokenValue IS RECORD (
    vcFoundTag tAttribute,
    vcEndTag   tAttribute
  );

  TYPE tTokenValueList IS TABLE OF tTokenValue INDEX BY BINARY_INTEGER;

  TYPE tSimpleStyle IS RECORD (
    vcWhenExpression tExpression,
    vcFGColor        tColor,
    vcBGColor        tColor,
    vcFont           tFont,
    vcFontStyle      tFontStyle,
    vcAlignment      tAlignment,
    vcVerticalAlign  tAlignment,
    nFontSize        NUMBER,
    nLineWidth       NUMBER,
    nBoxTop          NUMBER,
    nBoxLeft         NUMBER,
    nBoxBottom       NUMBER,
    nBoxRight        NUMBER,
    nTopPadding      NUMBER,
    nLeftPadding     NUMBER,
    nBottomPadding   NUMBER,
    nRightPadding    NUMBER,
    vcBoxTopColor    tColor,
    vcBoxLeftColor   tColor,
    vcBoxBottomColor tColor,
    vcBoxRightColor  tColor,
    nLineSpacing     NUMBER,
    vcOpaque        tYesNo
  );

  TYPE tSimpleStyleList IS TABLE OF tSimpleStyle INDEX BY tStyleName;

  TYPE tStyle IS RECORD (
    rStyle             tSimpleStyle,
    lConditionalStyles tSimpleStyleList
  );

  TYPE tStyleList IS TABLE OF tStyle INDEX BY tStyleName;

  TYPE tPageFrameMarker IS RECORD (
    nBorderTop   NUMBER,
    nBorderWidth NUMBER,
    rStyle       tSimpleStyle
  );

  TYPE tPageFrameMarkerList IS TABLE OF tPageFrameMarker INDEX BY BINARY_INTEGER;


  TYPE tLine IS RECORD (
    nX                NUMBER,
    nY                NUMBER,
    nWidth            NUMBER,
    nHeight           NUMBER,
    vcLineColor       tColor,
    nLineWidth        NUMBER,
    vcStretch         tYesNo,
    vcStyle           tStyleName,
    vcWhenExpression  tExpression,
    vcPositionType    tPositionType
  );

  TYPE tLineList IS TABLE OF tLine INDEX BY BINARY_INTEGER;

  TYPE tRectangle IS RECORD (
    nX               NUMBER,
    nY               NUMBER,
    nWidth           NUMBER,
    nHeight          NUMBER,
    vcLineColor      tColor,
    vcFillColor      tColor,
    nLineWidth       NUMBER,
    vcStretch        tYesNo,
    vcStyle          tStyleName,
    vcWhenExpression tExpression,
    vcOpaque         tYesNo,
    vcPositionType   tPositionType
);

  TYPE tRectangleList IS TABLE OF tRectangle INDEX BY BINARY_INTEGER;

  TYPE tImage IS RECORD (
    vcImageName      tName,
    nX               NUMBER,
    nY               NUMBER,
    nWidth           NUMBER,
    nHeight          NUMBER,
    vcLineColor      tColor,
    vcFillColor      tColor,
    vcStyle          tStylename,
    nBoxTop          NUMBER,
    nBoxLeft         NUMBER,
    nBoxBottom       NUMBER,
    nBoxRight        NUMBER,
    vcBoxTopColor    tColor,
    vcBoxLeftColor   tColor,
    vcBoxBottomColor tColor,
    vcBoxRightColor  tColor,
    vcWhenExpression tExpression,
    vcPositionType   tPositionType
  );

  TYPE tImageList IS TABLE OF tImage INDEX BY BINARY_INTEGER;

  TYPE tText IS RECORD (
    vcText           tMaxVarchar2,
    nX               NUMBER,
    nY               NUMBER,
    nWidth           NUMBER,
    nHeight          NUMBER,
    vcBGColor        tColor,
    vcFGColor        tColor,
    vcStyle          tStylename,
    vcFont           tFont,
    vcFontStyle      tFontStyle,
    vcAlignment      tAlignment,
    vcVerticalAlign  tAlignment,
    nFontSize        NUMBER,
    nBoxTop          NUMBER,
    nBoxLeft         NUMBER,
    nBoxBottom       NUMBER,
    nBoxRight        NUMBER,
    nTopPadding      NUMBER,
    nLeftPadding     NUMBER,
    nBottomPadding   NUMBER,
    nRightPadding    NUMBER,
    vcBoxTopColor    tColor,
    vcBoxLeftColor   tColor,
    vcBoxBottomColor tColor,
    vcBoxRightColor  tColor,
    vcStretch        tYesNo,
    vcWhenExpression tExpression,
    vcOpaque         tYesNo,
    vcPositionType   tPositionType,
    vcRotation       tAttribute
  );

  TYPE tTextList IS TABLE OF tText INDEX BY BINARY_INTEGER;

  TYPE tField IS RECORD (
    vcExpression        tExpression,
    vcPattern           tPattern,
    vcPatternExpression tExpression,
    vcPrintRepeated     tYesNo,
    nX                  NUMBER,
    nY                  NUMBER,
    nWidth              NUMBER,
    nHeight             NUMBER,
    vcBGColor           tColor,
    vcFGColor           tColor,
    vcStyle             tStyleName,
    vcFont              tFont,
    vcFontStyle         tFontStyle,
    vcAlignment         tAlignment,
    vcVerticalAlign     tAlignment,
    nFontSize           NUMBER,
    nBoxTop             NUMBER,
    nBoxLeft            NUMBER,
    nBoxBottom          NUMBER,
    nBoxRight           NUMBER,
    nTopPadding         NUMBER,
    nLeftPadding        NUMBER,
    nBottomPadding      NUMBER,
    nRightPadding       NUMBER,
    vcBoxTopColor       tColor,
    vcBoxLeftColor      tColor,
    vcBoxBottomColor    tColor,
    vcBoxRightColor     tColor,
    vcStretch           tYesNo,
    vcStretchOverflow   tYesNo,
    vcEvaluationTime    tAttribute,
    vcEvaluationGroup   tGroupname,
    vcWhenExpression    tExpression,
    nLineSpacing        NUMBER,
    vcOpaque            tYesNo,
    vcPositionType      tPositionType,
    vcMarkup            tAttribute,
    vcKey               tAttribute,
    vcRotation          tAttribute
  );

  TYPE tFieldList IS TABLE OF tField INDEX BY BINARY_INTEGER;

  TYPE tParameter IS RECORD (
    vcName  VARCHAR2(255),
    vcValue VARCHAR2(32767)
  );

  TYPE tParamList IS TABLE OF tParameter INDEX BY BINARY_INTEGER;

  TYPE tSubreport IS RECORD (
    vcReportName     tName,
    nX               NUMBER,
    nY               NUMBER,
    nWidth           NUMBER,
    nHeight          NUMBER,
    vcStyle          tStylename,
    lParamList       tParamList
  );

  TYPE tSubreportList IS TABLE OF tSubreport INDEX BY BINARY_INTEGER;

  TYPE tBreak IS RECORD (
    nX                NUMBER,
    nY                NUMBER,
    nWidth            NUMBER,
    nHeight           NUMBER,
    vcType            tAttribute,
    vcWhenExpression  tExpression
  );

  TYPE tBreakList IS TABLE OF tBreak INDEX BY BINARY_INTEGER;

  TYPE tDataEntry IS RECORD (
    vcDataType tDatatype,
    vcData     tMaxVarchar2,
    dtData     DATE,
    nData      NUMBER,
    blData     BLOB
  );

  TYPE tDataEntries IS TABLE OF tDataEntry INDEX BY tIndex;

  TYPE tQueryResult IS TABLE OF tDataEntries INDEX BY BINARY_INTEGER;

  TYPE tObject IS RECORD (
    nType     NUMBER,
    nPosition NUMBER
  );

  TYPE tObjectList IS TABLE OF tObject INDEX BY BINARY_INTEGER;

  TYPE tRegion IS RECORD (
    vcBaseNode tAttribute,
    lObjects   tObjectList
  );

  TYPE tRegionList IS TABLE OF tRegion INDEX BY BINARY_INTEGER;

  TYPE tBand IS RECORD (
    nX               NUMBER,
    nY               NUMBER,
    nWidth           NUMBER,
    nHeight          NUMBER,
    vcBGColor        tColor,
    vcFGColor        tColor,
    vcStyle          tStyleName,
    nBoxTop          NUMBER,
    nBoxLeft         NUMBER,
    nBoxBottom       NUMBER,
    nBoxRight        NUMBER,
    vcBoxTopColor    tColor,
    vcBoxLeftColor   tColor,
    vcBoxBottomColor tColor,
    vcBoxRightColor  tColor,
    vcStretch        tYesNo,
    vcOpaque         tYesNo,
    vcTyp            tAttribute,
    bHasBreaks       BOOLEAN,
    lObjects         tObjectList,
    vcWhenExpression tExpression,
    vcPositionType   tPositionType,
    bHasTopPos       BOOLEAN,
    bHasBottomPos    BOOLEAN,
    nMaxPosTop       NUMBER,
    nMinPosBottom    NUMBER,
    vcSplitType      tSplitType
  );

  TYPE tBandList IS TABLE OF tBand INDEX BY BINARY_INTEGER;

  TYPE tCrosstabRow IS RECORD (
    vcName             tName,
    nWidth             NUMBER,
    nHeight            NUMBER,
    vcBucketExpression tExpression,
    vcTotalPosition    tPosition,
    vcOrderBy          tAttribute,
    vcDatatype         tDatatype,
    nHeaderPos         NUMBER,
    nTotalHeaderPos    NUMBER,
    vcHeaderPosition   tHeaderPosition
  );

  TYPE tCrosstabRowList IS TABLE OF tCrosstabRow INDEX BY BINARY_INTEGER;

  TYPE tCrosstabMeasure IS RECORD (
    vcName             tName,
    vcCalculation      tCalculation,
    vcExpression       tExpression,
    vcDatatype         tDatatype
  );

  TYPE tCrosstabMeasureList IS TABLE OF tCrosstabMeasure INDEX BY BINARY_INTEGER;

  TYPE tCrosstabCell IS RECORD (
    vcName             tCrosstabCellname,
    vcRowGroup         tName,
    vcColgroup         tName,
    nWidth             NUMBER,
    nHeight            NUMBER,
    nCellPos           NUMBER
  );

  TYPE tCrosstabCellList IS TABLE OF tCrosstabCell INDEX BY tCrosstabCellName;

  TYPE tCrosstab IS RECORD (
    nX                 NUMBER,
    nY                 NUMBER,
    nWidth             NUMBER,
    nHeight            NUMBER,
    vcRepeatColHeaders tYesNo,
    vcRepeatRowHeaders tYesNo,
    nColBreakOffset    NUMBER,
    vcStyle            tStylename,
    vcQuery            tMaxVarchar2,
    vcWhenExpression   tExpression,
    lParams            tParamList,
    lRows              tCrossTabRowList,
    lColumns           tCrossTabRowList,
    lMeasures          tCrosstabMeasureList,
    lCells             tCrosstabCellList,
    rHeaderCell        tCrosstabCell
  );

  TYPE tCrosstabList IS TABLE OF tCrosstab INDEX BY BINARY_INTEGER;

  TYPE tGroup IS RECORD (
    vcName            tGroupname,
    vcExpression      tExpression,
    vcStartOnNewPage  tYesNo,
    vcResetPageNumber tYesNo,
    vcReprintHeader   tYesNo,
    nMinHeightForPage NUMBER,
    rGroupHeader      tRegion,
    rGroupFooter      tRegion
  );

  TYPE tGroupList IS TABLE OF tGroup INDEX BY BINARY_INTEGER;

  TYPE tDataset IS RECORD (
    vcQuery tMaxVarchar2,
    lGroups tGroupList
  );

  TYPE tDatasetList    IS TABLE OF tDataset INDEX BY tIndex;


  TYPE tMap IS RECORD (
    nX                    NUMBER,
    nY                    NUMBER,
    nWidth                NUMBER,
    nHeight               NUMBER,
    vcLineColor           tColor,
    vcFillColor           tColor,
    vcStyle               tStylename,
    nBoxTop               NUMBER,
    nBoxLeft              NUMBER,
    nBoxBottom            NUMBER,
    nBoxRight             NUMBER,
    vcBoxTopColor         tColor,
    vcBoxLeftColor        tColor,
    vcBoxBottomColor      tColor,
    vcBoxRightColor       tColor,
    vcWhenExpression      tExpression,
    vcPositionType        tPositionType,
    vcLongitudeExpression tExpression,
    vcLatitudeExpression  tExpression,
    vcZoomExpression      tExpression
  );

  TYPE tMapList IS TABLE OF tMap INDEX BY BINARY_INTEGER;

  TYPE tQuery IS RECORD (
    iCursor             PLS_INTEGER,
    lDescTab            DBMS_SQL.DESC_TAB,
    lQueryResult        tQueryResult,
    bEOF                BOOLEAN,
    nRecordPosition     NUMBER,
    nRecordRead         NUMBER,
    nPreviousRecord     PLS_INTEGER,
    nCurrentRecord      PLS_INTEGER,
    nNextRecord         PLS_INTEGER,
    bTreatLastAsCurrent BOOLEAN
  );

  TYPE tLocaledata IS RECORD (
    vcLocale         tAttribute,
    vcDateDefault    tAttribute,
    vcDateShort      tAttribute,
    vcDateMedium     tAttribute,
    vcDateLong       tAttribute,
    vcTimeDefault    tAttribute,
    vcTimeShort      tAttribute,
    vcTimeMedium     tAttribute,
    vcTimeLong       tAttribute,
    vcDateTimeString tAttribute,
    vcCurrency       tAttribute,
    vcNumericChars   tAttribute,
    vcDateLanguage   tAttribute
  );

  TYPE tResourceList IS TABLE OF tMaxVarchar2 INDEX BY tMaxVarchar2;

  TYPE tCategorySeries IS RECORD (
    vcSeriesExpression   tExpression,
    vcValueExpression    tExpression,
    vcCategoryExpression tExpression
  );

  TYPE tCategorySeriesList IS TABLE OF tCategorySeries INDEX BY BINARY_INTEGER;

  TYPE tBarChart IS RECORD (
    nX                            NUMBER,
    nY                            NUMBER,
    nWidth                        NUMBER,
    nHeight                       NUMBER,
    vcLineColor                   tColor,
    vcFillColor                   tColor,
    vcStyle                       tStylename,
    nBoxTop                       NUMBER,
    nBoxLeft                      NUMBER,
    nBoxBottom                    NUMBER,
    nBoxRight                     NUMBER,
    vcBoxTopColor                 tColor,
    vcBoxLeftColor                tColor,
    vcBoxBottomColor              tColor,
    vcBoxRightColor               tColor,
    nTopPadding                   NUMBER,
    nLeftPadding                  NUMBER,
    nBottomPadding                NUMBER,
    nRightPadding                 NUMBER,
    vcOpaque                      tYesNo,
    vcWhenExpression              tExpression,
    vcPositionType                tPositionType,
    vcQuery                       tMaxVarchar2,
    lParams                       tParamList,
    vcIs3D                        tYesNo,
    vcIsStacked                   tYesNo,
    vcTitleExpression             tExpression,
    vcTitleColor                  tColor,
    vcTitlePosition               tPosition,
    vcTitleFont                   tFont,
    vcTitleFontStyle              tFontStyle,
    nTitleFontSize                NUMBER,
    vcSubTitleExpression          tExpression,
    vcSubTitleColor               tColor,
    vcSubTitleFont                tFont,
    vcSubTitleFontStyle           tFontStyle,
    nSubTitleFontSize             NUMBER,
    vcShowLegend                  tYesNo,
    vcLegendTextColor             tColor,
    vcLegendBgColor               tColor,
    vcLegendFont                  tFont,
    vcLegendFontStyle             tFontStyle,
    nLegendFontSize               NUMBER,
    vcLegendPosition              tPosition,
    lSeries                       tCategorySeriesList,
    lSeriesColors                 tColorList,
    vcShowLabels                  tYesNo,
    vcLabelFont                   tFont,
    vclabelFontStyle              tFontStyle,
    vcLabelColor                  tColor,
    nLabelFontSize                NUMBER,
    vcShowTickLabels              tYesNo,
    vcShowTickMarks               tYesNo,
    vcCatAxisLabelExpression      tExpression,
    vcCatAxisLabelColor           tColor,
    vcCatAxisLabelFont            tFont,
    vcCatAxisLabelFontStyle       tFontStyle,
    nCatAxisLabelFontSize         NUMBER,
    vcCatAxisTickLabelColor       tColor,
    vcCatAxisTickLabelFont        tFont,
    vcCatAxisTickLabelFontStyle   tFontStyle,
    nCatAxisTickLabelFontSize     NUMBER,
    vcCatAxisTickLabelPattern     tPattern,
    vcCatAxisLineColor            tColor,
    vcValueAxisLabelExpression    tExpression,
    vcValueAxisLabelColor         tColor,
    vcValueAxisLabelFont          tFont,
    vcValueAxisLabelFontStyle     tFontStyle,
    nValueAxisLabelFontSize       NUMBER,
    vcValueAxisTickLabelColor     tColor,
    vcValueAxisTickLabelFont      tFont,
    vcValueAxisTickLabelFontStyle tFontStyle,
    nAxisTickLabelFontSize        NUMBER,
    vcValueAxisTickLabelPattern   tPattern,
    vcValueAxisLineColor          tColor,
    vcValueAxisMinValExpression   tExpression,
    vcValueAxisMaxValExpression   tExpression,
    vcCatAxisMinValExpression     tExpression,
    vcCatAxisMaxValExpression     tExpression,
    vcCustomizerClass             tMaxVarchar2,
    nLabelRotation                NUMBER
  );

  TYPE tBarChartList IS TABLE OF tBarChart INDEX BY BINARY_INTEGER;

  TYPE tLineChart IS RECORD (
    nX                            NUMBER,
    nY                            NUMBER,
    nWidth                        NUMBER,
    nHeight                       NUMBER,
    vcLineColor                   tColor,
    vcFillColor                   tColor,
    vcStyle                       tStylename,
    nBoxTop                       NUMBER,
    nBoxLeft                      NUMBER,
    nBoxBottom                    NUMBER,
    nBoxRight                     NUMBER,
    vcBoxTopColor                 tColor,
    vcBoxLeftColor                tColor,
    vcBoxBottomColor              tColor,
    vcBoxRightColor               tColor,
    nTopPadding                   NUMBER,
    nLeftPadding                  NUMBER,
    nBottomPadding                NUMBER,
    nRightPadding                 NUMBER,
    vcOpaque                      tYesNo,
    vcWhenExpression              tExpression,
    vcPositionType                tPositionType,
    vcQuery                       tMaxVarchar2,
    lParams                       tParamList,
    vcIs3D                        tYesNo,
    vcTitleExpression             tExpression,
    vcTitleColor                  tColor,
    vcTitlePosition               tPosition,
    vcTitleFont                   tFont,
    vcTitleFontStyle              tFontStyle,
    nTitleFontSize                NUMBER,
    vcSubTitleExpression          tExpression,
    vcSubTitleColor               tColor,
    vcSubTitleFont                tFont,
    vcSubTitleFontStyle           tFontStyle,
    nSubTitleFontSize             NUMBER,
    vcShowLegend                  tYesNo,
    vcLegendTextColor             tColor,
    vcLegendBgColor               tColor,
    vcLegendFont                  tFont,
    vcLegendFontStyle             tFontStyle,
    nLegendFontSize               NUMBER,
    vcLegendPosition              tPosition,
    lSeries                       tCategorySeriesList,
    lSeriesColors                 tColorList,
    vcShowLabels                  tYesNo,
    vcLabelFont                   tFont,
    vclabelFontStyle              tFontStyle,
    vcLabelColor                  tColor,
    nLabelFontSize                NUMBER,
    vcShowTickLabels              tYesNo,
    vcShowTickMarks               tYesNo,
    vcCatAxisLabelExpression      tExpression,
    vcCatAxisLabelColor           tColor,
    vcCatAxisLabelFont            tFont,
    vcCatAxisLabelFontStyle       tFontStyle,
    nCatAxisLabelFontSize         NUMBER,
    vcCatAxisTickLabelColor       tColor,
    vcCatAxisTickLabelFont        tFont,
    vcCatAxisTickLabelFontStyle   tFontStyle,
    nCatAxisTickLabelFontSize     NUMBER,
    vcCatAxisTickLabelPattern     tPattern,
    vcCatAxisLineColor            tColor,
    vcValueAxisLabelExpression    tExpression,
    vcValueAxisLabelColor         tColor,
    vcValueAxisLabelFont          tFont,
    vcValueAxisLabelFontStyle     tFontStyle,
    nValueAxisLabelFontSize       NUMBER,
    vcValueAxisTickLabelColor     tColor,
    vcValueAxisTickLabelFont      tFont,
    vcValueAxisTickLabelFontStyle tFontStyle,
    nAxisTickLabelFontSize        NUMBER,
    vcValueAxisTickLabelPattern   tPattern,
    vcValueAxisLineColor          tColor,
    vcValueAxisMinValExpression   tExpression,
    vcValueAxisMaxValExpression   tExpression,
    vcCatAxisMinValExpression     tExpression,
    vcCatAxisMaxValExpression     tExpression,
    vcCustomizerClass             tMaxVarchar2,
    nLabelRotation                NUMBER,
    vcShowLines                   tYesNo,
    vcShowShapes                  tYesNo
  );

  TYPE tLineChartList IS TABLE OF tLineChart INDEX BY BINARY_INTEGER;

  TYPE tPieChart IS RECORD (
    nX                    NUMBER,
    nY                    NUMBER,
    nWidth                NUMBER,
    nHeight               NUMBER,
    vcLineColor           tColor,
    vcFillColor           tColor,
    vcStyle               tStylename,
    nBoxTop               NUMBER,
    nBoxLeft              NUMBER,
    nBoxBottom            NUMBER,
    nBoxRight             NUMBER,
    vcBoxTopColor         tColor,
    vcBoxLeftColor        tColor,
    vcBoxBottomColor      tColor,
    vcBoxRightColor       tColor,
    nTopPadding           NUMBER,
    nLeftPadding          NUMBER,
    nBottomPadding        NUMBER,
    nRightPadding         NUMBER,
    vcOpaque              tYesNo,
    vcWhenExpression      tExpression,
    vcPositionType        tPositionType,
    vcQuery               tMaxVarchar2,
    lParams               tParamList,
    vcIs3D                tYesNo,
    vcTitleExpression     tExpression,
    vcTitleColor          tColor,
    vcTitlePosition       tPosition,
    vcTitleFont           tFont,
    vcTitleFontStyle      tFontStyle,
    nTitleFontSize        NUMBER,
    vcSubTitleExpression  tExpression,
    vcSubTitleColor       tColor,
    vcSubTitleFont        tFont,
    vcSubTitleFontStyle   tFontStyle,
    nSubTitleFontSize     NUMBER,
    vcShowLegend          tYesNo,
    vcLegendTextColor     tColor,
    vcLegendBgColor       tColor,
    vcLegendFont          tFont,
    vcLegendFontStyle     tFontStyle,
    nLegendFontSize       NUMBER,
    vcLegendPosition      tPosition,
    vcKeyExpression       tExpression,
    vcValueExpression     tExpression,
    lSeriesColors         tColorList,
    vcShowLabels          tYesNo,
    vcLabelFont           tFont,
    vclabelFontStyle      tFontStyle,
    vcLabelColor          tColor,
    nLabelFontSize        NUMBER,
    vcLabelFormat         tExpression,
    vcLegendLabelFormat   tExpression,
    vcCustomizerClass     tMaxVarchar2
  );

  TYPE tPieChartList IS TABLE OF tPieChart INDEX BY BINARY_INTEGER;

  TYPE tReport IS RECORD (
     nId                     NUMBER,
     nTyp                    NUMBER,
     nPageWidth              NUMBER,
     nPageHeight             NUMBER,
     nLeftMargin             NUMBER,
     nRightMargin            NUMBER,
     nTopMargin              NUMBER,
     nBottomMargin           NUMBER,
     vcFloatColumnFooter     tYesNo,
     vcTitleOnNewPage        tYesNo,
     vcSummaryOnNewPage      tYesNo,
     vcSummaryWithHdrFtr     tYesNo,
     vcResourceBundle        tName,
     nPageHdrHeight          NUMBER,
     nColumnHdrheight        NUMBER,
     nColumnFtrheight        NUMBER,
     nPageFtrHeight          NUMBER,
     nSummaryHeight          NUMBER,
     vcQuery                 tMaxVarchar2,
     lParams                 tParamList,
     lStyles                 tStyleList,
     rQuery                  tQuery,
     rLocaleData             tLocaleData,
     nXPositionHdr           NUMBER,
     rTitleRegion            tRegion,
     rPageHeaderRegion       tRegion,
     rColumnHeaderRegion     tRegion,
     rDetailRegion           tRegion,
     rColumnFooterRegion     tRegion,
     rPageFooterRegion       tRegion,
     rSummaryRegion          tRegion,
     rBackgroundRegion       tRegion,
     lObjects                tObjectList,
     lRegions                tRegionList,
     lBands                  tBandList,
     lLines                  tLineList,
     lRectangles             tRectangleList,
     lImages                 tImageList,
     lTexts                  tTextList,
     lFields                 tFieldList,
     lSubreports             tSubreportList,
     lBreaks                 tBreakList,
     lEvalLaterFields        tFieldList,
     lEvalLaterPageNo        tNumList,
     lGroups                 tGroupList,
     lGroupExpressions       tMaxVarcharList,
     lDatasets               tDatasetList,
     lCrosstabs              tCrosstabList,
     lMaps                   tMapList,
     lBarCharts              tBarchartList,
     lPieCharts              tPieChartList,
     lCategoryLineCharts     tLineChartList,
     lXYLineCharts           tLineChartList,
     lTimeLineCharts         tLineChartList,
     lVariableCache          tDataEntries,
     lResources              tResourceList,
     lPageNumbers            tNumList,
     lLogicalPageNumbers     tNumList,
     lStartOfPageDone        tNumList,
     lEndOfPageDone          tNumList,
     nCurrentPagePointer     NUMBER,
     lStoredPositions        tAreaList,
     lPageFrameMarkers       tPageFrameMarkerList
  );

  TYPE tReportCache IS TABLE OF tReport INDEX BY tIndex;

  TYPE tFontNames IS TABLE OF tFont INDEX BY tFont;

  TYPE tXmlRegion IS RECORD (
    xml     XMLTYPE,
    nHeight NUMBER
  );

  TYPE tXmlRegionList IS TABLE OF tXmlRegion INDEX BY BINARY_INTEGER;

  TYPE tPageSetup IS RECORD (
    nPageWidth    NUMBER,
    nPageHeight   NUMBER,
    nLeftMargin   NUMBER,
    nRightMargin  NUMBER,
    nTopMargin    NUMBER,
    nBottomMargin NUMBER
  );

  TYPE tReportList IS TABLE OF tReport INDEX BY BINARY_INTEGER;

  TYPE tTextPiece IS RECORD (
    nX      NUMBER,
    nHeight NUMBER,
    nGroup  NUMBER,
    nLength NUMBER,
    vcText  tMaxVarchar2
  );

  TYPE tTextPieceList IS TABLE OF tTextPiece INDEX BY BINARY_INTEGER;

  TYPE tHtmlSettings IS RECORD (
    vcFont            tFont,
    nFontSize         NUMBER,
    nH1FontSize       NUMBER,
    nH2FontSize       NUMBER,
    nH3FontSize       NUMBER,
    nH4FontSize       NUMBER,
    nXXlargeFontSize  NUMBER,
    nXlargeFontSize   NUMBER,
    nLargeFontSize    NUMBER,
    nMediumFontSize   NUMBER,
    nSmallFontSize    NUMBER,
    nXsmallFontSize   NUMBER,
    nXXsmallFontSize  NUMBER,
    nLargerFontSize   NUMBER,
    nSmallerFontSize  NUMBER,
    nListIndent       NUMBER,
    vcUlChar          tChar,
    nH1LineSpacing    NUMBER,
    nH2LineSpacing    NUMBER,
    nH3LineSpacing    NUMBER,
    nH4LineSpacing    NUMBER,
    nLiLineSpacing    NUMBER,
    nBrLineSpacing    NUMBER,
    nLineSpacing      NUMBER
  );

  TYPE tHtmlToken IS RECORD (
    vcTag       tAttribute,
    bHasEndTag  BOOLEAN,
    bHasAttribs BOOLEAN
  );

  TYPE tHtmlPiece IS RECORD (
    vcTag       tAttribute,
    lAttribs    tAttributeList,
    vcText      tMaxVarchar2,
    lTextPieces tTextPieceList,
    vcFont      tFont,
    nGroup      NUMBER,
    nFontSize   NUMBER,
    vcFontStyle tFontStyle,
    vcColor     tColor,
    bIsPRE      BOOLEAN,
    bInList     BOOLEAN,
    nListIndex  NUMBER,
    nX          NUMBER,
    nHeight     NUMBER,
    lSubPieces  tNumList
  );

  TYPE tHtmlPieceList IS TABLE OF tHtmlPiece INDEX BY BINARY_INTEGER;

  TYPE tHtmlTree IS RECORD (
    lPieces    tHtmlPieceList
  );

  TYPE tHtmlTokenList IS TABLE OF tHtmlToken INDEX BY BINARY_INTEGER;

  TYPE tHtmlReplacementList IS TABLE OF VARCHAR2(10) INDEX BY VARCHAR2(20);

  TYPE tTextCacheEntry IS RECORD (
    lText          tTextPieceList,
    nCurrentHeight NUMBER,
    nX             NUMBER,
    nGroup         NUMBER
  );

  TYPE tTextCacheList IS TABLE OF tTextCacheEntry INDEX BY tMaxVarchar2;


END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_TYPES IS



END;
/