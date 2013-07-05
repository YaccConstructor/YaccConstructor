create or replace
PACKAGE PK_JRXML2PDF_REPGEN AUTHID CURRENT_USER IS
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

  $name    PK_JRXML2PDF_REPGEN

  $created 22.05.2012

  $author  Andreas Weiden

  $desc    Package to generate PDF-files from a JRXML-report-definition
           see documentation for further details

  $version 0.5.0.0 22.05.2012 Weiden
           initial beta-version

  $version 0.5.0.1 29.05.2012 Weiden
           Bug-Fix: Init of PDF before loading needed for page-headers with fields
                    Read values of last record in End of page-rendering

  $version 0.5.1.1 30.05.2012 Weiden
           Enhance: Add Parameter i_vcContentDisposition to PR_SHOW_REPORT

  $version 0.6.0.0 31.05.2012 Weiden
           Added support for custom fonts

  $version 0.6.0.1 08.06.2012 Weiden
           Bug-Fixes: Error in Evaluation with more than one field
                      Do not try to load font if fontname is empty

  $version 0.6.1.1 08.06.2012 Weiden
           Enhance: Added Support für Line-Spacing
                    Rewrite of procedures to calculate Box-Height and write boxed text
                    in preparation for page-overflowing fields

  $version 0.6.3.1 13.07.2012 Weiden
           Enhance/Bugfix: Rewrite of all XMLTABLE-usages because
                           of strange behaviour in 11G 11.2.0.3.0

  $version 0.7.0.0 02.08.2012 Weiden
           Features: - support for jr:table-component with
                       + tableHeader, columnHeader, detailCell, columnFooter and tableFooter-cells
                       + nested columngroups
                       + conditional rendering of columns (only top-level, not inside columngroups)
                       + border around table-component
           Bugfixes: - "Stretch with overflow" in combination with paddings caused
                       missing textparts in multiline text
                     - Styles with names longer than 20 chars where cut off and not applied
                     - Parameter expressions ($P{}) where not parsed
                     - when a frame's content was extended in height, the fill-area for the frame
                       was not adjusted
                     - Special handling of parameter SUBREPORT_DIR
                     - empty expressions lead to endless loop
                     - bandheight could be smaller than minimum bandwidth when using subreports
                       (Thanks to Tom Petrus for providing a solution)
                     - fine-tuning of borders, especially when forming boxes
                     - Style-correction: Bold/Italic from style not used
                     - Style-correction: Boxline-colors and widths from style not used
           Enhance : - The property "mode" is now supported, so that objects without a color and
                       mode not set to "Opaque" are transparent and therefore have the color
                       of the underlying object
                     - the property verticalAlignment is now supported for Static text and textfields
                     - Added expression-support for
                       ! as NOT
                       .startsWith(EXPRESSION) as LIKE ('%' || EXPRESSION || '%')
                     - support for properties pen/@lineWidth and pen/@lineColor in various objects

  $version 0.8.0.0 08.09.2012 Weiden

           Features: - Support for crosstabs
                       + multiple column-groups, row-groups and measures
                       + measure-functions COUNT, AVG, SUM, HIGHEST, LOWEST, FIRST, NOTHING
                       + summaries at start or end
                       + positioning of row- and column-headers
                       + access to summary-variables at detail-level
                       + crosstabs-splitting on page-end or column-end
                     - Locale/NLS-support
                       + Usage of resource-files (including unincode-chars)
                       + Usage of $R{} syntax in expressions
                       + Usage of msg()-function
                       + auto-detection of date-, time-, and number-formats depedning on locale and or nls
                       + support for date-formats short, medium, long and default
           Bugfixes: - NULL-Pagesetup lead to endless loop in calculation of Overflow-fields in Sections other than detail
                     - Objects with negative y-Offset did not render

           Enhance : - Support for group-property isReprintHeaderOnEachPage
                     - Support for group-property minHeightToStartNewPage
                     - Linetuning for linespacings "Single", "1_1_2" and "Double"
                     - Allow to add filename to PR_SHOW_REPORT
                     - support for textfield-property patternExpression

  $version 0.9.0.0 04.10.2012 Weiden

           Features: - support for textfields with markup html both in standard way and enhanced
                       (different implementation than Jasper)
                     - Map-component to show google-maps
           Enhance : - support for property positionType for various objects
                     - support for more thane one subreport in the same detail-section (side-by-side)
                     - support for property Stretch-type in bands, enabling fields to span on different pages
                     - conditional constants for DBMS_SQL-types, so that the package compiles in 10G
           Bugfixes: - Bug introduced in 0.8.0.0 with group-headers starting on new page fixed
                     - VARCHAR2-results larger than 4000 (coming from clob) now works up to 32K
                     - Offset above crosstabs was too large
                     - height of grouped column-headers in tables was caclulated wrong under certain conditions

  $version 1.0.0.0.beta 11.11.2012 Weiden

           Features: - Text-rotation for "standard text" (not for html-markup)
                     - new report-properties for Title on new page and Summary on new page
                     - beta-support for barchart, 3D-barchart and piechart
           Enhance : - code restructured and divided into smaller packages
                     - Performance-Enhancement via DBMS_PROFILER
           Bugfixes: - empty line before first line of HTML-text removed

  $version 1.0.0.1.beta 05.12.2012 Weiden

           Bugfixes: - Corrected wrong framesize for a text with a pagebreak and paddings

  $version 1.0.0.2.beta 06.12.2012 Weiden

           Bugfixes: - Corrected wrong index in FK_GET_QUERY_RESULT if there occurs a pagebreak in
                     - the first record and a field of the record is used in the footer

  $version 1.0.1.3.beta 10.12.2012 Weiden

           Enhance:  - support for category-linechart
           Bugfixes: - Corrected bug in Rendering of HTML with setting "Stretch with overflow"

  $version 1.0.2.3.beta 14.12.2012 Weiden

           Enhance:  - support for property text-align in html-markup

  $version 1.0.2.4.beta 17.12.2012 Weiden

           Bugfix:  - Corrected wrong framesize for a text with a pagebreak and paddings

  $version 1.0.3.4.beta 17.12.2012 Weiden

           Adjusted usage of subdatasets according to new type-structure

  $version 1.0.3.4 28.12.2012 Weiden

           release version

  $version 1.1.0.0 05.01.2013 Weiden

           Features: - run multiple reports into one pdf
                       + possibility of different page-formats
                       + page numbering either continous or starting with 1
                         for each report
           Enhance : - support for stacked barcharts (2D and 3D)
                     - Title, Author, subject and Keywords which are accessible in the 
                       properties of the generated pdf, can be set when calling the report by passing
                       parameters $$TITLE$$, $$AUTHOR$$, $$SUBJECT$$, $$KEYWORDS$$
           Bugfixes: - corrected NLS-settings for numbers and dates in charts

  $version 1.2.0.1 05.02.2013 Weiden
           
           General:  - Fixed version-number-mismatch
           
           Bugfixes: - Subreports where not allowed in Pageheaders
                     - Locale-settings where not transfered to Table-Subreport
                     - Changes in AS_PDF3_MOD fixing error "invalid RAW-length"
                     
  $version 1.2.0.2 15.02.2013 Weiden
           
           Bugfixes: - Frames around table-cells did not stretch
                     - Mode "Transparent" for rectangles did not work properly

  $version 1.2.1.2 26.02.2013 Weiden
           
           Enhance:  - Detection and usage of currency symbol in number-formats

  $version 1.2.1.3 09.03.2013 Weiden
           
           Bugfix:  - Parameter resolving for cross-tabs was missing

  $version 1.2.2.3 11.03.2013 Weiden
           Enhance: Added support for HeaderCells for crosstabs

  $version 1.2.3.3 06.04.2013 Weiden
           Enhance: - Various performance-changes in AS_PDF3_MOD
                    - Handling of Adler32-checksum for static images
                    - new system parameters 
                      + $$EXTERNAL_FONT_ARIAL$$
                      + $$EXTERNAL_FONT_TIMES$$
                      + $$EXTERNAL_FONT_COURIER$$
                      
                      to allow the usage of custom fonts for standard font-names.
*/

  TYPE tParameter IS RECORD (
    vcName  VARCHAR2(255),
    vcValue VARCHAR2(32767)
  );

  TYPE tParamList IS TABLE OF tParameter INDEX BY BINARY_INTEGER;

/**

  $name    FK_RUN

  $created 22.05.2012

  $author  Andreas Weiden

  $desc    Function to create a pdf from the report-definition with the given
           name. A report-definition with that name has to exist in JRXML_REPORT_DEFINITIONS

  $param   i_vcName    Name of the report, must match a record in JRXML_REPORT_DEFINITIONS

  $param   i_lParams   List of parameters and values to pass to the report

  $return  a blob containing the pdf-result

  $version 1.0.0.0 22.05.2012 Weiden
           initial version

*/
  FUNCTION FK_RUN(i_vcName  IN VARCHAR2,
                  i_lParams IN tParamList)
  RETURN BLOB;

/**

  $name    FK_RUN

  $created 22.05.2012

  $author  Andreas Weiden

  $desc    Function to create a pdf from the report-definition with the given
           name. A report-definition with that name has to exist in JRXML_REPORT_DEFINITIONS

  $param   i_vcName    Name of the report, must match a record in JRXML_REPORT_DEFINITIONS

  $return  a blob containing the pdf-result

  $version 1.0.0.0 22.05.2012 Weiden
           initial version

*/
  FUNCTION FK_RUN(i_vcName  IN VARCHAR2)
  RETURN BLOB;

/**

  $name    FK_RUN

  $created 05.01.2013

  $author  Andreas Weiden

  $desc    Function to create one pdf from all the report-definitions given in i_lReports
           name. A report-definition with that name has to exist in JRXML_REPORT_DEFINITIONS for each entry in 
           i_lReports.
           
           The reports can have different page-formats 

  $param   i_lReports         Names of the report, each must match a record in JRXML_REPORT_DEFINITIONS

  $param   i_lParams          List of parameters and values to pass to the report

  $param   i_bResetPageNumber Flag, if the page-numbers should start with 1 for each report
  
  $return  a blob containing the pdf-result

  $version 1.0.0.0 05.01.2013 Weiden
           initial version

*/
  FUNCTION FK_RUN(i_lReports         IN PK_JRXML2PDF_TYPES.tReportNameList,
                  i_lParams          IN tParamList,
                  i_bResetPageNumber IN BOOLEAN)
  RETURN BLOB;

/**

  $name    PR_RUN_TO_FILE

  $created 22.05.2012

  $author  Andreas Weiden

  $desc    Function to create a pdf from the report-definition with the given
           name. A report-definition with that name has to exist in JRXML_REPORT_DEFINITIONS

  $param   i_vcName     Name of the report, must match a record in JRXML_REPORT_DEFINITIONS

  $param   i_lParams    List of parameters and values to pass to the report

  $return  i_vcDir      name of the database-directory to write the record to
                        must be created using CREATE DIRECTORY

  $return  i_vcFilename name of the result-file containing the pdf-result

  $version 1.0.0.0 22.05.2012 Weiden
           initial version

*/
  PROCEDURE PR_RUN_TO_FILE(i_vcName     IN VARCHAR2,
                           i_lParams    IN tParamList,
                           i_vcDir      IN VARCHAR2,
                           i_vcFilename IN VARCHAR2);

/**

  $name    PR_RUN_TO_FILE

  $created 05.01.2013

  $author  Andreas Weiden

  $desc    Function to create a pdf from all the report-definitions given in i_lReports. 
           A report-definition with that name has to exist in JRXML_REPORT_DEFINITIONS for each

  $param   i_lReports         Names of the report, each must match a record in JRXML_REPORT_DEFINITIONS

  $param   i_lParams          List of parameters and values to pass to the report

  $return  i_vcDir            name of the database-directory to write the record to
                              must be created using CREATE DIRECTORY

  $return  i_vcFilename       name of the result-file containing the pdf-result

  $param   i_bResetPageNumber Flag, if the page-numbers should start with 1 for each report

  $version 1.0.0.0 05.01.2013 Weiden
           initial version

*/
  PROCEDURE PR_RUN_TO_FILE(i_lReports         IN PK_JRXML2PDF_TYPES.tReportNameList,
                           i_lParams          IN tParamList,
                           i_vcDir            IN VARCHAR2,
                           i_vcFilename       IN VARCHAR2,
                           i_bResetPageNumber IN BOOLEAN);

/**

  $name    PR_SHOW_REPORT

  $created 22.05.2012

  $author  Andreas Weiden

  $desc    Helper-function to output the file in APEX using OWA-methods

  $param   io_blReport BLOB to output

  $param   i_vcContentDisposition content-disposition, may be "inline" or "attachment"

  $param   i_vcFilename filename to be used to save the result locally

  $version 1.0.0.0 22.05.2012 Weiden
           initial version
  $version 1.0.1.0 30.05.2012 Weiden
           Enhance: Add Parameter i_vcContentDisposition
  $version 1.0.2.0 03.09.2012 Weiden
           Enhance: Add Parameter i_vcFilename

*/
  PROCEDURE PR_SHOW_REPORT(io_blReport            IN OUT BLOB,
                           i_vcContentDisposition IN VARCHAR2 DEFAULT 'inline',
                           i_vcFilename           IN VARCHAR2 DEFAULT 'reportresult.pdf');

END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_REPGEN IS

  VERSION_NUMBER        CONSTANT VARCHAR2(20):='1.2.3.3';

  -- Forwards
  FUNCTION FK_RENDER_SUBREPORT(i_nX                IN NUMBER,
                               i_nY                IN NUMBER,
                               i_nWidth            IN NUMBER,
                               i_nHeight           IN NUMBER,
                               i_vcStyle           IN PK_JRXML2PDF_TYPES.tStyleName,
                               i_rArea             IN PK_JRXML2PDF_TYPES.tArea,
                               i_vcReportName      IN PK_JRXML2PDF_TYPES.tName,
                               i_lParamList        IN PK_JRXML2PDF_TYPES.tParamList,
                               i_nMasterRecord     IN NUMBER
                               )
  RETURN PK_JRXML2PDF_TYPES.tArea;

  FUNCTION FK_EVALUATE_EXPRESSION(i_nStackPos      IN NUMBER,
                                  i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                  i_vcPattern      IN PK_JRXML2PDF_TYPES.tPattern,
                                  i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN VARCHAR2;

  FUNCTION FK_EVALUATE_IMAGE(i_nStackPos      IN NUMBER,
                             i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression)
  RETURN BLOB;

  FUNCTION FK_EVALUATE_CONDITION(i_nStackPos      IN NUMBER,
                                 i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                 i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN BOOLEAN;

  PROCEDURE PR_FINISH_PAGE_AND_START_NEW(io_rArea                 IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                                         i_nStackPos              IN NUMBER,
                                         i_bInnerToBottom         IN BOOLEAN    DEFAULT FALSE,
                                         i_bBeforeSubReportHdr    IN BOOLEAN    DEFAULT FALSE,
                                         i_bBeforeSubReportColHdr IN BOOLEAN    DEFAULT FALSE,
                                         i_bResetPagenumber       IN BOOLEAN    DEFAULT FALSE,
                                         i_bOneRecordBack         IN BOOLEAN    DEFAULT FALSE,
                                         i_vcStyleToApply         IN PK_JRXML2PDF_TYPES.tStyleName DEFAULT NULL,
                                         i_bRenderGroups          IN BOOLEAN    DEFAULT TRUE,
                                         i_bPageHeaders           IN BOOLEAN    DEFAULT TRUE
                                         );

  PROCEDURE PR_RENDER_GROUP_HEADERS(i_nStackPos     IN NUMBER,
                                    io_rArea        IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                                    i_bReprintOnly  IN BOOLEAN DEFAULT FALSE,
                                    i_bIsOnNewPage  IN BOOLEAN DEFAULT FALSE);

  FUNCTION FK_RENDER_TEXT(i_nX               IN NUMBER,
                          i_nY               IN NUMBER,
                          i_nWidth           IN NUMBER,
                          i_nHeight          IN NUMBER,
                          i_vcText           IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                          i_vcFont           IN PK_JRXML2PDF_TYPES.tFont,
                          i_nFontSize        IN NUMBER,
                          i_vcFontStyle      IN PK_JRXML2PDF_TYPES.tFontStyle,
                          i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcVerticalAlign  IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcBgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcFgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_nBoxTop          IN NUMBER,
                          i_nBoxLeft         IN NUMBER,
                          i_nBoxBottom       IN NUMBER,
                          i_nBoxRight        IN NUMBER,
                          i_nTopPadding      IN NUMBER,
                          i_nLeftPadding     IN NUMBER,
                          i_nBottomPadding   IN NUMBER,
                          i_nRightPadding    IN NUMBER,
                          i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                          i_nLineSpacing     IN NUMBER,
                          i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo,
                          i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle,
                          i_bMayPageBreak    IN BOOLEAN DEFAULT FALSE,
                          i_rArea            IN PK_JRXML2PDF_TYPES.tArea DEFAULT NULL,
                          i_vcRotation       IN PK_JRXML2PDF_TYPES.tAttribute DEFAULT NULL)
  RETURN PK_JRXML2PDF_TYPES.tArea;

  PROCEDURE PR_PUSH_SUBREPORT_DATA(io_rReport IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport);

  FUNCTION FK_FETCH_ROW(i_nStackPos IN NUMBER)
  RETURN BOOLEAN;

  PROCEDURE PR_POP_SUBREPORT_DATA;

  FUNCTION FK_RENDER_BAND(i_nStackPos           IN NUMBER,
                          i_rBand               IN PK_JRXML2PDF_TYPES.tBand,
                          i_rArea               IN PK_JRXML2PDF_TYPES.tArea,
                          i_bAllowSubReports    IN BOOLEAN,
                          i_nRenderFrame        IN NUMBER,
                          i_vcContentAdjustment IN PK_JRXML2PDF_TYPES.tAlignment DEFAULT PK_JRXML2PDF_TYPES.NONE_ALIGN,
                          i_nXFactor            IN NUMBER DEFAULT 1,
                          i_nYFactor            IN NUMBER DEFAULT 1,
                          i_nNeededHeight       IN NUMBER DEFAULT NULL
                         )
  RETURN PK_JRXML2PDF_TYPES.tArea;

  FUNCTION FK_FITS_IN_PAGE(i_nCurrentY           IN NUMBER,
                           i_nDetailHeight       IN NUMBER)
  RETURN BOOLEAN;

  lTokens             PK_JRXML2PDF_TYPES.tTokenList;
  rPageSetup          PK_JRXML2PDF_TYPES.tPageSetup;

  lReportStack        PK_JRXML2PDF_TYPES.tReportList;
  lDatePattern        PK_JRXML2PDF_TYPES.tPatternList;
  lHtmlTokens         PK_JRXML2PDF_TYPES.tHtmlTokenList;
  lHtmlReplacements   PK_JRXML2PDF_TYPES.tHtmlReplacementList;
  rMasterHtmlSettings PK_JRXML2PDF_TYPES.tHtmlSettings;
  rLastStyle          PK_JRXML2PDF_TYPES.tSimpleStyle;
  rLastStyleName      PK_JRXML2PDF_TYPES.tStyleName:=PK_JRXML2PDF_TYPES.NIL;

  blPDF               BLOB;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_TOKEN(i_vcStartTag IN PK_JRXML2PDF_TYPES.tAttribute,
                         i_vcEndTag   IN PK_JRXML2PDF_TYPES.tAttribute DEFAULT NULL) IS
    iPos PLS_INTEGER:=lTokens.COUNT+1;
  BEGIN
    lTokens(iPos).vcStartTag:=i_vcStartTag;
    lTokens(iPos).vcEndTag  :=i_vcEndTag;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_HTML_REPLACEMENT (i_vcHtmlTag IN PK_JRXML2PDF_TYPES.tAttribute,
                                     i_vcPdfChar IN PK_JRXML2PDF_TYPES.tAttribute) IS
  BEGIN
    lHtmlReplacements(i_vcHtmlTag):=i_vcPdfChar;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_PUSH_PAGE_FRAME_MARKER(i_nStackPos    IN NUMBER,
                                      i_nBorderTop   IN NUMBER,
                                      i_nBorderWidth IN NUMBER,
                                      i_rStyle       IN PK_JRXML2PDF_TYPES.tSimpleStyle) IS
    rPageFrameMarker PK_JRXML2PDF_TYPES.tPageFrameMarker;
  BEGIN
    rPageFrameMarker.nBorderTop:=i_nBorderTop;
    rPageFrameMarker.nBorderWidth:=i_nBorderWidth;
    rPageFrameMarker.rStyle:=i_rStyle;
    lReportStack(i_nStackpos).lPageFrameMarkers(lReportStack(i_nStackpos).lPageFrameMarkers.COUNT+1):=rPageFrameMarker;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_POP_PAGE_FRAME_MARKER(i_nStackPos    IN NUMBER) IS
  BEGIN
    IF lReportStack(i_nStackpos).lPageFrameMarkers.COUNT>0 THEN
      lReportStack(i_nStackpos).lPageFrameMarkers.DELETE(lReportStack(i_nStackpos).lPageFrameMarkers.COUNT);
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_STORE_PAGE_POSITION(i_rArea IN PK_JRXML2PDF_TYPES.tArea) IS
    rArea PK_JRXML2PDF_TYPES.tArea:=i_rArea;
  BEGIN
    FOR i IN 1..lReportStack.COUNT LOOP
      -- store the Pagepointer
      rArea.nPage:=lReportStack(i).nCurrentPagePointer;
      -- position is stored in last report on stack
      lReportStack(i).lStoredPositions(lReportStack(i).lStoredPositions.COUNT+1):=rArea;
    END LOOP;

  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GOTO_LAST_STORED_POSITION
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    rArea PK_JRXML2PDF_TYPES.tArea;
  BEGIN
    FOR i IN 1..lReportStack.COUNT LOOP
      -- position is stored in last report on stack
      rArea:=lReportStack(i).lStoredPositions(lReportStack(i).lStoredPositions.COUNT);
      lReportStack(i).nCurrentPagePointer:=rArea.nPage;
    END LOOP;

    IF lReportStack(1).lPageNumbers.COUNT=lReportStack(1).nCurrentPagePointer THEN
      AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
    ELSE
      AS_PDF3_MOD.PR_GOTO_PAGE(lReportStack(1).lPageNumbers(lReportStack(1).nCurrentPagePointer));
    END IF;
    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_REMOVE_LAST_STORED_POSITION IS
    rArea PK_JRXML2PDF_TYPES.tArea;
  BEGIN
    FOR i IN 1..lReportStack.COUNT LOOP
      -- position is stored in last report on stack
      rArea:=lReportStack(i).lStoredPositions(lReportStack(i).lStoredPositions.COUNT);
      lReportStack(i).lStoredPositions.DELETE(lReportStack(i).lStoredPositions.COUNT);
    END LOOP;
    IF lReportStack(1).lStoredPositions.COUNT=0 THEN
      AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_GOTO_STORED_POSITION(i_rArea IN PK_JRXML2PDF_TYPES.tArea) IS
  BEGIN
    lReportStack(1).nCurrentPagePointer:=i_rArea.nPage;
    IF lReportStack(1).nCurrentPagePointer=lReportStack(1).lPageNumbers.COUNT THEN
      AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
      -- Reset current page pointers for all subreports
      FOR i IN 2..lReportStack.COUNT LOOP
        lReportStack(i).nCurrentPagePointer:=lReportStack(i).lPageNumbers(lReportStack(i).lPageNumbers.COUNT);
      END LOOP;
    ELSE
      AS_PDF3_MOD.PR_GOTO_PAGE(lReportStack(1).lPageNumbers(lReportStack(1).nCurrentPagePointer));
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_NUM_PATTERN(i_vcPattern IN PK_JRXML2PDF_TYPES.tPattern)
  RETURN VARCHAR2 IS
    -- official format
    CURRENCYPATTERN     CONSTANT VARCHAR2(4) :=UNISTR('\00A4');
    ISOCURRENCYPATTERN  CONSTANT VARCHAR2(8) :=UNISTR('\00A4\00A4');
    FULLCURRENCYPATTERN CONSTANT VARCHAR2(12):=UNISTR('\00A4\00A4\00A4');
    
    FUNCTION FK_PATTERN(i_vcPattern IN VARCHAR2)
    RETURN VARCHAR2 IS
      iPos     PLS_INTEGER:=INSTR(i_vcPattern, CURRENCYPATTERN);
      vcResult PK_JRXML2PDF_TYPES.tPattern;
    BEGIN
      vcResult:=TRANSLATE(i_vcPattern, '#0.,', '90DG');
      IF iPos>0 THEN
        vcResult:=REPLACE(vcResult, FULLCURRENCYPATTERN, 'C');
        vcResult:=REPLACE(vcResult, ISOCURRENCYPATTERN, 'C');
        vcResult:=REPLACE(vcResult, CURRENCYPATTERN, 'L');
        vcResult:=REPLACE(vcResult, ' ', '');
      END IF;
      RETURN vcResult;
    END;
      
  BEGIN
    IF INSTR(i_vcPattern, ';')>0 THEN
      RETURN FK_PATTERN(SUBSTR(i_vcPattern, 1,INSTR(i_vcPattern, ';')-1));
    ELSE
      RETURN FK_PATTERN(i_vcPattern);
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LOAD_REPORT (i_vcReportName IN VARCHAR2,
                           i_lParamList   IN PK_JRXML2PDF_TYPES.tParamList)
  RETURN PK_JRXML2PDF_TYPES.tReport IS
    rReport PK_JRXML2PDF_TYPES.tReport;
  BEGIN
    rReport:=PK_JRXML2PDF_LOADER.FK_LOAD_REPORT(i_vcReportName =>i_vcReportName,
                                                i_lParamList   =>i_lParamList);
    RETURN rReport;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_STYLE(i_nStackPos IN NUMBER,
                        i_vcStyle   IN PK_JRXML2PDF_TYPES.tStyleName)
  RETURn PK_JRXML2PDF_TYPES.tSimpleStyle IS
    rSimpleStyle PK_JRXML2PDF_TYPES.tSimpleStyle;
    rCondStyle   PK_JRXML2PDF_TYPES.tSimpleStyle;
    iPos         PLS_INTEGER;
    bOk          BOOLEAN;
  BEGIN
    IF i_vcStyle IS NOT NULL THEN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Search Style ' || i_vcStyle);
      END IF;
      IF rLastStyleName=i_vcStyle THEN
        rSimpleStyle:=rLastStyle;
        bOk:=TRUE;
      ELSE
        BEGIN
          rSimpleStyle:=lReportStack(i_nStackPos).lStyles(i_vcStyle).rStyle;
          rLastStyleName:=i_vcStyle;
          rLastStyle:=rSimpleStyle;
          bOk:=TRUE;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            NULL;
        END;
      END IF;
    END IF;
    IF bOk THEN

      -- Apply conditional styles
      iPos:=lReportStack(i_nStackPos).lStyles(i_vcStyle).lConditionalStyles.FIRST;
      WHILE iPos IS NOT NULL LOOP
        -- evaluate expression
        IF FK_EVALUATE_CONDITION(i_nStackPos,
                                 lReportStack(i_nStackPos).lStyles(i_vcStyle).lConditionalStyles(iPos).vcWhenExpression) THEN
          rCondStyle:=lReportStack(i_nStackPos).lStyles(i_vcStyle).lConditionalStyles(iPos);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Apply conditional style:'||lReportStack(i_nStackPos).lStyles(i_vcStyle).lConditionalStyles(iPos).vcWhenExpression);
          END IF;

          -- apply conditional attributes
          rSimpleStyle.vcFGColor        :=NVL(rCondStyle.vcFGColor       , rSimpleStyle.vcFGColor);
          rSimpleStyle.vcBGColor        :=NVL(rCondStyle.vcBGColor       , rSimpleStyle.vcBGColor);
          rSimpleStyle.vcFont           :=NVL(rCondStyle.vcFont          , rSimpleStyle.vcFont);
          rSimpleStyle.vcFontStyle      :=NVL(rCondStyle.vcFontStyle     , rSimpleStyle.vcFontStyle);
          rSimpleStyle.vcAlignment      :=NVL(rCondStyle.vcAlignment     , rSimpleStyle.vcAlignment);
          rSimpleStyle.nFontSize        :=NVL(rCondStyle.nFontSize       , rSimpleStyle.nFontSize);
          rSimpleStyle.nLineWidth       :=NVL(rCondStyle.nLineWidth      , rSimpleStyle.nLineWidth);
          rSimpleStyle.nBoxTop          :=NVL(rCondStyle.nBoxTop         , rSimpleStyle.nBoxTop);
          rSimpleStyle.nBoxLeft         :=NVL(rCondStyle.nBoxLeft        , rSimpleStyle.nBoxLeft);
          rSimpleStyle.nBoxBottom       :=NVL(rCondStyle.nBoxBottom      , rSimpleStyle.nBoxBottom);
          rSimpleStyle.nBoxRight        :=NVL(rCondStyle.nBoxRight       , rSimpleStyle.nBoxRight);
          rSimpleStyle.vcBoxTopColor    :=NVL(rCondStyle.vcBoxTopColor   , rSimpleStyle.vcBoxTopColor);
          rSimpleStyle.vcBoxLeftColor   :=NVL(rCondStyle.vcBoxLeftColor  , rSimpleStyle.vcBoxLeftColor);
          rSimpleStyle.vcBoxBottomColor :=NVL(rCondStyle.vcBoxBottomColor, rSimpleStyle.vcBoxBottomColor);
          rSimpleStyle.vcBoxRightColor  :=NVL(rCondStyle.vcBoxRightColor , rSimpleStyle.vcBoxRightColor);
          rSimpleStyle.vcOpaque         :=NVL(rCondStyle.vcOpaque        , rSimpleStyle.vcOpaque);
          iPos:=NULL;
        ELSE
          iPos:=lReportStack(i_nStackPos).lStyles(i_vcStyle).lConditionalStyles.NEXT(iPos);
        END IF;
      END LOOP;

      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Found');
      END IF;

      RETURN rSimpleStyle;
    ELSE
      RETURN NULL;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_MAKE_STYLE_FROM_ATTRIBS(i_vcFont           IN PK_JRXML2PDF_TYPES.tFont      DEFAULT NULL,
                                      i_nFontSize        IN NUMBER     DEFAULT NULL,
                                      i_nLineWidth       IN NUMBER     DEFAULT NULL,
                                      i_vcFontStyle      IN VARCHAR2   DEFAULT NULL,
                                      i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment DEFAULT NULL,
                                      i_vcVerticalAlign  IN PK_JRXML2PDF_TYPES.tAlignment DEFAULT NULL,
                                      i_vcBgColor        IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_vcFgColor        IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_nBoxTop          IN NUMBER     DEFAULT NULL,
                                      i_nBoxLeft         IN NUMBER     DEFAULT NULL,
                                      i_nBoxBottom       IN NUMBER     DEFAULT NULL,
                                      i_nBoxRight        IN NUMBER     DEFAULT NULL,
                                      i_nTopPadding      IN NUMBER     DEFAULT NULL,
                                      i_nLeftPadding     IN NUMBER     DEFAULT NULL,
                                      i_nBottomPadding   IN NUMBER     DEFAULT NULL,
                                      i_nRightPadding    IN NUMBER     DEFAULT NULL,
                                      i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor     DEFAULT NULL,
                                      i_nLineSpacing     IN NUMBER     DEFAULT NULL,
                                      i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo     DEFAULT NULL,
                                      i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle)
  RETURN PK_JRXML2PDF_TYPES.tSimpleStyle IS
    rStyle PK_JRXML2PDF_TYPES.tSimpleStyle;
  BEGIN
    rStyle.vcFGColor        :=NVL(i_vcFGColor       , i_rStyle.vcFGColor);
    rStyle.vcBGColor        :=NVL(i_vcBGColor       , i_rStyle.vcBGColor);
    rStyle.vcFont           :=NVL(i_vcFont          , i_rStyle.vcFont);
    rStyle.vcFontStyle      :=NVL(i_vcFontStyle     , i_rStyle.vcFontStyle);
    rStyle.vcAlignment      :=NVL(i_vcAlignment     , i_rStyle.vcAlignment);
    rStyle.nFontSize        :=NVL(i_nFontSize       , i_rStyle.nFontSize);
    rStyle.nLineWidth       :=NVL(i_nLineWidth      , i_rStyle.nLineWidth);
    rStyle.nBoxTop          :=NVL(i_nBoxTop         , i_rStyle.nBoxTop);
    rStyle.nBoxLeft         :=NVL(i_nBoxLeft        , i_rStyle.nBoxLeft);
    rStyle.nBoxBottom       :=NVL(i_nBoxBottom      , i_rStyle.nBoxBottom);
    rStyle.nBoxRight        :=NVL(i_nBoxRight       , i_rStyle.nBoxRight);
    rStyle.vcBoxTopColor    :=NVL(i_vcBoxTopColor   , i_rStyle.vcBoxTopColor);
    rStyle.vcBoxLeftColor   :=NVL(i_vcBoxLeftColor  , i_rStyle.vcBoxLeftColor);
    rStyle.vcBoxBottomColor :=NVL(i_vcBoxBottomColor, i_rStyle.vcBoxBottomColor);
    rStyle.vcBoxRightColor  :=NVL(i_vcBoxRightColor , i_rStyle.vcBoxRightColor);
    rStyle.vcOpaque         :=NVL(i_vcOpaque        , i_rStyle.vcOpaque);
    RETURn rStyle;
  END;
  -- ---------------------------------------------------------------------------

  PROCEDURE PR_INIT_PDF(i_vcTitle    IN VARCHAR2,
                        i_vcAuthor   IN VARCHAR2,
                        i_vcSubject  IN VARCHAR2,
                        i_vcKeywords IN VARCHAR2) IS
  BEGIN
    AS_PDF3_MOD.INIT;
    IF    i_vcTitle IS NOT NULL
       OR i_vcAuthor IS NOT NULL
       OR i_vcSubject IS NOT NULL
       OR i_vcKeywords IS NOT NULL THEN
      AS_PDF3_MOD.set_info(p_title   =>i_vcTitle,
                           p_author  =>i_vcAuthor,
                           p_subject =>i_vcSubject,
                           p_keywords=>i_vcKeywords
                          );
   END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_FINISH_PDF IS
  BEGIN
    blPDF:=AS_PDF3_MOD.get_pdf;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SETUP_PAGE(i_nWidth        IN NUMBER,
                          i_nHeight       IN NUMBER,
                          i_nLeftMargin   IN NUMBER,
                          i_nRightMargin  IN NUMBER,
                          i_nTopMargin    IN NUMBER,
                          i_nBottomMargin IN NUMBER
                         ) IS
  BEGIN
    AS_PDF3_MOD.set_page_size(p_width  =>i_nWidth,
                              p_height =>i_nHeight,
                              p_unit   =>PK_JRXML2PDF_TYPES.PIXEL
                             );

    AS_PDF3_MOD.set_margins(p_top   =>i_nTopMargin,
                            p_left  =>i_nLeftMargin,
                            p_bottom=>i_nBottomMargin,
                            p_right =>i_nRightMargin,
                            p_unit   =>PK_JRXML2PDF_TYPES.PIXEL
                           );
    rPageSetup.nPageWidth   :=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_PAGE_WIDTH);
    rPageSetup.nPageHeight  :=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_PAGE_HEIGHT);
    rPageSetup.nLeftMargin  :=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_MARGIN_LEFT);
    rPageSetup.nRightMargin :=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_MARGIN_RIGHT);
    rPageSetup.nTopMargin   :=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_MARGIN_TOP);
    rPageSetup.nBottomMargin:=AS_PDF3_MOD.get(AS_PDF3_MOD.C_GET_MARGIN_BOTTOM);
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_NEW_PAGE(i_bResetPageNumber IN BOOLEAN) IS
  BEGIN
    IF NOT lReportStack(1).lStartOfPageDone.EXISTS(lReportStack(1).nCurrentPagePointer+1) THEN
      -- its a new page

      AS_PDF3_MOD.new_page;
      -- set pointer to current page
      AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('New Page for Report : ' || lReportStack.COUNT);
      END IF;

      FOR i IN 1..lReportStack.COUNT LOOP
        -- increase Stack of Pagenumber
        
        lReportStack(i).nCurrentPagePointer:=lReportStack(i).nCurrentPagePointer+1;
        lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer):=lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer-1)+1;
        IF     i=lReportStack.COUNT
           AND i_bResetPageNumber THEN
          lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer):=1;
        ELSE
          lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer):=lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer-1)+1;
        END IF;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Increase Page no for report : ' ||TO_CHAR(lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer)) || '/' || TO_CHAR(i));
        END IF;

      END LOOP;
    ELSE
      IF lReportStack(1).nCurrentPagePointer+1=lReportStack(1).lPageNumbers.COUNT THEN
        AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
      ELSE
        AS_PDF3_MOD.PR_GOTO_PAGE(lReportStack(1).lPageNumbers(lReportStack(1).nCurrentPagePointer+1));
      END IF;
      -- we are on a page were the already have been
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Return to Page for Report : ' || lReportStack.COUNT);
      END IF;

      FOR i IN 1..lReportStack.COUNT LOOP
        -- increase Stack of Pagenumber

        lReportStack(i).nCurrentPagePointer:=lReportStack(i).nCurrentPagePointer+1;
        IF NOT lReportStack(i).lStartOfPageDone.EXISTS(lReportStack(i).nCurrentPagePointer) THEN
          lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer):=lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer-1)+1;
          IF     i=lReportStack.COUNT
             AND i_bResetPageNumber THEN
            lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer):=1;
          ELSE
            lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer):=lReportStack(i).lLogicalPageNumbers(lReportStack(i).nCurrentPagePointer-1)+1;
          END IF;

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Increase Page no for report : ' ||TO_CHAR(lReportStack(i).lPageNumbers(lReportStack(i).nCurrentPagePointer)) || '/' || TO_CHAR(i));
          END IF;
        END IF;
      END LOOP;
    END IF;
  END;


  PROCEDURE PR_RENDER_LINE(i_nX          IN NUMBER,
                           i_nY          IN NUMBER,
                           i_nWidth      IN NUMBER,
                           i_nHeight     IN NUMBER,
                           i_nLineWidth  IN NUMBER,
                           i_vcLineColor IN PK_JRXML2PDF_TYPES.tColor,
                           i_rStyle      IN PK_JRXML2PDF_TYPES.tSimpleStyle
                          ) IS
  BEGIN
    IF i_nWidth=1 THEN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('vert Line x/y/Height/Linewidth' || TO_CHAR(i_nX) || '/' ||
                                                                             TO_CHAR(i_nY) || '/' ||
                                                                             TO_CHAR(i_nHeight) || '/' ||
                                                                             TO_CHAR(COALESCE(i_nLineWidth, i_rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN))
                                        );
      END IF;

      -- vertical line
      AS_PDF3_MOD.vertical_line(p_x          =>rPageSetup.nLeftMargin+i_nX,
                                p_y          =>rPageSetup.nPageHeight-i_nY-rPageSetup.nTopMargin,
                                p_height     =>-i_nHeight,
                                p_line_width =>COALESCE(i_nLineWidth, i_rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN),
                                p_line_color =>REPLACE(COALESCE(i_vcLineColor, i_rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK), '#', '')
                               );
    ELSIF i_nHeight=1 THEN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('horz Line x/y/Width/Linewidth'  || TO_CHAR(i_nX) || '/' ||
                                                                             TO_CHAR(i_nY) || '/' ||
                                                                             TO_CHAR(i_nWidth) || '/' ||
                                                                             TO_CHAR(COALESCE(i_nLineWidth, i_rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN))
                                        );
      END IF;
      -- horizontal line
      AS_PDF3_MOD.horizontal_line(p_x          =>rPageSetup.nLeftMargin+i_nX,
                                  p_y          =>rPageSetup.nPageHeight-rPageSetup.nTopMargin-i_nY,
                                  p_width      =>i_nWidth,
                                  p_line_width =>COALESCE(i_nLineWidth, i_rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN),
                                  p_line_color =>REPLACE(COALESCE(i_vcLineColor, i_rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK), '#', '')
                                 );
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_RECT(i_nX          IN NUMBER,
                           i_nY          IN NUMBER,
                           i_nWidth      IN NUMBER,
                           i_nHeight     IN NUMBER,
                           i_nLineWidth  IN NUMBER,
                           i_vcLineColor IN PK_JRXML2PDF_TYPES.tColor,
                           i_vcFillColor IN PK_JRXML2PDF_TYPES.tColor,
                           i_rStyle      IN PK_JRXML2PDF_TYPES.tSimpleStyle
                          ) IS
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Rect ' || i_nX || '-' || i_nY || '-' || i_nWidth || '-' || i_nHeight);
    END IF;


    -- vertical line
    AS_PDF3_MOD.rect(p_x          =>rPageSetup.nLeftMargin+i_nX,
                     p_y          =>rPageSetup.nPageHeight-rPageSetup.nTopMargin-i_nY,
                     p_width      =>i_nWidth,
                     p_height     =>-i_nHeight,
                     p_line_color =>REPLACE(COALESCE(i_vcLineColor, i_rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK), '#', ''),
                     p_fill_color =>REPLACE(COALESCE(i_vcFillColor, i_rStyle.vcBgColor, PK_JRXML2PDF_TYPES.WHITE), '#', ''),
                     p_line_width =>COALESCE(i_nLineWidth, i_rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN)
                    );
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_IMAGE(i_nX          IN NUMBER,
                            i_nY          IN NUMBER,
                            i_nWidth      IN NUMBER,
                            i_nHeight     IN NUMBER,
                            i_blImage     IN BLOB,
                            i_vcAdler32   IN PK_JRXML2PDF_TYPEs.tMaxVarchar2
                          ) IS
    blTemp  BLOB;
    rDummy PK_JRXML2PDF_TYPES.tArea;
  BEGIN
    -- Evaluate Expression of image
    IF i_blImage IS NOT NULL THEN
      DBMS_LOB.CREATETEMPORARY(blTemp, true);
      DBMS_LOB.COPY(dest_lob=>blTemp,
                    src_lob =>i_blImage,
                    amount  =>dbms_lob.getlength(i_blImage)
                   );
      AS_PDF3_MOD.put_image(p_img        =>blTemp,
                            p_x          =>rPageSetup.nLeftMargin+i_nX,
                            p_y          =>rPageSetup.nPageHeight-rPageSetup.nTopMargin-i_nY-i_nHeight,
                            p_width      =>i_nWidth,
                            p_height     =>i_nHeight,
                            p_align      =>PK_JRXML2PDF_TYPES.LEFT_ALIGN,
                            p_adler32    =>i_vcAdler32
                           );
      DBMS_LOB.FREETEMPORARY(blTemp);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      -- render error, output error as text
      rDummy:=FK_RENDER_TEXT(i_nX              =>i_nX,
                             i_nY              =>i_nY,
                             i_nWidth          =>i_nWidth,
                             i_nHeight         =>i_nHeight,
                             i_vcText          =>SQLCODE || SQLERRM ,
                             i_vcFont          =>PK_JRXML2PDF_TYPES.ARIAL,
                             i_nFontSize       =>10,
                             i_vcFontStyle     =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                             i_vcAlignment     =>PK_JRXML2PDF_TYPES.LEFT_ALIGN,
                             i_vcVerticalAlign =>PK_JRXML2PDF_TYPES.TOP_ALIGN,
                             i_vcBgColor       =>PK_JRXML2PDF_TYPES.WHITE,
                             i_vcFgColor       =>PK_JRXML2PDF_TYPES.BLACK,
                             i_nBoxTop         =>NULL,
                             i_nBoxLeft        =>NULL,
                             i_nBoxBottom      =>NULL,
                             i_nBoxRight       =>NULL,
                             i_nTopPadding     =>NULL,
                             i_nLeftPadding    =>NULL,
                             i_nBottomPadding  =>NULL,
                             i_nRightPadding   =>NULL,
                             i_vcBoxTopColor   =>NULL,
                             i_vcBoxLeftColor  =>NULL,
                             i_vcBoxBottomColor=>NULL,
                             i_vcBoxRightColor =>NULL,
                             i_nLineSpacing    =>PK_JRXML2PDF_TYPES.MIN_LINE_SPACING,
                             i_vcOpaque        =>PK_JRXML2PDF_TYPES.NO,
                             i_rStyle          =>NULL
                            );
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_BOX(i_nX               IN NUMBER,
                          i_nY               IN NUMBER,
                          i_nWidth           IN NUMBER,
                          i_nHeight          IN NUMBER,
                          i_vcBgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcFgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_nBoxTop          IN VARCHAR2,
                          i_nBoxLeft         IN VARCHAR2,
                          i_nBoxBottom       IN VARCHAR2,
                          i_nBoxRight        IN VARCHAR2,
                          i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo,
                          i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle) IS
    nStartOffset NUMBER;
    nEndOffset   NUMBER;
  BEGIN
    IF NVL(i_vcBgColor, i_rStyle.vcBgColor) IS NOT NULL
       AND COALESCE(i_vcOpaque, i_rStyle.vcOpaque, PK_JRXML2PDF_TYPES.NO)=PK_JRXML2PDF_TYPES.YES THEN
      -- Render rectangle
      PR_RENDER_RECT(i_nX         =>i_nX+0.5,
                     i_nY         =>i_nY+0.5,
                     i_nWidth     =>i_nWidth-1,
                     i_nHeight    =>i_nHeight-1,
                     i_nLineWidth =>NULL,
                     i_vcLineColor=>NVL(i_vcBgColor, i_rStyle.vcBgColor),
                     i_vcFillColor=>NVL(i_vcBgColor, i_rStyle.vcBgColor),
                     i_rStyle     =>i_rStyle
                    );
    END IF;
    IF NVL(i_nBoxTop, i_rStyle.nBoxTop) >0 THEN
      nStartOffset:=COALESCE(i_nBoxLeft, i_rStyle.nBoxLeft, 0)/2;
      nEndOffset  :=COALESCE(i_nBoxRight, i_rStyle.nBoxRight, 0)/2;
      -- horizontal line
      PR_RENDER_LINE(i_nX         =>i_nX-nStartOffset,
                     i_nY         =>i_nY,
                     i_nWidth     =>i_nWidth+nStartOffset+nEndOffset,
                     i_nHeight    =>1,
                     i_nLineWidth =>NVL(i_nBoxTop, i_rStyle.nBoxTop),
                     i_vcLineColor=>NVL(i_vcBoxTopColor, i_rStyle.vcBoxTopColor),
                     i_rStyle     =>i_rStyle
                    );
    END IF;
    IF NVL(i_nBoxLeft, i_rStyle.nBoxLeft)>0 THEN
      nStartOffset:=COALESCE(i_nBoxTop, i_rStyle.nBoxTop,0)/2;
      nEndOffset  :=COALESCE(i_nBoxBottom, i_rStyle.nBoxBottom,0)/2;
      -- vertical line
      PR_RENDER_LINE(i_nX         =>i_nX,
                     i_nY         =>i_nY-nStartOffset,
                     i_nWidth     =>1,
                     i_nHeight    =>i_nHeight+nStartOffset+nEndOffset,
                     i_nLineWidth =>NVL(i_nBoxLeft, i_rStyle.nBoxLeft),
                     i_vcLineColor=>NVL(i_vcBoxLeftColor, i_rStyle.vcBoxLeftColor),
                     i_rStyle     =>i_rStyle
                    );
    END IF;
    IF NVL(i_nBoxBottom, i_rStyle.nBoxBottom)>0 THEN
      nStartOffset:=COALESCE(i_nBoxLeft, i_rStyle.nBoxLeft, 0)/2;
      nEndOffset  :=COALESCE(i_nBoxRight, i_rStyle.nBoxRight, 0)/2;
      -- horizontal line
      PR_RENDER_LINE(i_nX         =>i_nX-nStartOffset,
                     i_nY         =>i_nY+i_nHeight,
                     i_nWidth     =>i_nWidth+nStartOffset+nEndOffset,
                     i_nHeight    =>1,
                     i_nLineWidth =>NVL(i_nBoxBottom, i_rStyle.nBoxBottom),
                     i_vcLineColor=>NVL(i_vcBoxBottomColor, i_rStyle.vcBoxBottomColor),
                     i_rStyle     =>i_rStyle
                    );
    END IF;
    IF NVL(i_nBoxRight, i_rStyle.nBoxRight)>0 THEN
      nStartOffset:=COALESCE(i_nBoxTop, i_rStyle.nBoxTop, 0)/2;
      nEndOffset  :=COALESCE(i_nBoxBottom, i_rStyle.nBoxBottom, 0)/2;
      -- horizontal line
      PR_RENDER_LINE(i_nX         =>i_nX+ i_nWidth,
                     i_nY         =>i_nY-nStartOffset,
                     i_nWidth     =>1,
                     i_nHeight    =>i_nHeight+nStartOffset+nEndOffset,
                     i_nLineWidth =>NVL(i_nBoxRight, i_rStyle.nBoxRight),
                     i_vcLineColor=>NVL(i_vcBoxRightColor, i_rStyle.vcBoxRightColor),
                     i_rStyle     =>i_rStyle
                    );
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_APPLY_REPORT_STYLE(i_nStackPos      IN NUMBER,
                                  i_nX             IN NUMBER,
                                  i_nBottom        IN NUMBER
                                 ) IS
    rStyle PK_JRXML2PDF_TYPES.tSimpleStyle;
  BEGIN
    FOR i IN 1..lReportStack(i_nStackPos).lPageFrameMarkers.COUNT LOOP
      rStyle:=lReportStack(i_nStackPos).lPageFrameMarkers(i).rStyle;
      -- Render a transparent box around the whole report-area
      PR_RENDER_BOX(i_nX              =>i_nX,
                    i_nY              =>lReportStack(i_nStackPos).lPageFrameMarkers(i).nBorderTop,
                    i_nWidth          =>lReportStack(i_nStackPos).lPageFrameMarkers(i).nBorderWidth,
                    i_nHeight         =>i_nBottom-lReportStack(i_nStackPos).lPageFrameMarkers(i).nBorderTop,
                    i_vcBgColor       =>rStyle.vcBgColor,
                    i_vcFgColor       =>rStyle.vcFgColor,
                    i_nBoxTop         =>rStyle.nBoxTop,
                    i_nBoxLeft        =>rStyle.nBoxLeft,
                    i_nBoxBottom      =>rStyle.nBoxBottom,
                    i_nBoxRight       =>rStyle.nBoxRight,
                    i_vcBoxTopColor   =>rStyle.vcBoxTopColor,
                    i_vcBoxLeftColor  =>rStyle.vcBoxLeftColor,
                    i_vcBoxBottomColor=>rStyle.vcBoxBottomColor,
                    i_vcBoxRightColor =>rStyle.vcBoxRightColor,
                    i_vcOpaque        =>rStyle.vcOpaque,
                    i_rStyle          =>NULL
                   );
    END LOOP;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RENDER_TEXT(i_nX               IN NUMBER,
                          i_nY               IN NUMBER,
                          i_nWidth           IN NUMBER,
                          i_nHeight          IN NUMBER,
                          i_vcText           IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                          i_vcFont           IN PK_JRXML2PDF_TYPES.tFont,
                          i_nFontSize        IN NUMBER,
                          i_vcFontStyle      IN PK_JRXML2PDF_TYPES.tFontStyle,
                          i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcVerticalAlign  IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcBgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcFgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_nBoxTop          IN NUMBER,
                          i_nBoxLeft         IN NUMBER,
                          i_nBoxBottom       IN NUMBER,
                          i_nBoxRight        IN NUMBER,
                          i_nTopPadding      IN NUMBER,
                          i_nLeftPadding     IN NUMBER,
                          i_nBottomPadding   IN NUMBER,
                          i_nRightPadding    IN NUMBER,
                          i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                          i_nLineSpacing     IN NUMBER,
                          i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo,
                          i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle,
                          i_bMayPageBreak    IN BOOLEAN DEFAULT FALSE,
                          i_rArea            IN PK_JRXML2PDF_TYPES.tArea DEFAULT NULL,
                          i_vcRotation       IN PK_JRXML2PDF_TYPES.tAttribute DEFAULT NULL)
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    rArea     PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    nHeight   NUMBER;
    nWidth    NUMBER;
    nRotation NUMBER;

    PROCEDURE PR_SET_FONT_AND_COLOR IS
    BEGIN
      -- set font
      PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont   =>NVL(i_vcFont, i_rStyle.vcFont),
                                    i_vcStyle  =>COALESCE(i_vcFontStyle, i_rStyle.vcFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL),
                                    i_nSize    =>COALESCE(i_nFontSize, i_rStyle.nFontSize, 10)
                                   );

      AS_PDF3_MOD.set_color(NVL(REPLACE(COALESCE(i_vcFgColor, i_rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK),'#',''), PK_JRXML2PDF_TYPES.BLACK));
    END;

    PROCEDURE PR_RENDER_BOX_FOR_TEXT(i_nY      IN NUMBER,
                                     i_nHeight IN NUMBER) IS
    BEGIN

      PR_RENDER_BOX(i_nX               =>i_nX,
                    i_nY               =>i_nY,
                    i_nWidth           =>i_nWidth,
                    i_nHeight          =>i_nHeight,
                    i_vcBgColor        =>i_vcBgColor,
                    i_vcFgColor        =>i_vcFgColor,
                    i_nBoxTop          =>i_nBoxTop,
                    i_nBoxLeft         =>i_nBoxLeft,
                    i_nBoxBottom       =>i_nBoxBottom,
                    i_nBoxRight        =>i_nBoxRight,
                    i_vcBoxTopColor    =>i_vcBoxTopColor,
                    i_vcBoxLeftColor   =>i_vcBoxLeftColor,
                    i_vcBoxBottomColor =>i_vcBoxBottomColor,
                    i_vcBoxRightColor  =>i_vcBoxRightColor,
                    i_vcOpaque         =>i_vcOpaque,
                    i_rStyle           =>i_rStyle);
    END;


    PROCEDURE PR_WRITE_TEXT_TO_BOX(i_nMaxHeight      IN NUMBER,
                                   i_nLineSpacing    IN NUMBER,
                                   i_nMaxWidth       IN NUMBER,
                                   i_vcAlignment     IN PK_JRXML2PDF_TYPES.tAlignment,
                                   i_vcVerticalAlign IN PK_JRXML2PDF_TYPES.tAlignment
                                  ) IS
      lText          PK_JRXML2PDF_TYPES.tTextPieceList;
      nX             NUMBER:=i_nX;
      nxStart        NUMBER;
      nY             NUMBER;
      nCurrentHeight NUMBER:=0;
      nGroup         NUMBER:=0;
      nLineSpacing   NUMBER;
      vcAlignment    PK_JRXML2PDF_TYPES.tAlignment:=LOWER(i_vcAlignment);
      nHeight        NUMBER:=0;
      nBaseY         NUMBER;
      bTrackY        BOOLEAN:=FALSE;
      bFirstPage     BOOLEAN:=TRUE;
      nStopHeight    NUMBER;
      nStartY        NUMBER;

      FUNCTION FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  IN NUMBER,
                                            i_nCurrentY IN NUMBER,
                                            i_nTopY     IN NUMBER)
      RETURN NUMBER IS
        nHeight  NUMBER:=0;
        i        PLS_INTEGER:=i_nTextPos;
        bNoFit   BOOLEAN:=FALSE;
      BEGIN
        IF i_bMayPageBreak THEN
          LOOP
            EXIT WHEN i>lText.COUNT;
            IF NOT FK_FITS_IN_PAGE(i_nCurrentY, nHeight+
                                                lText(i).nHeight+
                                                NVL(i_nTopPadding,0)+
                                                NVL(i_nBottomPadding,0)
                                  ) THEN
              bNoFit:=TRUE;
              EXIT;
            END IF;
            nHeight:=nHeight+lText(i).nHeight;
            i:=i+1;
          END LOOP;
        END IF;
        IF     bFirstPage
          AND nHeight<i_nHeight THEN
          nHeight:=i_nHeight;
        ELSIF bFirstPage THEN
          nHeight:=nHeight+
                   NVL(i_nBottomPadding,0)+
                   NVL(i_nTopPadding,0);
        ELSIF bNoFit AND nHeight<i_nHeight THEN
          nHeight:=i_nHeight;
        END IF;
        -- now render the box
        PR_RENDER_BOX_FOR_TEXT(i_nY     =>i_nTopY,
                               i_nHeight=>nHeight+
                                          CASE WHEN bFirstPage THEN
                                            0
                                          ELSE
                                            NVL(i_nBottomPadding,0)+
                                            NVL(i_nTopPadding,0)
                                          END
                              );
        bFirstPage:=FALSE;
        RETURN nHeight;
      END;

      PROCEDURE PR_TEXT_NO_ROTATION IS
      BEGIN
        IF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.MIDDLE_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxHeight THEN
            nY:=i_nY+(i_nMaxHeight-nHeight)/2;
          ELSE
            nY:=i_nY;
          END IF;
        ELSIF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.BOTTOM_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxHeight THEN
            nY:=i_nY+(i_nMaxHeight-nHeight);
          ELSE
            nY:=i_nY;
          END IF;
        ELSE
          nY:=i_nY;
        END IF;

        nStartY:=nY;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Alignment vertical/horizontal/nY ' || i_vcVerticalAlign || '/' || vcAlignment || '/' || nY);
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('No. of text parts ' || lText.COUNT);
        END IF;

        -- calculate the positions for the next possible page-breaks
        nStopHeight:=FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  =>1,
                                                  i_nCurrentY =>nY,
                                                  i_nTopY     =>i_nY
                                                 );
        -- write the textparts
        FOR i IN 1..lText.COUNT LOOP
          IF    INSTR(vcAlignment, PK_JRXML2PDF_TYPES.RIGHT_ALIGN ) > 0
             OR INSTR(vcAlignment, PK_JRXML2PDF_TYPES.END_ALIGN ) > 0 THEN
            nX:=nXstart+i_nMaxWidth-lText(i).nLength;
          ELSIF INSTR(vcAlignment, PK_JRXML2PDF_TYPES.CENTER_ALIGN ) > 0 THEN
            nX:=nXstart+(i_nMaxWidth-lText(i).nLength) / 2;
          ELSE
            nX:=nXStart;
          END IF;
          AS_PDF3_MOD.put_txt(p_x               =>nX,
                              p_y               =>nBaseY-nY,
                              p_txt             =>lText(i).vcText);
          IF i_bMayPageBreak AND i<lText.COUNT THEN
            nY:=nY+lText(i).nHeight;
            IF NOT FK_FITS_IN_PAGE(nY, lText(i+1).nHeight+NVL(i_nBottomPadding,0)+
                                                          NVL(i_nTopPadding,0)) THEN
              rArea.nY:=nStartY+nStopHeight;
              PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                           i_nStackPos     =>lReportStack.COUNT,
                                           i_bOneRecordBack=>FALSE);
              nY:=rArea.nY;
              nStartY:=rArea.nY;
              -- after a page-break, the new Y-positin must be tracked and returned
              bTrackY:=TRUE;
              -- calculate the positions for the next possible page-breaks
              nStopHeight:=FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  =>i+1,
                                                        i_nCurrentY =>nY,
                                                        i_nTopY     =>nY
                                                       );
              -- reset font and color, maybe changed by footers and headers
              PR_SET_FONT_AND_COLOR;
            END IF;
          ELSE
            nY:=nY+lText(i).nHeight;
          END IF;
        END LOOP;
      END;

      PROCEDURE PR_TEXT_90_ROTATION IS
      BEGIN
        IF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.MIDDLE_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxWidth THEN
            nX:=nXStart+(i_nMaxWidth-nHeight)/2;
          ELSE
            nX:=nXStart;
          END IF;
        ELSIF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.BOTTOM_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxWidth THEN
            nX:=nXStart+(i_nMaxHeight-nHeight);
          ELSE
            nX:=nXStart;
          END IF;
        ELSE
          nX:=nXStart;
        END IF;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Alignment vertical/horizontal/nY ' || i_vcVerticalAlign || '/' || vcAlignment || '/' || nY);
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('No. of text parts ' || lText.COUNT);
        END IF;
        -- calculate the positions for the next possible page-breaks
        nStopHeight:=FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  =>1,
                                                  i_nCurrentY =>nY,
                                                  i_nTopY     =>i_nY
                                                 );

        -- write the textparts
        FOR i IN 1..lText.COUNT LOOP
          IF    INSTR(vcAlignment, PK_JRXML2PDF_TYPES.RIGHT_ALIGN ) > 0
             OR INSTR(vcAlignment, PK_JRXML2PDF_TYPES.END_ALIGN ) > 0 THEN
            nY:=i_nY+lText(i).nLength;
          ELSIF INSTR(vcAlignment, PK_JRXML2PDF_TYPES.CENTER_ALIGN ) > 0 THEN
            nY:=i_nY+i_nHeight-(i_nMaxHeight-lText(i).nLength) / 2;
          ELSE
            nY:=i_nY+i_nHeight;
          END IF;
          AS_PDF3_MOD.put_txt(p_x               =>nX+lText(i).nHeight*0.66,
                              p_y               =>nBaseY-nY+lText(i).nHeight*0.80,
                              p_txt             =>lText(i).vcText,
                              p_degrees_rotation=>90);
          nX:=nX+lText(i).nHeight;
        END LOOP;
      END;

      PROCEDURE PR_TEXT_180_ROTATION IS
        lDummy PK_JRXML2PDF_TYPES.tTextPiece;
      BEGIN
        IF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.MIDDLE_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxHeight THEN
            nY:=i_nY+(i_nMaxHeight-nHeight)/2;
          ELSE
            nY:=i_nY;
          END IF;
        ELSIF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.BOTTOM_ALIGN THEN
          nY:=i_nY;
        ELSE
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxHeight THEN
            nY:=i_nY+(i_nMaxHeight-nHeight);
          ELSE
            nY:=i_nY;
          END IF;
        END IF;

        nStartY:=nY;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Alignment vertical/horizontal/nY ' || i_vcVerticalAlign || '/' || vcAlignment || '/' || nY);
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('No. of text parts ' || lText.COUNT);
        END IF;

        -- calculate the positions for the next possible page-breaks
        nStopHeight:=FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  =>1,
                                                  i_nCurrentY =>nY,
                                                  i_nTopY     =>i_nY
                                                 );
        -- upside-down, revert text-pieces
        FOR i IN 1..TRUNC(lText.COUNT/2) LOOP
          lDummy:=lText(i);
          lText(i):=lText(lText.COUNT-i+1);
          lText(lText.COUNT-i+1):=lDummy;
        END LOOP;
        -- write the textparts
        FOR i IN 1..lText.COUNT LOOP
          IF    INSTR(vcAlignment, PK_JRXML2PDF_TYPES.RIGHT_ALIGN ) > 0
             OR INSTR(vcAlignment, PK_JRXML2PDF_TYPES.END_ALIGN ) > 0 THEN
            nX:=nXstart+lText(i).nLength;
          ELSIF INSTR(vcAlignment, PK_JRXML2PDF_TYPES.CENTER_ALIGN ) > 0 THEN
            nX:=nXstart+(i_nMaxWidth-lText(i).nLength) / 2 + lText(i).nLength;
          ELSE
            nX:=nXStart+nWidth;
          END IF;
          AS_PDF3_MOD.put_txt(p_x               =>nX,
                              p_y               =>nBaseY-nY+lText(i).nHeight*0.66,
                              p_txt             =>lText(i).vcText,
                              p_degrees_rotation=>180);
          nY:=nY+lText(i).nHeight;
        END LOOP;
      END;

      PROCEDURE PR_TEXT_270_ROTATION IS
        lDummy PK_JRXML2PDF_TYPES.tTextPiece;
      BEGIN
        IF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.MIDDLE_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxWidth THEN
            nX:=nXStart+(i_nMaxWidth-nHeight)/2;
          ELSE
            nX:=nXStart;
          END IF;
        ELSIF i_vcVerticalAlign=PK_JRXML2PDF_TYPES.TOP_ALIGN THEN
          -- calculate total text height
          FOR i IN 1..lText.COUNT LOOP
            nHeight:=nHeight+lText(i).nHeight;
          END LOOP;
          IF nHeight<i_nMaxWidth THEN
            nX:=nXStart+(i_nMaxWidth-nHeight);
          ELSE
            nX:=nXStart;
          END IF;
        ELSE
          nX:=nXStart;
        END IF;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Alignment vertical/horizontal/nY ' || i_vcVerticalAlign || '/' || vcAlignment || '/' || nY);
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('No. of text parts ' || lText.COUNT);
        END IF;
        -- calculate the positions for the next possible page-breaks
        nStopHeight:=FK_CALC_BREAK_AND_RENDER_BOX(i_nTextPos  =>1,
                                                  i_nCurrentY =>nY,
                                                  i_nTopY     =>i_nY
                                                 );
        -- upside-down, revert text-pieces
        FOR i IN 1..TRUNC(lText.COUNT/2) LOOP
          lDummy:=lText(i);
          lText(i):=lText(lText.COUNT-i+1);
          lText(lText.COUNT-i+1):=lDummy;
        END LOOP;

        -- write the textparts
        FOR i IN 1..lText.COUNT LOOP
          IF    INSTR(vcAlignment, PK_JRXML2PDF_TYPES.RIGHT_ALIGN ) > 0
             OR INSTR(vcAlignment, PK_JRXML2PDF_TYPES.END_ALIGN ) > 0 THEN
            nY:=i_nY+i_nHeight-lText(i).nLength;
          ELSIF INSTR(vcAlignment, PK_JRXML2PDF_TYPES.CENTER_ALIGN ) > 0 THEN
            nY:=i_nY+(i_nMaxHeight-lText(i).nLength) / 2;
          ELSE
            nY:=i_nY;
          END IF;
          AS_PDF3_MOD.put_txt(p_x               =>nX+lText(i).nHeight*0.20,
                              p_y               =>nBaseY-nY+lText(i).nHeight*0.80,
                              p_txt             =>lText(i).vcText,
                              p_degrees_rotation=>270);
          nX:=nX+lText(i).nHeight;
        END LOOP;
      END;

    BEGIN
      nBaseY:=rPageSetup.nPageHeight
              -rPageSetup.nTopMargin
              -AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)
              -COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0);

      nXStart:=rPageSetup.nLeftMargin+i_nX+COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0);
      nx:=nXStart;
      nLineSpacing:=(NVL(i_nLineSpacing, PK_JRXML2PDF_TYPES.MIN_LINE_SPACING)-1)*AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);

      IF    nRotation IS NULL
         OR nRotation=180 THEN
        PK_JRXML2PDF_UTIL.PR_GET_TEXT_PIECES(io_lText          =>lText,
                                             io_nCurrentHeight =>nCurrentHeight,
                                             io_nX             =>nX,
                                             i_vcText          =>i_vcText,
                                             i_nMaxHeight      =>CASE WHEN i_bMayPageBreak THEN
                                                                   99999
                                                                 ELSE
                                                                   i_nMaxheight
                                                                 END,
                                             io_nGroup         =>nGroup,
                                             i_nLineSpacing    =>nLineSpacing,
                                             i_nXStart         =>nXStart,
                                             i_nMaxWidth       =>i_nMaxWidth
                                            );
      ELSE
        -- switch width and height
        PK_JRXML2PDF_UTIL.PR_GET_TEXT_PIECES(io_lText          =>lText,
                                             io_nCurrentHeight =>nCurrentHeight,
                                             io_nX             =>nX,
                                             i_vcText          =>i_vcText,
                                             i_nMaxHeight      =>CASE WHEN i_bMayPageBreak THEN
                                                                   99999
                                                                 ELSE
                                                                   i_nMaxWidth
                                                                 END,
                                             io_nGroup         =>nGroup,
                                             i_nLineSpacing    =>nLineSpacing,
                                             i_nXStart         =>nXStart,
                                             i_nMaxWidth       =>i_nMaxHeight
                                            );
      END IF;
      IF nRotation IS NULL THEN
        PR_TEXT_NO_ROTATION;
      ELSIF nRotation=90 THEN
        PR_TEXT_90_ROTATION;
      ELSIF nRotation=180 THEN
        PR_TEXT_180_ROTATION;
      ELSIF nRotation=270 THEN
        PR_TEXT_270_ROTATION;
      END IF;
      IF bTrackY THEN
        rArea.nY:=nY;
      END IF;
    END;
  BEGIN

    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Text ' || i_nX || '-' || i_nY || '-' || i_nWidth || '-' || i_nHeight|| '-' || COALESCE(i_vcFgColor, i_rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK) || '-' || i_vcText);
    END IF;

    PR_SET_FONT_AND_COLOR;

    -- Rotation
    IF i_vcRotation IS NOT NULL THEN
      IF i_vcRotation='UpsideDown' THEN
        nRotation:=180;
      ELSIF i_vcRotation='Left' THEN
        nRotation:=90;
      ELSIF i_vcRotation='Right' THEN
        nRotation:=270;
      END IF;
    END IF;
    nHeight:=i_nHeight-COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0)
                      -COALESCE(i_nBottomPadding, i_rStyle.nBottomPadding, 0);

    nWidth:=i_nWidth-COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0)
                    -COALESCE(i_nRightPadding, i_rStyle.nRightPadding, 0);

    PR_WRITE_TEXT_TO_BOX(i_nMaxHeight      =>nHeight,
                         i_nLineSpacing    =>COALESCE(i_nLineSpacing, i_rStyle.nLineSpacing, PK_JRXML2PDF_TYPES.MIN_LINE_SPACING),
                         i_nMaxWidth       =>nWidth,
                         i_vcAlignment     =>LOWER(COALESCE(i_vcAlignment, i_rStyle.vcAlignment, PK_JRXML2PDF_TYPES.LEFT_ALIGN)),
                         i_vcVerticalAlign =>LOWER(COALESCE(i_vcVerticalAlign, i_rStyle.vcVerticalAlign, PK_JRXML2PDF_TYPES.TOP_ALIGN))
                        );

    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Text finished');
    END IF;

    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_BUILD_HTML_ATTRIB_LIST(i_vcAttribs IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tAttributeList IS
    lAttribs     PK_JRXML2PDF_TYPES.tAttributeList;
    vcAllAttribs PK_JRXML2PDF_TYPES.tMaxVarchar2:=TRIM(i_vcAttribs);
    vcOneAttrib  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcName       PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcValue      PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(' Attribs: [' || vcAllAttribs ||  ']');
    END IF;
    IF vcAllAttribs LIKE 'style="%' THEN
      -- only check the style-attributes
      vcAllAttribs:=REPLACE(REPLACE(vcAllAttribs, 'style="', ''), '"', '');
      LOOP
        vcOneAttrib:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcAllAttribs, ';');
        EXIT WHEN vcOneAttrib IS NULL;
        vcName:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcOneAttrib, ':');
        vcValue:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcOneAttrib, ':');
        IF    vcName IS NOT NULL
           AND vcValue IS NOT NULL THEN
          lAttribs(TRIM(vcName)):=TRIM(vcValue);
        END IF;
      END LOOP;
    END IF;
    RETURN lAttribs;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_TOKENIZE_HTML(i_nFontSize IN NUMBER,
                            i_vcText    IN VARCHAR2,
                            i_vcColor   IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tHtmlTree IS
    lResult         PK_JRXML2PDF_TYPES.tHtmlTree;
    nPos            NUMBER:=0;
    nTagEnd         NUMBER;
    nLength         NUMBER:=LENGTH(i_vcText);
    vcText          PK_JRXML2PDF_TYPES.tMaxVarchar2:=i_vcText;
    bInList         BOOLEAN:=FALSE;
    nListIndex      NUMBER;

    PROCEDURE PR_FIT_TEXT(io_rPiece IN OUT PK_JRXML2PDF_TYPES.tHtmlPiece, i_vcText IN VARCHAR2) IS
      vcText PK_JRXML2PDF_TYPES.tMaxVarchar2:=i_vcText;
      iPos   PLS_INTEGER;
      iPos2  PLS_INTEGER;
      vctag  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      IF NOT io_rPiece.bIsPRE THEN
        vcText:=REPLACE(REPLACE(REPLACE(vcText, CHR(10), ''), CHR(9), ''), CHR(13), '');
      END IF;
      -- remove leading spaces
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(' FIT_TEXT for tag ' || io_rPiece.vcTag || CASE WHEN io_rPiece.bIsPRE THEN 'PRE' END);
      END IF;
      IF NOT io_rPiece.bIsPRE THEN
        -- cut leading spaces
        vcText:=LTRIM(vcText);
        -- elimite double spaces
        vcText:=REPLACE(vcText, '  ', ' ');
      END IF;
      iPos:=0;
      IF vcText IS NOT NULL THEN
        LOOP
          -- extract tag
          iPos:=INSTR(vctext, '&', iPos+1);
          EXIT WHEN iPos=0;
          iPos2:=INSTR(vctext, ';', iPos);
          EXIT WHEN iPos2=0;
          vcTag:=SUBSTR(vcText, iPos,iPos2-iPos+1);
          IF lHtmlReplacements.EXISTS(vcTag) THEN
            vcText:=REPLACE(vcText, vcTag, lHtmlReplacements(vcTag));
          ELSIF vcTag LIKE '&#x%' THEN
            vcText:=REPLACE(vcText, vcTag, '\' || LPAD(TRANSLATE(vcTag, '&#x;',''), 4, '0'));
          ELSIF vcTag LIKE '&#%' THEN
            vcText:=REPLACE(vcText, vcTag, '\' || TO_CHAR(TO_NUMBER(TRANSLATE(vcTag, '&#;','')), 'FM000X'));
          END IF;
          iPos:=iPos+1;
        END LOOP;
      END IF;
      io_rPiece.vcText:=vcText;
    END;

    PROCEDURE PR_REKU_TOKENIZE_HTML(io_rMasterHtml IN OUT PK_JRXML2PDF_TYPES.tHtmlPiece, i_nLevel IN NUMBER) IS
      rPiece          PK_JRXML2PDF_TYPES.tHtmlPiece;
      rEmptyPiece     PK_JRXML2PDF_TYPES.tHtmlPiece;
      bEndTag         BOOLEAN;
      vcTag           PK_JRXML2PDF_TYPES.tMaxVarchar2;
      vcAttribs       PK_JRXML2PDF_TYPES.tMaxVarchar2;
      iTag            PLS_INTEGER;
      iPos            PLS_INTEGER;
    BEGIN
      LOOP
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(i_nLevel || ' Start');
        END IF;
        nPos:=INSTR(vcText, PK_JRXML2PDF_TYPES.HTML_LT);
        IF nPos>0 THEN
          IF nPos>1 THEN
            IF     io_rMasterHtml.vcText IS NULL
               AND lResult.lPieces.COUNT=0 THEN
              io_rMasterHtml.vcText:=SUBSTR(vcText, 1, nPos-1);
              PR_FIT_TEXT(io_rMasterHtml, SUBSTR(vcText, 1, nPos-1));
            ELSE
              -- additional text, add another text-tag
              rPiece:=io_rMasterHtml;
              rPiece.lSubPieces.DELETE;
              IF rPiece.vcTag IN (PK_JRXML2PDF_TYPES.HTML_H1,
                                  PK_JRXML2PDF_TYPES.HTML_H2,
                                  PK_JRXML2PDF_TYPES.HTML_H3,
                                  PK_JRXML2PDF_TYPES.HTML_H4,
                                  PK_JRXML2PDF_TYPES.HTML_P) THEN
                -- keep leading spaces
                rPiece.bIsPRE:=TRUE;
              END IF;
              rPiece.vcTag:=PK_JRXML2PDF_TYPES.HTML_TEXT;
              PR_FIT_TEXT(rpiece, SUBSTR(vcText, 1, nPos-1));

              iPos:=lResult.lPieces.COUNT+1;
              lResult.lPieces(iPos):=rPiece;
              io_rMasterHtml.lSubPieces(io_rMasterHtml.lSubPieces.COUNT+1):=iPos;
            END IF;
          END IF;
          vcText:=SUBSTR(vcText, nPos+1);
          -- Identify tag
          bEndTag:=(SUBSTR(vcText,1,1)='/');
          IF bEndTag THEN
            vcText:=SUBSTR(vcText,2);
          END IF;
          iTag:=1;
          LOOP
            EXIT WHEN vcText LIKE lhtmlTokens(iTag).vcTag || '%';
            iTag:=iTag+1;
            EXIT WHEN iTag>lhtmlTokens.COUNT;
          END LOOP;
          nPos:=INSTR(vcText, PK_JRXML2PDF_TYPES.HTML_GT);
          IF nPos>0 THEN
            IF iTag<=lhtmlTokens.COUNT THEN
              vcTag:=lhtmlTokens(iTag).vcTag;
            ELSE
              vcTag:='';
            END IF;
            vcAttribs:=SUBSTR(vcText, LENGTH(vcTag)+1, nPos-1-LENGTH(vcTag));
            vcText:=SUBSTR(vcText, nPos+1);
          END IF;
          IF vcTag IN (PK_JRXML2PDF_TYPES.HTML_UL, PK_JRXML2PDF_TYPES.HTML_OL) THEN
            -- reset list-settings
            bInList:=NOT bEndTag;
            IF vcTag=PK_JRXML2PDF_TYPES.HTML_UL THEN
              nListIndex:=-1;
            ELSE
              nListIndex:=0;
            END IF;
          END IF;
          IF     bInList
             AND vcTag=PK_JRXML2PDF_TYPES.HTML_LI THEN
            IF     nListIndex>=0
               AND NOT bEndTag THEN
              nListIndex:=nListIndex+1;
            END IF;
          END IF;
          IF NOT bEndTag THEN

            -- one level deeper
            rPiece:=rEmptyPiece;
            rPiece.nListIndex:=nListIndex;
            rPiece.vcTag:=vcTag;
            rPiece.lAttribs:=FK_BUILD_HTML_ATTRIB_LIST(vcAttribs);
            -- Take over the text-align-style-attribute from the master to the current if set
            IF         io_rMasterHtml.lAttribs.EXISTS('text-align')
               AND NOT rPiece.lAttribs.EXISTS('text-align') THEN
              rPiece.lAttribs('text-align'):=io_rMasterHtml.lAttribs('text-align');
            END IF;

            rPiece.bIsPRE:=(vcTag IN (PK_JRXML2PDF_TYPES.HTML_PRE, PK_JRXML2PDF_TYPES.HTML_CODE, PK_JRXML2PDF_TYPES.HTML_SPAN));
            rPiece.bInList:=bInList;
            iPos:=lResult.lPieces.COUNT+1;
            lResult.lPieces(iPos):=rPiece;
            io_rMasterHtml.lSubPieces(io_rMasterHtml.lSubPieces.COUNT+1):=iPos;
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(vcTag || ':' || (io_rMasterHtml.lSubPieces.COUNT) || ':' || iPos);
            END IF;
            PR_REKU_TOKENIZE_HTML(rPiece, i_nLevel+1);
            lResult.lPieces(iPos):=rPiece;
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(i_nLevel || ' Back from sub store in Pos/Text:' || iPos || '/' || lResult.lPieces(iPos).vcText);
            END IF;
          END IF;
        ELSE
          vcText:=NULL;
        END IF;
        EXIT WHEN bEndtag;
        EXIT WHEN vcText IS NULL;
      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL(i_nLevel || ' Ende');
      END IF;
    END;
  BEGIN
    lResult.lPieces(1).bIsPRE:=FALSE;
    lResult.lPieces(1).nFontSize:=i_nFontSize;
    lResult.lPieces(1).vcColor:=i_vcColor;
    IF INSTR(vcText, PK_JRXML2PDF_TYPES.HTML_LT)=0 THEN
      vcText:=PK_JRXML2PDF_TYPES.HTML_P_START || vcText || PK_JRXML2PDF_TYPES.HTML_P_END;
    END IF;
    PR_REKU_TOKENIZE_HTML(lResult.lPieces(1), 1);
    RETURN lResult;
  END;

  -- ----------------------------------------------------------------------

  PROCEDURE PR_LAYOUT_HTML(i_rHtmlSetting IN PK_JRXML2PDF_TYPES.tHtmlSettings,
                           io_lHtml       IN OUT NOCOPY PK_JRXML2PDF_TYPES.tHtmlTree,
                           i_nX           IN NUMBER,
                           i_nWidth       IN NUMBER,
                           i_vcColor      IN VARCHAR2) IS

    ALIGN_LEFT    CONSTANT NUMBER:=1;
    ALIGN_RIGHT   CONSTANT NUMBER:=2;
    ALIGN_CENTER  CONSTANT NUMBER:=3;
    ALIGN_JUSTIFY CONSTANT NUMBER:=4;

    bInList       BOOLEAN:=FALSE;
    nGroup        NUMBER:=0;
    nDummy        NUMBER;
    lGroupWidth   PK_JRXML2PDF_TYPES.tNumList;
    lWordCount    PK_JRXML2PDF_TYPES.tNumList;
    lGroupAligns  PK_JRXML2PDF_TYPES.tNumList;
    nX            NUMBER:=i_nX;
    nGroupPos     NUMBER;
    nCurrentGroup NUMBER;
    bSplitWord    BOOLEAN;
    nAlign        NUMBER;

    PROCEDURE PR_GET_HTML_FONT(i_nPiece IN NUMBER, i_nMasterPiece IN NUMBER, i_vcTag IN VARCHAR2) IS
      vcFont      PK_JRXML2PDF_TYPES.tFont;
      nFontSize   NUMBER;
      vcFontStyle PK_JRXML2PDF_TYPES.tFontStyle;
      vcColor     PK_JRXML2PDF_TYPES.tColor;
      vcAttrib    PK_JRXML2PDF_TYPES.tMaxVarchar2;
      vcR         PK_JRXML2PDF_TYPES.tAttribute;
      vcG         PK_JRXML2PDF_TYPES.tAttribute;
      vcB         PK_JRXML2PDF_TYPES.tAttribute;
    BEGIN
      IF i_nMasterPiece IS NULL THEN
        vcFontStyle:=PK_JRXML2PDF_TYPES.FONT_NORMAL;
        vcFont:=i_rHtmlSetting.vcFont;
      ELSE
        vcFontStyle:=io_lHtml.lPieces(i_nMasterPiece).vcFontStyle;
        vcFont:=io_lHtml.lPieces(i_nMasterPiece).vcFont;
      END IF;
      IF i_vcTag IN (PK_JRXML2PDF_TYPES.HTML_B, PK_JRXML2PDF_TYPES.HTML_STRONG) THEN
        vcFontStyle:=PK_JRXML2PDF_TYPES.FONT_BOLD;
      END IF;
      IF i_vcTag=PK_JRXML2PDF_TYPES.HTML_H1 THEN
        nFontSize:=i_rHtmlSetting.nH1FontSize;
      ELSIF i_vcTag=PK_JRXML2PDF_TYPES.HTML_H2 THEN
        nFontSize:=i_rHtmlSetting.nH2FontSize;
      ELSIF i_vcTag=PK_JRXML2PDF_TYPES.HTML_H3 THEN
        nFontSize:=i_rHtmlSetting.nH3FontSize;
      ELSIF i_vcTag=PK_JRXML2PDF_TYPES.HTML_H4 THEN
        nFontSize:=i_rHtmlSetting.nH4FontSize;
      ELSIF i_vcTag=PK_JRXML2PDF_TYPES.HTML_P THEN
        nFontSize:=i_rHtmlSetting.nFontSize;
      ELSIF i_nMasterPiece IS NOT NULL THEN
        nFontSize:=io_lHtml.lPieces(i_nMasterPiece).nFontSize;
      ELSE
        nFontSize:=i_rHtmlSetting.nFontSize;
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Attrib-COUNT:' || io_lHtml.lPieces(i_nPiece).lAttribs.COUNT);
      END IF;
      vcAttrib:=io_lHtml.lPieces(i_nPiece).lAttribs.FIRST;
      WHILE vcAttrib IS NOT NULL LOOP
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Attrib:' || vcAttrib || '#' || io_lHtml.lPieces(i_nPiece).lAttribs(vcAttrib));
        END IF;
        vcAttrib:=io_lHtml.lPieces(i_nPiece).lAttribs.NEXT(vcAttrib);
      END LOOP;
      IF    io_lHtml.lPieces(i_nPiece).lAttribs.EXISTS('font-size') THEN
        vcAttrib:=io_lHtml.lPieces(i_nPiece).lAttribs('font-size');
        IF vcAttrib='xx-large' THEN
          nFontSize:=i_rHtmlSetting.nXXLargeFontSize;
        ELSIF vcAttrib='x-large' THEN
          nFontSize:=i_rHtmlSetting.nXLargeFontSize;
        ELSIF vcAttrib='large' THEN
          nFontSize:=i_rHtmlSetting.nLargeFontSize;
        ELSIF vcAttrib='medium' THEN
          nFontSize:=i_rHtmlSetting.nMediumFontSize;
        ELSIF vcAttrib='small' THEN
          nFontSize:=i_rHtmlSetting.nSmallFontSize;
        ELSIF vcAttrib='x-small' THEN
          nFontSize:=i_rHtmlSetting.nXSmallFontSize;
        ELSIF vcAttrib='xx-small' THEN
          nFontSize:=i_rHtmlSetting.nXXSmallFontSize;
        ELSIF vcAttrib='smaller' THEN
          nFontSize:=i_rHtmlSetting.nSmallerFontSize;
        ELSIF vcAttrib='larger' THEN
          nFontSize:=i_rHtmlSetting.nLargerFontSize;
        ELSE
          -- a number
          BEGIN
            nFontSize:=TO_NUMBER(REPLACE(vcAttrib, PK_JRXML2PDF_TYPES.PIXEL, ''));
          EXCEPTION
            WHEN OTHERS THEN
              nFontSize:=i_rHtmlSetting.nFontSize;
          END;
        END IF;
      END IF;
      IF    io_lHtml.lPieces(i_nPiece).lAttribs.EXISTS('color') THEN
        vcColor:=REPLACE(io_lHtml.lPieces(i_nPiece).lAttribs('color'), '#');
        IF UPPER(vcColor) LIKE 'RGB(%' THEN
          vcColor:=REPLACE(UPPER(vcColor), 'RGB');
          vcR:=REPLACE(PK_JRXML2PDF_UTIL.FK_SPLIT(vcColor, ','), '(');
          vcG:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcColor, ',');
          vcB:=PK_JRXML2PDF_UTIL.FK_SPLIT(vcColor, ')');
          vcColor:=TO_CHAR(TO_NUMBER(vcR), 'FM0X') ||
                   TO_CHAR(TO_NUMBER(vcG), 'FM0X') ||
                   TO_CHAR(TO_NUMBER(vcB), 'FM0X');
        END IF;
      ELSIF i_nMasterPiece IS NOT NULL THEN
        vcColor:=io_lHtml.lPieces(i_nMasterPiece).vcColor;
      ELSE
        vcColor:=i_vcColor;
      END IF;
      IF i_vcTag=PK_JRXML2PDF_TYPES.HTML_PRE THEN
        vcFont:=PK_JRXML2PDF_TYPES.COURIER_NEW;
      ELSIF io_lHtml.lPieces(i_nPiece).lAttribs.EXISTS('font-family') THEN
        vcFont:=io_lHtml.lPieces(i_nPiece).lAttribs('font-family');
        IF INSTR(vcFont, ',')>0 THEN
          vcFont:=SUBSTR(vcFont, 1, INSTR(vcFont, ',')-1);
        END IF;
      END IF;
      io_lHtml.lPieces(i_nPiece).vcFont:=vcFont;
      io_lHtml.lPieces(i_nPiece).nFontSize:=nFontSize;
      io_lHtml.lPieces(i_nPiece).vcFontStyle:=vcFontStyle;
      io_lHtml.lPieces(i_nPiece).vcColor:=vcColor;

      -- set font
      PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>vcFont,
                                    i_vcStyle =>vcFontStyle,
                                    i_nSize   =>nFontSize
                                   );
    END;

    PROCEDURE PR_REKU_LAYOUT_HTML(i_nMasterPiece IN NUMBER, i_nPiece IN NUMBER, i_nLevel IN NUMBER) IS
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('>'|| RPAD(' ', i_nLevel*2, ' ') || io_lHtml.lPieces(i_nPiece).vcTag ||':' || io_lHtml.lPieces(i_nPiece).vcText);
      END IF;
      IF io_lHtml.lPieces(i_nPiece).vcTag IN (PK_JRXML2PDF_TYPES.HTML_UL, PK_JRXML2PDF_TYPES.HTML_OL) THEN
        bInList:=TRUE;
      END IF;
      IF io_lHtml.lPieces(i_nPiece).vcTag IN (PK_JRXML2PDF_TYPES.HTML_BR,PK_JRXML2PDF_TYPES.HTML_LI) THEN
        IF bInList THEN
          nx:=i_nX+i_rHtmlSetting.nListIndent;
        ELSE
          nx:=i_nX;
        END IF;
        nGroup:=nGroup+1;
      ELSIF io_lHtml.lPieces(i_nPiece).vcTag IN (PK_JRXML2PDF_TYPES.HTML_H1,
                                                 PK_JRXML2PDF_TYPES.HTML_H2,
                                                 PK_JRXML2PDF_TYPES.HTML_H3,
                                                 PK_JRXML2PDF_TYPES.HTML_H4,
                                                 PK_JRXML2PDF_TYPES.HTML_P,
                                                 PK_JRXML2PDF_TYPES.HTML_BR) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Tag is ' || io_lHtml.lPieces(i_nPiece).vcTag);
        END IF;
        -- new line, but only if last tag was not a new line
        IF i_nMasterPiece IS NOT NULL THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('AND Master Tag is ' || io_lHtml.lPieces(i_nMasterPiece).vcTag);
          END IF;
          IF    io_lHtml.lPieces(i_nMasterPiece).vcTag NOT IN (PK_JRXML2PDF_TYPES.HTML_LI)
             OR io_lHtml.lPieces(i_nMasterPiece).vcTag IS NULL THEN
            IF bInList THEN
              nx:=i_nX+i_rHtmlSetting.nListIndent;
            ELSE
              nx:=i_nX;
            END IF;
            nGroup:=nGroup+1;
          END IF;
        END IF;
      END IF;
      IF nGroup=0 THEN
        nGroup:=1;
      END IF;

      io_lHtml.lPieces(i_nPiece).nGroup:=nGroup;
      PR_GET_HTML_FONT(i_nPiece, i_nMasterPiece, io_lHtml.lPieces(i_nPiece).vcTag);

      IF     i_nPiece=1
         AND io_lHtml.lPieces(i_nPiece).vcTag IS NULL
         AND io_lHtml.lPieces(i_nPiece).vcText IS NULL THEN
          -- dummy-piece at the beginning, set size to 0
        io_lHtml.lPieces(i_nPiece).nHeight:=0;
      ELSE
        io_lHtml.lPieces(i_nPiece).nHeight:=AS_PDF3_MOD.GET(AS_PDF3_MOD.c_get_fontsize);
      END IF;

      io_lHtml.lPieces(i_nPiece).nX:=nX;

      IF io_lHtml.lPieces(i_nPiece).vcText IS NOT NULL THEN
        -- check for alignment
        IF io_lHtml.lPieces(i_nPiece).lAttribs.EXISTS('text-align') THEN
          bSplitWord:=io_lHtml.lPieces(i_nPiece).lAttribs('text-align')='justify';
        ELSE
          bSplitWord:=FALSE;
        END IF;
        -- layout the complete text in lines
        PK_JRXML2PDF_UTIL.PR_GET_TEXT_PIECES(io_lText          =>io_lHtml.lPieces(i_nPiece).lTextPieces,
                                             io_nCurrentHeight =>nDummy,
                                             io_nX             =>nX,
                                             i_vcText          =>io_lHtml.lPieces(i_nPiece).vcText,
                                             i_nMaxHeight      =>30,
                                             io_nGroup         =>nGroup,
                                             i_nLineSpacing    =>0,
                                             i_nXStart         =>CASE WHEN bInList THEN
                                                                   i_nX+i_rHtmlSetting.nListIndent
                                                                 ELSE
                                                                   i_nX
                                                                 END,
                                             i_nMaxWidth       =>i_nWidth,
                                             i_bSplitByWord    =>bSplitWord
                                            );
      END IF;
      FOR i IN 1..io_lHtml.lPieces(i_nPiece).lSubPieces.COUNT LOOP
        PR_REKU_LAYOUT_HTML(i_nPiece, io_lHtml.lPieces(i_nPiece).lSubPieces(i), i_nLevel+1);
      END LOOP;
      IF io_lHtml.lPieces(i_nPiece).vcTag IN (PK_JRXML2PDF_TYPES.HTML_UL, PK_JRXML2PDF_TYPES.HTML_OL) THEN
        bInList:=FALSE;
      END IF;
    END;

  BEGIN
    PR_REKU_LAYOUT_HTML(NULL, 1,1);

    -- restore widths for each group
    FOR i IN 1..io_lHtml.lPieces.COUNT LOOP
      FOR j IN 1..io_lHtml.lPieces(i).lTextPieces.COUNT LOOP
        IF NOT lGroupWidth.EXISTS(io_lHtml.lPieces(i).lTextPieces(j).nGroup) THEN
          lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=io_lHtml.lPieces(i).lTextPieces(j).nLength;
          lWordCount(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=1;
          -- first match defines alignment
          IF io_lHtml.lPieces(i).lAttribs.EXISTS('text-align') THEN
            IF io_lHtml.lPieces(i).lAttribs('text-align')='center' THEN
              lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=ALIGN_CENTER;
            ELSIF io_lHtml.lPieces(i).lAttribs('text-align')='right' THEN
              lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=ALIGN_RIGHT;
            ELSIF io_lHtml.lPieces(i).lAttribs('text-align')='justify' THEN
              lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=ALIGN_JUSTIFY;
            ELSE
              lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=ALIGN_LEFT;
            END IF;
          ELSE
            lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=ALIGN_LEFT;
          END IF;
        ELSE
          lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup)+io_lHtml.lPieces(i).lTextPieces(j).nLength;
          lWordCount(io_lHtml.lPieces(i).lTextPieces(j).nGroup):=lWordCount(io_lHtml.lPieces(i).lTextPieces(j).nGroup)+1;
        END IF;
      END LOOP;
    END LOOP;
    nGroupPos:=0;
    nCurrentGroup:=-1;
    FOR i IN 1..io_lHtml.lPieces.COUNT LOOP
      FOR j IN 1..io_lHtml.lPieces(i).lTextPieces.COUNT LOOP
        IF nCurrentGroup!=io_lHtml.lPieces(i).lTextPieces(j).nGroup THEN
          nGroupPos:=0;
          nCurrentGroup:=io_lHtml.lPieces(i).lTextPieces(j).nGroup;
          nAlign:=lGroupAligns(io_lHtml.lPieces(i).lTextPieces(j).nGroup);
        END IF;
        IF lGroupWidth.EXISTS(io_lHtml.lPieces(i).lTextPieces(j).nGroup) THEN
          IF nAlign=ALIGN_CENTER THEN
            io_lHtml.lPieces(i).lTextPieces(j).nX:=io_lHtml.lPieces(i).lTextPieces(j).nX+(i_nWidth-lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup))/2;
          ELSIF nAlign=ALIGN_RIGHT THEN
            io_lHtml.lPieces(i).lTextPieces(j).nX:=io_lHtml.lPieces(i).lTextPieces(j).nX+i_nWidth-lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup);
          ELSIF nAlign=ALIGN_JUSTIFY THEN
            IF (lWordCount(io_lHtml.lPieces(i).lTextPieces(j).nGroup)-1)>0 THEN
              io_lHtml.lPieces(i).lTextPieces(j).nX:=io_lHtml.lPieces(i).lTextPieces(j).nX+((i_nWidth-lGroupWidth(io_lHtml.lPieces(i).lTextPieces(j).nGroup))/(lWordCount(io_lHtml.lPieces(i).lTextPieces(j).nGroup)-1))*nGroupPos;
            END IF;
            nGroupPos:=nGroupPos+1;
          END IF;
        END IF;
      END LOOP;
    END LOOP;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_HTML_HEIGHT(i_nX               IN NUMBER,
                               i_nY               IN NUMBER,
                               i_nWidth           IN NUMBER,
                               i_nHeight          IN NUMBER,
                               i_vcText           IN VARCHAR2,
                               i_vcFont           IN PK_JRXML2PDF_TYPES.tFont,
                               i_nFontSize        IN NUMBER,
                               i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment,
                               i_nTopPadding      IN NUMBER,
                               i_nLeftPadding     IN NUMBER,
                               i_nBottomPadding   IN NUMBER,
                               i_nRightPadding    IN NUMBER,
                               i_nLineSpacing     IN NUMBER,
                               i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle,
                               i_bEnhanced        IN BOOLEAN)
  RETURN NUMBER IS
    lResult       PK_JRXML2PDF_TYPES.tHtmlTree;
    nX            NUMBER;
    nXStart       NUMBER;
    nWidth        NUMBER;
    nOutputHeight NUMBER;
    nGroup        NUMBER:=1;
    lGroupHeight  PK_JRXML2PDF_TYPES.tNumList;

    vcLastTag     PK_JRXML2PDF_TYPES.tAttribute;
    rHtmlSettings PK_JRXML2PDF_TYPES.tHtmlSettings:=rMasterHtmlSettings;

    PROCEDURE PR_REKU_SCALE_HTML(i_rPiece IN PK_JRXML2PDF_TYPES.tHtmlPiece,
                                 i_nLevel IN NUMBER) IS

      FUNCTION FK_GET_LINE_SPACING
      RETURN NUMBER IS
        nResult NUMBER;
      BEGIN
        IF vcLastTag IS NULL THEN
          nResult:=0;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H1 THEN
          nResult:=rHtmlSettings.nH1LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H2 THEN
          nResult:=rHtmlSettings.nH2LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H3 THEN
          nResult:=rHtmlSettings.nH3LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H4 THEN
          nResult:=rHtmlSettings.nH4LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_UL THEN
          nResult:=rHtmlSettings.nBrLineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_LI THEN
          IF vcLastTag!=PK_JRXML2PDF_TYPES.HTML_LI THEN
            nResult:=rHtmlSettings.nH4LineSpacing;
          ELSE
            nResult:=rHtmlSettings.nLiLineSpacing;
          END IF;
        ELSIF i_rPiece.vcTag IN (PK_JRXML2PDF_TYPES.HTML_BR, PK_JRXML2PDF_TYPES.HTML_P) THEN
          nResult:=rHtmlSettings.nBrLineSpacing;
        ELSE
          nResult:=rHtmlSettings.nLineSpacing;
        END IF;
        vcLastTag:=i_rPiece.vcTag;
        RETURN nResult;
      END;
    BEGIN

      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('>'|| RPAD(' ', i_nLevel*2, ' ') || i_rPiece.vcTag ||':' || i_rPiece.vcText || ':' || i_rPiece.vcColor);
      END IF;
      IF i_rPiece.vcText IS NOT NULL THEN
        -- write text
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('scale x/height/text' || i_rPiece.nx || '/' || nOutputHeight || '/[' || i_rPiece.vcText || ']');
        END IF;
        FOR i IN 1..i_rPiece.lTextPieces.COUNT LOOP
          IF nGroup!=i_rPiece.lTextPieces(i).nGroup THEN
            nGroup:=i_rPiece.lTextPieces(i).nGroup;
            IF i=1 THEN
              -- big spacing
              nOutputHeight:=nOutputHeight+lGroupHeight(i_rPiece.lTextPieces(i).nGroup)+FK_GET_LINE_SPACING;
            ELSE
              -- smallm spacing
              nOutputHeight:=nOutputHeight+lGroupHeight(i_rPiece.lTextPieces(i).nGroup)+rHtmlSettings.nLineSpacing;
            END IF;
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Group/height ' || nGroup || '/' || nOutputHeight);
            END IF;
          END IF;
        END LOOP;
      ELSE
        IF nGroup!=i_rPiece.nGroup THEN
          nGroup:=i_rPiece.nGroup;
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Group/height ' || nGroup || '/' || nOutputHeight);
          END IF;
          -- big spacing
          nOutputHeight:=nOutputHeight+lGroupHeight(i_rPiece.nGroup)+FK_GET_LINE_SPACING;
        END IF;
      END IF;
      FOR i IN 1..i_rPiece.lSubPieces.COUNT LOOP
        PR_REKU_SCALE_HTML(lResult.lPieces(i_rPiece.lSubPieces(i)), i_nLevel+1);
      END LOOP;
    END;


  BEGIN
    -- basic font and color are taken from the input values
    rHtmlSettings.vcFont   :=i_vcFont;
    rHtmlSettings.nFontSize:=i_nFontSize;
    IF NOT i_bEnhanced THEN
      -- no enhanced mode for HTML, no rendering of H1, H2, H3, H4
      rHtmlSettings.nH1FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH2FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH3FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH4FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH1LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH2LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH3LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH4LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nLiLineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nBrLineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nLineSpacing  :=rHtmlSettings.nLineSpacing;
    END IF;
    nXStart:=rPageSetup.nLeftMargin+i_nX+COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0);

    lResult:=FK_TOKENIZE_HTML(i_nFontSize=>rHtmlSettings.nFontSize,
                              i_vcText   =>i_vcText,
                              i_vcColor  =>PK_JRXML2PDF_TYPES.BLACK);

    nWidth:=i_nWidth;
    -- initally scale all parts
    PR_LAYOUT_HTML(i_rHtmlSetting=>rHtmlSettings,
                   io_lHtml      =>lResult,
                   i_nX          =>nXStart,
                   i_nWidth      =>i_nWidth,
                   i_vcColor     =>PK_JRXML2PDF_TYPES.BLACK);
    -- for each "line" get the maximum height
    FOR i IN 1..lResult.lPieces.COUNT LOOP
      FOR j IN 1..lResult.lPieces(i).lTextPieces.COUNT LOOP
        IF NOT lGroupHeight.EXISTS(lResult.lPieces(i).lTextPieces(j).nGroup) THEN
          lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup):=lResult.lPieces(i).lTextPieces(j).nHeight;
        ELSE
          lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup):=GREATEST(lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup), lResult.lPieces(i).lTextPieces(j).nHeight);
        END IF;
      END LOOP;
      IF NOT lGroupHeight.EXISTS(lResult.lPieces(i).nGroup) THEN
        lGroupHeight(lResult.lPieces(i).nGroup):=NVL(lResult.lPieces(i).nHeight,0);
      ELSE
        lGroupHeight(lResult.lPieces(i).nGroup):=GREATEST(lGroupHeight(lResult.lPieces(i).nGroup), NVL(lResult.lPieces(i).nHeight,0));
      END IF;
    END LOOP;
    nGroup:=1;
    nOutputHeight:=0;
    vcLastTag:=NULL;
    PR_REKU_SCALE_HTML(lResult.lPieces(1),1);
    -- Add paddings to height
    RETURN nOutputHeight+GREATEST(COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0),0)
                        +GREATEST(COALESCE(i_nBottomPadding, i_rStyle.nBottomPadding,0), 0);
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_TEXT_HEIGHT(i_nX               IN NUMBER,
                               i_nY               IN NUMBER,
                               i_nWidth           IN NUMBER,
                               i_nHeight          IN NUMBER,
                               i_vcText           IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                               i_vcFont           IN PK_JRXML2PDF_TYPES.tFont,
                               i_nFontSize        IN NUMBER,
                               i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment,
                               i_nTopPadding      IN NUMBER,
                               i_nLeftPadding     IN NUMBER,
                               i_nBottomPadding   IN NUMBER,
                               i_nRightPadding    IN NUMBER,
                               i_nLineSpacing     IN NUMBER,
                               i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle)
  RETURN NUMBER IS
    vcFont        VARCHAR2(60);
    nX            NUMBER;
    nY            NUMBER;
    nWidth        NUMBER;
    nLineSpacing  NUMBER;
    nTextHeight   NUMBER;
  BEGIN
    -- set font
    PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont   =>NVL(i_vcFont, i_rStyle.vcFont),
                                  i_vcStyle  =>COALESCE(i_rStyle.vcFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL),
                                  i_nSize    =>COALESCE(i_nFontSize, i_rStyle.nFontSize, 10)
                                 );

    nX:=rPageSetup.nLeftMargin+i_nX+COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0);
    nY:=rPageSetup.nPageHeight-rPageSetup.nTopMargin
                              -i_nY
                              -AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)
                              -COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0);

    nWidth:=i_nWidth-COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0)
                    -COALESCE(i_nRightPadding, i_rStyle.nRightPadding, 0);

    nLineSpacing:=COALESCE(i_nLineSpacing, i_rStyle.nLineSpacing, PK_JRXML2PDF_TYPES.MIN_LINE_SPACING);

    nTextHeight:=PK_JRXML2PDF_UTIL.FK_CALC_BOX_HEIGHT(i_vcText          =>i_vcText,
                                                      i_nX              =>nX,
                                                      i_nMaxHeight      =>9999,
                                                      i_nLineSpacing    =>nLineSpacing,
                                                      i_nXStart         =>nX,
                                                      i_nMaxWidth       =>nWidth
                                                     );
    -- Add paddings to height
    RETURN nTextHeight+GREATEST(COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0),0)
                      +GREATEST(COALESCE(i_nBottomPadding, i_rStyle.nBottomPadding,0), 0);
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_FITS_IN_PAGE(i_nCurrentY           IN NUMBER,
                           i_nDetailHeight       IN NUMBER)
  RETURN BOOLEAN IS
    nColumnFooterHeight NUMBER:=0;
    nPageFooterheight   NUMBER:=0;
  BEGIN
    nColumnFooterHeight:=0;
    nPageFooterheight:=0;
    FOR i IN 1..lReportStack.COUNT LOOP
      nColumnFooterHeight:=nColumnFooterHeight+lReportStack(i).nColumnFtrHeight;
      nPageFooterheight  :=nPageFooterheight  +lReportStack(i).nPageFtrHeight;
    END LOOP;

    IF rPageSetup.nPageHeight
       -rPageSetup.nTopMargin
       -i_nCurrentY
       -i_nDetailHeight
       -nColumnFooterHeight
       -nPageFooterheight
       -rPageSetup.nBottomMargin>=0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_REST_OF_PAGE(i_nCurrentY IN NUMBER)
  RETURN NUMBER IS
    nColumnFooterHeight NUMBER:=0;
    nPageFooterheight   NUMBER:=0;
  BEGIN
    nColumnFooterHeight:=0;
    nPageFooterheight:=0;
    FOR i IN 1..lReportStack.COUNT LOOP
      nColumnFooterHeight:=nColumnFooterHeight+lReportStack(i).nColumnFtrHeight;
      nPageFooterheight  :=nPageFooterheight  +lReportStack(i).nPageFtrHeight;
    END LOOP;

    RETURN rPageSetup.nPageHeight
           -rPageSetup.nTopMargin
           -i_nCurrentY
           -nColumnFooterHeight
           -nPageFooterheight
           -rPageSetup.nBottomMargin;
  END;

  -- ----------------------------------------------------------------------

  FUNCTION FK_RENDER_HTML(i_nX               IN NUMBER,
                          i_nY               IN NUMBER,
                          i_nWidth           IN NUMBER,
                          i_nHeight          IN NUMBER,
                          i_vcText           IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                          i_vcFont           IN PK_JRXML2PDF_TYPES.tFont,
                          i_nFontSize        IN NUMBER,
                          i_vcFontStyle      IN PK_JRXML2PDF_TYPES.tFontStyle,
                          i_vcAlignment      IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcVerticalAlign  IN PK_JRXML2PDF_TYPES.tAlignment,
                          i_vcBgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcFgColor        IN PK_JRXML2PDF_TYPES.tColor,
                          i_nBoxTop          IN NUMBER,
                          i_nBoxLeft         IN NUMBER,
                          i_nBoxBottom       IN NUMBER,
                          i_nBoxRight        IN NUMBER,
                          i_nTopPadding      IN NUMBER,
                          i_nLeftPadding     IN NUMBER,
                          i_nBottomPadding   IN NUMBER,
                          i_nRightPadding    IN NUMBER,
                          i_vcBoxTopColor    IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxLeftColor   IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxBottomColor IN PK_JRXML2PDF_TYPES.tColor,
                          i_vcBoxRightColor  IN PK_JRXML2PDF_TYPES.tColor,
                          i_nLineSpacing     IN NUMBER,
                          i_vcOpaque         IN PK_JRXML2PDF_TYPES.tYesNo,
                          i_rStyle           IN PK_JRXML2PDF_TYPES.tSimpleStyle,
                          i_bMayPageBreak    IN BOOLEAN,
                          i_rArea            IN PK_JRXML2PDF_TYPES.tArea,
                          i_bEnhanced        IN BOOLEAN)
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    rArea         PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    lResult       PK_JRXML2PDF_TYPES.tHtmlTree;
    nBaseY        NUMBER;
    nX            NUMBER;
    nXStart       NUMBER;
    nY            NUMBER;
    nWidth        NUMBER;
    nOutputHeight NUMBER;
    nGroup        NUMBER:=1;
    lGroupHeight  PK_JRXML2PDF_TYPES.tNumList;
    bInList       BOOLEAN;
    nDummy        NUMBER;
    vcLastTag     PK_JRXML2PDF_TYPES.tAttribute;
    nMaxheight    NUMBER;
    nRestOfPage   NUMBER;
    bTrackY       BOOLEAN:=FALSE;
    nTotalHeight  NUMBER;
    rHtmlSettings PK_JRXML2PDF_TYPES.tHtmlSettings:=rMasterHtmlSettings;

    PROCEDURE PR_RENDER_BOX_FOR_TEXT(i_nY      IN NUMBER,
                                     i_nHeight IN NUMBER) IS
    BEGIN
      PR_RENDER_BOX(i_nX               =>i_nX,
                    i_nY               =>i_nY,
                    i_nWidth           =>i_nWidth,
                    i_nHeight          =>i_nHeight,
                    i_vcBgColor        =>i_vcBgColor,
                    i_vcFgColor        =>i_vcFgColor,
                    i_nBoxTop          =>i_nBoxTop,
                    i_nBoxLeft         =>i_nBoxLeft,
                    i_nBoxBottom       =>i_nBoxBottom,
                    i_nBoxRight        =>i_nBoxRight,
                    i_vcBoxTopColor    =>i_vcBoxTopColor,
                    i_vcBoxLeftColor   =>i_vcBoxLeftColor,
                    i_vcBoxBottomColor =>i_vcBoxBottomColor,
                    i_vcBoxRightColor  =>i_vcBoxRightColor,
                    i_vcOpaque         =>i_vcOpaque,
                    i_rStyle           =>i_rStyle);
    END;

    PROCEDURE PR_REKU_OUTPUT_HTML(i_rPiece IN PK_JRXML2PDF_TYPES.tHtmlPiece, i_nLevel IN NUMBER) IS
      nSpacing     NUMBER;
      nYToAdd      NUMBER;
      FUNCTION FK_GET_LINE_SPACING
      RETURN NUMBER IS
        nResult NUMBER;
      BEGIN
        IF vcLastTag IS NULL THEN
          nResult:=0;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H1 THEN
          nResult:=rHtmlSettings.nH1LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H2 THEN
          nResult:=rHtmlSettings.nH2LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H3 THEN
          nResult:=rHtmlSettings.nH3LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_H4 THEN
          nResult:=rHtmlSettings.nH4LineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_UL THEN
          nResult:=rHtmlSettings.nBrLineSpacing;
        ELSIF i_rPiece.vcTag=PK_JRXML2PDF_TYPES.HTML_LI THEN
          IF vcLastTag!=PK_JRXML2PDF_TYPES.HTML_LI THEN
            nResult:=rHtmlSettings.nH4LineSpacing;
          ELSE
            nResult:=rHtmlSettings.nLiLineSpacing;
          END IF;
        ELSIF i_rPiece.vcTag IN (PK_JRXML2PDF_TYPES.HTML_BR, PK_JRXML2PDF_TYPES.HTML_P) THEN
          nResult:=rHtmlSettings.nBrLineSpacing;
        ELSE
          nResult:=rHtmlSettings.nLineSpacing;
        END IF;
        vcLastTag:=i_rPiece.vcTag;
        RETURN nResult;
      END;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('>'|| RPAD(' ', i_nLevel*2, ' ') || i_rPiece.vcTag ||':' || i_rPiece.vcText || ':' || i_rPiece.vcColor);
      END IF;
      -- set color
      AS_PDF3_MOD.set_color(i_rPiece.vcColor);
      IF i_rPiece.vcText IS NOT NULL THEN
        -- set font
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                      i_vcStyle =>i_rPiece.vcFontStyle,
                                      i_nSize   =>i_rPiece.nFontSize
                                     );

        -- write text
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('write x/y/text' || i_rPiece.nx || '/' || ny || '/[' || i_rPiece.vcText || ']');
        END IF;
        FOR i IN 1..i_rPiece.lTextPieces.COUNT LOOP
          IF nGroup!=i_rPiece.lTextPieces(i).nGroup THEN
            nGroup:=i_rPiece.lTextPieces(i).nGroup;
            IF i=1 THEN
              -- big spacing
              nSpacing:=FK_GET_LINE_SPACING;
            ELSE
              -- smallm spacing
              nSpacing:=rHtmlSettings.nLineSpacing;
            END IF;
            nYToAdd:=lGroupHeight(i_rPiece.lTextPieces(i).nGroup);
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('GRUPPE/y ' || nGroup || '/' || ny);
            END IF;
          ELSE
            nYToAdd:=0;
            nSpacing:=0;
          END IF;
          EXIT WHEN     nOutputHeight+nYToAdd+nSpacing>i_nHeight
                    AND NOT i_bMayPageBreak;

          IF nOutputHeight+nYToAdd+nSpacing>nRestOfPage THEN
            -- new page
            rArea.nY:=nY;
            PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                         i_nStackPos     =>lReportStack.COUNT,
                                         i_bOneRecordBack=>FALSE);
            nY:=rArea.nY;
            -- reduce height
            nTotalHeight:=nTotalHeight-nOutputheight-nSpacing;
            -- after a page-break, the new Y-positin must be tracked and returned
            bTrackY:=TRUE;
            nOutputHeight:=nYToAdd;
            nRestOfPage:=FK_REST_OF_PAGE(i_nY);
            -- render box
            PR_RENDER_BOX_FOR_TEXT(i_nY     =>nY,
                                   i_nHeight=>LEAST(nTotalHeight, nRestOfPage)
                                  );
            -- re-set Color and font, as it may have changed during header-rendering
            AS_PDF3_MOD.set_color(i_rPiece.vcColor);
            -- set font
            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                          i_vcStyle =>i_rPiece.vcFontStyle,
                                          i_nSize   =>i_rPiece.nFontSize
                                         );

            -- we have to remember y-value at the end
            bTrackY:=TRUE;
            ny:=ny+nYToAdd;
          ELSE
            ny:=ny+nYToAdd+nSpacing;
            nOutputHeight:=nOutputHeight+nYToAdd+nSpacing;
          END IF;
          IF i=1 THEN
            IF i_rPiece.vcTag =PK_JRXML2PDF_TYPES.HTML_LI THEN
              IF i_rPiece.nListindex>0 THEN
                PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                              i_vcStyle =>i_rPiece.vcFontStyle,
                                              i_nSize   =>i_rPiece.nFontSize
                                             );
                as_pdf3_mod.put_txt(i_rPiece.nx-15,
                                    nBaseY-ny,
                                    TO_CHAR(i_rPiece.nListindex, 'FM990') || '.'
                                   );
              ELSE
                PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>PK_JRXML2PDF_TYPES.WINGDINGS,
                                              i_vcStyle =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                              i_nSize   =>6
                                             );
                as_pdf3_mod.put_txt(i_rPiece.nx-15,
                                    nBaseY-ny+2,
                                    rHtmlSettings.vcUlChar
                                   );
                PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                              i_vcStyle =>i_rPiece.vcFontStyle,
                                              i_nSize   =>i_rPiece.nFontSize
                                             );
              END IF;
            END IF;
          END IF;
          as_pdf3_mod.put_txt(i_rPiece.lTextPieces(i).nX,
                              nBaseY-ny,
                              i_rPiece.lTextPieces(i).vcText
                             );
        END LOOP;
      ELSE
        IF nGroup!=i_rPiece.nGroup THEN
          nGroup:=i_rPiece.nGroup;
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Group/y ' || nGroup || '/' || ny);
          END IF;
          -- big spacing
          nSpacing:=FK_GET_LINE_SPACING;
          nYToAdd:=lGroupHeight(i_rPiece.nGroup);
        ELSE
          nYToAdd:=0;
          nSpacing:=0;
        END IF;
        IF nOutputHeight+nYToAdd+nSpacing>nRestOfPage THEN
          -- new page
          rArea.nY:=nY;
          PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                       i_nStackPos     =>lReportStack.COUNT,
                                       i_bOneRecordBack=>FALSE);
          nY:=rArea.nY;
          -- reduce height
          nTotalHeight:=nTotalHeight-nOutputheight-nSpacing;
          -- after a page-break, the new Y-positin must be tracked and returned
          bTrackY:=TRUE;
          nOutputHeight:=nYToAdd;
          nRestOfPage:=FK_REST_OF_PAGE(i_nY);

          -- render box
          PR_RENDER_BOX_FOR_TEXT(i_nY     =>nY,
                                 i_nHeight=>LEAST(nTotalHeight, nRestOfPage)
                                );
          -- re-set Color and font, as it may have changed during header-rendering
          AS_PDF3_MOD.set_color(i_rPiece.vcColor);
          -- set font
          PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                        i_vcStyle =>i_rPiece.vcFontStyle,
                                        i_nSize   =>i_rPiece.nFontSize
                                       );
          -- we have to remember y-value at the end
          bTrackY:=TRUE;
          ny:=ny+nYToAdd;
        ELSE
          ny:=ny+nYToAdd+nSpacing;
          nOutputHeight:=nOutputHeight+nYToAdd+nSpacing;
        END IF;
        IF     i_rPiece.vcTag =PK_JRXML2PDF_TYPES.HTML_LI
           AND nOutputHeight<=i_nHeight THEN
          IF i_rPiece.nListindex>0 THEN
            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                          i_vcStyle =>i_rPiece.vcFontStyle,
                                          i_nSize   =>i_rPiece.nFontSize
                                         );
            as_pdf3_mod.put_txt(i_rPiece.nx-15,
                                nBaseY-ny,
                                TO_CHAR(i_rPiece.nListindex, 'FM990') || '.'
                               );
          ELSE
            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>PK_JRXML2PDF_TYPES.WINGDINGS,
                                          i_vcStyle =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                          i_nSize   =>6
                                         );
            as_pdf3_mod.put_txt(i_rPiece.nx-15,
                                nBaseY-ny ,
                                rHtmlSettings.vcUlChar
                               );
            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont  =>i_rPiece.vcFont,
                                          i_vcStyle =>i_rPiece.vcFontStyle,
                                          i_nSize   =>i_rPiece.nFontSize
                                         );
          END IF;
        END IF;
      END IF;
      FOR i IN 1..i_rPiece.lSubPieces.COUNT LOOP
        PR_REKU_OUTPUT_HTML(lResult.lPieces(i_rPiece.lSubPieces(i)), i_nLevel+1);

        EXIT WHEN         nOutputHeight>i_nHeight
                  AND NOT i_bMayPageBreak;
      END LOOP;
    END;

  BEGIN
    -- basic font and color are taken from the input values
    rHtmlSettings.vcFont   :=COALESCE(i_vcFont, i_rStyle.vcFont, rMasterHtmlSettings.vcFont);
    rHtmlSettings.nFontSize:=COALESCE(i_nFontSize, i_rStyle.nFontSize, rMasterHtmlSettings.nFontSize);
    IF NOT i_bEnhanced THEN
      -- no enhanced mode for HTML, no rendering of H1, H2, H3, H4
      rHtmlSettings.nH1FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH2FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH3FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH4FontSize   :=rHtmlSettings.nFontSize;
      rHtmlSettings.nH1LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH2LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH3LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nH4LineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nLiLineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nBrLineSpacing:=rHtmlSettings.nLineSpacing;
      rHtmlSettings.nLineSpacing  :=rHtmlSettings.nLineSpacing;
    END IF;


    nBaseY:=rPageSetup.nPageHeight
            -rPageSetup.nTopMargin
            -COALESCE(i_nTopPadding, i_rStyle.nTopPadding, 0);

    nXStart:=rPageSetup.nLeftMargin+i_nX+COALESCE(i_nLeftPadding, i_rStyle.nLeftPadding, 0);

    nY:=i_nY;

    nMaxheight:=FK_CALC_HTML_HEIGHT(i_nX              =>i_nX,
                                    i_nY              =>i_nY,
                                    i_nWidth          =>i_nWidth,
                                    i_nHeight         =>i_nHeight,
                                    i_vcText          =>i_vcText,
                                    i_vcFont          =>i_vcFont,
                                    i_nFontSize       =>i_nFontSize,
                                    i_vcAlignment     =>i_vcAlignment,
                                    i_nTopPadding     =>i_nTopPadding,
                                    i_nLeftPadding    =>i_nLeftPadding,
                                    i_nBottomPadding  =>i_nBottomPadding,
                                    i_nRightPadding   =>i_nRightPadding,
                                    i_nLineSpacing    =>i_nLineSpacing,
                                    i_rStyle          =>i_rStyle,
                                    i_bEnhanced       =>i_bEnhanced);

    lResult:=FK_TOKENIZE_HTML(i_nFontSize=>rHtmlSettings.nFontSize,
                              i_vcText   =>i_vcText,
                              i_vcColor  =>COALESCE(i_vcFgColor, i_rStyle.vcFgColor, PK_JRXML2PDF_TYPES.BLACK));

    nWidth:=i_nWidth;
    -- initally scale all parts
    PR_LAYOUT_HTML(i_rHtmlSetting=>rHtmlSettings,
                   io_lHtml      =>lResult,
                   i_nX          =>nXStart,
                   i_nWidth      =>i_nWidth,
                   i_vcColor     =>COALESCE(i_vcFgColor, i_rStyle.vcFgColor, PK_JRXML2PDF_TYPES.BLACK)
                  );
    -- for each "line" get the maximum height
    FOR i IN 1..lResult.lPieces.COUNT LOOP
      FOR j IN 1..lResult.lPieces(i).lTextPieces.COUNT LOOP
        IF NOT lGroupHeight.EXISTS(lResult.lPieces(i).lTextPieces(j).nGroup) THEN
          lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup):=lResult.lPieces(i).lTextPieces(j).nHeight;
        ELSE
          lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup):=GREATEST(lGroupHeight(lResult.lPieces(i).lTextPieces(j).nGroup), lResult.lPieces(i).lTextPieces(j).nHeight);
        END IF;
      END LOOP;
      IF NOT lGroupHeight.EXISTS(lResult.lPieces(i).nGroup) THEN
        lGroupHeight(lResult.lPieces(i).nGroup):=NVL(lResult.lPieces(i).nHeight,0);
      ELSE
        lGroupHeight(lResult.lPieces(i).nGroup):=GREATEST(lGroupHeight(lResult.lPieces(i).nGroup), NVL(lResult.lPieces(i).nHeight,0));
      END IF;
    END LOOP;
    nTotalHeight:=nMaxHeight;

    nRestOfPage:=FK_REST_OF_PAGE(i_nY);

    PR_RENDER_BOX_FOR_TEXT(i_nY     =>i_nY,
                           i_nHeight=>LEAST(i_nHeight, nRestOfPage)
                          );
    nGroup:=0;
    nOutputHeight:=0;
    vcLastTag:=NULL;
    PR_REKU_OUTPUT_HTML(lResult.lPieces(1),1);
    IF bTrackY THEN
      rArea.nY:=nY;
    END IF;
    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_QUERY_RESULT(i_nStackPos     IN NUMBER,
                               i_nRecordOffset IN NUMBER
                              )
  RETURN PK_JRXML2PDF_TYPES.tDataEntries IS
    lResult PK_JRXML2PDF_TYPES.tDataEntries;
    nIndex  NUMBER;
  BEGIN
    IF i_nRecordOffset=PK_JRXML2PDF_TYPES.OFFSET_NONE THEN
      IF lReportStack(i_nStackPos).rQuery.bTreatLastAsCurrent THEN
        IF lReportStack(i_nStackPos).rQuery.nPreviousRecord IS NOT NULL THEN
          nIndex:=lReportStack(i_nStackPos).rQuery.nPreviousRecord;
        ELSE
          nIndex:=lReportStack(i_nStackPos).rQuery.nCurrentRecord;
        END IF;
      ELSE
        nIndex:=lReportStack(i_nStackPos).rQuery.nCurrentRecord;
      END IF;
    ELSIF i_nRecordOffset=PK_JRXML2PDF_TYPES.OFFSET_PREVIOUS THEN
      IF lReportStack(i_nStackPos).rQuery.bTreatLastAsCurrent THEN
        nIndex:=-1;
      ELSE
        nIndex:=lReportStack(i_nStackPos).rQuery.nPreviousRecord;
      END IF;
    ELSIF i_nRecordOffset=PK_JRXML2PDF_TYPES.OFFSET_NEXT THEN
      IF lReportStack(i_nStackPos).rQuery.bTreatLastAsCurrent THEN
        nIndex:=lReportStack(i_nStackPos).rQuery.nCurrentRecord;
      ELSE
        nIndex:=lReportStack(i_nStackPos).rQuery.nNextRecord;
      END IF;
    END IF;
    BEGIN
      lResult:=lReportStack(i_nStackPos).rQuery.lQueryResult(nIndex);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
    END;

    RETURN lResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_EXPRESSION_INT(i_nStackPos      IN NUMBER,
                                      i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                      i_vcPattern      IN PK_JRXML2PDF_TYPES.tPattern,
                                      i_bForSqlUsage   IN BOOLEAN,
                                      i_nRecordOffset  IN NUMBER)
  RETURN VARCHAR2 IS
    lTokenValues PK_JRXML2PDF_TYPES.tTokenValueList;
    vcResult     PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rEntry       PK_JRXML2PDF_TYPES.tDataEntry;

    vcVarName    PK_JRXML2PDF_TYPES.tAttribute;
    iPos         PLS_INTEGER;
    iEndPos      PLS_INTEGER;
    iLastPos     PLS_INTEGER;

    PROCEDURE PR_TOKENIZE IS
      iPos      PLS_INTEGER:=1;
      iFoundPos PLS_INTEGER;
      iToken    PLS_INTEGER;
    BEGIN
      LOOP
        EXIT WHEN iPos>LENGTH(i_vcExpression);
        iFoundPos:=99999;
        FOR i IN 1..lTokens.COUNT LOOP
          iToken:=INSTR(i_vcExpression, lTokens(i).vcStartTag, iPos);
          IF iToken>0 THEN
            iFoundPos:=LEAST(iFoundPos, iToken);

            lTokenValues(iToken).vcFoundTag:=lTokens(i).vcStartTag;
            lTokenValues(iToken).vcEndTag:=lTokens(i).vcEndTag;
            iToken:=INSTR(i_vcExpression, lTokens(i).vcEndTag, iToken+1);
            IF iToken>0 THEN
              lTokenValues(iToken).vcFoundTag:=lTokens(i).vcEndTag;
            END IF;
          END IF;
        END LOOP;
        iPos:=iFoundPos+1;
      END LOOP;
    END;

    FUNCTION FK_DATE_PATTERN(i_vcPattern IN PK_JRXML2PDF_TYPES.tPattern)
    RETURN VARCHAR2 IS
      vcPattern PK_JRXML2PDF_TYPES.tPattern;
      PROCEDURE PR_REDEFINE_SHORTCUTS IS
        vcdatePattern PK_JRXML2PDF_TYPES.tPattern;
        vcTimePattern PK_JRXML2PDF_TYPES.tPattern;
      BEGIN
        IF i_vcPattern='short' THEN
          vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateShort;
          vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeShort;
        ELSIF i_vcPattern='medium' THEN
          vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateMedium;
          vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeMedium;
        ELSIF i_vcPattern='long' THEN
          vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateLong;
          vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeLong;
        ELSIF i_vcPattern='default' THEN
          vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateDefault;
          vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeDefault;
        ELSE
          IF INSTR(i_vcPattern, 'short,')>0 THEN
            vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateShort;
          ELSIF INSTR(i_vcPattern, 'medium,')>0 THEN
            vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateMedium;
          ELSIF INSTR(i_vcPattern, 'long,')>0 THEN
            vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateLong;
          ELSIF INSTR(i_vcPattern, 'default,')>0 THEN
            vcDatePattern:=lReportStack(i_nStackPos).rLocaleData.vcDateDefault;
          END IF;
          IF INSTR(i_vcPattern, ',short')>0 THEN
            vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeShort;
          ELSIF INSTR(i_vcPattern, ',medium')>0 THEN
            vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeMedium;
          ELSIF INSTR(i_vcPattern, ',long')>0 THEN
            vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeLong;
          ELSIF INSTR(i_vcPattern, ',default')>0 THEN
            vcTimePattern:=lReportStack(i_nStackPos).rLocaleData.vcTimeDefault;
          END IF;
        END IF;
        IF    vcDatePattern IS NOT NULL
           OR vcTimePattern IS NOT NULL THEN
          -- convert, LTR or RTL
          vcPattern:=PK_JRXML2PDF_UTIL.FK_TEXT_MESSAGE(lReportStack(i_nStackPos).rLocaleData.vcDateTimeString,
                                                       vcTimePattern,
                                                       vcDatePattern
                                                      );
        END IF;
      END;
    BEGIN
      IF NOT lDatePattern.EXISTS(i_vcPattern) THEN
        -- handle special pattern, like short,short
        PR_REDEFINE_SHORTCUTS;
        IF vcPattern IS NOT NULL THEN
          -- use predefined pattern
          lDatePattern(i_vcPattern):=vcPattern;
        ELSE
          -- create the oracle-format-mask
          lDatePattern(i_vcPattern):=PK_JRXML2PDF_UTIL.FK_ORA_DATE_PATTERN(i_vcPattern);
        END IF;
      END IF;
      -- now its parsed, simple return
      RETURN lDatePattern(i_vcPattern);
    END;

    FUNCTION FK_GET_FIELD_VALUE(i_vcFieldName IN VARCHAr2,
                                i_nStackPos   IN NUMBER)
    RETURN VARCHAR2 IS
      vcValue    PK_JRXML2PDF_TYPES.tMaxVarchar2;
      lDataEntry PK_JRXML2PDF_TYPES.tDataEntries;
    BEGIN
      lDataEntry:=FK_GET_QUERY_RESULT(i_nStackPos    =>i_nStackPos,
                                      i_nRecordOffset=>i_nRecordOffset
                                     );
      BEGIN
        rEntry:=lDataEntry(UPPER(i_vcFieldName));
        IF rEntry.vcDataType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF i_bForSqlUsage THEN
            IF rEntry.nData IS NOT NULL THEN
              vcValue:=TO_CHAR(rEntry.nData, PK_JRXML2PDF_TYPES.NUM_FORMAT_MASK, 'NLS_NUMERIC_CHARACTERS=.,');
            ELSE
              vcValue:='NULL';
            END IF;
          ELSE
            IF i_vcPattern IS NOT NULL THEN
              BEGIN
                vcValue:=TO_CHAR(rEntry.nData,FK_NUM_PATTERN(i_vcPattern), lReportStack(i_nStackPos).rLocaleData.vcNumericChars || lReportStack(i_nStackPos).rLocaleData.vcCurrency);
              EXCEPTION
                WHEN OTHERS THEN
                  -- format error
                  vcValue:='Format error :' || FK_NUM_PATTERN(i_vcPattern);
              END;
            ELSE
              IF rEntry.ndata=TRUNC(rEntry.ndata) THEN
                vcValue:=TO_CHAR(rEntry.nData, PK_JRXML2PDF_TYPES.DEFAULT_INT_FORMAT);--, lReportStack(i_nStackPos).rLocaleData.vcNumericChars);
              ELSE
                vcValue:=TO_CHAR(rEntry.nData, PK_JRXML2PDF_TYPES.DEFAULT_FLOAT_FORMAT);--, lReportStack(i_nStackPos).rLocaleData.vcNumericChars);
              END IF;
            END IF;
          END IF;
        ELSIF rEntry.vcDataType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          IF i_bForSqlUsage THEN
            IF rEntry.dtData IS NOT NULL THEN
              vcValue:='TO_DATE(''' || TO_CHAR(rEntry.dtData, PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK) || ''', ''' || PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK || ''')';
            ELSE
              vcValue:='NULL';
            END IF;
          ELSE
            IF i_vcPattern IS NOT NULL THEN
              BEGIN
                vcValue:=TO_CHAR(rEntry.dtData,
                                 FK_DATE_PATTERN(i_vcPattern),
                                 lReportStack(i_nStackPos).rLocaleData.vcDateLanguage
                                );
              EXCEPTION
                WHEN OTHERS THEN
                  -- format error
                  vcValue:='Format error :' || i_vcPattern;
              END;
            ELSE
              vcValue:=TO_CHAR(rEntry.dtData,
                               lReportStack(i_nStackPos).rLocaleData.vcDateShort || ' ' ||
                               lReportStack(i_nStackPos).rLocaleData.vcTimeShort,
                               lReportStack(i_nStackPos).rLocaleData.vcDateLanguage);
            END IF;
          END IF;
        ELSIF rEntry.vcDataType=PK_JRXML2PDF_TYPES.DATATYPE_BLOB THEN
          vcValue:=PK_JRXML2PDF_TYPES.THIS_IS_A_BLOB;
        ELSE
          IF i_bForSqlUsage THEN
            IF rEntry.vcData IS NOT NULL THEN
              vcValue:='''' || REPLACE(rEntry.vcData, '''', '''''') || '''';
            ELSE
              vcValue:='NULL';
            END IF;
          ELSE
            vcValue:=rEntry.vcData;
          END IF;
        END IF;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          IF i_nStackPos>1 THEN
            -- try on master-report
            vcValue:=FK_GET_FIELD_VALUE(i_vcFieldName=>i_vcFieldName,
                                        i_nStackPos  =>i_nStackPos-1);
          END IF;
      END;
      RETURN vcValue;
    END;

    FUNCTION FK_GET_RESOURCE_VALUE(i_vcResName IN VARCHAR2)
    RETURN VARCHAR2 IS
      vcValue    PK_JRXML2PDF_TYPES.tMaxVarchar2;
      vcResource PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      IF lReportStack(i_nStackPos).lResources.EXISTS(i_vcResName) THEN
        vcResource:=lReportStack(i_nStackPos).lResources(i_vcResName);
      END IF;
      IF i_bForSqlUsage THEN
        vcValue:='''' || vcResource || '''';
      ELSE
        vcValue:=vcResource;
      END IF;
      RETURN vcValue;
    END;

    FUNCTION FK_GET_VAR_VALUE(i_vcVarName IN VARCHAr2)
    RETURN VARCHAR2 IS
      vcValue PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      IF i_vcVarname='PAGE_NUMBER' THEN
        vcValue:=TO_CHAR(lReportStack(i_nStackPos).lLogicalPageNumbers(lReportStack(i_nStackPos).nCurrentPagePointer));
      ELSIF i_vcVarname='REPORT_COUNT' THEN
        vcValue:=TO_CHAR(lReportStack(i_nStackPos).rQuery.nRecordPosition);
      ELSIF i_vcVarname IN ('SYSDATE') THEN
        IF i_bForSqlUsage THEN
          vcValue:='TO_DATE(''' || TO_CHAR(SYSDATE, PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK) || ''', ''' || PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK || ''')';
        ELSE
          IF i_vcPattern IS NOT NULL THEN
            BEGIN
              vcValue:=TO_CHAR(SYSDATE,FK_DATE_PATTERN(i_vcPattern), lReportStack(i_nStackPos).rLocaleData.vcDateLanguage);
            EXCEPTION
              WHEN OTHERS THEN
                -- format error
                vcValue:='Format error :' || i_vcPattern;
            END;
          ELSE
            vcValue:=TO_CHAR(SYSDATE, lReportStack(i_nStackPos).rLocaleData.vcDateShort || ' ' ||
                                      lReportStack(i_nStackPos).rLocaleData.vcTimeShort
                                    , lReportStack(i_nStackPos).rLocaleData.vcDateLanguage);
          END IF;
        END IF;
      ELSE
        -- Check reports variable cache
        IF lReportStack(i_nStackPos).lVariableCache.EXISTS(UPPER(i_vcVarname)) THEN
          IF lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
            IF i_bForSqlUsage THEN
              IF lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData IS NOT NULL THEN
                vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData, PK_JRXML2PDF_TYPES.NUM_FORMAT_MASK, 'NLS_NUMERIC_CHARACTERS=.,');
              ELSE
                vcValue:='NULL';
              END IF;
            ELSE
              IF i_vcPattern IS NOT NULL THEN
                BEGIN
                  vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData,
                                   FK_NUM_PATTERN(i_vcPattern),
                                   lReportStack(i_nStackPos).rLocaleData.vcNumericChars || ' ' ||
                                   lReportStack(i_nStackPos).rLocaleData.vcCurrency
                                  );
                EXCEPTION
                  WHEN OTHERS THEN
                    -- format error
                    vcValue:='Format error :' || i_vcPattern;
                END;
              ELSE
                IF lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData=TRUNC(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData) THEN
                  vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData, PK_JRXML2PDF_TYPES.DEFAULT_INT_FORMAT, lReportStack(i_nStackPos).rLocaleData.vcNumericChars);
                ELSE
                  vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).nData, PK_JRXML2PDF_TYPES.DEFAULT_FLOAT_FORMAT, lReportStack(i_nStackPos).rLocaleData.vcNumericChars);
                END IF;
              END IF;
            END IF;
          ELSIF lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
            IF i_bForSqlUsage THEN
              vcValue:='TO_DATE(''' || TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).dtData, PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK) || ''', ''' || PK_JRXML2PDF_TYPES.DATE_FORMAT_MASK || ''')';
            ELSE
              IF i_vcPattern IS NOT NULL THEN
                BEGIN
                  vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).dtData,
                                   FK_DATE_PATTERN(i_vcPattern),
                                   lReportStack(i_nStackPos).rLocaleData.vcDateLanguage
                                  );
                EXCEPTION
                  WHEN OTHERS THEN
                    -- format error
                    vcValue:='Format error :' || i_vcPattern;
                END;
              ELSE
                vcValue:=TO_CHAR(lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).dtData,
                                 lReportStack(i_nStackPos).rLocaleData.vcDateShort || ' ' ||
                                 lReportStack(i_nStackPos).rLocaleData.vcTimeShort,
                                 lReportStack(i_nStackPos).rLocaleData.vcDateLanguage
                                );
              END IF;
            END IF;
          ELSE
            vcValue:=lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcVarname)).vcData;
          END IF;

        END IF;
      END IF;
      RETURN vcValue;
    END;

    FUNCTION FK_GET_PARAM_VALUE(i_vcParamName IN VARCHAr2)
    RETURN VARCHAR2 IS
      vcValue PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      -- special handling for SUBREPORT_DIR, always return ''
      IF i_vcParamName!='SUBREPORT_DIR' THEN
        FOR i IN 1..lReportStack(i_nStackPos).lParams.COUNT LOOP
          IF lReportStack(i_nStackPos).lParams(i).vcName=i_vcParamName THEN
            IF i_bForSqlUsage THEN
              IF lReportStack(i_nStackPos).lParams(i).vcValue IS NOT NULL THEN
                vcValue:='''' || REPLACE(lReportStack(i_nStackPos).lParams(i).vcValue, '''', '''''') || '''';
              ELSE
                vcValue:='NULL';
              END IF;
            ELSE
              vcValue:=lReportStack(i_nStackPos).lParams(i).vcValue;
            END IF;
            EXIT;
          END IF;
        END LOOP;
      END IF;
      IF     i_bForSqlUsage
         AND vcValue IS NULL THEN
        vcValue:='NULL';
      END IF;
      RETURN vcValue;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Evaluate expression:' || i_vcExpression);
    END IF;
    IF i_vcExpression IS NOT NULL THEN
      -- for performance issues, check the most happeing case explicitly
      IF SUBSTR(i_vcExpression, 1, 3)='$F{'
         AND INSTR(i_vcExpression, '}')=LENGTH(i_vcExpression) THEN
        -- a simple field
        -- String, match end-Token
        vcVarName:=SUBSTR(i_vcExpression,4, LENGTH(i_vcExpression)-4);

        vcResult:=vcResult || FK_GET_FIELD_VALUE(vcVarName, i_nStackPos);

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Fast field,fieldname/result:' || vcVarname || '/' || vcResult);
        END IF;
      ELSE
        PR_TOKENIZE;
        IF lTokenValues.COUNT>0 THEN
          iLastPos:=1;
          iPos:=lTokenValues.FIRST;
          WHILE iPos IS NOT NULL LOOP

            IF i_bForSqlUsage AND iPos>iLastPos THEN
              -- take value from gap and add
              vcResult:=vcResult||SUBSTR(i_vcExpression, iLastPos,iPos-iLastPos);
            END IF;
            iEndPos:=iPos;
            iLastPos:=iPos+LENGTH(lTokenValues(iPos).vcFoundTag);
            IF lTokenValues(iPos).vcEndTag IS NOT NULL THEN
              iEndPos:=lTokenValues.NEXT(iEndPos);
              WHILE iEndPos IS NOT NULL LOOP
                EXIT WHEN lTokenValues(iEndPos).vcFoundTag=lTokenValues(iPos).vcEndTag;
                iEndPos:=lTokenValues.NEXT(iEndPos);
              END LOOP;
              iLastPos:=iEndPos+1;
            END IF;
            IF     iPos IS NOT NULL
               AND iEndPos IS NOT NULL THEN
              IF lTokenValues(iPos).vcFoundTag='"' THEN
                -- String, match end-Token
                IF i_bForSqlUsage THEN
                  vcResult:=vcResult || '''' || REPLACE(SUBSTR(i_vcExpression,iPos+1, iEndPos-iPos-1), '''', '''''') || '''';
                ELSE
                  vcResult:=vcResult || SUBSTR(i_vcExpression,iPos+1, iEndPos-iPos-1);
                END IF;

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add string,result:' || vcResult);
                END IF;

              ELSIF lTokenValues(iPos).vcFoundTag='$F{' THEN
                -- String, match end-Token
                vcVarName:=SUBSTR(i_vcExpression,iPos+3, iEndPos-iPos-3);

                vcResult:=vcResult || FK_GET_FIELD_VALUE(vcVarName, i_nStackPos);

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add field,fieldname/result:' || vcVarname || '/' || vcResult);
                END IF;
              ELSIF lTokenValues(iPos).vcFoundTag='$V{' THEN
                -- String, match end-Token
                vcVarName:=SUBSTR(i_vcExpression,iPos+3, iEndPos-iPos-3);

                vcResult:=vcResult || FK_GET_VAR_VALUE(vcVarName);

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add variable,varname/result:' || vcVarname || '/' || vcResult);
                END IF;
              ELSIF lTokenValues(iPos).vcFoundTag='$P{' THEN
                -- String, match end-Token
                vcVarName:=SUBSTR(i_vcExpression,iPos+3, iEndPos-iPos-3);
                vcResult:=vcResult || FK_GET_PARAM_VALUE(vcVarName);

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add parameter,parname/result:' || vcVarname || '/' || vcResult);
                END IF;
              ELSIF lTokenValues(iPos).vcFoundTag='$R{' THEN
                -- String, match end-Token
                vcVarName:=SUBSTR(i_vcExpression,iPos+3, iEndPos-iPos-3);

                vcResult:=vcResult || FK_GET_RESOURCE_VALUE(vcVarName);

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add resource,name/result:' || vcVarname || '/' || vcResult);
                END IF;
              ELSIF lTokenValues(iPos).vcFoundTag='new java.util.Date()' THEN
                -- String, match end-Token
                vcVarName:='SYSDATE';
                vcResult:=vcResult || FK_GET_VAR_VALUE(vcVarName);

                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add java.util.date,vresult:' || vcResult);
                END IF;
              ELSIF i_bForSqlUsage THEN
                -- Other tokens only when used in SQL
                IF lTokenValues(iPos).vcFoundTag='==' THEN
                  vcResult:=vcResult || '=';
                ELSIF lTokenValues(iPos).vcFoundTag='==null' THEN
                  vcResult:=vcResult || ' IS NULL ';
                ELSIF lTokenValues(iPos).vcFoundTag='!=null' THEN
                  vcResult:=vcResult || ' IS NOT NULL ';
                ELSIF lTokenValues(iPos).vcFoundTag='||' THEN
                  vcResult:=vcResult || ' OR ';
                ELSIF lTokenValues(iPos).vcFoundTag='&&' THEN
                  vcResult:=vcResult || ' AND ';
                ELSIF lTokenValues(iPos).vcFoundTag='jrxml2pdf.Wrappers.mod' THEN
                  vcResult:=vcResult || ' MOD ';
                ELSIF lTokenValues(iPos).vcFoundTag='.startsWith(' THEN
                  -- String, match end-Token
                  vcVarName:=REPLACE(SUBSTR(i_vcExpression,iPos+12, iEndPos-iPos-13), '"', '''');
                  vcResult:=vcResult || ' LIKE ' || vcVarname || ' || ''%''';
                ELSIF lTokenValues(iPos).vcFoundTag='!' THEN
                  -- String, match end-Token
                  vcResult:=vcResult || ' NOT ';
                ELSE
                  vcResult:=vcResult || lTokenValues(iPos).vcFoundTag;
                END IF;
              END IF;
            END IF;
            iPos:=lTokenValues.NEXT(iEndPos);
          END LOOP;
          IF i_bForSqlUsage AND iLastPos<=LENGTH(i_vcExpression) THEN
            vcResult:=vcResult || SUBSTR(i_vcExpression, iLastPos);
          END IF;
        ELSE
          vcResult:=i_vcExpression;
        END IF;
      END IF;
    END IF;
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('expression result:' || vcResult);
    END IF;

    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_EXPRESSION(i_nStackPos      IN NUMBER,
                                  i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                  i_vcPattern      IN PK_JRXML2PDF_TYPES.tPattern,
                                  i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN VARCHAR2 IS
    vcResult PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    IF i_vcExpression LIKE 'msg(%' THEN
      -- special case, language specific msg, cobvert to SQL
      -- and then execute immediate
      vcResult:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos      =>i_nStackPos,
                                           i_vcExpression   =>i_vcExpression,
                                           i_vcPattern      =>i_vcPattern,
                                           i_bForSqlUsage   =>TRUE,
                                           i_nRecordOffset  =>i_nRecordOffset
                                          );
      EXECUTE IMMEDIATE 'SELECT ' || REPLACE(vcResult, 'msg(', 'PK_JRXML2PDF_UTIL.FK_TEXT_MESSAGE(') || 'FROM DUAL'
                   INTO vcResult;
    ELSE
      vcResult:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos      =>i_nStackPos,
                                           i_vcExpression   =>i_vcExpression,
                                           i_vcPattern      =>i_vcPattern,
                                           i_bForSqlUsage   =>FALSE,
                                           i_nRecordOffset  =>i_nRecordOffset
                                          );
    END IF;
    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos      IN NUMBER,
                                         i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                         i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN NUMBER IS
    vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nResult     NUMBER;
  BEGIN
    IF i_vcExpression IS NOT NULL THEN
      vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_vcExpression,
                                              i_vcPattern    =>NULL,
                                              i_bForSQLUsage =>TRUE,
                                              i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE
                                             );
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Try to evaluate as Number:' || vcStatement);
      END IF;

      EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                   INTO nResult;
    END IF;
    RETURn nResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_EXPRESSION_DATE(i_nStackPos      IN NUMBER,
                                       i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                       i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN DATE IS
    vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
    dtResult    DATE;
  BEGIN
    IF i_vcExpression IS NOT NULL THEN
      vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_vcExpression,
                                              i_vcPattern    =>NULL,
                                              i_bForSQLUsage =>TRUE,
                                              i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE
                                             );
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Try to evaluate as Date:' || vcStatement);
      END IF;

      EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                   INTO dtResult;
    END IF;
    RETURn dtResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_CONDITION(i_nStackPos      IN NUMBER,
                                 i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression,
                                 i_nRecordOffset  IN NUMBER DEFAULT PK_JRXML2PDF_TYPES.OFFSET_NONE)
  RETURN BOOLEAN IS
    vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nResult     NUMBER;
  BEGIN
    vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos      =>i_nStackPos,
                                            i_vcExpression   =>i_vcExpression,
                                            i_vcPattern      =>NULL,
                                            i_bForSqlUsage   =>TRUE,
                                            i_nRecordOffset  =>i_nRecordOffset);

    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Evaluate boolean:' || vcStatement);
    END IF;

    EXECUTE IMMEDIATE 'SELECT CASE WHEN ' || vcStatement || ' THEN 1 END FROM DUAL'
            INTO nResult;
    RETURN NVL(nResult,0)=1;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_EVALUATE_IMAGE(i_nStackPos      IN NUMBER,
                             i_vcExpression   IN PK_JRXML2PDF_TYPES.tExpression)
  RETURN BLOB IS
    iField       PLS_INTEGER:=INSTR(i_vcExpression, '$F');
    iEndField    PLS_INTEGER;
    vcFieldName  PK_JRXML2PDF_TYPES.tAttribute;
    rEntry       PK_JRXML2PDF_TYPES.tDataEntry;
    blValue      BLOB;
    lDataEntry   PK_JRXML2PDF_TYPES.tDataEntries;
  BEGIN
    IF iField>0 THEN
      iEndField:=INSTR(i_vcExpression, '}', iField+1);
      IF iEndField>iField THEN
        vcFieldName:=SUBSTR(i_vcExpression, iField+3, iEndField-iField-3);

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Fieldname is ' || vcFieldName);
        END IF;
        lDataEntry:=FK_GET_QUERY_RESULT(i_nStackPos    =>i_nStackPos,
                                        i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE);
        IF lDataEntry.EXISTS(UPPER(vcFieldName)) THEN

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Found Entry of datatype ' || rEntry.vcDatatype);
          END IF;

          rEntry:=lDataEntry(UPPER(vcFieldName));
          IF rEntry.vcDataType=PK_JRXML2PDF_TYPES.DATATYPE_BLOB THEN
            blValue:=rEntry.blData;
          END IF;
        END IF;
      END IF;
    END IF;
    RETURN blValue;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SET_VARIABLE(i_nStackPos IN NUMBER,
                            i_vcName    IN VARCHAR2,
                            i_vcValue   IN VARCHAR2
                           ) IS
    rVariable PK_JRXML2PDF_TYPES.tDataEntry;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable Stackpos / Name / value ' || TO_CHAR(i_nStackPos) || '/' ||
                                                                                  i_vcName             || '/' ||
                                                                                  i_vcValue);
    END IF;
    rVariable.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
    rVariable.vcData    :=i_vcValue;
    -- Set value into reports variable-cache
    lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcName)):=rVariable;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SET_VARIABLE(i_nStackPos IN NUMBER,
                            i_vcName    IN VARCHAR2,
                            i_nValue    IN NUMBER
                           ) IS
    rVariable PK_JRXML2PDF_TYPES.tDataEntry;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable Stackpos / Name / value ' || TO_CHAR(i_nStackPos) || '/' ||
                                                                                  i_vcName             || '/' ||
                                                                                  i_nValue);
    END IF;
    rVariable.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    rVariable.nData     :=i_nValue;
    -- Set value into reports variable-cache
    lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcName)):=rVariable;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SET_VARIABLE(i_nStackPos IN NUMBER,
                            i_vcName    IN VARCHAR2,
                            i_dtValue   IN DATE
                           ) IS
    rVariable PK_JRXML2PDF_TYPES.tDataEntry;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable Stackpos / Name / value ' || TO_CHAR(i_nStackPos) || '/' ||
                                                                                  i_vcName             || '/' ||
                                                                                  i_dtValue);
    END IF;
    rVariable.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
    rVariable.dtData    :=i_dtValue;
    -- Set value into reports variable-cache
    lReportStack(i_nStackPos).lVariableCache(UPPER(i_vcName)):=rVariable;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_UNSET_VARIABLE(i_nStackPos IN NUMBER,
                              i_vcName    IN VARCHAR2) IS
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Unset variable Stackpos / Name' || TO_CHAR(i_nStackPos) || '/' ||
                                                                           i_vcName);
    END IF;
    -- Set value into reports variable-cache
    lReportStack(i_nStackPos).lVariableCache.DELETE(UPPER(i_vcName));
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_POPULATE_PARAMLIST(i_nStackPos  IN NUMBER,
                                 i_lParamList IN PK_JRXML2PDF_TYPES.tParamList)
  RETURN PK_JRXML2PDF_TYPES.tParamList IS
    lParams PK_JRXML2PDF_TYPES.tParamList:=i_lParamList;
    iPos    PLS_INTEGER:=0;
  BEGIN
    FOR i IN 1..lParams.COUNT LOOP
      lParams(i).vcValue:=FK_EVALUATE_EXPRESSION(i_nStackPos      =>i_nStackPos,
                                                 i_vcExpression   =>lParams(i).vcValue,
                                                 i_vcPattern      =>NULL
                                                );
    END LOOP;
    -- for nls-support, pass the parameter PK_JRXML2PDF_TYPES.REPORT_LOCALE, if set in the main-report
    FOR i iN 1..lReportStack(i_nStackPos).lParams.COUNT LOOP
      IF lReportStack(i_nStackPos).lParams(i).vcName=PK_JRXML2PDF_TYPES.REPORT_LOCALE THEN
        iPos:=lParams.COUNT+1;
        lParams(iPos).vcName:=PK_JRXML2PDF_TYPES.REPORT_LOCALE;
        lParams(iPos).vcValue:=lReportStack(i_nStackPos).lParams(i).vcValue;
        EXIT;
      END IF;
    END LOOP;
    RETURN lParams;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CALC_BAND_HEIGHT(i_nStackPos        IN NUMBER,
                                i_rBand            IN PK_JRXML2PDF_TYPES.tBand,
                                i_bForceCalculate  IN BOOLEAN DEFAULT FALSE,
                                o_nbandHeight      OUT NUMBER,
                                o_nBandOffset      OUT NUMBER) IS

    nBandHeight   NUMBER:=0;
    vcText        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcPattern     PK_JRXML2PDF_TYPES.tPattern;
    rDummyArea    PK_JRXML2PDF_TYPES.tArea;
    blImage       BLOB;
    nHeightOffset NUMBER:=0;
    nDetailOffset NUMBER:=0;
    nDummy        NUMBER;
    nTextheight   NUMBER;
    nPos          NUMBER;
    bRender       BOOLEAN:=TRUE;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Scale band');
    END IF;

    IF i_rBand.vcWhenExpression IS NOT NULL THEN
      bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                     i_vcExpression=>i_rBand.vcWhenExpression
                                    );
    END IF;
    IF bRender THEN
      nBandHeight:=NVL(i_rBand.nHeight, 0);
      IF    i_rband.vcSplitType!=PK_JRXML2PDF_TYPES.STRETCH
         OR i_bForceCalculate THEN
        -- is not allowed to stretch, calculate object heights
        FOR i IN 1..i_rband.lObjects.COUNT LOOP

          nPos:=i_rband.lObjects(i).nPosition;
          IF i_rband.lObjects(i).nType=PK_JRXML2PDF_TYPES.SUBFRAME THEN

            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Scale subframe');
            END IF;

            PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                                i_rBand            =>lReportStack(i_nStackPos).lBands(nPos),
                                o_nbandHeight      =>nDummy,
                                o_nBandOffset      =>nDetailOffset
                               );
            nHeightOffset:=GREATEST(nHeightOffset, nDetailOffset);
          ELSIF i_rband.lObjects(i).nType=PK_JRXML2PDF_TYPES.FIELD THEN
            IF lReportStack(i_nStackPos).lFields(nPos).vcStretchOverflow=PK_JRXML2PDF_TYPES.YES THEN

              IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Scale Textfield ' || lReportStack(i_nStackPos).lFields(nPos).vcExpression);
              END IF;

              IF lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression IS NOT NULL THEN
                vcPattern:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                                  i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression,
                                                  i_vcPattern   =>NULL);
              ELSE
                vcPattern:=lReportStack(i_nStackPos).lFields(nPos).vcPattern;
              END IF;
              vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                             i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcExpression,
                                             i_vcPattern   =>vcPattern);

              IF lReportStack(i_nStackPos).lFields(nPos).vcMarkup='html' THEN
                nTextheight:=FK_CALC_HTML_HEIGHT(i_nX              =>lReportStack(i_nStackPos).lFields(nPos).nX,
                                                 i_nY              =>lReportStack(i_nStackPos).lFields(nPos).nY,
                                                 i_nWidth          =>lReportStack(i_nStackPos).lFields(nPos).nWidth,
                                                 i_nHeight         =>lReportStack(i_nStackPos).lFields(nPos).nHeight,
                                                 i_vcText          =>vcText,
                                                 i_vcFont          =>lReportStack(i_nStackPos).lFields(nPos).vcFont,
                                                 i_nFontSize       =>lReportStack(i_nStackPos).lFields(nPos).nFontSize,
                                                 i_vcAlignment     =>lReportStack(i_nStackPos).lFields(nPos).vcAlignment,
                                                 i_nTopPadding     =>lReportStack(i_nStackPos).lFields(nPos).nTopPadding,
                                                 i_nLeftPadding    =>lReportStack(i_nStackPos).lFields(nPos).nLeftPadding,
                                                 i_nBottomPadding  =>lReportStack(i_nStackPos).lFields(nPos).nBottomPadding,
                                                 i_nRightPadding   =>lReportStack(i_nStackPos).lFields(nPos).nRightPadding,
                                                 i_nLineSpacing    =>lReportStack(i_nStackPos).lFields(nPos).nLineSpacing,
                                                 i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lFields(nPos).vcStyle),
                                                 i_bEnhanced       =>NVL(lReportStack(i_nStackPos).lFields(nPos).vcKey, '.') LIKE 'enhanced%');
              ELSE
                nTextheight:=FK_CALC_TEXT_HEIGHT(i_nX              =>lReportStack(i_nStackPos).lFields(nPos).nX,
                                                 i_nY              =>lReportStack(i_nStackPos).lFields(nPos).nY,
                                                 i_nWidth          =>lReportStack(i_nStackPos).lFields(nPos).nWidth,
                                                 i_nHeight         =>lReportStack(i_nStackPos).lFields(nPos).nHeight,
                                                 i_vcText          =>vcText,
                                                 i_vcFont          =>lReportStack(i_nStackPos).lFields(nPos).vcFont,
                                                 i_nFontSize       =>lReportStack(i_nStackPos).lFields(nPos).nFontSize,
                                                 i_vcAlignment     =>lReportStack(i_nStackPos).lFields(nPos).vcAlignment,
                                                 i_nTopPadding     =>lReportStack(i_nStackPos).lFields(nPos).nTopPadding,
                                                 i_nLeftPadding    =>lReportStack(i_nStackPos).lFields(nPos).nLeftPadding,
                                                 i_nBottomPadding  =>lReportStack(i_nStackPos).lFields(nPos).nBottomPadding,
                                                 i_nRightPadding   =>lReportStack(i_nStackPos).lFields(nPos).nRightPadding,
                                                 i_nLineSpacing    =>lReportStack(i_nStackPos).lFields(nPos).nLineSpacing,
                                                 i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lFields(nPos).vcStyle));
              END IF;
              IF nTextheight>lReportStack(i_nStackPos).lFields(nPos).nHeight THEN
                IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
                  PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Field stretched from/to: ' || TO_CHAR(lReportStack(i_nStackPos).lFields(nPos).nHeight) || '/' || TO_CHAR(nTextheight));
                END IF;
                -- Field stretched
                nHeightOffset:=GREATEST(nHeightOffset,nTextheight-lReportStack(i_nStackPos).lFields(nPos).nHeight);
              END IF;
            END IF;
          END IF;
        END LOOP;
      END IF;
    END IF;
    o_nBandHeight:=nBandHeight+nHeightOffset;
    o_nBandOffset:=nHeightOffset;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_FIELD_FOR_LATER(i_nStackPos IN NUMBER,
                                   i_nX       IN NUMBER,
                                   i_nY       IN NUMBER,
                                   i_rField   IN PK_JRXML2PDF_TYPES.tField
                                  ) IS
    iPos   PLS_INTEGER:=NVL(lReportStack(i_nStackPos).lEvalLaterFields.LAST+1,1);
    rField PK_JRXML2PDF_TYPES.tField:=i_rField;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add Field for Later Expression / Page / Pos ' ||
                                       i_rField.vcExpression ||'/' ||
                                       lReportStack(1).lLogicalPageNumbers(lReportStack(1).nCurrentPagePointer) ||'/' || iPos);
    END IF;
    -- absoulte Position
    rField.nX:=rField.nX+i_nX;
    rField.nY:=rField.nY+i_nY;
    -- The page number has to be the gloabl pagenumber
    lReportStack(i_nStackPos).lEvalLaterPageNo(iPos):=lReportStack(1).lPageNumbers(lReportStack(1).nCurrentPagePointer);
    lReportStack(i_nStackPos).lEvalLaterFields(iPos):=rField;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_LATER_FIELDS(i_nStackPos         IN NUMBER,
                                   i_vcEvaluationTime  IN VARCHAr2,
                                   i_vcEvaluationGroup IN VARCHAR2 DEFAULT NULL,
                                   i_bKeepPagenumber   IN BOOLEAN  DEFAULT FALSE
                                  ) IS
    iPos      PLS_INTEGER:=lReportStack(i_nStackPos).lEvalLaterFields.FIRST;
    iDelPos   PLS_INTEGER;
    vcText    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcPattern PK_JRXML2PDF_TYPES.tPattern;
    rDummy    PK_JRXML2PDF_TYPES.tArea;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Render later fields, type/count ' || i_vcEvaluationTime || '/' || TO_CHAR(lReportStack(i_nStackPos).lEvalLaterFields.COUNT));
    END IF;
    WHILE iPos IS NOT NULL LOOP
      IF     lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcEvaluationTime=i_vcEvaluationTime
         AND (   i_vcEvaluationTime!=PK_JRXML2PDF_TYPES.EVALUATION_GROUP
              OR (    i_vcEvaluationTime=PK_JRXML2PDF_TYPES.EVALUATION_GROUP
                  AND i_vcEvaluationGroup=lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcEvaluationGroup
                 )
             ) THEN
        -- Do it now, same like in Render band
        -- navigate to appropiate page
        -- special case PAGE_NUMBER is not rendered when i_bKeepPagenumber and evaluation tiem is REPORT
        IF     lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcEvaluationTime!=PK_JRXML2PDF_TYPES.EVALUATION_REPORT
           OR (    lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcEvaluationTime=PK_JRXML2PDF_TYPES.EVALUATION_REPORT 
               AND NOT i_bKeepPagenumber
              )
           OR (    lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcEvaluationTime=PK_JRXML2PDF_TYPES.EVALUATION_REPORT 
               AND i_bKeepPagenumber
               AND INSTR(lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcExpression, '{PAGE_NUMBER}')=0
              ) THEN
          AS_PDF3_MOD.PR_GOTO_PAGE(lReportStack(i_nStackPos).lEvalLaterPageNo(iPos));
  
          IF lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcPatternExpression IS NOT NULL THEN
            vcPattern:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                              i_vcExpression=>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcPatternExpression,
                                              i_vcPattern   =>NULL);
          ELSE
            vcPattern:=lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcPattern;
          END IF;
  
          vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>i_nStackPos,
                                         i_vcExpression =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcExpression,
                                         i_vcPattern    =>vcPattern);
  
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render later field Page/Text: ' || lReportStack(i_nStackPos).lEvalLaterPageNo(iPos)
                                                                                 || vcText
                                               );
          END IF;
          rDummy:=FK_RENDER_TEXT(i_nX              =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nX,
                                 i_nY              =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nY,
                                 i_nWidth          =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nWidth,
                                 i_nHeight         =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nHeight,
                                 i_vcText          =>vcText,
                                 i_vcFont          =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcFont,
                                 i_nFontSize       =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nFontSize,
                                 i_vcFontStyle     =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcFontStyle,
                                 i_vcAlignment     =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcAlignment,
                                 i_vcVerticalAlign =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcVerticalAlign,
                                 i_vcBgColor       =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcBGColor,
                                 i_vcFgColor       =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcFGColor,
                                 i_nBoxTop         =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nBoxTop,
                                 i_nBoxLeft        =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nBoxLeft,
                                 i_nBoxBottom      =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nBoxBottom,
                                 i_nBoxRight       =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nBoxRight,
                                 i_nTopPadding     =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nTopPadding,
                                 i_nLeftPadding    =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nLeftPadding,
                                 i_nBottomPadding  =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nBottomPadding,
                                 i_nRightPadding   =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nRightPadding,
                                 i_vcBoxTopColor   =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcBoxTopColor,
                                 i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcBoxLeftColor,
                                 i_vcBoxBottomColor=>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcBoxBottomColor,
                                 i_vcBoxRightColor =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcBoxRightColor,
                                 i_nLineSpacing    =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).nLineSpacing,
                                 i_vcOpaque        =>lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcOpaque,
                                 i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lEvalLaterFields(iPos).vcStyle)
                                );
          -- back to current page
          AS_PDF3_MOD.PR_GOTO_CURRENT_PAGE;
          iDelPos:=iPos;
          iPos:=lReportStack(i_nStackPos).lEvalLaterFields.NEXT(iPos);
          -- delete written entry
          lReportStack(i_nStackPos).lEvalLaterFields.DELETE(iDelPos);
          lReportStack(i_nStackPos).lEvalLaterPageNo.DELETE(iDelPos);
        ELSE
          iPos:=lReportStack(i_nStackPos).lEvalLaterFields.NEXT(iPos);
        END IF;
      ELSE
        iPos:=lReportStack(i_nStackPos).lEvalLaterFields.NEXT(iPos);
      END IF;
    END LOOP;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RENDER_CROSSTAB(i_nStackPos        IN NUMBER,
                              i_rCrosstab        IN PK_JRXML2PDF_TYPES.tCrosstab,
                              i_rArea            IN PK_JRXML2PDF_TYPES.tArea)
  RETURN PK_JRXML2PDF_TYPES.tArea IS

    TYPE tCrossGroupEntry IS RECORD (
      vcIndex     PK_JRXML2PDF_TYPES.tMaxVarchar2,
      vcValue     PK_JRXML2PDF_TYPES.tMaxVarchar2,
      nValue      NUMBER,
      dtValue     DATE,
      iDetailList PLS_INTEGER
    );

    TYPE tCrossGroupEntries IS TABLE OF tCrossGroupEntry INDEX BY PK_JRXML2PDF_TYPES.tName;
    TYPE tCrossGroupEntriesList IS TABLE OF tCrossGroupEntries INDEX BY BINARY_INTEGER;

    TYPE tCellData IS RECORD (
      vcDatatype  PK_JRXML2PDF_TYPES.tDatatype,
      iCount      PLS_INTEGER,
      nSum        NUMBER,
      vcValue     PK_JRXML2PDF_TYPES.tMaxVarchar2,
      dtValue     DATE,
      nValue      NUMBER
    );

    TYPE tCellDataList IS TABLE OF tCellData INDEX BY PK_JRXML2PDF_TYPES.tName;
    TYPE tCellList     IS TABLE OF tCellDataList INDEX BY PK_JRXML2PDF_TYPES.tName;
    TYPE tRowList      IS TABLE OF PK_JRXML2PDF_TYPES.tName INDEX BY PK_JRXML2PDF_TYPES.tName;
    TYPE tMatrix       IS TABLE OF tRowList INDEX BY BINARY_INTEGER;

    TYPE tRowHeader IS RECORD (
      nStartX NUMBER,
      nStartY NUMBER,
      rBand   PK_JRXML2PDF_TYPES.tBand
    );

    TYPE tRowHeaderList IS TABLE OF tRowHeader INDEX BY BINARY_INTEGER;

    lRowHeaders         tRowHeaderList;
    lRowgroups          tCrossGroupEntriesList;
    lColGroups          tCrossGroupEntriesList;
    lCells              tCellList;
    lEmptyList          tCrossGroupEntries;
    rArea               PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    nRowOffset          NUMBER;
    nColPosition        NUMBER;
    nColGroupIdx        NUMBER;
    lColumnMatrix       tMatrix;
    rDummyArea          PK_JRXML2PDF_TYPES.tArea;
    nColumnFooterHeight NUMBER;
    nPageFooterheight   NUMBER;
    bDummy              BOOLEAN;
    lColHeaderHeights   PK_JRXML2PDF_TYPES.tNumList;
    bNoRowHeaders       BOOLEAN:=FALSE;
    bNeedsColumnHeader  BOOLEAN:=FALSE;
    lColKeys            PK_JRXML2PDF_TYPES.tIndexList;
    lRowKeys            PK_JRXML2PDF_TYPES.tIndexList;
    rEmptydata          tCellData;
    rHeaderBand         PK_JRXML2PDF_TYPES.tBand;
    
    PROCEDURE PR_SET_VARIABLE_FROM_DATA(i_nStackPos IN NUMBER,
                                        i_vcName    IN PK_JRXML2PDF_TYPES.tName,
                                        i_rData     IN tCellData,
                                        i_rMeasure  IN PK_JRXML2PDF_TYPES.tCrossTabMeasure
                                       ) IS
    BEGIN
      IF i_rMeasure.vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_COUNT THEN

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable/value for COUNT:' || i_vcName || '/' || TO_CHAR(i_rData.iCount));
        END IF;

        PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                        i_vcName   =>i_vcName,
                        i_nValue   =>NVL(i_rData.iCount,0)
                       );
      ELSIF i_rMeasure.vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_SUM THEN
        IF i_rData.vcdatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set number-variable/value:' || i_vcName || '/' || TO_CHAR(i_rData.nValue));
          END IF;

          PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                          i_vcName   =>i_vcName,
                          i_nValue   =>NVL(i_rData.nValue, 0)
                         );
        ELSE
          PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                          i_vcName   =>i_vcName,
                          i_nValue   =>0
                         );
        END IF;
      ELSIF i_rMeasure.vcCalculation IN (PK_JRXML2PDF_TYPES.CALCULATION_LOWEST,
                                         PK_JRXML2PDF_TYPES.CALCULATION_HIGHEST,
                                         PK_JRXML2PDF_TYPES.CALCULATION_FIRST,
                                         PK_JRXML2PDF_TYPES.CALCULATION_NOTHING) THEN
        IF i_rData.vcdatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set number-variable/value:' || i_vcName || '/' || TO_CHAR(i_rData.nValue));
          END IF;

          PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                          i_vcName   =>i_vcName,
                          i_nValue   =>i_rData.nValue
                         );
        ELSIF i_rData.vcdatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set date-variable/value:' || i_vcName || '/' || TO_CHAR(i_rData.dtValue));
          END IF;

          PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                          i_vcName   =>i_vcName,
                          i_dtValue  =>i_rData.dtValue
                         );
        ELSE
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set varchar2-variable/value:' || i_vcName || '/' || TO_CHAR(i_rData.vcValue));
          END IF;
          PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                          i_vcName   =>i_vcName,
                          i_vcValue  =>i_rData.vcValue
                         );
        END IF;
      ELSIF i_rMeasure.vcCalculation IN (PK_JRXML2PDF_TYPES.CALCULATION_AVERAGE) THEN
        IF i_rData.vcdatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF i_rData.iCount>0 THEN
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable/value for AVG:' || i_vcName || '/' || TO_CHAR(i_rData.nValue/i_rData.iCount));
            END IF;
            PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                            i_vcName   =>i_vcName,
                            i_nValue   =>i_rData.nValue/i_rData.iCount
                           );
          ELSE
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set variable/value for AVG:' || i_vcName || '/0');
            END IF;
            PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                            i_vcName   =>i_vcName,
                            i_nValue   =>0
                           );
          END IF;
        END IF;
      END IF;
    END;

    PROCEDURE PR_SET_VARIABLE_FROM_CROSSTAB(i_vcName     IN PK_JRXML2PDF_TYPES.tName,
                                            i_vcDatatype IN PK_JRXML2PDF_TYPES.tDatatype,
                                            i_rEntry     IN tCrossGroupEntry
                                           ) IS
    BEGIN
      IF i_vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set crosstab-number-variable/value:' || i_vcName || '/' || TO_CHAR(i_rEntry.nValue));
        END IF;
        PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                        i_vcName   =>i_vcName,
                        i_nValue   =>i_rEntry.nValue
                       );
      ELSIF i_vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set crosstab-varchar2-variable/value:' || i_vcName || '/' || TO_CHAR(i_rEntry.vcValue));
        END IF;
        PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                        i_vcName   =>i_vcName,
                        i_vcValue  =>i_rEntry.vcValue
                       );
      ELSIF i_vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set crosstab-date-variable/value:' || i_vcName || '/' || TO_CHAR(i_rEntry.dtValue));
        END IF;
        PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                        i_vcName   =>i_vcName,
                        i_dtValue  =>i_rEntry.dtValue
                      );
      END IF;
    END;

    FUNCTION FK_MAKE_ENTRY(i_vcDatatype   IN PK_JRXML2PDF_TYPES.tDatatype,
                           i_vcExpression IN PK_JRXML2PDF_TYPES.tExpression
                          )
    RETURN tCrossGroupEntry IS
      rEntry      tCrossGroupEntry;
      vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_vcExpression,
                                              i_vcPattern    =>NULL,
                                              i_bForSQLUsage =>TRUE,
                                              i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE
                                             );
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Create entry-index for row/column from statement:' || vcStatement);
      END IF;

      IF i_vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
        EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                     INTO rEntry.nValue;
        rEntry.vcIndex:=TO_CHAR(rEntry.nValue, 'FM00000000000000000000D00000000000000000');
      ELSIF i_vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
        EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                     INTO rEntry.dtValue;
        rEntry.vcIndex:=TO_CHAR(rEntry.dtValue, 'YYYYMMDDHH24MISS');
      ELSE
        EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                     INTO rEntry.vcValue;
        rEntry.vcIndex:=rEntry.vcValue;
      END IF;
      RETURn rEntry;
    END;

    FUNCTION FK_REKU_ROWGROUPS(i_nRow          IN NUMBER,
                               i_nGroupListIdx IN PLS_INTEGER)
    RETURN PK_JRXML2PDF_TYPES.tMaxVarcharList IS
      rEntry      tCrossGroupEntry;
      lRowIndexes PK_JRXML2PDF_TYPES.tMaxVarcharList;
    BEGIN
      IF i_nRow<=i_rCrosstab.lRows.COUNT THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Recursive rowgroup-entry row/expression:' || TO_CHAR(i_nRow) || '/' || i_rCrosstab.lRows(i_nRow).vcBucketExpression);
        END IF;
        rEntry:=FK_MAKE_ENTRY(i_vcDatatype  =>i_rCrosstab.lRows(i_nRow).vcDatatype,
                              i_vcExpression=>i_rCrosstab.lRows(i_nRow).vcBucketExpression
                             );
        IF NOT lRowgroups(i_nGroupListIdx).EXISTS(rEntry.vcIndex) THEN
          lRowgroups(i_nGroupListIdx)(rEntry.vcIndex):=rEntry;
          lRowgroups(lRowgroups.COUNT+1):=lEmptyList;
          lRowgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList:=lRowgroups.COUNT;
        END IF;
        lRowIndexes:=FK_REKU_ROWGROUPS(i_nRow+1, lRowgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList);
        lRowIndexes(lRowIndexes.COUNT+1):=TO_CHAR(lRowgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList) || '_';
        IF i_nRow=1 THEN
          lRowIndexes(lRowIndexes.COUNT+1):=NULL;
        END IF;
      END IF;
      RETURN lRowIndexes;
    END;

    FUNCTION FK_REKU_COLGROUPS(i_nCol          IN NUMBER,
                               i_nGroupListIdx IN PLS_INTEGER)
    RETURN PK_JRXML2PDF_TYPES.tMaxVarcharList IS
      rEntry      tCrossGroupEntry;
      lColIndexes PK_JRXML2PDF_TYPES.tMaxVarcharList;
    BEGIN
      IF i_nCol<=i_rCrosstab.lColumns.COUNT THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Recursive colgroup-entry row/expression:' || TO_CHAR(i_nCol) || '/' || i_rCrosstab.lColumns(i_nCol).vcBucketExpression);
        END IF;
        -- evaluate value for row
        rEntry:=FK_MAKE_ENTRY(i_vcDatatype  =>i_rCrosstab.lColumns(i_nCol).vcDatatype,
                              i_vcExpression=>i_rCrosstab.lColumns(i_nCol).vcBucketExpression
                             );
        IF NOT lColgroups(i_nGroupListIdx).EXISTS(rEntry.vcIndex) THEN
          lColgroups(i_nGroupListIdx)(rEntry.vcIndex):=rEntry;
          lColgroups(lColgroups.COUNT+1):=lEmptyList;
          lColgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList:=lColgroups.COUNT;
          -- coun the number of last-level-columns
        END IF;
        lColIndexes:=FK_REKU_COLGROUPS(i_nCol+1, lColgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList);
        lColIndexes(lColIndexes.COUNT+1):=TO_CHAR(lColgroups(i_nGroupListIdx)(rEntry.vcIndex).iDetailList) || '_';
        IF i_nCol=1 THEN
          lColIndexes(lColIndexes.COUNT+1):=NULL;
        END IF;
      END IF;
      RETURN lColIndexes;
    END;

    PROCEDURE PR_LOAD_DATA IS
      rReport     PK_JRXML2PDF_TYPES.tReport;
      vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
      vcIndex     PK_JRXML2PDF_TYPES.tName;
      vcRowIndex  PK_JRXML2PDF_TYPES.tName;
      vcColIndex  PK_JRXML2PDF_TYPES.tName;
      vcIndex2    PK_JRXML2PDF_TYPES.tName;
      rCell       tCellDataList;
      rEmptyCell  tCellDataList;
      rData       tCellData;
      rEmptydata  tCellData;
      lRowIndexes PK_JRXML2PDF_TYPES.tMaxVarcharList;
      lColIndexes PK_JRXML2PDF_TYPES.tMaxVarcharList;
      nValue      NUMBER;
      dtValue     DATE;
      vcValue     PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Load crosstab-data from query');
      END IF;
      -- fake a report to execute the query
      rReport.vcQuery:=i_rCrosstab.vcQuery;
      -- Push to stack
      PR_PUSH_SUBREPORT_DATA(rReport);

      -- Init lists
      lRowgroups(1):=lEmptyList;
      lColgroups(1):=lEmptyList;

      PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rCrosstab.lParams);

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Fetching crosstab-data');
      END IF;
      IF FK_FETCH_ROW(lReportStack.COUNT) THEN
        LOOP
          -- build up rows and columns recursively from the values
          lRowIndexes:=FK_REKU_ROWGROUPS(1, 1);
          lColIndexes:=FK_REKU_COLGROUPS(1, 1);
          FOR i IN 1..lRowIndexes.COUNT LOOP
            FOR j IN 1..lColIndexes.COUNT LOOP
              vcIndex:=lRowIndexes(i) || '#' || lColIndexes(j);
              -- now put the measure in the cell
              IF lCells.EXISTS(vcIndex) THEN
                rCell:=lCells(vcIndex);
              ELSE
                rCell:=rEmptyCell;
              END IF;
              -- add up values
              FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
                IF rCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN
                  rData:=rCell(i_rCrosstab.lMeasures(i).vcName);
                ELSE
                  rData:=rEmptydata;
                END IF;
                rData.iCount:=NVL(rData.iCount,0)+1;
                IF i_rCrosstab.lMeasures(i).vcCalculation!=PK_JRXML2PDF_TYPES.CALCULATION_COUNT THEN
                  vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos    =>lReportStack.COUNT,
                                                          i_vcExpression =>i_rCrosstab.lMeasures(i).vcExpression,
                                                          i_vcPattern    =>NULL,
                                                          i_bForSQLUsage =>TRUE,
                                                          i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE
                                                         );

                  IF i_rCrosstab.lMeasures(i).vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
                    rData.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
                    EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                                 INTO nValue;
                    IF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_HIGHEST THEN
                      rData.nValue:=NVL(GREATEST(rData.nValue, nValue), nValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation IN (PK_JRXML2PDF_TYPES.CALCULATION_SUM, PK_JRXML2PDF_TYPES.CALCULATION_AVERAGE) THEN
                      rData.nValue:=NVL(rData.nValue,0)+ NVL(nValue,0);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_LOWEST THEN
                      rData.nValue:=NVL(LEAST(rData.nValue, nValue), nValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_FIRST THEN
                      IF rData.nValue IS NULL THEN
                        rData.nValue:=nValue;
                      END IF;
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_NOTHING THEN
                      rData.nValue:=nValue;
                    END IF;
                  ELSIF i_rCrosstab.lMeasures(i).vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
                    rData.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
                    EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                                 INTO dtValue;
                    IF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_HIGHEST THEN
                      rData.dtValue:=NVL(GREATEST(rData.dtValue, dtValue), dtValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_LOWEST THEN
                      rData.dtValue:=NVL(LEAST(rData.dtValue, dtValue), dtValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_FIRST THEN
                      IF rData.dtValue IS NULL THEN
                        rData.dtValue:=dtValue;
                      END IF;
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_NOTHING THEN
                      rData.dtValue:=dtValue;
                    END IF;
                  ELSE
                    rData.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
                    EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                                 INTO vcValue;
                    IF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_HIGHEST THEN
                      rData.vcValue:=NVL(GREATEST(rData.vcValue, vcValue), vcValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_LOWEST THEN
                      rData.vcValue:=NVL(LEAST(rData.vcValue, vcValue), vcValue);
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_FIRST THEN
                      IF rData.vcValue IS NULL THEN
                        rData.vcValue:=vcValue;
                      END IF;
                    ELSIF i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_NOTHING THEN
                      rData.vcValue:=vcValue;
                    END IF;
                  END IF;
                END IF;
                rCell(i_rCrosstab.lMeasures(i).vcName):=rData;
              END LOOP;
              -- store value back
              lCells(vcIndex):=rCell;
            END LOOP;
          END LOOP;
          EXIT WHEN NOT FK_FETCH_ROW(lReportStack.COUNT);
        END LOOP;
      END IF;
      -- take report back from stack
      PR_POP_SUBREPORT_DATA;
    END;

    PROCEDURE PR_ADD_COLUMN(i_vcDataIndex IN VARCHAR2,
                            i_vcName      IN VARCHAR2,
                            i_nColWidth   IN NUMBER
                           ) IS
      rEmptyEntry   tRowList;
    BEGIN
      IF nColPosition+i_nColWidth>i_rCrosstab.nWidth THEN
        -- new Columngroup
        nColGroupIdx:=nColGroupIdx+1;
        nColPosition:=nRowOffset;
        lColumnMatrix(nColGroupIdx):=rEmptyEntry;
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Add column Index/Name' || TO_CHAR(nColGroupIdx) || '/' || i_vcName);
      END IF;
      lColumnMatrix(nColGroupIdx)(NVL(i_vcDataIndex, PK_JRXML2PDF_TYPES.NIL)):=i_vcName;
      nColPosition:=nColPosition+i_nColWidth;
    END;

    PROCEDURE PR_REKU_ADD_COLUMN(i_nCol          IN NUMBER,
                                 i_nGroupListIdx IN NUMBER,
                                 i_vcKey         IN VARCHAR2) IS
      vcIndex PK_JRXML2PDF_TYPES.tIndex;
    BEGIN
      -- add all columnTotal with position start
      IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='Start' THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Recursive column-add, total at start ' || i_vcKey);
        END IF;
        PR_ADD_COLUMN(i_vcDataIndex=>i_vcKey,
                      i_vcName     =>i_rCrosstab.lColumns(i_nCol).vcName,
                      i_nColWidth  =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos).nWidth
                     );
      END IF;
      IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
        vcIndex:=lColGroups(i_nGroupListIdx).FIRST;
      ELSE
        vcIndex:=lColGroups(i_nGroupListIdx).LAST;
      END IF;
      WHILE vcIndex IS NOT NULL LOOP
        IF i_nCol<i_rCrosstab.lColumns.COUNT THEN
          -- process next level
          PR_REKU_ADD_COLUMN(i_nCol         =>i_nCol+1,
                             i_nGroupListIdx=>lColGroups(i_nGroupListIdx)(vcIndex).iDetailList,
                             i_vcKey        =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_'
                            );
        ELSE
          -- last level, add column to layout
          PR_ADD_COLUMN(i_vcDataIndex=>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_',
                        i_vcName     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_',
                        i_nColWidth  =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nHeaderPos).nWidth
                       );
        END IF;
        IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
          vcIndex:=lColGroups(i_nGroupListIdx).NEXT(vcIndex);
        ELSE
          vcIndex:=lColGroups(i_nGroupListIdx).PRIOR(vcIndex);
        END IF;
      END LOOP;
      -- add all columnTotal with position start
      IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='End' THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Recursive column-add, total at end ' || i_vcKey);
        END IF;
        PR_ADD_COLUMN(i_vcDataIndex=>i_vcKey,
                      i_vcName     =>i_rCrosstab.lColumns(i_nCol).vcName,
                      i_nColWidth  =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos).nWidth
                     );
      END IF;
    END;

    FUNCTION FK_CHECK_IN_COLSET(i_nColMatrixIdx IN NUMBER,
                                i_vcColkey      IN VARCHAR2
                               )
    RETURN BOOLEAN IS
    BEGIN
      RETURN lColumnMatrix(i_nColMatrixIdx).EXISTS(NVL(i_vcColkey, PK_JRXML2PDF_TYPES.NIL));
    END;

    PROCEDURE PR_SUMMARY_VARIABLES(i_nCurrentRow IN NUMBER,
                                   i_nCurrentCol IN NUMBER) IS
      rDataCell   tCelldataList;
      rEmptyCell  tCellDataList;
      vcIndex     PK_JRXML2PDF_TYPES.tName;
      vcRowIndex  PK_JRXML2PDF_TYPES.tName;
      vcColIndex  PK_JRXML2PDF_TYPES.tName;

      PROCEDURE PR_SET_ROW_VALUES IS
      BEGIN
        rDataCell:=rEmptyCell;
        vcIndex:='#';
        IF lCells.EXISTS(vcIndex) THEN
          rDataCell:=lCells(vcIndex);
        END IF;

        vcColIndex:=NULL;
        vcRowIndex:=NULL;
        FOR j IN 1..i_rCrossTab.lColumns.COUNT LOOP
          IF lColKeys.EXISTS(j)
             AND j<=i_nCurrentCol THEN
            -- now set the cell-values as variables
            vcColIndex:=lColKeys(j);
          END IF;
        END LOOP;

        FOR j IN 1..i_rCrossTab.lRows.COUNT LOOP
          IF     lRowKeys.EXISTS(j-1)
                AND j<=i_nCurrentRow THEN
            -- get the row-index from the above row
            vcRowIndex:=lRowKeys(j-1);
          END IF;

          vcIndex:=vcRowIndex || '#' || vcColIndex;
          IF lCells.EXISTS(vcIndex) THEN
            rDataCell:=lCells(vcIndex);
          END IF;

          FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
            -- set variable with value
            IF rDataCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN

              PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                        i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lRows(j).vcName || '_ALL',
                                        i_rData    =>rDataCell(i_rCrosstab.lMeasures(i).vcName),
                                        i_rMeasure =>i_rCrosstab.lMeasures(i)
                                       );
            ELSE
              -- set variable to null
              PR_UNSET_VARIABLE(i_nStackPos=>i_nStackPos,
                                i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lRows(j).vcName || '_ALL'
                               );
            END IF;
          END LOOP;
        END LOOP;
      END;

      PROCEDURE PR_SET_COLUMN_VALUES IS
      BEGIN
        rDataCell:=rEmptyCell;
        vcIndex:='#';
        IF lCells.EXISTS(vcIndex) THEN
          rDataCell:=lCells(vcIndex);
        END IF;

        vcColIndex:=NULL;
        vcRowIndex:=NULL;
        FOR j IN 1..i_rCrossTab.lRows.COUNT LOOP
          IF lRowKeys.EXISTS(j)
             AND j<=i_nCurrentRow THEN
            -- now set the cell-values as variables
            vcRowIndex:=lRowKeys(j);
          END IF;
        END LOOP;

        FOR j IN 1..i_rCrossTab.lColumns.COUNT LOOP
          IF     lColKeys.EXISTS(j-1)
                AND j<=i_nCurrentCol THEN
            -- get the row-index from the above row
            vcColIndex:=lColKeys(j-1);
          END IF;

          vcIndex:=vcRowIndex || '#' || vcColIndex;
          IF lCells.EXISTS(vcIndex) THEN
            rDataCell:=lCells(vcIndex);
          END IF;

          FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
            -- set variable with value
            IF rDataCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN
              PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                        i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lColumns(j).vcName || '_ALL',
                                        i_rData    =>rDataCell(i_rCrosstab.lMeasures(i).vcName),
                                        i_rMeasure =>i_rCrosstab.lMeasures(i)
                                       );
            ELSE
              -- set variable to null
              PR_UNSET_VARIABLE(i_nStackPos=>i_nStackPos,
                                i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lColumns(j).vcName || '_ALL'
                               );
            END IF;
          END LOOP;
        END LOOP;
      END;

      PROCEDURE PR_SET_CELL_VALUES IS
      BEGIN
        vcRowIndex:=NULL;
        FOR j IN 1..i_rCrossTab.lRows.COUNT LOOP
          IF lRowKeys.EXISTS(j-1) THEN
            -- now set the cell-values as variables
            vcRowIndex:=lRowKeys(j-1);
          END IF;
          vcColIndex:=NULL;
          FOR z IN 1..i_rCrossTab.lColumns.COUNT LOOP
            IF lColKeys.EXISTS(z-1) THEN
              -- now set the cell-values as variables
              vcColIndex:=lColKeys(z-1);
            END IF;

            IF lCells.EXISTS(vcRowIndex || '#' || vcColIndex) THEN
              rDataCell:=lCells(vcRowIndex || '#' || vcColIndex);
            END IF;
            FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
              -- set variable with value
              IF rDataCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN
                PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                          i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lRows(j).vcName || '_' || i_rCrosstab.lColumns(z).vcName || '_ALL',
                                          i_rData    =>rDataCell(i_rCrosstab.lMeasures(i).vcName),
                                          i_rMeasure =>i_rCrosstab.lMeasures(i)
                                         );
              ELSE
                -- set variable to null
                PR_UNSET_VARIABLE(i_nStackPos=>i_nStackPos,
                                  i_vcName   =>i_rCrosstab.lMeasures(i).vcName || '_' || i_rCrosstab.lRows(j).vcName || '_' || i_rCrosstab.lColumns(z).vcName || '_ALL'
                                 );
              END IF;
            END LOOP;
          END LOOP;
        END LOOP;
      END;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Setting summary-variables for rows/cols/cells ');
      END IF;

      PR_SET_ROW_VALUES;

      PR_SET_COLUMN_VALUES;

      PR_SET_CELL_VALUES;
    END;

    PROCEDURE PR_COLUMN_HEIGHT(i_nCol          IN  NUMBER,
                               i_nColMatrixIdx IN  NUMBER,
                               i_vcColIndex    IN  VARCHAR2,
                               i_vcColkey      IN  VARCHAR2
                              ) IS
      nHeight NUMBER;
      nDummy  NUMBER;
      rBand   PK_JRXML2PDF_TYPES.tBand;
    BEGIN
      IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                            i_vcColkey     =>i_vcColKey) THEN

        -- set variables for all row
        PR_SET_VARIABLE(i_nStackPos=>i_nStackPos,
                        i_vcName   =>i_rCrosstab.lColumns(i_nCol).vcName,
                        i_vcValue  =>i_vcColIndex
                       );
        rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nHeaderPos);
        -- Calc height for row-part
        PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                            i_rBand       =>rBand,
                            o_nbandHeight =>nHeight,
                            o_nBandOffset =>nDummy
                           );
        lColHeaderHeights(i_nCol):=GREATEST(lColHeaderHeights(i_nCol), nHeight);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Calculate rowheight name/key/height:' || i_rCrosstab.lColumns(i_nCol).vcName || '/' || i_vcColIndex|| '/' || nHeight);
        END IF;
      END IF;
    END;

    PROCEDURE PR_REKU_COLUMN_HEIGHTS(i_nColMatrixIdx IN NUMBER,
                                     i_nCol          IN NUMBER,
                                     i_nGroupListIdx IN NUMBER,
                                     i_vcColKey      IN PK_JRXML2PDF_TYPES.tIndex)  IS
      vcIndex       PK_JRXML2PDF_TYPES.tName;
      nMinHeight    NUMBER;
      nDummy        NUMBER;
      nYOffset      NUMBER;
      rDummyArea    PK_JRXML2PDF_TYPES.tArea:=rArea;
      rReturnArea   PK_JRXML2PDF_TYPES.tArea;
      rBand         PK_JRXML2PDF_TYPES.tBand;
      nStartWidth   NUMBER;
      nHeight       NUMBER;
    BEGIN
      IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                            i_vcColKey     =>i_vcColKey) THEN
        IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition IN ('Start', 'End') THEN

          -- calculate height for total column
          rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos);

          PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                              i_rBand       =>rBand,
                              o_nbandHeight =>nHeight,
                              o_nBandOffset =>nDummy
                             );
          lColHeaderHeights(i_nCol):=GREATEST(lColHeaderHeights(i_nCol), nHeight);
        END IF;
      END IF;
      IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
        vcIndex:=lColGroups(i_nGroupListIdx).FIRST;
      ELSE
        vcIndex:=lColGroups(i_nGroupListIdx).LAST;
      END IF;

      WHILE vcIndex IS NOT NULL LOOP
        PR_SUMMARY_VARIABLES(i_nCurrentRow =>1,
                             i_nCurrentCol =>i_nCol);
        -- put the value of the col-value into the variable-cache
        PR_SET_VARIABLE_FROM_CROSSTAB(i_vcName    =>i_rCrosstab.lColumns(i_nCol).vcName,
                                      i_vcDatatype=>i_rCrosstab.lColumns(i_nCol).vcDatatype,
                                      i_rEntry    =>lColGroups(i_nGroupListIdx)(vcIndex)
                                     );
        IF i_nCol<i_rCrosstab.lColumns.COUNT THEN
          -- calculate the height for the current header, if its not the last
          PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                              i_rBand       =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nHeaderPos),
                              o_nbandHeight =>nHeight,
                              o_nBandOffset =>nDummy
                             );
          lColHeaderHeights(i_nCol):=GREATEST(lColHeaderHeights(i_nCol), nHeight);

          PR_REKU_COLUMN_HEIGHTS(i_nColMatrixIdx=>i_nColMatrixIdx,
                                 i_nCol         =>i_nCol+1,
                                 i_nGroupListIdx=>lColGroups(i_nGroupListIdx)(vcIndex).iDetailList,
                                 i_vcColKey     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_'
                                );
        ELSE
          -- last level, add column to layout
          PR_COLUMN_HEIGHT(i_nCol         =>i_nCol,
                           i_nColMatrixIdx=>i_nColMatrixIdx,
                           i_vcColIndex   =>vcIndex,
                           i_vcColkey     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_'
                           );
        END IF;
        IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
          vcIndex:=lColGroups(i_nGroupListIdx).NEXT(vcIndex);
        ELSE
          vcIndex:=lColGroups(i_nGroupListIdx).PRIOR(vcIndex);
        END IF;
      END LOOP;
    END;

    FUNCTION FK_RENDER_COL(i_nCol          IN  NUMBER,
                           i_nColMatrixIdx IN  NUMBER,
                           i_vcColIndex    IN  PK_JRXML2PDF_TYPES.tIndex,
                           i_vcColkey      IN  PK_JRXML2PDF_TYPES.tIndex,
                           i_nColWidth     IN  NUMBER,
                           i_nYOffset      IN  NUMBER,
                           o_bInSet        OUT BOOLEAN
                          )
    RETURN PK_JRXML2PDF_TYPES.tArea IS
      nCellHeight NUMBER;
      nCellOffset NUMBER;
      rBand       PK_JRXML2PDF_TYPES.tBand;
    BEGIN
      o_bInSet:=FK_CHECK_IN_COLSET(i_nColMatrixIdx, i_vcColKey);
      IF NOT o_bInset THEN
        rDummyArea:=rArea;
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render column Num/Key:' || TO_CHAR(i_nCol) || '/' || i_vcColkey);
        END IF;
        rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nHeaderPos);
        -- Calc height for row-part
        PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                            i_rBand       =>rBand,
                            o_nbandHeight =>nCellHeight,
                            o_nBandOffset =>nCellOffset
                           );
        IF lColHeaderheights(i_nCol)<nCellHeight THEN
          rBand.nHeight:=lColHeaderheights(i_nCol);
        END IF;
        -- Render the row-header
        rDummyArea.nX:=i_nColWidth;
        rDummyArea.nY:=rArea.nY+i_nYOffset;
        rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                   i_rBand           =>rBand,
                                   i_rArea           =>rDummyArea,
                                   i_bAllowSubReports=>FALSE,
                                   i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);
        rDummyArea.nX:=i_nColWidth+rBand.nWidth;
        o_bInSet:=TRUE;
      END IF;
      RETURN rDummyArea;
    END;

    FUNCTION FK_REKU_RENDER_COLS(i_nColMatrixIdx IN NUMBER,
                                 i_nCol          IN NUMBER,
                                 i_nGroupListIdx IN NUMBER,
                                 i_vcColKey      IN PK_JRXML2PDF_TYPES.tIndex,
                                 i_nMinHeight    IN NUMBER,
                                 i_nXOffset      IN NUMBER,
                                 i_nYOffset      IN NUMBER,
                                 o_bInSet        OUT BOOLEAN)
    RETURN PK_JRXML2PDF_TYPES.tArea IS
      vcIndex       PK_JRXML2PDF_TYPES.tIndex;
      nMinHeight    NUMBER;
      nDummy        NUMBER;
      nYOffset      NUMBER;
      rDummyArea    PK_JRXML2PDF_TYPES.tArea:=rArea;
      rReturnArea   PK_JRXML2PDF_TYPES.tArea;
      rBand         PK_JRXML2PDF_TYPES.tBand;
      nStartWidth   NUMBER;
      bInSet        BOOLEAN;
      nXForTotal    NUMBER;

    BEGIN
      o_bInSet:=FALSE;
      rReturnArea.nX:=i_nXOffset;
      rReturnArea.nY:=i_nYOffset;
      IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                            i_vcColKey     =>i_vcColKey) THEN
        IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='Start' THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Keep space for total at start col/key:' || TO_CHAR(i_nCol) || '/' || i_vcColKey);
          END IF;
          -- Render the col-header for this column
          rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos);
          -- just remeber the x-position and reserve space
          nXForTotal:=rReturnArea.nX;
          rReturnArea.nX:=rReturnArea.nX+rBand.nWidth;
        END IF;
      END IF;
      IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
        vcIndex:=lColGroups(i_nGroupListIdx).FIRST;
      ELSE
        vcIndex:=lColGroups(i_nGroupListIdx).LAST;
      END IF;
      WHILE vcIndex IS NOT NULL LOOP
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render column col/key:' || TO_CHAR(i_nCol) || '/' || vcIndex);
        END IF;

        lColKeys(i_nCol):=TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_';

        PR_SUMMARY_VARIABLES(i_nCurrentRow =>1,
                             i_nCurrentCol =>i_nCol);

        -- put the value of the col-value into the variable-cache
        PR_SET_VARIABLE_FROM_CROSSTAB(i_vcName    =>i_rCrosstab.lColumns(i_nCol).vcName,
                                      i_vcDatatype=>i_rCrosstab.lColumns(i_nCol).vcDatatype,
                                      i_rEntry    =>lColGroups(i_nGroupListIdx)(vcIndex)
                                     );

        IF i_nCol<i_rCrosstab.lColumns.COUNT THEN
          -- calculate the height for the current header, if its not the last
          nYOffset:=i_rCrosstab.lColumns(i_nCol).nHeight;
          nStartWidth:=rReturnArea.nX;
          rDummyArea:=FK_REKU_RENDER_COLS(i_nColMatrixIdx=>i_nColMatrixIdx,
                                          i_nCol         =>i_nCol+1,
                                          i_nGroupListIdx=>lColGroups(i_nGroupListIdx)(vcIndex).iDetailList,
                                          i_vcColKey     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_',
                                          i_nMinHeight   =>GREATEST(nMinHeight, i_nMinHeight),
                                          i_nXOffset     =>rReturnArea.nX,
                                          i_nYOffset     =>i_nYOffset+nYOffset,
                                          o_bInSet       =>bInSet
                                         );
          IF bInset THEN
            PR_SUMMARY_VARIABLES(i_nCurrentRow =>1,
                                 i_nCurrentCol =>i_nCol);

            rReturnArea.nX:=rDummyArea.nX;
            rReturnArea.nY:=GREATEST(rDummyArea.nY, rReturnArea.nY);

            -- Render the col-header for this column
            rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nHeaderPos);
            rBand.nX:=0;
            rBand.nWidth:=rReturnArea.nX-nStartWidth;
            rDummyArea.nY:=rArea.nY+i_nYOffset;
            rDummyArea.nX:=nStartWidth;
            rDummyArea:=FK_RENDER_BAND(i_nStackPos          =>i_nStackPos,
                                       i_rBand              =>rBand,
                                       i_rArea              =>rDummyArea,
                                       i_bAllowSubReports   =>FALSE,
                                       i_nRenderFrame       =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER,
                                       i_vcContentAdjustment=>i_rCrosstab.lColumns(i_nCol).vcHeaderPosition
                                      );
            nStartWidth:=rDummyArea.nX+rBand.nWidth;
            rReturnArea.nX:=rDummyArea.nX+rBand.nWidth;
            rReturnArea.nY:=GREATEST(rDummyArea.nY, rReturnArea.nY);
            o_bInSet:=TRUE;
          END IF;
        ELSE
          -- last level, add column to layout
          rDummyArea:=FK_RENDER_COL(i_nCol         =>i_nCol,
                                    i_nColMatrixIdx=>i_nColMatrixIdx,
                                    i_vcColIndex   =>vcIndex,
                                    i_vcColkey     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_',
                                    i_nColWidth    =>rReturnArea.nX,
                                    i_nYOffset     =>i_nYOffset,
                                    o_bInSet       =>bInSet
                                   );
          IF bInSet THEN
            rReturnArea.nX:=rDummyArea.nX;
            rReturnArea.nY:=GREATEST(rDummyArea.nY, rReturnArea.nY);
            o_bInSet:=TRUE;
          END IF;
        END IF;
        IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
          vcIndex:=lColGroups(i_nGroupListIdx).NEXT(vcIndex);
        ELSE
          vcIndex:=lColGroups(i_nGroupListIdx).PRIOR(vcIndex);
        END IF;
      END LOOP;

      lColKeys.DELETE(i_nCol);

      IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                            i_vcColKey     =>i_vcColKey) THEN
        IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='Start' THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render total at start col/key:' || TO_CHAR(i_nCol) || '/' || i_vcColKey);
          END IF;
          -- Render the col-header for this column
          rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos);
          rDummyArea.nY:=rArea.nY+i_nYOffset;
          -- set height
          --rBand.nHeight:=lColHeaderHeights(i_nCol);
          rBand.nHeight:=rReturnArea.nY-rDummyArea.nY;
          -- x-position is the one saved at start
          rDummyArea.nX:=nXForTotal;
          rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                     i_rBand           =>rBand,
                                     i_rArea           =>rDummyArea,
                                     i_bAllowSubReports=>FALSE,
                                     i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);
          rReturnArea.nY:=GREATEST(rDummyArea.nY, rReturnArea.nY);
          o_bInSet:=TRUE;
        ELSIF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='End' THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render total at end col/key:' || TO_CHAR(i_nCol) || '/' || i_vcColKey);
          END IF;
          -- Render the col-header for this column
          rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lColumns(i_nCol).nTotalHeaderPos);
          rDummyArea.nY:=rArea.nY+i_nYOffset;
          -- set height
          rBand.nHeight:=GREATEST(rReturnArea.nY-rDummyArea.nY, rBand.nHeight);

          rDummyArea.nX:=rReturnArea.nX;
          rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                     i_rBand           =>rBand,
                                     i_rArea           =>rDummyArea,
                                     i_bAllowSubReports=>FALSE,
                                     i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);
          rReturnArea.nX:=rDummyArea.nX+rBand.nWidth;
          rReturnArea.nY:=GREATEST(rDummyArea.nY, rReturnArea.nY);
          o_bInSet:=TRUE;
        END IF;
      END IF;

      RETURN rReturnArea;
    END;

    PROCEDURE PR_RENDER_ROWHEADER(i_nHeaderPos IN NUMBER,
                                  i_bRenderAll IN BOOLEAN
                                 ) IS
      rDummyArea PK_JRXML2PDF_TYPES.tArea;
      rBand      PK_JRXML2PDF_TYPES.tBand;
      iStart     PLS_INTEGER:=i_nHeaderPos;
      iEnd       PLS_INTEGER:=i_nHeaderPos;
    BEGIN
      IF NOT bNoRowHeaders
         AND lRowHeaders.COUNT>0 THEN
        IF i_bRenderAll THEN
          iEnd:=1;
        END IF;
        FOR i IN REVERSE iStart..iEnd LOOP
          -- Render the row-header for this row
          rBand:=lRowHeaders(i).rBand;
          rBand.nHeight:=rArea.nY-lRowHeaders(i).nStartY;
          rDummyArea.nX:=lRowHeaders(i).nStartX;
          rDummyArea.nY:=lRowHeaders(i).nStartY;
          rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                     i_rBand           =>rBand,
                                     i_rArea           =>rDummyArea,
                                     i_bAllowSubReports=>FALSE,
                                     i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER,
                                     i_vcContentAdjustment=>i_rCrosstab.lRows(i).vcHeaderPosition
                                    );
        END LOOP;
      END IF;
    END;

    FUNCTION FK_CALC_ROW_HEIGHT(i_nRow          IN  NUMBER,
                                i_nColMatrixIdx IN  NUMBER,
                                i_nMinHeight    IN  NUMBER,
                                i_vcColBandName IN  PK_JRXML2PDF_TYPES.tName,
                                i_rRowBand      IN  PK_JRXML2PDF_TYPES.tBand)
    RETURN NUMBER IS
      nRowheight  NUMBER:=i_nMinHeight;
      nCellHeight NUMBER;
      nCellOffset NUMBER;
      rCell       PK_JRXML2PDF_TYPES.tCrossTabCell;
      vcIndex     PK_JRXML2PDF_TYPES.tIndex;
    BEGIN
      IF i_rCrosstab.lCells.EXISTS(i_rCrosstab.lRows(i_nRow).vcName || '_') THEN
        rCell:=i_rCrosstab.lCells(i_rCrosstab.lRows(i_nRow).vcName || '_');
      ELSE
        rCell:=i_rCrosstab.lCells('_');
      END IF;
      -- Calc height for row-part
      PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                          i_rBand       =>i_rRowBand,
                          o_nbandHeight =>nCellHeight,
                          o_nBandOffset =>nCellOffset
                         );
      nRowHeight:=GREATEST(nRowHeight, nCellHeight);
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Calculate max row height for row :' || i_rCrosstab.lRows(i_nRow).vcName || '/' || TO_CHAR(nCellHeight));
      END IF;

      -- calculate height for each cell, take the maximum for the whole row
      vcIndex:=lColumnMatrix(i_nColMatrixIdx).FIRST;
      WHILE vcIndex IS NOT NULL LOOP
        -- get the cell matric the column-value
        IF i_rCrosstab.lCells.EXISTS(i_vcColBandName || '_' || lColumnMatrix(i_nColMatrixIdx)(vcIndex)) THEN
          rCell:=i_rCrosstab.lCells(i_vcColBandName || '_' || lColumnMatrix(i_nColMatrixIdx)(vcIndex));
        ELSIF i_rCrosstab.lCells.EXISTS(i_vcColBandName || '_') THEN
          rCell:=i_rCrosstab.lCells(i_vcColBandName || '_');
        ELSE
          rCell:=i_rCrosstab.lCells('_');
        END IF;
        -- not found then get the matrix-cell itself

        PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                            i_rBand       =>lReportStack(i_nStackPos).lBands(rCell.nCellPos),
                            o_nbandHeight =>nCellHeight,
                            o_nBandOffset =>nCellOffset
                           );
        nRowHeight:=GREATEST(nRowHeight, nCellHeight);
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Calculate max row height for row :' || i_rCrosstab.lRows(i_nRow).vcName || '/' || vcIndex || '/' || TO_CHAR(nCellHeight));
        END IF;
        vcIndex:=lColumnMatrix(i_nColMatrixIdx).NEXT(vcIndex);
      END LOOP;
      RETURN nRowHeight;
    END;

    FUNCTION FK_RENDER_ROW(i_nRow          IN  NUMBER,
                           i_nColMatrixIdx IN  NUMBER,
                           i_vcRowIndex    IN  PK_JRXML2PDF_TYPES.tIndex,
                           i_vcRowkey      IN  PK_JRXML2PDF_TYPES.tIndex,
                           i_nMinHeight    IN  NUMBER,
                           i_nXOffset      IN  NUMBER,
                           i_rRowband      IN  PK_JRXML2PDF_TYPES.tBand,
                           i_vcColBandName IN  PK_JRXML2PDF_TYPES.tName)
    RETURN PK_JRXML2PDF_TYPES.tArea IS
      nRowheight  NUMBER:=i_nMinHeight;
      nColHeight  NUMBER:=0;
      nCellHeight NUMBER;
      nCellOffset NUMBER;
      rDummyArea  PK_JRXML2PDF_TYPES.tArea;
      rCell       PK_JRXML2PDF_TYPES.tCrossTabCell;
      rBand       PK_JRXML2PDF_TYPES.tBand;
      rDataCell   tCelldataList;
      rEmptyCell  tCellDataList;
      vcIndex     PK_JRXML2PDF_TYPES.tName;
      nDummy      NUMBER;
      rReturnArea PK_JRXML2PDF_TYPES.tArea;
      nTempY      NUMBER;
      
      PROCEDURE PR_RENDER_CELL(i_nCol IN NUMBER, i_vcColKey IN VARCHAR2) IS
        vcname  PK_JRXML2PDF_TYPES.tName;
        vcIndex PK_JRXML2PDF_TYPES.tName;
      BEGIN
        vcName:=lColumnMatrix(i_nColMatrixIdx)(NVL(i_vcColKey, PK_JRXML2PDF_TYPES.NIL));

        -- set summary values
        PR_SUMMARY_VARIABLES(i_nCurrentRow =>i_nRow,
                             i_nCurrentCol =>i_nCol);

      -- get the cell matric the column-value
        IF i_rCrosstab.lCells.EXISTS(i_vcColBandName || '_' || vcName) THEN
          rCell:=i_rCrosstab.lCells(i_vcColBandName || '_' || vcName);
        ELSIF i_rCrosstab.lCells.EXISTS(i_vcColBandName || '_') THEN
          rCell:=i_rCrosstab.lCells(i_vcColBandName || '_');
        ELSE
          rCell:=i_rCrosstab.lCells('_');
        END IF;
        rDummyArea.nY:=rArea.nY;
        rDummyArea.nPage:=rArea.nPage;
        -- now set the cell-values as variables
        vcIndex:=i_vcRowKey  || '#' || i_vcColKey;
        IF lCells.EXISTS(vcIndex) THEN
          rDataCell:=lCells(vcIndex);
        ELSE
          rDataCell:=rEmptyCell;
        END IF;
        FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
          -- set variable with value
          IF rDataCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN
            PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                      i_vcName   =>i_rCrosstab.lMeasures(i).vcName,
                                      i_rData    =>rDataCell(i_rCrosstab.lMeasures(i).vcName),
                                      i_rMeasure =>i_rCrosstab.lMeasures(i)
                                     );
          ELSE
            -- Type count will return 0, others NULL
            IF    i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_COUNT
               OR i_rCrosstab.lMeasures(i).vcCalculation=PK_JRXML2PDF_TYPES.CALCULATION_SUM THEN
              PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                        i_vcName   =>i_rCrosstab.lMeasures(i).vcName,
                                        i_rData    =>rEmptydata,
                                        i_rMeasure =>i_rCrosstab.lMeasures(i)
                                       );
            ELSE
              -- set variable to null
              PR_UNSET_VARIABLE(i_nStackPos=>i_nStackPos,
                                i_vcName   =>i_rCrosstab.lMeasures(i).vcname
                               );
            END IF;
          END IF;
        END LOOP;
        rBand:=lReportStack(i_nStackPos).lBands(rCell.nCellPos);
        rBand.nHeight:=nRowHeight;

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render cell :' || TO_CHAR(i_nCol)  || '/' || i_vcColKey);
        END IF;

        rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                   i_rBand           =>rBand,
                                   i_rArea           =>rDummyArea,
                                   i_bAllowSubReports=>FALSE,
                                   i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);
        rDummyArea.nX:=rDummyArea.nX+rBand.nWidth;
      END;

      PROCEDURE PR_REKU_RENDER_CELLS(i_nCol          IN NUMBER,
                                     i_nGroupListIdx IN NUMBER,
                                     i_vcColKey      IN PK_JRXML2PDF_TYPES.tName) IS
        vcIndex PK_JRXML2PDF_TYPES.tName;
      BEGIN
        IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                              i_vcColKey     =>i_vcColKey) THEN
          IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='Start' THEN
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render cell total at start:' || TO_CHAR(i_nCol)  || '/' || i_vcColKey);
            END IF;
            -- Render the col-header for this column
            --lColumnMatrix(i_nColMatrixIdx)(i).vcName
            PR_RENDER_CELL(i_nCol    =>i_nCol,
                           i_vcColKey=>i_vcColKey);
          END IF;
        END IF;
        IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
          vcIndex:=lColGroups(i_nGroupListIdx).FIRST;
        ELSE
          vcIndex:=lColGroups(i_nGroupListIdx).LAST;
        END IF;
        WHILE vcIndex IS NOT NULL LOOP
          lColKeys(i_nCol):=TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_';

          IF i_nCol<i_rCrosstab.lColumns.COUNT THEN
            -- calculate the height for the current header, if its not the last
            PR_REKU_RENDER_CELLS(i_nCol         =>i_nCol+1,
                                 i_nGroupListIdx=>lColGroups(i_nGroupListIdx)(vcIndex).iDetailList,
                                 i_vcColKey     =>TO_CHAR(lColGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_'
                                );
          ELSE
            IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                                  i_vcColKey     =>lColKeys(i_nCol)) THEN
              PR_RENDER_CELL(i_nCol    =>i_nCol,
                             i_vcColKey=>lColKeys(i_nCol)
                            );
            END IF;

          END IF;
          IF i_rCrosstab.lColumns(i_nCol).vcOrderBy=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
            vcIndex:=lColGroups(i_nGroupListIdx).NEXT(vcIndex);
          ELSE
            vcIndex:=lColGroups(i_nGroupListIdx).PRIOR(vcIndex);
          END IF;
          lColKeys.DELETE(i_nCol);
        END LOOP;

        IF FK_CHECK_IN_COLSET(i_nColMatrixIdx=>i_nColMatrixIdx,
                              i_vcColKey     =>i_vcColKey) THEN
          IF i_rCrosstab.lColumns(i_nCol).vcTotalPosition='End' THEN
            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render cell total at end:' || TO_CHAR(i_nCol)  || '/' || i_vcColKey);
            END IF;
            -- Render the col-header for this column
            PR_RENDER_CELL(i_nCol    =>i_nCol,
                           i_vcColKey=>i_vcColKey
                          );
          END IF;
        END IF;
      END;

    BEGIN
      rReturnArea.nX:=i_nXOffset;

      IF bNeedsColumnHeader THEN
        FOR i IN 1..lColHeaderHeights.COUNT LOOP
          nColHeight:=nColheight+lColHeaderHeights(i);
        END LOOP;
      END IF;

      nRowHeight:=FK_CALC_ROW_HEIGHT(i_nRow         =>i_nRow,
                                     i_nColMatrixIdx=>i_nColMatrixIdx,
                                     i_nMinHeight   =>i_nMinHeight,
                                     i_vcColBandName=>i_vcColBandName,
                                     i_rRowband     =>i_rRowband
                                    );
      -- Check if region fits on page
      IF NOT FK_FITS_IN_PAGE(rArea.nY, nColHeight+nRowHeight) THEN
        -- finish open row-headers
        PR_RENDER_ROWHEADER(i_nHeaderPos=>lRowHeaders.COUNT,
                            i_bRenderAll=>TRUE
                           );

        -- skip to last record to have the correct data in the buffer
        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render crostab-row on new page,record:' || lReportStack(i_nStackPos).rQuery.nRecordPosition);
        END IF;

        PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                     i_nStackPos     =>i_nStackPos,
                                     i_bOneRecordBack=>TRUE);

        -- check, if Colheaders have to be repeated
        IF i_rCrosstab.vcRepeatColHeaders=PK_JRXML2PDF_TYPES.YES THEN

          rDummyArea:=FK_REKU_RENDER_COLS(i_nColMatrixIdx =>i_nColMatrixIdx,
                                          i_nCol          =>1,
                                          i_nGroupListIdx =>1,
                                          i_vcColKey      =>NULL,
                                          i_nMinHeight    =>0,
                                          i_nXOffset      =>nRowOffset,
                                          i_nYOffset      =>0,
                                          o_bInSet        =>bDummy);
          nTempY:=rArea.nY;
          rArea.nY:=rDummyArea.nY;
        END IF;
        -- set the start-positions for the rowheaders
        FOR i IN 1..lRowHeaders.COUNT LOOP
          lRowHeaders(i).nStartY:=rArea.nY;
        END LOOP;
        IF i_rCrosstab.rHeaderCell.nCellPos IS NOT NULL THEN
          -- render the Headercell
           rHeaderBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.rHeaderCell.nCellPos);
           rHeaderBand.nHeight:=rArea.nY-nTempY;
           rHeaderBand.nWidth:=nRowOffset;
           rDummyArea.nx:=rReturnArea.nX;
           rDummyArea.nY:=nTempY;
           rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                      i_rBand           =>rHeaderBand,
                                      i_rArea           =>rDummyArea,
                                      i_bAllowSubReports=>FALSE,
                                      i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);

        END IF;
        -- Reset flag for column-headers, are already done
        bNeedsColumnHeader:=FALSE;
      ELSE
        IF bNeedsColumnHeader THEN
          bNeedsColumnHeader:=FALSE;
          rDummyArea:=FK_REKU_RENDER_COLS(i_nColMatrixIdx =>i_nColMatrixIdx,
                                          i_nCol          =>1,
                                          i_nGroupListIdx =>1,
                                          i_vcColKey      =>NULL,
                                          i_nMinHeight    =>0,
                                          i_nXOffset      =>nRowOffset,
                                          i_nYOffset      =>0,
                                          o_bInSet        =>bDummy);
          nTempY:=rArea.nY;
          rArea.nY:=rDummyArea.nY;
          -- set the start-positions for the rowheaders
          FOR i IN 1..lRowHeaders.COUNT LOOP
            lRowHeaders(i).nStartY:=rArea.nY;
          END LOOP;
          IF i_rCrosstab.rHeaderCell.nCellPos IS NOT NULL THEN
            -- render the Headercell
             rHeaderBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.rHeaderCell.nCellPos);
             rHeaderBand.nHeight:=rArea.nY-nTempY;
             rHeaderBand.nWidth:=nRowOffset;
             rDummyArea.nx:=rReturnArea.nX;
             rDummyArea.nY:=nTempY;
             rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                        i_rBand           =>rHeaderBand,
                                        i_rArea           =>rDummyArea,
                                        i_bAllowSubReports=>FALSE,
                                        i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);

          END IF;
        END IF;
      END IF;

      PR_SUMMARY_VARIABLES(i_nCurrentRow =>i_nRow,
                           i_nCurrentCol =>1
                          );

      rDummyArea.nX:=rArea.nX+i_nXOffset;
      rDummyArea.nY:=rArea.nY;

      rBand:=i_rRowband;
      rBand.nHeight:=nRowHeight;
      IF NOT bNoRowHeaders THEN
        -- set cell-measures to row-values
        vcIndex:=i_vcRowKey  || '#';
        IF lCells.EXISTS(vcIndex) THEN
          rDataCell:=lCells(vcIndex);
        ELSE
          rDataCell:=rEmptyCell;
        END IF;
        FOR i IN 1..i_rCrosstab.lMeasures.COUNT LOOP
          -- set variable with value
          IF rDataCell.EXISTS(i_rCrosstab.lMeasures(i).vcName) THEN
            PR_SET_VARIABLE_FROM_DATA(i_nStackPos=>i_nStackPos,
                                      i_vcName   =>i_rCrosstab.lMeasures(i).vcName,
                                      i_rData    =>rDataCell(i_rCrosstab.lMeasures(i).vcName),
                                      i_rMeasure =>i_rCrosstab.lMeasures(i)
                                     );
          ELSE
            -- set variable to null
            PR_UNSET_VARIABLE(i_nStackPos=>i_nStackPos,
                              i_vcName   =>i_rCrosstab.lMeasures(i).vcname
                             );
          END IF;
        END LOOP;

        -- Render the row-header
        rDummyArea:=FK_RENDER_BAND(i_nStackPos       =>i_nStackPos,
                                   i_rBand           =>rBand,
                                   i_rArea           =>rDummyArea,
                                   i_bAllowSubReports=>FALSE,
                                   i_nRenderFrame    =>PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER);
        rDummyArea.nX:=rArea.nX+nRowOffset;
      ELSE
        rDummyArea.nX:=rArea.nX;
      END IF;

      PR_REKU_RENDER_CELLS(1, 1, NULL);

      rArea.nY:=rArea.nY+nRowHeight;
      rReturnArea.nX:=i_nXOffset+i_rRowBand.nWidth;

      RETURN rReturnArea;
    END;

    FUNCTION FK_REKU_RENDER_ROWS(i_nColMatrixIdx IN NUMBER,
                                 i_nRow          IN NUMBER,
                                 i_nGroupListIdx IN NUMBER,
                                 i_vcRowKey      IN PK_JRXML2PDF_TYPES.tIndex,
                                 i_nMinHeight    IN NUMBER,
                                 i_nXOffset      IN NUMBER)
    RETURN PK_JRXML2PDF_TYPES.tArea IS
      vcIndex       PK_JRXML2PDF_TYPES.tName;
      nMinHeight    NUMBER;
      nDummy        NUMBER;
      rDummyArea    PK_JRXML2PDF_TYPES.tArea:=rArea;
      rBand         PK_JRXML2PDF_TYPES.tBand;
      rReturnArea   PK_JRXML2PDF_TYPES.tArea;
      nNewheight    NUMBER;
      nStartY       NUMBER;
      iHeader       PLS_INTEGER;
      nYForTotal    NUMBER;
      nYSave        NUMBER;
    BEGIN
      rReturnArea.nX:=0;
      rReturnArea.nY:=rArea.nY;
      IF i_rCrosstab.lRows(i_nRow).vcTotalPosition='Start' THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Keep space for row total at start:' || TO_CHAR(i_nRow)  || '/' || i_vcRowKey);
        END IF;
        -- save psotion for row-header
        nYForTotal:=rReturnArea.nY;
        -- calculate the row-height
        rArea.nY:=rArea.nY+FK_CALC_ROW_HEIGHT(i_nRow         =>i_nRow,
                                              i_nColMatrixIdx=>i_nColMatrixIdx,
                                              i_nMinHeight   =>i_nMinHeight,
                                              i_vcColBandName=>NULL,
                                              i_rRowBand     =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nTotalHeaderPos)
                                             );
      END IF;
      IF i_rCrosstab.lRows(i_nRow).vcOrderby=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
        vcIndex:=lRowGroups(i_nGroupListIdx).FIRST;
      ELSE
        vcIndex:=lRowGroups(i_nGroupListIdx).LAST;
      END IF;
      WHILE vcIndex IS NOT NULL LOOP
        lRowKeys(i_nRow):=TO_CHAR(lRowGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_';

        PR_SUMMARY_VARIABLES(i_nCurrentRow =>i_nRow,
                             i_nCurrentCol =>1);

        -- put the value of the row-value into the variable-cache
        PR_SET_VARIABLE_FROM_CROSSTAB(i_vcName    =>i_rCrosstab.lRows(i_nRow).vcName,
                                      i_vcDatatype=>i_rCrosstab.lRows(i_nRow).vcDatatype,
                                      i_rEntry    =>lRowGroups(i_nGroupListIdx)(vcIndex)
                                     );

        IF i_nRow<i_rCrosstab.lRows.COUNT THEN
          -- Calc height for row-part
          PR_CALC_BAND_HEIGHT(i_nStackPos   =>i_nStackPos,
                              i_rBand       =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nHeaderPos),
                              o_nbandHeight =>nMinHeight,
                              o_nBandOffset =>nDummy
                             );

          IF NOT bNoRowHeaders THEN
            iHeader:=lRowHeaders.COUNT+1;
            lRowHeaders(iHeader).nStartY:=rArea.nY;
            lRowHeaders(iHeader).nStartX:=rArea.nX+i_nXOffset;
            lRowHeaders(iHeader).rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nHeaderPos);
            nStartY:=rArea.nY;
            rReturnArea.nX:=i_rCrosstab.lRows(i_nRow).nWidth;
          END IF;
          rDummyArea:=FK_REKU_RENDER_ROWS(i_nColMatrixIdx=>i_nColMatrixIdx,
                                          i_nRow         =>i_nRow+1,
                                          i_nGroupListIdx=>lRowGroups(i_nGroupListIdx)(vcIndex).iDetailList,
                                          i_vcRowKey     =>TO_CHAR(lRowGroups(i_nGroupListIdx)(vcIndex).iDetailList)  || '_',
                                          i_nMinHeight   =>0,
                                          i_nXOffset     =>i_nXOffset+rReturnArea.nX
                                         );
          rReturnArea.nY:=NVL(rDummyArea.nY, rReturnArea.nY);

          PR_SUMMARY_VARIABLES(i_nCurrentRow =>i_nRow,
                               i_nCurrentCol =>1);

          rReturnArea.nX:=rDummyArea.nX;
          IF NOT bNoRowHeaders THEN
            PR_RENDER_ROWHEADER(i_nHeaderPos=>lRowHeaders.COUNT,
                                i_bRenderAll=>FALSE
                               );
          END IF;
          rReturnArea.nY:=rArea.nY;
          lRowHeaders.DELETE(lRowHeaders.COUNT);
        ELSE
          -- last level, add column to layout
          rDummyArea:=FK_RENDER_ROW(i_nRow         =>i_nRow,
                                    i_nColMatrixIdx=>i_nColMatrixIdx,
                                    i_vcRowIndex   =>vcIndex,
                                    i_vcRowkey     =>TO_CHAR(lRowGroups(i_nGroupListIdx)(vcIndex).iDetailList) || '_',
                                    i_nMinHeight   =>i_nMinHeight,
                                    i_nXOffset     =>i_nXOffset,
                                    i_rRowband     =>lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nHeaderPos),
                                    i_vcColBandName=>NULL);
          rReturnArea.nX:=GREATEST(rReturnArea.nX, rDummyArea.nX);
        END IF;
        IF i_rCrosstab.lRows(i_nRow).vcOrderby=PK_JRXML2PDF_TYPES.ORDERBY_ASCENDING THEN
          vcIndex:=lRowGroups(i_nGroupListIdx).NEXT(vcIndex);
        ELSE
          vcIndex:=lRowGroups(i_nGroupListIdx).PRIOR(vcIndex);
        END IF;

      END LOOP;
      lRowKeys.DELETE(i_nRow);

      IF i_rCrosstab.lRows(i_nRow).vcTotalPosition='Start' THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render row total at start:' || TO_CHAR(i_nRow)  || '/' || i_vcRowKey);
        END IF;
        -- Render the col-header for this column
        rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nTotalHeaderPos);
        rBand.nWidth:=rReturnArea.nX-i_nXOffset;
        -- set the Y-position to the formerly saved value
        nYSave:=rArea.nY;
        rArea.nY:=nYForTotal;
        -- last level, add column to layout
        rDummyArea:=FK_RENDER_ROW(i_nRow         =>i_nRow,
                                  i_nColMatrixIdx=>i_nColMatrixIdx,
                                  i_vcRowIndex   =>vcIndex,
                                  i_vcRowkey     =>i_vcRowKey,
                                  i_nMinHeight   =>i_nMinHeight,
                                  i_nXOffset     =>i_nXOffset,
                                  i_rRowband     =>rBand,
                                  i_vcColBandName=>i_rCrosstab.lRows(i_nRow).vcName
                                 );
       rArea.nY:=nYSave;
      ELSIF i_rCrosstab.lRows(i_nRow).vcTotalPosition='End' THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render row total at end:' || TO_CHAR(i_nRow)  || '/' || i_vcRowKey);
        END IF;
        -- Render the col-header for this column
        rBand:=lReportStack(i_nStackPos).lBands(i_rCrosstab.lRows(i_nRow).nTotalHeaderPos);
        rBand.nWidth:=rReturnArea.nX-i_nXOffset;

        -- last level, add column to layout
        rDummyArea:=FK_RENDER_ROW(i_nRow         =>i_nRow,
                                  i_nColMatrixIdx=>i_nColMatrixIdx,
                                  i_vcRowIndex   =>vcIndex,
                                  i_vcRowkey     =>i_vcRowKey,
                                  i_nMinHeight   =>i_nMinHeight,
                                  i_nXOffset     =>i_nXOffset,
                                  i_rRowband     =>rBand,
                                  i_vcColBandName=>i_rCrosstab.lRows(i_nRow).vcName
                                 );
      END IF;

      RETURN rReturnArea;
    END;

    PROCEDURE PR_DEBUG_DATA IS
      vcIndex PK_JRXML2PDF_TYPES.tName;
    BEGIN
      vcIndex:=lCells.FIRST;
      WHILE vcIndex IS NOT NULL LOOP
        -- put the value of the row-value into the variable-cache
        FOR i IN 1..i_rCrossTab.lMeasures.COUNT LOOP
           NULL;--  DBMS_OUTPUT.PUT_LINE(lCells(vcIndex)(i_rCrossTab.lMeasures(i).vcName).iCount);
        END LOOP;
        vcIndex:=lCells.NEXT(vcIndex);
      END LOOP;
    END;
  BEGIN
    -- Get the data for the crosstab,
    -- and calculate aggregates
    PR_LOAD_DATA;

    --PR_DEBUG_DATA;
    -- Calculate area for pagefooter
    nColumnFooterHeight:=0;
    nPageFooterheight:=0;
    FOR i IN 1..lReportStack.COUNT LOOP
      nColumnFooterHeight:=nColumnFooterHeight+lReportStack(i).nColumnFtrHeight;
      nPageFooterheight  :=nPageFooterheight  +lReportStack(i).nPageFtrHeight;
    END LOOP;

    -- set up the matrix width by calculating how many column fit in each row
    nRowOffset:=0;
    nColPosition:=999999;
    nColGroupIdx:=0;
    -- calculate the space for the row-Headers in front
    FOR i IN 1..i_rCrosstab.lRows.COUNT LOOP
      nRowOffset:=nRowOffset+lReportstack(i_nStackPos).lBands(i_rCrosstab.lRows(i).nHeaderPos).nWidth;
    END LOOP;

    -- now add all last-level columns from the data
    PR_REKU_ADD_COLUMN(1, 1, NULL);

    -- now we have one or more list of columns each of which forms one column-header, so
    --
    -- row1 row2 ColVal1 ColVal2 ColVal3 ColVal4 ColTotal
    --
    -- maybe split into
    --
    -- row1 row2 ColVal1 ColVal2 ColVal3
    -- row1 row2 ColVal4 ColTotal
    -- and each of the column-header will be rendered for all rows in the rowgroups

    FOR iColMatrixIdx IN 1..lColumnMatrix.COUNT LOOP
      -- calculate the heights of all column-headers
      lColHeaderHeights.DELETE;
      FOR i IN 1..i_rCrosstab.lColumns.COUNT LOOP
        lColHeaderHeights(i):=0;
      END LOOP;

      PR_REKU_COLUMN_HEIGHTS(i_nColMatrixIdx=>iColMatrixIdx,
                             i_nCol         =>1,
                             i_nGroupListIdx=>1,
                             i_vcColKey     =>NULL
                            );

      -- maybe do not print the row-headers anymore
      IF     iColMatrixIdx>1
         AND i_rCrosstab.vcRepeatRowHeaders=PK_JRXML2PDF_TYPES.NO THEN
        nRowOffset:=0;
        bNoRowHeaders:=TRUE;
      END IF;

      -- Set flag, that the column-headers have to be rendered
      -- this is checked inside RENDER_ROW to make sure that
      -- the colheader is only rendered if at least one row of data fits beneath the header
      bNeedsColumnHeader:=TRUE;
      -- Render rows
      rDummyArea:=FK_REKU_RENDER_ROWS(iColMatrixIdx, 1, 1, NULL, 0, rArea.nX);
      -- Add column-break-offset
      rArea.nY:=rArea.nY+i_rCrosstab.nColBreakOffset;
    END LOOP;
    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_BARCHART(i_nX        IN NUMBER,
                               i_nY        IN NUMBER,
                               i_nWidth    IN NUMBER,
                               i_nHeight   IN NUMBER,
                               i_rBarChart IN PK_JRXML2PDF_TYPES.tBarChart,
                               i_rStyle    IN PK_JRXML2PDF_TYPES.tSimpleStyle
                              ) IS
    rChart          PK_JRXML2PDF_CHARTS.tChart;
    rReport         PK_JRXML2PDF_TYPES.tReport;
    lDatasets       PK_JRXML2PDF_CHARTS.tDatasetList;
    vcSeries        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rValueDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rRangeDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rDataEntry      PK_JRXML2PDF_CHARTS.tDataEntry;
    rSeries         PK_JRXML2PDF_CHARTS.tSeries;
    rLocaleData     PK_JRXML2PDF_TYPES.tLocaleData:=lReportStack(lReportStack.COUNT).rLocaledata;
  BEGIN
    -- fake a report to execute the query
    rReport.vcQuery:=i_rBarChart.vcQuery;
    -- Push to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    -- Transform a JRXMLchart into a PL-JRXML2PDF-chart
    IF i_rBarChart.vcIsStacked=PK_JRXML2PDF_TYPES.NO THEN
      rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_CATEGORY_BAR_CHART(i_nWidth=> i_nWidth
                                                                         -NVL(i_rBarChart.nLeftPadding,0)
                                                                         -NVL(i_rBarChart.nRightPadding,0),
                                                               i_nHeight=> i_nHeight
                                                                          -NVL(i_rBarChart.nTopPadding,0)
                                                                          -NVL(i_rBarChart.nBottomPadding,0)
                                                              );
    ELSE
      rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_STACKED_BAR_CHART(i_nWidth=> i_nWidth
                                                                        -NVL(i_rBarChart.nLeftPadding,0)
                                                                        -NVL(i_rBarChart.nRightPadding,0),
                                                              i_nHeight=> i_nHeight
                                                                         -NVL(i_rBarChart.nTopPadding,0)
                                                                         -NVL(i_rBarChart.nBottomPadding,0)
                                                             );
    END IF;
    -- transfer properties
    rChart.vcIs3D:=i_rBarchart.vcIs3D;

    rChart.vcTitlePosition:=i_rBarChart.vcTitlePosition;

    rChart.vcTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                           i_vcExpression =>i_rBarChart.vcTitleExpression,
                                           i_vcPattern    =>NULL);

    rChart.vcTitleFont     :=NVL(i_rBarChart.vcTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcTitleFontStyle:=NVL(i_rBarChart.vcTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nTitleFontSize  :=NVL(i_rBarChart.nTitleFontSize  , 16);
    rChart.vcTitleColor    :=REPLACE(NVL(i_rBarChart.vcTitleColor    , PK_JRXML2PDF_TYPES.BLACK), '#','');

    rChart.vcSubTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_rBarChart.vcSubTitleExpression,
                                              i_vcPattern    =>NULL);
    rChart.vcSubTitleFont     :=NVL(i_rBarChart.vcSubTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcSubTitleFontStyle:=NVL(i_rBarChart.vcSubTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nSubTitleFontSize  :=NVL(i_rBarChart.nSubTitleFontSize,   10);
    rChart.vcSubTitleColor    :=REPLACE(NVL(i_rBarChart.vcSubTitleColor,     PK_JRXML2PDF_TYPES.BLACK),'#','');

    IF NVL(i_rBarchart.vcShowLegend, PK_JRXML2PDF_TYPES.BOOL_YES)=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.rLegend.vcPosition   :=i_rBarchart.vcLegendPosition;
      rChart.rLegend.vcFont       :=NVL(i_rBarchart.vcLegendFont,      PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.rLegend.vcFontStyle  :=NVL(i_rBarchart.vcLegendFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.rLegend.nFontSize    :=NVL(i_rBarchart.nLegendFontSize  , 10);
      rChart.rLegend.vcFontColor  :=REPLACE(NVL(i_rBarchart.vcLegendTextColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.rLegend.vcBgColor    :=i_rBarchart.vcLegendBgColor;
    ELSE
      rChart.rLegend.vcPosition   :=PK_JRXML2PDF_CHARTS.POSITION_NONE;
    END IF;

    IF i_rBarchart.vcShowLabels=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_OUTSIDE;
      rChart.lPlots(1).vcLabelColor            :=REPLACE(NVL(i_rBarChart.vcLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.lPlots(1).vcLabelFont             :=NVL(i_rBarChart.vcLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.lPlots(1).vcLabelFontStyle        :=NVL(i_rBarChart.vcLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.lPlots(1).vcLabelPattern          :=FK_NUM_PATTERN(NVL(i_rBarchart.vcValueAxisTickLabelPattern, '##########0'));
      rChart.lPlots(1).nLabelFontSize          :=NVL(i_rBarChart.nLabelFontSize, 10);
    ELSE
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_NONE;
    END IF;

    -- Value-Axis-Settings
    rChart.lValueAxis(1).vcShowTickLabels         :=NVL(i_rBarchart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcShowTickMarks          :=NVL(i_rBarchart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rBarchart.vcValueAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lValueAxis(1).vcLabelColor             :=REPLACE(NVL(i_rBarchart.vcValueAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcLabelFont              :=NVL(i_rBarchart.vcValueAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcLabelFontStyle         :=NVL(i_rBarchart.vcValueAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nLabelFontSize           :=NVL(i_rBarchart.nValueAxisLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rBarchart.vcValueAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcTickLabelFont          :=NVL(i_rBarchart.vcValueAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcTickLabelFontStyle     :=NVL(i_rBarchart.vcValueAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nTickLabelFontSize       :=NVL(i_rBarchart.nAxisTickLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rBarchart.vcValueAxisTickLabelPattern);
    rChart.lValueAxis(1).vcLineColor              :=REPLACE(NVL(i_rBarchart.vcValueAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rBarchart.vcValueAxisMinValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rBarchart.vcValueAxisMaxValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;

    -- Range-Axis-Settings
    rChart.lRangeAxis(1).vcShowTickLabels         :=NVL(i_rBarchart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcShowTickMarks          :=PK_JRXML2PDF_TYPES.BOOL_NO;
    rChart.lRangeAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rBarchart.vcCatAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lRangeAxis(1).vcLabelColor             :=REPLACE(NVL(i_rBarchart.vcCatAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcLabelFont              :=NVL(i_rBarchart.vcCatAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcLabelFontStyle         :=NVL(i_rBarchart.vcCatAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nLabelFontSize           :=NVL(i_rBarchart.nCatAxisLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rBarchart.vcCatAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcTickLabelFont          :=NVL(i_rBarchart.vcCatAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcTickLabelFontStyle     :=NVL(i_rBarchart.vcCatAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nTickLabelFontSize       :=NVL(i_rBarchart.nCatAxisTickLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rBarchart.vcCatAxisTickLabelPattern);
    rChart.lRangeAxis(1).vcLineColor              :=REPLACE(NVL(i_rBarchart.vcCatAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).nTickLabelRotation       :=i_rBarChart.nLabelRotation;
    -- create datasets
    FOR i IN 1..i_rBarChart.lSeries.COUNT LOOP
      lDatasets(i):=PK_JRXML2PDF_CHARTS.FK_CREATE_CATEGORY_DATASET;
      lDatasets(i).vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
      lDatasets(i).nSeries:=i;
    END LOOP;

    -- Add data from query
    PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rBarchart.lParams);
    WHILE FK_FETCH_ROW(lReportStack.COUNT) LOOP
      FOR i IN 1..i_rBarChart.lSeries.COUNT LOOP
        rValueDataEntry.nValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                       i_vcExpression =>i_rBarChart.lSeries(i).vcValueExpression,
                                                       i_vcPattern    =>NULL
                                                      );
        rRangeDataEntry.vcValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                        i_vcExpression =>i_rBarChart.lSeries(i).vcCategoryExpression,
                                                        i_vcPattern    =>NULL
                                                      );
        lDatasets(i).lValueDataEntries(lDatasets(i).lValueDataEntries.COUNT+1):=rValueDataEntry;
        lDatasets(i).lRangeDataEntries(lDatasets(i).lRangeDataEntries.COUNT+1):=rRangeDataEntry;
      END LOOP;
    END LOOP;

    FOR i IN 1..i_rBarChart.lSeries.COUNT LOOP
      vcSeries:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                       i_vcExpression =>i_rBarChart.lSeries(i).vcSeriesExpression,
                                       i_vcPattern    =>NULL
                                      );
      rSeries:=PK_JRXML2PDF_CHARTS.FK_CREATE_SERIES(vcSeries);
      IF i_rBarchart.lSeriesColors.EXISTS(i) THEN
        rSeries.vcColor:=REPLACE(i_rBarchart.lSeriesColors(i), '#', '');
        rSeries.vcBorderColor:=rSeries.vcColor;
      END IF;
      PK_JRXML2PDF_CHARTS.PR_ADD_SERIES(rChart, rSeries);
      PK_JRXML2PDF_CHARTS.PR_ADD_DATASET(rChart, 1, lDatasets(i), vcSeries);
    END LOOP;
    -- If set, run the customizer-procedure
    IF i_rBarChart.vcCustomizerClass IS NOT NULL THEN
      -- set Chart as public chart-object
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=rChart;
      EXECUTE IMMEDIATE 'BEGIN ' || i_rBarChart.vcCustomizerClass || '; END;';
      rChart:=PK_JRXML2PDF_CHARTS.rCustomizableChart;
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=NULL;
    END IF;

    -- render box for chart
    PR_RENDER_BOX(i_nX              =>i_nX,
                  i_nY              =>i_nY,
                  i_nWidth          =>i_nWidth,
                  i_nHeight         =>i_nHeight,
                  i_vcBgColor       =>i_rBarChart.vcFillColor,
                  i_vcFgColor       =>i_rBarChart.vcLineColor,
                  i_nBoxTop         =>i_rBarChart.nBoxTop,
                  i_nBoxLeft        =>i_rBarChart.nBoxLeft,
                  i_nBoxBottom      =>i_rBarChart.nBoxBottom,
                  i_nBoxRight       =>i_rBarChart.nBoxRight,
                  i_vcBoxTopColor   =>i_rBarChart.vcBoxTopColor,
                  i_vcBoxLeftColor  =>i_rBarChart.vcBoxLeftColor,
                  i_vcBoxBottomColor=>i_rBarChart.vcBoxBottomColor,
                  i_vcBoxRightColor =>i_rBarChart.vcBoxRightColor,
                  i_vcOpaque        =>i_rBarChart.vcOpaque,
                  i_rStyle          =>i_rStyle
                 );

    -- set locale
    rChart.rLocaleData:=rLocaleData;
    PK_JRXML2PDF_CHARTS.PR_CHART_TO_PDF(i_rChart=>rChart,
                                        i_nX    =>rPageSetup.nLeftMargin+i_nX+NVL(i_rBarChart.nLeftPadding,0),
                                        i_nY    => rPageSetup.nPageHeight
                                                  -rPageSetup.nTopMargin
                                                  -i_nY
                                                  -NVL(i_rBarChart.nTopPadding,0)
                                       );
    -- pop pseudo-subreport from stack
    PR_POP_SUBREPORT_DATA;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_PIECHART(i_nX        IN NUMBER,
                               i_nY        IN NUMBER,
                               i_nWidth    IN NUMBER,
                               i_nHeight   IN NUMBER,
                               i_rPieChart IN PK_JRXML2PDF_TYPES.tPieChart,
                               i_rStyle    IN PK_JRXML2PDF_TYPES.tSimpleStyle
                              ) IS
    rChart          PK_JRXML2PDF_CHARTS.tChart;
    rReport         PK_JRXML2PDF_TYPES.tReport;
    rDataset        PK_JRXML2PDF_CHARTS.tDataset;
    vcSeries        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rValueDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rRangeDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rDataEntry      PK_JRXML2PDF_CHARTS.tDataEntry;
    rSeries         PK_JRXML2PDF_CHARTS.tSeries;
    rLocaleData     PK_JRXML2PDF_TYPES.tLocaleData:=lReportStack(lReportStack.COUNT).rLocaledata;
  BEGIN
    -- fake a report to execute the query
    rReport.vcQuery:=i_rPieChart.vcQuery;
    -- Push to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    -- Transform a JRXMLchart into a PL-JRXML2PDF-chart
    rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_PIE_CHART(i_nWidth=> i_nWidth
                                                              -NVL(i_rPieChart.nLeftPadding,0)
                                                              -NVL(i_rPieChart.nRightPadding,0),
                                                    i_nHeight=> i_nHeight
                                                               -NVL(i_rPieChart.nTopPadding,0)
                                                               -NVL(i_rPieChart.nBottomPadding,0)
                                                   );
    -- transfer properties
    rChart.vcIs3D:=i_rPiechart.vcIs3D;

    rChart.vcTitlePosition:=i_rPieChart.vcTitlePosition;
    rChart.vcTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                           i_vcExpression =>i_rPieChart.vcTitleExpression,
                                           i_vcPattern    =>NULL);

    rChart.vcTitleFont     :=NVL(i_rPieChart.vcTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcTitleFontStyle:=NVL(i_rPieChart.vcTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nTitleFontSize  :=NVL(i_rPieChart.nTitleFontSize  , 16);
    rChart.vcTitleColor    :=REPLACE(NVL(i_rPieChart.vcTitleColor    , PK_JRXML2PDF_TYPES.BLACK), '#','');

    rChart.vcSubTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_rPieChart.vcSubTitleExpression,
                                              i_vcPattern    =>NULL);
    rChart.vcSubTitleFont     :=NVL(i_rPieChart.vcSubTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcSubTitleFontStyle:=NVL(i_rPieChart.vcSubTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nSubTitleFontSize  :=NVL(i_rPieChart.nSubTitleFontSize,   10);
    rChart.vcSubTitleColor    :=REPLACE(NVL(i_rPieChart.vcSubTitleColor,     PK_JRXML2PDF_TYPES.BLACK),'#','');

    IF NVL(i_rPieChart.vcShowLegend, PK_JRXML2PDF_TYPES.BOOL_YES)=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.rLegend.vcPosition   :=i_rPieChart.vcLegendPosition;
      rChart.rLegend.vcFont       :=NVL(i_rPieChart.vcLegendFont,      PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.rLegend.vcFontStyle  :=NVL(i_rPieChart.vcLegendFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.rLegend.nFontSize    :=NVL(i_rPieChart.nLegendFontSize  , 10);
      rChart.rLegend.vcFontColor  :=REPLACE(NVL(i_rPieChart.vcLegendTextColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.rLegend.vcBgColor    :=i_rPieChart.vcLegendBgColor;
    ELSE
      rChart.rLegend.vcPosition   :=PK_JRXML2PDF_CHARTS.POSITION_NONE;
    END IF;

    rChart.lPlots(1).vcKeyFormat:=NVL(i_rPieChart.vcLegendLabelFormat, '{0}');

    IF i_rPieChart.vcShowLabels=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_OUTSIDE;
      rChart.lPlots(1).vcLabelColor            :=REPLACE(NVL(i_rPieChart.vcLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.lPlots(1).vcLabelFont             :=NVL(i_rPieChart.vcLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.lPlots(1).vcLabelFontStyle        :=NVL(i_rPieChart.vcLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.lPlots(1).nLabelFontSize          :=NVL(i_rPieChart.nLabelFontSize, 10);
      rChart.lPlots(1).vcLabelFormat           :=NVL(i_rPieChart.vcLabelFormat, '{0}');
    ELSE
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_NONE;
    END IF;

    rDataset:=PK_JRXML2PDF_CHARTS.FK_CREATE_CATEGORY_DATASET;
    rDataset.vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;

    -- Add data from query
    PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rPieChart.lParams);
    WHILE FK_FETCH_ROW(lReportStack.COUNT) LOOP
      rValueDataEntry.nValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rPieChart.vcValueExpression,
                                                     i_vcPattern    =>NULL
                                                    );
      rRangeDataEntry.vcValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                      i_vcExpression =>i_rPieChart.vcKeyExpression,
                                                      i_vcPattern    =>NULL
                                                    );
      rDataset.lValueDataEntries(rDataset.lValueDataEntries.COUNT+1):=rValueDataEntry;
      rDataset.lRangeDataEntries(rDataset.lRangeDataEntries.COUNT+1):=rRangeDataEntry;
    END LOOP;

    PK_JRXML2PDF_CHARTS.PR_ADD_DATASET(rChart, 1, rDataset);

    FOR i IN 1..i_rPieChart.lSeriesColors.COUNT LOOP
      rSeries:=PK_JRXML2PDF_CHARTS.FK_CREATE_SERIES(TO_CHAR(i));
      rSeries.vcColor:=REPLACE(i_rPieChart.lSeriesColors(i), '#', '');
      rSeries.vcBorderColor:=rSeries.vcColor;
      PK_JRXML2PDF_CHARTS.PR_ADD_SERIES(rChart, rSeries);
    END LOOP;

    -- If set, run the customizer-procedure
    IF i_rPieChart.vcCustomizerClass IS NOT NULL THEN
      -- set Chart as public chart-object
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=rChart;
      EXECUTE IMMEDIATE 'BEGIN ' || i_rPieChart.vcCustomizerClass || '; END;';
      rChart:=PK_JRXML2PDF_CHARTS.rCustomizableChart;
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=NULL;
    END IF;
    -- render box for chart
    PR_RENDER_BOX(i_nX              =>i_nX,
                  i_nY              =>i_nY,
                  i_nWidth          =>i_nWidth,
                  i_nHeight         =>i_nHeight,
                  i_vcBgColor       =>i_rPieChart.vcFillColor,
                  i_vcFgColor       =>i_rPieChart.vcLineColor,
                  i_nBoxTop         =>i_rPieChart.nBoxTop,
                  i_nBoxLeft        =>i_rPieChart.nBoxLeft,
                  i_nBoxBottom      =>i_rPieChart.nBoxBottom,
                  i_nBoxRight       =>i_rPieChart.nBoxRight,
                  i_vcBoxTopColor   =>i_rPieChart.vcBoxTopColor,
                  i_vcBoxLeftColor  =>i_rPieChart.vcBoxLeftColor,
                  i_vcBoxBottomColor=>i_rPieChart.vcBoxBottomColor,
                  i_vcBoxRightColor =>i_rPieChart.vcBoxRightColor,
                  i_vcOpaque        =>i_rPieChart.vcOpaque,
                  i_rStyle          =>i_rStyle
                 );

    rChart.rLocaleData:=rLocaleData;
    PK_JRXML2PDF_CHARTS.PR_CHART_TO_PDF(i_rChart=>rChart,
                                        i_nX    =>rPageSetup.nLeftMargin+i_nX+NVL(i_rPieChart.nLeftPadding,0),
                                        i_nY    => rPageSetup.nPageHeight
                                                  -rPageSetup.nTopMargin
                                                  -i_nY
                                                  -NVL(i_rPieChart.nTopPadding,0)
                                       );
    -- pop pseudo-subreport from stack
    PR_POP_SUBREPORT_DATA;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_CAT_LINECHART(i_nX                 IN NUMBER,
                                    i_nY                 IN NUMBER,
                                    i_nWidth             IN NUMBER,
                                    i_nHeight            IN NUMBER,
                                    i_rCategoryLineChart IN PK_JRXML2PDF_TYPES.tLineChart,
                                    i_rStyle             IN PK_JRXML2PDF_TYPES.tSimpleStyle
                                   ) IS
    rChart          PK_JRXML2PDF_CHARTS.tChart;
    rReport         PK_JRXML2PDF_TYPES.tReport;
    lDatasets       PK_JRXML2PDF_CHARTS.tDatasetList;
    vcSeries        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rValueDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rRangeDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rDataEntry      PK_JRXML2PDF_CHARTS.tDataEntry;
    rSeries         PK_JRXML2PDF_CHARTS.tSeries;
    rLocaleData     PK_JRXML2PDF_TYPES.tLocaleData:=lReportStack(lReportStack.COUNT).rLocaledata;
  BEGIN
    -- fake a report to execute the query
    rReport.vcQuery:=i_rCategoryLineChart.vcQuery;
    -- Push to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    -- Transform a JRXMLchart into a PL-JRXML2PDF-chart
    rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_CATEGORY_LINE_CHART(i_nWidth=> i_nWidth
                                                                        -NVL(i_rCategoryLineChart.nLeftPadding,0)
                                                                        -NVL(i_rCategoryLineChart.nRightPadding,0),
                                                              i_nHeight=> i_nHeight
                                                                         -NVL(i_rCategoryLineChart.nTopPadding,0)
                                                                         -NVL(i_rCategoryLineChart.nBottomPadding,0)
                                                             );
    -- transfer properties
    rChart.vcIs3D:=i_rCategoryLineChart.vcIs3D;

    rChart.vcTitlePosition:=i_rCategoryLineChart.vcTitlePosition;

    rChart.vcTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                           i_vcExpression =>i_rCategoryLineChart.vcTitleExpression,
                                           i_vcPattern    =>NULL);

    rChart.vcTitleFont     :=NVL(i_rCategoryLineChart.vcTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcTitleFontStyle:=NVL(i_rCategoryLineChart.vcTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nTitleFontSize  :=NVL(i_rCategoryLineChart.nTitleFontSize  , 16);
    rChart.vcTitleColor    :=REPLACE(NVL(i_rCategoryLineChart.vcTitleColor    , PK_JRXML2PDF_TYPES.BLACK), '#','');

    rChart.vcSubTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_rCategoryLineChart.vcSubTitleExpression,
                                              i_vcPattern    =>NULL);
    rChart.vcSubTitleFont     :=NVL(i_rCategoryLineChart.vcSubTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcSubTitleFontStyle:=NVL(i_rCategoryLineChart.vcSubTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nSubTitleFontSize  :=NVL(i_rCategoryLineChart.nSubTitleFontSize,   10);
    rChart.vcSubTitleColor    :=REPLACE(NVL(i_rCategoryLineChart.vcSubTitleColor,     PK_JRXML2PDF_TYPES.BLACK),'#','');

    IF NVL(i_rCategoryLineChart.vcShowLegend, PK_JRXML2PDF_TYPES.BOOL_YES)=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.rLegend.vcPosition   :=i_rCategoryLineChart.vcLegendPosition;
      rChart.rLegend.vcFont       :=NVL(i_rCategoryLineChart.vcLegendFont,      PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.rLegend.vcFontStyle  :=NVL(i_rCategoryLineChart.vcLegendFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.rLegend.nFontSize    :=NVL(i_rCategoryLineChart.nLegendFontSize  , 10);
      rChart.rLegend.vcFontColor  :=REPLACE(NVL(i_rCategoryLineChart.vcLegendTextColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.rLegend.vcBgColor    :=i_rCategoryLineChart.vcLegendBgColor;
    ELSE
      rChart.rLegend.vcPosition   :=PK_JRXML2PDF_CHARTS.POSITION_NONE;
    END IF;

    IF i_rCategoryLineChart.vcShowLabels=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_OUTSIDE;
      rChart.lPlots(1).vcLabelColor            :=REPLACE(NVL(i_rCategoryLineChart.vcLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.lPlots(1).vcLabelFont             :=NVL(i_rCategoryLineChart.vcLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.lPlots(1).vcLabelFontStyle        :=NVL(i_rCategoryLineChart.vcLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.lPlots(1).vcLabelPattern          :=FK_NUM_PATTERN(NVL(i_rCategoryLineChart.vcValueAxisTickLabelPattern, '##########0'));
      rChart.lPlots(1).nLabelFontSize          :=NVL(i_rCategoryLineChart.nLabelFontSize, 10);
    ELSE
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_NONE;
    END IF;
    rChart.lPlots(1).vcShowLines                  :=i_rCategoryLineChart.vcShowLines;
    rChart.lPlots(1).vcShowShapes                 :=i_rCategoryLineChart.vcShowShapes;

    -- Value-Axis-Settings
    rChart.lValueAxis(1).vcShowTickLabels         :=NVL(i_rCategoryLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcShowTickMarks          :=NVL(i_rCategoryLineChart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rCategoryLineChart.vcValueAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lValueAxis(1).vcLabelColor             :=REPLACE(NVL(i_rCategoryLineChart.vcValueAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcLabelFont              :=NVL(i_rCategoryLineChart.vcValueAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcLabelFontStyle         :=NVL(i_rCategoryLineChart.vcValueAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nLabelFontSize           :=NVL(i_rCategoryLineChart.nValueAxisLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rCategoryLineChart.vcValueAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcTickLabelFont          :=NVL(i_rCategoryLineChart.vcValueAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcTickLabelFontStyle     :=NVL(i_rCategoryLineChart.vcValueAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nTickLabelFontSize       :=NVL(i_rCategoryLineChart.nAxisTickLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rCategoryLineChart.vcValueAxisTickLabelPattern);
    rChart.lValueAxis(1).vcLineColor              :=REPLACE(NVL(i_rCategoryLineChart.vcValueAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rCategoryLineChart.vcValueAxisMinValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rCategoryLineChart.vcValueAxisMaxValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;

    -- Range-Axis-Settings
    rChart.lRangeAxis(1).vcShowTickLabels         :=NVL(i_rCategoryLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcShowTickMarks          :=PK_JRXML2PDF_TYPES.BOOL_NO;
    rChart.lRangeAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rCategoryLineChart.vcCatAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lRangeAxis(1).vcLabelColor             :=REPLACE(NVL(i_rCategoryLineChart.vcCatAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcLabelFont              :=NVL(i_rCategoryLineChart.vcCatAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcLabelFontStyle         :=NVL(i_rCategoryLineChart.vcCatAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nLabelFontSize           :=NVL(i_rCategoryLineChart.nCatAxisLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rCategoryLineChart.vcCatAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcTickLabelFont          :=NVL(i_rCategoryLineChart.vcCatAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcTickLabelFontStyle     :=NVL(i_rCategoryLineChart.vcCatAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nTickLabelFontSize       :=NVL(i_rCategoryLineChart.nCatAxisTickLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rCategoryLineChart.vcCatAxisTickLabelPattern);
    rChart.lRangeAxis(1).vcLineColor              :=REPLACE(NVL(i_rCategoryLineChart.vcCatAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).nTickLabelRotation       :=i_rCategoryLineChart.nLabelRotation;
    -- create datasets
    FOR i IN 1..i_rCategoryLineChart.lSeries.COUNT LOOP
      lDatasets(i):=PK_JRXML2PDF_CHARTS.FK_CREATE_CATEGORY_DATASET;
      lDatasets(i).vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
      lDatasets(i).nSeries:=i;
    END LOOP;

    -- Add data from query
    PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rCategoryLineChart.lParams);
    WHILE FK_FETCH_ROW(lReportStack.COUNT) LOOP
      FOR i IN 1..i_rCategoryLineChart.lSeries.COUNT LOOP
        rValueDataEntry.nValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                       i_vcExpression =>i_rCategoryLineChart.lSeries(i).vcValueExpression,
                                                       i_vcPattern    =>NULL
                                                      );
        rRangeDataEntry.vcValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                        i_vcExpression =>i_rCategoryLineChart.lSeries(i).vcCategoryExpression,
                                                        i_vcPattern    =>NULL
                                                      );
        lDatasets(i).lValueDataEntries(lDatasets(i).lValueDataEntries.COUNT+1):=rValueDataEntry;
        lDatasets(i).lRangeDataEntries(lDatasets(i).lRangeDataEntries.COUNT+1):=rRangeDataEntry;
      END LOOP;
    END LOOP;

    FOR i IN 1..i_rCategoryLineChart.lSeries.COUNT LOOP
      vcSeries:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                       i_vcExpression =>i_rCategoryLineChart.lSeries(i).vcSeriesExpression,
                                       i_vcPattern    =>NULL
                                      );
      rSeries:=PK_JRXML2PDF_CHARTS.FK_CREATE_SERIES(vcSeries);
      IF i_rCategoryLineChart.lSeriesColors.EXISTS(i) THEN
        rSeries.vcColor:=REPLACE(i_rCategoryLineChart.lSeriesColors(i), '#', '');
        rSeries.vcBorderColor:=rSeries.vcColor;
      END IF;
      PK_JRXML2PDF_CHARTS.PR_ADD_SERIES(rChart, rSeries);
      PK_JRXML2PDF_CHARTS.PR_ADD_DATASET(rChart, 1, lDatasets(i), vcSeries);
    END LOOP;

    -- If set, run the customizer-procedure
    IF i_rCategoryLineChart.vcCustomizerClass IS NOT NULL THEN
      -- set Chart as public chart-object
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=rChart;
      EXECUTE IMMEDIATE 'BEGIN ' || i_rCategoryLineChart.vcCustomizerClass || '; END;';
      rChart:=PK_JRXML2PDF_CHARTS.rCustomizableChart;
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=NULL;
    END IF;

    -- render box for chart
    PR_RENDER_BOX(i_nX              =>i_nX,
                  i_nY              =>i_nY,
                  i_nWidth          =>i_nWidth,
                  i_nHeight         =>i_nHeight,
                  i_vcBgColor       =>i_rCategoryLineChart.vcFillColor,
                  i_vcFgColor       =>i_rCategoryLineChart.vcLineColor,
                  i_nBoxTop         =>i_rCategoryLineChart.nBoxTop,
                  i_nBoxLeft        =>i_rCategoryLineChart.nBoxLeft,
                  i_nBoxBottom      =>i_rCategoryLineChart.nBoxBottom,
                  i_nBoxRight       =>i_rCategoryLineChart.nBoxRight,
                  i_vcBoxTopColor   =>i_rCategoryLineChart.vcBoxTopColor,
                  i_vcBoxLeftColor  =>i_rCategoryLineChart.vcBoxLeftColor,
                  i_vcBoxBottomColor=>i_rCategoryLineChart.vcBoxBottomColor,
                  i_vcBoxRightColor =>i_rCategoryLineChart.vcBoxRightColor,
                  i_vcOpaque        =>i_rCategoryLineChart.vcOpaque,
                  i_rStyle          =>i_rStyle
                 );

    rChart.rLocaledata:=rLocaleData;
    PK_JRXML2PDF_CHARTS.PR_CHART_TO_PDF(i_rChart=>rChart,
                                        i_nX    =>rPageSetup.nLeftMargin+i_nX+NVL(i_rCategoryLineChart.nLeftPadding,0),
                                        i_nY    => rPageSetup.nPageHeight
                                                  -rPageSetup.nTopMargin
                                                  -i_nY
                                                  -NVL(i_rCategoryLineChart.nTopPadding,0)
                                       );
    -- pop pseudo-subreport from stack
    PR_POP_SUBREPORT_DATA;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_XY_LINECHART(i_nX           IN NUMBER,
                                   i_nY           IN NUMBER,
                                   i_nWidth       IN NUMBER,
                                   i_nHeight      IN NUMBER,
                                   i_rXYLineChart IN PK_JRXML2PDF_TYPES.tLineChart,
                                   i_rStyle       IN PK_JRXML2PDF_TYPES.tSimpleStyle
                                   ) IS
    rChart          PK_JRXML2PDF_CHARTS.tChart;
    rReport         PK_JRXML2PDF_TYPES.tReport;
    lDatasets       PK_JRXML2PDF_CHARTS.tDatasetList;
    vcSeries        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rValueDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rRangeDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rDataEntry      PK_JRXML2PDF_CHARTS.tDataEntry;
    rSeries         PK_JRXML2PDF_CHARTS.tSeries;
    rLocaleData     PK_JRXML2PDF_TYPES.tLocaleData:=lReportStack(lReportStack.COUNT).rLocaledata;
  BEGIN
    -- fake a report to execute the query
    rReport.vcQuery:=i_rXYLineChart.vcQuery;
    -- Push to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    -- Transform a JRXMLchart into a PL-JRXML2PDF-chart
    rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_XY_LINE_CHART(i_nWidth=> i_nWidth
                                                                  -NVL(i_rXyLineChart.nLeftPadding,0)
                                                                  -NVL(i_rXyLineChart.nRightPadding,0),
                                                        i_nHeight=> i_nHeight
                                                                   -NVL(i_rXyLineChart.nTopPadding,0)
                                                                   -NVL(i_rXyLineChart.nBottomPadding,0)
                                                       );
    -- transfer properties
    rChart.vcIs3D:=i_rXyLineChart.vcIs3D;

    rChart.vcTitlePosition:=i_rXyLineChart.vcTitlePosition;

    rChart.vcTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                           i_vcExpression =>i_rXyLineChart.vcTitleExpression,
                                           i_vcPattern    =>NULL);

    rChart.vcTitleFont     :=NVL(i_rXyLineChart.vcTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcTitleFontStyle:=NVL(i_rXyLineChart.vcTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nTitleFontSize  :=NVL(i_rXyLineChart.nTitleFontSize  , 16);
    rChart.vcTitleColor    :=REPLACE(NVL(i_rXyLineChart.vcTitleColor    , PK_JRXML2PDF_TYPES.BLACK), '#','');

    rChart.vcSubTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_rXyLineChart.vcSubTitleExpression,
                                              i_vcPattern    =>NULL);
    rChart.vcSubTitleFont     :=NVL(i_rXyLineChart.vcSubTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcSubTitleFontStyle:=NVL(i_rXyLineChart.vcSubTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nSubTitleFontSize  :=NVL(i_rXyLineChart.nSubTitleFontSize,   10);
    rChart.vcSubTitleColor    :=REPLACE(NVL(i_rXyLineChart.vcSubTitleColor,     PK_JRXML2PDF_TYPES.BLACK),'#','');


    IF NVL(i_rXyLineChart.vcShowLegend, PK_JRXML2PDF_TYPES.BOOL_YES)=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.rLegend.vcPosition   :=i_rXyLineChart.vcLegendPosition;
      rChart.rLegend.vcFont       :=NVL(i_rXyLineChart.vcLegendFont,      PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.rLegend.vcFontStyle  :=NVL(i_rXyLineChart.vcLegendFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.rLegend.nFontSize    :=NVL(i_rXyLineChart.nLegendFontSize  , 10);
      rChart.rLegend.vcFontColor  :=REPLACE(NVL(i_rXyLineChart.vcLegendTextColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.rLegend.vcBgColor    :=i_rXyLineChart.vcLegendBgColor;
    ELSE
      rChart.rLegend.vcPosition   :=PK_JRXML2PDF_CHARTS.POSITION_NONE;
    END IF;

    IF i_rXyLineChart.vcShowLabels=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_OUTSIDE;
      rChart.lPlots(1).vcLabelColor            :=REPLACE(NVL(i_rXyLineChart.vcLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.lPlots(1).vcLabelFont             :=NVL(i_rXyLineChart.vcLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.lPlots(1).vcLabelFontStyle        :=NVL(i_rXyLineChart.vcLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.lPlots(1).vcLabelPattern          :=FK_NUM_PATTERN(NVL(i_rXyLineChart.vcValueAxisTickLabelPattern, '##########0'));
      rChart.lPlots(1).nLabelFontSize          :=NVL(i_rXyLineChart.nLabelFontSize, 10);
    ELSE
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_NONE;
    END IF;
    rChart.lPlots(1).vcShowLines                  :=i_rXyLineChart.vcShowLines;
    rChart.lPlots(1).vcShowShapes                 :=i_rXyLineChart.vcShowShapes;

    -- Value-Axis-Settings
    rChart.lValueAxis(1).vcShowTickLabels         :=NVL(i_rXyLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcShowTickMarks          :=NVL(i_rXyLineChart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rXyLineChart.vcValueAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lValueAxis(1).vcLabelColor             :=REPLACE(NVL(i_rXyLineChart.vcValueAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcLabelFont              :=NVL(i_rXyLineChart.vcValueAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcLabelFontStyle         :=NVL(i_rXyLineChart.vcValueAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nLabelFontSize           :=NVL(i_rXyLineChart.nValueAxisLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rXyLineChart.vcValueAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcTickLabelFont          :=NVL(i_rXyLineChart.vcValueAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcTickLabelFontStyle     :=NVL(i_rXyLineChart.vcValueAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nTickLabelFontSize       :=NVL(i_rXyLineChart.nAxisTickLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rXyLineChart.vcValueAxisTickLabelPattern);
    rChart.lValueAxis(1).vcLineColor              :=REPLACE(NVL(i_rXyLineChart.vcValueAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rXyLineChart.vcValueAxisMinValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rXyLineChart.vcValueAxisMaxValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;

    -- Range-Axis-Settings
    rChart.lRangeAxis(1).vcShowTickLabels         :=NVL(i_rXyLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcShowTickMarks          :=NVL(i_rXyLineChart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rXyLineChart.vcCatAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lRangeAxis(1).vcLabelColor             :=REPLACE(NVL(i_rXyLineChart.vcCatAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcLabelFont              :=NVL(i_rXyLineChart.vcCatAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcLabelFontStyle         :=NVL(i_rXyLineChart.vcCatAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nLabelFontSize           :=NVL(i_rXyLineChart.nCatAxisLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rXyLineChart.vcCatAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcTickLabelFont          :=NVL(i_rXyLineChart.vcCatAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcTickLabelFontStyle     :=NVL(i_rXyLineChart.vcCatAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nTickLabelFontSize       :=NVL(i_rXyLineChart.nCatAxisTickLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rXyLineChart.vcCatAxisTickLabelPattern);
    rChart.lRangeAxis(1).vcLineColor              :=REPLACE(NVL(i_rXyLineChart.vcCatAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).nTickLabelRotation       :=i_rXyLineChart.nLabelRotation;

    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rXyLineChart.vcCatAxisMinValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lRangeAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lRangeAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rXyLineChart.vcCatAxisMaxValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lRangeAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lRangeAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;


    -- create datasets
    FOR i IN 1..i_rXyLineChart.lSeries.COUNT LOOP
      lDatasets(i):=PK_JRXML2PDF_CHARTS.FK_CREATE_XY_DATASET;
      lDatasets(i).nSeries:=i;
    END LOOP;

    -- Add data from query
    PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rXyLineChart.lParams);
    WHILE FK_FETCH_ROW(lReportStack.COUNT) LOOP
      FOR i IN 1..i_rXyLineChart.lSeries.COUNT LOOP
        rValueDataEntry.nValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                       i_vcExpression =>i_rXyLineChart.lSeries(i).vcValueExpression,
                                                       i_vcPattern    =>NULL
                                                      );
        rRangeDataEntry.nValue:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                       i_vcExpression =>i_rXyLineChart.lSeries(i).vcCategoryExpression,
                                                       i_vcPattern    =>NULL
                                                      );
        lDatasets(i).lValueDataEntries(lDatasets(i).lValueDataEntries.COUNT+1):=rValueDataEntry;
        lDatasets(i).lRangeDataEntries(lDatasets(i).lRangeDataEntries.COUNT+1):=rRangeDataEntry;
      END LOOP;
    END LOOP;

    FOR i IN 1..i_rXyLineChart.lSeries.COUNT LOOP
      vcSeries:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                       i_vcExpression =>i_rXyLineChart.lSeries(i).vcSeriesExpression,
                                       i_vcPattern    =>NULL
                                      );
      rSeries:=PK_JRXML2PDF_CHARTS.FK_CREATE_SERIES(vcSeries);
      IF i_rXyLineChart.lSeriesColors.EXISTS(i) THEN
        rSeries.vcColor:=REPLACE(i_rXyLineChart.lSeriesColors(i), '#', '');
        rSeries.vcBorderColor:=rSeries.vcColor;
      END IF;
      PK_JRXML2PDF_CHARTS.PR_ADD_SERIES(rChart, rSeries);
      PK_JRXML2PDF_CHARTS.PR_ADD_DATASET(rChart, 1, lDatasets(i), vcSeries);
    END LOOP;

    -- If set, run the customizer-procedure
    IF i_rXyLineChart.vcCustomizerClass IS NOT NULL THEN
      -- set Chart as public chart-object
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=rChart;
      EXECUTE IMMEDIATE 'BEGIN ' || i_rXyLineChart.vcCustomizerClass || '; END;';
      rChart:=PK_JRXML2PDF_CHARTS.rCustomizableChart;
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=NULL;
    END IF;
    -- render box for chart
    PR_RENDER_BOX(i_nX              =>i_nX,
                  i_nY              =>i_nY,
                  i_nWidth          =>i_nWidth,
                  i_nHeight         =>i_nHeight,
                  i_vcBgColor       =>i_rXyLineChart.vcFillColor,
                  i_vcFgColor       =>i_rXyLineChart.vcLineColor,
                  i_nBoxTop         =>i_rXyLineChart.nBoxTop,
                  i_nBoxLeft        =>i_rXyLineChart.nBoxLeft,
                  i_nBoxBottom      =>i_rXyLineChart.nBoxBottom,
                  i_nBoxRight       =>i_rXyLineChart.nBoxRight,
                  i_vcBoxTopColor   =>i_rXyLineChart.vcBoxTopColor,
                  i_vcBoxLeftColor  =>i_rXyLineChart.vcBoxLeftColor,
                  i_vcBoxBottomColor=>i_rXyLineChart.vcBoxBottomColor,
                  i_vcBoxRightColor =>i_rXyLineChart.vcBoxRightColor,
                  i_vcOpaque        =>i_rXyLineChart.vcOpaque,
                  i_rStyle          =>i_rStyle
                 );

    rChart.rLocaleData:=rLocaleData;
    PK_JRXML2PDF_CHARTS.PR_CHART_TO_PDF(i_rChart=>rChart,
                                        i_nX    =>rPageSetup.nLeftMargin+i_nX+NVL(i_rXyLineChart.nLeftPadding,0),
                                        i_nY    => rPageSetup.nPageHeight
                                                  -rPageSetup.nTopMargin
                                                  -i_nY
                                                  -NVL(i_rXyLineChart.nTopPadding,0)
                                       );
    -- pop pseudo-subreport from stack
    PR_POP_SUBREPORT_DATA;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_TIME_LINECHART(i_nX             IN NUMBER,
                                     i_nY             IN NUMBER,
                                     i_nWidth         IN NUMBER,
                                     i_nHeight        IN NUMBER,
                                     i_rTimeLineChart IN PK_JRXML2PDF_TYPES.tLineChart,
                                     i_rStyle         IN PK_JRXML2PDF_TYPES.tSimpleStyle
                                    ) IS
    rChart          PK_JRXML2PDF_CHARTS.tChart;
    rReport         PK_JRXML2PDF_TYPES.tReport;
    lDatasets       PK_JRXML2PDF_CHARTS.tDatasetList;
    vcSeries        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rValueDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rRangeDataEntry PK_JRXML2PDF_CHARTS.tDataEntry;
    rDataEntry      PK_JRXML2PDF_CHARTS.tDataEntry;
    rSeries         PK_JRXML2PDF_CHARTS.tSeries;
    rLocaleData     PK_JRXML2PDF_TYPES.tLocaleData:=lReportStack(lReportStack.COUNT).rLocaledata;
  BEGIN
    -- fake a report to execute the query
    rReport.vcQuery:=i_rTimeLineChart.vcQuery;
    -- Push to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    -- Transform a JRXMLchart into a PL-JRXML2PDF-chart
    rChart:=PK_JRXML2PDF_CHARTS.FK_CREATE_TIME_LINE_CHART(i_nWidth=> i_nWidth
                                                                    -NVL(i_rTimeLineChart.nLeftPadding,0)
                                                                    -NVL(i_rTimeLineChart.nRightPadding,0),
                                                          i_nHeight=> i_nHeight
                                                                     -NVL(i_rTimeLineChart.nTopPadding,0)
                                                                     -NVL(i_rTimeLineChart.nBottomPadding,0)
                                                         );
    -- transfer properties
    rChart.vcIs3D:=i_rTimeLineChart.vcIs3D;

    rChart.vcTitlePosition:=i_rTimeLineChart.vcTitlePosition;

    rChart.vcTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                           i_vcExpression =>i_rTimeLineChart.vcTitleExpression,
                                           i_vcPattern    =>NULL);

    rChart.vcTitleFont     :=NVL(i_rTimeLineChart.vcTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcTitleFontStyle:=NVL(i_rTimeLineChart.vcTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nTitleFontSize  :=NVL(i_rTimeLineChart.nTitleFontSize  , 16);
    rChart.vcTitleColor    :=REPLACE(NVL(i_rTimeLineChart.vcTitleColor    , PK_JRXML2PDF_TYPES.BLACK), '#','');

    rChart.vcSubTitle:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                              i_vcExpression =>i_rTimeLineChart.vcSubTitleExpression,
                                              i_vcPattern    =>NULL);
    rChart.vcSubTitleFont     :=NVL(i_rTimeLineChart.vcSubTitleFont,      PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.vcSubTitleFontStyle:=NVL(i_rTimeLineChart.vcSubTitleFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.nSubTitleFontSize  :=NVL(i_rTimeLineChart.nSubTitleFontSize,   10);
    rChart.vcSubTitleColor    :=REPLACE(NVL(i_rTimeLineChart.vcSubTitleColor,     PK_JRXML2PDF_TYPES.BLACK),'#','');


    IF NVL(i_rTimeLineChart.vcShowLegend, PK_JRXML2PDF_TYPES.BOOL_YES)=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.rLegend.vcPosition   :=i_rTimeLineChart.vcLegendPosition;
      rChart.rLegend.vcFont       :=NVL(i_rTimeLineChart.vcLegendFont,      PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.rLegend.vcFontStyle  :=NVL(i_rTimeLineChart.vcLegendFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.rLegend.nFontSize    :=NVL(i_rTimeLineChart.nLegendFontSize  , 10);
      rChart.rLegend.vcFontColor  :=REPLACE(NVL(i_rTimeLineChart.vcLegendTextColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.rLegend.vcBgColor    :=i_rTimeLineChart.vcLegendBgColor;
    ELSE
      rChart.rLegend.vcPosition   :=PK_JRXML2PDF_CHARTS.POSITION_NONE;
    END IF;

    IF i_rTimeLineChart.vcShowLabels=PK_JRXML2PDF_TYPES.BOOL_YES THEN
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_OUTSIDE;
      rChart.lPlots(1).vcLabelColor            :=REPLACE(NVL(i_rTimeLineChart.vcLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
      rChart.lPlots(1).vcLabelFont             :=NVL(i_rTimeLineChart.vcLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
      rChart.lPlots(1).vcLabelFontStyle        :=NVL(i_rTimeLineChart.vcLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
      rChart.lPlots(1).vcLabelPattern          :=FK_NUM_PATTERN(NVL(i_rTimeLineChart.vcValueAxisTickLabelPattern, '##########0'));
      rChart.lPlots(1).nLabelFontSize          :=NVL(i_rTimeLineChart.nLabelFontSize, 10);
    ELSE
      rChart.lPlots(1).nLabelPosition          :=PK_JRXML2PDF_CHARTS.VALUE_POSITION_NONE;
    END IF;
    rChart.lPlots(1).vcShowLines                  :=i_rTimeLineChart.vcShowLines;
    rChart.lPlots(1).vcShowShapes                 :=i_rTimeLineChart.vcShowShapes;

    -- Value-Axis-Settings
    rChart.lValueAxis(1).vcShowTickLabels         :=NVL(i_rTimeLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcShowTickMarks          :=NVL(i_rTimeLineChart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lValueAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rTimeLineChart.vcValueAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lValueAxis(1).vcLabelColor             :=REPLACE(NVL(i_rTimeLineChart.vcValueAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcLabelFont              :=NVL(i_rTimeLineChart.vcValueAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcLabelFontStyle         :=NVL(i_rTimeLineChart.vcValueAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nLabelFontSize           :=NVL(i_rTimeLineChart.nValueAxisLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rTimeLineChart.vcValueAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lValueAxis(1).vcTickLabelFont          :=NVL(i_rTimeLineChart.vcValueAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lValueAxis(1).vcTickLabelFontStyle     :=NVL(i_rTimeLineChart.vcValueAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lValueAxis(1).nTickLabelFontSize       :=NVL(i_rTimeLineChart.nAxisTickLabelFontSize, 10);
    rChart.lValueAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rTimeLineChart.vcValueAxisTickLabelPattern);
    rChart.lValueAxis(1).vcLineColor              :=REPLACE(NVL(i_rTimeLineChart.vcValueAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rTimeLineChart.vcValueAxisMinValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                     i_vcExpression =>i_rTimeLineChart.vcValueAxisMaxValExpression);
    IF rDataEntry.nValue IS NOT NULL THEN
      rChart.lValueAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lValueAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;

    -- Range-Axis-Settings
    rChart.lRangeAxis(1).vcShowTickLabels         :=NVL(i_rTimeLineChart.vcShowTickLabels, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcShowTickMarks          :=NVL(i_rTimeLineChart.vcShowTickMarks, PK_JRXML2PDF_TYPES.BOOL_YES);
    rChart.lRangeAxis(1).vcLabelText              :=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                                                           i_vcExpression =>i_rTimeLineChart.vcCatAxisLabelExpression,
                                                                           i_vcPattern    =>NULL);
    rChart.lRangeAxis(1).vcLabelColor             :=REPLACE(NVL(i_rTimeLineChart.vcCatAxisLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcLabelFont              :=NVL(i_rTimeLineChart.vcCatAxisLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcLabelFontStyle         :=NVL(i_rTimeLineChart.vcCatAxisLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nLabelFontSize           :=NVL(i_rTimeLineChart.nCatAxisLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelColor         :=REPLACE(NVL(i_rTimeLineChart.vcCatAxisTickLabelColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).vcTickLabelFont          :=NVL(i_rTimeLineChart.vcCatAxisTickLabelFont, PK_JRXML2PDF_TYPES.HELVETICA);
    rChart.lRangeAxis(1).vcTickLabelFontStyle     :=NVL(i_rTimeLineChart.vcCatAxisTickLabelFontStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL);
    rChart.lRangeAxis(1).nTickLabelFontSize       :=NVL(i_rTimeLineChart.nCatAxisTickLabelFontSize, 10);
    rChart.lRangeAxis(1).vcTickLabelPattern       :=FK_NUM_PATTERN(i_rTimeLineChart.vcCatAxisTickLabelPattern);
    rChart.lRangeAxis(1).vcLineColor              :=REPLACE(NVL(i_rTimeLineChart.vcCatAxisLineColor, PK_JRXML2PDF_TYPES.BLACK),'#','');
    rChart.lRangeAxis(1).nTickLabelRotation       :=i_rTimeLineChart.nLabelRotation;

    rDataEntry.dtValue:=FK_EVALUATE_EXPRESSION_DATE(i_nStackPos    =>lReportStack.COUNT,
                                                    i_vcExpression =>i_rTimeLineChart.vcCatAxisMinValExpression);
    IF rDataEntry.dtValue IS NOT NULL THEN
      rChart.lRangeAxis(1).rMinValueDataEntry       :=rDataEntry;
      rChart.lRangeAxis(1).nMinValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;
    rDataEntry.dtValue:=FK_EVALUATE_EXPRESSION_DATE(i_nStackPos    =>lReportStack.COUNT,
                                                    i_vcExpression =>i_rTimeLineChart.vcCatAxisMaxValExpression);
    IF rDataEntry.dtValue IS NOT NULL THEN
      rChart.lRangeAxis(1).rMaxValueDataEntry       :=rDataEntry;
      rChart.lRangeAxis(1).nMaxValueType:=PK_JRXML2PDF_CHARTS.VALUE_FIXED;
    END IF;


    -- create datasets
    FOR i IN 1..i_rTimeLineChart.lSeries.COUNT LOOP
      lDatasets(i):=PK_JRXML2PDF_CHARTS.FK_CREATE_TIMESERIES_DATASET;
      lDatasets(i).nSeries:=i;
    END LOOP;

    -- Add data from query
    PK_JRXML2PDF_LOADER.PR_EXECUTE_QUERY(lReportStack(lReportStack.COUNT), i_rTimeLineChart.lParams);
    WHILE FK_FETCH_ROW(lReportStack.COUNT) LOOP
      FOR i IN 1..i_rTimeLineChart.lSeries.COUNT LOOP
        rValueDataEntry.nValue:=FK_EVALUATE_EXPRESSION_NUMBER(i_nStackPos    =>lReportStack.COUNT,
                                                              i_vcExpression =>i_rTimeLineChart.lSeries(i).vcValueExpression
                                                             );
        rRangeDataEntry.dtValue:=FK_EVALUATE_EXPRESSION_DATE(i_nStackPos    =>lReportStack.COUNT,
                                                             i_vcExpression =>i_rTimeLineChart.lSeries(i).vcCategoryExpression
                                                            );
        lDatasets(i).lValueDataEntries(lDatasets(i).lValueDataEntries.COUNT+1):=rValueDataEntry;
        lDatasets(i).lRangeDataEntries(lDatasets(i).lRangeDataEntries.COUNT+1):=rRangeDataEntry;
      END LOOP;
    END LOOP;

    FOR i IN 1..i_rTimeLineChart.lSeries.COUNT LOOP
      vcSeries:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>lReportStack.COUNT,
                                       i_vcExpression =>i_rTimeLineChart.lSeries(i).vcSeriesExpression,
                                       i_vcPattern    =>NULL
                                      );
      rSeries:=PK_JRXML2PDF_CHARTS.FK_CREATE_SERIES(vcSeries);
      IF i_rTimeLineChart.lSeriesColors.EXISTS(i) THEN
        rSeries.vcColor:=REPLACE(i_rTimeLineChart.lSeriesColors(i), '#', '');
        rSeries.vcBorderColor:=rSeries.vcColor;
      END IF;
      PK_JRXML2PDF_CHARTS.PR_ADD_SERIES(rChart, rSeries);
      PK_JRXML2PDF_CHARTS.PR_ADD_DATASET(rChart, 1, lDatasets(i), vcSeries);
    END LOOP;

    -- If set, run the customizer-procedure
    IF i_rTimeLineChart.vcCustomizerClass IS NOT NULL THEN
      -- set Chart as public chart-object
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=rChart;
      EXECUTE IMMEDIATE 'BEGIN ' || i_rTimeLineChart.vcCustomizerClass || '; END;';
      rChart:=PK_JRXML2PDF_CHARTS.rCustomizableChart;
      PK_JRXML2PDF_CHARTS.rCustomizableChart:=NULL;
    END IF;
    -- render box for chart
    PR_RENDER_BOX(i_nX              =>i_nX,
                  i_nY              =>i_nY,
                  i_nWidth          =>i_nWidth,
                  i_nHeight         =>i_nHeight,
                  i_vcBgColor       =>i_rTimeLineChart.vcFillColor,
                  i_vcFgColor       =>i_rTimeLineChart.vcLineColor,
                  i_nBoxTop         =>i_rTimeLineChart.nBoxTop,
                  i_nBoxLeft        =>i_rTimeLineChart.nBoxLeft,
                  i_nBoxBottom      =>i_rTimeLineChart.nBoxBottom,
                  i_nBoxRight       =>i_rTimeLineChart.nBoxRight,
                  i_vcBoxTopColor   =>i_rTimeLineChart.vcBoxTopColor,
                  i_vcBoxLeftColor  =>i_rTimeLineChart.vcBoxLeftColor,
                  i_vcBoxBottomColor=>i_rTimeLineChart.vcBoxBottomColor,
                  i_vcBoxRightColor =>i_rTimeLineChart.vcBoxRightColor,
                  i_vcOpaque        =>i_rTimeLineChart.vcOpaque,
                  i_rStyle          =>i_rStyle
                 );

    rChart.rLocaleData:=rLocaleData;
    PK_JRXML2PDF_CHARTS.PR_CHART_TO_PDF(i_rChart=>rChart,
                                        i_nX    =>rPageSetup.nLeftMargin+i_nX+NVL(i_rTimeLineChart.nLeftPadding,0),
                                        i_nY    => rPageSetup.nPageHeight
                                                  -rPageSetup.nTopMargin
                                                  -i_nY
                                                  -NVL(i_rTimeLineChart.nTopPadding,0)
                                       );
    -- pop pseudo-subreport from stack
    PR_POP_SUBREPORT_DATA;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RENDER_BAND(i_nStackPos           IN NUMBER,
                          i_rBand               IN PK_JRXML2PDF_TYPES.tBand,
                          i_rArea               IN PK_JRXML2PDF_TYPES.tArea,
                          i_bAllowSubReports    IN BOOLEAN,
                          i_nRenderFrame        IN NUMBER,
                          i_vcContentAdjustment IN PK_JRXML2PDF_TYPES.tAlignment DEFAULT PK_JRXML2PDF_TYPES.NONE_ALIGN,
                          i_nXFactor            IN NUMBER DEFAULT 1,
                          i_nYFactor            IN NUMBER DEFAULT 1,
                          i_nNeededHeight       IN NUMBER DEFAULT NULL)
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    CURSOR crImage(i_nJrdId  IN NUMBER,
                   i_vcImage IN VARCHAR2) IS
      SELECT JRI_IMAGE,
             JRI_ID,
             JRI_ADLER32,
             JRI_ADLER32_VALID
        FROM JRXML_REPORT_IMAGES
       WHERE JRI_NAME=i_vcImage
         AND (   JRI_JRD_ID=i_nJrdId
              OR JRI_JRD_ID IS NULL
             )
       ORDER BY CASE WHEN JRI_JRD_ID IS NOT NULL THEN
                  1
                ELSE
                  2
                END;

    rArea                    PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    rOriginalArea            PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    rSubArea                 PK_JRXML2PDF_TYPES.tArea;
    nBandHeight              NUMBER:=0;
    nBandOffset              NUMBER:=0;
    vcText                   PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcPriorText              PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rDummyArea               PK_JRXML2PDF_TYPES.tArea;
    blImage                  BLOB;
    vcAdler32                PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcAdler32Valid           PK_JRXML2PDF_TYPES.tYesNo;
    nJriId                   NUMBER;
    bPrintText               BOOLEAN;
    bSubreports              BOOLEAN:=FALSE;
    vcImage                  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nPos                     NUMBER;
    vcReportName             PK_JRXML2PDF_TYPES.tName;
    lYList                   PK_JRXML2PDF_TYPES.tNumList;
    nLastY                   NUMBER;
    bRender                  BOOLEAN:=TRUE;
    -- Minimum bounds for content adjustments bounds
    nMinX                    NUMBER:=999999;
    nMaxX                    NUMBER:=-1;
    nMinY                    NUMBER:=999999;
    nMaxY                    NUMBER:=-1;
    nXFactor                 NUMBER:=i_nXFactor;
    nYFactor                 NUMBER:=i_nYFactor;
    nMaxHeightForCurrentPage NUMBER;
    bPageFrame               BOOLEAN:=FALSE;
    rStyle                   PK_JRXML2PDF_TYPES.tSimpleStyle;

    PROCEDURE PR_PROCESS_CROSSTAB(i_nMinY          IN NUMBER,
                                  i_nMaxY          IN NUMBER,
                                  i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender   BOOLEAN:=FALSE;
      rCrossTab PK_JRXML2PDF_TYPES.tCrosstab;
    BEGIN
      IF      lReportStack(i_nStackPos).lCrosstabs(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lCrosstabs(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lCrosstabs(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render crosstab');
        END IF;
        rDummyArea:=rArea;
        rDummyArea.nX:=rArea.nX+lReportStack(i_nStackPos).lCrosstabs(nPos).nX;
        rDummyArea.nY:=rArea.nY+lReportStack(i_nStackPos).lCrosstabs(nPos).nY;

        rCrossTab:=lReportStack(i_nStackPos).lCrosstabs(nPos);
        -- resolve parameters
        rCrossTab.lParams:=FK_POPULATE_PARAMLIST(i_nStackPos =>i_nStackPos,
                                                 i_lParamList=>rCrossTab.lParams);

        rArea:=FK_RENDER_CROSSTAB(i_nStackPos        =>i_nStackPos,
                                  i_rCrosstab        =>rCrossTab,
                                  i_rArea            =>rDummyArea
                                 );
      END IF;
    END;


    PROCEDURE PR_PROCESS_SUBFRAME(i_nMinY          IN NUMBER,
                                  i_nMaxY          IN NUMBER,
                                  i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender       BOOLEAN:=FALSE;
      rBand         PK_JRXML2PDF_TYPES.tBand;
      nNeededHeight NUMBER;
    BEGIN
      IF     lReportStack(i_nStackPos).lbands(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lbands(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;

      IF bRender AND lReportStack(i_nStackPos).lbands(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lbands(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render subframe');
        END IF;
        rDummyArea:=rArea;
        rDummyArea.nX:=rArea.nX+lReportStack(i_nStackPos).lbands(nPos).nX*nXFactor;
        rDummyArea.nY:=rArea.nY+lReportStack(i_nStackPos).lbands(nPos).nY*nYFactor;

        rBand:=lReportStack(i_nStackPos).lbands(nPos);
        rBand.nWidth:=rBand.nWidth*nXFactor;
        -- subframes can be stretched
        IF rBand.vcStretch=PK_JRXML2PDF_TYPES.YES THEN
          IF (rBand.nHeight+nBandOffset<=nMaxHeightForCurrentPage) THEN
            nNeededHeight:=(rBand.nHeight+nBandOffset)*nYFactor;
          ELSE
            nNeededHeight:=(nMaxHeightForCurrentPage)*nYFactor;
          END IF;
        END IF;
        rBand.nHeight:=rBand.nHeight*nYFactor;

        rDummyArea:=FK_RENDER_BAND(i_nStackPos          =>i_nStackPos,
                                   i_rBand              =>rBand,
                                   i_rArea              =>rDummyArea,
                                   i_bAllowSubReports   =>i_bAllowSubReports,
                                   i_nRenderFrame       =>PK_JRXML2PDF_TYPES.RENDER_FRAME_BEFORE,
                                   i_nXFactor           =>nXFactor,
                                   i_nYFactor           =>nYFactor,
                                   i_nNeededHeight      =>nNeededheight
                                  );
        IF rDummyArea.nPage>rArea.nPage THEN
          rArea.nPage:=rDummyArea.nPage;
          rArea.nY:=rDummyArea.nY;
        END IF;
      END IF;
    END;

    PROCEDURE PR_PROCESS_LINE(i_nMinY          IN NUMBER,
                              i_nMaxY          IN NUMBER,
                              i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender BOOLEAN:=FALSE;
    BEGIN
      IF     lReportStack(i_nStackPos).lLines(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lLines(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lLines(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lLines(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Line');
        END IF;

        PR_RENDER_LINE(i_nX         =>rArea.nX + lReportStack(i_nStackPos).lLines(nPos).nX*nXFactor,
                       i_nY         =>rArea.nY + lReportStack(i_nStackPos).lLines(nPos).nY*nYFactor,
                       i_nWidth     =>lReportStack(i_nStackPos).lLines(nPos).nWidth*nXFactor,
                       i_nHeight    =>CASE WHEN lReportStack(i_nStackPos).lLines(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
                                        (lReportStack(i_nStackPos).lLines(nPos).nHeight+nBandOffset)*nYFactor
                                      ELSE
                                        lReportStack(i_nStackPos).lLines(nPos).nHeight*nYFactor
                                      END,
                       i_nLineWidth =>lReportStack(i_nStackPos).lLines(nPos).nLineWidth,
                       i_vcLineColor=>lReportStack(i_nStackPos).lLines(nPos).vcLineColor,
                       i_rStyle     =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lLines(nPos).vcStyle)
                      );
      END IF;
    END;

    PROCEDURE PR_PROCESS_RECTANGLE(i_nMinY          IN NUMBER,
                                   i_nMaxY          IN NUMBER,
                                   i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender     BOOLEAN:=FALSE;
      rStyle      PK_JRXML2PDF_TYPES.tSimpleStyle;
      vcLineColor PK_JRXML2PDF_TYPES.tColor;
      vcFillColor PK_JRXML2PDF_TYPES.tColor;
      nLineWidth  NUMBER;
      vcOpaque    PK_JRXML2PDF_TYPES.tYesNo;
    BEGIN
      IF     lReportStack(i_nStackPos).lRectangles(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lRectangles(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lRectangles(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lRectangles(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Rectangle');
        END IF;

        -- defaults
        rStyle:=FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lRectangles(nPos).vcStyle);
        vcLineColor:=REPLACE(COALESCE(lReportStack(i_nStackPos).lRectangles(nPos).vcLineColor, rStyle.vcFGColor, PK_JRXML2PDF_TYPES.BLACK), '#', '');
        vcFillColor:=REPLACE(COALESCE(lReportStack(i_nStackPos).lRectangles(nPos).vcFillColor, rStyle.vcBgColor, PK_JRXML2PDF_TYPES.WHITE), '#', '');
        nLineWidth:=COALESCE(lReportStack(i_nStackPos).lRectangles(nPos).nLineWidth, rStyle.nLineWidth, PK_JRXML2PDF_TYPES.THIN);
        vcOpaque  :=COALESCE(lReportStack(i_nStackPos).lRectangles(nPos).vcOpaque,   rStyle.vcOpaque, PK_JRXML2PDF_TYPES.YES);
        PR_RENDER_BOX(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lRectangles(nPos).nX*nXFactor,
                      i_nY              =>rArea.nY + lReportStack(i_nStackPos).lRectangles(nPos).nY*nYFactor,
                      i_nWidth          =>lReportStack(i_nStackPos).lRectangles(nPos).nWidth*nXFactor,
                      i_nHeight         =>CASE WHEN lReportStack(i_nStackPos).lRectangles(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
                                            (lReportStack(i_nStackPos).lRectangles(nPos).nHeight+nBandOffset)*nYFactor
                                          ELSE
                                            (lReportStack(i_nStackPos).lRectangles(nPos).nHeight)*nYFactor
                                          END,
                      i_vcBgColor       =>vcFillColor,
                      i_vcFgColor       =>vcLineColor,
                      i_nBoxTop         =>nLineWidth,
                      i_nBoxLeft        =>nLineWidth,
                      i_nBoxBottom      =>nLineWidth,
                      i_nBoxRight       =>nLineWidth,
                      i_vcBoxTopColor   =>vcLineColor,
                      i_vcBoxLeftColor  =>vcLineColor,
                      i_vcBoxBottomColor=>vcLineColor,
                      i_vcBoxRightColor =>vcLineColor,
                      i_vcOpaque        =>vcOpaque,
                      i_rStyle          =>rStyle
                     );
      END IF;
    END;

    PROCEDURE PR_PROCESS_IMAGE(i_nMinY          IN NUMBER,
                               i_nMaxY          IN NUMBER,
                               i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender BOOLEAN:=FALSE;
    BEGIN
      IF     lReportStack(i_nStackPos).lImages(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lImages(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lImages(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lImages(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                  i_vcExpression=>lReportStack(i_nStackPos).lImages(nPos).vcImageName,
                                  i_vcPattern   =>NULL
                                 )=PK_JRXML2PDF_TYPES.THIS_IS_A_BLOB THEN
          blImage:=FK_EVALUATE_IMAGE(i_nStackPos   =>i_nStackPos,
                                     i_vcExpression=>lReportStack(i_nStackPos).lImages(nPos).vcImageName
                                    );
          vcAdler32:=NULL;
        ELSE
          vcImage:=REPLACE(lReportStack(i_nStackPos).lImages(nPos).vcImageName, '"', '');
          IF INSTR(vcImage, '\', -1)>0 THEN
            vcImage:=SUBSTR(vcImage, INSTR(vcImage, '\', -1)+1);
          END IF;
          OPEN crImage(lReportStack(i_nStackPos).nId, vcImage);
          FETCH crImage INTO blImage,
                             nJriId,
                             vcAdler32,
                             vcAdler32Valid;
          CLOSE crImage;
          IF vcAdler32Valid=PK_JRXML2PDF_TYPES.NO THEN
            -- precalulate Adler32 and store in Report-table
              vcAdler32:=PK_JRXML2PDF_UTIL.FK_UPDATE_ADLER32_FOR_IMAGE(i_nId    =>nJriId,
                                                                       i_blImage=>blImage);
          END IF;
        END IF;
        IF blImage IS NOT NULL THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Image x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lImages(nPos).nX) || '/' ||
                                                                                    TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lImages(nPos).nY) || '/' ||
                                                                                    TO_CHAR(lReportStack(i_nStackPos).lImages(nPos).nWidth)        || '/' ||
                                                                                    TO_CHAR(lReportStack(i_nStackPos).lImages(nPos).nHeight)
                                               );
          END IF;

          PR_RENDER_IMAGE(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lImages(nPos).nX*nXFactor,
                          i_nY              =>rArea.nY + lReportStack(i_nStackPos).lImages(nPos).nY*nYFactor,
                          i_nWidth          =>lReportStack(i_nStackPos).lImages(nPos).nWidth*nXFactor,
                          i_nHeight         =>lReportStack(i_nStackPos).lImages(nPos).nHeight*nYFactor,
                          i_blImage         =>blImage,
                          i_vcAdler32       =>vcAdler32
                         );
          PR_RENDER_BOX(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lImages(nPos).nX*nXFactor,
                        i_nY              =>rArea.nY + lReportStack(i_nStackPos).lImages(nPos).nY*nYFactor,
                        i_nWidth          =>lReportStack(i_nStackPos).lImages(nPos).nWidth*nXFactor,
                        i_nHeight         =>lReportStack(i_nStackPos).lImages(nPos).nHeight*nYFactor,
                        i_vcBgColor       =>NULL,
                        i_vcFgColor       =>lReportStack(i_nStackPos).lImages(nPos).vcFillColor,
                        i_nBoxTop         =>lReportStack(i_nStackPos).lImages(nPos).nBoxTop,
                        i_nBoxLeft        =>lReportStack(i_nStackPos).lImages(nPos).nBoxLeft,
                        i_nBoxBottom      =>lReportStack(i_nStackPos).lImages(nPos).nBoxBottom,
                        i_nBoxRight       =>lReportStack(i_nStackPos).lImages(nPos).nBoxRight,
                        i_vcBoxTopColor   =>lReportStack(i_nStackPos).lImages(nPos).vcBoxTopColor,
                        i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lImages(nPos).vcBoxLeftColor,
                        i_vcBoxBottomColor=>lReportStack(i_nStackPos).lImages(nPos).vcBoxBottomColor,
                        i_vcBoxRightColor =>lReportStack(i_nStackPos).lImages(nPos).vcBoxRightColor,
                        i_vcOpaque        =>PK_JRXML2PDF_TYPES.YES,
                        i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lImages(nPos).vcStyle)
                       );
        END IF;
      END IF;
    END;

    PROCEDURE PR_PROCESS_MAP(i_nMinY          IN NUMBER,
                             i_nMaxY          IN NUMBER,
                             i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      nLatitude  NUMBER;
      nLongitude NUMBER;
      nZoomLevel NUMBER;
      vcUrl      PK_JRXML2PDF_TYPES.tMaxVarchar2;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

      FUNCTION FK_MAP_PARAMETER(i_vcExpression IN VARCHAR2)
      RETURN NUMBER IS
        vcStatement PK_JRXML2PDF_TYPES.tMaxVarchar2;
        nValue      NUMBER;
      BEGIN
        vcStatement:=FK_EVALUATE_EXPRESSION_INT(i_nStackPos    =>i_nStackPos,
                                                i_vcExpression =>i_vcExpression,
                                                i_vcPattern    =>NULL,
                                                i_bForSQLUsage =>TRUE,
                                                i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NONE
                                               );
        EXECUTE IMMEDIATE 'SELECT ' || vcStatement || ' FROM DUAL'
                    INTO nValue;
        RETURN nValue;
      END;
    BEGIN
      IF     lReportStack(i_nStackPos).lMaps(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lMaps(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lMaps(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lMaps(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        BEGIN
          -- evaluate conditions for map-location
          nLatitude:=FK_MAP_PARAMETER(i_vcExpression=>lReportStack(i_nStackPos).lMaps(nPos).vcLatitudeExpression);
          nLongitude:=FK_MAP_PARAMETER(i_vcExpression=>lReportStack(i_nStackPos).lMaps(nPos).vcLongitudeExpression);
          nZoomLevel:=FK_MAP_PARAMETER(i_vcExpression=>lReportStack(i_nStackPos).lMaps(nPos).vcZoomExpression);
          vcUrl:=REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(PK_JRXML2PDF_TYPES.MAP_URL,
                                                         '#LATITUDE#',
                                                         TO_CHAR(nLatitude, PK_JRXML2PDF_TYPES.DEFAULT_FLOAT_FORMAT, 'NLS_NUMERIC_CHARACTERS=.,')
                                                        ),
                                                 '#LONGITUDE#',
                                                 TO_CHAR(nLongitude, PK_JRXML2PDF_TYPES.DEFAULT_FLOAT_FORMAT, 'NLS_NUMERIC_CHARACTERS=.,')
                                                ),
                                         '#ZOOM#',
                                         TO_CHAR(TRUNC(nZoomLevel))
                                        ),
                                 '#WIDTH#',
                                 TO_CHAR(TRUNC(lReportStack(i_nStackPos).lMaps(nPos).nWidth))
                                ),
                         '#HEIGHT#',
                         TO_CHAR(TRUNC(lReportStack(i_nStackPos).lMaps(nPos).nHeight))
                        );
          blImage:=HTTPURITYPE(vcUrl).GETBLOB();
        EXCEPTION
          WHEN OTHERS THEN
            -- render error, output error as text
            rDummy:=FK_RENDER_TEXT(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lMaps(nPos).nX,
                                   i_nY              =>rArea.nY + lReportStack(i_nStackPos).lMaps(nPos).nY,
                                   i_nWidth          =>lReportStack(i_nStackPos).lMaps(nPos).nWidth,
                                   i_nHeight         =>lReportStack(i_nStackPos).lMaps(nPos).nHeight,
                                   i_vcText          =>'Error when reading map-image',
                                   i_vcFont          =>'ARIAL',
                                   i_nFontSize       =>10,
                                   i_vcFontStyle     =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                   i_vcAlignment     =>PK_JRXML2PDF_TYPES.LEFT_ALIGN,
                                   i_vcVerticalAlign =>PK_JRXML2PDF_TYPES.TOP_ALIGN,
                                   i_vcBgColor       =>PK_JRXML2PDF_TYPES.WHITE,
                                   i_vcFgColor       =>PK_JRXML2PDF_TYPES.BLACK,
                                   i_nBoxTop         =>NULL,
                                   i_nBoxLeft        =>NULL,
                                   i_nBoxBottom      =>NULL,
                                   i_nBoxRight       =>NULL,
                                   i_nTopPadding     =>NULL,
                                   i_nLeftPadding    =>NULL,
                                   i_nBottomPadding  =>NULL,
                                   i_nRightPadding   =>NULL,
                                   i_vcBoxTopColor   =>NULL,
                                   i_vcBoxLeftColor  =>NULL,
                                   i_vcBoxBottomColor=>NULL,
                                   i_vcBoxRightColor =>NULL,
                                   i_nLineSpacing    =>PK_JRXML2PDF_TYPES.MIN_LINE_SPACING,
                                   i_vcOpaque        =>PK_JRXML2PDF_TYPES.NO,
                                   i_rStyle          =>NULL
                                  );

            bRender:=FALSE;
        END;
        IF bRender THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lMaps(nPos).nX) || '/' ||
                                                                               TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lMaps(nPos).nY) || '/' ||
                                                                               TO_CHAR(lReportStack(i_nStackPos).lMaps(nPos).nWidth)        || '/' ||
                                                                               TO_CHAR(lReportStack(i_nStackPos).lMaps(nPos).nHeight)
                                               );
          END IF;

          PR_RENDER_IMAGE(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lMaps(nPos).nX*nXFactor,
                          i_nY              =>rArea.nY + lReportStack(i_nStackPos).lMaps(nPos).nY*nYFactor,
                          i_nWidth          =>lReportStack(i_nStackPos).lMaps(nPos).nWidth*nXFactor,
                          i_nHeight         =>lReportStack(i_nStackPos).lMaps(nPos).nHeight*nYFactor,
                          i_blImage         =>blImage,
                          i_vcAdler32       =>NULL
                         );
          DBMS_LOB.FREETEMPORARY(blImage);
          PR_RENDER_BOX(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lMaps(nPos).nX*nXFactor,
                        i_nY              =>rArea.nY + lReportStack(i_nStackPos).lMaps(nPos).nY*nYFactor,
                        i_nWidth          =>lReportStack(i_nStackPos).lMaps(nPos).nWidth*nXFactor,
                        i_nHeight         =>lReportStack(i_nStackPos).lMaps(nPos).nHeight*nYFactor,
                        i_vcBgColor       =>NULL,
                        i_vcFgColor       =>lReportStack(i_nStackPos).lMaps(nPos).vcFillColor,
                        i_nBoxTop         =>lReportStack(i_nStackPos).lMaps(nPos).nBoxTop,
                        i_nBoxLeft        =>lReportStack(i_nStackPos).lMaps(nPos).nBoxLeft,
                        i_nBoxBottom      =>lReportStack(i_nStackPos).lMaps(nPos).nBoxBottom,
                        i_nBoxRight       =>lReportStack(i_nStackPos).lMaps(nPos).nBoxRight,
                        i_vcBoxTopColor   =>lReportStack(i_nStackPos).lMaps(nPos).vcBoxTopColor,
                        i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lMaps(nPos).vcBoxLeftColor,
                        i_vcBoxBottomColor=>lReportStack(i_nStackPos).lMaps(nPos).vcBoxBottomColor,
                        i_vcBoxRightColor =>lReportStack(i_nStackPos).lMaps(nPos).vcBoxRightColor,
                        i_vcOpaque        =>PK_JRXML2PDF_TYPES.YES,
                        i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lMaps(nPos).vcStyle)
                       );
        END IF;
      END IF;
    END;

    PROCEDURE PR_PROCESS_BARCHART(i_nMinY          IN NUMBER,
                                  i_nMaxY          IN NUMBER,
                                  i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

    BEGIN
      IF     lReportStack(i_nStackPos).lBarcharts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lBarcharts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lBarcharts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lBarcharts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lBarcharts(nPos).nX) || '/' ||
                                                                             TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lBarcharts(nPos).nY) || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lBarcharts(nPos).nWidth)        || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lBarcharts(nPos).nHeight)
                                             );
        END IF;

        PR_RENDER_BARCHART(i_nX        =>rArea.nX + lReportStack(i_nStackPos).lBarcharts(nPos).nX*nXFactor,
                           i_nY        =>rArea.nY + lReportStack(i_nStackPos).lBarcharts(nPos).nY*nYFactor,
                           i_nWidth    =>lReportStack(i_nStackPos).lBarcharts(nPos).nWidth*nXFactor,
                           i_nHeight   =>lReportStack(i_nStackPos).lBarcharts(nPos).nHeight*nYFactor,
                           i_rBarChart =>lReportStack(i_nStackPos).lBarcharts(nPos),
                           i_rStyle    =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lBarcharts(nPos).vcStyle)
                          );
      END IF;
    END;

    PROCEDURE PR_PROCESS_PIECHART(i_nMinY          IN NUMBER,
                                  i_nMaxY          IN NUMBER,
                                  i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

    BEGIN
      IF     lReportStack(i_nStackPos).lPiecharts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lPiecharts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lPiecharts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lPiecharts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lPiecharts(nPos).nX) || '/' ||
                                                                             TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lPiecharts(nPos).nY) || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lPiecharts(nPos).nWidth)        || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lPiecharts(nPos).nHeight)
                                             );
        END IF;

        PR_RENDER_PIECHART(i_nX        =>rArea.nX + lReportStack(i_nStackPos).lPiecharts(nPos).nX*nXFactor,
                           i_nY        =>rArea.nY + lReportStack(i_nStackPos).lPiecharts(nPos).nY*nYFactor,
                           i_nWidth    =>lReportStack(i_nStackPos).lPiecharts(nPos).nWidth*nXFactor,
                           i_nHeight   =>lReportStack(i_nStackPos).lPiecharts(nPos).nHeight*nYFactor,
                           i_rPieChart =>lReportStack(i_nStackPos).lPiecharts(nPos),
                           i_rStyle    =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lPiecharts(nPos).vcStyle)
                          );
      END IF;
    END;

    PROCEDURE PR_PROCESS_CAT_LINECHART(i_nMinY          IN NUMBER,
                                       i_nMaxY          IN NUMBER,
                                       i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

    BEGIN
      IF     lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lCategoryLinecharts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lCategoryLinecharts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lCategoryLinecharts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nX) || '/' ||
                                                                             TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nY) || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nWidth)        || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nHeight)
                                             );
        END IF;

        PR_RENDER_CAT_LINECHART(i_nX                =>rArea.nX + lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nX*nXFactor,
                                i_nY                =>rArea.nY + lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nY*nYFactor,
                                i_nWidth            =>lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nWidth*nXFactor,
                                i_nHeight           =>lReportStack(i_nStackPos).lCategoryLinecharts(nPos).nHeight*nYFactor,
                                i_rCategoryLineChart=>lReportStack(i_nStackPos).lCategoryLinecharts(nPos),
                                i_rStyle            =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lCategoryLinecharts(nPos).vcStyle)
                               );
      END IF;
    END;

    PROCEDURE PR_PROCESS_XY_LINECHART(i_nMinY          IN NUMBER,
                                      i_nMaxY          IN NUMBER,
                                      i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

    BEGIN
      IF     lReportStack(i_nStackPos).lXYLineCharts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lXYLineCharts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lXYLineCharts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lXYLineCharts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lXYLineCharts(nPos).nX) || '/' ||
                                                                             TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lXYLineCharts(nPos).nY) || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lXYLineCharts(nPos).nWidth)        || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lXYLineCharts(nPos).nHeight)
                                             );
        END IF;

        PR_RENDER_XY_LINECHART(i_nX           =>rArea.nX + lReportStack(i_nStackPos).lXYLineCharts(nPos).nX*nXFactor,
                               i_nY           =>rArea.nY + lReportStack(i_nStackPos).lXYLineCharts(nPos).nY*nYFactor,
                               i_nWidth       =>lReportStack(i_nStackPos).lXYLineCharts(nPos).nWidth*nXFactor,
                               i_nHeight      =>lReportStack(i_nStackPos).lXYLineCharts(nPos).nHeight*nYFactor,
                               i_rXYLineChart =>lReportStack(i_nStackPos).lXYLineCharts(nPos),
                               i_rStyle       =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lXYLineCharts(nPos).vcStyle)
                              );
      END IF;
    END;

    PROCEDURE PR_PROCESS_TIME_LINECHART(i_nMinY          IN NUMBER,
                                        i_nMaxY          IN NUMBER,
                                        i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender    BOOLEAN:=FALSE;
      rDummy     PK_JRXML2PDF_TYPES.tArea;

    BEGIN
      IF     lReportStack(i_nStackPos).lTimeLineCharts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lTimeLineCharts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lTimeLineCharts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lTimeLineCharts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Map x/Y/Width/Height:' || TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lTimeLineCharts(nPos).nX) || '/' ||
                                                                             TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lTimeLineCharts(nPos).nY) || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lTimeLineCharts(nPos).nWidth)        || '/' ||
                                                                             TO_CHAR(lReportStack(i_nStackPos).lTimeLineCharts(nPos).nHeight)
                                             );
        END IF;

        PR_RENDER_TIME_LINECHART(i_nX            =>rArea.nX + lReportStack(i_nStackPos).lTimeLineCharts(nPos).nX*nXFactor,
                                 i_nY            =>rArea.nY + lReportStack(i_nStackPos).lTimeLineCharts(nPos).nY*nYFactor,
                                 i_nWidth        =>lReportStack(i_nStackPos).lTimeLineCharts(nPos).nWidth*nXFactor,
                                 i_nHeight       =>lReportStack(i_nStackPos).lTimeLineCharts(nPos).nHeight*nYFactor,
                                 i_rTimeLineChart=>lReportStack(i_nStackPos).lTimeLineCharts(nPos),
                                 i_rStyle        =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lTimeLineCharts(nPos).vcStyle)
                                );
      END IF;
    END;

    PROCEDURE PR_PROCESS_TEXT(i_nMinY          IN NUMBER,
                              i_nMaxY          IN NUMBER,
                              i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      bRender BOOLEAN:=FALSE;
      rDummy  PK_JRXML2PDF_TYPES.tArea;
    BEGIN
      IF     lReportStack(i_nStackPos).lTexts(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lTexts(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lTexts(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lTexts(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN

        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render Static Text:' || lReportStack(i_nStackPos).lTexts(nPos).vcText);
        END IF;

        rDummy:=FK_RENDER_TEXT(i_nX              =>rArea.nX + lReportStack(i_nStackPos).lTexts(nPos).nX*nXFactor,
                               i_nY              =>rArea.nY + lReportStack(i_nStackPos).lTexts(nPos).nY*nYFactor,
                               i_nWidth          =>lReportStack(i_nStackPos).lTexts(nPos).nWidth*nXFactor,
                               i_nHeight         =>CASE WHEN lReportStack(i_nStackPos).lTexts(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
                                                     (lReportStack(i_nStackPos).lTexts(nPos).nHeight+nBandOffset)*nYFactor
                                                   ELSE
                                                     (lReportStack(i_nStackPos).lTexts(nPos).nHeight)*nYFactor
                                                   END,
                               i_vcText          =>lReportStack(i_nStackPos).lTexts(nPos).vcText,
                               i_vcFont          =>lReportStack(i_nStackPos).lTexts(nPos).vcFont,
                               i_nFontSize       =>lReportStack(i_nStackPos).lTexts(nPos).nFontSize,
                               i_vcFontStyle     =>lReportStack(i_nStackPos).lTexts(nPos).vcFontStyle,
                               i_vcAlignment     =>lReportStack(i_nStackPos).lTexts(nPos).vcAlignment,
                               i_vcVerticalAlign =>lReportStack(i_nStackPos).lTexts(nPos).vcVerticalAlign,
                               i_vcBgColor       =>lReportStack(i_nStackPos).lTexts(nPos).vcBGColor,
                               i_vcFgColor       =>lReportStack(i_nStackPos).lTexts(nPos).vcFGColor,
                               i_nBoxTop         =>lReportStack(i_nStackPos).lTexts(nPos).nBoxTop,
                               i_nBoxLeft        =>lReportStack(i_nStackPos).lTexts(nPos).nBoxLeft,
                               i_nBoxBottom      =>lReportStack(i_nStackPos).lTexts(nPos).nBoxBottom,
                               i_nBoxRight       =>lReportStack(i_nStackPos).lTexts(nPos).nBoxRight,
                               i_nTopPadding     =>lReportStack(i_nStackPos).lTexts(nPos).nTopPadding,
                               i_nLeftPadding    =>lReportStack(i_nStackPos).lTexts(nPos).nLeftPadding,
                               i_nBottomPadding  =>lReportStack(i_nStackPos).lTexts(nPos).nBottomPadding,
                               i_nRightPadding   =>lReportStack(i_nStackPos).lTexts(nPos).nRightPadding,
                               i_vcBoxTopColor   =>lReportStack(i_nStackPos).lTexts(nPos).vcBoxTopColor,
                               i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lTexts(nPos).vcBoxLeftColor,
                               i_vcBoxBottomColor=>lReportStack(i_nStackPos).lTexts(nPos).vcBoxBottomColor,
                               i_vcBoxRightColor =>lReportStack(i_nStackPos).lTexts(nPos).vcBoxRightColor,
                               i_nLineSpacing    =>PK_JRXML2PDF_TYPES.MIN_LINE_SPACING,
                               i_vcOpaque        =>lReportStack(i_nStackPos).lTexts(nPos).vcOpaque,
                               i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lTexts(nPos).vcStyle),
                               i_vcRotation      =>lReportStack(i_nStackPos).lTexts(nPos).vcRotation
                              );
      END IF;
    END;

    FUNCTION FK_PROCESS_FIELD(i_nMinY          IN NUMBER,
                              i_nMaxY          IN NUMBER,
                              i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType)
    RETURN BOOLEAN IS
      bPageBreaks BOOLEAN:=FALSE;
      bRender     BOOLEAN:=FALSE;
      vcPattern   PK_JRXML2PDF_TYPES.tPattern;
      nPage       NUMBER:=i_rArea.nPage;
    BEGIN
      IF     lReportStack(i_nStackPos).lFields(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=NVL(lReportStack(i_nStackPos).lFields(nPos).vcPositionType, PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP) THEN
        bRender:=TRUE;
      END IF;
      IF bRender AND lReportStack(i_nStackPos).lFields(nPos).vcWhenExpression IS NOT NULL THEN
        bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                       i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcWhenExpression
                                      );
      END IF;
      IF bRender THEN
        IF NVL(lReportStack(i_nStackPos).lFields(nPos).vcEvaluationTime,PK_JRXML2PDF_TYPES.EVALUATION_NOW)=PK_JRXML2PDF_TYPES.EVALUATION_NOW THEN

          IF lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression IS NOT NULL THEN
            vcPattern:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                              i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression,
                                              i_vcPattern   =>NULL);
          ELSE
            vcPattern:=lReportStack(i_nStackPos).lFields(nPos).vcPattern;
          END IF;

          vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                         i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcExpression,
                                         i_vcPattern   =>vcPattern);
          bPrintText:=TRUE;
          IF     lReportStack(i_nStackPos).lFields(nPos).vcPrintRepeated=PK_JRXML2PDF_TYPES.NO
             AND lReportStack(i_nStackPos).rQuery.nPreviousRecord IS NOT NULL THEN

            IF lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression IS NOT NULL THEN
              vcPattern:=FK_EVALUATE_EXPRESSION(i_nStackPos   =>i_nStackPos,
                                                i_vcExpression=>lReportStack(i_nStackPos).lFields(nPos).vcPatternExpression,
                                                i_vcPattern   =>NULL);
            ELSE
              vcPattern:=lReportStack(i_nStackPos).lFields(nPos).vcPattern;
            END IF;

            vcPriorText:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>i_nStackPos,
                                                i_vcExpression =>lReportStack(i_nStackPos).lFields(nPos).vcExpression,
                                                i_vcPattern    =>lReportStack(i_nStackPos).lFields(nPos).vcpattern,
                                                i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_PREVIOUS);
            IF vcText=vcPriorText THEN
              bPrintText:=FALSE;
            END IF;
          END IF;
          IF bPrintText THEN

            IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
              PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render TextField, Stretch, Opaque, x, y, Offset' ||
                                               vcText || ', ' ||
                                               lReportStack(i_nStackPos).lFields(nPos).vcStretch || ',' ||
                                               lReportStack(i_nStackPos).lFields(nPos).vcOpaque  || ',' ||
                                               TO_CHAR(rArea.nX + lReportStack(i_nStackPos).lFields(nPos).nX*nXFactor) || ',' ||
                                               TO_CHAR(rArea.nY + lReportStack(i_nStackPos).lFields(nPos).nY*nYFactor) || ',' ||
                                               TO_CHAR(nBandOffset)
                                              );
            END IF;

            IF lReportStack(i_nStackPos).lFields(nPos).vcMarkup='html' THEN
              rArea:=FK_RENDER_HTML( i_nX              =>rArea.nX + lReportStack(i_nStackPos).lFields(nPos).nX*nXFactor,
                                     i_nY              =>rArea.nY + lReportStack(i_nStackPos).lFields(nPos).nY*nYFactor,
                                     i_nWidth          =>lReportStack(i_nStackPos).lFields(nPos).nWidth*nXFactor,
                                     i_nHeight         =>CASE WHEN lReportStack(i_nStackPos).lFields(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
                                                           (lReportStack(i_nStackPos).lFields(nPos).nHeight+nBandOffset)*nYFactor
                                                         ELSE
                                                           (lReportStack(i_nStackPos).lFields(nPos).nHeight)*nYFactor
                                                         END,
                                     i_vcText          =>vcText,
                                     i_vcFont          =>lReportStack(i_nStackPos).lFields(nPos).vcFont,
                                     i_nFontSize       =>lReportStack(i_nStackPos).lFields(nPos).nFontSize,
                                     i_vcFontStyle     =>lReportStack(i_nStackPos).lFields(nPos).vcFontStyle,
                                     i_vcAlignment     =>lReportStack(i_nStackPos).lFields(nPos).vcAlignment,
                                     i_vcVerticalAlign =>lReportStack(i_nStackPos).lFields(nPos).vcVerticalAlign,
                                     i_vcBgColor       =>lReportStack(i_nStackPos).lFields(nPos).vcBGColor,
                                     i_vcFgColor       =>lReportStack(i_nStackPos).lFields(nPos).vcFGColor,
                                     i_nBoxTop         =>lReportStack(i_nStackPos).lFields(nPos).nBoxTop,
                                     i_nBoxLeft        =>lReportStack(i_nStackPos).lFields(nPos).nBoxLeft,
                                     i_nBoxBottom      =>lReportStack(i_nStackPos).lFields(nPos).nBoxBottom,
                                     i_nBoxRight       =>lReportStack(i_nStackPos).lFields(nPos).nBoxRight,
                                     i_nTopPadding     =>lReportStack(i_nStackPos).lFields(nPos).nTopPadding,
                                     i_nLeftPadding    =>lReportStack(i_nStackPos).lFields(nPos).nLeftPadding,
                                     i_nBottomPadding  =>lReportStack(i_nStackPos).lFields(nPos).nBottomPadding,
                                     i_nRightPadding   =>lReportStack(i_nStackPos).lFields(nPos).nRightPadding,
                                     i_vcBoxTopColor   =>lReportStack(i_nStackPos).lFields(nPos).vcBoxTopColor,
                                     i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lFields(nPos).vcBoxLeftColor,
                                     i_vcBoxBottomColor=>lReportStack(i_nStackPos).lFields(nPos).vcBoxBottomColor,
                                     i_vcBoxRightColor =>lReportStack(i_nStackPos).lFields(nPos).vcBoxRightColor,
                                     i_nLineSpacing    =>lReportStack(i_nStackPos).lFields(nPos).nLineSpacing,
                                     i_vcOpaque        =>lReportStack(i_nStackPos).lFields(nPos).vcOpaque,
                                     i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lFields(nPos).vcStyle),
                                     i_bMayPageBreak   =>    lReportStack(i_nStackPos).lFields(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES
                                                         AND nBandOffset>0,
                                     i_rArea           =>rArea,
                                     i_bEnhanced       =>NVL(lReportStack(i_nStackPos).lFields(nPos).vcKey, '.') LIKE 'enhanced%'
                                    );
            ELSE
              rArea:=FK_RENDER_TEXT( i_nX              =>rArea.nX + lReportStack(i_nStackPos).lFields(nPos).nX*nXFactor,
                                     i_nY              =>rArea.nY + lReportStack(i_nStackPos).lFields(nPos).nY*nYFactor,
                                     i_nWidth          =>lReportStack(i_nStackPos).lFields(nPos).nWidth*nXFactor,
                                     i_nHeight         =>CASE WHEN lReportStack(i_nStackPos).lFields(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
                                                           (lReportStack(i_nStackPos).lFields(nPos).nHeight+nBandOffset)*nYFactor
                                                         ELSE
                                                           (lReportStack(i_nStackPos).lFields(nPos).nHeight)*nYFactor
                                                         END,
                                     i_vcText          =>vcText,
                                     i_vcFont          =>lReportStack(i_nStackPos).lFields(nPos).vcFont,
                                     i_nFontSize       =>lReportStack(i_nStackPos).lFields(nPos).nFontSize,
                                     i_vcFontStyle     =>lReportStack(i_nStackPos).lFields(nPos).vcFontStyle,
                                     i_vcAlignment     =>lReportStack(i_nStackPos).lFields(nPos).vcAlignment,
                                     i_vcVerticalAlign =>lReportStack(i_nStackPos).lFields(nPos).vcVerticalAlign,
                                     i_vcBgColor       =>lReportStack(i_nStackPos).lFields(nPos).vcBGColor,
                                     i_vcFgColor       =>lReportStack(i_nStackPos).lFields(nPos).vcFGColor,
                                     i_nBoxTop         =>lReportStack(i_nStackPos).lFields(nPos).nBoxTop,
                                     i_nBoxLeft        =>lReportStack(i_nStackPos).lFields(nPos).nBoxLeft,
                                     i_nBoxBottom      =>lReportStack(i_nStackPos).lFields(nPos).nBoxBottom,
                                     i_nBoxRight       =>lReportStack(i_nStackPos).lFields(nPos).nBoxRight,
                                     i_nTopPadding     =>lReportStack(i_nStackPos).lFields(nPos).nTopPadding,
                                     i_nLeftPadding    =>lReportStack(i_nStackPos).lFields(nPos).nLeftPadding,
                                     i_nBottomPadding  =>lReportStack(i_nStackPos).lFields(nPos).nBottomPadding,
                                     i_nRightPadding   =>lReportStack(i_nStackPos).lFields(nPos).nRightPadding,
                                     i_vcBoxTopColor   =>lReportStack(i_nStackPos).lFields(nPos).vcBoxTopColor,
                                     i_vcBoxLeftColor  =>lReportStack(i_nStackPos).lFields(nPos).vcBoxLeftColor,
                                     i_vcBoxBottomColor=>lReportStack(i_nStackPos).lFields(nPos).vcBoxBottomColor,
                                     i_vcBoxRightColor =>lReportStack(i_nStackPos).lFields(nPos).vcBoxRightColor,
                                     i_nLineSpacing    =>lReportStack(i_nStackPos).lFields(nPos).nLineSpacing,
                                     i_vcOpaque        =>lReportStack(i_nStackPos).lFields(nPos).vcOpaque,
                                     i_rStyle          =>FK_GET_STYLE(i_nStackPos, lReportStack(i_nStackPos).lFields(nPos).vcStyle),
                                     i_bMayPageBreak   =>    lReportStack(i_nStackPos).lFields(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES
                                                         AND nBandOffset>0,
                                     i_rArea           =>rArea,
                                     i_vcRotation      =>lReportStack(i_nStackPos).lFields(nPos).vcRotation
                                    );
            END IF;
            IF rArea.nPage>nPage THEN
              bPageBreaks:=TRUE;
            END IF;

          END IF;
        ELSE
          -- evaluate later
          PR_ADD_FIELD_FOR_LATER(i_nStackPos, rArea.nX, rArea.nY, lReportStack(i_nStackPos).lFields(nPos));
        END IF;
      END IF;
      -- return flag, if pagebreaks may have occured
      RETURN bPageBreaks;
    END;

    PROCEDURE PR_PROCESS_SUBREPORT(i_nMinY          IN NUMBER,
                                   i_nMaxY          IN NUMBER,
                                   i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
    BEGIN
      IF     lReportStack(i_nStackPos).lSubreports(nPos).nY BETWEEN i_nMinY AND i_nMaxY-1
         AND i_vcPositionType=PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP THEN
        -- Subreports, if allowed
        IF i_bAllowSubReports THEN
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Reportname is ' || vcReportName);
          END IF;
          -- Evaluate
          vcReportName:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>PK_JRXML2PDF_TYPES.NO_RECORD,
                                               i_vcExpression =>lReportStack(i_nStackPos).lSubreports(nPos).vcReportName,
                                               i_vcPattern    =>NULL
                                              );
          vcReportName:=REPLACE(vcReportName, '.jasper','');
          IF INSTR(vcReportName, '\', -1)>0 THEN
            vcReportName:=SUBSTR(vcReportName, INSTR(vcReportName, '\', -1)+1);
          END IF;

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render subreport ' || vcReportName);
          END IF;

          rSubArea:=FK_RENDER_SUBREPORT(i_nX               =>rArea.nX + lReportStack(i_nStackPos).lSubreports(nPos).nX*nXFactor,
                                        i_nY               =>rArea.nY + lReportStack(i_nStackPos).lSubreports(nPos).nY*nYFactor,
                                        i_nWidth           =>lReportStack(i_nStackPos).lSubreports(nPos).nWidth*nXFactor,
                                        i_nHeight          =>lReportStack(i_nStackPos).lSubreports(nPos).nHeight*nYFactor,
                                        i_vcStyle          =>lReportStack(i_nStackPos).lSubreports(nPos).vcStyle,
                                        i_rArea            =>i_rArea,
                                        i_vcReportName     =>vcReportName,
                                        i_lParamList       =>FK_POPULATE_PARAMLIST(i_nStackPos, lReportStack(i_nStackPos).lSubreports(nPos).lParamList),
                                        i_nMasterRecord    =>lReportStack(i_nStackPos).rQuery.nRecordPosition
                                       );
          IF    rSubArea.nY>rArea.nY
             OR rSubArea.nPage>rArea.nPage THEN
            rArea.nY:=rSubArea.nY;
            rArea.nPage:=rSubArea.nPage;
            bSubreports:=TRUE;
          END IF;
        END IF;
      END IF;
    END;

    PROCEDURE PR_CALCULATE_MBR IS
      nMinCheckX NUMBER;
      nMinCheckY NUMBER;
      nMaxCheckX NUMBER;
      nMaxCheckY NUMBER;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Calculating MBR');
      END IF;
      -- now check all objects
      FOR i IN 1..i_rBand.lObjects.COUNT LOOP
        nPos:=i_rBand.lObjects(i).nPosition;
        IF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.SUBFRAME THEN
          nMinCheckX:=lReportStack(i_nStackPos).lbands(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lbands(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lbands(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lbands(nPos).nHeight;
        ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.LINE THEN
          nMinCheckX:=lReportStack(i_nStackPos).lLines(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lLines(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lLines(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lLines(nPos).nHeight;
        ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.RECTANGLE THEN
          nMinCheckX:=lReportStack(i_nStackPos).lRectangles(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lRectangles(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lRectangles(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lRectangles(nPos).nHeight;
        ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.IMAGE THEN
          nMinCheckX:=lReportStack(i_nStackPos).lImages(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lImages(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lImages(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lImages(nPos).nHeight;
        ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.TEXT THEN
          nMinCheckX:=lReportStack(i_nStackPos).lTexts(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lTexts(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lTexts(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lTexts(nPos).nHeight;
        ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.FIELD THEN
          nMinCheckX:=lReportStack(i_nStackPos).lFields(nPos).nX;
          nMinCheckY:=lReportStack(i_nStackPos).lFields(nPos).nY;
          nMaxCheckX:=nMinCheckX+lReportStack(i_nStackPos).lFields(nPos).nWidth;
          nMaxCheckY:=nMinCheckY+lReportStack(i_nStackPos).lFields(nPos).nHeight;
        END IF;
        -- calculate bounds
        nMinX:=LEAST(nMinX, nMinCheckX);
        nMinY:=LEAST(nMinY, nMinCheckY);
        nMaxX:=GREATEST(nMaxX, nMaxCheckX);
        nMaxY:=GREATEST(nMaxY, nMaxCheckY);

      END LOOP;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('MBR Min x/y Max x/y :' || nMinX || '/'|| nMinY || '/'|| nMaxX || '/' || nMinY);
      END IF;
    END;

    PROCEDURE PR_RENDER_BAND(i_vcPositionType IN PK_JRXML2PDF_TYPES.tPositionType) IS
      rMaxArea   PK_JRXML2PDF_TYPES.tArea;
      bAreaReset BOOLEAN:=FALSE;
      bStorePos  BOOLEAN:=FALSE;
    BEGIN
      rMaxArea:=rArea;
      nLastY:=-9999;
      FOR z IN 1..lYList.COUNT LOOP
        IF z>1 THEN
          -- Page break here
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Page break');
          END IF;

          PR_FINISH_PAGE_AND_START_NEW(io_rArea         =>rArea,
                                       i_nStackPos      =>i_nStackPos,
                                       i_bInnerToBottom =>TRUE
                                      );
        END IF;
        FOR i IN 1..i_rBand.lObjects.COUNT LOOP
          nPos:=i_rBand.lObjects(i).nPosition;

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Render object no:' || nPos);
          END IF;
          IF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.SUBFRAME THEN
            -- store the current position of the page
            IF NOT bStorePos THEN
              bStorePos:=TRUE;
              PR_STORE_PAGE_POSITION(rArea);
            END IF;
            PR_PROCESS_SUBFRAME(nLastY, lYList(z), i_vcPositionType);
            IF rArea.nPage>rMaxArea.nPage THEN
              bSubreports:=TRUE;
              rMaxArea:=rArea;
              rArea:=FK_GOTO_LAST_STORED_POSITION;
              bAreaReset:=TRUE;
            END IF;
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.LINE THEN
            PR_PROCESS_LINE(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.RECTANGLE THEN
            PR_PROCESS_RECTANGLE(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.IMAGE THEN
            PR_PROCESS_IMAGE(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.TEXT THEN
            PR_PROCESS_TEXT(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.FIELD THEN
            -- if field may strecth, we have to keep the position
            IF lReportStack(i_nStackPos).lFields(nPos).vcStretch=PK_JRXML2PDF_TYPES.YES THEN
              -- store the current position of the page
              IF NOT bStorePos THEN
                bStorePos:=TRUE;
                PR_STORE_PAGE_POSITION(rArea);
              END IF;
            END IF;
            IF FK_PROCESS_FIELD(nLastY, lYList(z), i_vcPositionType) THEN
              -- treat pagebreak like a sub-report
              bSubreports:=TRUE;
              IF rArea.nPage>rMaxArea.nPage THEN
                rMaxArea:=rArea;
              ELSIF     rArea.nPage=rMaxArea.nPage
                    AND rArea.nY>rMaxArea.nY THEN
                rMaxArea.nY:=rArea.nY;
              END IF;
              rArea:=FK_GOTO_LAST_STORED_POSITION;
              bAreaReset:=TRUE;
            END IF;
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.SUBREPORT THEN
            -- store the current position of the page
            IF NOT bStorePos THEN
              bStorePos:=TRUE;
              PR_STORE_PAGE_POSITION(rArea);
            END IF;
            PR_PROCESS_SUBREPORT(nLastY, lYList(z), i_vcPositionType);
            IF rArea.nPage>rMaxArea.nPage THEN
              rMaxArea:=rArea;
            ELSIF     rArea.nPage=rMaxArea.nPage
                  AND rArea.nY>rMaxArea.nY THEN
              rMaxArea.nY:=rArea.nY;
            END IF;
            rArea:=FK_GOTO_LAST_STORED_POSITION;
            bAreaReset:=TRUE;
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.CROSSTAB THEN
            -- store the current position of the page
            IF NOT bStorePos THEN
              bStorePos:=TRUE;
              PR_STORE_PAGE_POSITION(rArea);
            END IF;
            PR_PROCESS_CROSSTAB(nLastY, lYList(z), i_vcPositionType);
            IF rArea.nPage>rMaxArea.nPage THEN
              rMaxArea:=rArea;
            ELSIF     rArea.nPage=rMaxArea.nPage
                  AND rArea.nY>rMaxArea.nY THEN
              rMaxArea.nY:=rArea.nY;
            END IF;
            rArea:=FK_GOTO_LAST_STORED_POSITION;
            bAreaReset:=TRUE;
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.MAP THEN
            PR_PROCESS_MAP(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.BARCHART THEN
            PR_PROCESS_BARCHART(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.PIECHART THEN
            PR_PROCESS_PIECHART(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.CATEGORYLINECHART THEN
            PR_PROCESS_CAT_LINECHART(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.XYLINECHART THEN
            PR_PROCESS_XY_LINECHART(nLastY, lYList(z), i_vcPositionType);
          ELSIF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.TIMELINECHART THEN
            PR_PROCESS_TIME_LINECHART(nLastY, lYList(z), i_vcPositionType);
          END IF;
        END LOOP;
        IF bStorePos THEN
          PR_REMOVE_LAST_STORED_POSITION;
        END IF;
        nLastY:=lYList(z);
      END LOOP;
      IF bAreaReset THEN
        -- restore position
        rArea:=rMaxArea;
        PR_GOTO_STORED_POSITION(rArea);
      END IF;

    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Render band/ object count/Y-Before/Page-Before:' || TO_CHAR(i_rBand.lObjects.COUNT) || '/' ||
                                                                                        TO_CHAR(rArea.nY)               || '/' ||
                                                                                        TO_CHAR(rArea.nPage)
                                  );
    END IF;
    IF i_rBand.vcWhenExpression IS NOT NULL THEN
      bRender:=FK_EVALUATE_CONDITION(i_nStackPos   =>i_nStackPos,
                                     i_vcExpression=>i_rBand.vcWhenExpression
                                    );
    END IF;
    IF bRender THEN
      nBandHeight:=i_rBand.nHeight;
      IF i_rBand.bHasBreaks THEN
        -- build up multiple ranges of items
        FOR i IN 1..i_rBand.lObjects.COUNT LOOP
          IF i_rBand.lObjects(i).nType=PK_JRXML2PDF_TYPES.BREAK THEN
            lYList(lYList.COUNT+1):=lReportStack(i_nStackPos).lBreaks(i_rBand.lObjects(i).nPosition).nY;
          END IF;
        END LOOP;
        lYList(lYList.COUNT+1):=9999999;
      ELSE
        -- render all items
        lYList(1):=9999999;
      END IF;
      -- Check band-Height for Stretching
      PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                          i_rBand            =>i_rband,
                          i_bForceCalculate  =>TRUE,
                          o_nbandHeight      =>nBandHeight,
                          o_nBandOffset      =>nBandOffset
                         );
      -- amke sure its scaled up to the needed height                         
      IF     i_nNeededHeight IS NOT NULL 
         AND nBandHeight+nBandOffset<i_nNeededHeight THEN
        nBandOffset:=i_nNeededHeight-nBandHeight;
        nBandHeight:=i_nNeededHeight;
      END IF;
      IF nBandOffset>0 THEN
        -- page may break, check height
        nMaxHeightForCurrentPage:=FK_REST_OF_PAGE(rArea.nY);
        IF nMaxHeightForCurrentPage<nBandHeight THEN
          -- no longer
          nBandOffset:=nBandOffset-(nBandHeight-nMaxHeightForCurrentPage);
          IF i_rband.nWidth IS NOT NULL THEN
            -- add frame to page-break-frames
            bPageFrame:=TRUE;
            rStyle:=FK_MAKE_STYLE_FROM_ATTRIBS(i_vcBgColor        =>i_rband.vcBGColor,
                                               i_vcFgColor        =>i_rband.vcFGColor,
                                               i_nBoxTop          =>i_rband.nBoxTop,
                                               i_nBoxLeft         =>i_rband.nBoxLeft,
                                               i_nBoxBottom       =>i_rband.nBoxBottom,
                                               i_nBoxRight        =>i_rband.nBoxRight,
                                               i_vcBoxTopColor    =>i_rband.vcBoxTopColor,
                                               i_vcBoxLeftColor   =>i_rband.vcBoxLeftColor,
                                               i_vcBoxBottomColor =>i_rband.vcBoxBottomColor,
                                               i_vcBoxRightColor  =>i_rband.vcBoxRightColor,
                                               i_vcOpaque         =>i_rBand.vcOpaque,
                                               i_rStyle           =>FK_GET_STYLE(i_nStackPos, i_rband.vcStyle)
                                              );
            -- add to stack
            PR_PUSH_PAGE_FRAME_MARKER(i_nStackPos   =>i_nStackpos,
                                      i_nBorderTop  =>i_rArea.nY,
                                      i_nBorderWidth=>i_rband.nWidth,
                                      i_rStyle      =>rStyle);
          END IF;
        ELSE
          nMaxHeightForCurrentPage:=nBandHeight;
        END IF;
      ELSE
        nMaxHeightForCurrentPage:=nBandHeight;
      END IF;
      IF i_vcContentAdjustment!=PK_JRXML2PDF_TYPES.NONE_ALIGN THEN
        -- adjustment of contentposition requested, calculate MBR of content
        PR_CALCULATE_MBR;
        IF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.RIGHT_ALIGN THEN
          -- set so that all x-values are adjusted and appear right-aligned
          rArea.nX:=rArea.nX+(i_rband.nWidth-nMaxX);
        ELSIF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.STRETCH_ALIGN THEN
          -- set up and X-factor which is afterwards applied to all objects
          IF     nMaxX>0
             AND i_rband.nWidth>nMaxX THEN
            nXFactor:=i_rband.nWidth/nMaxX;
          END IF;
        ELSIF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.STRETCH_Y_ALIGN THEN
          -- set up and Y-factor which is afterwards applied to all objects
          IF     nMaxY>0
             AND nBandHeight>nMaxY THEN
            nYFactor:=nBandHeight/nMaxY;
          END IF;
        ELSIF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.CENTER_ALIGN THEN
          -- set so that all x-values are adjusted and appear centered
          rArea.nX:=rArea.nX+(i_rband.nWidth-(nMaxX-nMinX))/2;
        ELSIF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.BOTTOM_ALIGN THEN
          -- set so that all y-values are adjusted to the bottom
          rArea.nY:=rArea.nY+i_rband.nHeight-(nMaxY+nBandOffset);
        ELSIF i_vcContentAdjustment=PK_JRXML2PDF_TYPES.MIDDLE_ALIGN THEN
          -- set so that all y-values are adjusted and appear centered
          rArea.nY:=rArea.nY+(GREATEST(i_rband.nHeight,nBandHeight)-(nMaxY-nMinY))/2;
        END IF;
      END IF;

      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Start rendering band, height and offset=' || TO_CHAR(nBandHeight)  || '/' || TO_CHAR(nBandOffset));
      END IF;

      IF     i_nRenderFrame IN (PK_JRXML2PDF_TYPES.RENDER_FRAME_BEFORE, PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER)
         AND NOT bPageFrame THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Rendering box for band before band');
        END IF;
        PR_RENDER_BOX(i_nX              =>i_rArea.nX,
                      i_nY              =>i_rArea.nY,
                      i_nWidth          =>i_rband.nWidth,
                      i_nHeight         =>nMaxHeightForCurrentPage,
                      i_vcBgColor       =>i_rband.vcBGColor,
                      i_vcFgColor       =>i_rband.vcFGColor,
                      i_nBoxTop         =>i_rband.nBoxTop,
                      i_nBoxLeft        =>i_rband.nBoxLeft,
                      i_nBoxBottom      =>i_rband.nBoxBottom,
                      i_nBoxRight       =>i_rband.nBoxRight,
                      i_vcBoxTopColor   =>i_rband.vcBoxTopColor,
                      i_vcBoxLeftColor  =>i_rband.vcBoxLeftColor,
                      i_vcBoxBottomColor=>i_rband.vcBoxBottomColor,
                      i_vcBoxRightColor =>i_rband.vcBoxRightColor,
                      i_vcOpaque        =>i_rband.vcOpaque,
                      i_rStyle          =>FK_GET_STYLE(i_nStackPos, i_rband.vcStyle)
                     );
      END IF;

      IF NVL(i_rBand.bHasTopPos, TRUE) THEN
        -- Render everything with position Fix relative to top
        PR_RENDER_BAND(PK_JRXML2PDF_TYPES.RELATIVE_TO_TOP);
      END IF;
      IF i_rBand.bHasBottomPos THEN
        IF FK_FITS_IN_PAGE(rArea.nY, i_rBand.nHeight-NVL(i_rBand.nMaxPosTop,i_rBand.nHeight)) THEN
          IF NOT bSubReports THEN
            IF i_rBand.bHasTopPos THEN
              rArea.nY:=rArea.nY+NVL(nBandHeight,0)-i_rBand.nHeight-NVL(i_rBand.nMaxPosTop,i_rBand.nHeight)+NVL(i_rBand.nMinPosBottom,0);
            ELSE
              rArea.nY:=rArea.nY+NVL(nBandHeight,0)-i_rBand.nHeight+NVL(i_rBand.nMinPosBottom,0);
            END IF;
          ELSE
            -- relative adjustment
            rArea.nY:=rArea.nY-NVL(i_rBand.nMaxPosTop,i_rBand.nHeight);
            -- reset Bandheight to original value
            nBandHeight:=i_rBand.nHeight;
          END IF;
          -- Render fix relative to bottom
          PR_RENDER_BAND(PK_JRXML2PDF_TYPES.RELATIVE_TO_BOTTOM);
        ELSE
          PR_FINISH_PAGE_AND_START_NEW(io_rArea         =>rArea,
                                       i_nStackPos      =>i_nStackPos,
                                       i_bInnerToBottom =>TRUE
                                      );
          -- position up to skip gap between band-start and bottom-part
          rArea.nY:=rArea.nY-i_rBand.nMinPosBottom;
          -- Render fix relative to bottom
          PR_RENDER_BAND(PK_JRXML2PDF_TYPES.RELATIVE_TO_BOTTOM);
        END IF;
      END IF;

      IF     i_nRenderFrame=PK_JRXML2PDF_TYPES.RENDER_FRAME_AFTER
         AND NOT bPageFrame THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Rendering box for band after band');
        END IF;
        -- Rendering again, but only the lines, not the background color

        PR_RENDER_BOX(i_nX              =>i_rArea.nX,
                      i_nY              =>i_rArea.nY,
                      i_nWidth          =>i_rband.nWidth,
                      i_nHeight         =>nMaxHeightForCurrentPage,
                      i_vcBgColor       =>i_rband.vcBGColor,
                      i_vcFgColor       =>i_rband.vcFGColor,
                      i_nBoxTop         =>i_rband.nBoxTop,
                      i_nBoxLeft        =>i_rband.nBoxLeft,
                      i_nBoxBottom      =>i_rband.nBoxBottom,
                      i_nBoxRight       =>i_rband.nBoxRight,
                      i_vcBoxTopColor   =>i_rband.vcBoxTopColor,
                      i_vcBoxLeftColor  =>i_rband.vcBoxLeftColor,
                      i_vcBoxBottomColor=>i_rband.vcBoxBottomColor,
                      i_vcBoxRightColor =>i_rband.vcBoxRightColor,
                      i_vcOpaque        =>PK_JRXML2PDF_TYPES.NO,
                      i_rStyle          =>FK_GET_STYLE(i_nStackPos, i_rband.vcStyle)
                     );
      END IF;

      IF NOT bSubReports AND NOT i_rBand.bHasBreaks THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('No-subreport and no Breaks, added height ' || TO_CHAR(nBandHeight));
        END IF;
        IF i_rBand.bHasBottomPos THEN
          IF NOT bSubReports THEN
            IF i_rBand.bHasTopPos THEN
              rArea.nY:=rArea.nY
                        -NVL(nBandHeight,0)+i_rBand.nHeight+NVL(i_rBand.nMaxPosTop,i_rBand.nHeight)-NVL(i_rBand.nMinPosBottom,0)
                        +i_rBand.nHeight-NVL(i_rBand.nMaxPosTop,i_rBand.nHeight)
                        +GREATEST(NVL(nBandHeight,0),NVL(i_rBand.nHeight,0));
            ELSE
              rArea.nY:=rArea.nY
                        -NVL(nBandHeight,0)+i_rBand.nHeight-NVL(i_rBand.nMinPosBottom,0)
                        +GREATEST(NVL(nBandHeight,0),NVL(i_rBand.nHeight,0));
            END IF;
          ELSE
            -- relative adjustment
            rArea.nY:=rArea.nY+i_rBand.nMinPosBottom;
          END IF;
        ELSE
          rArea.nY:=rArea.nY+NVL(nBandHeight,0);
        END IF;
      ELSIF     bSubReports
            AND rArea.nPage=rOriginalArea.nPage
            AND rArea.nY<rOriginalArea.nY+NVL(nBandHeight,0) THEN
        rArea.nY:=rOriginalArea.nY+NVL(nBandHeight,0);
      ELSIF     bSubReports
            AND i_rBand.bHasBottomPos THEN
        rArea.nY:=rArea.nY+NVL(nBandHeight,0);
      END IF;
      IF bPageFrame THEN
        PR_APPLY_REPORT_STYLE(i_nStackPos=>i_nStackpos,
                              i_nX       =>rArea.nX,
                              i_nBottom  =>rArea.nY
                             );
        -- add to stack
        PR_POP_PAGE_FRAME_MARKER(i_nStackPos   =>i_nStackpos);
      END IF;
    END IF;
    -- set back x which may have been changed for adjustments
    rArea.nX:=i_rArea.nX;

    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Finished Y-After/Page-After:' || TO_CHAR(rArea.nY) || '/' ||
                                                                     TO_CHAR(rArea.nPage)
                                  );
    END IF;
    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RENDER_REGION(i_nStackPos         IN NUMBER,
                            i_rRegion           IN PK_JRXML2PDF_TYPES.tRegion,
                            i_rArea             IN PK_JRXML2PDF_TYPES.tArea,
                            i_bAllowSubReports  IN BOOLEAN
                           )
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    rArea PK_JRXML2PDF_TYPES.tArea:=i_rArea;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Render Region:' || i_rRegion.vcBaseNode);
    END IF;

    FOR i IN 1..i_rRegion.lObjects.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
        PK_JRXML2PDF_LOG.PR_LOG_FINE('Render Band:' || i);
      END IF;
      rArea:=FK_RENDER_BAND(i_nStackPos,
                            lReportStack(i_nStackPos).lBands(i_rRegion.lObjects(i).nPosition),
                            rArea,
                            i_bAllowSubReports,
                            PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE);
    END LOOP;

    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('X/Y after rendering:' || rArea.nX || ' - ' || rArea.nY);
    END IF;

    RETURN rArea;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_REGION_HEIGHT(i_nStackPos IN NUMBER,
                                 i_rRegion   IN PK_JRXML2PDF_TYPES.tRegion)
  RETURN NUMBER IS
    nHeight     NUMBER:=0;
    nBandHeight NUMBER;
    nBandOffset NUMBER;
    rArea       PK_JRXML2PDF_TYPES.tArea;
  BEGIN
    FOR i IN 1..i_rRegion.lObjects.COUNT LOOP
      PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                          i_rBand            =>lReportStack(i_nStackPos).lBands(i_rRegion.lObjects(i).nPosition),
                          o_nbandHeight      =>nBandHeight,
                          o_nBandOffset      =>nBandOffset
                         );
      nHeight:=nHeight+NVL(nBandHeight,0);
    END LOOP;
    IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
      PK_JRXML2PDF_LOG.PR_LOG_FINE('Calculate Region height Node/height:' || i_rRegion.vcBaseNode || '/' || TO_CHAR(nHeight));
    END IF;
    RETURN nHeight;
  END;

  FUNCTION FK_FETCH_ROW(i_nStackPos IN NUMBER)
  RETURN BOOLEAN IS
    clTemp CLOB;
    bReturn BOOLEAN:=TRUE;

    FUNCTION FK_FETCH
    RETURN PK_JRXML2PDF_TYPES.tDataEntries IS
      nRows   NUMBER;
      rResult PK_JRXML2PDF_TYPES.tDataEntries;
      rEntry  PK_JRXML2PDF_TYPES.tDataEntry;
    BEGIN
      nRows := DBMS_SQL.FETCH_ROWS(lReportStack(i_nStackPos).rQuery.iCursor);

      lReportStack(i_nStackPos).rQuery.bEOF:=(nRows=0);
      IF NOT lReportStack(i_nStackPos).rQuery.bEOF THEN
        lReportStack(i_nStackPos).rQuery.nRecordRead:=lReportStack(i_nStackPos).rQuery.nRecordRead+1;
      END IF;
      FOR i IN 1..lReportStack(i_nStackPos).rQuery.lDescTab.COUNT LOOP

        IF lReportStack(i_nStackPos).rQuery.lDescTab(i).col_type IN     (PK_JRXML2PDF_TYPES.DBMS_SQL_VARCHAR2_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_CHAR_TYPE
                                                                        ) THEN
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
          DBMS_SQL.COLUMN_VALUE(lReportStack(i_nStackPos).rQuery.iCursor, i, rEntry.vcdata);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('VARCHAR2-value for row / column : ' ||
                                                lReportStack(i_nStackPos).rQuery.nRecordRead ||'/' ||
                                                UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME) || ':' ||
                                                rEntry.vcdata);
          END IF;
        ELSIF lReportStack(i_nStackPos).rQuery.lDescTab(i).col_type =PK_JRXML2PDF_TYPES.DBMS_SQL_CLOB_TYPE THEN
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
          DBMS_SQL.COLUMN_VALUE(lReportStack(i_nStackPos).rQuery.iCursor, i, clTemp);

          rEntry.vcdata:=DBMS_LOB.SUBSTR(clTemp, 32767, 1);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('CLOB-value for row / column : ' ||
                                                lReportStack(i_nStackPos).rQuery.nRecordRead ||'/' ||
                                                UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME) || ':' ||
                                                rEntry.vcdata);
          END IF;

        ELSIF lReportStack(i_nStackPos).rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_NUMBER_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_FLOAT_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_BINARY_BOUBLE_TYPE
                                                                        ) THEN
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
          DBMS_SQL.COLUMN_VALUE(lReportStack(i_nStackPos).rQuery.iCursor, i, rEntry.nData);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('NUMBER-value for row / column : ' ||
                                                lReportStack(i_nStackPos).rQuery.nRecordRead ||'/' ||
                                                UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME) || ':' ||
                                                rEntry.nData);
          END IF;
        ELSIF lReportStack(i_nStackPos).rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_DATE_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_TIMESTAMP_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_TSTMP_WITH_TZ_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_IV_YEAR_TO_MONTH_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_IV_DAY_TO_SECOND_TYPE,
                                                                         PK_JRXML2PDF_TYPES.DBMS_SQL_TS_WTH_LOCAL_TZ_TYPE
                                                                        ) THEN
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
          DBMS_SQL.COLUMN_VALUE(lReportStack(i_nStackPos).rQuery.iCursor, i, rEntry.dtData);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DATE-value for row / column : ' ||
                                                lReportStack(i_nStackPos).rQuery.nRecordRead ||'/' ||
                                                UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME) || ':' ||
                                                rEntry.dtData);
          END IF;

        ELSIF lReportStack(i_nStackPos).rQuery.lDescTab(i).col_type IN  (PK_JRXML2PDF_TYPES.DBMS_SQL_BLOB_TYPE
                                                                        ) THEN
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_BLOB;
          DBMS_SQL.COLUMN_VALUE(lReportStack(i_nStackPos).rQuery.iCursor, i, rEntry.blData);

          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('BLOB-value for row / column : ' ||
                                                lReportStack(i_nStackPos).rQuery.nRecordRead ||'/' ||
                                                UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME) || ':BLOB');
          END IF;
        ELSE
          rEntry.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
          rEntry.vcdata:='Unsupported datatype';
        END IF;
        rResult(UPPER(lReportStack(i_nStackPos).rQuery.lDescTab(i).COL_NAME)):=rEntry;
      END LOOP;
      RETURN rResult;
    END;
  BEGIN
    IF     NOT lReportStack(i_nStackPos).rQuery.bEOF
       AND lReportStack(i_nStackPos).rQuery.nCurrentRecord IS NULL THEN
      -- first record
      -- fetch current record
      lReportStack(i_nStackPos).rQuery.nCurrentRecord:=1;
      lReportStack(i_nStackPos).rQuery.lQueryResult(1):=FK_FETCH;
      IF NOT lReportStack(i_nStackPos).rQuery.bEOF THEN
        lReportStack(i_nStackPos).rQuery.nRecordPosition:=1;
        lReportStack(i_nStackPos).rQuery.nNextRecord:=2;
        lReportStack(i_nStackPos).rQuery.lQueryResult(2):=FK_FETCH;
        IF lReportStack(i_nStackPos).rQuery.bEOF THEN
          lReportStack(i_nStackPos).rQuery.nNextRecord:=NULL;
        END IF;
      ELSE
        lReportStack(i_nStackPos).rQuery.nCurrentRecord:=NULL;
        bReturn:=FALSE;
      END IF;
    ELSIF     lReportStack(i_nStackPos).rQuery.bEOF
          AND lReportStack(i_nStackPos).rQuery.nNextRecord IS NOT NULL THEN
      -- the next record is the last
      lReportStack(i_nStackPos).rQuery.nPreviousRecord:=lReportStack(i_nStackPos).rQuery.nCurrentRecord;
      lReportStack(i_nStackPos).rQuery.nCurrentRecord:=lReportStack(i_nStackPos).rQuery.nNextRecord;
      lReportStack(i_nStackPos).rQuery.nNextRecord:=NULL;
      lReportStack(i_nStackPos).rQuery.nRecordPosition:=lReportStack(i_nStackPos).rQuery.nRecordPosition+1;
    ELSIF     lReportStack(i_nStackPos).rQuery.bEOF THEN
      -- no more records
      bReturn:=FALSE;
    ELSE
      -- swap record and fetch next record
      lReportStack(i_nStackPos).rQuery.nPreviousRecord:=lReportStack(i_nStackPos).rQuery.nCurrentRecord;
      lReportStack(i_nStackPos).rQuery.nCurrentRecord:=lReportStack(i_nStackPos).rQuery.nNextRecord;
      IF lReportStack(i_nStackPos).rQuery.nNextRecord=1 THEN
        lReportStack(i_nStackPos).rQuery.nNextRecord:=2;
      ELSIF lReportStack(i_nStackPos).rQuery.nNextRecord=2 THEN
        lReportStack(i_nStackPos).rQuery.nNextRecord:=3;
      ELSIF lReportStack(i_nStackPos).rQuery.nNextRecord=3 THEN
        lReportStack(i_nStackPos).rQuery.nNextRecord:=1;
      END IF;
      lReportStack(i_nStackPos).rQuery.lQueryResult(lReportStack(i_nStackPos).rQuery.nNextRecord):=FK_FETCH;
      IF lReportStack(i_nStackPos).rQuery.bEOF THEN
        lReportStack(i_nStackPos).rQuery.nNextRecord:=NULL;
      END IF;
      lReportStack(i_nStackPos).rQuery.nRecordPosition:=lReportStack(i_nStackPos).rQuery.nRecordPosition+1;
    END IF;
    RETURN bReturn;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_PUSH_SUBREPORT_DATA(io_rReport IN OUT NOCOPY PK_JRXML2PDF_TYPES.tReport) IS

    iPos PLS_INTEGER:=lReportStack.COUNT+1;
  BEGIN
    lReportStack(iPos):=io_rReport;

    lReportStack(iPos).nPageHdrHeight:=FK_CALC_REGION_HEIGHT  (i_nStackPos=>iPos,
                                                               i_rRegion  =>io_rReport.rPageHeaderRegion);
    lReportStack(iPos).nColumnHdrHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>iPos,
                                                               i_rRegion  =>io_rReport.rColumnHeaderRegion);
    lReportStack(iPos).nColumnFtrHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>iPos,
                                                               i_rRegion  =>io_rReport.rColumnFooterRegion);
    lReportStack(iPos).nPageFtrHeight:=FK_CALC_REGION_HEIGHT  (i_nStackPos=>iPos,
                                                               i_rRegion  =>io_rReport.rPageFooterRegion);
    lReportStack(iPos).nSummaryHeight:=FK_CALC_REGION_HEIGHT  (i_nStackPos=>iPos,
                                                               i_rRegion  =>io_rReport.rSummaryRegion);

  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_POP_SUBREPORT_DATA IS
    iPos PLS_INTEGER:=lReportStack.COUNT;
  BEGIN
    IF iPos>0 THEN
      -- Close Cursor
      DBMS_SQL.CLOSE_CURSOR(lReportStack(iPos).rQuery.iCursor);
      lReportStack.DELETE(iPos);
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_FINISH_PAGE(io_rArea                 IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                           i_nStackPos              IN NUMBER,
                           i_bInnerToBottom         IN BOOLEAN DEFAULT FALSE,
                           i_bBeforeSubReportHdr    IN BOOLEAN DEFAULT FALSE,
                           i_bBeforeSubReportColHdr IN BOOLEAN DEFAULT FALSE,
                           i_bResetPagenumber       IN BOOLEAN DEFAULT FALSE,
                           i_vcStyleToApply         IN PK_JRXML2PDF_TYPES.tStyleName DEFAULT NULL
                          ) IS
    nXSave     NUMBER;
    nMax       NUMBER;
    rDummyArea PK_JRXML2PDF_TYPES.tArea;

  BEGIN
    IF i_nStackPos=1 THEN
      -- Main report
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Finish page for main report');
      END IF;
      -- finish page, but only if not already done
      IF NOT lReportStack(i_nStackPos).lEndOfPageDone.EXISTS(lReportStack(i_nStackPos).nCurrentPagePointer) THEN
        PR_APPLY_REPORT_STYLE(i_nStackPos     =>i_nStackPos,
                              i_nX            =>io_rArea.nX,
                              i_nBottom       =>io_rArea.nY
                             );

        IF lReportStack(i_nStackPos).vcFloatColumnFooter=PK_JRXML2PDF_TYPES.YES THEN
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render column footer (floating)');
          END IF;
          io_rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnFooterRegion, io_rArea, TRUE);
          -- psoition to bottom for page-footer
          io_rArea.nY:=rPageSetup.nPageHeight-
                       rPageSetup.nBottomMargin-
                       rPageSetup.nTopMargin-
                       lReportStack(i_nStackPos).nPageFtrHeight;
        ELSE
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render column footer (at bottom)');
          END IF;
          -- psoition to bottom for page-footer
          io_rArea.nY:=rPageSetup.nPageHeight-
                       rPageSetup.nBottomMargin-
                       rPageSetup.nTopMargin-
                       lReportStack(i_nStackPos).nColumnFtrHeight-
                       lReportStack(i_nStackPos).nPageFtrHeight;
          io_rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnFooterRegion, io_rArea, TRUE);
        END IF;
        IF NOT i_bBeforeSubReportHdr THEN
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render page footer');
          END IF;

          io_rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rPageFooterRegion, io_rArea, FALSE);
          lReportStack(i_nStackPos).lEndOfPageDone(lReportStack(i_nStackPos).nCurrentPagePointer):=1;
        END IF;
      END IF;
    ELSE
      nXSave:=io_rArea.nX;
      -- finish page, but only if not already done
      IF NOT lReportStack(i_nStackPos).lEndOfPageDone.EXISTS(lReportStack(i_nStackPos).nCurrentPagePointer) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Finish page for subreport ' || TO_CHAR(i_nStackPos));
        END IF;
        -- Render all column-Footer below, reverse order
        IF     NOT i_bInnerToBottom
           AND (   NOT i_bBeforeSubReportHdr
                OR NOT i_bBeforeSubReportColHdr
               ) THEN
          io_rArea.nX:=lReportStack(lReportStack.COUNT).nXPositionHdr;
          IF NOT i_bBeforeSubReportColHdr THEN
            io_rArea:=FK_RENDER_REGION(lReportStack.COUNT, lReportStack(lReportStack.COUNT).rColumnFooterRegion, io_rArea, TRUE);
          END IF;
          IF NOT i_bBeforeSubReportHdr THEN
            io_rArea:=FK_RENDER_REGION(lReportStack.COUNT, lReportStack(lReportStack.COUNT).rPageFooterRegion, io_rArea, TRUE);
          END IF;

          PR_APPLY_REPORT_STYLE(i_nStackPos     =>lReportStack.COUNT,
                                i_nX            =>io_rArea.nX,
                                i_nBottom       =>io_rArea.nY
                               );
        END IF;
        lReportStack(i_nStackPos).lEndOfPageDone(lReportStack(i_nStackPos).nCurrentPagePointer):=1;
      END IF;
      IF     i_bInnerToBottom
         AND (   NOT i_bBeforeSubReportHdr
              OR NOT i_bBeforeSubReportColHdr
             ) THEN
        nMax:=lReportStack.COUNT;
      ELSE
        nMax:=lReportStack.COUNT-1;
      END IF;
      io_rArea.nY:=rPageSetup.nPageHeight-
                   rPageSetup.nBottomMargin-
                   rPageSetup.nTopMargin;

      FOR j IN  REVERSE 1..nMax LOOP
        io_rArea.nY:=io_rArea.nY-lReportStack(j).nPageFtrHeight-
                                 lReportStack(j).nColumnFtrHeight;
      END LOOP;

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render all page footers and column footers, report count:' || lReportStack.COUNT);
      END IF;
      FOR j IN  REVERSE 1..nMax LOOP
        io_rArea.nX:=lReportStack(j).nXPositionHdr;
        -- finish page, but only if not already done
        IF NOT lReportStack(j).lEndOfPageDone.EXISTS(lReportStack(j).nCurrentPagePointer) THEN
          lReportStack(j).lEndOfPageDone(lReportStack(j).nCurrentPagePointer):=1;
          IF j=1 THEN
            PR_APPLY_REPORT_STYLE(i_nStackPos     =>j,
                                  i_nX            =>io_rArea.nX,
                                  i_nBottom       =>io_rArea.nY
                                 );
          END IF;
          io_rArea:=FK_RENDER_REGION(j, lReportStack(j).rColumnFooterRegion, io_rArea, TRUE);
          io_rArea:=FK_RENDER_REGION(j, lReportStack(j).rPageFooterRegion,   io_rArea, TRUE);
          IF j>1 THEN
            PR_APPLY_REPORT_STYLE(i_nStackPos     =>j,
                                  i_nX            =>io_rArea.nX,
                                  i_nBottom       =>io_rArea.nY
                                 );
          END IF;
        END IF;
      END LOOP;
      io_rArea.nX:=nXSave;
    END IF;
  END;

  PROCEDURE PR_START_NEW_PAGE(io_rArea                 IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                              i_nStackPos              IN NUMBER,
                              i_bInnerToBottom         IN BOOLEAN DEFAULT FALSE,
                              i_bBeforeSubReportHdr    IN BOOLEAN DEFAULT FALSE,
                              i_bBeforeSubReportColHdr IN BOOLEAN DEFAULT FALSE,
                              i_bResetPagenumber       IN BOOLEAN DEFAULT FALSE,
                              i_bRenderGroups          IN BOOLEAN DEFAULT TRUE,
                              i_bPageHeaders           IN BOOLEAN DEFAULT TRUE) IS
    nXSave        NUMBER;
    nMax          NUMBER;
    rDummyArea    PK_JRXML2PDF_TYPES.tArea;
    nDetailHeight NUMBER;
  BEGIN
    IF i_nStackPos=1 THEN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Start new page for main report');
      END IF;

      PR_NEW_PAGE(i_bResetPageNumber);

      io_rArea.nX:=0;
      io_rArea.nY:=0;
      io_rArea.nPage:=io_rArea.nPage+1;
      IF NOT lReportStack(i_nStackPos).lStartOfPageDone.EXISTS(lReportStack(i_nStackPos).nCurrentPagePointer) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render background');
        END IF;
        -- Background for the page
        rDummyArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rBackgroundRegion, io_rArea, FALSE);

        IF i_bPageHeaders THEN
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render page header');
          END IF;
          io_rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rPageHeaderRegion, io_rArea,  FALSE);
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render column header');
          END IF;
          io_rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnHeaderRegion, io_rArea, TRUE);
        END IF;

        FOR z IN 1..lReportStack(1).lPageFrameMarkers.COUNT LOOP
          lReportStack(1).lPageFrameMarkers(z).nBorderTop:=io_rArea.nY;
        END LOOP;

        IF i_bRenderGroups THEN
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render group headers');
          END IF;
          -- Render all group-Headers above
          PR_RENDER_GROUP_HEADERS(i_nStackPos     =>i_nStackPos,
                                  io_rArea        =>io_rArea,
                                  i_bReprintOnly  =>TRUE,
                                  i_bIsOnNewPage  =>TRUE
                                 );
        END IF;
        -- Store Y-Position after header
        lReportStack(i_nStackPos).lStartOfPageDone(lReportStack(i_nStackPos).nCurrentPagePointer):=io_rArea.nY;
      ELSE
        io_rArea.nY:=lReportStack(i_nStackPos).lStartOfPageDone(lReportStack(i_nStackPos).nCurrentPagePointer);
      END IF;
    ELSE
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Start new page for subreport ' || TO_CHAR(i_nStackPos));
      END IF;
      nXSave:=io_rArea.nX;
      PR_NEW_PAGE(i_bResetPagenumber);
      io_rArea.nY:=0;
      io_rArea.nX:=0;
      io_rArea.nPage:=io_rArea.nPage+1;

      IF NOT lReportStack(1).lStartOfPageDone.EXISTS(lReportStack(1).nCurrentPagePointer) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render background');
        END IF;
        -- Background for the page, taken from main-report
        rDummyArea:=FK_RENDER_REGION(1, lReportStack(1).rBackgroundRegion, io_rArea, FALSE);
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render all page headers and column headers, report count:' || lReportStack.COUNT);
      END IF;

      -- Render all column-Headers above
      FOR j IN 1..lReportStack.COUNT LOOP
        io_rArea.nX:=lReportStack(j).nXPositionHdr;
        IF j=lReportStack.COUNT THEN
          FOR z IN 1..lReportStack(j).lPageFrameMarkers.COUNT LOOP
            lReportStack(j).lPageFrameMarkers(z).nBorderTop:=io_rArea.nY;
          END LOOP;
        END IF;
        IF    j<lReportStack.COUNT
           OR (    j=lReportStack.COUNT
               AND (   NOT i_bBeforeSubReportHdr
                    OR NOT i_bBeforeSubReportColHdr
                   )
              ) THEN
          IF NOT lReportStack(j).lStartOfPageDone.EXISTS(lReportStack(j).nCurrentPagePointer) THEN
            IF    i_bPageHeaders
               OR j<lReportStack.COUNT THEN
              IF    NOT i_bBeforeSubReportHdr
                 OR j<lReportStack.COUNT THEN
                -- In a subreport, headers may already do not fit
                nDetailHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>j,
                                                     i_rRegion   =>lReportStack(j).rPageHeaderRegion
                                                    );
                IF FK_FITS_IN_PAGE(io_rArea.nY,
                                   nDetailHeight
                                  ) THEN
                  io_rArea:=FK_RENDER_REGION(j, lReportStack(j).rPageHeaderRegion, io_rArea, TRUE);
                ELSE
                  -- new page
                  PR_FINISH_PAGE_AND_START_NEW(io_rArea       =>io_rArea,
                                               i_nStackPos    =>i_nStackPos,
                                               i_bRenderGroups=>FALSE);
                  -- stop
                END IF;
              END IF;
              IF    NOT i_bBeforeSubReportColHdr
                 OR j<lReportStack.COUNT THEN
                -- In a subreport, headers may already do not fit
                nDetailHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>j,
                                                     i_rRegion   =>lReportStack(j).rColumnHeaderRegion
                                                    );
                IF FK_FITS_IN_PAGE(io_rArea.nY,
                                   nDetailHeight
                                  ) THEN
                  io_rArea:=FK_RENDER_REGION(j, lReportStack(j).rColumnHeaderRegion, io_rArea, TRUE);
                ELSE
                  PR_FINISH_PAGE_AND_START_NEW(io_rArea       =>io_rArea,
                                               i_nStackPos    =>i_nStackPos,
                                               i_bRenderGroups=>FALSE);
                  -- stop
                END IF;
              END IF;
            END IF;
            IF i_bRenderGroups THEN
              -- Render all column-Headers above
              PR_RENDER_GROUP_HEADERS(i_nStackPos     =>j,
                                      io_rArea        =>io_rArea,
                                      i_bReprintOnly  =>TRUE,
                                      i_bIsOnNewPage  =>TRUE
                                     );
            END IF;
            lReportStack(j).lStartOfPageDone(lReportStack(j).nCurrentPagePointer):=io_rArea.nY;
          ELSE
            io_rArea.nY:=lReportStack(j).lStartOfPageDone(lReportStack(j).nCurrentPagePointer);
          END IF;
        END IF;
      END LOOP;
      io_rArea.nX:=nXSave;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_GROUP_HEADERS(i_nStackPos     IN NUMBER,
                                    io_rArea        IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                                    i_bReprintOnly  IN BOOLEAN DEFAULT FALSE,
                                    i_bIsOnNewPage  IN BOOLEAN DEFAULT FALSE) IS
    vcText        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    bRenderGroup  BOOLEAN:=FALSE;
    nDetailheight NUMBER;
    nDummy        NUMBER;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render group-headers, group-count/ReprintOnly:' || lReportStack(i_nStackPos).lGroups.COUNT || '/' ||
                                                                                         CASE WHEN i_bReprintOnly THEN
                                                                                           PK_JRXML2PDF_TYPES.YES
                                                                                         ELSE
                                                                                           PK_JRXML2PDF_TYPES.NO
                                                                                         END);
    END IF;
    FOR i IN 1..lReportStack(i_nStackPos).lGroups.COUNT LOOP
      vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>i_nStackPos,
                                     i_vcExpression =>lReportStack(i_nStackPos).lGroups(i).vcExpression,
                                     i_vcPattern    =>NULL
                                    );
      bRenderGroup:=    i_bReprintOnly
                    AND lReportStack(i_nStackPos).lGroups(i).vcReprintHeader=PK_JRXML2PDF_TYPES.YES;
      IF NOT bRenderGroup THEN
        IF NVL(vcText,PK_JRXML2PDF_TYPES.NIL)!=NVL(lReportStack(i_nStackPos).lGroupExpressions(i), PK_JRXML2PDF_TYPES.NIL) THEN
          -- group change, escalade down
          FOR j IN i+1..lReportStack(i_nStackPos).lGroupExpressions.COUNT LOOP
            lReportStack(i_nStackPos).lGroupExpressions(j):=NULL;
          END LOOP;
          -- store new value
          lReportStack(i_nStackPos).lGroupExpressions(i):=vcText;
          bRenderGroup:=TRUE;
        END IF;
      END IF;
      IF bRenderGroup THEN
        -- Eventually start on new page
        IF     NOT i_bIsOnNewPage
           AND (
                   (    lReportStack(i_nStackPos).rQuery.nPreviousRecord IS NOT NULL
                    AND lReportStack(i_nStackPos).lGroups(i).vcStartOnNewPage=PK_JRXML2PDF_TYPES.YES
                   )
                OR (    lReportStack(i_nStackPos).lGroups(i).nMinHeightForPage IS NOT NULL
                    AND NOT FK_FITS_IN_PAGE(io_rArea.nY, lReportStack(i_nStackPos).lGroups(i).nMinHeightForPage)
                   )
               ) THEN
          -- new page
          PR_FINISH_PAGE(io_rArea          =>io_rArea,
                         i_nStackPos       =>i_nStackPos,
                         i_bResetPageNumber=>lReportStack(i_nStackPos).lGroups(i).vcResetPageNumber=PK_JRXML2PDF_TYPES.YES);
          -- Now render all fiels marked as Evaluate later type Group
          PR_RENDER_LATER_FIELDS(i_nStackPos        =>i_nStackPos,
                                 i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_GROUP,
                                 i_vcEvaluationGroup=>lReportStack(i_nStackPos).lGroups(i).vcName
                                );
          -- new page
          PR_START_NEW_PAGE(io_rArea          =>io_rArea,
                            i_nStackPos       =>i_nStackPos,
                            i_bResetPageNumber=>lReportStack(i_nStackPos).lGroups(i).vcResetPageNumber=PK_JRXML2PDF_TYPES.YES,
                            i_bRenderGroups   =>TRUE);
          -- stop here, rest in done in the next recursion level
          -- only case we have something to do if we statrt the new page because of property "start on ny page"
          IF     lReportStack(i_nStackPos).rQuery.nPreviousRecord IS NOT NULL
             AND lReportStack(i_nStackPos).lGroups(i).vcStartOnNewPage=PK_JRXML2PDF_TYPES.YES THEN
            IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
              PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group Header band because of "Start on new page"');
            END IF;
            -- Render group header
            FOR ii IN 1..lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects.COUNT LOOP
              PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                                  i_rBand            =>lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects(ii).nPosition),
                                  o_nbandHeight      =>nDetailHeight,
                                  o_nBandOffset      =>nDummy
                             );

              io_rArea:=FK_RENDER_BAND(i_nStackPos,
                                       lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects(ii).nPosition),
                                       io_rArea,
                                       TRUE,
                                       PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                      );
            END LOOP;
          END IF;
        ELSE
          -- Render group header
          FOR ii IN 1..lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects.COUNT LOOP
            PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                                i_rBand            =>lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects(ii).nPosition),
                                o_nbandHeight      =>nDetailHeight,
                                o_nBandOffset      =>nDummy
                           );
            -- Check if region fits on page
            IF FK_FITS_IN_PAGE(io_rArea.nY, nDetailHeight) THEN
              IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group Header band (same page)');
              END IF;

              io_rArea:=FK_RENDER_BAND(i_nStackPos,
                                       lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects(ii).nPosition),
                                       io_rArea,
                                       TRUE,
                                       PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                      );
            ELSE
              IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group Header band (new page)');
              END IF;

              PR_FINISH_PAGE_AND_START_NEW(io_rArea       =>io_rArea,
                                           i_nStackPos    =>i_nStackPos,
                                           i_bRenderGroups=>FALSE);
              io_rArea:=FK_RENDER_BAND(i_nStackPos,
                                       lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(i).rGroupHeader.lObjects(ii).nPosition),
                                       io_rArea,
                                       TRUE,
                                       PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                      );
            END IF;
          END LOOP;
        END IF;
      END IF;
    END LOOP;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_FINISH_PAGE_AND_START_NEW(io_rArea                 IN OUT NOCOPY PK_JRXML2PDF_TYPES.tArea,
                                         i_nStackPos              IN NUMBER,
                                         i_bInnerToBottom         IN BOOLEAN    DEFAULT FALSE,
                                         i_bBeforeSubReportHdr    IN BOOLEAN    DEFAULT FALSE,
                                         i_bBeforeSubReportColHdr IN BOOLEAN    DEFAULT FALSE,
                                         i_bResetPagenumber       IN BOOLEAN    DEFAULT FALSE,
                                         i_bOneRecordBack         IN BOOLEAN    DEFAULT FALSE,
                                         i_vcStyleToApply         IN PK_JRXML2PDF_TYPES.tStyleName DEFAULT NULL,
                                         i_bRenderGroups          IN BOOLEAN    DEFAULT TRUE,
                                         i_bPageHeaders           IN BOOLEAN    DEFAULT TRUE
                                         ) IS
  BEGIN
    IF i_bOneRecordBack THEN
      lReportStack(i_nStackPos).rQuery.bTreatLastAsCurrent:=TRUE;
    END IF;

    PR_FINISH_PAGE(io_rArea                =>io_rArea,
                   i_nStackPos             =>i_nStackPos,
                   i_bInnerToBottom        =>i_bInnerToBottom,
                   i_bBeforeSubReportHdr   =>i_bBeforeSubReportHdr,
                   i_bBeforeSubReportColHdr=>i_bBeforeSubReportColHdr,
                   i_bResetPagenumber      =>i_bResetPagenumber,
                   i_vcStyleToApply        =>i_vcStyleToApply
                  );

    IF i_bOneRecordBack THEN
      lReportStack(i_nStackPos).rQuery.bTreatLastAsCurrent:=FALSE;
    END IF;

    PR_START_NEW_PAGE(io_rArea                =>io_rArea,
                      i_nStackPos             =>i_nStackPos,
                      i_bInnerToBottom        =>i_bInnerToBottom,
                      i_bBeforeSubReportHdr   =>i_bBeforeSubReportHdr,
                      i_bBeforeSubReportColHdr=>i_bBeforeSubReportColHdr,
                      i_bResetPagenumber      =>i_bResetPagenumber,
                      i_bRenderGroups         =>i_bRenderGroups,
                      i_bPageHeaders          =>i_bPageHeaders
                     );
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RENDER_SUBREPORT(i_nX                IN NUMBER,
                               i_nY                IN NUMBER,
                               i_nWidth            IN NUMBER,
                               i_nHeight           IN NUMBER,
                               i_vcStyle           IN PK_JRXML2PDF_TYPES.tStyleName,
                               i_rArea             IN PK_JRXML2PDF_TYPES.tArea,
                               i_vcReportName      IN PK_JRXML2PDF_TYPES.tName,
                               i_lParamList        IN PK_JRXML2PDF_TYPES.tParamList,
                               i_nMasterRecord     IN NUMBER
                               )
  RETURN PK_JRXML2PDF_TYPES.tArea IS
    rArea               PK_JRXML2PDF_TYPES.tArea:=i_rArea;
    nXSave              NUMBER;
    rReport             PK_JRXML2PDF_TYPES.tReport;
    nColumnFooterHeight NUMBER;
    nPageHeaderheight   NUMBER:=0;
    nPageFooterheight   NUMBER:=0;
    nDetailHeight       NUMBER;
    bSummaryAtEnd       BOOLEAN:=FALSE;
    nDummy              NUMBER;
    nStackPos           NUMBER;

    PROCEDURE PR_RENDER_GROUP_FOOTERS IS
      vcText PK_JRXML2PDF_TYPES.tMaxVarchar2;
      bBreak BOOLEAN:=FALSE;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport-group-footers, group-count:' || lReportStack(nStackPos).lGroups.COUNT);
      END IF;
      FOR i IN 1..lReportStack(nStackPos).lGroups.COUNT LOOP
        IF lReportStack(nStackPos).rQuery.bEOF AND
           lReportStack(nStackPos).rQuery.nNextRecord IS NULL THEN
          bBreak:=TRUE;
        ELSE
          vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>nStackPos,
                                         i_vcExpression =>lReportStack(nStackPos).lGroups(i).vcExpression,
                                         i_vcPattern    =>NULL,
                                         i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NEXT
                                        );
          IF NVL(vcText,PK_JRXML2PDF_TYPES.NIL)!=NVL(lReportStack(nStackPos).lGroupExpressions(i), PK_JRXML2PDF_TYPES.NIL) THEN
            bBreak:=TRUE;
          END IF;
        END IF;
        IF bBreak THEN
          -- group change, render group footer up to current level
          FOR j IN REVERSE i..lReportStack(nStackPos).lGroupExpressions.COUNT LOOP
            -- Render group header
            FOR ii IN 1..lReportStack(nStackPos).lGroups(j).rGroupFooter.lObjects.COUNT LOOP
              PR_CALC_BAND_HEIGHT(i_nStackPos        =>nStackPos,
                                  i_rBand            =>lReportStack(nStackPos).lBands(lReportStack(nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                  o_nbandHeight      =>nDetailHeight,
                                  o_nBandOffset      =>nDummy
                             );
              -- Check if region fits on page
              IF FK_FITS_IN_PAGE(rArea.nY, nDetailHeight) THEN

                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group footer band (same page)');
                END IF;

                rArea:=FK_RENDER_BAND(nStackPos,
                                      lReportStack(nStackPos).lBands(lReportStack(nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                      rArea,
                                      TRUE,
                                      PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                     );
              ELSE

                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group footer band (new page)');
                END IF;

                PR_FINISH_PAGE_AND_START_NEW(io_rArea          =>rArea,
                                             i_nStackPos       =>nStackPos,
                                             i_vcStyleToApply  =>i_vcStyle,
                                             i_bRenderGroups   =>TRUE
                                            );
                rArea:=FK_RENDER_BAND(nStackPos,
                                      lReportStack(nStackPos).lBands(lReportStack(nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                      rArea,
                                      TRUE,
                                      PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                     );
              END IF;
            END LOOP;
          END LOOP;
          -- exit
          EXIT;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_RENDER_SUBREPORT_BEGIN IS
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Calculate heights for regions');
      END IF;
      nColumnFooterHeight:=0;
      nPageHeaderheight:=0;
      nPageFooterheight:=0;
      FOR i IN 1..lReportStack.COUNT LOOP
        nColumnFooterHeight:=nColumnFooterHeight+lReportStack(i).nColumnFtrHeight;
        nPageHeaderheight  :=nPageHeaderheight  +lReportStack(i).nPageHdrHeight;
        nPageFooterheight  :=nPageFooterheight  +lReportStack(i).nPageFtrHeight;
      END LOOP;
      -- In a subreport, headers may already do not fit
      nDetailHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>nStackPos,
                                           i_rRegion   =>lReportStack(nStackPos).rTitleRegion
                                          );
      IF FK_FITS_IN_PAGE(rArea.nY,
                         nDetailHeight
                        ) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport title region (same page)');
        END IF;
        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rTitleRegion, rArea, TRUE);
      ELSE

        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport title region (new page)');
        END IF;

        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea                =>rArea,
                                     i_nStackPos             =>nStackPos,
                                     i_bBeforeSubReportHdr   =>TRUE,
                                     i_bBeforeSubReportColHdr=>TRUE,
                                     i_vcStyleToApply        =>i_vcStyle
                                    );
        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rTitleRegion, rArea, TRUE);
      END IF;
      IF lReportStack(nStackPos).vcTitleOnNewPage=PK_JRXML2PDF_TYPES.YES THEN
        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea                =>rArea,
                                     i_nStackPos             =>nStackPos,
                                     i_bBeforeSubReportHdr   =>TRUE,
                                     i_bBeforeSubReportColHdr=>TRUE,
                                     i_vcStyleToApply        =>i_vcStyle
                                    );
      END IF;
      nDetailHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>nStackPos,
                                           i_rRegion  =>lReportStack(nStackPos).rPageHeaderRegion
                                          );
      IF FK_FITS_IN_PAGE(rArea.nY,
                         nDetailHeight
                        ) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport page header region (same page)');
        END IF;

        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rPageHeaderRegion, rArea, TRUE);
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport page header region (new page)');
        END IF;

        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea                =>rArea,
                                     i_nStackPos             =>nStackPos,
                                     i_bBeforeSubReportHdr   =>TRUE,
                                     i_bBeforeSubReportColHdr=>TRUE,
                                     i_vcStyleToApply        =>i_vcStyle
                                    );
        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rPageHeaderRegion, rArea, TRUE);
      END IF;

      nDetailHeight:=FK_CALC_REGION_HEIGHT(i_nStackPos=>nStackPos,
                                           i_rRegion  =>lReportStack(nStackPos).rColumnHeaderRegion
                                          );
      IF FK_FITS_IN_PAGE(rArea.nY,
                         nDetailHeight
                        ) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport column header region (same page)');
        END IF;

        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rColumnHeaderRegion, rArea, TRUE);
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport column header region (new page)');
        END IF;

        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea                =>rArea,
                                     i_nStackPos             =>nStackPos,
                                     i_bBeforeSubReportHdr   =>FALSE,
                                     i_bBeforeSubReportColHdr=>TRUE,
                                     i_vcStyleToApply        =>i_vcStyle
                                    );
        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rColumnHeaderRegion, rArea, TRUE);
      END IF;
    END;

    PROCEDURE PR_RENDER_RECORD IS
    BEGIN

      -- evaluate group-conditions and check changes
      PR_RENDER_GROUP_HEADERS(i_nStackPos    =>nStackPos,
                              io_rArea       =>rArea
                             );

      FOR ii IN 1..lReportStack(nStackPos).rDetailRegion.lObjects.COUNT LOOP
        PR_CALC_BAND_HEIGHT(i_nStackPos        =>nStackPos,
                            i_rBand            =>lReportStack(nStackPos).lBands(lReportStack(nStackPos).rDetailRegion.lObjects(ii).nPosition),
                            o_nbandHeight      =>nDetailHeight,
                            o_nBandOffset      =>nDummy
                       );
        -- Check if region fits on page
        IF FK_FITS_IN_PAGE(rArea.nY,
                           nDetailHeight
                          ) THEN
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport record (same page),record:' || lReportStack(nStackPos).rQuery.nRecordPosition);
          END IF;
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('nY / detailHeight / ColFtrHeight / PageFtrHeight:' || TO_CHAR(rArea.nY) || '/' ||
                                                                                                    TO_CHAR(nDetailHeight) || '/' ||
                                                                                                    TO_CHAR(nColumnFooterHeight) || '/' ||
                                                                                                    TO_CHAR(nPageFooterheight)
                                            );
          END IF;

          rArea:=FK_RENDER_BAND(nStackPos,
                                lReportStack(nStackPos).lBands(lReportStack(nStackPos).rDetailRegion.lObjects(ii).nPosition),
                                rArea,
                                TRUE,
                                PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                               );
        ELSE
          IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
            PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport detail region (new page),record:' || lReportStack(nStackPos).rQuery.nRecordPosition);
          END IF;
          IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
            PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('nY / detailHeight / ColFtrHeight / PageFtrHeight:' || TO_CHAR(rArea.nY) || '/' ||
                                                                                                    TO_CHAR(nDetailHeight) || '/' ||
                                                                                                    TO_CHAR(nColumnFooterHeight) || '/' ||
                                                                                                    TO_CHAR(nPageFooterheight)
                                            );
          END IF;
          -- Finish page
          PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                       i_nStackPos     =>nStackPos,
                                       i_bOneRecordBack=>TRUE,
                                       i_vcStyleToApply=>i_vcStyle
                                       );

          rArea:=FK_RENDER_BAND(nStackPos,
                                lReportStack(nStackPos).lBands(lReportStack(nStackPos).rDetailRegion.lObjects(ii).nPosition),
                                rArea,
                                TRUE,
                                PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                               );
        END IF;
      END LOOP;
      -- evaluate group-conditions and check changes
      PR_RENDER_GROUP_FOOTERS;
      -- clear text-cache
      PK_JRXML2PDF_UTIL.PR_CLEAR_TEXT_CACHE;
    END;

    PROCEDURE PR_RENDER_SUBREPORT_END IS
      rSummaryRegion       PK_JRXML2PDF_TYPES.tRegion;
      nSummaryHeight       NUMBER;
      rColumnFooterRegion  PK_JRXML2PDF_TYPES.tRegion;
    BEGIN
      -- Regions are skipped in layout of table
      IF lReportStack(nStackPos).nTyp=PK_JRXML2PDF_TYPES.REPORT_TYP_TABLE THEN
        rSummaryRegion       :=lReportStack(nStackPos).rColumnFooterRegion;
        rColumnFooterRegion  :=lReportStack(nStackPos).rSummaryRegion;
      ELSE
        rSummaryRegion       :=lReportStack(nStackPos).rSummaryRegion;
        rColumnFooterRegion  :=lReportStack(nStackPos).rColumnFooterRegion;
      END IF;

      IF FK_FITS_IN_PAGE(rArea.nY,
                         lReportStack(nStackPos).nSummaryHeight
                        ) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport summary region (same page)');
        END IF;
        IF lReportStack(nStackPos).vcSummaryOnNewPage=PK_JRXML2PDF_TYPES.YES THEN
          PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                       i_nStackPos     =>nStackPos,
                                       i_vcStyleToApply=>i_vcStyle,
                                       i_bPageHeaders  =>lReportStack(nStackPos).vcSummaryWithHdrFtr=PK_JRXML2PDF_TYPES.YES
                                      );
        END IF;
        rArea:=FK_RENDER_REGION(nStackPos, rSummaryRegion, rArea, TRUE);
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport summary region (new page)');
        END IF;
        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                     i_nStackPos     =>nStackPos,
                                     i_vcStyleToApply=>i_vcStyle,
                                     i_bPageHeaders  =>lReportStack(nStackPos).vcSummaryWithHdrFtr=PK_JRXML2PDF_TYPES.YES
                                    );
        rArea:=FK_RENDER_REGION(nStackPos, rSummaryRegion, rArea, TRUE);
        bSummaryAtEnd:=TRUE;
      END IF;

      IF NOT bSummaryAtEnd THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport column footer region');
        END IF;
        rArea:=FK_RENDER_REGION(nStackPos, rColumnFooterRegion, rArea, TRUE);

        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport page footer region');
        END IF;
        rArea:=FK_RENDER_REGION(nStackPos, lReportStack(nStackPos).rPageFooterRegion, rArea, FALSE);
      END IF;

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render subreport later fields');
      END IF;
      IF lReportStack(nStackPos).lGroups.COUNT>0 THEN
        -- Now render all fiels marked as Evaluate later type Group
        PR_RENDER_LATER_FIELDS(i_nStackPos        =>nStackPos,
                               i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_GROUP,
                               i_vcEvaluationGroup=>lReportStack(nStackPos).lGroups(1).vcName
                              );
      END IF;
      -- Now render all fiels marked as Evaluate later type Report
      PR_RENDER_LATER_FIELDS(i_nStackPos        =>nStackPos,
                             i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_REPORT
                            );
    END;

    PROCEDURE PR_ADJUST_COLUMNS(i_rRegion IN PK_JRXML2PDF_TYPES.tRegion) IS
      nAdjustWidth NUMBER:=0;
      nMaxWidth    NUMBER:=0;
      rband        PK_JRXML2PDF_TYPES.tBand;
    BEGIN
      FOR i IN 1..i_rRegion.lObjects.COUNT LOOP
        IF i_rRegion.lObjects(i).nType=PK_JRXML2PDF_TYPES.BAND THEN
          rBand:=lReportStack(nStackPos).lBands(i_rRegion.lObjects(i).nPosition);
          FOR j IN 1..rBand.lObjects.COUNT LOOP
            IF rBand.lObjects(j).nType=PK_JRXML2PDF_TYPES.SUBFRAME THEN
              lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nX:=lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nX-nAdjustWidth;
              IF lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).vcWhenExpression IS NOT NULL THEN
                -- evaluate expression for master-report
                IF NOT FK_EVALUATE_CONDITION(i_nStackPos    =>nStackPos-1,
                                             i_vcExpression =>lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).vcWhenExpression
                                            ) THEN
                  -- column is hidden, remember width to adjust following columns
                  nAdjustWidth:=nAdjustWidth+lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nWidth;
                ELSE
                  nMaxWidth:=GREATEST(nMaxWidth, lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nX+
                                                 lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nWidth
                                     );
                END IF;
              ELSE
                nMaxWidth:=GREATEST(nMaxWidth, lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nX+
                                               lReportStack(nStackPos).lBands(rBand.lObjects(j).nPosition).nWidth
                                   );
              END IF;
            END IF;
          END LOOP;
        END IF;
      END LOOP;
      FOR i IN 1..lReportStack(nStackPos).lPageFrameMarkers.COUNT LOOP
        lReportStack(nStackPos).lPageFrameMarkers(i).nBorderWidth:=GREATEST(lReportStack(nStackPos).lPageFrameMarkers(i).nBorderWidth, nMaxWidth);
      END LOOP;
    END;

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Starting subreport:' || i_vcReportName);
    END IF;

    rReport:=FK_LOAD_REPORT(i_vcReportName, i_lParamList);

    rArea.nX:=i_nX+rReport.nLeftMargin;
    rArea.nY:=i_nY+rReport.nTopMargin;
    rReport.nXPositionHdr:=i_nX+rReport.nLeftMargin;


    PR_PUSH_SUBREPORT_DATA(rReport);
    nStackPos:=lReportStack.COUNT;

    -- set a page-frame-marker if style to be applied is set
    IF i_vcStyle IS NOT NULL THEN
      -- mark the initial y-Position, in case a style has to be applied
      IF rReport.nTyp=PK_JRXML2PDF_TYPES.REPORT_TYP_TABLE THEN
        -- the width will be calculated in PR_ADJUST_COLUMNS
        PR_PUSH_PAGE_FRAME_MARKER(i_nStackPos   =>nStackPos,
                                  i_nBorderTop  =>rArea.nY,
                                  i_nBorderWidth=>0,
                                  i_rStyle      =>FK_GET_STYLE(i_nStackPos=>nStackPos,
                                                               i_vcStyle  =>i_vcStyle
                                                              )
                                 );
      ELSE
        -- width is the report's total width
        PR_PUSH_PAGE_FRAME_MARKER(i_nStackPos   =>nStackPos,
                                  i_nBorderTop  =>rArea.nY,
                                  i_nBorderWidth=>i_nWidth,
                                  i_rStyle      =>FK_GET_STYLE(i_nStackPos=>nStackPos,
                                                               i_vcStyle  =>i_vcStyle
                                                              )
                                 );
      END IF;
    END IF;

    IF FK_FETCH_ROW(nStackPos) THEN

      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render subreport');
      END IF;

      IF lReportStack(nStackPos).nTyp=PK_JRXML2PDF_TYPES.REPORT_TYP_TABLE THEN
        -- check if there are any condiions on columns, and if so
        -- evaluate the conditions and adjust column-positions, if needed
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Subreport is table, check columns');
        END IF;
        PR_ADJUST_COLUMNS(lReportStack(nStackPos).rTitleRegion);
        PR_ADJUST_COLUMNS(lReportStack(nStackPos).rColumnHeaderRegion);
        PR_ADJUST_COLUMNS(lReportStack(nStackPos).rDetailRegion);
        PR_ADJUST_COLUMNS(lReportStack(nStackPos).rColumnFooterRegion);
        PR_ADJUST_COLUMNS(lReportStack(nStackPos).rSummaryRegion);
      END IF;

      PR_RENDER_SUBREPORT_BEGIN;

      LOOP
        PR_RENDER_RECORD;

        EXIT WHEN NOT FK_FETCH_ROW(nStackPos);
      END LOOP;

      PR_RENDER_SUBREPORT_END;

      -- finally apply subreport-style
      PR_APPLY_REPORT_STYLE(i_nStackPos     =>nStackPos,
                            i_nX            =>i_nX,
                            i_nBottom       =>rArea.nY
                           );

      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('End of subreport record-count:' || lReportStack(nStackPos).rQuery.nRecordRead);
      END IF;

      -- Reset X-Value to Input-value
      rArea.nX:=i_rArea.nX;
      rArea.nY:=rArea.nY+lReportStack(nStackPos).nBottomMargin;

    END IF;

    -- Take report from stack
    PR_POP_SUBREPORT_DATA;

    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('End of subreport');
    END IF;
    RETURn rArea;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_REPORT(i_nStackPos            IN NUMBER,
                             i_bFinishPdf           IN BOOLEAN,
                             i_bKeepPageNosForLater IN BOOLEAN) IS
    rArea               PK_JRXML2PDF_TYPES.tArea;
    rDummyArea          PK_JRXML2PDF_TYPES.tArea;
    nColumnFooterHeight NUMBER;
    nPageHeaderheight   NUMBER;
    nPageFooterheight   NUMBER;
    nDetailHeight       NUMBER;
    nSummaryHeight      NUMBER;
    nDummy              NUMBER;


    PROCEDURE PR_RENDER_GROUP_FOOTERS IS
      vcText PK_JRXML2PDF_TYPES.tMaxVarchar2;
      bBreak BOOLEAN:=FALSE;
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render group-footers, group-count:' || lReportStack(i_nStackPos).lGroups.COUNT);
      END IF;
      FOR i IN 1..lReportStack(i_nStackPos).lGroups.COUNT LOOP
        IF     lReportStack(i_nStackPos).rQuery.bEOF
           AND lReportStack(i_nStackPos).rQuery.nNextRecord IS NULL THEN
          bBreak:=TRUE;
        ELSE
          vcText:=FK_EVALUATE_EXPRESSION(i_nStackPos    =>i_nStackPos,
                                         i_vcExpression =>lReportStack(i_nStackPos).lGroups(i).vcExpression,
                                         i_vcPattern    =>NULL,
                                         i_nRecordOffset=>PK_JRXML2PDF_TYPES.OFFSET_NEXT
                                        );
          IF NVL(vcText,PK_JRXML2PDF_TYPES.NIL)!=NVL(lReportStack(i_nStackPos).lGroupExpressions(i), PK_JRXML2PDF_TYPES.NIL) THEN
            bBreak:=TRUE;
          END IF;
        END IF;
        IF bBreak THEN
          -- group change, render group footer up to current level
          FOR j IN REVERSE i..lReportStack(i_nStackPos).lGroupExpressions.COUNT LOOP
            -- Render group header
            FOR ii IN 1..lReportStack(i_nStackPos).lGroups(j).rGroupFooter.lObjects.COUNT LOOP
              PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                                  i_rBand            =>lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                  o_nbandHeight      =>nDetailHeight,
                                  o_nBandOffset      =>nDummy
                             );
              -- Check if region fits on page
              IF FK_FITS_IN_PAGE(rArea.nY, nDetailHeight) THEN

                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group footer band (same page)');
                END IF;

                rArea:=FK_RENDER_BAND(i_nStackPos,
                                      lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                      rArea,
                                      TRUE,
                                      PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                     );
              ELSE

                IF PK_JRXML2PDF_LOG.FK_IS_FINE THEN
                  PK_JRXML2PDF_LOG.PR_LOG_FINE('Render group footer band (new page)');
                END IF;

                PR_FINISH_PAGE_AND_START_NEW(io_rArea       =>rArea,
                                             i_nStackPos    =>i_nStackPos,
                                             i_bRenderGroups=>TRUE
                                            );
                rArea:=FK_RENDER_BAND(i_nStackPos,
                                      lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).lGroups(j).rGroupFooter.lObjects(ii).nPosition),
                                      rArea,
                                      TRUE,
                                      PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                                     );
              END IF;
            END LOOP;
          END LOOP;
          -- exit
          EXIT;
        END IF;
      END LOOP;
    END;

    PROCEDURE PR_INITIALIZE IS
    BEGIN

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Setup page width/height:' || TO_CHAR(lReportStack(i_nStackPos).nPageWidth) || '/' ||
                                                                        TO_CHAR(lReportStack(i_nStackPos).nPageHeight));
      END IF;

      PR_SETUP_PAGE(i_nWidth       =>lReportStack(i_nStackPos).nPageWidth,
                    i_nHeight      =>lReportStack(i_nStackPos).nPageHeight,
                    i_nLeftMargin  =>lReportStack(i_nStackPos).nLeftMargin,
                    i_nRightMargin =>lReportStack(i_nStackPos).nRightMargin,
                    i_nTopMargin   =>lReportStack(i_nStackPos).nTopMargin,
                    i_nBottomMargin=>lReportStack(i_nStackPos).nBottomMargin
                   );
      -- create a new page
      AS_PDF3_MOD.new_page;
      -- Initialize location-pointer
      rArea.nX:=0;
      lReportStack(i_nStackPos).nXPositionHdr:=0;
      rArea.nY:=0;
      IF i_bFinishPdf THEN
        rArea.nPage:=1;
      ELSE
        rArea.nPage:=lReportStack(i_nStackPos).nCurrentPagePointer;
      END IF;
      
      nPageHeaderheight  :=FK_CALC_REGION_HEIGHT(i_nStackPos, lReportStack(i_nStackPos).rPageHeaderRegion);
      nPageFooterheight  :=FK_CALC_REGION_HEIGHT(i_nStackPos, lReportStack(i_nStackPos).rPageFooterRegion);
      nSummaryheight     :=FK_CALC_REGION_HEIGHT(i_nStackPos, lReportStack(i_nStackPos).rSummaryRegion);
      nColumnFooterHeight:=lReportStack(i_nStackPos).nColumnFtrHeight;
    END;

    PROCEDURE PR_RENDER_REPORT_BEGIN IS
    BEGIN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render first background');
      END IF;
      -- Background for the page
      rDummyArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rBackgroundRegion, rArea, FALSE);

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render title-region');
      END IF;
      rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rTitleRegion, rArea, TRUE);

      IF lReportStack(i_nStackPos).vcTitleOnNewPage=PK_JRXML2PDF_TYPES.YES THEN
        -- Finish page
        PR_FINISH_PAGE_AND_START_NEW(io_rArea                =>rArea,
                                     i_nStackPos             =>i_nStackPos,
                                     i_bBeforeSubReportHdr   =>TRUE,
                                     i_bBeforeSubReportColHdr=>TRUE,
                                     i_bPageHeaders          =>FALSE
                                    );
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render first page header');
      END IF;
      rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rPageHeaderRegion, rArea, TRUE);

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render first column header');
      END IF;
      rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnHeaderRegion, rArea, TRUE);
    END;

    PROCEDURE PR_RENDER_REPORT_END IS
    BEGIN
      IF FK_FITS_IN_PAGE(rArea.nY, nSummaryHeight) THEN

        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render summary on same page');
        END IF;
        IF lReportStack(i_nStackPos).vcSummaryOnNewPage=PK_JRXML2PDF_TYPES.YES THEN
          PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                       i_nStackPos     =>i_nStackPos,
                                       i_bPageHeaders  =>lReportStack(i_nStackPos).vcSummaryWithHdrFtr=PK_JRXML2PDF_TYPES.YES
                                      );
        END IF;

        rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rSummaryRegion, rArea, TRUE);
      ELSE

        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render summary on new page');
        END IF;
        PR_FINISH_PAGE_AND_START_NEW(io_rArea   =>rArea,
                                     i_nStackPos=>i_nStackPos
                                    );
        rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rSummaryRegion, rArea, TRUE);
      END IF;

      IF lReportStack(i_nStackPos).vcFloatColumnFooter=PK_JRXML2PDF_TYPES.YES THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render last column footer (floating)');
        END IF;

        rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnFooterRegion, rArea, TRUE);
        rArea.nY:=rPageSetup.nPageHeight-
                  rPageSetup.nBottomMargin-
                  rPageSetup.nTopMargin-
                  nPageFooterheight;
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render last column footer (bottom)');
        END IF;

        rArea.nY:=rPageSetup.nPageHeight-
                  rPageSetup.nBottomMargin-
                  rPageSetup.nTopMargin-
                  lReportStack(i_nStackPos).nColumnFtrHeight-
                  nPageFooterheight;
        rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rColumnFooterRegion, rArea, TRUE);
      END IF;
      IF    (    lReportStack(i_nStackPos).vcSummaryOnNewPage=PK_JRXML2PDF_TYPES.YES
             AND lReportStack(i_nStackPos).vcSummaryWithHdrFtr=PK_JRXML2PDF_TYPES.YES
            )
         OR (lReportStack(i_nStackPos).vcSummaryOnNewPage=PK_JRXML2PDF_TYPES.NO) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render last page footer');
        END IF;
        rArea:=FK_RENDER_REGION(i_nStackPos, lReportStack(i_nStackPos).rPageFooterRegion, rArea, FALSE);
      END IF;

      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Render later fields');
      END IF;
      -- evaluate later for last group
      IF lReportStack(i_nStackPos).lGroups.COUNT>0 THEN
        -- Now render all fiels marked as Evaluate later type Group
        PR_RENDER_LATER_FIELDS(i_nStackPos        =>i_nStackPos,
                               i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_GROUP,
                               i_vcEvaluationGroup=>lReportStack(i_nStackPos).lGroups(1).vcName
                              );
      END IF;
      -- Now render all fiels marked as Evaluate later type Report
      PR_RENDER_LATER_FIELDS(i_nStackPos        =>i_nStackPos,
                             i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_REPORT,
                             i_bKeepPagenumber  =>i_bKeepPageNosForLater
                            );
    END;

    PROCEDURE PR_RENDER_RECORD IS
    BEGIN
      -- evaluate group-conditions and check changes
      PR_RENDER_GROUP_HEADERS(i_nStackPos=>i_nStackPos,
                              io_rArea   =>rArea
                             );

      -- Loop over all bands of the detail
      FOR ii IN 1..lReportStack(i_nStackPos).rDetailRegion.lObjects.COUNT LOOP
        PR_CALC_BAND_HEIGHT(i_nStackPos        =>i_nStackPos,
                            i_rBand            =>lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).rDetailRegion.lObjects(ii).nPosition),
                            o_nbandHeight      =>nDetailHeight,
                            o_nBandOffset      =>nDummy
                       );
        -- Check if region fits on page
        IF FK_FITS_IN_PAGE(rArea.nY, nDetailHeight) THEN

          IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
            PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render record on same page,record:' || lReportStack(i_nStackPos).rQuery.nRecordPosition);
          END IF;
          rArea:=FK_RENDER_BAND(i_nStackPos,
                                lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).rDetailRegion.lObjects(ii).nPosition),
                                rArea,
                                TRUE,
                                PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                               );
        ELSE
          -- skip to last record to have the correct data in the buffer
          IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
            PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render record on new page,record:' || lReportStack(i_nStackPos).rQuery.nRecordPosition);
          END IF;
          PR_FINISH_PAGE_AND_START_NEW(io_rArea        =>rArea,
                                       i_nStackPos     =>i_nStackPos,
                                       i_bOneRecordBack=>TRUE
                                      );

          rArea:=FK_RENDER_BAND(i_nStackPos,
                                lReportStack(i_nStackPos).lBands(lReportStack(i_nStackPos).rDetailRegion.lObjects(ii).nPosition),
                                rArea,
                                TRUE,
                                PK_JRXML2PDF_TYPES.RENDER_FRAME_NONE
                               );
        END IF;
      END LOOP;
      -- evaluate group-conditions and check changes
      PR_RENDER_GROUP_FOOTERS;
      -- clear text-cache
      PK_JRXML2PDF_UTIL.PR_CLEAR_TEXT_CACHE;
    END;
  BEGIN

    IF FK_FETCH_ROW(i_nStackPos) THEN

      -- There are records, start
      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Initialize the report');
      END IF;
      PR_INITIALIZE;

      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render begin of report');
      END IF;
      PR_RENDER_REPORT_BEGIN;

      IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
        PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Render records');
      END IF;
      LOOP

        PR_RENDER_RECORD;
        EXIT WHEN NOT FK_FETCH_ROW(i_nStackPos);
      END LOOP;

      IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Records rendered:' || lReportStack(i_nStackPos).rQuery.nRecordRead);
        PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Render end of report');
      END IF;
      PR_RENDER_REPORT_END;

      IF i_bFinishPdf THEN
        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Finish pdf');
        END IF;

        PR_POP_SUBREPORT_DATA;
        PR_FINISH_PDF;
      ELSE
        IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
          PK_JRXML2PDF_LOG.PR_LOG_PROCESS('pdf not finished');
        END IF;
      END IF;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CLEAN_UP IS
  BEGIN
    rPageSetup:=NULL;
    lReportStack.DELETE;
    PK_JRXML2PDF_LOADER.PR_CLEAR_CACHE;
    lDatePattern.DELETE;
    PK_JRXML2PDF_UTIL.PR_CLEAR_TEXT_CACHE;
    rLastStyle:=NULL;
    rLastStyleName:=PK_JRXML2PDF_TYPES.NIL;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SAVE_TO_FILE(i_blData     IN BLOB,
                            i_vcDir      IN VARCHAR2,
                            i_vcFilename IN VARCHAR2) IS
    LEN   CONSTANT NUMBER:=32767;
    rFile UTL_FILE.FILE_TYPE;
  BEGIN
    rFile:= UTL_FILE.FOPEN(i_vcDir, i_vcFilename, 'wb');

    FOR i IN 0..TRUNC((DBMS_LOB.GETLENGTH(i_blData)-1)/LEN) LOOP
      UTL_FILE.PUT_RAW(rFile,
                       DBMS_LOB.SUBSTR(i_blData, LEN, i*LEN+1)
                      );
    END LOOP;
    UTL_FILE.FCLOSE(rFile);
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RESET_PAGE_SETUP IS
  BEGIN
    rPageSetup.nPageWidth   :=9999;
    rPageSetup.nPageHeight  :=9999;
    rPageSetup.nLeftMargin  :=0;
    rPageSetup.nRightMargin :=0;
    rPageSetup.nTopMargin   :=0;
    rPageSetup.nBottomMargin:=0;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_COPY_PARAMS(i_lParams       IN  tParamList,
                           o_lLocaleParams OUT PK_JRXML2PDF_TYPES.tParamList,
                           o_vcTitle       OUT PK_JRXML2PDF_TYPES.tMaxVarchar2,
                           o_vcAuthor      OUT PK_JRXML2PDF_TYPES.tMaxVarchar2,
                           o_vcSubject     OUT PK_JRXML2PDF_TYPES.tMaxVarchar2,
                           o_vcKeywords    OUT PK_JRXML2PDF_TYPES.tMaxVarchar2
                          ) IS
  BEGIN
    -- Convert parameterlist, take over PDF-parameters
    FOR i IN 1..i_lParams.COUNT LOOP
      o_lLocaleParams(i).vcName:=i_lParams(i).vcName;
      o_lLocaleParams(i).vcValue:=i_lParams(i).vcValue;
      -- extract PDF-parameters, if set
      IF o_lLocaleParams(i).vcName='$$TITLE$$' THEN
        o_vcTitle:=o_lLocaleParams(i).vcValue;
      ELSIF o_lLocaleParams(i).vcName='$$AUTHOR$$' THEN
        o_vcAuthor:=o_lLocaleParams(i).vcValue;
      ELSIF o_lLocaleParams(i).vcName='$$SUBJECT$$' THEN
        o_vcSubject:=o_lLocaleParams(i).vcValue;
      ELSIF o_lLocaleParams(i).vcName='$$KEYWORDS$$' THEN
        o_vcKeywords:=o_lLocaleParams(i).vcValue;
      END IF;
    END LOOP;
  END;
  
  FUNCTION FK_RUN(i_vcName  IN VARCHAR2,
                  i_lParams IN tParamList)
  RETURN BLOB IS
    rReport       PK_JRXML2PDF_TYPES.tReport;
    lLocaleParams PK_JRXML2PDF_TYPES.tParamList;
    vcTitle       PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcAuthor      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcSubject     PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcKeywords    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    

  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Initializing PK_JRXML_REPGEN-Version:' || VERSION_NUMBER);
    END IF;

    PR_CLEAN_UP;

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Initializing Standard-fonts');
    END IF;

    PR_COPY_PARAMS(i_lParams       =>i_lParams,
                   o_lLocaleParams =>lLocaleParams,
                   o_vcTitle       =>vcTitle,
                   o_vcAuthor      =>vcAuthor,
                   o_vcSubject     =>vcSubject,
                   o_vcKeywords    =>vcKeywords
                  );

    PK_JRXML2PDF_UTIL.PR_INIT_FONTS(lLocaleParams);
    -- initialize setup
    PR_RESET_PAGE_SETUP;

    blPDF:=NULL;

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Load report ' || i_vcName);
    END IF;

    -- Initialize the PDF
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('initialize pdf');
    END IF;
    PR_INIT_PDF(i_vcTitle   =>vcTitle,
                i_vcAuthor  =>vcAuthor,
                i_vcSubject =>vcSubject,
                i_vcKeywords=>vcKeywords
               );

    rReport:=FK_LOAD_REPORT(i_vcReportName =>i_vcName,
                            i_lParamList   =>lLocaleParams);

    -- Add to stack
    PR_PUSH_SUBREPORT_DATA(rReport);

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Render report');
    END IF;

    PR_RENDER_REPORT(i_nStackPos           =>1,
                     i_bFinishPdf          =>TRUE,
                     i_bKeepPageNosForLater=>FALSE);

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Clean-up');
    END IF;

    PR_CLEAN_UP;

    RETURN blPDF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RUN(i_vcName  IN VARCHAR2)
  RETURN BLOB IS
    lEmptyParams tParamList;
  BEGIN
    RETURN FK_RUN(i_vcName  =>i_vcName,
                  i_lParams =>lEmptyParams
                 );
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_RUN(i_lReports         IN PK_JRXML2PDF_TYPES.tReportNameList,
                  i_lParams          IN tParamList,
                  i_bResetPageNumber IN BOOLEAN)
  RETURN BLOB IS
    rReport         PK_JRXML2PDF_TYPES.tReport;
    lLocaleParams   PK_JRXML2PDF_TYPES.tParamList;
    vcTitle         PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcAuthor        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcSubject       PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcKeywords      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nLastPage       NUMBER;    
    lLastEvalfields PK_JRXML2PDF_TYPES.tFieldList;
    lLastEvalPageno PK_JRXML2PDF_TYPES.tNumList;
    lLastStyles     PK_JRXML2PDF_TYPES.tStyleList;
    rStyle          PK_JRXML2PDF_TYPES.tSimpleStyle;
    vcStyle         PK_JRXML2PDF_TYPES.tName;
    iLastEvalPos    PLS_INTEGER:=0;
    iPos            PLS_INTEGER;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Initializing PK_JRXML_REPGEN-Version:' || VERSION_NUMBER);
    END IF;

    PR_CLEAN_UP;

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Initializing Standard-fonts');
    END IF;

    PR_COPY_PARAMS(i_lParams       =>i_lParams,
                   o_lLocaleParams =>lLocaleParams,
                   o_vcTitle       =>vcTitle,
                   o_vcAuthor      =>vcAuthor,
                   o_vcSubject     =>vcSubject,
                   o_vcKeywords    =>vcKeywords
                  );

    PK_JRXML2PDF_UTIL.PR_INIT_FONTS(lLocaleParams);
    -- initialize setup
    PR_RESET_PAGE_SETUP;

    blPDF:=NULL;

    -- Initialize the PDF
    IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
      PK_JRXML2PDF_LOG.PR_LOG_DETAIL('initialize pdf');
    END IF;
    PR_INIT_PDF(i_vcTitle   =>vcTitle,
                i_vcAuthor  =>vcAuthor,
                i_vcSubject =>vcSubject,
                i_vcKeywords=>vcKeywords
               );

    FOR i IN 1..i_lReports.COUNT LOOP
      IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
        PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Load report ' || i_lReports(i));
      END IF;


      rReport:=FK_LOAD_REPORT(i_vcReportName =>i_lReports(i),
                              i_lParamList   =>lLocaleParams);
  
      IF     i>1 THEN
        rReport.nCurrentPagePointer:=nLastPage+1;
        -- fill pagenumber array with dummies
        FOR i IN 1..nLastPage LOOP
          rReport.lPageNumbers(i):=i;
          rReport.lLogicalPageNumbers(i):=i;
        END LOOP;
        rReport.lPageNumbers(rReport.nCurrentPagePointer):=rReport.nCurrentPagePointer;
        IF i_bResetPageNumber THEN
          rReport.lLogicalPageNumbers(rReport.nCurrentPagePointer):=1;
        ELSE
          rReport.lLogicalPageNumbers(rReport.nCurrentPagePointer):=nLastPage+1;
        END IF;
        -- take over outstanding evaluationfields
        rReport.lEvalLaterFields:=lLastEvalfields;
        rReport.lEvalLaterPageNo:=lLastEvalPageno;
        vcStyle:=lLastStyles.FIRST;
        WHILE vcStyle IS NOT NULL LOOP
          rReport.lStyles(vcStyle):=lLastStyles(vcStyle);
          vcStyle:=lLastStyles.NEXT(vcStyle);
        END LOOP;
      END IF;
      -- Add to stack
      PR_PUSH_SUBREPORT_DATA(rReport);
  
      IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
        PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Render report');
      END IF;
  
      PR_RENDER_REPORT(i_nStackPos           =>1,
                       i_bFinishPdf          =>FALSE,
                       i_bKeepPageNosForLater=>NOT i_bResetPageNumber);
  
      nLastPage:=lReportStack(1).nCurrentPagePointer;

      IF i=i_lReports.COUNT  THEN
        -- add the report-margins to the last added fields
        iPos:=lReportStack(1).lEvalLaterFields.NEXT(iLastEvalPos);
        WHILE iPos IS NOT NULL LOOP
          lReportStack(1).lEvalLaterFields(iPos).nX:= lReportStack(1).lEvalLaterFields(iPos).nX
                                                     +rPageSetup.nLeftMargin;
          lReportStack(1).lEvalLaterFields(iPos).nY:= lReportStack(1).lEvalLaterFields(iPos).nY
                                                     -rPageSetup.nPageHeight
                                                     +rPageSetup.nTopMargin;
          iPos:=lReportStack(1).lEvalLaterFields.NEXT(iPos);
        END LOOP;
        -- reset margins, for they are included in the x and y-coordinates
        rPageSetup.nLeftMargin:=0;
        rPageSetup.nPageHeight:=0;
        rPageSetup.nTopMargin:=0;
        -- Now render all fiels marked as Evaluate later type Report
        PR_RENDER_LATER_FIELDS(i_nStackPos        =>1,
                               i_vcEvaluationTime =>PK_JRXML2PDF_TYPES.EVALUATION_REPORT
                              );
      ELSE
        -- store outstanding evaluation fields and process them in next report
        lLastEvalfields:=lReportStack(1).lEvalLaterFields;
        lLastEvalPageno:=lReportStack(1).lEvalLaterPageNo;
        -- to keep the styles, take them from the current report, rename them and store them
        iPos:=lLastEvalfields.NEXT(iLastEvalPos);
        WHILE iPos IS NOT NULL LOOP
          IF lLastEvalfields(iPos).vcStyle IS NOT NULL THEN
            -- preserve style
            rStyle:=FK_GET_STYLE(1, lLastEvalfields(iPos).vcStyle);
            vcStyle:='$$' || TO_CHAR(i*10000) || TO_CHAR(iPos);
            lLastEvalfields(iPos).vcStyle:=vcStyle;
            lLastStyles(vcStyle).rStyle:=rStyle;
          END IF;
          -- takeover margin into absolute coordinates
          lLastEvalfields(iPos).nX:= lLastEvalfields(iPos).nX 
                                    +rPageSetup.nLeftMargin;
          lLastEvalfields(iPos).nY:= lLastEvalfields(iPos).nY
                                    -rPageSetup.nPageHeight
                                    +rPageSetup.nTopMargin;
          iPos:=lLastEvalfields.NEXT(iPos);
        END LOOP;
        iLastEvalPos:=NVL(lReportStack(1).lEvalLaterFields.LAST,0);
      END IF;  

      -- now, take report from stack
      PR_POP_SUBREPORT_DATA;
      -- cleanup
      IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
        PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Clean-up');
      END IF;

      PR_CLEAN_UP;
    END LOOP;


    
    -- now finish the pdf
    IF PK_JRXML2PDF_LOG.FK_IS_PROCESS THEN
      PK_JRXML2PDF_LOG.PR_LOG_PROCESS('Finish pdf');
    END IF;
    PR_FINISH_PDF;
    
    RETURN blPDF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RUN_TO_FILE(i_vcName     IN VARCHAR2,
                           i_lParams    IN tParamList,
                           i_vcDir      IN VARCHAR2,
                           i_vcFilename IN VARCHAR2) IS
    bl BLOB;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Start run');
    END IF;

    bl:=FK_RUN(i_vcName  =>i_vcName,
               i_lParams =>i_lParams
              );

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Save result to dir/file:' || i_vcDir || '/' || i_vcFilename);
    END IF;

    PR_SAVE_TO_FILE(i_blData     =>bl,
                    i_vcDir      =>i_vcDir,
                    i_vcFilename =>i_vcFilename
                   );

    DBMS_LOB.FREETEMPORARY(bl);

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Finish');
    END IF;

  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RUN_TO_FILE(i_lReports         IN PK_JRXML2PDF_TYPES.tReportNameList,
                           i_lParams          IN tParamList,
                           i_vcDir            IN VARCHAR2,
                           i_vcFilename       IN VARCHAR2,
                           i_bResetPageNumber IN BOOLEAN) IS
    bl BLOB;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Start run');
    END IF;

    bl:=FK_RUN(i_lReports        =>i_lReports,
               i_lParams         =>i_lParams,
               i_bResetPageNumber=>i_bResetPageNumber
              );

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Save result to dir/file:' || i_vcDir || '/' || i_vcFilename);
    END IF;

    PR_SAVE_TO_FILE(i_blData     =>bl,
                    i_vcDir      =>i_vcDir,
                    i_vcFilename =>i_vcFilename
                   );

    DBMS_LOB.FREETEMPORARY(bl);

    IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
      PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Finish');
    END IF;

  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SHOW_REPORT(io_blReport            IN OUT BLOB,
                           i_vcContentDisposition IN VARCHAR2 DEFAULT 'inline',
                           i_vcFilename           IN VARCHAR2 DEFAULT 'reportresult.pdf') IS
     l_mime        VARCHAR2 (255);
     l_length      NUMBER;
     l_file_name   VARCHAR2 (2000);
     lob_loc       BLOB;

  BEGIN
     OWA_UTIL.mime_header ('application/pdf', FALSE);
     HTP.p ('Content-Length: ' || DBMS_LOB.GETLENGTH(io_blReport));
     HTP.p ('Content-Disposition: ' || i_vcContentDisposition ||'; filename="' || i_vcFilename || '"');
     OWA_UTIL.http_header_close;
     WPG_DOCLOAD.download_file(io_blReport);

  END;

  -- ---------------------------------------------------------------------------

BEGIN
  -- Setup Tokens
  PR_ADD_TOKEN('"', '"');
  PR_ADD_TOKEN('+');
  PR_ADD_TOKEN('!','');
  PR_ADD_TOKEN('$F{', '}');
  PR_ADD_TOKEN('$V{', '}');
  PR_ADD_TOKEN('$P{', '}');
  PR_ADD_TOKEN('$R{', '}');
  PR_ADD_TOKEN('-');
  PR_ADD_TOKEN('==');
  PR_ADD_TOKEN('!=');
  PR_ADD_TOKEN('<');
  PR_ADD_TOKEN('<=');
  PR_ADD_TOKEN('>');
  PR_ADD_TOKEN('>=');
  PR_ADD_TOKEN('==null');
  PR_ADD_TOKEN('!=null');
  PR_ADD_TOKEN('||');
  PR_ADD_TOKEN('&&');
  PR_ADD_TOKEN('jrxml2pdf.Wrappers.mod');
  PR_ADD_TOKEN('new java.util.Date()', '');
  PR_ADD_TOKEN('.startsWith(', ')');

  lHtmlTokens(1).vcTag:=PK_JRXML2PDF_TYPES.HTML_H1;
  lHtmlTokens(1).bHasEndTag:=TRUE;
  lHtmlTokens(1).bHasAttribs:=FALSE;

  lHtmlTokens(2).vcTag:=PK_JRXML2PDF_TYPES.HTML_H2;
  lHtmlTokens(2).bHasEndTag:=TRUE;
  lHtmlTokens(2).bHasAttribs:=FALSE;

  lHtmlTokens(3).vcTag:=PK_JRXML2PDF_TYPES.HTML_H3;
  lHtmlTokens(3).bHasEndTag:=TRUE;
  lHtmlTokens(3).bHasAttribs:=FALSE;

  lHtmlTokens(4).vcTag:=PK_JRXML2PDF_TYPES.HTML_H4;
  lHtmlTokens(4).bHasEndTag:=TRUE;
  lHtmlTokens(4).bHasAttribs:=FALSE;

  lHtmlTokens(5).vcTag:=PK_JRXML2PDF_TYPES.HTML_PRE;
  lHtmlTokens(5).bHasEndTag:=TRUE;
  lHtmlTokens(5).bHasAttribs:=FALSE;


  lHtmlTokens(6).vcTag:=PK_JRXML2PDF_TYPES.HTML_UL;
  lHtmlTokens(6).bHasEndTag:=TRUE;
  lHtmlTokens(6).bHasAttribs:=FALSE;

  lHtmlTokens(7).vcTag:=PK_JRXML2PDF_TYPES.HTML_OL;
  lHtmlTokens(7).bHasEndTag:=TRUE;
  lHtmlTokens(7).bHasAttribs:=FALSE;

  lHtmlTokens(8).vcTag:=PK_JRXML2PDF_TYPES.HTML_LI;
  lHtmlTokens(8).bHasEndTag:=TRUE;
  lHtmlTokens(8).bHasAttribs:=FALSE;

  lHtmlTokens(9).vcTag:='span';
  lHtmlTokens(9).bHasEndTag:=TRUE;
  lHtmlTokens(9).bHasAttribs:=TRUE;

  lHtmlTokens(10).vcTag:=PK_JRXML2PDF_TYPES.HTML_P;
  lHtmlTokens(10).bHasEndTag:=TRUE;
  lHtmlTokens(10).bHasAttribs:=FALSE;

  lHtmlTokens(11).vcTag:=PK_JRXML2PDF_TYPES.HTML_BR;
  lHtmlTokens(11).bHasEndTag:=FALSE;
  lHtmlTokens(11).bHasAttribs:=FALSE;

  lHtmlTokens(12).vcTag:=PK_JRXML2PDF_TYPES.HTML_B;
  lHtmlTokens(12).bHasEndTag:=FALSE;
  lHtmlTokens(12).bHasAttribs:=FALSE;

  lHtmlTokens(13).vcTag:=PK_JRXML2PDF_TYPES.HTML_STRONG;
  lHtmlTokens(13).bHasEndTag:=FALSE;
  lHtmlTokens(13).bHasAttribs:=FALSE;

  -- Defaults for HTML
  rMasterHtmlSettings.vcFont            :=PK_JRXML2PDF_TYPES.ARIAL;
  rMasterHtmlSettings.nFontSize         :=12;
  rMasterHtmlSettings.nH1FontSize       :=24;
  rMasterHtmlSettings.nH2FontSize       :=18;
  rMasterHtmlSettings.nH3FontSize       :=14;
  rMasterHtmlSettings.nH4FontSize       :=12;
  rMasterHtmlSettings.nXXlargeFontSize  :=30;
  rMasterHtmlSettings.nXlargeFontSize   :=22;
  rMasterHtmlSettings.nLargeFontSize    :=17;
  rMasterHtmlSettings.nMediumFontSize   :=14;
  rMasterHtmlSettings.nSmallFontSize    :=12;
  rMasterHtmlSettings.nXsmallFontSize   :=10;
  rMasterHtmlSettings.nXXsmallFontSize  :=7;
  rMasterHtmlSettings.nLargerFontSize   :=12;
  rMasterHtmlSettings.nSmallerFontSize  :=10;

  rMasterHtmlSettings.nListIndent :=40;
  rMasterHtmlSettings.vcUlChar    :=CHR(108);

  rMasterHtmlSettings.nH1LineSpacing:=18;
  rMasterHtmlSettings.nH2LineSpacing:=16;
  rMasterHtmlSettings.nH3LineSpacing:=14;
  rMasterHtmlSettings.nH4LineSpacing:=12;
  rMasterHtmlSettings.nLiLineSpacing:=1;
  rMasterHtmlSettings.nBrLineSpacing:=12;
  rMasterHtmlSettings.nLineSpacing:=3;

  PR_ADD_HTML_REPLACEMENT('&quot;', UNISTR('\0022'));
  PR_ADD_HTML_REPLACEMENT('&Aacute;', UNISTR('\00C1'));
  PR_ADD_HTML_REPLACEMENT('&aacute;', UNISTR('\00E1'));
  PR_ADD_HTML_REPLACEMENT('&Acirc;', UNISTR('\00C2'));
  PR_ADD_HTML_REPLACEMENT('&acirc;', UNISTR('\00E2'));
  PR_ADD_HTML_REPLACEMENT('&acute;', UNISTR('\00B4'));
  PR_ADD_HTML_REPLACEMENT('&AElig;', UNISTR('\00C6'));
  PR_ADD_HTML_REPLACEMENT('&aelig;', UNISTR('\00E6'));
  PR_ADD_HTML_REPLACEMENT('&Agrave;', UNISTR('\00C0'));
  PR_ADD_HTML_REPLACEMENT('&agrave;', UNISTR('\00E0'));
  PR_ADD_HTML_REPLACEMENT('&Alpha;', UNISTR('\0391'));
  PR_ADD_HTML_REPLACEMENT('&alpha;', UNISTR('\03B1'));
  PR_ADD_HTML_REPLACEMENT('&amp;', UNISTR('\0026'));
  PR_ADD_HTML_REPLACEMENT('&and;', UNISTR('\2227'));
  PR_ADD_HTML_REPLACEMENT('&ang;', UNISTR('\2220'));
  PR_ADD_HTML_REPLACEMENT('&Aring;', UNISTR('\00C5'));
  PR_ADD_HTML_REPLACEMENT('&aring;', UNISTR('\00E5'));
  PR_ADD_HTML_REPLACEMENT('&asymp;', UNISTR('\2248'));
  PR_ADD_HTML_REPLACEMENT('&Atilde;', UNISTR('\00C3'));
  PR_ADD_HTML_REPLACEMENT('&atilde;', UNISTR('\00E3'));
  PR_ADD_HTML_REPLACEMENT('&Auml;', UNISTR('\00C4'));
  PR_ADD_HTML_REPLACEMENT('&auml;', UNISTR('\00E4'));
  PR_ADD_HTML_REPLACEMENT('&Beta;', UNISTR('\0392'));
  PR_ADD_HTML_REPLACEMENT('&beta;', UNISTR('\03B2'));
  PR_ADD_HTML_REPLACEMENT('&brvbar;', UNISTR('\00A6'));
  PR_ADD_HTML_REPLACEMENT('&cap;', UNISTR('\2229'));
  PR_ADD_HTML_REPLACEMENT('&Ccedil;', UNISTR('\00C7'));
  PR_ADD_HTML_REPLACEMENT('&ccedil;', UNISTR('\00E7'));
  PR_ADD_HTML_REPLACEMENT('&cedil;', UNISTR('\00B8'));
  PR_ADD_HTML_REPLACEMENT('&cent;', UNISTR('\00A2'));
  PR_ADD_HTML_REPLACEMENT('&Chi;', UNISTR('\03A7'));
  PR_ADD_HTML_REPLACEMENT('&chi;', UNISTR('\03C7'));
  PR_ADD_HTML_REPLACEMENT('&cong;', UNISTR('\2245'));
  PR_ADD_HTML_REPLACEMENT('&copy;', UNISTR('\00A9'));
  PR_ADD_HTML_REPLACEMENT('&cup;', UNISTR('\222A'));
  PR_ADD_HTML_REPLACEMENT('&curren;', UNISTR('\00A4'));
  PR_ADD_HTML_REPLACEMENT('&deg;', UNISTR('\00B0'));
  PR_ADD_HTML_REPLACEMENT('&Delta;', UNISTR('\0394'));
  PR_ADD_HTML_REPLACEMENT('&delta;', UNISTR('\03B4'));
  PR_ADD_HTML_REPLACEMENT('&divide;', UNISTR('\00F7'));
  PR_ADD_HTML_REPLACEMENT('&Eacute;', UNISTR('\00C9'));
  PR_ADD_HTML_REPLACEMENT('&eacute;', UNISTR('\00E9'));
  PR_ADD_HTML_REPLACEMENT('&Ecirc;', UNISTR('\00CA'));
  PR_ADD_HTML_REPLACEMENT('&ecirc;', UNISTR('\00EA'));
  PR_ADD_HTML_REPLACEMENT('&Egrave;', UNISTR('\00C8'));
  PR_ADD_HTML_REPLACEMENT('&egrave;', UNISTR('\00E8'));
  PR_ADD_HTML_REPLACEMENT('&empty;', UNISTR('\2205'));
  PR_ADD_HTML_REPLACEMENT('&Epsilon;', UNISTR('\0395'));
  PR_ADD_HTML_REPLACEMENT('&epsilon;', UNISTR('\03B5'));
  PR_ADD_HTML_REPLACEMENT('&equiv;', UNISTR('\2261'));
  PR_ADD_HTML_REPLACEMENT('&Eta;', UNISTR('\0397'));
  PR_ADD_HTML_REPLACEMENT('&eta;', UNISTR('\03B7'));
  PR_ADD_HTML_REPLACEMENT('&ETH;', UNISTR('\00D0'));
  PR_ADD_HTML_REPLACEMENT('&eth;', UNISTR('\00F0'));
  PR_ADD_HTML_REPLACEMENT('&Euml;', UNISTR('\00CB'));
  PR_ADD_HTML_REPLACEMENT('&euml;', UNISTR('\00EB'));
  PR_ADD_HTML_REPLACEMENT('&exist;', UNISTR('\2203'));
  PR_ADD_HTML_REPLACEMENT('&forall;', UNISTR('\2200'));
  PR_ADD_HTML_REPLACEMENT('&frac12;', UNISTR('\00BD'));
  PR_ADD_HTML_REPLACEMENT('&frac14;', UNISTR('\00BC'));
  PR_ADD_HTML_REPLACEMENT('&frac34;', UNISTR('\00BE'));
  PR_ADD_HTML_REPLACEMENT('&Gamma;', UNISTR('\0393'));
  PR_ADD_HTML_REPLACEMENT('&gamma;', UNISTR('\03B3'));
  PR_ADD_HTML_REPLACEMENT('&ge;', UNISTR('\2265'));
  PR_ADD_HTML_REPLACEMENT('&gt;', UNISTR('\003E'));
  PR_ADD_HTML_REPLACEMENT('&Iacute;', UNISTR('\00CD'));
  PR_ADD_HTML_REPLACEMENT('&iacute;', UNISTR('\00ED'));
  PR_ADD_HTML_REPLACEMENT('&Icirc;', UNISTR('\00CE'));
  PR_ADD_HTML_REPLACEMENT('&icirc;', UNISTR('\00EE'));
  PR_ADD_HTML_REPLACEMENT('&iexcl;', UNISTR('\00A1'));
  PR_ADD_HTML_REPLACEMENT('&Igrave;', UNISTR('\00CC'));
  PR_ADD_HTML_REPLACEMENT('&igrave;', UNISTR('\00EC'));
  PR_ADD_HTML_REPLACEMENT('&infin;', UNISTR('\221E'));
  PR_ADD_HTML_REPLACEMENT('&int;', UNISTR('\222B'));
  PR_ADD_HTML_REPLACEMENT('&Iota;', UNISTR('\0399'));
  PR_ADD_HTML_REPLACEMENT('&iota;', UNISTR('\03B9'));
  PR_ADD_HTML_REPLACEMENT('&iquest;', UNISTR('\00BF'));
  PR_ADD_HTML_REPLACEMENT('&isin;', UNISTR('\2208'));
  PR_ADD_HTML_REPLACEMENT('&Iuml;', UNISTR('\00CF'));
  PR_ADD_HTML_REPLACEMENT('&iuml;', UNISTR('\00EF'));
  PR_ADD_HTML_REPLACEMENT('&Kappa;', UNISTR('\039A'));
  PR_ADD_HTML_REPLACEMENT('&kappa;', UNISTR('\03BA'));
  PR_ADD_HTML_REPLACEMENT('&Lambda;', UNISTR('\039B'));
  PR_ADD_HTML_REPLACEMENT('&lambda;', UNISTR('\03BB'));
  PR_ADD_HTML_REPLACEMENT('&lang;', UNISTR('\2329'));
  PR_ADD_HTML_REPLACEMENT('&laquo;', UNISTR('\00AB'));
  PR_ADD_HTML_REPLACEMENT('&lceil;', UNISTR('\2308'));
  PR_ADD_HTML_REPLACEMENT('&le;', UNISTR('\2264'));
  PR_ADD_HTML_REPLACEMENT('&lfloor;', UNISTR('\230A'));
  PR_ADD_HTML_REPLACEMENT('&lowast;', UNISTR('\2217'));
  PR_ADD_HTML_REPLACEMENT('&loz;', UNISTR('\25CA'));
  PR_ADD_HTML_REPLACEMENT('&lt;', UNISTR('\003C'));
  PR_ADD_HTML_REPLACEMENT('&macr;', UNISTR('\00AF'));
  PR_ADD_HTML_REPLACEMENT('&micro;', UNISTR('\00B5'));
  PR_ADD_HTML_REPLACEMENT('&middot;', UNISTR('\00B7'));
  PR_ADD_HTML_REPLACEMENT('&minus;', UNISTR('\2212'));
  PR_ADD_HTML_REPLACEMENT('&Mu;', UNISTR('\039C'));
  PR_ADD_HTML_REPLACEMENT('&mu;', UNISTR('\03BC'));
  PR_ADD_HTML_REPLACEMENT('&nabla;', UNISTR('\2207'));
  PR_ADD_HTML_REPLACEMENT('&nbsp;', UNISTR('\00A0'));
  PR_ADD_HTML_REPLACEMENT('&ne;', UNISTR('\2260'));
  PR_ADD_HTML_REPLACEMENT('&ni;', UNISTR('\220B'));
  PR_ADD_HTML_REPLACEMENT('&not;', UNISTR('\00AC'));
  PR_ADD_HTML_REPLACEMENT('&notin;', UNISTR('\2209'));
  PR_ADD_HTML_REPLACEMENT('&nsub;', UNISTR('\2284'));
  PR_ADD_HTML_REPLACEMENT('&Ntilde;', UNISTR('\00D1'));
  PR_ADD_HTML_REPLACEMENT('&ntilde;', UNISTR('\00F1'));
  PR_ADD_HTML_REPLACEMENT('&Nu;', UNISTR('\039D'));
  PR_ADD_HTML_REPLACEMENT('&nu;', UNISTR('\03BD'));
  PR_ADD_HTML_REPLACEMENT('&Oacute;', UNISTR('\00D3'));
  PR_ADD_HTML_REPLACEMENT('&oacute;', UNISTR('\00F3'));
  PR_ADD_HTML_REPLACEMENT('&Ocirc;', UNISTR('\00D4'));
  PR_ADD_HTML_REPLACEMENT('&ocirc;', UNISTR('\00F4'));
  PR_ADD_HTML_REPLACEMENT('&Ograve;', UNISTR('\00D2'));
  PR_ADD_HTML_REPLACEMENT('&ograve;', UNISTR('\00F2'));
  PR_ADD_HTML_REPLACEMENT('&Omega;', UNISTR('\03A9'));
  PR_ADD_HTML_REPLACEMENT('&omega;', UNISTR('\03C9'));
  PR_ADD_HTML_REPLACEMENT('&Omicron;', UNISTR('\039F'));
  PR_ADD_HTML_REPLACEMENT('&omicron;', UNISTR('\03BF'));
  PR_ADD_HTML_REPLACEMENT('&oplus;', UNISTR('\2295'));
  PR_ADD_HTML_REPLACEMENT('&or;', UNISTR('\2228'));
  PR_ADD_HTML_REPLACEMENT('&ordf;', UNISTR('\00AA'));
  PR_ADD_HTML_REPLACEMENT('&ordm;', UNISTR('\00BA'));
  PR_ADD_HTML_REPLACEMENT('&Oslash;', UNISTR('\00D8'));
  PR_ADD_HTML_REPLACEMENT('&oslash;', UNISTR('\00F8'));
  PR_ADD_HTML_REPLACEMENT('&Otilde;', UNISTR('\00D5'));
  PR_ADD_HTML_REPLACEMENT('&otilde;', UNISTR('\00F5'));
  PR_ADD_HTML_REPLACEMENT('&otimes;', UNISTR('\2297'));
  PR_ADD_HTML_REPLACEMENT('&Ouml;', UNISTR('\00D6'));
  PR_ADD_HTML_REPLACEMENT('&ouml;', UNISTR('\00F6'));
  PR_ADD_HTML_REPLACEMENT('&para;', UNISTR('\00B6'));
  PR_ADD_HTML_REPLACEMENT('&part;', UNISTR('\2202'));
  PR_ADD_HTML_REPLACEMENT('&perp;', UNISTR('\22A5'));
  PR_ADD_HTML_REPLACEMENT('&Phi;', UNISTR('\03A6'));
  PR_ADD_HTML_REPLACEMENT('&phi;', UNISTR('\03C6'));
  PR_ADD_HTML_REPLACEMENT('&Pi;', UNISTR('\03A0'));
  PR_ADD_HTML_REPLACEMENT('&pi;', UNISTR('\03C0'));
  PR_ADD_HTML_REPLACEMENT('&piv;', UNISTR('\03D6'));
  PR_ADD_HTML_REPLACEMENT('&plusmn;', UNISTR('\00B1'));
  PR_ADD_HTML_REPLACEMENT('&pound;', UNISTR('\00A3'));
  PR_ADD_HTML_REPLACEMENT('&prod;', UNISTR('\220F'));
  PR_ADD_HTML_REPLACEMENT('&prop;', UNISTR('\221D'));
  PR_ADD_HTML_REPLACEMENT('&Psi;', UNISTR('\03A8'));
  PR_ADD_HTML_REPLACEMENT('&psi;', UNISTR('\03C8'));
  PR_ADD_HTML_REPLACEMENT('&radic;', UNISTR('\221A'));
  PR_ADD_HTML_REPLACEMENT('&rang;', UNISTR('\232A'));
  PR_ADD_HTML_REPLACEMENT('&raquo;', UNISTR('\00BB'));
  PR_ADD_HTML_REPLACEMENT('&rceil;', UNISTR('\2309'));
  PR_ADD_HTML_REPLACEMENT('&reg;', UNISTR('\00AE'));
  PR_ADD_HTML_REPLACEMENT('&rfloor;', UNISTR('\230B'));
  PR_ADD_HTML_REPLACEMENT('&Rho;', UNISTR('\03A1'));
  PR_ADD_HTML_REPLACEMENT('&rho;', UNISTR('\03C1'));
  PR_ADD_HTML_REPLACEMENT('&sdot;', UNISTR('\22C5'));
  PR_ADD_HTML_REPLACEMENT('&sect;', UNISTR('\00A7'));
  PR_ADD_HTML_REPLACEMENT('&shy;', UNISTR('\00AD'));
  PR_ADD_HTML_REPLACEMENT('&Sigma;', UNISTR('\03A3'));
  PR_ADD_HTML_REPLACEMENT('&sigma;', UNISTR('\03C3'));
  PR_ADD_HTML_REPLACEMENT('&sigmaf;', UNISTR('\03C2'));
  PR_ADD_HTML_REPLACEMENT('&sim;', UNISTR('\223C'));
  PR_ADD_HTML_REPLACEMENT('&sub;', UNISTR('\2282'));
  PR_ADD_HTML_REPLACEMENT('&sube;', UNISTR('\2286'));
  PR_ADD_HTML_REPLACEMENT('&sum;', UNISTR('\2211'));
  PR_ADD_HTML_REPLACEMENT('&sup;', UNISTR('\2283'));
  PR_ADD_HTML_REPLACEMENT('&sup1;', UNISTR('\00B9'));
  PR_ADD_HTML_REPLACEMENT('&sup2;', UNISTR('\00B2'));
  PR_ADD_HTML_REPLACEMENT('&sup3;', UNISTR('\00B3'));
  PR_ADD_HTML_REPLACEMENT('&supe;', UNISTR('\2287'));
  PR_ADD_HTML_REPLACEMENT('&szlig;', UNISTR('\00DF'));
  PR_ADD_HTML_REPLACEMENT('&Tau;', UNISTR('\03A4'));
  PR_ADD_HTML_REPLACEMENT('&tau;', UNISTR('\03C4'));
  PR_ADD_HTML_REPLACEMENT('&there4;', UNISTR('\2234'));
  PR_ADD_HTML_REPLACEMENT('&Theta;', UNISTR('\0398'));
  PR_ADD_HTML_REPLACEMENT('&theta;', UNISTR('\03B8'));
  PR_ADD_HTML_REPLACEMENT('&thetasym;', UNISTR('\03D1'));
  PR_ADD_HTML_REPLACEMENT('&THORN;', UNISTR('\00DE'));
  PR_ADD_HTML_REPLACEMENT('&thorn;', UNISTR('\00FE'));
  PR_ADD_HTML_REPLACEMENT('&times;', UNISTR('\00D7'));
  PR_ADD_HTML_REPLACEMENT('&Uacute;', UNISTR('\00DA'));
  PR_ADD_HTML_REPLACEMENT('&uacute;', UNISTR('\00FA'));
  PR_ADD_HTML_REPLACEMENT('&Ucirc;', UNISTR('\00DB'));
  PR_ADD_HTML_REPLACEMENT('&ucirc;', UNISTR('\00FB'));
  PR_ADD_HTML_REPLACEMENT('&Ugrave;', UNISTR('\00D9'));
  PR_ADD_HTML_REPLACEMENT('&ugrave;', UNISTR('\00F9'));
  PR_ADD_HTML_REPLACEMENT('&uml;', UNISTR('\00A8'));
  PR_ADD_HTML_REPLACEMENT('&upsih;', UNISTR('\03D2'));
  PR_ADD_HTML_REPLACEMENT('&Upsilon;', UNISTR('\03A5'));
  PR_ADD_HTML_REPLACEMENT('&upsilon;', UNISTR('\03C5'));
  PR_ADD_HTML_REPLACEMENT('&Uuml;', UNISTR('\00DC'));
  PR_ADD_HTML_REPLACEMENT('&uuml;', UNISTR('\00FC'));
  PR_ADD_HTML_REPLACEMENT('&Xi;', UNISTR('\039E'));
  PR_ADD_HTML_REPLACEMENT('&xi;', UNISTR('\03BE'));
  PR_ADD_HTML_REPLACEMENT('&Yacute;', UNISTR('\00DD'));
  PR_ADD_HTML_REPLACEMENT('&yacute;', UNISTR('\00FD'));
  PR_ADD_HTML_REPLACEMENT('&yen;', UNISTR('\00A5'));
  PR_ADD_HTML_REPLACEMENT('&yuml;', UNISTR('\00FF'));
  PR_ADD_HTML_REPLACEMENT('&Zeta;', UNISTR('\0396'));
  PR_ADD_HTML_REPLACEMENT('&zeta;', UNISTR('\03B6'));

END;
/
