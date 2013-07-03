create or replace
PACKAGE PK_JRXML2PDF_UTIL IS
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

  $name    PK_JRXML2PDF_UTIL

  $created 01.09.2012

  $author  Andreas Weiden

  $desc    Helper-Package for PL-jrxml2pdf with various functions around
           -NLS/locale
           -helpers

  $version 1.0.0.0 01.09.2012 Weiden
           initial version

  $version 1.0.0.1 29.09.2012 Weiden
           added FK_SPLIT

  $version 1.0.0.2 16.10.2012 Weiden
           moved types to PK_JRXML2PDF_TYPES

  $version 1.0.0.2 25.11.2012 Weiden
           included several procedures from PK_JRXML2PDF_REPGEN

  $version 1.0.0.3 08.12.2012 Weiden
           Bug-FIx in PR_SPLIT_TEXT_TO_BOX

  $version 1.0.0.4 13.12.2012 Weiden
           Bug-Fix in PR_SET_FONT: Read ebcoding from JRXML_FONTS,
           needed for unicode fonts (CID)

  $version 1.0.0.5 17.12.2012 Weiden
           Bug-Fix in PR_SPLIT_TEXT_TO_BOX

  $version 1.0.1.5 24.12.2012 Weiden
           Enhancement in PR_SET_FONT: Read flag for compression from JRXML_FONTS,
           needed for performance issues with large unicode fonts

  $version 1.0.2.0 31.03.2013 Weiden
           Added FK_JAVA_DATE_PATTERN

  $version 1.0.2.0 05.04.2013 Weiden
           Added FK_UPDATE_ADLER32_FOR_IMAGE
*/


/**

  $name    FK_GET_LOCALE_DATA

  $created 01.09.2012

  $author  Andreas Weiden

  $desc    checks the given locale and reads the associated NLS-data from JRXML_NLS_PARAMETERS

  $param   i_vcLocale Locale in java-notation (like en_US)
                      If no value is given, the current sessions NLS-settings are taken from NLS_SESSION_PARAMETERS

  $return  Record with locale-data

  $version 1.0.0.0 01.09.2012 Weiden
           initial version
*/
  FUNCTION FK_GET_LOCALE_DATA(i_vcLocale IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tLocaleData;

/**

  $name    FK_GET_RESOURCE

  $created 01.09.2012

  $author  Andreas Weiden

  $desc    reads all resource-entries for the givane name and locale
           If no matching resource is found, a record matching only the language is searched
           If still no matching recource is found, a record matching only the name is searched

  $param   i_vcRecourcename Name of the Resource-file

  $param   i_vcLocale Locale in java-notation (like en_US)

  $return  Array with ressource-entries, index by Key

  $version 1.0.0.0 01.09.2012 Weiden
           initial version
*/
  FUNCTION FK_GET_RESOURCE(i_vcResourceName IN VARCHAR2,
                           i_vcLocale       IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tResourceList;

/**

  $name    FK_ORA_DATE_PATTERN

  $created 01.09.2012

  $author  Andreas Weiden

  $desc    transforms the given Java-date-pattern into an Oracle-date-pattern
           currently no support for Timezone

  $param   i_vcPattern pattern in java-notation

  $return  Pattern in oracle-notation

  $version 1.0.0.0 01.09.2012 Weiden
           initial version
*/
  FUNCTION FK_ORA_DATE_PATTERN(i_vcPattern IN VARCHAR2)
  RETURN VARCHAR2;

/**

  $name    FK_TEXT_MESSAGE

  $created 01.09.2012

  $author  Andreas Weiden

  $desc    builds a textmessage- If the given text contains {0} {1} {2}, these placeholders
           are replaced by the given placeholder-values

  $param   i_vcText         Text containing placeholders

  $param   i_vcPlaceholder1 Text to put in placeholder {0}

  $param   i_vcPlaceholder2 Text to put in placeholder {1}

  $param   i_vcPlaceholder3 Text to put in placeholder {2}

  $return  Text for placeholder-substitutions

  $version 1.0.0.0 01.09.2012 Weiden
           initial version

  $version 1.0.0.1 28.10.2012 Weiden
           Changed so that you can pass {2} without {1}
*/
  FUNCTION FK_TEXT_MESSAGE(i_vcText         IN VARCHAR2,
                           i_vcPlaceholder1 IN VARCHAR2 DEFAULT NULL,
                           i_vcPlaceholder2 IN VARCHAR2 DEFAULT NULL,
                           i_vcPlaceholder3 IN VARCHAR2 DEFAULT NULL)
  RETURN VARCHAR2;

/**

  $name    FK_SPLIT

  $created 29.09.2012

  $author  Andreas Weiden

  $desc    cuts a piece of text out of the given text at the position of the given delimiter
           The original text is shortened by the cutoff text

  $param   io_vcText        Original text, returned cut off after the delimiter-position

  $param   i_vcDelimiter    Delimiter, defining the position to split

  $return  text before the delimiter-position

  $version 1.0.0.0 29.09.2012 Weiden
           initial version
*/
  FUNCTION FK_SPLIT(io_vcText     IN OUT NOCOPY VARCHAR2,
                    i_vcDelimiter IN     VARCHAR2 DEFAULT '|')
  RETURN VARCHAR2;

/**

  $name    PR_SET_FONT

  $created 08.11.2012

  $author  Andreas Weiden

  $desc    sets the given font, style and size as current font in AS_PDF3_MOD
           If the font is not present in the standard-fonts, the procedure
           tries to load the font from the custom-font-table and include it in the
           PDF.

  $param   i_vcFont         Name of the font to set

  $param   i_vcStyle        Fontstyle

  $param   i_nSize          Fontsize

  $version 1.0.0.0 08.11.2012 Weiden
           extracted from PK_JRXML2PDF_REPGEN
*/
  PROCEDURE PR_SET_FONT(i_vcFont  IN VARCHAR2,
                        i_vcStyle IN PK_JRXML2PDF_TYPES.tStyleName,
                        i_nSize   IN NUMBER);

/**

  $name    PR_INIT_FONTS

  $created 08.11.2012

  $author  Andreas Weiden

  $desc    Initializes the font-list and clears the custom font-list.

  $param   i_lParams List of parameter to check, if one of the standard-fonts should
                     not be mapped ($$EXTERNAL_FONT_ARIAL$$, $$EXTERNAL_FONT_TIMES$$, $$EXTERNAL_FONT_COURIER$$)
  $version 1.0.0.0 08.11.2012 Weiden
           extracted from PK_JRXML2PDF_REPGEN
           
  $version 1.0.0.1 06.04.2013
           added parameter lParams to filter if specific fonts should be treated
           as externaö
*/
  PROCEDURE PR_INIT_FONTS(i_lParams in PK_JRXML2PDF_TYPES.tParamList);

/**

  $name    PR_CLEAR_TEXT_CACHE

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    Clears the cache for text-fragments

  $version 1.0.0.0 25.11.2012 Weiden
           extracted from PK_JRXML2PDF_REPGEN
*/
  PROCEDURE PR_CLEAR_TEXT_CACHE;

/**

  $name    PR_GET_TEXT_PIECES

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    Splits a text into pieces depending on max-width and max height

  $param   io_lText          resulting array of text-pieces

  $param   io_nCurrentHeight starting height as input, resulting height as output

  $param   io_nX             Starting x on new lines, resulting x as output

  $param   i_vcText          text to split

  $param   i_nMaxHeight      maximum allowed height for the text, internal processing stops at that height

  $param   io_nGroup         Textgroup in case of HTML

  $param   i_nLineSpacing    inter-linespacing

  $param   i_nXStart         starting x in first row

  $param   i_nMaxWidth       maximum allowed text-width, used as split-border

  $param   i_bSplitByWord    Flag that for each a new textpiece should be created

  $version 1.0.0.0 25.11.2012 Weiden
           extracted from PK_JRXML2PDF_REPGEN

  $version 1.0.0.1 14.12.2012 Weiden
           added parameter i_bSplitByWord

  $version 1.0.0.2 17.12.2012 Weiden
           bugfix for SplitByWord: Added length of space to each piece
*/
  PROCEDURE PR_GET_TEXT_PIECES(io_lText          IN OUT NOCOPY PK_JRXML2PDF_TYPES.tTextPieceList,
                               io_nCurrentHeight IN OUT NOCOPY NUMBER,
                               io_nX             IN OUT NOCOPY NUMBER,
                               i_vcText          IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                               i_nMaxHeight      IN NUMBER,
                               io_nGroup         IN OUT NUMBER,
                               i_nLineSpacing    IN NUMBER,
                               i_nXStart         IN NUMBER,
                               i_nMaxWidth       IN NUMBER,
                               i_bSplitByWord    IN BOOLEAN DEFAULT FALSE
                              );

/**

  $name    PR_GET_TEXT_PIECES

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    calculates the height of the given text based on max-width and max height

  $param   i_vcText          text to split

  $param   i_nX              starting x

  $param   i_nMaxHeight      maximum allowed height for the text, internal processing stops at that height

  $param   i_nLineSpacing    inter-linespacing

  $param   i_nXStart         starting x in first row

  $param   i_nMaxWidth       maximum allowed text-width, used as split-border

  $return  resulting Testheight

  $version 1.0.0.0 25.11.2012 Weiden
           extracted from PK_JRXML2PDF_REPGEN
*/
  FUNCTION FK_CALC_BOX_HEIGHT(i_vcText          IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                              i_nX              IN NUMBER,
                              i_nMaxHeight      IN NUMBER,
                              i_nLineSpacing    IN NUMBER,
                              i_nXStart         IN NUMBER,
                              i_nMaxWidth       IN NUMBER
                             )
  RETURN NUMBER;

/**

  $name    FK_JAVA_DATE_PATTERN

  $created 31.03.2013

  $author  Andreas Weiden

  $desc    transforms the given oracle-date-pattern into a java-date-pattern
           currently no support for Timezone

  $param   i_vcPattern pattern in oracle-notation

  $return  Pattern in java-notation

  $version 1.0.0.0 31.03.2013 Weiden
           initial version
*/
  FUNCTION FK_JAVA_DATE_PATTERN(i_vcPattern IN VARCHAR2)
  RETURN VARCHAR2;

/**

  $name    FK_UPDATE_ADLER32_FOR_IMAGE

  $created 06.04.2013

  $author  Andreas Weiden

  $desc    Calculates the adler32 for the given image and update the JRXML_REPORT_IMAGES-table
           accordingly

  $param   i_nId Id of the image in JRXML_REPORT_IMAGES

  $param   i_blImage Blob with image-data

  $return  adler32-value for the image

  $version 1.0.0.0 06.04.2013 Weiden
           initial version
*/
  FUNCTION FK_UPDATE_ADLER32_FOR_IMAGE(i_nId     IN NUMBER,
                                       i_blImage IN BLOB)
  RETURN VARCHAR2;
  
END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_UTIL IS

  TYPE tDateSplitChars IS TABLE OF VARCHAR2(1) INDEX BY BINARY_INTEGER;

  lDateSplitChars     tDateSplitChars;

  lFontNames          PK_JRXML2PDF_TYPES.tFontNames;
  lCustomFonts        PK_JRXML2PDF_TYPES.tFontNames;
  lTextCache          PK_JRXML2PDF_TYPES.tTextCacheList;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_EXTRACT_RESOURCES_FROM_CLOB(i_nJrrId IN NUMBER) IS

    PRAGMA AUTONOMOUS_TRANSACTION;

    CURSOR crFile IS
      SELECT JRR_CONTENT
        FROM JRXML_RESOURCE_FILES
       WHERE JRR_ID=i_nJrrId;

    clData      CLOB;
    vcLine      VARCHAR2(32767);
    vcKey       VARCHAR2(4000);
    vcValue     VARCHAR2(4000);
    nLength     NUMBER;
    nOffset     NUMBER;
    nPartlength NUMBER;
  BEGIN
    -- Delete current resources
    DELETE JRXML_RESOURCE_ENTRIES
     WHERE JRS_JRR_ID=i_nJrrId;

    OPEN crFile;
    FETCH crFile INTO clData;
    CLOSE crFile;
    DBMS_LOB.OPEN(clData, DBMS_LOB.LOB_READONLY);
    nLength:=DBMS_LOB.GETLENGTH(clData);
    nOffset:=1;
    WHILE (nOffset < nLength AND nOffset>0) LOOP
      IF DBMS_LOB.INSTR(clData, CHR(10), nOffset)>0 THEN
        nPartlength:=DBMS_LOB.INSTR(clData, CHR(10), nOffset)-nOffset;
      ELSE
        nPartlength:=nLength-nOffset;
      END IF;
      vcLine:=DBMS_LOB.SUBSTR(clData, nPartLength, nOffset);
      IF vcLine IS NOT NULL THEN
        -- Split into Key and Value
        IF INSTR(vcLine, '=')>0 THEN
          vcKey:=SUBSTR(vcLine, 1, INSTR(vcLine, '=')-1);
          vcValue:=SUBSTR(vcLine, INSTR(vcLine, '=')+1);
          -- In the value, replace Unicode-encodings and linefeed
          IF INSTR(vcValue, '\n')>0 THEN
            vcValue:=REPLACE(vcValue, '\n', CHR(10));
          END IF;
          IF INSTR(vcValue, '\u')>0 THEN
            vcValue:=UNISTR(REPLACE(vcValue, '\u', '\'));
          END IF;
          INSERT INTO JRXML_RESOURCE_ENTRIES (
            JRS_ID,
            JRS_JRR_ID,
            JRS_KEY,
            JRS_VALUE
          ) VALUES (
            JRXML_SEQ.NEXTVAL,
            i_nJrrId,
            vcKey,
            vcValue
          );
        END IF;
      END IF;
      nOffset:=nOffset+nPartLength+1;
    END LOOP;
    DBMS_LOB.CLOSE(clData);
    -- set valid-flag
    UPDATE JRXML_RESOURCE_FILES SET
      JRR_VALID='Y'
     WHERE JRR_ID=i_nJrrId;

    COMMIT;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CHECK_LOCALE_DATA(i_vcResourceName IN VARCHAR2,
                                i_vcLocale       IN VARCHAR2
                               )
  RETURN NUMBER IS
    CURSOR crFile IS
      SELECT 1 SORT,
             JRR_ID,
             JRR_VALID
        FROM JRXML_RESOURCE_FILES
       WHERE JRR_NAME=i_vcResourceName
         AND JRR_LOCALE=i_vcLocale
      UNION ALL
      SELECT 2 SORT,
             JRR_ID,
             JRR_VALID
        FROM JRXML_RESOURCE_FILES
       WHERE JRR_NAME=i_vcResourceName
         AND JRR_LOCALE=SUBSTR(i_vcLocale, 1, INSTR(i_vcLocale, '_'))
      UNION ALL
      SELECT 3 SORT,
             JRR_ID,
             JRR_VALID
        FROM JRXML_RESOURCE_FILES
       WHERE JRR_NAME=i_vcResourceName
         AND JRR_LOCALE IS NULL
      ORDER BY 1;
    recFile crFile%ROWTYPE;
  BEGIN
    OPEN crFile;
    FETCH crFile INTO recFile;
    IF crFile%FOUND THEN
      IF recFile.JRR_VALID!='Y' THEN
        PR_EXTRACT_RESOURCES_FROM_CLOB(recFile.JRR_ID);
      END IF;
    END IF;
    CLOSE crFile;
    -- return ID for resource
    RETURN recFile.JRR_ID;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_LOCALE_DATA(i_vcLocale IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tLocaleData IS
    CURSOR crNls IS
      SELECT JRN_LOCALE_LANGUAGE || '_' || JRN_LOCALE_COUNTRY LOCALE,
             JRN_DEFAULT_DATE_FORMAT,
             JRN_SHORT_DATE_FORMAT,
             JRN_MEDIUM_DATE_FORMAT,
             JRN_LONG_DATE_FORMAT,
             JRN_DEFAULT_TIME_FORMAT,
             JRN_SHORT_TIME_FORMAT,
             JRN_MEDIUM_TIME_FORMAT,
             JRN_LONG_TIME_FORMAT,
             JRN_DATE_TIME_STRING,
             JRN_NUMERIC_CHARACTERS,
             JRN_CURRENCY,
             JRN_NLS_LANGUAGE
        FROM JRXML_NLS_PARAMETERS,
             (SELECT VALUE SESSION_LANGUAGE
                FROM NLS_SESSION_PARAMETERS
               WHERE PARAMETER='NLS_LANGUAGE'
             ),
             (SELECT VALUE SESSION_TERRITORY
                FROM NLS_SESSION_PARAMETERS
               WHERE PARAMETER='NLS_TERRITORY'
             )
       WHERE JRN_NLS_LANGUAGE=SESSION_LANGUAGE
         AND JRN_NLS_TERRITORY=SESSION_TERRITORY;

    CURSOR crLocale(i_vcLanguage  IN VARCHAR2,
                    i_vcTerritory IN VARCHAR2) IS
      SELECT 1 SORT,
             JRN_LOCALE_LANGUAGE || '_' || JRN_LOCALE_COUNTRY LOCALE,
             JRN_DEFAULT_DATE_FORMAT,
             JRN_SHORT_DATE_FORMAT,
             JRN_MEDIUM_DATE_FORMAT,
             JRN_LONG_DATE_FORMAT,
             JRN_DEFAULT_TIME_FORMAT,
             JRN_SHORT_TIME_FORMAT,
             JRN_MEDIUM_TIME_FORMAT,
             JRN_LONG_TIME_FORMAT,
             JRN_DATE_TIME_STRING,
             JRN_NUMERIC_CHARACTERS,
             JRN_CURRENCY,
             JRN_NLS_LANGUAGE
        FROM JRXML_NLS_PARAMETERS
       WHERE JRN_LOCALE_LANGUAGE=i_vcLanguage
         AND (   JRN_LOCALE_COUNTRY=i_vcTerritory
              OR (    JRN_LOCALE_COUNTRY IS NULL
                  AND i_vcTerritory IS NULL
                 )
             )
       UNION ALL
      SELECT 2 SORT,
             JRN_LOCALE_LANGUAGE || '_' || JRN_LOCALE_COUNTRY LOCALE,
             JRN_DEFAULT_DATE_FORMAT,
             JRN_SHORT_DATE_FORMAT,
             JRN_MEDIUM_DATE_FORMAT,
             JRN_LONG_DATE_FORMAT,
             JRN_DEFAULT_TIME_FORMAT,
             JRN_SHORT_TIME_FORMAT,
             JRN_MEDIUM_TIME_FORMAT,
             JRN_LONG_TIME_FORMAT,
             JRN_DATE_TIME_STRING,
             JRN_NUMERIC_CHARACTERS,
             JRN_CURRENCY,
             JRN_NLS_LANGUAGE
        FROM JRXML_NLS_PARAMETERS
       WHERE JRN_LOCALE_LANGUAGE=i_vcLanguage
       ORDER BY 1;


    rLocaleData PK_JRXML2PDF_TYPES.tLocaleData;
    nDummy      NUMBER;
  BEGIN
    IF i_vcLocale IS NULL THEN
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Load Resources from NLS-Parameters');
      END IF;
      -- get locale from NLS-settings
      OPEN crNls;
      FETCH crNls INTO rLocaleData.vcLocale,
                       rLocaleData.vcDateDefault,
                       rLocaleData.vcDateShort,
                       rLocaleData.vcDateMedium,
                       rLocaleData.vcDateLong,
                       rLocaleData.vcTimeDefault,
                       rLocaleData.vcTimeShort,
                       rLocaleData.vcTimeMedium,
                       rLocaleData.vcTimeLong,
                       rLocaleData.vcDateTimeString,
                       rLocaleData.vcNumericChars,
                       rLocaleData.vcCurrency,
                       rLocaleData.vcDateLanguage;
      CLOSE crNls;
    ELSE
      IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
        PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Load Resources from Locale ' || i_vcLocale);
      END IF;
      OPEN crLocale(SUBSTR(i_vcLocale, 1, INSTR(i_vcLocale, '_')-1),
                    SUBSTR(i_vcLocale, INSTR(i_vcLocale, '_')+1)
                   );
      FETCH crLocale INTO nDummy,
                          rLocaleData.vcLocale,
                          rLocaleData.vcDateDefault,
                          rLocaleData.vcDateShort,
                          rLocaleData.vcDateMedium,
                          rLocaleData.vcDateLong,
                          rLocaleData.vcTimeDefault,
                          rLocaleData.vcTimeShort,
                          rLocaleData.vcTimeMedium,
                          rLocaleData.vcTimeLong,
                          rLocaleData.vcDateTimeString,
                          rLocaleData.vcNumericChars,
                          rLocaleData.vcCurrency,
                          rLocaleData.vcDateLanguage;
      CLOSE crLocale;
    END IF;
    IF rLocaleData.vcLocale IS NULL THEN
      IF PK_JRXML2PDF_LOG.FK_IS_OVERVIEW THEN
        PK_JRXML2PDF_LOG.PR_LOG_OVERVIEW('Invalid locale ' || i_vcLocale);
      END IF;
      RAISE NO_DATA_FOUND;
    END IF;
    -- now convert the formats to oracle
    rLocaleData.vcDateDefault:=FK_ORA_DATE_PATTERN(rLocaleData.vcDateDefault);
    rLocaleData.vcDateShort  :=FK_ORA_DATE_PATTERN(rLocaleData.vcDateShort);
    rLocaleData.vcDateMedium :=FK_ORA_DATE_PATTERN(rLocaleData.vcDateMedium);
    rLocaleData.vcDateLong   :=FK_ORA_DATE_PATTERN(rLocaleData.vcDateLong);
    rLocaleData.vcTimeDefault:=FK_ORA_DATE_PATTERN(rLocaleData.vcTimeDefault);
    rLocaleData.vcTimeShort  :=FK_ORA_DATE_PATTERN(rLocaleData.vcTimeShort);
    rLocaleData.vcTimeMedium :=FK_ORA_DATE_PATTERN(rLocaleData.vcTimeMedium);
    rLocaleData.vcTimeLong   :=FK_ORA_DATE_PATTERN(rLocaleData.vcTimeLong);
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Locale-settings read');
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Locale       : ' || rLocaleData.vcLocale);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DateDefault  : ' || rLocaleData.vcDateDefault);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DateShort    : ' || rLocaleData.vcDateShort);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DateMedium   : ' || rLocaleData.vcDateMedium);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DateLong     : ' || rLocaleData.vcDateLong);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('TimeDefault  : ' || rLocaleData.vcTimeDefault);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('TimeShort    : ' || rLocaleData.vcTimeShort);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('TimeMedium   : ' || rLocaleData.vcTimeMedium);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Time Long    : ' || rLocaleData.vcTimeLong);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('DateTime     : ' || rLocaleData.vcDateTimeString);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Currency     : ' || rLocaleData.vcCurrency);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Num-Chars    : ' || rLocaleData.vcNumericChars);
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Date-Language: ' || rLocaleData.vcDateLanguage);
    END IF;
    rLocaleData.vcDateLanguage:=' NLS_DATE_LANGUAGE=''' || rLocaleData.vcDateLanguage || '''';
    rLocaleData.vcNumericChars:=' NLS_NUMERIC_CHARACTERS=''' || RPAD(rLocaleData.vcNumericChars, 2, ' ') || '''';
    rLocaleData.vcCurrency    :=' NLS_CURRENCY=''' || rLocaleData.vcCurrency || '''';
    RETURN rLocaleData;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_RESOURCE(i_vcResourceName IN VARCHAR2,
                           i_vcLocale       IN VARCHAR2)
  RETURN PK_JRXML2PDF_TYPES.tResourceList IS
    CURSOR crResources(i_nJrrId IN NUMBER) IS
      SELECT JRS_KEY,
             JRS_VALUE
       FROM JRXML_RESOURCE_ENTRIES
      WHERE JRS_JRR_ID=i_nJrrId;
    TYPE tRessources IS TABLE OF crResources%ROWTYPE;
    lResources tRessources;
    lReturn    PK_JRXML2PDF_TYPES.tResourceList;
    nJrrId     NUMBER;
  BEGIN
    nJrrId:=FK_CHECK_LOCALE_DATA(i_vcResourceName=>i_vcResourceName,
                                 i_vcLocale      =>i_vcLocale
                                );

    OPEN crResources(nJrrId);
    FETCH crResources
    BULK COLLECT INTO lResources;
    CLOSE crResources;
    FOR i IN 1..lResources.COUNT LOOP
      lReturn(lResources(i).JRS_KEY):=lResources(i).JRS_VALUE;
    END LOOP;
    RETURN lReturn;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_ORA_DATE_PATTERN(i_vcPattern IN VARCHAR2)
  RETURN VARCHAR2 IS
    iPos          PLS_INTEGER;
    iSplitPos     PLS_INTEGER;
    lSplitPos     tDateSplitChars;
    vcResult      VARCHAR2(4000);


    PROCEDURE PR_TOKENIZE IS
      iPos      PLS_INTEGER:=1;
      iFoundPos PLS_INTEGER;
      iToken    PLS_INTEGER;
    BEGIN
      LOOP
        EXIT WHEN iPos>LENGTH(i_vcPattern);
        iFoundPos:=99999;
        FOR i IN 1..lDateSplitChars.COUNT LOOP
          iToken:=INSTR(i_vcPattern, lDateSplitChars(i), iPos);
          IF iToken>0 THEN
            iFoundPos:=LEAST(iFoundPos, iToken);

            lSplitPos(iToken):=lDateSplitChars(i);
          END IF;
        END LOOP;
        iPos:=iFoundPos+1;
      END LOOP;
    END;

    FUNCTION FK_CONVERT_PATTERN(i_vcToConvert IN VARCHAR2)
    RETURN VARCHAR2 IS
      vcNewPattern VARCHAR2(20);
    BEGIN
      vcNewPattern:=CASE WHEN i_vcToConvert='GG' THEN
                      'AD'
                    WHEN i_vcToConvert='M' THEN
                      'MM'
                    WHEN i_vcToConvert='MM' THEN
                      'MM'
                    WHEN i_vcToConvert='MMM' THEN
                      'Mon'
                    WHEN i_vcToConvert='MMMM' THEN
                      'FMMonth'
                    WHEN i_vcToConvert='MMMMM' THEN
                      'FMMonth'
                    WHEN i_vcToConvert='d' THEN
                      'DD'
                    WHEN i_vcToConvert='dd' THEN
                      'DD'
                    WHEN i_vcToConvert='h' THEN
                      'HH'
                    WHEN i_vcToConvert='hh' THEN
                      'HH'
                    WHEN i_vcToConvert='HH' THEN
                      'HH24'
                    WHEN i_vcToConvert='m' THEN
                      'MI'
                    WHEN i_vcToConvert='mm' THEN
                      'MI'
                    WHEN i_vcToConvert='s' THEN
                      'SS'
                    WHEN i_vcToConvert='ss' THEN
                      'SS'
                    WHEN i_vcToConvert='S' THEN
                      NULL
                    WHEN i_vcToConvert='EEE' THEN
                      'Dy'
                    WHEN i_vcToConvert='EEEEE' THEN
                      'Day'
                    WHEN i_vcToConvert='D' THEN
                      'DDD'
                    WHEN i_vcToConvert='DD' THEN
                      'DDD'
                    WHEN i_vcToConvert='DDD' THEN
                      'DDD'
                    WHEN i_vcToConvert='w' THEN
                      'IW'
                    WHEN i_vcToConvert='a' THEN
                      'PM'
                    WHEN i_vcToConvert='aa' THEN
                      'PM'
                    WHEN i_vcToConvert='z' THEN
                      NULL
                    WHEN i_vcToConvert='zz' THEN
                      NULL
                    WHEN i_vcToConvert='zzz' THEN
                      NULL
                    WHEN i_vcToConvert='yy' THEN
                      'YY'
                    WHEN i_vcToConvert='yyyy' THEN
                      'YYYY'
                    END;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Convert ' || i_vcToConvert || ' to ' || vcNewPattern);
      END IF;

      RETURN vcNewPattern;
    END;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Scan date format ' || i_vcPattern);
    END IF;
    IF i_vcPattern IS NOT NULL THEN
      PR_TOKENIZE;
      iPos:=1;
      iSplitPos:=lSplitPos.FIRST;
      WHILE iSplitPos IS NOT NULL LOOP
        IF iSplitPos>iPos THEN
          vcResult:=vcResult || FK_CONVERT_PATTERN(SUBSTR(i_vcPattern, iPos, iSplitPos-iPos));
          iPos:=iSplitPos;
        END IF;
        IF lSplitPos(iPos)='''' THEN
          -- search end of fixed part
          iSplitPos:=lSplitPos.NEXT(iSplitPos);
          WHILE     iSplitPos IS NOT NULL
                AND lSplitPos(iSplitPos)!='''' LOOP
            iSplitPos:=lSplitPos.NEXT(iSplitPos);
          END LOOP;
          IF iSplitPos IS NOT NULL THEN
            vcResult:=vcResult || '"' || SUBSTR(i_vcPattern, iPos+1, iSplitPos-iPos-1) || '"';
          ELSE
            vcResult:=vcResult || '"' || SUBSTR(i_vcPattern, iPos+1) || '"';
          END IF;
          iPos:=iSplitPos+1;
        ELSE
          vcResult:=vcResult || lSplitPos(iSplitPos);
          iPos:=iSplitPos+1;
        END IF;
        iSplitPos:=lSplitPos.NEXT(iSplitPos);
      END LOOP;
      IF iPos <= LENGTH(i_vcPattern) THEN
        vcResult:=vcResult || FK_CONVERT_PATTERN(SUBSTR(i_vcPattern, iPos));
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Result date format ' || vcResult);
      END IF;
    END IF;
    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_JAVA_DATE_PATTERN(i_vcPattern IN VARCHAR2)
  RETURN VARCHAR2 IS
    iPos          PLS_INTEGER;
    iSplitPos     PLS_INTEGER;
    lSplitPos     tDateSplitChars;
    vcResult      VARCHAR2(4000);


    PROCEDURE PR_TOKENIZE IS
      iPos      PLS_INTEGER:=1;
      iFoundPos PLS_INTEGER;
      iToken    PLS_INTEGER;
    BEGIN
      LOOP
        EXIT WHEN iPos>LENGTH(i_vcPattern);
        iFoundPos:=99999;
        FOR i IN 1..lDateSplitChars.COUNT LOOP
          iToken:=INSTR(i_vcPattern, lDateSplitChars(i), iPos);
          IF iToken>0 THEN
            iFoundPos:=LEAST(iFoundPos, iToken);

            lSplitPos(iToken):=lDateSplitChars(i);
          END IF;
        END LOOP;
        iPos:=iFoundPos+1;
      END LOOP;
    END;

    FUNCTION FK_CONVERT_PATTERN(i_vcToConvert IN VARCHAR2)
    RETURN VARCHAR2 IS
      vcNewPattern VARCHAR2(20);
    BEGIN
      vcNewPattern:=CASE WHEN UPPER(i_vcToConvert)='AD' THEN
                      'GG'
                    WHEN UPPER(i_vcToConvert)='MM' THEN
                      'M'
                    WHEN UPPER(i_vcToConvert)='MM' THEN
                      'MM'
                    WHEN UPPER(i_vcToConvert)='MON' THEN
                      'MMM'
                    WHEN UPPER(i_vcToConvert)='FMMONTH' THEN
                      'MMMM'
                    WHEN UPPER(i_vcToConvert)='MONTH' THEN
                      'MMMM'
                    WHEN UPPER(i_vcToConvert)='DD' THEN
                      'dd'
                    WHEN UPPER(i_vcToConvert)='HH' THEN
                      'hh'
                    WHEN UPPER(i_vcToConvert)='HH24' THEN
                      'HH'
                    WHEN UPPER(i_vcToConvert)='MI' THEN
                      'mm'
                    WHEN UPPER(i_vcToConvert)='SS' THEN
                      'ss'
                    WHEN UPPER(i_vcToConvert)='DY' THEN
                      'EEE'
                    WHEN UPPER(i_vcToConvert)='DAY' THEN
                      'EEEEE'
                    WHEN UPPER(i_vcToConvert)='DDD' THEN
                      'DD'
                    WHEN UPPER(i_vcToConvert)='IW' THEN
                      'w'
                    WHEN UPPER(i_vcToConvert)='PM' THEN
                      'aa'
                    WHEN UPPER(i_vcToConvert)='YY' THEN
                      'yy'
                    WHEN UPPER(i_vcToConvert)='YYYY' THEN
                      'yyyy'
                    END;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Convert ' || i_vcToConvert || ' to ' || vcNewPattern);
      END IF;

      RETURN vcNewPattern;
    END;
  BEGIN
    IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
      PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Scan date format ' || i_vcPattern);
    END IF;
    IF i_vcPattern IS NOT NULL THEN
      PR_TOKENIZE;
      iPos:=1;
      iSplitPos:=lSplitPos.FIRST;
      WHILE iSplitPos IS NOT NULL LOOP
        IF iSplitPos>iPos THEN
          vcResult:=vcResult || FK_CONVERT_PATTERN(SUBSTR(i_vcPattern, iPos, iSplitPos-iPos));
          iPos:=iSplitPos;
        END IF;
        IF lSplitPos(iPos)='''' THEN
          -- search end of fixed part
          iSplitPos:=lSplitPos.NEXT(iSplitPos);
          WHILE     iSplitPos IS NOT NULL
                AND lSplitPos(iSplitPos)!='''' LOOP
            iSplitPos:=lSplitPos.NEXT(iSplitPos);
          END LOOP;
          IF iSplitPos IS NOT NULL THEN
            vcResult:=vcResult || '"' || SUBSTR(i_vcPattern, iPos+1, iSplitPos-iPos-1) || '"';
          ELSE
            vcResult:=vcResult || '"' || SUBSTR(i_vcPattern, iPos+1) || '"';
          END IF;
          iPos:=iSplitPos+1;
        ELSE
          vcResult:=vcResult || lSplitPos(iSplitPos);
          iPos:=iSplitPos+1;
        END IF;
        iSplitPos:=lSplitPos.NEXT(iSplitPos);
      END LOOP;
      IF iPos <= LENGTH(i_vcPattern) THEN
        vcResult:=vcResult || FK_CONVERT_PATTERN(SUBSTR(i_vcPattern, iPos));
      END IF;
      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Result date format ' || vcResult);
      END IF;
    END IF;
    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_TEXT_MESSAGE(i_vcText         IN VARCHAR2,
                           i_vcPlaceholder1 IN VARCHAR2 DEFAULT NULL,
                           i_vcPlaceholder2 IN VARCHAR2 DEFAULT NULL,
                           i_vcPlaceholder3 IN VARCHAR2 DEFAULT NULL)
  RETURN VARCHAR2 IS
    vcResult VARCHAR2(4000):=i_vcText;
  BEGIN
    IF INSTR(vcresult, '{0}')>0 THEN
      vcResult:=REPLACE(vcResult, '{0}', i_vcPlaceholder1);
    END IF;
    IF INSTR(vcresult, '{1}')>0 THEN
      vcResult:=REPLACE(vcResult, '{1}', i_vcPlaceholder2);
    END IF;
    IF INSTR(vcresult, '{2}')>0 THEN
      vcResult:=REPLACE(vcResult, '{2}', i_vcPlaceholder3);
    END IF;
    RETURN vcResult;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_SPLIT(io_vcText     IN OUT NOCOPY VARCHAR2,
                    i_vcDelimiter IN     VARCHAR2 DEFAULT '|')
  RETURN VARCHAR2 IS
    vcText VARCHAR2(2000);
  BEGIN
    IF INSTR(io_vcText, i_vcDelimiter) > 0 THEN
      vcText := SUBSTR(io_vcText, 1, INSTR(io_vcText, i_vcDelimiter)-1);
      io_vcText := SUBSTR(io_vcText, INSTR(io_vcText, i_vcDelimiter)+1);
    ELSE
      vcText := io_vcText;
      io_vcText := '';
    END IF;
    RETURN vcText;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SET_FONT(i_vcFont  IN VARCHAR2,
                        i_vcStyle IN PK_JRXML2PDF_TYPES.tStyleName,
                        i_nSize   IN NUMBER) IS
   CURSOR crFont(i_vcCustomFont IN VARCHAR2) IS
     SELECT JRF_NAME,
            JRF_TYPE,
            JRF_FONT,
            NVL(JRF_ENCODING, 'WINDOWS-1252') JRF_ENCODING,
            JRF_COMPRESS
       FROM JRXML_FONTS
      WHERE JRF_NAME=i_vcCustomFont;

    recFont crFont%ROWTYPE;

    vcFont           PK_JRXML2PDF_TYPES.tFont:=i_vcFont;
    nFontIndex       NUMBER;
    vcCustomFontName PK_JRXML2PDF_TYPES.tFont;

    FUNCTION FK_GET_CUSTOM_FONT(i_vcCustomFont IN VARCHAR2)
    RETURN NUMBER IS
      nReturn NUMBER;
    BEGIN
      IF NOT lCustomFonts.EXISTS(UPPER(i_vcCustomFont)) THEN
        IF PK_JRXML2PDF_LOG.FK_IS_DETAIL THEN
          PK_JRXML2PDF_LOG.PR_LOG_DETAIL('Load custom font ' || i_vcCustomFont);
        END IF;
        -- Load font from table
        OPEN crFont(i_vcCustomFont);
        FETCh crFont INTO recFont;
        IF crFont%FOUND THEN
          -- Add font to pdf
          IF recFont.JRF_TYPE='TTF' THEN
            -- only ttf-fonts are supported for now
            nReturn:=AS_PDF3_MOD.load_ttf_font(p_font     =>recFont.JRF_FONT,
                                               p_encoding =>recFont.JRF_ENCODING,
                                               p_embed    =>TRUE,
                                               p_compress =>(recFont.JRF_COMPRESS=PK_JRXML2PDF_TYPES.YES)
                                              );
          END IF;
          -- add to mapping-table
          lCustomFonts(UPPER(i_vcCustomFont)):=nReturn;
        END IF;
        CLOSE crFont;
      ELSE
        nReturn:=lCustomFonts(UPPER(i_vcCustomFont));
      END IF;
      RETURN nReturn;
    END;
  BEGIN
    -- default if no font defined
    IF TRIM(vcFont) IS NULL THEN
      vcFont:=PK_JRXML2PDF_TYPES.ARIAL;
    END IF;
    -- set font ans size
    IF lFontNames.EXISTS(UPPER(vcFont)) THEN
      -- standard-font, map to internal name
      vcFont:=lFontNames(UPPER(vcFont));

      IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
        PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Set font to ' || vcFont || '-' || NVL(i_vcStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL) || '-' ||NVL(i_nSize, 10));
      END IF;

      AS_PDF3_MOD.set_font(p_family     =>vcFont,
                           p_style      =>NVL(i_vcStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL),
                           p_fontsize_pt=>NVL(i_nSize, 10)
                          );
    ELSE
      -- custom font, build lookup-fontname
      vcCustomFontName:=vcFont || CASE WHEN i_vcStyle=PK_JRXML2PDF_TYPES.FONT_BOLD THEN
                                      '-Bold'
                                    WHEN i_vcStyle=PK_JRXML2PDF_TYPES.FONT_ITALIC THEN
                                      '-Italic'
                                    WHEN i_vcStyle=PK_JRXML2PDF_TYPES.FONT_BOLD_ITALIC THEN
                                      '-BoldItalic'
                                    END;
      -- get the font-index
      nFontIndex:=FK_GET_CUSTOM_FONT(vcCustomFontName);
      IF     nFontIndex IS NULL
         AND NVL(i_vcStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL)!=PK_JRXML2PDF_TYPES.FONT_NORMAL THEN
        -- font with given style not found,
        -- check for Normal font
        nFontIndex:=FK_GET_CUSTOM_FONT(vcFont);
      END IF;
      -- set font by index
      IF nFontIndex IS NOT NULL THEN
        AS_PDF3_MOD.set_font(p_index => nFontIndex,
                             p_fontsize_pt=>NVL(i_nSize, 10)
                            );
      ELSE
        -- Default to PK_JRXML2PDF_TYPES.HELVETICA
        AS_PDF3_MOD.set_font(p_family     =>PK_JRXML2PDF_TYPES.HELVETICA,
                             p_style      =>NVL(i_vcStyle, PK_JRXML2PDF_TYPES.FONT_NORMAL),
                             p_fontsize_pt=>NVL(i_nSize, 10)
                            );
      END IF;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_INIT_FONTS(i_lParams in PK_JRXML2PDF_TYPES.tParamList) IS
    bMapArial BOOLEAN:=TRUE;
    bMapTimes BOOLEAN:=TRUE;
    bMapCourier BOOLEAN:=TRUE;
  BEGIN
    -- check parameter for not mapping fon
    FOR i IN 1..i_lParams.COUNT LOOP
      IF     i_lParams(i).vcName='$$EXTERNAL_FONT_ARIAL$$'
         AND i_lParams(i).vcValue=PK_JRXML2PDF_TYPES.YES THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Arial font will not be mapped!');
        END IF;
        bMapArial:=FALSE;
      ELSIF    i_lParams(i).vcName='$$EXTERNAL_FONT_TIMES$$'
           AND i_lParams(i).vcValue=PK_JRXML2PDF_TYPES.YES THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Times font will not be mapped!');
        END IF;
        bMapTimes:=FALSE;
      ELSIF    i_lParams(i).vcName='$$EXTERNAL_FONT_COURIER$$'
           AND i_lParams(i).vcValue=PK_JRXML2PDF_TYPES.YES THEN
        IF PK_JRXML2PDF_LOG.FK_IS_INTERNAL THEN
          PK_JRXML2PDF_LOG.PR_LOG_INTERNAL('Courier font will not be mapped!');
        END IF;
        bMapCourier:=FALSE;
      END IF;
    END LOOP;
    -- Clear fonts
    lFontNames.DELETE;
    -- Init Fontmapping
    IF bMapArial THEN
      lFontNames('ARIAL'):='helvetica';
    END IF;
    IF bMapTimes THEN
      lFontNames('TIMES NEW ROMAN'):='times';
    END IF;
    IF bMapCourier THEN
      lFontNames('COURIER NEW'):='courier';
    END IF;
    lFontNames('WINGDINGS'):='zapfdingbats';
    -- Clear custom fonts
    lCustomFonts.DELETE;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_SPLIT_TEXT_TO_BOX(io_lText          IN OUT NOCOPY PK_JRXML2PDF_TYPES.tTextPieceList,
                                 io_nCurrentHeight IN OUT NOCOPY NUMBER,
                                 io_nX             IN OUT NOCOPY NUMBER,
                                 i_vcText          IN VARCHAR2,
                                 i_nMaxHeight      IN NUMBER,
                                 io_nGroup         IN OUT NUMBER,
                                 i_nLineSpacing    IN NUMBER,
                                 i_nXStart         IN NUMBER,
                                 i_nMaxWidth       IN NUMBER,
                                 i_bSplitByWord    IN BOOLEAN
                                ) IS
    nLen        NUMBER;
    iCnt        PLS_INTEGER;
    iIdx        PLS_INTEGER;
    nLineHeight NUMBER;
    nXTemp      NUMBER;
  begin
    IF i_vcText IS NULL THEN
      RETURN;
    END IF;

    nLineHeight:=AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);

    iIdx:=INSTR(i_vcText, CHR(10));
    IF iIdx>0 THEN

      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           i_vcText          =>RTRIM(SUBSTR(i_vcText, 1, iIdx-1), CHR(13)),
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nX             =>io_nX,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXstart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
      io_nGroup:=io_nGroup+1;
      io_nX:=i_nXStart;
      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           i_vcText          =>SUBSTR(i_vcText, iIdx+1),
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nX             =>io_nX,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXstart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
      RETURN;
    END IF;
    nLen:=AS_PDF3_MOD.str_len(i_vcText);
    IF nLen<=i_nMaxWidth-io_nX+i_nXStart THEN
      IF io_nCurrentHeight+nLineHeight>i_nMaxHeight THEN
        -- stop
        RETURN;
      ELSIF i_bSplitByWord THEN
        -- fits, but split in separate entries
        iIdx:=INSTR(i_vcText, ' ');
        IF iIdx>0 THEN
          PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                               io_nCurrentHeight =>io_nCurrentHeight,
                               i_vcText          =>SUBSTR(i_vcText, 1, iIdx-1),
                               i_nMaxHeight      =>i_nMaxHeight,
                               io_nX             =>io_nX,
                               io_nGroup         =>io_nGroup,
                               i_nLineSpacing    =>i_nLineSpacing,
                               i_nXstart         =>i_nXStart,
                               i_nMaxWidth       =>i_nMaxWidth,
                               i_bSplitByWord    =>i_bSplitByWord
                              );
          PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                               io_nCurrentHeight =>io_nCurrentHeight,
                               i_vcText          =>SUBSTR(i_vcText, iIdx+1),
                               i_nMaxHeight      =>i_nMaxHeight,
                               io_nX             =>io_nX,
                               io_nGroup         =>io_nGroup,
                               i_nLineSpacing    =>i_nLineSpacing,
                               i_nXstart         =>i_nXStart,
                               i_nMaxWidth       =>i_nMaxWidth,
                               i_bSplitByWord    =>i_bSplitByWord
                              );
          RETURN;
        ELSE
          nLen:=nLen+AS_PDF3_MOD.str_len(' ');
          -- put text-piece in array
          iIdx:=io_lText.COUNT+1;
          io_lText(iIdx).nX:=io_nX;
          io_lText(iIdx).nGroup:=io_nGroup;
          io_lText(iIdx).nHeight:=nLineHeight+i_nLineSpacing;
          -- if wordsplitting is activated, add length for one space
          io_lText(iIdx).nLength:=nLen;
          io_lText(iIdx).vcText:=i_vcText;
          io_nX:=io_nX+nLen;
          io_nCurrentHeight:=io_nCurrentHeight+nLineHeight+i_nLineSpacing;
          RETURN;
        END IF;
      ELSE
        -- put text-piece in array
        iIdx:=io_lText.COUNT+1;
        io_lText(iIdx).nX:=io_nX;
        io_lText(iIdx).nGroup:=io_nGroup;
        io_lText(iIdx).nHeight:=nLineHeight+i_nLineSpacing;
        io_lText(iIdx).nLength:=nLen;
        io_lText(iIdx).vcText:=i_vcText;
        io_nX:=io_nX+nLen;
        io_nCurrentHeight:=io_nCurrentHeight+nLineHeight+i_nLineSpacing;
        RETURN;
      END IF;
    END IF;
    -- Text does not fit in whole, split by spaces
    iCnt:=0;
    WHILE (   INSTR(i_vcText, ' ', 1, iCnt+1)>0
           AND AS_PDF3_MOD.str_len(SUBSTR(i_vcText, 1, INSTR(i_vcText, ' ', 1, iCnt+1)-1))<=i_nMaxWidth-io_nX+i_nXStart
          ) LOOP
      iCnt:=iCnt+1;
    END LOOP;
    IF iCnt>0 THEN
      iIdx:=INSTR(i_vcText, ' ', 1, iCnt);
      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           i_vcText          =>SUBSTR(i_vcText, 1, iIdx-1),
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nX             =>io_nX,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXstart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
      io_nGroup:=io_nGroup+1;
      io_nX:=i_nXStart;
      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           i_vcText          =>SUBSTR(i_vcText, iIdx+1),
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nX             =>io_nX,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXstart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
      RETURN;
    END IF;

    IF     io_nX>i_nXStart
       AND nLen <i_nMaxWidth THEN
      io_nGroup:=io_nGroup+1;
      io_nX:=i_nXStart;
      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           i_vcText          =>i_vcText,
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nX             =>io_nX,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXstart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
    ELSE
      IF LENGTH(i_vcText)=1 THEN
        IF io_nX>i_nXStart THEN
          io_nGroup:=io_nGroup+1;
          io_nX:=i_nXStart;
          PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                               io_nCurrentHeight =>io_nCurrentHeight,
                               i_vcText          =>i_vcText,
                               i_nMaxHeight      =>i_nMaxHeight,
                               io_nX             =>io_nX,
                               io_nGroup         =>io_nGroup,
                               i_nLineSpacing    =>i_nLineSpacing,
                               i_nXstart         =>i_nXStart,
                               i_nMaxWidth       =>i_nMaxWidth,
                               i_bSplitByWord    =>i_bSplitByWord
                              );
        ELSE
          -- 1 char fits always
          IF io_nCurrentHeight+nLineHeight>i_nMaxHeight THEN
            -- stop
            RETURN;
          ELSE
            -- put text-piece in array
            iIdx:=io_lText.COUNT+1;
            io_lText(iIdx).nX:=io_nX;
            io_lText(iIdx).nGroup:=io_nGroup;
            io_lText(iIdx).nHeight:=nLineHeight+i_nLineSpacing;
            io_lText(iIdx).nLength:=nLen;
            io_lText(iIdx).vcText:=i_vcText;
            io_nX:=io_nX+nLen;
            io_nCurrentHeight:=io_nCurrentHeight+nLineHeight+i_nLineSpacing;
            RETURN;
          END IF;
        END IF;
      ELSE
        iIdx:=2;
        WHILE AS_PDF3_MOD.str_len(SUBSTR(i_vcText, 1, iIdx))<=i_nMaxWidth-io_nX+i_nXStart LOOP
          iIdx:=iIdx+1;
        END LOOP;
        PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                             io_nCurrentHeight =>io_nCurrentHeight,
                             i_vcText          =>SUBSTR(i_vcText, 1, iIdx-1),
                             i_nMaxHeight      =>i_nMaxHeight,
                             io_nX             =>io_nX,
                             io_nGroup         =>io_nGroup,
                             i_nLineSpacing    =>i_nLineSpacing,
                             i_nXstart         =>i_nXStart,
                             i_nMaxWidth       =>i_nMaxWidth,
                             i_bSplitByWord    =>i_bSplitByWord
                            );
        io_nGroup:=io_nGroup+1;
        io_nX:=i_nXStart;
        PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                             io_nCurrentHeight =>io_nCurrentHeight,
                             i_vcText          =>SUBSTR(i_vcText, iIdx),
                             i_nMaxHeight      =>i_nMaxHeight,
                             io_nX             =>io_nX,
                             io_nGroup         =>io_nGroup,
                             i_nLineSpacing    =>i_nLineSpacing,
                             i_nXstart         =>i_nXStart,
                             i_nMaxWidth       =>i_nMaxWidth,
                             i_bSplitByWord    =>i_bSplitByWord
                            );
      END IF;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_GET_TEXT_PIECES(io_lText          IN OUT NOCOPY PK_JRXML2PDF_TYPES.tTextPieceList,
                               io_nCurrentHeight IN OUT NOCOPY NUMBER,
                               io_nX             IN OUT NOCOPY NUMBER,
                               i_vcText          IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                               i_nMaxHeight      IN NUMBER,
                               io_nGroup         IN OUT NUMBER,
                               i_nLineSpacing    IN NUMBER,
                               i_nXStart         IN NUMBER,
                               i_nMaxWidth       IN NUMBER,
                               i_bSplitByWord    IN BOOLEAN DEFAULT FALSE
                              ) IS
    vcIndex    PK_JRXML2PDF_TYPES.tMaxVarchar2:=TO_CHAR(AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)) || '^' ||
                                                TO_CHAR(i_nMaxHeight) || '^' ||
                                                TO_CHAR(io_nX) || '^' ||
                                                TO_CHAR(io_nGroup) || '^' ||
                                                i_vcText;
    rTextCache PK_JRXML2PDF_TYPES.tTextCacheEntry;
  BEGIN
    IF lTextCache.EXISTS(vcIndex) THEN
      rTextCache:=lTextCache(vcIndex);
      io_lText:=rTextCache.lText;
      io_nCurrentHeight:=rTextCache.nCurrentheight;
      io_nX:=rTextCache.nX;
      io_nGroup:=rTextCache.nGroup;
    ELSE
      PR_SPLIT_TEXT_TO_BOX(io_lText          =>io_lText,
                           io_nCurrentHeight =>io_nCurrentHeight,
                           io_nX             =>io_nX,
                           i_vcText          =>i_vcText,
                           i_nMaxHeight      =>i_nMaxHeight,
                           io_nGroup         =>io_nGroup,
                           i_nLineSpacing    =>i_nLineSpacing,
                           i_nXStart         =>i_nXStart,
                           i_nMaxWidth       =>i_nMaxWidth,
                           i_bSplitByWord    =>i_bSplitByWord
                          );
      rTextCache.lText:=io_lText;
      rTextCache.nCurrentheight:=io_nCurrentHeight;
      rTextCache.nX:=io_nX;
      rTextCache.nGroup:=io_nGroup;
      lTextCache(vcIndex):=rTextCache;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_BOX_HEIGHT(i_vcText          IN PK_JRXML2PDF_TYPES.tMaxVarchar2,
                              i_nX              IN NUMBER,
                              i_nMaxHeight      IN NUMBER,
                              i_nLineSpacing    IN NUMBER,
                              i_nXStart         IN NUMBER,
                              i_nMaxWidth       IN NUMBER
                             )
  RETURN NUMBER IS
    lText          PK_JRXML2PDF_TYPES.tTextPieceList;
    nX             NUMBER:=i_nX;
    nCurrentHeight NUMBER:=0;
    nGroup         NUMBER:=0;
    nLineSpacing   NUMBER;
  BEGIN
    nLineSpacing:=(NVL(i_nLineSpacing, PK_JRXML2PDF_TYPES.MIN_LINE_SPACING)-1)*AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);

    PR_GET_TEXT_PIECES(io_lText          =>lText,
                       io_nCurrentHeight =>nCurrentHeight,
                       io_nX             =>nX,
                       i_vcText          =>i_vcText,
                       i_nMaxHeight      =>i_nMaxheight,
                       io_nGroup         =>nGroup,
                       i_nLineSpacing    =>nLineSpacing,
                       i_nXStart         =>i_nXStart,
                       i_nMaxWidth       =>i_nMaxWidth
                      );

    RETURN nCurrentHeight;
  END;

  -- ---------------------------------------------------------------------------
  FUNCTION FK_UPDATE_ADLER32_FOR_IMAGE(i_nId     IN NUMBER,
                                       i_blImage IN BLOB)
  RETURN VARCHAR2 IS

    PRAGMA AUTONOMOUS_TRANSACTION;
    vcAdler32 PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    -- calc checksum
    vcAdler32:=AS_PDF3_MOD.adler32(i_blImage);
    -- update table
    UPDATE JRXML_REPORT_IMAGES SET
      JRI_ADLER32=vcAdler32,
      JRI_ADLER32_VALID='Y'
    WHERE JRI_ID=i_nId;
    -- COMMIT, is an autonomous transaction
    COMMIT;
    -- return adler
    RETURN vcAdler32;
  END;

  -- ---------------------------------------------------------------------------
  PROCEDURE PR_CLEAR_TEXT_CACHE IS
  BEGIN
    lTextcache.DELETE;
  END;

BEGIN
  -- Initialize Date-split-Chars
  lDateSplitChars(1):='/';
  lDateSplitChars(2):=' ';
  lDateSplitChars(3):=',';
  lDateSplitChars(4):='.';
  lDateSplitChars(5):=':';
  lDateSplitChars(6):='-';
  lDateSplitChars(7):='''';
  lDateSplitChars(8):='"';
END;
/
