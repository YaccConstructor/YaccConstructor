create or replace
PACKAGE PK_JRXML2PDF_LOG IS
/* *****************************************************************************

Copyright (C) 2012 by Andreas Weiden

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

  $name    PK_JRXML2PDF_LOG
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Helper-Package to generate logging-data.
           This packaged is used by PK_JRXML2PDF_REPGEN to log the process
           of generating a pdf. Depending on the given LEVEL more or less logging-
           information is generated.
           
           The delivered version uses simple DBMS_OUTPUT for logging. The package may be modified
           to hook in existing logging-frameworks.
           
           Start logging by calling PR_SET_LEVEL with the appropiate level of detail before running
           a report using PK_JRXML2PDF_REPGEN-methods
           
  $version 0.5.0.0 22.05.2012 Weiden
           initial version

  $version 0.6.0.0 31.05.2012 Weiden
           Added possibility to log to table JRXML_LOGGING
  $version 0.6.1.0 13.07.2012 Weiden
           Make PR_LOG an autonomous transaction
           
*/
  -- Constants for the possible logging-levels
  LEVEL_NONE     CONSTANT NUMBER:=0;
  LEVEL_OVERVIEW CONSTANT NUMBER:=1;
  LEVEL_PROCESS  CONSTANT NUMBER:=2;
  LEVEL_DETAIL   CONSTANT NUMBER:=3;
  LEVEL_FINE     CONSTANT NUMBER:=4;
  LEVEL_INTERNAL CONSTANT NUMBER:=5;
  -- constants for the possuble Log-modes
  LOGMODE_DBMS_OUTPUT CONSTANT NUMBER:=0;
  LOGMODE_TABLE       CONSTANT NUMBER:=1;

/**

  $name    PR_SET_LEVEL
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Set the logging-level to the requested level. The higher the level, the more
           details will apear in the logs. LEVEL_NONE stops the generation of loggings
           
           If i_nLogMode is set to LOGMODE_TABLE, all logings will be written to the table
           JRXML_LOGGING. When calling PR_SET_LEVEL, a unique JRL_RUN_ID is created and applied
           to all following Logs until the next call to PR_SET_LEVEL.
           
  $param   i_nLevel  level of detail, see constants in the package-spec

  $param   i_nLogMode method of logging, defines if logs are done by DBMS_OUTPUT or into the table JRXML_LOGGING,
                      see constants in the package-spec
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version

  $version 1.1.0.0 31.05.2012 Weiden
           Added parameter to set the log-method
           
*/
  PROCEDURE PR_SET_LEVEL(i_nLevel   IN NUMBER,
                         i_nLogMode IN NUMBER DEFAULT LOGMODE_DBMS_OUTPUT);
  
/**

  $name    FK_IS_OVERVIEW
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Check if logs with level OVERVIEW will be logged
           
  $return  TRUE when log-level is LEVEL_OVERVIEW
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  FUNCTION FK_IS_OVERVIEW
  RETURN BOOLEAN;

/**

  $name    FK_IS_PROCESS
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Check if logs with level PROCESS will be logged
           
  $return  TRUE when log-level is LEVEL_OVERVIEW, LEVEL_PROCESS
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  FUNCTION FK_IS_PROCESS
  RETURN BOOLEAN;

/**

  $name    FK_IS_DETAIL
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Check if logs with level DETAIL will be logged
           
  $return  TRUE when log-level is LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  FUNCTION FK_IS_DETAIL
  RETURN BOOLEAN;

/**

  $name    FK_IS_FINE
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Check if logs with level FINE will be logged
           
  $return  TRUE when log-level is LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL, LEVEL_FINE
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  FUNCTION FK_IS_FINE
  RETURN BOOLEAN;

/**

  $name    FK_IS_INTERNAL
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Check if logs with level INTERNAL will be logged
           
  $return  TRUE when log-level is LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL, LEVEL_FINE, LEVEL_INTERNAL
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  FUNCTION FK_IS_INTERNAL
  RETURN BOOLEAN;

/**

  $name    PR_LOG_OVERVIEW
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Logs the given text when log-level is one of
           LEVEL_OVERVIEW
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  PROCEDURE PR_LOG_OVERVIEW(i_vcText IN VARCHAR2);

/**

  $name    PR_LOG_PROCESS
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Logs the given text when log-level is one of
           LEVEL_OVERVIEW, LEVEL_PROCESS
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  PROCEDURE PR_LOG_PROCESS(i_vcText IN VARCHAR2);

/**

  $name    PR_LOG_DETAIL
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Logs the given text when log-level is one of
           LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  PROCEDURE PR_LOG_DETAIL(i_vcText IN VARCHAR2);

/**

  $name    PR_LOG_FINE
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Logs the given text when log-level is one of
           LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL, LEVEL_FINE
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  PROCEDURE PR_LOG_FINE(i_vcText IN VARCHAR2);

/**

  $name    PR_LOG_INTERNAL
  
  $created 22.05.2012
  
  $author  Andreas Weiden
  
  $desc    Logs the given text when log-level is one of
           LEVEL_OVERVIEW, LEVEL_PROCESS, LEVEL_DETAIL, LEVEL_FINE, LEVEL_INTERNAL
           
  $version 1.0.0.0 22.05.2012 Weiden
           initial version
           
*/
  PROCEDURE PR_LOG_INTERNAL(i_vcText IN VARCHAR2);


END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_LOG IS

  nLevel    NUMBER:=LEVEL_NONE;
  nLogMode  NUMBER:=LOGMODE_DBMS_OUTPUT;
  nRunid    NUMBER;
  tsLastTimeStamp TIMESTAMP;

  PROCEDURE PR_SET_LEVEL(i_nLevel   IN NUMBER,
                         i_nLogMode IN NUMBER DEFAULT LOGMODE_DBMS_OUTPUT) IS
    CURSOR crSeq IS
      SELECT JRXML_SEQ.NEXTVAL 
        FROM DUAL;
  BEGIN
    DBMS_OUTPUT.ENABLE(40000000);
    nLevel:=i_nLevel;
    nLogMode:=i_nLogMode;
    tsLastTimeStamp:=SYSTIMESTAMP;
    IF i_nLogMode=LOGMODE_TABLE THEN
      -- create a unique run-id
      OPEN crSeq;
      FETCh crSeq INTO nRunId;
      CLOSE crSeq;
    END IF;
  END;
  
  PROCEDURE PR_LOG(i_nLevel IN NUMBER, 
                   i_vcText IN VARCHAR2) IS
  PRAGMA AUTONOMOUS_TRANSACTION;
    tsNew TIMESTAMP;
    nMillis NUMBER;
  BEGIN
    IF nLevel>=i_nLevel THEN
      tsNew:=SYSTIMESTAMP;
      nMillis:=TRUNC(EXTRACT(HOUR         FROM tsNew-tsLastTimeStamp)*3600000)+
               TRUNC(EXTRACT(MINUTE       FROM tsNew-tsLastTimeStamp)*60000)+
               TRUNC(EXTRACT(SECOND       FROM tsNew-tsLastTimeStamp)*1000);
      IF nLogMode=LOGMODE_TABLE THEN
        INSERT INTO JRXML_LOGGING (
          JRL_ID,
          JRL_RUN_ID,
          JRL_TIME,
          JRL_TIME_SINCE_LAST_LOG,
          JRL_TEXT
        ) VALUES (
          JRXML_SEQ.NEXTVAL,
          nRunId,
          tsNew,
          nMillis,
          RPAD('| ', 2*(i_nLevel-1), '| ') || i_vcText
        );
      ELSE
        DBMS_OUTPUT.PUT_LINE(TO_CHAR(tsNew, 'HH24:MI:SS:FF') || '  ' || 
                             TO_CHAR(nMillis, '9999999990') ||'  ' || 
                             RPAD('| ', 2*(i_nLevel-1), '| ') || i_vcText);
      END IF;
      tsLastTimeStamp:=tsNew;
    END IF;
    COMMIT;
  END;
                   
  FUNCTION FK_IS_OVERVIEW
  RETURN BOOLEAN IS
  BEGIN
    RETURN nLevel>LEVEL_NONE;
  END;

  FUNCTION FK_IS_PROCESS
  RETURN BOOLEAN IS
  BEGIN
    RETURN nLevel>LEVEL_OVERVIEW;
  END;

  FUNCTION FK_IS_DETAIL
  RETURN BOOLEAN IS
  BEGIN
    RETURN nLevel>LEVEL_PROCESS;
  END;

  FUNCTION FK_IS_FINE
  RETURN BOOLEAN IS
  BEGIN
    RETURN nLevel>LEVEL_DETAIL;
  END;

  FUNCTION FK_IS_INTERNAL
  RETURN BOOLEAN IS
  BEGIN
    RETURN nLevel=LEVEL_INTERNAL;
  END;

  PROCEDURE PR_LOG_OVERVIEW(i_vcText IN VARCHAR2) IS
  BEGIN
    PR_LOG(LEVEL_OVERVIEW, i_vcText);
  END;

  PROCEDURE PR_LOG_PROCESS(i_vcText IN VARCHAR2) IS
  BEGIN
    PR_LOG(LEVEL_PROCESS, i_vcText);
  END;

  PROCEDURE PR_LOG_DETAIL(i_vcText IN VARCHAR2) IS
  BEGIN
    PR_LOG(LEVEL_DETAIL, i_vcText);
  END;
  

  PROCEDURE PR_LOG_FINE(i_vcText IN VARCHAR2) IS
  BEGIN
    PR_LOG(LEVEL_FINE, i_vcText);
  END;

  PROCEDURE PR_LOG_INTERNAL(i_vcText IN VARCHAR2) IS
  BEGIN
    PR_LOG(LEVEL_INTERNAL, i_vcText);
  END;
  

END;
/
