--------------------------------------------------------
--  Datei erstellt -Dienstag-Mai-22-2012   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Table JRXML_REPORT_DEFINITIONS
--------------------------------------------------------

CREATE TABLE JRXML_REPORT_DEFINITIONS 
  (	JRD_ID NUMBER NOT NULL, 
    JRD_NAME VARCHAR2(255) NOT NULL, 
    JRD_DESCRIPTION VARCHAR2(4000), 
    JRD_XML CLOB
  );


--------------------------------------------------------
--  Constraints for Table JRXML_REPORT_DEFINITIONS
--------------------------------------------------------

ALTER TABLE JRXML_REPORT_DEFINITIONS ADD CONSTRAINT JRD_UK UNIQUE (JRD_NAME);


ALTER TABLE JRXML_REPORT_DEFINITIONS ADD CONSTRAINT JRD_PK PRIMARY KEY (JRD_ID);
