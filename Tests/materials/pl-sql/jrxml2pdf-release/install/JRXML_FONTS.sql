--------------------------------------------------------
--  Datei erstellt -Dienstag-Mai-22-2012   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Table JRXML_FONTS
--------------------------------------------------------

CREATE TABLE JRXML_FONTS 
  (	JRF_ID NUMBER NOT NULL, 
	JRF_NAME VARCHAR2(80) NOT NULL, 
    JRF_TYPE VARCHAR2(3) NOT NULL, 
	JRF_COMPRESS VARCHAR2(1) DEFAULT 'Y' NOT NULL,
	JRF_ENCODING VARCHAR2(30),
	JRF_FONT BLOB
  );


--------------------------------------------------------
--  Constraints for Table JRXML_FONTS
--------------------------------------------------------

ALTER TABLE JRXML_FONTS ADD CONSTRAINT JRF_TYPE_CK CHECK (JRF_TYPE IN ('TTF', 'TTC')) ENABLE;


ALTER TABLE JRXML_FONTS ADD CONSTRAINT JRF_UK UNIQUE (JRF_NAME);


ALTER TABLE JRXML_FONTS ADD CONSTRAINT JRF_PK PRIMARY KEY (JRF_ID);


