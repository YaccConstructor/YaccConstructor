--------------------------------------------------------
--  Datei erstellt -Dienstag-Mai-22-2012   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Table JRXML_REPORT_IMAGES
--------------------------------------------------------

CREATE TABLE JRXML_REPORT_IMAGES
  (	JRI_ID NUMBER NOT NULL, 
   	JRI_JRD_ID NUMBER, 
    JRI_NAME VARCHAR2(255) NOT NULL, 
    JRI_DESCRIPTION VARCHAR2(4000), 
   	JRI_IMAGE BLOB,
	JRI_ADLER32 VARCHAR2(30),
	JRI_ADLER32_VALID  VARCHAR2(1)  DEFAULT 'N' NOT NULL
  );

--------------------------------------------------------
--  Constraints for Table JRXML_REPORT_IMAGES
--------------------------------------------------------

ALTER TABLE JRXML_REPORT_IMAGES ADD CONSTRAINT JRI_UK UNIQUE (JRI_JRD_ID, JRI_NAME);


ALTER TABLE JRXML_REPORT_IMAGES ADD CONSTRAINT JRI_PK PRIMARY KEY (JRI_ID);


--------------------------------------------------------
--  Ref Constraints for Table JRXML_REPORT_IMAGES
--------------------------------------------------------

ALTER TABLE JRXML_REPORT_IMAGES ADD CONSTRAINT JRI_JRD_FK FOREIGN KEY (JRI_JRD_ID)
REFERENCES JRXML_REPORT_DEFINITIONS (JRD_ID) ENABLE;

ALTER TABLE JRXML_REPORT_IMAGES ADD CONSTRAINT JRI_ALER32_CK CHECK (JRI_ADLER32_VALID IN ('Y', 'N')) ENABLE;

CREATE OR REPLACE TRIGGER TRG_JRI_BRUI 
BEFORE INSERT OR UPDATE ON JRXML_REPORT_IMAGES 
FOR EACH ROW 
BEGIN
  IF INSERTING THEN 
    :NEW.JRI_ADLER32_VALID:='N';
  ELSE
    IF     :OLD.JRI_ADLER32_VALID='Y'
       AND :NEW.JRI_ADLER32_VALID='Y' THEN
      :NEW.JRI_ADLER32_VALID:='N';
    END IF;
  END IF;
END;
/