--------------------------------------------------------
--  DDL for Table JRXML_RESOURCE_ENTRIES
--------------------------------------------------------

CREATE TABLE JRXML_RESOURCE_ENTRIES
  (	JRS_ID      NUMBER         NOT NULL, 
	JRS_JRR_ID  NUMBER         NOT NULL,
	JRS_KEY     VARCHAR2(2000) NOT NULL, 
    JRS_VALUE   VARCHAR2(4000)
  );

--------------------------------------------------------
--  Constraints for Table JRXML_RESOURCE_ENTRIES
--------------------------------------------------------

ALTER TABLE JRXML_RESOURCE_ENTRIES ADD CONSTRAINT JRS_UK UNIQUE (JRS_JRR_ID, JRS_KEY);

ALTER TABLE JRXML_RESOURCE_ENTRIES ADD CONSTRAINT JRS_PK PRIMARY KEY (JRS_ID);

--------------------------------------------------------
--  Ref Constraints for Table JRXML_RESOURCE_ENTRIES
--------------------------------------------------------

ALTER TABLE JRXML_RESOURCE_ENTRIES ADD CONSTRAINT JRS_JRR_FK FOREIGN KEY (JRS_JRR_ID)
REFERENCES JRXML_RESOURCE_FILES (JRR_ID) ENABLE;


