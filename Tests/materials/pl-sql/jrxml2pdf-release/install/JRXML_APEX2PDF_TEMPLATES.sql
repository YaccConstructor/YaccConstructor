--------------------------------------------------------
--  DDL for Table JRXML_APEX2JRXML_TEMPLATES
--------------------------------------------------------

  CREATE TABLE JRXML_APEX2JRXML_TEMPLATES
   (	JAT_ID NUMBER NOT NULL,
	JAT_NAME VARCHAR2(255) NOT NULL, 
	JAT_TYPE VARCHAR2(2) DEFAULT 'RE' NOT NULL, 
	JAT_TEMPLATE_START CLOB, 
	JAT_TEMPLATE_END CLOB, 
	JAT_HEIGHT_OFFSET NUMBER, 
	JAT_CONTENT_WIDTH NUMBER, 
	JAT_STANDARD VARCHAR2(1) DEFAULT 'N' NOT NULL
   );
   
--------------------------------------------------------
--  Constraints for Table JRXML_APEX2JRXML_TEMPLATES
--------------------------------------------------------

ALTER TABLE JRXML_APEX2JRXML_TEMPLATES ADD CONSTRAINT JAT_UK UNIQUE (JAT_TYPE, JAT_NAME);


ALTER TABLE JRXML_APEX2JRXML_TEMPLATES  ADD CONSTRAINT JAT_PK PRIMARY KEY (JAT_ID);


