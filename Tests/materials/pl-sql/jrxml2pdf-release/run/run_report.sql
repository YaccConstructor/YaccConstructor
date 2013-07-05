DECLARE
  I_VCNAME VARCHAR2(200);
  I_LPARAMS PK_JRXML2PDF_REPGEN.TPARAMLIST;
  I_VCDIR VARCHAR2(200);
  I_VCFILENAME VARCHAR2(200);
BEGIN
  -- you may adjust the parameter-values accodring to the package-specification
  PK_JRXML2PDF_LOG.PR_SET_LEVEL(i_nLevel  =>PK_JRXML2PDF_LOG.LEVEL_OVERVIEW,
                                i_nLogMode=>PK_JRXML2PDF_LOG.LOGMODE_DBMS_OUTPUT);
  -- make sure you put the correct name in here
  I_VCNAME := 'images';
  -- Modify the code to initialize the variable
  -- I_LPARAMS := NULL;
  I_VCDIR := 'MY_DIR';
  I_VCFILENAME := 'my.pdf';

  PK_JRXML2PDF_REPGEN.PR_RUN_TO_FILE(
    I_VCNAME => I_VCNAME,
    I_LPARAMS => I_LPARAMS,
    I_VCDIR => I_VCDIR,
    I_VCFILENAME => I_VCFILENAME
  );
END;