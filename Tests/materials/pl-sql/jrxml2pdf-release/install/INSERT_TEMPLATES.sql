declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<summary>' || unistr('\000a') || 
                    '		<band height="28">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="Summary" x="0" y="0" width="802" height="27"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[This is the summary for report #REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</summary>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Lanscape summary',
    'RS',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<title>' || unistr('\000a') || 
                    '		<band height="28">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="Title" x="0" y="0" width="802" height="27"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[This is the title for report #REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape Title',
    'RT',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '             <frame>' || unistr('\000a') || 
                    '               <reportElement x="#XPOSITION#" y="#YPOSITION#" width="#WIDTH#" height="20"/> ' || unistr('\000a') || 
                    '               <staticText>' || unistr('\000a') || 
                    '                 <reportElement style="FormRegiontitle" x="0" y="0" width="801" height="20"/>' || unistr('\000a') || 
                    '                 <textElement/>' || unistr('\000a') || 
                    '                 <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '               </staticText>' || unistr('\000a') || 
                    '             </frame>' || unistr('\000a') || 
                    '             <componentElement> ' || unistr('\000a') || 
                    '               <reportElement x="0" y="20" width="802" height="48"/>' || unistr('\000a') || 
                    '               <jr:table';

  clstart := clstart || ' xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd"> ' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '               </jr:table>' || unistr('\000a') || 
                    '             </componentElement>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape ',
    'TR',
    71,
    802,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '                       <jr:detailCell height="20">' || unistr('\000a') || 
                    '                            <textField pattern="#PATTERN#">' || unistr('\000a') || 
                    '                                <reportElement style="TabularItem" x="0" y="0" width="#COLUMNWIDTH#" height="20"/>' || unistr('\000a') || 
                    '                                <textElement textAlignment="#ALIGNMENT#"/>' || unistr('\000a') || 
                    '                                <textFieldExpression><![CDATA[$F{#FIELD#}]]></textFieldExpression>' || unistr('\000a') || 
                    '                            </textField>' || unistr('\000a') || 
                    '                        </jr:detailCell>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'Default',
    'TI',
    20,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '                    <staticText>' || unistr('\000a') || 
                    '                        <reportElement style="FormItemLabel" x="#XPOSITION#" y="#YPOSITION#" width="#WIDTH#" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                        <textElement/>' || unistr('\000a') || 
                    '                        <text><![CDATA[#LABEL#]]></text>' || unistr('\000a') || 
                    '                    </staticText>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'Standard',
    'FL',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '                    <textField pattern="#PATTERN#">' || unistr('\000a') || 
                    '                        <reportElement style="FormItem" x="#XPOSITION#" y="#YPOSITION#" width="#WIDTH#" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                        <textElement/>' || unistr('\000a') || 
                    '                        <textFieldExpression><![CDATA[#FIELD#]]></textFieldExpression>' || unistr('\000a') || 
                    '                    </textField>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'Standard',
    'FI',
    20,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '            <frame>' || unistr('\000a') || 
                    '                <reportElement x="#XPOSITION#" y="0" width="555" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                <staticText>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegiontitle" x="0" y="0" width="555" height="20"/>' || unistr('\000a') || 
                    '                    <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '                </staticText>' || unistr('\000a') || 
                    '                <frame>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegionFrame" x="0" y="20" width="555" height="#INNERHEIGHT#"/>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '                </frame>' || unistr('\000a') || 
                    '            </frame>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait',
    'FR',
    23,
    555,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '            <frame>' || unistr('\000a') || 
                    '                <reportElement x="#XPOSITION#" y="0" width="555" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                <staticText>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegiontitle" x="0" y="0" width="802" height="20"/>' || unistr('\000a') || 
                    '                    <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '                </staticText>' || unistr('\000a') || 
                    '                <frame>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegionFrame" x="0" y="20" width="802" height="#INNERHEIGHT#"/>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '                </frame>' || unistr('\000a') || 
                    '            </frame>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape',
    'FR',
    23,
    802,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<pageFooter>' || unistr('\000a') || 
                    '		<band height="21" splitType="Stretch">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement style="Pagefooter" x="0" y="1" width="555" height="20"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement x="409" y="0" width="100" height="18"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA["Page " + $V{PAGE_NUMBER}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField evaluationTime="Report">' || unistr('\000a') || 
                    '				<reportElement x="509" y="0" widt';

  clstart := clstart || 'h="46" height="18"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[" of " + $V{PAGE_NUMBER}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait page numbering right',
    'RF',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<title>' || unistr('\000a') || 
                    '		<band height="28">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="Title" x="0" y="0" width="555" height="27"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[This is the title for report #REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait Title',
    'RT',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '			<image>' || unistr('\000a') || 
                    '				<reportElement x="#XPOSITION#" y="#YPOSITION#" width="50" height="50"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<leftPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<rightPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<imageExpression><![CDATA[#FIELD#]]></imageExpression>' || unistr('\000a') || 
                    '			</image>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'default image',
    'IT',
    50,
    50,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '			<image>' || unistr('\000a') || 
                    '				<reportElement x="#XPOSITION#" y="#YPOSITION#" width="50" height="50"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<leftPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<rightPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<imageExpression><![CDATA[#FIELD#]]></imageExpression>' || unistr('\000a') || 
                    '			</image>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'default image',
    'IF',
    50,
    50,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '          <frame>' || unistr('\000a') || 
                    '                <reportElement x="#XPOSITION#" y="0" width="275" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                <staticText>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegiontitle" x="0" y="0" width="275" height="20"/>' || unistr('\000a') || 
                    '                    <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '                </staticText>' || unistr('\000a') || 
                    '                <frame>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegionFrame" x="0" y="20" width="275" height="#INNERHEIGHT#"/>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '                </frame>' || unistr('\000a') || 
                    '            </frame>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait 2 column',
    'FR',
    23,
    277,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<pageFooter>' || unistr('\000a') || 
                    '		<band height="21" splitType="Stretch">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement style="Pagefooter" x="0" y="1" width="802" height="20"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement x="657" y="0" width="100" height="18"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA["Page " + $V{PAGE_NUMBER}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField evaluationTime="Report">' || unistr('\000a') || 
                    '				<reportElement x="756" y="0" widt';

  clstart := clstart || 'h="46" height="18"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[" of " + $V{PAGE_NUMBER}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape page numbering right',
    'RF',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="test" language="groovy" pageWidth="595" pageHeight="842" columnWidth="545" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ire';

  clstart := clstart || 'port.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Title" mode="Opaque" forecolor="#000000" backcolor="#CCCCCC" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Summary" mode="Opaque" forecolor="#000000" backcolor="#CCCCCC" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pageheader" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" fo';

  clstart := clstart || 'ntSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pagefooter" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormRegiontitle" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" fontSize="13" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<le';

  clstart := clstart || 'ftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormRegionFrame" mode="Opaque" forecolor="#000000" backcolor="#FAFAFA">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormItemLabel" forecolor="#000000" hAlign="Left" fontSize="10" isBold="false" isItalic="false" isUnderline="f';

  clstart := clstart || 'alse" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="TabularItemHeader" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" hAlign="Left" fontSize="10" isBold="false" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		';

  clstart := clstart || '	<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="TabularItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '		<conditionalStyle>' || unistr('\000a') || 
                    '			<conditionExpression><![CDATA[jrxml2pdf.Wrappers.mod($V{REPORT_COUNT},2) == 0]]></conditionExpression>' || unistr('\000a') || 
                    '		';

  clstart := clstart || '	<style mode="Opaque" backcolor="#FAFAFA"/>' || unistr('\000a') || 
                    '		</conditionalStyle>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '    #SUBDATASETS#' || unistr('\000a') || 
                    '    #PARAMETERS#' || unistr('\000a') || 
                    '    #MAINQUERY#' || unistr('\000a') || 
                    '    #BACKGROUNDREGION#' || unistr('\000a') || 
                    '    #TITLEREGION#' || unistr('\000a') || 
                    '    #PAGEHEADERREGION#' || unistr('\000a') || 
                    '    #DETAIL#' || unistr('\000a') || 
                    '    #PAGEFOOTERREGION#' || unistr('\000a') || 
                    '    #SUMMARYREGION#' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Grey Portrait',
    'RE',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '             <frame>' || unistr('\000a') || 
                    '               <reportElement x="#XPOSITION#" y="#YPOSITION#" width="#WIDTH#" height="20"/> ' || unistr('\000a') || 
                    '               <staticText>' || unistr('\000a') || 
                    '                 <reportElement style="FormRegiontitle" x="0" y="0" width="554" height="20"/>' || unistr('\000a') || 
                    '                 <textElement/>' || unistr('\000a') || 
                    '                 <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '               </staticText>' || unistr('\000a') || 
                    '             </frame>' || unistr('\000a') || 
                    '             <componentElement> ' || unistr('\000a') || 
                    '               <reportElement x="0" y="20" width="555" height="48"/>' || unistr('\000a') || 
                    '               <jr:table';

  clstart := clstart || ' xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd"> ' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '               </jr:table>' || unistr('\000a') || 
                    '             </componentElement>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait',
    'TR',
    71,
    555,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '<jr:column width="#COLUMNWIDTH#">';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '</jr:column>';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'Standard',
    'TC',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="test" language="groovy" pageWidth="595" pageHeight="842" columnWidth="545" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ire';

  clstart := clstart || 'port.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Title" mode="Opaque" forecolor="#000000" backcolor="#CCCCFF" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Summary" mode="Opaque" forecolor="#000000" backcolor="#CCCCFF" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pageheader" mode="Opaque" forecolor="#000000" backcolor="#EEEEFF" fo';

  clstart := clstart || 'ntSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pagefooter" mode="Opaque" forecolor="#000000" backcolor="#EEEEFF" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormRegiontitle" mode="Opaque" forecolor="#000000" backcolor="#EEEEFF" fontSize="13" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<le';

  clstart := clstart || 'ftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormRegionFrame" mode="Opaque" forecolor="#000000" backcolor="#FAFAFF">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormItemLabel" forecolor="#000000" hAlign="Left" fontSize="10" isBold="false" isItalic="false" isUnderline="f';

  clstart := clstart || 'alse" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="TabularItemHeader" mode="Opaque" forecolor="#000000" backcolor="#EEEEFF" hAlign="Left" fontSize="10" isBold="false" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		';

  clstart := clstart || '	<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="TabularItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '		<conditionalStyle>' || unistr('\000a') || 
                    '			<conditionExpression><![CDATA[jrxml2pdf.Wrappers.mod($V{REPORT_COUNT},2) == 0]]></conditionExpression>' || unistr('\000a') || 
                    '		';

  clstart := clstart || '	<style mode="Opaque" backcolor="#FAFAFF"/>' || unistr('\000a') || 
                    '		</conditionalStyle>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '    #SUBDATASETS#' || unistr('\000a') || 
                    '    #PARAMETERS#' || unistr('\000a') || 
                    '    #MAINQUERY#' || unistr('\000a') || 
                    '    #BACKGROUNDREGION#' || unistr('\000a') || 
                    '    #TITLEREGION#' || unistr('\000a') || 
                    '    #PAGEHEADERREGION#' || unistr('\000a') || 
                    '    #DETAIL#' || unistr('\000a') || 
                    '    #PAGEFOOTERREGION#' || unistr('\000a') || 
                    '    #SUMMARYREGION#' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Blue Portrait',
    'RE',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '                        <jr:columnHeader height="30"> ' || unistr('\000a') || 
                    '                            <staticText> ' || unistr('\000a') || 
                    '                                <reportElement style="TabularItemHeader" x="0" y="0" width="#COLUMNWIDTH#" height="30" /> ' || unistr('\000a') || 
                    '                                <textElement textAlignment="#ALIGNMENT#"/> ' || unistr('\000a') || 
                    '                                <text><![CDATA[#HEADERTEXT#]]></text> ' || unistr('\000a') || 
                    '                            </staticText> ' || unistr('\000a') || 
                    '                        </jr:columnHeader>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'Standard',
    'TH',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<summary>' || unistr('\000a') || 
                    '		<band height="28">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="Summary" x="0" y="0" width="555" height="27"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[This is the summary for report #REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</summary>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait Summary',
    'RS',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<pageHeader>' || unistr('\000a') || 
                    '		<band height="24">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement style="Pageheader" x="0" y="0" width="555" height="20"/>' || unistr('\000a') || 
                    '				<box bottomPadding="0">' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<textField pattern="dd/MM/yyyy">' || unistr('\000a') || 
                    '					<reportElement style="Pageheader" x="455" y="0" width="100" height="18"/>' || unistr('\000a') || 
                    '					<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '				<staticText>' || unistr('\000a') || 
                    '					<reportElement x="112" y="';

  clstart := clstart || '2" width="322" height="18"/>' || unistr('\000a') || 
                    '					<textElement textAlignment="Center"/>' || unistr('\000a') || 
                    '					<text><![CDATA[#REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '				</staticText>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Portrait Centered report-title',
    'RH',
    NULL,
    NULL,
    clStart,
    clEnd,
    'Y'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="test" language="groovy" pageWidth="842" pageHeight="595" orientation="Landscape" columnWidth="802" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"';

  clstart := clstart || '/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Title" mode="Opaque" forecolor="#000000" backcolor="#CCCCCC" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Summary" mode="Opaque" forecolor="#000000" backcolor="#CCCCCC" hAlign="Center" fontSize="15" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pageheader" mode="Opaque" forecolor="#000000';

  clstart := clstart || '" backcolor="#EEEEEE" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="Pagefooter" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormRegiontitle" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" fontSize="13" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen li';

  clstart := clstart || 'neWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormRegionFrame" mode="Opaque" forecolor="#000000" backcolor="#FAFAFA">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="FormItemLabel" forecolor="#000000" hAlign="Left" fontSize="10" isBold="false" isItali';

  clstart := clstart || 'c="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="FormItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false"/>' || unistr('\000a') || 
                    '	<style name="TabularItemHeader" mode="Opaque" forecolor="#000000" backcolor="#EEEEEE" hAlign="Left" fontSize="10" isBold="false" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPe';

  clstart := clstart || 'n lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="TabularItem" forecolor="#000000" hAlign="Left" fontSize="10" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '		<conditionalStyle>' || unistr('\000a') || 
                    '			<conditionExpression><![CDATA[jrxml2pdf.Wrappers.mod($V{REPORT_COUNT},2) == 0]]></';

  clstart := clstart || 'conditionExpression>' || unistr('\000a') || 
                    '			<style mode="Opaque" backcolor="#FAFAFA"/>' || unistr('\000a') || 
                    '		</conditionalStyle>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '    #SUBDATASETS#' || unistr('\000a') || 
                    '    #PARAMETERS#' || unistr('\000a') || 
                    '    #MAINQUERY#' || unistr('\000a') || 
                    '    #BACKGROUNDREGION#' || unistr('\000a') || 
                    '    #TITLEREGION#' || unistr('\000a') || 
                    '    #PAGEHEADERREGION#' || unistr('\000a') || 
                    '    #DETAIL#' || unistr('\000a') || 
                    '    #PAGEFOOTERREGION#' || unistr('\000a') || 
                    '    #SUMMARYREGION#' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Grey Landscape',
    'RE',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '          <frame>' || unistr('\000a') || 
                    '                <reportElement x="#XPOSITION#" y="0" width="399" height="#HEIGHT#"/>' || unistr('\000a') || 
                    '                <staticText>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegiontitle" x="0" y="0" width="399" height="20"/>' || unistr('\000a') || 
                    '                    <text><![CDATA[#REGION_TITLE#]]></text>' || unistr('\000a') || 
                    '                </staticText>' || unistr('\000a') || 
                    '                <frame>' || unistr('\000a') || 
                    '                    <reportElement style="FormRegionFrame" x="0" y="20" width="399" height="#INNERHEIGHT#"/>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  clend := clend || '                </frame>' || unistr('\000a') || 
                    '            </frame>' || unistr('\000a') || 
                    '';

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape 2 columns',
    'FR',
    23,
    401,
    clStart,
    clEnd,
    'N'
  );
END;
/
declare
  clStart clob;
  clEnd   clob;
begin
  dbms_lob.createtemporary(clstart, true, DBMS_LOB.SESSION);
  dbms_lob.open(clstart, dbms_lob.LOB_READWRITE);

  clstart := clstart || '	<pageHeader>' || unistr('\000a') || 
                    '		<band height="24">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement style="Pageheader" x="0" y="0" width="802" height="20"/>' || unistr('\000a') || 
                    '				<box bottomPadding="0">' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<textField pattern="dd/MM/yyyy">' || unistr('\000a') || 
                    '					<reportElement style="Pageheader" x="700" y="0" width="100" height="18"/>' || unistr('\000a') || 
                    '					<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '				<staticText>' || unistr('\000a') || 
                    '					<reportElement x="112" y="';

  clstart := clstart || '2" width="522" height="18"/>' || unistr('\000a') || 
                    '					<textElement textAlignment="Center"/>' || unistr('\000a') || 
                    '					<text><![CDATA[#REPORT_TITLE#]]></text>' || unistr('\000a') || 
                    '				</staticText>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '';

  dbms_lob.createtemporary(clend, true, DBMS_LOB.SESSION);
  dbms_lob.open(clend, dbms_lob.LOB_READWRITE);

  INSERT INTO JRXML_APEX2JRXML_TEMPLATES (
    JAT_ID,
    JAT_NAME,
    JAT_TYPE,
    JAT_HEIGHT_OFFSET,
    JAT_CONTENT_WIDTH,
    JAT_TEMPLATE_START,
    JAT_TEMPLATE_END,
    JAT_STANDARD
  ) VALUES (
    JRXML_SEQ.NEXTVAL,
    'A4 Landscape Centered report-title',
    'RH',
    NULL,
    NULL,
    clStart,
    clEnd,
    'N'
  );
END;
/

