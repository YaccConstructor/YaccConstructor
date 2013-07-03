declare
  v_data clob;
begin
  dbms_lob.createtemporary(v_data, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_data, dbms_lob.LOB_READWRITE);

  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="html_spacing" language="groovy" pageWidth="595" pageHeight="842" columnWidth="555" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="66eef8ee-b216-4206-b4c0-aa60d3f47962">' || unistr('\000a') || 
                    '	<property name=';

  v_data := v_data || '"ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[SELECT ''Some simple text'' TITLE,' || unistr('\000a') || 
                    '       ''This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. ';

  v_data := v_data || 'This is simple text. This is simple text. This is simple text. This is simple text. This is simple text. '' TEXT' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT ''Different headings'',' || unistr('\000a') || 
                    '       ''<h1 style="text-align: left;">Heading 1</h1> <h2>Heading 2</h2> <h3>Heading 3</h3> <h4>Heading 4</h4> <p>Some standard text</p> <p>&nbsp;</p>''' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT ''Lists, both ordered and unordered'',' || unistr('\000a') || 
                    '       ''<h1 style="text-align: left;">Lists</h1> <h2>An unordered list</h2> <ul> <li>one</li> <li>two</li> <li>three</li> </ul> <h2>';

  v_data := v_data || 'An ordered list</h2> <ol> <li>one</li> <li>two</li> <li>three</li> </ol>''' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT ''Different font-sizes and colors'',' || unistr('\000a') || 
                    '       ''<h1 style="text-align: left;">Inline Formatting</h1> <p><span style="font-size: medium;">This text contains text in different sizes, like <span style="font-size: x-small;">small</span>, <span style="font-size: x-large;">large</span>, <span style="font-size: xx-large;">very large</span>, and also you can use different color, like <span style="color: rgb(255, 0, 0);';

  v_data := v_data || '">red</span>, <span style="color: rgb(0, 255, 0);">green</span> and <span style="color: rgb(51, 102, 255);">blue</span>. And if you like to, you can <span style="font-size: xx-large;"><span style="color: rgb(255, 204, 0);">mix </span></span><span style="color: rgb(204, 153, 255);"><span style="font-size: small;">both </span></span><span style="color: rgb(255, 0, 0);"><span style="font-size: x-large;">options</span></span> <span style="color: rgb(255, 255, 0);"><span style="font-size: xx-small;">in </span></';

  v_data := v_data || 'span><span style="color: rgb(51, 102, 255);"><span style="font-size: large;">every</span></span> <span style="color: rgb(0, 255, 0);"><span style="font-size: xx-large;">way</span></span>.<br /> </span></p>''' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT ''Some preformatted text, in this case, PL/SQL'',' || unistr('\000a') || 
                    '       ''<pre><span style="color: rgb(51, 102, 255);">CREATE OR REPLACE FUNCTION </span>FK_TEXT(i_nDummy <span style="color: rgb(51, 102, 255);">IN </span><span style="color: rgb(255, 0, 0);">NUMBER</span>)'' || CHR(13) || CHR(10)';

  v_data := v_data || ' ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">RETURN </span><span style="color: rgb(255, 0, 0);">NUMBER </span><span style="color: rgb(51, 102, 255);">IS</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  CURSOR</span> crDummy <span style="color: rgb(51, 102, 255);">IS</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">    SELECT</span> i_nDummy+<span style="color: rgb(255, 204, 0);">1</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  ';

  v_data := v_data || '    FROM</span> DUAL;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''  nDummy <span style="color: rgb(255, 0, 0);">  NUMBER</span>;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">BEGIN</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(0, 255, 0);">  -- do some stuff</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  OPEN </span>crDummy;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  FETCh </span>crDummy <span style="color: rgb(51, 102, 255);">INTO </spa';

  v_data := v_data || 'n>nDummy;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  CLOSE </span>crDummy;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(0, 255, 0);">  -- return some stuff</span>'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">  RETURN </span>nDummy;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''<span style="color: rgb(51, 102, 255);">END</span>;'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''/'' || CHR(13) || CHR(10) ||' || unistr('\000a') || 
                    '''</pre>''' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="TITLE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<fi';

  v_data := v_data || 'eld name="TEXT" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="60" splitType="Stretch">' || unistr('\000a') || 
                    '			<line>' || unistr('\000a') || 
                    '				<reportElement uuid="73958f11-e607-4853-aadc-ad366d54115b" x="0" y="59" width="555" height="1"/>' || unistr('\000a') || 
                    '			</line>' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="d9db5dbd-70d5-496d-99f7-5bcb684e7a3e" x="0" y="0" width="555" height="59"/>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="14" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[This report shows the HTML-c';

  v_data := v_data || 'apabilities of PL-jrxml2pdf. Not the enhanced-mode which allows to render H1-H4-textes in its defined size.]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<columnHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnHeader>' || unistr('\000a') || 
                    '	<detail>' || unistr('\000a') || 
                    '		<band height="255" splitType="Stretch">' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="873cf153-72cc-445e-972f-4deb0b9c1f1c" key="enhanced" x="0" y="19" width="555" height="236" forecolor="#000000"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<pen lineWidth=';

  v_data := v_data || '"0.5"/>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<leftPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<rightPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<textElement markup="html">' || unistr('\000a') || 
                    '					<font fontName="Arial" size="12"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[""+$F{TEXT}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="5d960264-3270-427f-9f6a-1b20261531f8" mode="Opaque" x="0" y="0" width="555" height="18" backcolor="#CCCCCC"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<pen lineWidth="0.5"';

  v_data := v_data || '/>' || unistr('\000a') || 
                    '					<topPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<leftPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '					<rightPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{TITLE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</detail>' || unistr('\000a') || 
                    '	<columnFooter>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnFooter>' || unistr('\000a') || 
                    '	<pageFooter>' || unistr('\000a') || 
                    '		<band height="20" splitType="Stretch">' || unistr('\000a') || 
                    '			<line>' || unistr('\000a') || 
                    '				<reportElement uuid="73958f11-e607-4853-aadc-ad366d54115b" x="0" y="0" width="555" height="1"/>' || unistr('\000a') || 
                    '			</line>' || unistr('\000a') || 
                    '		</';

  v_data := v_data || 'band>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '	<summary>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</summary>' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

INSERT INTO JRXML_REPORT_DEFINITIONS ( 
  JRD_ID,
  JRD_NAME,
  JRD_DESCRIPTION,
  JRD_XML
) VALUES (
  41,
  'html',
  'Demo for using markup html',
  v_data
);
  dbms_lob.freetemporary(v_data);
end;
/

declare
  v_data clob;
begin
  dbms_lob.createtemporary(v_data, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_data, dbms_lob.LOB_READWRITE);

  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="map_component" language="groovy" pageWidth="595" pageHeight="842" whenNoDataType="AllSectionsNoDetail" columnWidth="555" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="9138ce81-47eb-41a';

  v_data := v_data || 'f-b033-4a6fa5be1ab6">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[SELECT 51.500152 LATITUDE,' || unistr('\000a') || 
                    '       -0.126236 LONGITUDE,' || unistr('\000a') || 
                    '       12  ZOOM,' || unistr('\000a') || 
                    '       ''London, GB'' TOWN' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 48.857035 LATITUDE,' || unistr('\000a') || 
                    '       2.352448 LONGITUDE,' || unistr('\000a') || 
                    '       11  ZOOM,' || unistr('\000a') || 
                    '       ''Paris, France'' TOWN' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 40.722283 LATITUDE,' || unistr('\000a') || 
                    '       -74.008026 LONGITUDE,' || unistr('\000a') || 
                    '       10  ZOOM,' || unistr('\000a') || 
                    '       ''New York, US'' TOWN';

  v_data := v_data || '' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="LATITUDE" class="java.lang.Float"/>' || unistr('\000a') || 
                    '	<field name="LONGITUDE" class="java.lang.Float"/>' || unistr('\000a') || 
                    '	<field name="ZOOM" class="java.lang.Integer"/>' || unistr('\000a') || 
                    '	<field name="TOWN" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="102" splitType="Stretch">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="5fbfd200-83b2-4c4c-85e0-ac8f722139f7" x="0" y="0" width="555" height="38"/>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="14" isBold="true"/>' || unistr('\000a') || 
                    '		';

  v_data := v_data || '		</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[Report with usage of the map-component. It shows some towns with according google-map.]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="1bd00469-5cfd-4853-89d0-3f0568ae7d70" x="0" y="49" width="555" height="17"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[If you don''t see any map, you might need to grant access to the map-url. See installation instructions.]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="893abd62-daa9-46c9-8f39-50be3b6410';

  v_data := v_data || '0e" x="0" y="66" width="555" height="36" forecolor="#FF3333"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[The map-component in PL-jrxml2pdf uses the static-map-features of the google-map-api. Be sure to read the usage-notes before you use this in a production environment]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<columnHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnHeader>' || unistr('\000a') || 
                    '	<detail>' || unistr('\000a') || 
                    '		<band height="324" splitType="Stretch">' || unistr('\000a') || 
                    '			<componentElement>' || unistr('\000a') || 
                    '		';

  v_data := v_data || '		<reportElement uuid="4fff35c3-6d5f-4662-9739-eb9519479d2a" x="186" y="0" width="369" height="312"/>' || unistr('\000a') || 
                    '				<mp:map xmlns:mp="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd">' || unistr('\000a') || 
                    '					<mp:latitudeExpression><![CDATA[$F{LATITUDE}]]></mp:latitudeExpression>' || unistr('\000a') || 
                    '					<mp:longitudeExpression><![CDATA[$F{LONGITUDE}]]></mp:longitudeExpression>' || unistr('\000a') || 
                    '					<mp:zoomExpression><![C';

  v_data := v_data || 'DATA[$F{ZOOM}]]></mp:zoomExpression>' || unistr('\000a') || 
                    '				</mp:map>' || unistr('\000a') || 
                    '			</componentElement>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="bc782060-1273-4b57-bbdc-74108f577c49" x="0" y="0" width="186" height="20"/>' || unistr('\000a') || 
                    '				<box>' || unistr('\000a') || 
                    '					<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '				</box>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="12" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{TOWN}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<line>' || unistr('\000a') || 
                    '				<reportElement uuid="8c09463d-25d6-4858-9cd0-7b83053f4fc3" x="0" y="320" width="555" height="1"';

  v_data := v_data || '/>' || unistr('\000a') || 
                    '				<graphicElement>' || unistr('\000a') || 
                    '					<pen lineWidth="3.0"/>' || unistr('\000a') || 
                    '				</graphicElement>' || unistr('\000a') || 
                    '			</line>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</detail>' || unistr('\000a') || 
                    '	<columnFooter>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnFooter>' || unistr('\000a') || 
                    '	<pageFooter>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '	<summary>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</summary>' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

INSERT INTO JRXML_REPORT_DEFINITIONS ( 
  JRD_ID,
  JRD_NAME,
  JRD_DESCRIPTION,
  JRD_XML
) VALUES (
  42,
  'city_maps',
  'Demo for using the map-component',
  v_data
);
  dbms_lob.freetemporary(v_data);
end;
/

declare
  v_data clob;
begin
  dbms_lob.createtemporary(v_data, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_data, dbms_lob.LOB_READWRITE);

  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="crosstab_daterow" language="groovy" pageWidth="842" pageHeight="595" orientation="Landscape" columnWidth="802" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="09d50ff3-e527-4e77-a499-013';

  v_data := v_data || '30d68354c">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Crosstab Data Text" hAlign="Center"/>' || unistr('\000a') || 
                    '	<style name="table">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="1.0" lineColor="#000000"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="table_TH" mode="Opaque" backcolor="#7FFFD4">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.5" lineColor="#000000"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="table_CH" mode="Opaque" backcolor="#BFFFEA">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.5" lineC';

  v_data := v_data || 'olor="#000000"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="table_TD" mode="Opaque" backcolor="#FFFFFF">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.5" lineColor="#000000"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '		<conditionalStyle>' || unistr('\000a') || 
                    '			<conditionExpression><![CDATA[new Boolean($V{REPORT_COUNT}.intValue()%2==0)]]></conditionExpression>' || unistr('\000a') || 
                    '			<style backcolor="#EFFFF9"/>' || unistr('\000a') || 
                    '		</conditionalStyle>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<subDataset name="States" uuid="3f736e95-c19c-457e-b645-2ffac69fab1a">' || unistr('\000a') || 
                    '		<queryString language="SQL">' || unistr('\000a') || 
                    '			<![CDATA[SELECT ST,' || unistr('\000a') || 
                    '       STATE_NAME' || unistr('\000a') || 
                    '  FROM DEMO_STATES' || unistr('\000a') || 
                    ' W';

  v_data := v_data || 'HERE ST IN (select CUST_STATE' || unistr('\000a') || 
                    '			    from demo_orders do,' || unistr('\000a') || 
                    '				     demo_customers dc' || unistr('\000a') || 
                    '			   where do.customer_id=dc.customer_id' || unistr('\000a') || 
                    '			  )' || unistr('\000a') || 
                    'ORDER BY 1]]>' || unistr('\000a') || 
                    '		</queryString>' || unistr('\000a') || 
                    '		<field name="ST" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<field name="STATE_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '	</subDataset>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[select CUST_STATE,' || unistr('\000a') || 
                    '       ORDER_TIMESTAMP MONTH,' || unistr('\000a') || 
                    '       ORDER_TOTAL' || unistr('\000a') || 
                    '  from demo_orders do,' || unistr('\000a') || 
                    '       demo_customers dc' || unistr('\000a') || 
                    'where do.customer_id=dc.customer_id]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="CUST_STATE" class="ja';

  v_data := v_data || 'va.lang.String"/>' || unistr('\000a') || 
                    '	<field name="MONTH" class="java.sql.Timestamp"/>' || unistr('\000a') || 
                    '	<field name="ORDER_TOTAL" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="34" splitType="Stretch">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="cf88ae6f-daeb-41c9-a2c5-fe0f9e645068" x="0" y="0" width="555" height="33"/>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="14" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[Crosstab-example, showing the amount of all demo_orders by date and';

  v_data := v_data || ' state]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<columnHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnHeader>' || unistr('\000a') || 
                    '	<detail>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</detail>' || unistr('\000a') || 
                    '	<columnFooter>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</columnFooter>' || unistr('\000a') || 
                    '	<pageFooter>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '	<summary>' || unistr('\000a') || 
                    '		<band height="90" splitType="Stretch">' || unistr('\000a') || 
                    '			<crosstab>' || unistr('\000a') || 
                    '				<reportElement uuid="5996be5e-d29a-45fb-bdd0-5c47cb22b1fa" x="0" y="0" width="555" height="90';

  v_data := v_data || '"/>' || unistr('\000a') || 
                    '				<rowGroup name="MONTH" width="70" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.sql.Timestamp">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{MONTH}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#00FFA9" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="d8279ab5-2b3a-41c2-ba40-9c0a6eaf2270" style="Crosstab Data Text" x="0" y="0" width="70" height="25"/>' || unistr('\000a') || 
                    '		';

  v_data := v_data || '						<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{MONTH}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabRowHeader>' || unistr('\000a') || 
                    '					<crosstabTotalRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="64f6abb7-b174-453a-94c3-501cbf994c62" x="0" y="0" width="70" height="35"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" v';

  v_data := v_data || 'erticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total for all months]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalRowHeader>' || unistr('\000a') || 
                    '				</rowGroup>' || unistr('\000a') || 
                    '				<columnGroup name="CUST_STATE" height="30" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{CUST_STATE}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#00FFA9" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineC';

  v_data := v_data || 'olor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="678c2c42-602f-4a47-bd46-ee05895acba9" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{CUST_STATE}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabColumnHeader>' || unistr('\000a') || 
                    '					<crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="';

  v_data := v_data || '#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="29a2d0cc-ae45-493a-a718-72f69e6a13fd" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total for all state]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '				</columnGroup>' || unistr('\000a') || 
                    '				<measure name="ORDER_TOTALMeasure" class="java.math.BigDecimal" calculation="Sum">' || unistr('\000a') || 
                    '					<measureExpression><![CDATA[$F{ORDER_TOTAL}]]></m';

  v_data := v_data || 'easureExpression>' || unistr('\000a') || 
                    '				</measure>' || unistr('\000a') || 
                    '				<crosstabCell width="50" height="25">' || unistr('\000a') || 
                    '					<cellContents>' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="ff31de81-d067-4f41-bb56-f79189bb6707" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<cros';

  v_data := v_data || 'stabCell height="35" rowTotalGroup="MONTH">' || unistr('\000a') || 
                    '					<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="dc425f5c-eb84-4ef5-820a-9da16a6e8187" style="Crosstab Data Text" x="0" y="0" width="50" height="35"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<c';

  v_data := v_data || 'rosstabCell width="50" columnTotalGroup="CUST_STATE">' || unistr('\000a') || 
                    '					<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="5f43475d-c104-4d33-9655-088f28564ce2" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCe';

  v_data := v_data || 'll>' || unistr('\000a') || 
                    '				<crosstabCell height="35" rowTotalGroup="MONTH" columnTotalGroup="CUST_STATE">' || unistr('\000a') || 
                    '					<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="70f9b929-df82-4d38-af1a-95260df3654e" style="Crosstab Data Text" x="0" y="0" width="50" height="35"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '				';

  v_data := v_data || '	</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '			</crosstab>' || unistr('\000a') || 
                    '			<componentElement>' || unistr('\000a') || 
                    '				<reportElement uuid="916e5687-d3ec-4463-8765-94b2d9359982" key="table" style="table" x="555" y="0" width="247" height="90"/>' || unistr('\000a') || 
                    '				<jr:table xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd">' || unistr('\000a') || 
                    '					<datasetRun subDataset="States" uuid="357a869e-bf0c-4c39-a300-e5d265f52cb2">' || unistr('\000a') || 
                    '';

  v_data := v_data || '						<connectionExpression><![CDATA[$P{REPORT_CONNECTION}]]></connectionExpression>' || unistr('\000a') || 
                    '					</datasetRun>' || unistr('\000a') || 
                    '					<jr:columnGroup uuid="8f10c918-886f-4b92-ac08-cafa5c7875df" width="181">' || unistr('\000a') || 
                    '						<jr:columnHeader height="30" rowSpan="1">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="96be062f-eae1-4109-9a76-ed4b147ff5db" x="0" y="0" width="181" height="30"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center">' || unistr('\000a') || 
                    '									<font fontName="Arial" isBold="true"/>' || unistr('\000a') || 
                    '								</textElement>' || unistr('\000a') || 
                    '								<text><![CDATA[State information]';

  v_data := v_data || ']></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</jr:columnHeader>' || unistr('\000a') || 
                    '						<jr:column uuid="da5be00f-d74f-4177-97ba-dfeb694ba8e9" width="61">' || unistr('\000a') || 
                    '							<jr:columnHeader height="21" rowSpan="1">' || unistr('\000a') || 
                    '								<staticText>' || unistr('\000a') || 
                    '									<reportElement uuid="4e38c13c-c0ff-4895-8d9e-c6d34346ad97" x="0" y="0" width="61" height="20"/>' || unistr('\000a') || 
                    '									<box>' || unistr('\000a') || 
                    '										<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '									</box>' || unistr('\000a') || 
                    '									<textElement textAlignment="Center">' || unistr('\000a') || 
                    '										<font fontName="Arial" isBold="true"/>' || unistr('\000a') || 
                    '									</textElement>' || unistr('\000a') || 
                    '									<text><![C';

  v_data := v_data || 'DATA[Shortcut]]></text>' || unistr('\000a') || 
                    '								</staticText>' || unistr('\000a') || 
                    '							</jr:columnHeader>' || unistr('\000a') || 
                    '							<jr:detailCell height="20" rowSpan="1">' || unistr('\000a') || 
                    '								<textField>' || unistr('\000a') || 
                    '									<reportElement uuid="9fae916a-4f8b-410a-a79c-107c44a86cd0" x="0" y="0" width="61" height="20"/>' || unistr('\000a') || 
                    '									<textElement>' || unistr('\000a') || 
                    '										<font fontName="Arial"/>' || unistr('\000a') || 
                    '									</textElement>' || unistr('\000a') || 
                    '									<textFieldExpression><![CDATA[$F{ST}]]></textFieldExpression>' || unistr('\000a') || 
                    '								</textField>' || unistr('\000a') || 
                    '							</jr:detailCell>' || unistr('\000a') || 
                    '						</jr:column>' || unistr('\000a') || 
                    '						<jr:column uuid="1151a373-341d-4f8d-b19c-ddf2';

  v_data := v_data || 'b8d3b2d7" width="120">' || unistr('\000a') || 
                    '							<jr:columnHeader height="21" rowSpan="1">' || unistr('\000a') || 
                    '								<staticText>' || unistr('\000a') || 
                    '									<reportElement uuid="a1f77b93-6863-46e2-9f70-1355a8265d61" x="0" y="0" width="120" height="20"/>' || unistr('\000a') || 
                    '									<box>' || unistr('\000a') || 
                    '										<leftPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '										<bottomPen lineWidth="0.5"/>' || unistr('\000a') || 
                    '									</box>' || unistr('\000a') || 
                    '									<textElement textAlignment="Center">' || unistr('\000a') || 
                    '										<font fontName="Arial" isBold="true"/>' || unistr('\000a') || 
                    '									</textElement>' || unistr('\000a') || 
                    '									<text><![CDATA[Description]]></text>' || unistr('\000a') || 
                    '								</staticText>' || unistr('\000a') || 
                    '							</jr:columnH';

  v_data := v_data || 'eader>' || unistr('\000a') || 
                    '							<jr:detailCell height="20" rowSpan="1">' || unistr('\000a') || 
                    '								<textField>' || unistr('\000a') || 
                    '									<reportElement uuid="b366559b-5947-4079-94d2-60ab9ffe741d" x="0" y="0" width="120" height="20"/>' || unistr('\000a') || 
                    '									<textElement>' || unistr('\000a') || 
                    '										<font fontName="Arial"/>' || unistr('\000a') || 
                    '									</textElement>' || unistr('\000a') || 
                    '									<textFieldExpression><![CDATA[$F{STATE_NAME}]]></textFieldExpression>' || unistr('\000a') || 
                    '								</textField>' || unistr('\000a') || 
                    '							</jr:detailCell>' || unistr('\000a') || 
                    '						</jr:column>' || unistr('\000a') || 
                    '					</jr:columnGroup>' || unistr('\000a') || 
                    '				</jr:table>' || unistr('\000a') || 
                    '			</componentElement>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</summary>' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';

INSERT INTO JRXML_REPORT_DEFINITIONS ( 
  JRD_ID,
  JRD_NAME,
  JRD_DESCRIPTION,
  JRD_XML
) VALUES (
  43,
  'crosstab_orders_with_states',
  'a crosstab-report along with a table-component',
  v_data
);
  dbms_lob.freetemporary(v_data);
end;
/

