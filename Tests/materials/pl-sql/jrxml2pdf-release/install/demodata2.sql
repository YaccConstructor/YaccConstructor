declare
  v_data VARCHAR2(32767) := '';
  v_clob clob;
begin
  dbms_lob.createtemporary(v_clob, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_clob, dbms_lob.LOB_READWRITE);
  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="report1" language="groovy" pageWidth="595" pageHeight="842" columnWidth="535" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ire';
  v_data := v_data || 'port.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="reportTitle" fontSize="20" isBold="true"/>' || unistr('\000a') || 
                    '	<style name="reportSubTitle" mode="Transparent" forecolor="#3333FF" fontName="Arial" fontSize="12">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.0"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="columnHeader" mode="Transparent" forecolor="#000000" backcolor="#EEEEEE" fontName="Arial" fontSize="12" isBold="true" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box topPadding="2" leftPadding="2" bot';
  v_data := v_data || 'tomPadding="2" rightPadding="2"/>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="standardField" mode="Transparent" forecolor="#000000" backcolor="#FFFFFF" vAlign="Middle" fontName="Arial" fontSize="12" isBold="false" isItalic="false" isUnderline="false" isStrikeThrough="false">' || unistr('\000a') || 
                    '		<box topPadding="2" leftPadding="2" bottomPadding="2" rightPadding="2">' || unistr('\000a') || 
                    '			<pen lineWidth="0.5" lineColor="#000000"/>' || unistr('\000a') || 
                    '			<topPen lineWidth="0.0"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="0.0"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="0.0"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="0.0"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</st';
  v_data := v_data || 'yle>' || unistr('\000a') || 
                    '	<style name="alternateRow" mode="Transparent" hAlign="Left" vAlign="Middle" fontName="Arial" fontSize="12">' || unistr('\000a') || 
                    '		<box topPadding="0" leftPadding="0" bottomPadding="0" rightPadding="0"/>' || unistr('\000a') || 
                    '		<conditionalStyle>' || unistr('\000a') || 
                    '			<conditionExpression><![CDATA[jrxml2pdf.Wrappers.mod($V{REPORT_COUNT},2) == 0]]></conditionExpression>' || unistr('\000a') || 
                    '			<style mode="Opaque" backcolor="#FFCCFF"/>' || unistr('\000a') || 
                    '		</conditionalStyle>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="table">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="2.0" lineColor="#000000"/>' || unistr('\000a') || 
                    '			<topPen lineWidth=';
  v_data := v_data || '"2.0"/>' || unistr('\000a') || 
                    '			<leftPen lineWidth="2.0"/>' || unistr('\000a') || 
                    '			<bottomPen lineWidth="2.0"/>' || unistr('\000a') || 
                    '			<rightPen lineWidth="2.0"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="table_columnHeader" mode="Opaque" forecolor="#000000" backcolor="#FFFFFF">' || unistr('\000a') || 
                    '		<box>' || unistr('\000a') || 
                    '			<pen lineWidth="0.5" lineColor="#000000"/>' || unistr('\000a') || 
                    '		</box>' || unistr('\000a') || 
                    '	</style>' || unistr('\000a') || 
                    '	<style name="pageFooter" mode="Opaque" forecolor="#000000" backcolor="#CCCCFF"/>' || unistr('\000a') || 
                    '	<subDataset name="user_tab_cols">' || unistr('\000a') || 
                    '		<parameter name="pTableName" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<queryString>' || unistr('\000a') || 
                    '			<![CDATA[select table_name, column_n';
  v_data := v_data || 'ame, data_type, data_length, data_precision, data_scale from user_tab_cols' || unistr('\000a') || 
                    'where table_name = $P{pTableName}' || unistr('\000a') || 
                    'order by column_name]]>' || unistr('\000a') || 
                    '		</queryString>' || unistr('\000a') || 
                    '		<field name="TABLE_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<field name="COLUMN_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<field name="DATA_TYPE" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<field name="DATA_LENGTH" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="DATA_PRECISION" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="DATA_SCALE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	</subDataset>' || unistr('\000a') || 
                    '	<quer';
  v_data := v_data || 'yString>' || unistr('\000a') || 
                    '		<![CDATA[select * from user_tables' || unistr('\000a') || 
                    'order by table_name]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="TABLE_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="TABLESPACE_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="CLUSTER_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="IOT_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="STATUS" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="PCT_FREE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="PCT_USED" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="INI_TRANS" class="java.math.BigDecima';
  v_data := v_data || 'l"/>' || unistr('\000a') || 
                    '	<field name="MAX_TRANS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="INITIAL_EXTENT" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="NEXT_EXTENT" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="MIN_EXTENTS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="MAX_EXTENTS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="PCT_INCREASE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="FREELISTS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="FREELIST_GROUPS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="LOGGING" class="java.lang';
  v_data := v_data || '.String"/>' || unistr('\000a') || 
                    '	<field name="BACKED_UP" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="NUM_ROWS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="BLOCKS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="EMPTY_BLOCKS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="AVG_SPACE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="CHAIN_CNT" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="AVG_ROW_LEN" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="AVG_SPACE_FREELIST_BLOCKS" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="NUM_FREELIST_BLOCKS" class';
  v_data := v_data || '="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="DEGREE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="INSTANCES" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="CACHE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="TABLE_LOCK" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="SAMPLE_SIZE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="LAST_ANALYZED" class="java.sql.Timestamp"/>' || unistr('\000a') || 
                    '	<field name="PARTITIONED" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="IOT_TYPE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="TEMPORARY" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field nam';
  v_data := v_data || 'e="SECONDARY" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="NESTED" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="BUFFER_POOL" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="FLASH_CACHE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="CELL_FLASH_CACHE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="ROW_MOVEMENT" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="GLOBAL_STATS" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="USER_STATS" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="DURATION" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="SKIP_CORRUPT" class="java.l';
  v_data := v_data || 'ang.String"/>' || unistr('\000a') || 
                    '	<field name="MONITORING" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="CLUSTER_OWNER" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="DEPENDENCIES" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="COMPRESSION" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="COMPRESS_FOR" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="DROPPED" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="READ_ONLY" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="SEGMENT_CREATED" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="RESULT_CACHE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<backgrou';
  v_data := v_data || 'nd>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="141" splitType="Stretch">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="reportTitle" x="219" y="0" width="333" height="40"/>' || unistr('\000a') || 
                    '				<textElement verticalAlignment="Middle">' || unistr('\000a') || 
                    '					<font fontName="Arial"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[Report with table-usage]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '			<image scaleImage="RealSize">' || unistr('\000a') || 
                    '				<reportElement x="1" y="0" width="208" height="132"/>' || unistr('\000a') || 
                    '				<imageExpression><![CDATA["logo.jpg"]]></imageExpression>' || unistr('\000a') || 
                    '';
  v_data := v_data || '			</image>' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="reportSubTitle" x="222" y="53" width="333" height="79"/>' || unistr('\000a') || 
                    '				<textElement verticalAlignment="Top">' || unistr('\000a') || 
                    '					<font fontName="Arial"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[The report uses a table-component to show the columns of a table. The datatype uses nested column-groups. For the table JRXML_FONTS the columngroup for datalength is not printed.]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<colu';
  v_data := v_data || 'mnHeader>' || unistr('\000a') || 
                    '		<band height="28" splitType="Stretch">' || unistr('\000a') || 
                    '			<line>' || unistr('\000a') || 
                    '				<reportElement positionType="FixRelativeToBottom" x="0" y="18" width="555" height="1"/>' || unistr('\000a') || 
                    '				<graphicElement>' || unistr('\000a') || 
                    '					<pen lineWidth="0.5" lineColor="#999999"/>' || unistr('\000a') || 
                    '				</graphicElement>' || unistr('\000a') || 
                    '			</line>' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement style="columnHeader" mode="Opaque" x="0" y="0" width="555" height="18"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<text><![CDATA[TABLE_NAME]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</columnHeader>' || unistr('\000a') || 
                    '	<detail>' || unistr('\000a') || 
                    '		<band height="40" splitType="Stret';
  v_data := v_data || 'ch">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement style="alternateRow" mode="Opaque" x="0" y="0" width="555" height="18"/>' || unistr('\000a') || 
                    '				<textField isStretchWithOverflow="true">' || unistr('\000a') || 
                    '					<reportElement style="standardField" x="0" y="0" width="555" height="18"/>' || unistr('\000a') || 
                    '					<textElement/>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA[$F{TABLE_NAME}]]></textFieldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '			<componentElement>' || unistr('\000a') || 
                    '				<reportElement key="table" style="table" x="1" y="18" width="551" height="18"/>' || unistr('\000a') || 
                    '				<jr:table xmlns:jr="http://jasperreports.sour';
  v_data := v_data || 'ceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd">' || unistr('\000a') || 
                    '					<datasetRun subDataset="user_tab_cols">' || unistr('\000a') || 
                    '						<datasetParameter name="pTableName">' || unistr('\000a') || 
                    '							<datasetParameterExpression><![CDATA[$F{TABLE_NAME}]]></datasetParameterExpression>' || unistr('\000a') || 
                    '						</datasetParameter>' || unistr('\000a') || 
                    '						<connectionExpression><![CDATA[$P{REPORT_CONNECTION}]]></connectionExpression>' || unistr('\000a') || 
                    '					</datasetRun>' || unistr('\000a') || 
                    '					<jr:column width="12';
  v_data := v_data || '2">' || unistr('\000a') || 
                    '						<jr:columnHeader style="table_columnHeader" height="82" rowSpan="3">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement style="columnHeader" x="0" y="0" width="122" height="22"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<text><![CDATA[COLUMN_NAME]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</jr:columnHeader>' || unistr('\000a') || 
                    '						<jr:detailCell style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement style="standardField" x="0" y="0" width="122" height="20"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldE';
  v_data := v_data || 'xpression><![CDATA[$F{COLUMN_NAME}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</jr:detailCell>' || unistr('\000a') || 
                    '					</jr:column>' || unistr('\000a') || 
                    '					<jr:column width="90">' || unistr('\000a') || 
                    '						<jr:columnHeader style="table_columnHeader" height="82" rowSpan="3">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement style="columnHeader" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<text><![CDATA[DATA_TYPE]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</jr:columnHeader>' || unistr('\000a') || 
                    '						<jr:detailCell style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '		';
  v_data := v_data || '					<textField>' || unistr('\000a') || 
                    '								<reportElement style="standardField" x="0" y="0" width="90" height="20"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$F{DATA_TYPE}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</jr:detailCell>' || unistr('\000a') || 
                    '					</jr:column>' || unistr('\000a') || 
                    '					<jr:columnGroup width="270">' || unistr('\000a') || 
                    '						<printWhenExpression><![CDATA[$F{TABLE_NAME}!="JRXML_FONTS"]]></printWhenExpression>' || unistr('\000a') || 
                    '						<jr:columnHeader style="table_columnHeader" height="22" rowSpan="1">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement style="co';
  v_data := v_data || 'lumnHeader" x="0" y="0" width="270" height="22"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<text><![CDATA[DATA_LENGTH]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</jr:columnHeader>' || unistr('\000a') || 
                    '						<jr:column width="90">' || unistr('\000a') || 
                    '							<jr:columnHeader style="table_columnHeader" height="60" rowSpan="2">' || unistr('\000a') || 
                    '								<staticText>' || unistr('\000a') || 
                    '									<reportElement style="columnHeader" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '									<textElement/>' || unistr('\000a') || 
                    '									<text><![CDATA[Length (Chars)]]></text>' || unistr('\000a') || 
                    '								</staticText>' || unistr('\000a') || 
                    '							</jr:columnHeader>' || unistr('\000a') || 
                    '							<jr:detailCe';
  v_data := v_data || 'll style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '								<textField>' || unistr('\000a') || 
                    '									<reportElement style="standardField" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '									<textElement/>' || unistr('\000a') || 
                    '									<textFieldExpression><![CDATA[$F{DATA_LENGTH}]]></textFieldExpression>' || unistr('\000a') || 
                    '								</textField>' || unistr('\000a') || 
                    '							</jr:detailCell>' || unistr('\000a') || 
                    '						</jr:column>' || unistr('\000a') || 
                    '						<jr:columnGroup width="180">' || unistr('\000a') || 
                    '							<jr:columnHeader style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '								<staticText>' || unistr('\000a') || 
                    '									<reportElement style="columnHeader" x="0" y="';
  v_data := v_data || '0" width="180" height="22"/>' || unistr('\000a') || 
                    '									<textElement/>' || unistr('\000a') || 
                    '									<text><![CDATA[Length (Number)]]></text>' || unistr('\000a') || 
                    '								</staticText>' || unistr('\000a') || 
                    '							</jr:columnHeader>' || unistr('\000a') || 
                    '							<jr:column width="90">' || unistr('\000a') || 
                    '								<jr:columnHeader style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '									<staticText>' || unistr('\000a') || 
                    '										<reportElement style="columnHeader" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '										<textElement/>' || unistr('\000a') || 
                    '										<text><![CDATA[Precision]]></text>' || unistr('\000a') || 
                    '									</staticText>' || unistr('\000a') || 
                    '								</jr:columnHeader>' || unistr('\000a') || 
                    '								<jr:detailCell style=';
  v_data := v_data || '"table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '									<textField isBlankWhenNull="true">' || unistr('\000a') || 
                    '										<reportElement style="standardField" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '										<textElement/>' || unistr('\000a') || 
                    '										<textFieldExpression><![CDATA[$F{DATA_PRECISION}]]></textFieldExpression>' || unistr('\000a') || 
                    '									</textField>' || unistr('\000a') || 
                    '								</jr:detailCell>' || unistr('\000a') || 
                    '							</jr:column>' || unistr('\000a') || 
                    '							<jr:column width="90">' || unistr('\000a') || 
                    '								<jr:columnHeader style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '									<staticText>' || unistr('\000a') || 
                    '										<reportElement style="c';
  v_data := v_data || 'olumnHeader" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '										<textElement/>' || unistr('\000a') || 
                    '										<text><![CDATA[Scale]]></text>' || unistr('\000a') || 
                    '									</staticText>' || unistr('\000a') || 
                    '								</jr:columnHeader>' || unistr('\000a') || 
                    '								<jr:detailCell style="table_columnHeader" height="30" rowSpan="1">' || unistr('\000a') || 
                    '									<textField isBlankWhenNull="true">' || unistr('\000a') || 
                    '										<reportElement style="standardField" x="0" y="0" width="90" height="22"/>' || unistr('\000a') || 
                    '										<textElement/>' || unistr('\000a') || 
                    '										<textFieldExpression><![CDATA[$F{DATA_SCALE}]]></textFieldExpression>' || unistr('\000a') || 
                    '									</textField>' || unistr('\000a') || 
                    '								</jr:det';
  v_data := v_data || 'ailCell>' || unistr('\000a') || 
                    '							</jr:column>' || unistr('\000a') || 
                    '						</jr:columnGroup>' || unistr('\000a') || 
                    '					</jr:columnGroup>' || unistr('\000a') || 
                    '				</jr:table>' || unistr('\000a') || 
                    '			</componentElement>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</detail>' || unistr('\000a') || 
                    '	<columnFooter>' || unistr('\000a') || 
                    '		<band height="45" splitType="Stretch">' || unistr('\000a') || 
                    '			<line>' || unistr('\000a') || 
                    '				<reportElement positionType="FixRelativeToBottom" x="0" y="3" width="555" height="1"/>' || unistr('\000a') || 
                    '				<graphicElement>' || unistr('\000a') || 
                    '					<pen lineWidth="0.5" lineColor="#999999"/>' || unistr('\000a') || 
                    '				</graphicElement>' || unistr('\000a') || 
                    '			</line>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</columnFooter>' || unistr('\000a') || 
                    '	<pageFooter>' || unistr('\000a') || 
                    '		<band height="25" splitType="Stretch">' || unistr('\000a') || 
                    '			<frame>' || unistr('\000a') || 
                    '				<reportElement styl';
  v_data := v_data || 'e="pageFooter" mode="Opaque" x="-21" y="1" width="597" height="24"/>' || unistr('\000a') || 
                    '				<textField evaluationTime="Report">' || unistr('\000a') || 
                    '					<reportElement style="standardField" x="533" y="0" width="40" height="20"/>' || unistr('\000a') || 
                    '					<textElement verticalAlignment="Middle">' || unistr('\000a') || 
                    '						<font size="10" isBold="false"/>' || unistr('\000a') || 
                    '					</textElement>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA[" " + $V{PAGE_NUMBER}]]></textFieldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '				<textField>' || unistr('\000a') || 
                    '					<reportElement style="standardField" x="453" y="0" width="80" height="20"/>' || unistr('\000a') || 
                    '					<textElement tex';
  v_data := v_data || 'tAlignment="Right" verticalAlignment="Middle">' || unistr('\000a') || 
                    '						<font size="10" isBold="false"/>' || unistr('\000a') || 
                    '					</textElement>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA["Page "+$V{PAGE_NUMBER}+" of"]]></textFieldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '				<textField pattern="EEEEE dd MMMMM yyyy">' || unistr('\000a') || 
                    '					<reportElement style="standardField" x="22" y="1" width="197" height="20"/>' || unistr('\000a') || 
                    '					<textElement verticalAlignment="Middle">' || unistr('\000a') || 
                    '						<font size="10" isBold="false"/>' || unistr('\000a') || 
                    '					</textElement>' || unistr('\000a') || 
                    '					<textFieldExpression><![CDATA[new java.util.Date()]]></textFie';
  v_data := v_data || 'ldExpression>' || unistr('\000a') || 
                    '				</textField>' || unistr('\000a') || 
                    '			</frame>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</pageFooter>' || unistr('\000a') || 
                    '</jasperReport>' || unistr('\000a') || 
                    '';
  dbms_lob.writeappend(v_clob, length(v_data), v_data);
INSERT INTO JRXML_REPORT_DEFINITIONS ( 
  JRD_ID,
  JRD_NAME,
  JRD_DESCRIPTION,
  JRD_XML
) VALUES (
  38,
  'table_usage_with_column_groups',
  'The report uses a table-component to show the columns of a table. The datatype uses nested column-groups. For the table JRXML_FONTS the columngroup for datalength is not printed.',
  v_clob
);
  dbms_lob.close(v_clob);
  dbms_lob.freetemporary(v_clob);
end;
/
