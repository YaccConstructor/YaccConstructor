declare
  v_data clob;
begin
  dbms_lob.createtemporary(v_data, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_data, dbms_lob.LOB_READWRITE);

  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="demo_charts" language="groovy" pageWidth="1190" pageHeight="842" orientation="Landscape" columnWidth="1150" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="12399ccc-b402-4eb9-9cad-5f4ed6';

  v_data := v_data || '7ec820">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="211"/>' || unistr('\000a') || 
                    '	<subDataset name="Chartdata" uuid="40abc873-785f-4426-88d3-f9892af721e9">' || unistr('\000a') || 
                    '		<queryString>' || unistr('\000a') || 
                    '			<![CDATA[SELECT CUST_LAST_NAME,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL) ORDER_SUM,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*-0.95 ORDER_SUM2,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.90 ORDER_SUM3,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.85 ORDER_SUM4,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.80 ORDER_SUM5,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.75 ORDER_SUM6,' || unistr('\000a') || 
                    '       SU';

  v_data := v_data || 'M(ORDER_TOTAL)*0.70 ORDER_SUM7,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.65 ORDER_SUM8,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.60 ORDER_SUM9,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.55 ORDER_SUM10,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.50 ORDER_SUM11,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.45 ORDER_SUM12,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.40 ORDER_SUM13,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.35 ORDER_SUM14,' || unistr('\000a') || 
                    '       SUM(ORDER_TOTAL)*0.30 ORDER_SUM15' || unistr('\000a') || 
                    '  FROM DEMO_ORDERS DO,' || unistr('\000a') || 
                    '       DEMO_CUSTOMERS DC' || unistr('\000a') || 
                    ' WHERE DO.CUSTOMER_ID=DC.CUSTOMER_ID' || unistr('\000a') || 
                    ' GROUP BY CUST_LAST_NAME]]>' || unistr('\000a') || 
                    '		</queryString>' || unistr('\000a') || 
                    '		<field name="CU';

  v_data := v_data || 'ST_LAST_NAME" class="java.lang.String"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM2" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM3" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM4" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM5" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM6" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM7" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM8" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field';

  v_data := v_data || ' name="ORDER_SUM9" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM10" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM11" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM12" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM13" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM14" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="ORDER_SUM15" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	</subDataset>' || unistr('\000a') || 
                    '	<subDataset name="Chartdata_Age" uuid="40abc873-785f-4426-88d3-f9892af721e9">' || unistr('\000a') || 
                    '		<queryString';

  v_data := v_data || '>' || unistr('\000a') || 
                    '			<![CDATA[SELECT 20 AGE,' || unistr('\000a') || 
                    '       1500 INCOME_MALE,' || unistr('\000a') || 
                    '       1350 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 25 AGE,' || unistr('\000a') || 
                    '       1700 INCOME_MALE,' || unistr('\000a') || 
                    '       1850 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 30 AGE,' || unistr('\000a') || 
                    '       2000 INCOME_MALE,' || unistr('\000a') || 
                    '       2100 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 35 AGE,' || unistr('\000a') || 
                    '       2200 INCOME_MALE,' || unistr('\000a') || 
                    '       2200 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 40 AGE,' || unistr('\000a') || 
                    '       2500 INCOME_MALE,' || unistr('\000a') || 
                    '       2250 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 45 AGE,' || unistr('\000a') || 
                    '       2700 INCOME_MALE,' || unistr('\000a') || 
                    '       2400 INCOME';

  v_data := v_data || '_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 50 AGE,' || unistr('\000a') || 
                    '       2750 INCOME_MALE,' || unistr('\000a') || 
                    '       2500 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 55 AGE,' || unistr('\000a') || 
                    '       2750 INCOME_MALE,' || unistr('\000a') || 
                    '       2550 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 60 AGE,' || unistr('\000a') || 
                    '       2800 INCOME_MALE,' || unistr('\000a') || 
                    '       2700 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT 65 AGE,' || unistr('\000a') || 
                    '       2900 INCOME_MALE,' || unistr('\000a') || 
                    '       2500 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '		</queryString>' || unistr('\000a') || 
                    '		<field name="AGE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field name="INCOME_MALE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '		<field n';

  v_data := v_data || 'ame="INCOME_FEMALE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	</subDataset>' || unistr('\000a') || 
                    '	<subDataset name="Chartdata_Date" uuid="40abc873-785f-4426-88d3-f9892af721e9">' || unistr('\000a') || 
                    '		<queryString>' || unistr('\000a') || 
                    '			<![CDATA[SELECT TO_DATE(''1.1.1992'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       700 INCOME_MALE,' || unistr('\000a') || 
                    '       1350 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1987'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       1700 INCOME_MALE,' || unistr('\000a') || 
                    '       1850 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1982'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2000 INCOME_MALE,' || unistr('\000a') || 
                    '       2100 INC';

  v_data := v_data || 'OME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1977'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2200 INCOME_MALE,' || unistr('\000a') || 
                    '       2200 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1972'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2500 INCOME_MALE,' || unistr('\000a') || 
                    '       2250 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1967'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2700 INCOME_MALE,' || unistr('\000a') || 
                    '       2400 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1962'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2750 INCOME_MALE,' || unistr('\000a') || 
                    '       2500 INCOME_FEMALE' || unistr('\000a') || 
                    '  FRO';

  v_data := v_data || 'M DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1957'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2750 INCOME_MALE,' || unistr('\000a') || 
                    '       2550 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1952'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2800 INCOME_MALE,' || unistr('\000a') || 
                    '       2700 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL' || unistr('\000a') || 
                    'UNION ALL' || unistr('\000a') || 
                    'SELECT TO_DATE(''1.1.1947'', ''DD.MM.YYYY'') BIRTHDAY,' || unistr('\000a') || 
                    '       2900 INCOME_MALE,' || unistr('\000a') || 
                    '       2500 INCOME_FEMALE' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '		</queryString>' || unistr('\000a') || 
                    '		<field name="BIRTHDAY" class="java.sql.Timestamp"/>' || unistr('\000a') || 
                    '		<field name="INCOME_MALE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '';

  v_data := v_data || '		<field name="INCOME_FEMALE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	</subDataset>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[SELECT 1 X' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="X" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="32" splitType="Stretch">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="4ac79d3b-30bd-47e6-808d-93c2b75eb045" x="0" y="0" width="1150" height="30"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Center">' || unistr('\000a') || 
                    '					<font size="22" isBold="true"/>' || unistr('\000a') || 
                    '				</textEleme';

  v_data := v_data || 'nt>' || unistr('\000a') || 
                    '				<text><![CDATA[The reports shows the usage of the chart-types supported in Release 1.1 of PL-JRXML2PDF]]></text>' || unistr('\000a') || 
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
                    '		<band height="618" splitType="Stretch">' || unistr('\000a') || 
                    '			<pi';

  v_data := v_data || 'eChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<reportElement uuid="6e76d982-6dce-44cf-9f63-22165bb681ff" x="0" y="0" width="386" height="326"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '						<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA["A Piechart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLegend/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '		';

  v_data := v_data || '		<pieDataset>' || unistr('\000a') || 
                    '					<dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata" uuid="e5c25c1f-a44d-47f7-8781-e28b834e8537"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<keyExpression><![CDATA[$F{CUST_LAST_NAME}]]></keyExpression>' || unistr('\000a') || 
                    '					<valueExpression><![CDATA[$F{ORDER_SUM}]]></valueExpression>' || unistr('\000a') || 
                    '				</pieDataset>' || unistr('\000a') || 
                    '				<piePlot>' || unistr('\000a') || 
                    '					<plot/>' || unistr('\000a') || 
                    '					<itemLabel/>' || unistr('\000a') || 
                    '				</piePlot>' || unistr('\000a') || 
                    '			</pieChart>' || unistr('\000a') || 
                    '			<barChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<reportElement uuid="637e222a-c293-456b-a228-428a9cff18ee" x="386" y="0" width="383" height="326"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen ';

  v_data := v_data || 'lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '						<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA["A barchart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLegend/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '				<categoryDataset>' || unistr('\000a') || 
                    '					<dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata" uuid="9389fd9f-b137-45dd-9c26-7a346cbc106b"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<c';

  v_data := v_data || 'ategorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM}]]></valueExpression>' || unistr('\000a') || 
                    '						<labelExpression><![CDATA[$F{ORDER_SUM}+""]]></labelExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum 80%"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpressio';

  v_data := v_data || 'n><![CDATA[$F{ORDER_SUM4}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum 60%"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM9}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '				</categoryDataset>' || unistr('\000a') || 
                    '				<barPlot isShowLabels="false">' || unistr('\000a') || 
                    '					<plot labelRotation="45.0"/>' || unistr('\000a') || 
                    '					<itemLabel/>' || unistr('\000a') || 
                    '				</barPlot>' || unistr('\000a') || 
                    '			</barChart>' || unistr('\000a') || 
                    '			<bar3DChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<report';

  v_data := v_data || 'Element uuid="e01381be-564f-481d-9932-d2f92c62a656" x="769" y="0" width="381" height="326"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '						<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA[" A 3d-barchart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLegend/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '				<categoryDataset>' || unistr('\000a') || 
                    '					<';

  v_data := v_data || 'dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata" uuid="480b1c43-1da0-4530-b856-c6f2f8eb1211"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum 60%"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NA';

  v_data := v_data || 'ME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM9}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum negative"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM2}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '				</categoryDataset>' || unistr('\000a') || 
                    '				<bar3DPlot>' || unistr('\000a') || 
                    '					<plot labelRotation="90.0">' || unistr('\000a') || 
                    '						<seriesColor seriesOrder="0" color="#FF9999"/>' || unistr('\000a') || 
                    '	';

  v_data := v_data || '					<seriesColor seriesOrder="1" color="#FF3333"/>' || unistr('\000a') || 
                    '						<seriesColor seriesOrder="2" color="#CC0033"/>' || unistr('\000a') || 
                    '					</plot>' || unistr('\000a') || 
                    '					<itemLabel/>' || unistr('\000a') || 
                    '				</bar3DPlot>' || unistr('\000a') || 
                    '			</bar3DChart>' || unistr('\000a') || 
                    '			<lineChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<reportElement uuid="045f944d-a729-4b99-a240-b3fdf23c4d21" x="0" y="326" width="386" height="292"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '	';

  v_data := v_data || '					<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA["A category linechart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLegend/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '				<categoryDataset>' || unistr('\000a') || 
                    '					<dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata" uuid="564be36a-4ab8-4434-be5e-b3323a943e14"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpres';

  v_data := v_data || 'sion><![CDATA[$F{ORDER_SUM}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum 80%"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{ORDER_SUM4}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '					<categorySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Order sum 60%"]]></seriesExpression>' || unistr('\000a') || 
                    '						<categoryExpression><![CDATA[$F{CUST_LAST_NAME}]]></categoryExpression>' || unistr('\000a') || 
                    '						<';

  v_data := v_data || 'valueExpression><![CDATA[$F{ORDER_SUM9}]]></valueExpression>' || unistr('\000a') || 
                    '					</categorySeries>' || unistr('\000a') || 
                    '				</categoryDataset>' || unistr('\000a') || 
                    '				<linePlot>' || unistr('\000a') || 
                    '					<plot/>' || unistr('\000a') || 
                    '				</linePlot>' || unistr('\000a') || 
                    '			</lineChart>' || unistr('\000a') || 
                    '			<xyLineChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<reportElement uuid="4bf4712a-28dc-46fb-98cb-7fc6df4d73f2" x="386" y="326" width="383" height="292"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '';

  v_data := v_data || '						<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA["A XY-linechart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLegend/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '				<xyDataset>' || unistr('\000a') || 
                    '					<dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata_Age" uuid="2df0bcc7-e7fc-438f-b604-4440ef83b7fd"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<xySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Income male"]]></seriesExpression>' || unistr('\000a') || 
                    '						<xValueExpression><![CDATA[$F{AGE}]]></xValueExpression>' || unistr('\000a') || 
                    '						<yValueExpression><![CDATA[$F{INCOME_M';

  v_data := v_data || 'ALE}]]></yValueExpression>' || unistr('\000a') || 
                    '						<labelExpression><![CDATA["Hand"]]></labelExpression>' || unistr('\000a') || 
                    '					</xySeries>' || unistr('\000a') || 
                    '					<xySeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Income female"]]></seriesExpression>' || unistr('\000a') || 
                    '						<xValueExpression><![CDATA[$F{AGE}]]></xValueExpression>' || unistr('\000a') || 
                    '						<yValueExpression><![CDATA[$F{INCOME_FEMALE}]]></yValueExpression>' || unistr('\000a') || 
                    '						<labelExpression><![CDATA["Hund"]]></labelExpression>' || unistr('\000a') || 
                    '					</xySeries>' || unistr('\000a') || 
                    '				</xyDataset>' || unistr('\000a') || 
                    '				<linePlot>' || unistr('\000a') || 
                    '					<plot labelRotation="0.0"/>' || unistr('\000a') || 
                    '				</linePlot>' || unistr('\000a') || 
                    '			</xyLineChart>' || unistr('\000a') || 
                    '			<timeS';

  v_data := v_data || 'eriesChart>' || unistr('\000a') || 
                    '				<chart>' || unistr('\000a') || 
                    '					<reportElement uuid="6bd14a77-0920-43e0-8202-cee424dcc8fb" x="769" y="326" width="381" height="292"/>' || unistr('\000a') || 
                    '					<box>' || unistr('\000a') || 
                    '						<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '						<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '					</box>' || unistr('\000a') || 
                    '					<chartTitle>' || unistr('\000a') || 
                    '						<font size="18" isBold="true"/>' || unistr('\000a') || 
                    '						<titleExpression><![CDATA["A timeseries-linechart"]]></titleExpression>' || unistr('\000a') || 
                    '					</chartTitle>' || unistr('\000a') || 
                    '					<chartSubtitle/>' || unistr('\000a') || 
                    '					<chartLege';

  v_data := v_data || 'nd/>' || unistr('\000a') || 
                    '				</chart>' || unistr('\000a') || 
                    '				<timeSeriesDataset>' || unistr('\000a') || 
                    '					<dataset>' || unistr('\000a') || 
                    '						<datasetRun subDataset="Chartdata_Date" uuid="270669f5-8f19-45d4-971a-43b85f079e05"/>' || unistr('\000a') || 
                    '					</dataset>' || unistr('\000a') || 
                    '					<timeSeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Male income"]]></seriesExpression>' || unistr('\000a') || 
                    '						<timePeriodExpression><![CDATA[$F{BIRTHDAY}]]></timePeriodExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{INCOME_MALE}]]></valueExpression>' || unistr('\000a') || 
                    '					</timeSeries>' || unistr('\000a') || 
                    '					<timeSeries>' || unistr('\000a') || 
                    '						<seriesExpression><![CDATA["Female income"]]></seriesExpression>' || unistr('\000a') || 
                    '						<';

  v_data := v_data || 'timePeriodExpression><![CDATA[$F{BIRTHDAY}]]></timePeriodExpression>' || unistr('\000a') || 
                    '						<valueExpression><![CDATA[$F{INCOME_FEMALE}]]></valueExpression>' || unistr('\000a') || 
                    '					</timeSeries>' || unistr('\000a') || 
                    '				</timeSeriesDataset>' || unistr('\000a') || 
                    '				<timeSeriesPlot isShowLines="false">' || unistr('\000a') || 
                    '					<plot/>' || unistr('\000a') || 
                    '				</timeSeriesPlot>' || unistr('\000a') || 
                    '			</timeSeriesChart>' || unistr('\000a') || 
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
  50,
  'demo_charts',
  'A demo of the currently available chart-types',
  v_data
);
  dbms_lob.freetemporary(v_data);
end;
/

