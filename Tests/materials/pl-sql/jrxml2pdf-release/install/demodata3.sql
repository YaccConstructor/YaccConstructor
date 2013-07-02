declare
  v_data clob;
begin
  dbms_lob.createtemporary(v_data, true, DBMS_LOB.SESSION);
  dbms_lob.open(v_data, dbms_lob.LOB_READWRITE);

  v_data := v_data || '<?xml version="1.0" encoding="UTF-8"?>' || unistr('\000a') || 
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="crosstab3" language="groovy" pageWidth="842" pageHeight="595" orientation="Landscape" columnWidth="802" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="e0d6b4f1-0bb0-4b9e-b632-c6b43e9e86';

  v_data := v_data || 'cc">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Crosstab Data Text" hAlign="Center" vAlign="Middle"/>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[WITH ALPHA AS (SELECT CHR(64+LEVEL) ALPHA,' || unistr('\000a') || 
                    '                      LEVEL-1       NUM_ALPHA' || unistr('\000a') || 
                    '                 FROM DUAL CONNECT BY LEVEL<=8' || unistr('\000a') || 
                    '              ),' || unistr('\000a') || 
                    '     NUMS  AS (SELECT TO_CHAR(LEVEL) NUM,' || unistr('\000a') || 
                    '                      LEVEL          NUM_NUM' || unistr('\000a') || 
                    '                 FROM DUAL CONNECT BY LEVEL<=8' || unistr('\000a') || 
                    ' ';

  v_data := v_data || '             ),' || unistr('\000a') || 
                    '     MATRIX AS(SELECT ALPHA,' || unistr('\000a') || 
                    '                      NUM,' || unistr('\000a') || 
                    '                      NUM_NUM+NUM_ALPHA*8 VALUE' || unistr('\000a') || 
                    '                 FROM ALPHA,' || unistr('\000a') || 
                    '                      NUMS' || unistr('\000a') || 
                    '              )' || unistr('\000a') || 
                    'SELECT ''A-H'' AH,' || unistr('\000a') || 
                    '       CASE WHEN ALPHA IN (''A'', ''B'', ''C'', ''D'') THEN' || unistr('\000a') || 
                    '         ''A-D''' || unistr('\000a') || 
                    '       ELSE' || unistr('\000a') || 
                    '         ''E-H''' || unistr('\000a') || 
                    '       END AD_EH,' || unistr('\000a') || 
                    '       CASE WHEN ALPHA IN (''A'', ''B'') THEN' || unistr('\000a') || 
                    '         ''A-B''' || unistr('\000a') || 
                    '       WHEN ALPHA IN (''C'', ''D'') THEN' || unistr('\000a') || 
                    '         ''C-D''' || unistr('\000a') || 
                    '       WHEN ALPHA IN (''E'', ''F'') THEN' || unistr('\000a') || 
                    '         ''E-F''' || unistr('\000a') || 
                    '       ELSE' || unistr('\000a') || 
                    '         ''G-H''' || unistr('\000a') || 
                    '   ';

  v_data := v_data || '    END AB_CD_EF_GH,' || unistr('\000a') || 
                    '       ALPHA,' || unistr('\000a') || 
                    '       ''1-8'' ONE_EIGHT,' || unistr('\000a') || 
                    '       CASE WHEN NUM IN (''1'', ''2'', ''3'', ''4'') THEN' || unistr('\000a') || 
                    '         ''1-4''' || unistr('\000a') || 
                    '       ELSE' || unistr('\000a') || 
                    '         ''5-8''' || unistr('\000a') || 
                    '       END ONEFOUR_FIVEEIGHT,' || unistr('\000a') || 
                    '       CASE WHEN NUM IN (''1'', ''2'') THEN' || unistr('\000a') || 
                    '         ''1-2''' || unistr('\000a') || 
                    '       WHEN NUM IN (''3'', ''4'') THEN' || unistr('\000a') || 
                    '         ''3-4''' || unistr('\000a') || 
                    '       WHEN NUM IN (''5'', ''6'') THEN' || unistr('\000a') || 
                    '         ''5-6''' || unistr('\000a') || 
                    '       ELSE' || unistr('\000a') || 
                    '         ''7-8''' || unistr('\000a') || 
                    '       END ONETWO_THREEFOUR_FIVESIX_SEVEN,' || unistr('\000a') || 
                    '       NUM,' || unistr('\000a') || 
                    '       VALUE' || unistr('\000a') || 
                    '  FROM MATRIX]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="AH" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<f';

  v_data := v_data || 'ield name="AD_EH" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="AB_CD_EF_GH" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="ALPHA" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="ONE_EIGHT" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="ONEFOUR_FIVEEIGHT" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="ONETWO_THREEFOUR_FIVESIX_SEVEN" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="NUM" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="VALUE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height';

  v_data := v_data || '="48" splitType="Stretch">' || unistr('\000a') || 
                    '			<staticText>' || unistr('\000a') || 
                    '				<reportElement uuid="d8723acd-138d-422d-aed3-b0c68675c75c" x="0" y="0" width="802" height="48"/>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="14" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<text><![CDATA[Multi-group-crosstab, grouping the fields from a chess-game (A-H, 1-8) in three levels. The measure-base starts with 1 on A-1 and ends with 64 on H-8]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<columnHeader>' || unistr('\000a') || 
                    '		<';

  v_data := v_data || 'band splitType="Stretch"/>' || unistr('\000a') || 
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
                    '		<band height="203" splitType="Stretch">' || unistr('\000a') || 
                    '			<crosstab>' || unistr('\000a') || 
                    '				<reportElement uuid="588cc748-db00-423b-b3c7-3b5082a21d62" x="0" y="0" width="802" height="203"/>' || unistr('\000a') || 
                    '				<rowGroup name="AH" width="70" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDAT';

  v_data := v_data || 'A[$F{AH}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#F0F8FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="8058810a-500c-46dc-8696-2f45e55f1160" style="Crosstab Data Text" x="0" y="0" width="70" height="25"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{AH}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					';

  v_data := v_data || '</crosstabRowHeader>' || unistr('\000a') || 
                    '					<crosstabTotalRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="fc946a42-835f-451b-bb53-b2e28a6541e6" x="0" y="0" width="140" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total AH]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</';

  v_data := v_data || 'crosstabTotalRowHeader>' || unistr('\000a') || 
                    '				</rowGroup>' || unistr('\000a') || 
                    '				<rowGroup name="AD_EH" width="70" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{AD_EH}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#F0F8FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="b378cfb0-656c-4411-afcb-9abadd4f6870" style="Crosstab Data Text" x="0"';

  v_data := v_data || ' y="0" width="70" height="25"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{AD_EH}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabRowHeader>' || unistr('\000a') || 
                    '					<crosstabTotalRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="bf503c4b-3396-4bf6-9eeb-2d44f88ba05e" x="0" y="0" width="70" height="25"/>' || unistr('\000a') || 
                    '								<te';

  v_data := v_data || 'xtElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total AD_EH]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalRowHeader>' || unistr('\000a') || 
                    '				</rowGroup>' || unistr('\000a') || 
                    '				<rowGroup name="AB_CD_EF_GH" width="100" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{AB_CD_EF_GH}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabRowHeader>' || unistr('\000a') || 
                    '						<cellContents>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="ec8cf8b7-eaec-4cbb-98ae-eb5018';

  v_data := v_data || 'ab5557" style="Crosstab Data Text" x="0" y="0" width="100" height="25"/>' || unistr('\000a') || 
                    '								<box>' || unistr('\000a') || 
                    '									<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								</box>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{AB_CD_EF_GH}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabRowHeader>' || unistr('\000a') || 
                    '					<crosstabTotalRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcol';

  v_data := v_data || 'or="#CCCCFF" mode="Opaque">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="7a2fa206-13a8-406b-9548-c5a9fe2b7d78" style="Crosstab Data Text" x="0" y="0" width="100" height="25"/>' || unistr('\000a') || 
                    '								<textElement markup="none"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total AB_CD_EF_GH]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalRowHeader>' || unistr('\000a') || 
                    '				</rowGroup>' || unistr('\000a') || 
                    '				<columnGroup name="ONE_EIGHT" height="30" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{ONE_EIGHT}]]';

  v_data := v_data || '></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#F0F8FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="e13c6218-53ec-4c23-8365-a1408b925e8e" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{ONE_EIGHT}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					';

  v_data := v_data || '</crosstabColumnHeader>' || unistr('\000a') || 
                    '					<crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="0c1ba967-471f-47ec-8b26-bdc615cf510c" x="0" y="0" width="50" height="60" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total ONE_EIGHT]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellConte';

  v_data := v_data || 'nts>' || unistr('\000a') || 
                    '					</crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '				</columnGroup>' || unistr('\000a') || 
                    '				<columnGroup name="ONEFOUR_FIVEEIGHT" height="30" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{ONEFOUR_FIVEEIGHT}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#F0F8FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="4acd3b57-68e2-4361-be';

  v_data := v_data || 'e5-f5cbe1667671" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{ONEFOUR_FIVEEIGHT}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabColumnHeader>' || unistr('\000a') || 
                    '					<crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="c9e27448-8cf3-4a9f-';

  v_data := v_data || '9caa-465489c7e9b4" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total ONEFOUR_FIVEEIGHT]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '				</columnGroup>' || unistr('\000a') || 
                    '				<columnGroup name="ONETWO_THREEFOUR_FIVESIX_SEVEN" height="30" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{ONETWO_THREEFOUR_FIVESIX_SEVEN}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket';

  v_data := v_data || '>' || unistr('\000a') || 
                    '					<crosstabColumnHeader>' || unistr('\000a') || 
                    '						<cellContents>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="810277ac-1ac1-4cdf-a72c-ae66db2cdf9f" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<box>' || unistr('\000a') || 
                    '									<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '									<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								</box>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{ONETWO_THREEFOUR_FIVESIX_SEVEN}]]><';

  v_data := v_data || '/textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabColumnHeader>' || unistr('\000a') || 
                    '					<crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#CCCCFF" mode="Opaque">' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="671e1025-6e3a-4c92-a7fc-a203c111ee70" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement markup="none"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total ]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '				</columnGroup>' || unistr('\000a') || 
                    '				<me';

  v_data := v_data || 'asure name="ALPHAMeasure" class="java.lang.Integer" calculation="Sum">' || unistr('\000a') || 
                    '					<measureExpression><![CDATA[$F{VALUE}]]></measureExpression>' || unistr('\000a') || 
                    '				</measure>' || unistr('\000a') || 
                    '				<crosstabCell width="50" height="25">' || unistr('\000a') || 
                    '					<cellContents>' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="4bf91319-2c49-4946-b70f-0287bcda0bd7" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDAT';

  v_data := v_data || 'A[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell height="25" rowTotalGroup="AH">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="4f4f56af-38ff-43a4-926d-0ef3b3001d2d" style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpr';

  v_data := v_data || 'ession><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell width="50" height="25" columnTotalGroup="ONE_EIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="44161833-9177-4bf7-8ae3-e6f1e31d6683" style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<';

  v_data := v_data || 'textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AH" columnTotalGroup="ONE_EIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="d9284c09-3aae-4ba5-9600-31106b087be4" style="Crosstab Data Text" x="0" y="0" width="50" height="25"';

  v_data := v_data || ' forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell width="50" height="25" columnTotalGroup="ONEFOUR_FIVEEIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="0faeb1ce-81ae-4873-9b2d-7f06d19f9100" style="Crosstab Da';

  v_data := v_data || 'ta Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AH" columnTotalGroup="ONEFOUR_FIVEEIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="13990aaf-2e31-43e0-8a83-ab06e1c8fc4b"';

  v_data := v_data || ' style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell height="25" rowTotalGroup="AD_EH">' || unistr('\000a') || 
                    '					<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="42b63f77-b831-4017-a';

  v_data := v_data || 'eaf-30d8e651c000" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AD_EH" columnTotalGroup="ONE_EIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="a9fb65';

  v_data := v_data || 'a3-f316-4a0e-842c-7b0596178e73" style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AD_EH" columnTotalGroup="ONEFOUR_FIVEEIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<tex';

  v_data := v_data || 'tField>' || unistr('\000a') || 
                    '							<reportElement uuid="a184457c-d80d-4c13-ab7e-28c472a91b18" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AB_CD_EF_GH">' || unistr('\000a') || 
                    '					<cellContents backcolor="#CCCCFF" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="e5b21ea7-5136-4223-a80c-b26ecf33e091" style="Crosstab Data Text" x';

  v_data := v_data || '="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AB_CD_EF_GH" columnTotalGroup="ONE_EIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005';

  v_data := v_data || 'FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="1e1c554a-61c1-47ab-9c78-b4b141e31633" style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>';

  v_data := v_data || '' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AB_CD_EF_GH" columnTotalGroup="ONEFOUR_FIVEEIGHT">' || unistr('\000a') || 
                    '					<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="c4df8127-bfef-4971-bcd0-02b6ba8dff95" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0';

  v_data := v_data || '.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell height="25" columnTotalGroup="ONETWO_THREEFOUR_FIVESIX_SEVEN">' || unistr('\000a') || 
                    '					<cellContents backcolor="#CCCCFF" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="34cee795-77d3-4e6b-8958-5325706580e1" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '		';

  v_data := v_data || '						<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AH" columnTotalGroup="ONETWO_THREEFOUR_FIVESIX_SEVEN">' || unistr('\000a') || 
                    '					<cellContents backcolor="#005FB3" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="a1fc8';

  v_data := v_data || '70c-0891-4253-9a9f-f701fbebd14d" style="Crosstab Data Text" x="0" y="0" width="50" height="25" forecolor="#FFFFFF"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGro';

  v_data := v_data || 'up="AD_EH" columnTotalGroup="ONETWO_THREEFOUR_FIVESIX_SEVEN">' || unistr('\000a') || 
                    '					<cellContents backcolor="#BFE1FF" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="e51e6c0d-f955-49a4-acc8-3e2bee09d6ba" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpr';

  v_data := v_data || 'ession><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell rowTotalGroup="AB_CD_EF_GH" columnTotalGroup="ONETWO_THREEFOUR_FIVESIX_SEVEN">' || unistr('\000a') || 
                    '					<cellContents backcolor="#CCCCFF" mode="Opaque">' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="26d841d5-d263-4373-a920-722ec3c9bd53" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<topPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<leftPe';

  v_data := v_data || 'n lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<bottomPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '								<rightPen lineWidth="0.25"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ALPHAMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '			</crosstab>' || unistr('\000a') || 
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
  JRXML_SEQ.NEXTVAL,
  'crosstab_chess',
  'Crosstab with several groups, expressing data from a chess-game with 1 one the first field and 64 on the last field',
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
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="crosstab_daterow" language="groovy" pageWidth="595" pageHeight="842" columnWidth="555" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" uuid="09d50ff3-e527-4e77-a499-01330d68354c">' || unistr('\000a') || 
                    '	<property n';

  v_data := v_data || 'ame="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<style name="Crosstab Data Text" hAlign="Center"/>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[select CUST_STATE,' || unistr('\000a') || 
                    '       ORDER_TIMESTAMP MONTH,' || unistr('\000a') || 
                    '       ORDER_TOTAL' || unistr('\000a') || 
                    '  from demo_orders do,' || unistr('\000a') || 
                    '       demo_customers dc' || unistr('\000a') || 
                    'where do.customer_id=dc.customer_id]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="CUST_STATE" class="java.lang.String"/>' || unistr('\000a') || 
                    '	<field name="MONTH" class="java.sql.Timestamp"/>' || unistr('\000a') || 
                    '	<field name="ORDER_TOTAL" class="java.math.Big';

  v_data := v_data || 'Decimal"/>' || unistr('\000a') || 
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
                    '				<text><![CDATA[Crosstab-example, showing the amount of all demo_orders by date and state]]></text>' || unistr('\000a') || 
                    '			</staticText>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<';

  v_data := v_data || 'columnHeader>' || unistr('\000a') || 
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
                    '				<reportElement uuid="5996be5e-d29a-45fb-bdd0-5c47cb22b1fa" x="0" y="0" width="555" height="90"/>' || unistr('\000a') || 
                    '				<rowGroup name="MONTH" width="70" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.sql.Timestamp">' || unistr('\000a') || 
                    '						<buck';

  v_data := v_data || 'etExpression><![CDATA[$F{MONTH}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#00FFA9" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="d8279ab5-2b3a-41c2-ba40-9c0a6eaf2270" style="Crosstab Data Text" x="0" y="0" width="70" height="25"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{MONTH}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '	';

  v_data := v_data || '					</cellContents>' || unistr('\000a') || 
                    '					</crosstabRowHeader>' || unistr('\000a') || 
                    '					<crosstabTotalRowHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="64f6abb7-b174-453a-94c3-501cbf994c62" x="0" y="0" width="70" height="35"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total for all months]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cell';

  v_data := v_data || 'Contents>' || unistr('\000a') || 
                    '					</crosstabTotalRowHeader>' || unistr('\000a') || 
                    '				</rowGroup>' || unistr('\000a') || 
                    '				<columnGroup name="CUST_STATE" height="30" totalPosition="End">' || unistr('\000a') || 
                    '					<bucket class="java.lang.String">' || unistr('\000a') || 
                    '						<bucketExpression><![CDATA[$F{CUST_STATE}]]></bucketExpression>' || unistr('\000a') || 
                    '					</bucket>' || unistr('\000a') || 
                    '					<crosstabColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#00FFA9" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<textField>' || unistr('\000a') || 
                    '								<reportElement uuid="678c2c42-602f-4a47-bd46-ee05895acba9';

  v_data := v_data || '" style="Crosstab Data Text" x="0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement/>' || unistr('\000a') || 
                    '								<textFieldExpression><![CDATA[$V{CUST_STATE}]]></textFieldExpression>' || unistr('\000a') || 
                    '							</textField>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabColumnHeader>' || unistr('\000a') || 
                    '					<crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '						<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '							<box>' || unistr('\000a') || 
                    '								<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '							</box>' || unistr('\000a') || 
                    '							<staticText>' || unistr('\000a') || 
                    '								<reportElement uuid="29a2d0cc-ae45-493a-a718-72f69e6a13fd" x="';

  v_data := v_data || '0" y="0" width="50" height="30"/>' || unistr('\000a') || 
                    '								<textElement textAlignment="Center" verticalAlignment="Middle"/>' || unistr('\000a') || 
                    '								<text><![CDATA[Total for all state]]></text>' || unistr('\000a') || 
                    '							</staticText>' || unistr('\000a') || 
                    '						</cellContents>' || unistr('\000a') || 
                    '					</crosstabTotalColumnHeader>' || unistr('\000a') || 
                    '				</columnGroup>' || unistr('\000a') || 
                    '				<measure name="ORDER_TOTALMeasure" class="java.math.BigDecimal" calculation="Sum">' || unistr('\000a') || 
                    '					<measureExpression><![CDATA[$F{ORDER_TOTAL}]]></measureExpression>' || unistr('\000a') || 
                    '				</measure>' || unistr('\000a') || 
                    '				<crosstabCell width="50" height="25">' || unistr('\000a') || 
                    '					<cellContents>' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<';

  v_data := v_data || 'pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="ff31de81-d067-4f41-bb56-f79189bb6707" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell height="35" rowTotalGroup="MONTH">' || unistr('\000a') || 
                    '					<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '					';

  v_data := v_data || '		<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="dc425f5c-eb84-4ef5-820a-9da16a6e8187" style="Crosstab Data Text" x="0" y="0" width="50" height="35"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell width="50" columnTotalGroup="CUST_STATE">' || unistr('\000a') || 
                    '					<cellContents backcolor="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<';

  v_data := v_data || 'box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="5f43475d-c104-4d33-9655-088f28564ce2" style="Crosstab Data Text" x="0" y="0" width="50" height="25"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '				<crosstabCell height="35" rowTotalGroup="MONTH" columnTotalGroup="CUST_STATE">' || unistr('\000a') || 
                    '					<cellContents backcolor';

  v_data := v_data || '="#E6FFF6" mode="Opaque">' || unistr('\000a') || 
                    '						<box>' || unistr('\000a') || 
                    '							<pen lineWidth="0.5" lineStyle="Solid" lineColor="#000000"/>' || unistr('\000a') || 
                    '						</box>' || unistr('\000a') || 
                    '						<textField>' || unistr('\000a') || 
                    '							<reportElement uuid="70f9b929-df82-4d38-af1a-95260df3654e" style="Crosstab Data Text" x="0" y="0" width="50" height="35"/>' || unistr('\000a') || 
                    '							<textElement/>' || unistr('\000a') || 
                    '							<textFieldExpression><![CDATA[$V{ORDER_TOTALMeasure}]]></textFieldExpression>' || unistr('\000a') || 
                    '						</textField>' || unistr('\000a') || 
                    '					</cellContents>' || unistr('\000a') || 
                    '				</crosstabCell>' || unistr('\000a') || 
                    '			</crosstab>' || unistr('\000a') || 
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
  JRXML_SEQ.NEXTVAL,
  'crosstab_orders',
  'Crosstab showing the sum of all demo_orders in columns by state and in rows by date',
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
                    '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="nls_demo" language="groovy" pageWidth="595" pageHeight="842" columnWidth="555" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20" resourceBundle="nls_demo" uuid="210db4a2-f605-4848-ad8c-9a2254b8b';

  v_data := v_data || '0d3">' || unistr('\000a') || 
                    '	<property name="ireport.zoom" value="1.0"/>' || unistr('\000a') || 
                    '	<property name="ireport.x" value="0"/>' || unistr('\000a') || 
                    '	<property name="ireport.y" value="0"/>' || unistr('\000a') || 
                    '	<queryString>' || unistr('\000a') || 
                    '		<![CDATA[SELECT 1          INT_VALUE,' || unistr('\000a') || 
                    '       1234.5     FLOAT_VALUE,' || unistr('\000a') || 
                    '       TO_DATE(''21.01.2012 13:44:55'', ''DD.MM.YYYY HH24:MI:SS'') DATE_VALUE' || unistr('\000a') || 
                    '  FROM DUAL]]>' || unistr('\000a') || 
                    '	</queryString>' || unistr('\000a') || 
                    '	<field name="INT_VALUE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="FLOAT_VALUE" class="java.math.BigDecimal"/>' || unistr('\000a') || 
                    '	<field name="DATE_VALUE" class="java.sql.Timestamp"/>' || unistr('\000a') || 
                    '	<background>' || unistr('\000a') || 
                    '		<band';

  v_data := v_data || ' splitType="Stretch"/>' || unistr('\000a') || 
                    '	</background>' || unistr('\000a') || 
                    '	<title>' || unistr('\000a') || 
                    '		<band height="30" splitType="Stretch">' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="44dc2e06-6055-422f-8159-b72f9e3b268e" x="0" y="0" width="553" height="28"/>' || unistr('\000a') || 
                    '				<textElement>' || unistr('\000a') || 
                    '					<font size="12" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[msg($R{header.text}, $P{REPORT_LOCALE})]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '		</band>' || unistr('\000a') || 
                    '	</title>' || unistr('\000a') || 
                    '	<pageHeader>' || unistr('\000a') || 
                    '		<band splitType="Stretch"/>' || unistr('\000a') || 
                    '	</pageHeader>' || unistr('\000a') || 
                    '	<columnHeader>' || unistr('\000a') || 
                    '		<band splitType="Stret';

  v_data := v_data || 'ch"/>' || unistr('\000a') || 
                    '	</columnHeader>' || unistr('\000a') || 
                    '	<detail>' || unistr('\000a') || 
                    '		<band height="268" splitType="Stretch">' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="4a7be9ec-270f-4110-8bf5-41c9cddfef59" x="0" y="23" width="155" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{int.no.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="e34e7d3c-1460-4960-91af-2dba6ab88f23" x="157" y="23" width="142" height="20"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[';

  v_data := v_data || '$F{INT_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="0d8f1158-36ed-486b-af59-1878cde961a9" x="0" y="45" width="155" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{float.no.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="ccbf22d1-f755-45b4-b00c-ea008c965979" x="157" y="45" width="142" height="20"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{FLOAT_VALUE}]]>';

  v_data := v_data || '</textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="d7b762ca-3d95-4f17-823b-06e7e8a3b5d8" x="0" y="96" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.no.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="96" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '	';

  v_data := v_data || '		<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="4ee24e72-ed50-435c-8d1b-22f3ec52a10e" x="311" y="23" width="121" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{int.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="2eecd167-d6e1-49a9-a40c-195aad42775d" x="311" y="45" width="121" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{float.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField pattern="###0';

  v_data := v_data || '000">' || unistr('\000a') || 
                    '				<reportElement uuid="e34e7d3c-1460-4960-91af-2dba6ab88f23" x="432" y="25" width="121" height="20"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{INT_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField pattern="#,###,000.000">' || unistr('\000a') || 
                    '				<reportElement uuid="ccbf22d1-f755-45b4-b00c-ea008c965979" x="432" y="45" width="123" height="20"/>' || unistr('\000a') || 
                    '				<textElement textAlignment="Right"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{FLOAT_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textFiel';

  v_data := v_data || 'd>' || unistr('\000a') || 
                    '			<textField pattern="short,short">' || unistr('\000a') || 
                    '				<reportElement uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="116" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="2c840db4-d33e-4dcf-b3ae-68492e8f84de" x="0" y="116" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.short.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textFi';

  v_data := v_data || 'eld pattern="medium,medium">' || unistr('\000a') || 
                    '				<reportElement uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="136" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="0c31b682-af75-4c4d-9464-2f55239ffbbd" x="0" y="136" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.medium.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField patter';

  v_data := v_data || 'n="long,long">' || unistr('\000a') || 
                    '				<reportElement uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="156" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="e5644cae-549e-4630-a7d8-9c274aba36d7" x="0" y="156" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.long.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField pattern="default,defau';

  v_data := v_data || 'lt">' || unistr('\000a') || 
                    '				<reportElement uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="176" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="9d9540f4-416d-40d8-9b87-1cd2540b2270" x="0" y="176" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.default.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField pattern="">' || unistr('\000a') || 
                    '				<reportElemen';

  v_data := v_data || 't uuid="5e8984a2-8abc-46f8-8ff2-e7d4de0526da" x="198" y="196" width="209" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$F{DATE_VALUE}]]></textFieldExpression>' || unistr('\000a') || 
                    '				<patternExpression><![CDATA[$R{dateformat}]]></patternExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="0db24941-97e4-40b8-bc7d-fc4a0d859ccd" x="0" y="196" width="198" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none"/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{date.resource.format}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textF';

  v_data := v_data || 'ield>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="12249994-6124-4b43-a103-2fd0ef863a14" x="0" y="247" width="551" height="20"/>' || unistr('\000a') || 
                    '				<textElement/>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[msg($R{text.for.msg}, "xx1", "xx2")]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="4abe1164-7273-49bf-92bc-4070c316550e" x="0" y="3" width="553" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none">' || unistr('\000a') || 
                    '					<font size="12" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{numbers.text}]]><';

  v_data := v_data || '/textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="00c44628-0f43-44c5-9287-b3de714c186d" x="2" y="76" width="553" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none">' || unistr('\000a') || 
                    '					<font size="12" isBold="true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{dates.text}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
                    '			<textField>' || unistr('\000a') || 
                    '				<reportElement uuid="bb221b70-57ef-4cca-a666-4f221c963b1e" x="2" y="226" width="553" height="20"/>' || unistr('\000a') || 
                    '				<textElement markup="none">' || unistr('\000a') || 
                    '					<font size="12" isBold="';

  v_data := v_data || 'true"/>' || unistr('\000a') || 
                    '				</textElement>' || unistr('\000a') || 
                    '				<textFieldExpression><![CDATA[$R{msg.text}]]></textFieldExpression>' || unistr('\000a') || 
                    '			</textField>' || unistr('\000a') || 
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
  JRXML_SEQ.NEXTVAL,
  'nls_demo',
  'Demo for NLS-support, pass the parameter REPORT_LOCALE when calling the report, resources exist for de_DE and en_US',
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

  v_data := v_data || 'header.text=report to show the nls/locale-functionality. The locale used  {0}, resource-locale is empty (default)' || unistr('\000a') || 
                    'numbers.text=Numbers with and without formatmask' || unistr('\000a') || 
                    'dates.text=Dates with and without formatmask' || unistr('\000a') || 
                    'msg.text=Msg-usage' || unistr('\000a') || 
                    'int.no.format=Integer, no format mask' || unistr('\000a') || 
                    'float.no.format=Float, no format' || unistr('\000a') || 
                    'int.format=Integer, with format mask' || unistr('\000a') || 
                    'float.format=Float, with format' || unistr('\000a') || 
                    'date.no.format=Date, no format mask' || unistr('\000a') || 
                    'date.short.format=Date, format short,short' || unistr('\000a') || 
                    'date.medium.format=Date, format medium,medium' || unistr('\000a') || 
                    'date.long.format=Dat';

  v_data := v_data || 'e, format long,long' || unistr('\000a') || 
                    'date.default.format=Date, format default,default' || unistr('\000a') || 
                    'date.resource.format=Date, format from ressourcefile' || unistr('\000a') || 
                    'dateformat=dd/MM/yyyy' || unistr('\000a') || 
                    'text.for.msg=This is the text which contains a placeholder here {0} and here {1}' || unistr('\000a') || 
                    '';

INSERT INTO JRXML_RESOURCE_FILES ( 
  JRR_ID,
  JRR_NAME,
  JRR_LOCALE,
  JRR_CONTENT
) VALUES (
  JRXML_SEQ.NEXTVAL,
  'nls_demo',
  NULL,
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

  v_data := v_data || 'header.text=Report zur Demonstration von ' || unistr('NLS/Locale-Funktionalit\00e4t.') || ' Reportaufruf erfolgte mit {0}, resource-locale ist de_DE' || unistr('\000a') || 
                    'numbers.text=Zahlen mit und ohne Formatmaske' || unistr('\000a') || 
                    'dates.text=Datumswerte mit und ohne Formatmaske' || unistr('\000a') || 
                    'msg.text=Verwendung von msg' || unistr('\000a') || 
                    'int.no.format=Integer, ohne Formatmaske' || unistr('\000a') || 
                    'float.no.format=Float, ohne Formatmaske' || unistr('\000a') || 
                    'int.format=Integer, mit Formatmaske' || unistr('\000a') || 
                    'float.format=Float, mit Formatmaske' || unistr('\000a') || 
                    'date.no.format=Datum, ohne Formatmaske' || unistr('\000a') || 
                    'date.short.format=Datum, mit Formatmaske short,short' || unistr('\000a') || 
                    'date.medium.format=Dat';

  v_data := v_data || 'um, mit Formatmaske medium,medium' || unistr('\000a') || 
                    'date.long.format=Datum, mit Formatmaske long,long' || unistr('\000a') || 
                    'date.default.format=Datum, mit Formatmaske default,default' || unistr('\000a') || 
                    'date.resource.format=Datum, mit Formatmaske aus Resource-file' || unistr('\000a') || 
                    'dateformat=dd.MM.yyyy' || unistr('\000a') || 
                    'text.for.msg=Dies ist ein Text mit einem Platzhalter hier {0} und einem hier {1}' || unistr('\000a') || 
                    '';

INSERT INTO JRXML_RESOURCE_FILES ( 
  JRR_ID,
  JRR_NAME,
  JRR_LOCALE,
  JRR_CONTENT
) VALUES (
  JRXML_SEQ.NEXTVAL,
  'nls_demo',
  'de_DE',
  v_data
);
  dbms_lob.freetemporary(v_data);
end;
/

