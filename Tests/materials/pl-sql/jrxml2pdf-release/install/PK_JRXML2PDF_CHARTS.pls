create or replace
PACKAGE PK_JRXML2PDF_CHARTS AUTHID CURRENT_USER IS

/* *****************************************************************************

Copyright (C) 2012-2013 by Andreas Weiden

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

****************************************************************************** */


  /** ******************************************************************************

      The algorithms in the function FK_CALC_PIESLICE where adopted from
      the java-class EllipticalArc witten by Luc Maisonobe.

      Here the original copyright-text from that class:


      // Copyright (c) 2003-2004, Luc Maisonobe
      // All rights reserved.
      //
      // Redistribution and use in source and binary forms, with
      // or without modification, are permitted provided that
      // the following conditions are met:
      //
      //    Redistributions of source code must retain the
      //    above copyright notice, this list of conditions and
      //    the following disclaimer.
      //    Redistributions in binary form must reproduce the
      //    above copyright notice, this list of conditions and
      //    the following disclaimer in the documentation
      //    and/or other materials provided with the
      //    distribution.
      //    Neither the names of spaceroots.org, spaceroots.com
      //    nor the names of their contributors may be used to
      //    endorse or promote products derived from this
      //    software without specific prior written permission.
      //
      // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
      // CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
      // WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
      // WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
      // PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
      // THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
      // DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
      // CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
      // PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
      // USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
      // HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
      // IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
      // NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
      // USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
      // POSSIBILITY OF SUCH DAMAGE.
  ******************************************************************************/

/**

  $name    PK_JRXML2PDF_CHARTS

  $created 30.10.2012

  $author  Andreas Weiden

  $desc    package for the creation of chart-diagrams into a PDF-document
           created by AS_PDF3_MOD.

           It can be used standalone with AS_PDF3_MOD for "handmade" PDF's,
           but its also part of the PK_JRXML2PDF-engine to render charts into
           PDF's generated with PK_JRXML2PDF_REPGEN

  $version 0.1.0.0 30.10.2012 Weiden
           initial draft-version

  $version 0.1.1.0 25.11.2012 Weiden
           initial version

  $version 1.0.0.0 28.12.2012 Weiden
           First release-version

*/

  SUBTYPE tTitle          IS VARCHAR2(2000);
  SUBTYPE tName           IS VARCHAR2(2000);
  SUBTYPE tTitlePosition  IS PK_JRXML2PDF_TYPES.tPosition;
  SUBTYPE tAxisType       IS NUMBER;
  SUBTYPE tValueType      IS NUMBER;
  SUBTYPE tOrientation    IS NUMBER;
  SUBTYPE tLegendPosition IS PK_JRXML2PDF_TYPES.tPosition;
  SUBTYPE tValuePosition  IS NUMBER;

  TYPE tPaddings IS RECORD (
    nLeft   NUMBER,
    nTop    NUMBER,
    nRight  NUMBER,
    nBottom NUMBER
  );

  TYPE tDataEntry IS RECORD (
    nValue  NUMBER,
    vcValue PK_JRXML2PDF_TYPES.tMaxVarchar2,
    dtValue DATE
  );

  TYPE tDataEntryList IS TABLE OF tdataEntry INDEX BY BINARY_INTEGER;

  TYPE tTickEntry IS RECORD (
    rDataEntry   tDataEntry,
    vcPattern    PK_JRXML2PDF_TYPES.tPattern,
    vcUnit       VARCHAR2(20)
  );

  TYPE tTickEntryList IS TABLE OF tTickEntry INDEX BY PK_JRXML2PDF_TYPES.tMaxVarchar2;

  TYPE tAxis IS RECORD (
    nAxisType            tAxisType,
    nValueType           tValueType,
    nOrientation         tOrientation,
    nPosition            NUMBER,
    nMinValueType        NUMBER,
    nMaxValueType        NUMBER,
    nAdditionalPercent   NUMBER,
    nHeightType          NUMBER,
    nWidthType           NUMBER,
    vcInclude0Value      PK_JRXML2PDF_TYPES.tYesNo,
    vcDatatype           PK_JRXML2PDF_TYPES.tDatatype,
    nMinMaxDiff          NUMBER,
    nPixelPerUnit        NUMBER,
    rMinValueDataEntry   tDataEntry,
    rMaxValueDataEntry   tDataEntry,
    nHeight              NUMBER,
    nWidth               NUMBER,
    nX                   NUMBER,
    nY                   NUMBER,
    vcDotLineColor       PK_JRXML2PDF_TYPES.tColor,
    bAutoTick            BOOLEAN,
    nTickSpace           NUMBER,
    nTickOffset          NUMBER,
    lTickEntries         tTickEntryList,
    vcTickLabelPattern   PK_JRXML2PDF_TYPES.tPattern,
    vcShowTickLabels     PK_JRXML2PDF_TYPES.tYesNo,
    vcShowTickMarks      PK_JRXML2PDF_TYPES.tYesNo,
    vcShowDotlines       PK_JRXML2PDF_TYPES.tYesNo,
    vcLabelText          PK_JRXML2PDF_TYPES.tExpression,
    vcLabelColor         PK_JRXML2PDF_TYPES.tColor,
    vcLabelFont          PK_JRXML2PDF_TYPES.tFont,
    vcLabelFontStyle     PK_JRXML2PDF_TYPES.tFontStyle,
    nLabelFontSize       NUMBER,
    vcTickLabelColor     PK_JRXML2PDF_TYPES.tColor,
    vcTickLabelFont      PK_JRXML2PDF_TYPES.tFont,
    vcTickLabelFontStyle PK_JRXML2PDF_TYPES.tFontStyle,
    nTickLabelFontSize   NUMBER,
    nTicklabelRotation   NUMBER,
    vcLineColor          PK_JRXML2PDF_TYPES.tColor,
    nAxisLineWidth       NUMBER,
    nStartOffset         NUMBER,
    nEndOffset           NUMBER,
    nDataOffset          NUMBER,
    nTickLabelOffset     NUMBER
  );

  TYPE tAxisList IS TABLE OF tAxis INDEX BY BINARY_INTEGER;

  TYPE tDataset IS RECORD (
    vcValueType        PK_JRXML2PDF_TYPES.tDatatype,
    vcRangeType        PK_JRXML2PDF_TYPES.tDatatype,
    nGroup            NUMBER,
    nSeries           NUMBER,
    nValueAxis        NUMBER,
    nRangeAxis        NUMBER,
    lValueDataEntries tDataEntryList,
    lRangeDataEntries tDataEntryList
  );

  TYPE tDatasetList IS TABLE OF tDataset INDEX BY BINARY_INTEGER;

  TYPE tSeries IS RECORD (
    vcColor       PK_JRXML2PDF_TYPES.tColor,
    vcBorderColor PK_JRXML2PDF_TYPES.tColor,
    vcName        tName,
    lShapePath    AS_PDF3_MOD.tPath
  );

  TYPE tSeriesList IS TABLE OF tSeries INDEX BY BINARY_INTEGER;

  TYPE tPlot IS RECORD (
    nType            NUMBER,
    lDataSets        tDatasetList,
    nLabelPosition   tValuePosition,
    vcLabelColor     PK_JRXML2PDF_TYPES.tColor,
    vcLabelFont      PK_JRXML2PDF_TYPES.tFont,
    vcLabelFontStyle PK_JRXML2PDF_TYPES.tFontStyle,
    vcLabelPattern   PK_JRXML2PDF_TYPES.tPattern,
    nLabelFontSize   NUMBER,
    nCategorySpace   NUMBER,
    nSeriesSpace     NUMBER,
    vcIsDonut        PK_JRXML2PDF_TYPES.tYesNo,
    vcLabelFormat    PK_JRXML2PDF_TYPES.tMaxVarchar2,
    vcKeyFormat      PK_JRXML2PDF_TYPES.tMaxVarchar2,
    vcShowLines      PK_JRXML2PDF_TYPES.tYesNo,
    vcShowShapes     PK_JRXML2PDF_TYPES.tYesNo
  );

  TYPE tPlotList IS TABLE OF tPlot INDEX BY BINARY_INTEGER;

  TYPE tLegend IS RECORD (
    nX                      NUMBER,
    nY                      NUMBER,
    nWidth                  NUMBER,
    nHeight                 NUMBER,
    rPlot                   tPlot,
    vcPosition              tLegendPosition,
    vcFont                  PK_JRXML2PDF_TYPES.tFont,
    vcFontStyle             PK_JRXML2PDF_TYPES.tFontStyle,
    nFontSize               NUMBER,
    vcFontColor             PK_JRXML2PDF_TYPES.tColor,
    vcBgColor               PK_JRXML2PDF_TYPES.tColor,
    vcBorderColor           PK_JRXML2PDF_TYPES.tColor,
    rOuterPaddings          tPaddings,
    rInnerPaddings          tPaddings,
    nEntryXSpacing          NUMBER,
    nEntryYSpacing          NUMBER,
    nScaleToWidthPercentage NUMBER
  );

  TYPE tChart IS RECORD (
    lPlots              tPlotList,
    lSeries             tSeriesList,
    lValueAxis          tAxisList,
    lRangeAxis          tAxisList,
    rLegend             tLegend,
    nWidth              NUMBER,
    nHeight             NUMBER,
    vcBgColor           PK_JRXML2PDF_TYPES.tColor,
    vcBorderColor       PK_JRXML2PDF_TYPES.tColor,
    nBorderWidth        NUMBER,
    vcTitle             tTitle,
    vcTitleFont         PK_JRXML2PDF_TYPES.tFont,
    vcTitleFontStyle    PK_JRXML2PDF_TYPES.tFontStyle,
    nTitleFontSize      NUMBER,
    vcTitleColor        PK_JRXML2PDF_TYPES.tColor,
    vcSubTitle          tTitle,
    vcSubTitleFont      PK_JRXML2PDF_TYPES.tFont,
    vcSubTitleFontStyle PK_JRXML2PDF_TYPES.tFontStyle,
    nSubTitleFontSize   NUMBER,
    vcSubTitleColor     PK_JRXML2PDF_TYPES.tColor,
    vcTitlePosition     PK_JRXML2PDF_TYPES.tPosition,
    vcIs3D              PK_JRXML2PDF_TYPES.tYesNo,
    rInnerPaddings      tPaddings,
    rLocaleData         PK_JRXML2PDF_TYPES.tLocaleData
  );

  PLOT_CATEGORY_BARCHART    CONSTANT NUMBER:=1;
  PLOT_XY_LINECHART         CONSTANT NUMBER:=2;
  PLOT_PIECHART             CONSTANT NUMBER:=3;
  PLOT_CATEGORY_LINECHART   CONSTANT NUMBER:=4;
  PLOT_TIMESERIES_LINECHART CONSTANT NUMBER:=5;
  PLOT_METER_CHART          CONSTANT NUMBER:=6;
  PLOT_STACKED_BARCHART     CONSTANT NUMBER:=7;

  VALUE_AUTOMATIC           CONSTANT NUMBER:=1;
  VALUE_FIXED               CONSTANT NUMBER:=2;
  POSITION_TOP_OR_LEFT      CONSTANT NUMBER:=1;
  POSITION_BOTTOM_OR_RIGHT  CONSTANT NUMBER:=2;
  POSITION_HIDDEN           CONSTANT NUMBER:=3;

  POSITION_NONE             CONSTANT PK_JRXML2PDF_TYPES.tPosition:='None';
  POSITION_TOP              CONSTANT PK_JRXML2PDF_TYPES.tPosition:='Top';
  POSITION_BOTTOM           CONSTANT PK_JRXML2PDF_TYPES.tPosition:='Bottom';
  POSITION_LEFT             CONSTANT PK_JRXML2PDF_TYPES.tPosition:='Left';
  POSITION_RIGHT            CONSTANT PK_JRXML2PDF_TYPES.tPosition:='Right';

  VALUE_POSITION_NONE       CONSTANT NUMBER:=1;
  VALUE_POSITION_OUTSIDE    CONSTANT NUMBER:=2;
  VALUE_POSITION_INSIDE     CONSTANT NUMBER:=3;

  AXIS_TYPE_VALUE           CONSTANT NUMBER:=1;
  AXIS_TYPE_RANGE           CONSTANT NUMBER:=2;
  AXIS_VALUE                CONSTANT NUMBER:=1;
  AXIS_CATEGORY             CONSTANT NUMBER:=2;
  ORIENTATION_HORIZONTAL    CONSTANT NUMBER:=1;
  ORIENTATION_VERTICAL      CONSTANT NUMBER:=2;

  rCustomizableChart        tChart;

/**

  $name    FK_RADIANS

  $created 31.12.2012

  $author  Andreas Weiden

  $desc    convert a angkle in degrees to radians

  $param   i_nDegree angle in degree

  $return  angle in radians

  $version 1.0.0.0 31.12.2012 Weiden
           initial version
*/
  FUNCTION FK_RADIANS(i_nDegree IN NUMBER)
  RETURN NUMBER DETERMINISTIC;

/**

  $name    FK_CREATE_CATEGORY_BAR_CHART

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    create a new chart-object for a category-barchart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_CATEGORY_BAR_CHART(i_nWidth  IN NUMBER,
                                        i_nHeight IN NUMBER
                                       )
  RETURN tChart;

/**

  $name    FK_CREATE_CATEGORY_BAR_CHART

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    create a new chart-object for a category-linechart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_CATEGORY_LINE_CHART(i_nWidth  IN NUMBER,
                                         i_nHeight IN NUMBER
                                        )
  RETURN tChart;

/**

  $name    FK_CREATE_XY_LINE_CHART

  $created 18.12.2012

  $author  Andreas Weiden

  $desc    create a new chart-object for a XY-linechart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 18.12.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_XY_LINE_CHART(i_nWidth  IN NUMBER,
                                   i_nHeight IN NUMBER
                                  )
  RETURN tChart;

/**

  $name    FK_CREATE_TIME_LINE_CHART

  $created 24.12.2012

  $author  Andreas Weiden

  $desc    create a new chart-object for a timeseries-linechart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 24.12.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_TIME_LINE_CHART(i_nWidth  IN NUMBER,
                                     i_nHeight IN NUMBER
                                    )
  RETURN tChart;

/**

  $name    FK_CREATE_PIE_CHART

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    create a new chart-object for a piechart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_PIE_CHART(i_nWidth  IN NUMBER,
                               i_nHeight IN NUMBER
                              )
  RETURN tChart;

/**

  $name    FK_CREATE_STACKED_BAR_CHART

  $created 04.01.2013

  $author  Andreas Weiden

  $desc    create a new chart-object for a stacked barchart

  $param   i_nWidth  Width of the chart

  $param   i_nHeight Height of the chart

  $return  chart-object

  $version 0.1.0.0 04.01.2013 Weiden
           initial version
*/
  FUNCTION FK_CREATE_STACKED_BAR_CHART(i_nWidth  IN NUMBER,
                                       i_nHeight IN NUMBER
                                      )
  RETURN tChart;

/**

  $name    FK_CREATE_SERIES

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    create a new series-object with the given name.
           The series can be added to a chart-object

  $param   i_vcName Name of the series

  $return  series-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_SERIES(i_vcName IN VARCHAR2)
  RETURN tSeries;

/**

  $name    PR_ADD_SERIES

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    adds the given series to the given chart

  $param   io_rChart Chart-object to add the series to

  $param   i_rSeries Series to add

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  PROCEDURE PR_ADD_SERIES(io_rChart IN OUT NOCOPY tChart,
                          i_rSeries IN tSeries);

/**

  $name    FK_CREATE_XY_DATASET

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    returns a dataset-object for xy-data

  $return  dataset-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_XY_DATASET
  RETURN tDataset;

/**

  $name    FK_CREATE_CATEGORY_DATASET

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    returns a dataset-object for category-data

  $return  dataset-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_CATEGORY_DATASET
  RETURN tDataset;

/**

  $name    FK_CREATE_TIMESERIES_DATASET

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    returns a dataset-object for timeseries-data

  $return  dataset-object

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  FUNCTION FK_CREATE_TIMESERIES_DATASET
  RETURN tDataset;

/**

  $name    PR_ADD_DATASET

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    adds the given dataset to the chart-object
           the plotindex defines the plot the dataset belongs to.
           the seriesname defines the series the dataset belongs to.

  $param   io_rChart Chart-object

  $param   i_nPlotIndex Index of the plot the dataset belongs to

  $param   i_rDataset Dataset to add

  $param   i_vcSeries name of the series

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  PROCEDURE PR_ADD_DATASET(io_rChart    IN OUT NOCOPY tChart,
                           i_nPlotIndex IN NUMBER,
                           i_rDataSet   IN tDataset,
                           i_vcSeries   IN VARCHAR2 DEFAULT NULL);

/**

  $name    PR_ADD_DATASET

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    adds the given dataset to the chart-object
           the plotindex defines the plot the dataset belongs to.
           the seriesindexe defines the series the dataset belongs to.

  $param   io_rChart Chart-object

  $param   i_nPlotIndex Index of the plot the dataset belongs to

  $param   i_rDataset Dataset to add

  $param   i_nSeries Index of the series

  $version 0.1.0.0 25.11.2012 Weiden
           initial version
*/
  PROCEDURE PR_ADD_DATASET(io_rChart    IN OUT NOCOPY tChart,
                           i_nPlotIndex IN NUMBER,
                           i_rDataSet   IN tDataset,
                           i_nSeries    IN NUMBER);

/**

  $name    PR_CHART_TO_PDF

  $created 25.11.2012

  $author  Andreas Weiden

  $desc    renders the chart-object into the currently processed pdf via
           AS_PDF3_MOD.

           The podf has to initialized before

  $param   i_rChart Chart-object to render

  $param   i_nX      X-position

  $param   i_nY      Y-position

  $version 0.1.0.0 25.11.2012 Weiden
           initial version

*/
  PROCEDURE PR_CHART_TO_PDF(i_rChart  IN tChart,
                            i_nX      IN NUMBER,
                            i_nY      IN NUMBER);

END;
/

create or replace
PACKAGE BODY PK_JRXML2PDF_CHARTS IS

  LIGHT_GRAY CONSTANT PK_JRXML2PDF_TYPES.tColor:='EEEEEE';
  MID_GRAY   CONSTANT PK_JRXML2PDF_TYPES.tColor:='C0C0C0';

  MAX_NUMBER CONSTANT NUMBER:=99999999999999999999999999999;
  MIN_NUMBER CONSTANT NUMBER:=-99999999999999999999999999999;
  MAX_DATE   CONSTANT DATE:=TO_DATE('01.01.4000','DD.MM.YYYY');
  MIN_DATE   CONSTANT DATE:=TO_DATE('01.01.1000','DD.MM.YYYY');
  PIXEL_PER_TICK NUMBER:=20;

  PI     CONSTANT NUMBER:=3.141592653589793;
  TWO_PI CONSTANT NUMBER:=3.141592653589793*2;

  TYPE tRect IS RECORD (
     nX      NUMBER,
     nY      NUMBER,
     nWidth  NUMBER,
     nHeight NUMBER
  );

  TYPE tPathList IS TABLE OF AS_PDF3_MOD.tPath INDEX BY BINARY_INTEGER;
  TYPE tMinMaxList IS TABLE OF NUMBER INDEX BY PK_JRXML2PDF_TYPES.tMaxVarchar2;


  lDefaultNumberTicks tTickEntryList;
  lDefaultDateTicks   tTickEntryList;
  lDefaultColors      PK_JRXML2PDF_TYPES.tColorList;
  lDefaultShapes      tPathList;

  FUNCTION FK_TRANSFORM_PATH(i_nX    IN NUMBER,
                             i_nY    IN NUMBER,
                             i_lPath IN AS_PDF3_MOD.tPath)
  RETURN AS_PDF3_MOD.tPath IS
    lPath AS_PDF3_MOD.tPath;
  BEGIN
    FOR i IN 1..i_lPath.COUNT LOOP
      lPath(i):=i_lPath(i);
      lPath(i).nVal1:=lPath(i).nVal1+i_nX;
      lPath(i).nVal2:=lPath(i).nVal2+i_nY;
      lPath(i).nVal3:=lPath(i).nVal3+i_nX;
      lPath(i).nVal4:=lPath(i).nVal4+i_nY;
      lPath(i).nVal5:=lPath(i).nVal5+i_nX;
      lPath(i).nVal6:=lPath(i).nVal6+i_nY;
    END LOOP;
    RETURN lPath;
  END;

  -- ---------------------------------------------------------------------------


  FUNCTION FK_RADIANS(i_nDegree IN NUMBER)
  RETURN NUMBER DETERMINISTIC IS
  BEGIN
    RETURN i_nDegree / 57.2957795;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_PIESLICE(i_nCX          IN NUMBER,
                             i_nCY         IN NUMBER,
                             i_nA          IN NUMBER,
                             i_nB          IN NUMBER,
                             i_nLambda1    IN NUMBER,
                             i_nLambda2    IN NUMBER,
                             i_bIsPieSlice IN BOOLEAN)
  RETURN AS_PDF3_MOD.tPath IS
    nCX NUMBER;
    nCY NUMBER;
    nA NUMBER;
    nB NUMBER;
    nTheta NUMBER;
    nCosTheta NUMBER;
    nSinTheta NUMBER;
    nEta1     NUMBER;
    nEta2     NUMBER;
    nX1 NUMBER;
    nX2 NUMBER;
    nY1 NUMBER;
    nY2 NUMBER;
    nXF1 NUMBER;
    nXF2 NUMBER;
    nYF1 NUMBER;
    nYF2 NUMBER;
    nXLeft NUMBER;
    nYUp NUMBER;
    nWidth NUMBER;
    nHeight NUMBER;
    bIsPieSlice BOOLEAN;
    nMaxDegree  NUMBER;
    nDefaultFlatness NUMBER;
    nF NUMBER;
    nE2 NUMBER;
    nG NUMBER;
    nG2 NUMBER;
    nd NUMBER;
    ndx NUMBER;
    ndy NUMBER;
    naCosEta1 NUMBER;
    nbSinEta1 NUMBER;
    naCosEta2 NUMBER;
    nbSinEta2 NUMBER;
    lPath     AS_PDF3_MOD.tPath;
    iPos      PLS_INTEGER;

    PROCEDURE PR_BUILD_PATH IS
      bFound BOOLEAN;
      n      PLS_INTEGER;
      ndEta  NUMBER;
      netaB  NUMBER;

      ncosEtaB  NUMBER;
      nsinEtaB  NUMBER;
      naCosEtaB NUMBER;
      nbSinEtaB NUMBER;
      naSinEtaB NUMBER;
      nbCosEtaB NUMBER;
      nxB       NUMBER;
      nyB       NUMBER;
      nxBDot    NUMBER;
      nyBDot    NUMBER;
      nt        NUMBER;
      nAlpha    NUMBER;
      netaA     NUMBER;
      nxA       NUMBER;
      nyA       NUMBER;
      nxADot    NUMBER;
      nyADot    NUMBER;
    BEGIN
      -- build path
      -- find the number of Bézier curves needed
      bfound := false;
      n := 1;
      while ((NOT bfound) AND (n < 1024)) LOOP
        ndEta := (neta2 - neta1) / n;
        if (ndEta <= 0.5 * PI) THEN
          netaB := neta1;
          bfound := true;
          FOR I IN 0..n-1 LOOP
            netaA := netaB;
            netaB :=nEtaB+ ndEta;
          END LOOP;
        END IF;
        n := n*2;
      END LOOP;

      ndEta := (neta2 - neta1) / n;
      netaB := neta1;

      ncosEtaB  := cos(netaB);
      nsinEtaB  := sin(netaB);
      naCosEtaB := na * ncosEtaB;
      nbSinEtaB := nb * nsinEtaB;
      naSinEtaB := na * nsinEtaB;
      nbCosEtaB := nb * ncosEtaB;
      nxB       := ncx + naCosEtaB * ncosTheta - nbSinEtaB * nsinTheta;
      nyB       := ncy + naCosEtaB * nsinTheta + nbSinEtaB * ncosTheta;
      nxBDot    := -naSinEtaB * ncosTheta - nbCosEtaB * nsinTheta;
      nyBDot    := -naSinEtaB * nsinTheta + nbCosEtaB * ncosTheta;

      if (bisPieSlice) THEN
        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
        lPath(iPos).nVal1:=ncx;
        lPath(iPos).nVal2:=ncy;
        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_LINE_TO;
        lPath(iPos).nVal1:=nXb;
        lPath(iPos).nVal2:=nYb;

      ELSE
        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
        lPath(iPos).nVal1:=nXb;
        lPath(iPos).nVal2:=nYb;
      END IF;

      nt     := tan(0.5 * ndEta);
      nalpha := sin(ndEta) * (sqrt(4 + 3 * nt * nt) - 1) / 3;

      FOR i IN 0..n-1 LOOP

        netaA  := netaB;
        nxA    := nxB;
        nyA    := nyB;
        nxADot := nxBDot;
        nyADot := nyBDot;

        netaB    :=netaB+ ndEta;
        ncosEtaB  := cos(netaB);
        nsinEtaB  := sin(netaB);
        naCosEtaB := na * ncosEtaB;
        nbSinEtaB := nb * nsinEtaB;
        naSinEtaB := na * nsinEtaB;
        nbCosEtaB := nb * ncosEtaB;
        nxB       := ncx + naCosEtaB * ncosTheta - nbSinEtaB * nsinTheta;
        nyB       := ncy + naCosEtaB * nsinTheta + nbSinEtaB * ncosTheta;
        nxBDot    := -naSinEtaB * ncosTheta - nbCosEtaB * nsinTheta;
        nyBDot    := -naSinEtaB * nsinTheta + nbCosEtaB * ncosTheta;

        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_CURVE_TO;
        lPath(iPos).nVal1:=nxA + nalpha * nxADot;
        lPath(iPos).nVal2:=nyA + nalpha * nyADot;
        lPath(iPos).nVal3:=nxB - nalpha * nxBDot;
        lPath(iPos).nVal4:=nyB - nalpha * nyBDot;
        lPath(iPos).nVal5:=nxB;
        lPath(iPos).nVal6:=nyB;

      END LOOP;

      if (bisPieSlice) THEN
        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_LINE_TO;
        lPath(iPos).nVal1:=ncx;
        lPath(iPos).nVal2:=ncy;
      ELSE
        iPos:=lPath.COUNT+1;
        lPath(iPos).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
        lPath(iPos).nVal1:=ncx;
        lPath(iPos).nVal2:=ncy;
      END IF;
      iPos:=lPath.COUNT+1;
      lPath(iPos).nType:=AS_PDF3_MOD.PATH_CLOSE;

    END;

  BEGIN
    ncx         := i_ncx;
    ncy         := i_ncy;
    na          := i_na;
    nb          := i_nb;
    ntheta      := 0;--i_ntheta;
    bisPieSlice := i_bisPieSlice;

    neta1       := ATAN2(sin(i_nlambda1) / nb,
                            cos(i_nlambda1) / na);
    neta2       := ATAN2(sin(i_nlambda2) / nb,
                            cos(i_nlambda2) / na);
    ncosTheta   := cos(ntheta);
    nsinTheta   := sin(ntheta);
    nmaxDegree  := 3;
    ndefaultFlatness := 0.5; -- half a pixel

    -- make sure we have eta1 <= eta2 <= eta1 + 2 PI
    neta2 :=nEta2-TWO_PI * floor((neta2 - neta1) / TWO_PI);

    -- the preceding correction fails if we have exactly et2 - eta1 = 2 PI
    -- it reduces the interval to zero length
    if ((i_nlambda2 - i_nlambda1 > PI) AND (neta2 - neta1 < PI)) THEN
      neta2 :=neta2+ 2 * PI;
    END IF;


    --computeFocii();
    nd  := sqrt(na * na - nb * nb);
    ndx := nd * ncosTheta;
    ndy := nd * nsinTheta;

    nxF1 := ncx - ndx;
    nyF1 := ncy - ndy;
    nxF2 := ncx + ndx;
    nyF2 := ncy + ndy;


    --computeEndPoints();
    -- start point
    naCosEta1 := na * cos(neta1);
    nbSinEta1 := nb * sin(neta1);
    nx1 := ncx + naCosEta1 * ncosTheta - nbSinEta1 * nsinTheta;
    ny1 := ncy + naCosEta1 * nsinTheta + nbSinEta1 * ncosTheta;

    -- end point
    naCosEta2 := na * cos(neta2);
    nbSinEta2 := nb * sin(neta2);
    nx2 := ncx + naCosEta2 * ncosTheta - nbSinEta2 * nsinTheta;
    ny2 := ncy + naCosEta2 * nsinTheta + nbSinEta2 * ncosTheta;


   -- computeDerivedFlatnessParameters();
    nf   := (na - nb) / na;
    ne2  := nf * (2.0 - nf);
    ng   := 1.0 - nf;
    ng2  := ng * ng;

    PR_BUILD_PATH;

    RETURN lPath;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_DEFAULT_SHAPES IS
  BEGIN
    -- Square
    lDefaultShapes(1)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(1)(1).nVal1:=-3;
    lDefaultShapes(1)(1).nVal2:=-3;
    lDefaultShapes(1)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(1)(2).nVal1:=-3;
    lDefaultShapes(1)(2).nVal2:=3;
    lDefaultShapes(1)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(1)(3).nVal1:=3;
    lDefaultShapes(1)(3).nVal2:=3;
    lDefaultShapes(1)(4).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(1)(4).nVal1:=3;
    lDefaultShapes(1)(4).nVal2:=-3;
    lDefaultShapes(1)(5).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- triangle down
    lDefaultShapes(2)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(2)(1).nVal1:=0;
    lDefaultShapes(2)(1).nVal2:=3;
    lDefaultShapes(2)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(2)(2).nVal1:=-3;
    lDefaultShapes(2)(2).nVal2:=-3;
    lDefaultShapes(2)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(2)(3).nVal1:=3;
    lDefaultShapes(2)(3).nVal2:=-3;
    lDefaultShapes(2)(4).nType:=AS_PDF3_MOD.PATH_CLOSE;

    lDefaultShapes(3):=FK_CALC_PIESLICE(i_nCX         =>0,
                                        i_nCY         =>0,
                                        i_nA          =>3.5,
                                        i_nB          =>3.5,
                                        i_nLambda1    =>0,
                                        i_nLambda2    =>360,
                                        i_bIsPieSlice =>FALSE);

    -- diamond
    lDefaultShapes(4)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(4)(1).nVal1:=-3;
    lDefaultShapes(4)(1).nVal2:=0;
    lDefaultShapes(4)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(4)(2).nVal1:=0;
    lDefaultShapes(4)(2).nVal2:=3;
    lDefaultShapes(4)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(4)(3).nVal1:=3;
    lDefaultShapes(4)(3).nVal2:=0;
    lDefaultShapes(4)(4).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(4)(4).nVal1:=0;
    lDefaultShapes(4)(4).nVal2:=-3;
    lDefaultShapes(4)(5).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- flat Square
    lDefaultShapes(5)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(5)(1).nVal1:=-3;
    lDefaultShapes(5)(1).nVal2:=-1.5;
    lDefaultShapes(5)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(5)(2).nVal1:=-3;
    lDefaultShapes(5)(2).nVal2:=1.5;
    lDefaultShapes(5)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(5)(3).nVal1:=3;
    lDefaultShapes(5)(3).nVal2:=1.5;
    lDefaultShapes(5)(4).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(5)(4).nVal1:=3;
    lDefaultShapes(5)(4).nVal2:=-1.5;
    lDefaultShapes(5)(5).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- triangle down
    lDefaultShapes(6)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(6)(1).nVal1:=0;
    lDefaultShapes(6)(1).nVal2:=-3;
    lDefaultShapes(6)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(6)(2).nVal1:=-3;
    lDefaultShapes(6)(2).nVal2:=3;
    lDefaultShapes(6)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(6)(3).nVal1:=3;
    lDefaultShapes(6)(3).nVal2:=3;
    lDefaultShapes(6)(4).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- flat ellipse
    lDefaultShapes(7):=FK_CALC_PIESLICE(i_nCX         =>0,
                                        i_nCY         =>0,
                                        i_nA          =>3.5,
                                        i_nB          =>1.5,
                                        i_nLambda1    =>0,
                                        i_nLambda2    =>360,
                                        i_bIsPieSlice =>FALSE);

    -- triangle right
    lDefaultShapes(8)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(8)(1).nVal1:=-3;
    lDefaultShapes(8)(1).nVal2:=-3;
    lDefaultShapes(8)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(8)(2).nVal1:=3;
    lDefaultShapes(8)(2).nVal2:=0;
    lDefaultShapes(8)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(8)(3).nVal1:=-3;
    lDefaultShapes(8)(3).nVal2:=3;
    lDefaultShapes(8)(4).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- flat Square
    lDefaultShapes(9)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(9)(1).nVal1:=-1.5;
    lDefaultShapes(9)(1).nVal2:=-3;
    lDefaultShapes(9)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(9)(2).nVal1:=-1.5;
    lDefaultShapes(9)(2).nVal2:=3;
    lDefaultShapes(9)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(9)(3).nVal1:=1.5;
    lDefaultShapes(9)(3).nVal2:=3;
    lDefaultShapes(9)(4).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(9)(4).nVal1:=1.5;
    lDefaultShapes(9)(4).nVal2:=-3;
    lDefaultShapes(9)(5).nType:=AS_PDF3_MOD.PATH_CLOSE;

    -- triangle left
    lDefaultShapes(10)(1).nType:=AS_PDF3_MOD.PATH_MOVE_TO;
    lDefaultShapes(10)(1).nVal1:=3;
    lDefaultShapes(10)(1).nVal2:=-3;
    lDefaultShapes(10)(2).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(10)(2).nVal1:=-3;
    lDefaultShapes(10)(2).nVal2:=0;
    lDefaultShapes(10)(3).nType:=AS_PDF3_MOD.PATH_LINE_TO;
    lDefaultShapes(10)(3).nVal1:=3;
    lDefaultShapes(10)(3).nVal2:=3;
    lDefaultShapes(10)(4).nType:=AS_PDF3_MOD.PATH_CLOSE;

  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_INIT_DEFAULT_NUM_TICKS IS
    nTick1 NUMBER:=0.000000000000001;
    nTick2 NUMBER:=0.000000000000002;
    nTick3 NUMBER:=0.0000000000000025;
    nTick4 NUMBER:=0.000000000000005;
    nTick5 NUMBER:=0.0000000000000075;
    vcPattern PK_JRXML2PDF_TYPES.tPattern:='0.0000000000000000';
    PROCEDURE PR_ADD_TICK(i_nValue    IN NUMBER,
                          i_vcPattern IN VARCHAR2) IS
      iPos PLS_INTEGER:=lDefaultNumberTicks.COUNT+1;
    BEGIN
      lDefaultNumberTicks(iPos).rDataEntry.nValue:=i_nValue;
      lDefaultNumberTicks(iPos).vcPattern:=i_vcPattern;
    END;
  BEGIN
    FOR i IN 1..30 LOOP

      PR_ADD_TICK(nTick1, 'FM' || vcPattern);
      PR_ADD_TICK(nTick2, 'FM' || vcPattern);
      PR_ADD_TICK(nTick3, 'FM' || vcPattern);
      PR_ADD_TICK(nTick4, 'FM' || vcPattern);
      PR_ADD_TICK(nTick5, 'FM' || vcPattern);

      nTick1:=nTick1*10;
      nTick2:=nTick2*10;
      nTick3:=nTick3*10;
      nTick4:=nTick4*10;
      nTick5:=nTick5*10;
      IF i<16 THEN
        vcPattern:=SUBSTR(vcPattern, 1, LENGTH(vcPattern)-1);
      ELSIF i=16 THEN
        vcPattern:='90';
      ELSE
        IF MOD(i,3)=0 THEN
          vcPattern:='G' || vcPattern;
        END IF;
        vcPattern:='9' || vcPattern;
      END IF;
    END LOOP;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_INIT_DEFAULT_DATE_TICKS IS
    nTick1 NUMBER:=0.000000000000001;
    nTick2 NUMBER:=0.000000000000002;
    nTick3 NUMBER:=0.0000000000000025;
    nTick4 NUMBER:=0.000000000000005;
    nTick5 NUMBER:=0.0000000000000075;
    vcPattern PK_JRXML2PDF_TYPES.tPattern:='0.0000000000000000';
    PROCEDURE PR_ADD_TICK(i_nValue    IN NUMBER,
                          i_vcPattern IN VARCHAR2,
                          i_vcUnit    IN VARCHAR2) IS
      iPos PLS_INTEGER:=lDefaultDateTicks.COUNT+1;
    BEGIN
      lDefaultDateTicks(iPos).rDataEntry.nValue:=i_nValue;
      lDefaultDateTicks(iPos).vcPattern:=i_vcPattern;
      lDefaultDateTicks(iPos).vcUnit:=i_vcUnit;
    END;
  BEGIN
    -- second
    PR_ADD_TICK( 1, 'MI:SS', 'SS');
    -- 2 second
    PR_ADD_TICK( 2, 'MI:SS', 'SS');
    -- 5 second
    PR_ADD_TICK( 5, 'MI:SS', 'SS');
    -- 10 second
    PR_ADD_TICK(10, 'MI:SS', 'SS');
    -- 15 second
    PR_ADD_TICK(15, 'MI:SS', 'SS');
    -- 30 second
    PR_ADD_TICK(30, 'MI:SS', 'SS');
    -- 1 minute
    PR_ADD_TICK( 1, 'HH24:MI', 'MI');
    -- 2 minute
    PR_ADD_TICK( 2, 'HH24:MI', 'MI');
    -- 5 minute
    PR_ADD_TICK( 5, 'HH24:MI', 'MI');
    -- 10 minute
    PR_ADD_TICK(10, 'HH24:MI', 'MI');
    -- 15 minute
    PR_ADD_TICK(15, 'HH24:MI', 'MI');
    -- 30 minute
    PR_ADD_TICK(30, 'HH24:MI', 'MI');
    -- 1 hour
    PR_ADD_TICK( 1, 'HH24', 'HH');
    -- 2 hour
    PR_ADD_TICK( 2, 'HH24', 'HH');
    -- 4 hour
    PR_ADD_TICK( 4, 'HH24', 'HH');
    -- 6 hour
    PR_ADD_TICK( 6, 'HH24', 'HH');
    -- 8 hour
    PR_ADD_TICK( 8, 'HH24', 'HH');
    -- 12 hour
    PR_ADD_TICK(12, 'HH24', 'HH');
    -- 1 day
    PR_ADD_TICK( 1, 'DD.MM.', 'DD');
    -- 2 days
    PR_ADD_TICK( 2, 'DD.MM.', 'DD');
    -- 5 days
    PR_ADD_TICK( 5, 'DD.MM.', 'DD');
    -- 10 days
    PR_ADD_TICK(10, 'DD.MM.', 'DD');
    -- 15 days
    PR_ADD_TICK(15, 'DD.MM.', 'DD');
    -- 1 month
    PR_ADD_TICK( 1, 'MM.YYYY', 'MM');
    -- 2 months
    PR_ADD_TICK( 2, 'MM.YYYY', 'MM');
    -- 3 months
    PR_ADD_TICK( 3, 'MM.YYYY', 'MM');
    -- 6 months
    PR_ADD_TICK( 6, 'MM.YYYY', 'MM');
    -- 1 year
    PR_ADD_TICK( 1, 'YYYY', 'YYYY');
    -- 2 years
    PR_ADD_TICK( 2, 'YYYY', 'YYYY');
    -- 5 years
    PR_ADD_TICK( 5, 'YYYY', 'YYYY');
    -- 10 years
    PR_ADD_TICK(10, 'YYYY', 'YYYY');
    -- 20 years
    PR_ADD_TICK(20, 'YYYY', 'YYYY');
    -- 25 years
    PR_ADD_TICK(25, 'YYYY', 'YYYY');
    -- 50 years
    PR_ADD_TICK(50, 'YYYY', 'YYYY');
    -- 100 years
    PR_ADD_TICK(100, 'YYYY', 'YYYY');
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_NUM_PATTERN_FOR_VALUE(i_nValue IN NUMBER)
  RETURN PK_JRXML2PDF_TYPES.tPattern IS
    vcPattern PK_JRXML2PDF_TYPES.tPattern;
  BEGIN
    FOR i IN 1..lDefaultNumberTicks.COUNT LOOP
      IF lDefaultNumberTicks(i).rDataEntry.nValue>i_nValue THEN
        vcPattern:=lDefaultNumberTicks(i).vcPattern;
        EXIT;
      END IF;
    END LOOP;
    RETURN vcPattern;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_NUM_TICK_ENTRIES(i_nMinValue  IN NUMBER,
                                    i_nMaxValue  IN NUMBER,
                                    i_nSpace     IN NUMBER,
                                    i_nTickSpace IN NUMBER,
                                    i_vcPattern  IN PK_JRXML2PDF_TYPES.tPattern)
  RETURN tTickEntryList IS
    lTickEntries             tTickEntryList;
    nTicksToShow             NUMBER:=GREATEST(ROUND(i_nSpace/i_nTickSpace),1);
    nValueRange              NUMBER:=i_nMaxValue-i_nMinValue;
    nApproximateTickDistance NUMBER:=nValueRange/nTicksToShow;
    nValue                   NUMBER;
    iTickIndex               PLS_INTEGER;
    vcIndex                  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcPattern                PK_JRXML2PDF_TYPES.tPattern:=i_vcPattern;
  BEGIN
    IF vcPattern IS NULL THEN
      vcPattern:=FK_GET_NUM_PATTERN_FOR_VALUE(nValueRange);
    END IF;
    FOR i IN 1..lDefaultNumberTicks.COUNT LOOP
      IF lDefaultNumberTicks(i).rDataEntry.nValue>=nApproximateTickDistance THEN
        iTickIndex:=i;
        EXIT;
      END IF;
    END LOOP;
    IF iTickIndex>1 THEN
      iTickIndex:=iTickIndex-1;
    END IF;
    IF iTickIndex>0 THEN
      -- round first tick to match spacing
      IF i_nMinValue!=0 THEN
        nValue:=ROUND(i_nMinValue/lDefaultNumberTicks(iTickIndex).rDataEntry.nValue)*lDefaultNumberTicks(iTickIndex).rDataEntry.nValue;
      ELSE
        nValue:=0;
      END IF;
      -- create ticks
      WHILE nValue<i_nMaxValue LOOP
        vcIndex:=TO_CHAR(nValue, 'FM00000000000000000000D00000000000000000');
        lTickEntries(vcIndex).rDataEntry.nValue:=nValue;
        lTickEntries(vcIndex).vcPattern:=vcPattern;
        nValue:=nValue+lDefaultNumberTicks(iTickIndex).rDataEntry.nValue;
      END LOOP;
    END IF;
    RETURN lTickEntries;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_TICK_VALUE_FOR_ENTRY(i_iIdx IN PLS_INTEGER)
  RETURN NUMBER IS
  BEGIN
    IF lDefaultDateTicks(i_iIdx).vcUnit='SS' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue/(24*60*60);
    ELSIF lDefaultDateTicks(i_iIdx).vcUnit='MI' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue/(24*60);
    ELSIF lDefaultDateTicks(i_iIdx).vcUnit='HH' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue/24;
    ELSIF lDefaultDateTicks(i_iIdx).vcUnit='DD' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue;
    ELSIF lDefaultDateTicks(i_iIdx).vcUnit='MM' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue*30;
    ELSIF lDefaultDateTicks(i_iIdx).vcUnit='YYYY' THEN
      RETURN lDefaultDateTicks(i_iIdx).rDataEntry.nValue*365;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_DATE_PATTERN_FOR_VALUE(i_nValue IN NUMBER)
  RETURN PK_JRXML2PDF_TYPES.tPattern IS
    vcPattern PK_JRXML2PDF_TYPES.tPattern;
  BEGIN
    FOR i IN 1..lDefaultDateTicks.COUNT LOOP
      IF FK_TICK_VALUE_FOR_ENTRY(i)>i_nValue THEN
        vcPattern:=lDefaultDateTicks(i).vcPattern;
        EXIT;
      END IF;
    END LOOP;
    RETURN vcPattern;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CALC_DATE_TICK_ENTRIES(i_dtMinValue IN DATE,
                                     i_dtMaxValue IN DATE,
                                     i_nSpace     IN NUMBER,
                                     i_nTickSpace IN NUMBER,
                                     i_vcPattern  IN PK_JRXML2PDF_TYPES.tPattern)
  RETURN tTickEntryList IS
    lTickEntries             tTickEntryList;
    nTicksToShow             NUMBER:=GREATEST(ROUND(i_nSpace/i_nTickSpace),1);
    nValueRange              NUMBER:=i_dtMaxValue-i_dtMinValue;
    nApproximateTickDistance NUMBER:=nValueRange/nTicksToShow;
    dtValue                  DATE;
    iTickIndex               PLS_INTEGER;
    vcIndex                  PK_JRXML2PDF_TYPES.tMaxVarchar2;
    vcPattern                PK_JRXML2PDF_TYPES.tPattern:=i_vcPattern;
  BEGIN
    IF vcPattern IS NULL THEN
      vcPattern:=FK_GET_DATE_PATTERN_FOR_VALUE(nValueRange);
    END IF;
    FOR i IN 1..lDefaultDateTicks.COUNT LOOP
      IF FK_TICK_VALUE_FOR_ENTRY(i)>=nApproximateTickDistance THEN
        iTickIndex:=i;
        EXIT;
      END IF;
    END LOOP;
    IF iTickIndex>1 THEN
      iTickIndex:=iTickIndex-1;
    END IF;
    IF iTickIndex>0 THEN
      -- round first tick to match spacing
      --TODO round to unit dtValue:=ROUND(i_dtMinValue/lDefaultDateTicks(iTickIndex).rDataEntry.nValue)*lDefaultDateTicks(iTickIndex).rDataEntry.nValue;
      dtValue:=i_dtMinValue;
      -- create ticks
      WHILE dtValue<i_dtMaxValue LOOP
        vcIndex:=TO_CHAR(dtValue, 'YYYYMMDDHH24MISS');
        lTickEntries(vcIndex).rDataEntry.dtValue:=dtValue;
        lTickEntries(vcIndex).vcPattern:=vcPattern;
        dtValue:=dtValue+FK_TICK_VALUE_FOR_ENTRY(iTickIndex);
      END LOOP;
    END IF;
    RETURN lTickEntries;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_BASE_PLOT
  RETURN tPlot IS
    rPlot tPlot;
  BEGIN
    rPlot.nLabelPosition  :=VALUE_POSITION_NONE;
    rPlot.vcLabelColor    :=PK_JRXML2PDF_TYPES.BLACK;
    rPlot.vcLabelFont     :=PK_JRXML2PDF_TYPES.ARIAL;
    rPlot.vcLabelFontStyle:=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rPlot.nLabelFontSize  :=10;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_CATEGORY_BAR_PLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType         :=PLOT_CATEGORY_BARCHART;

    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_CATEGORY_LINE_PLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType         :=PLOT_CATEGORY_LINECHART;
    rPlot.vcShowShapes  :=PK_JRXML2PDF_TYPES.YES;
    rPlot.vcShowLines   :=PK_JRXML2PDF_TYPES.YES;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_XY_LINE_PLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType:=PLOT_XY_LINECHART;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_PIECHART_PLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType:=PLOT_PIECHART;
    rPlot.nLabelPosition:=VALUE_POSITION_OUTSIDE;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_TIMESERIES_LINE_PLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType:=PLOT_TIMESERIES_LINECHART;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_STACKED_BARPLOT
  RETURN tPlot IS
    rPlot tPlot:=FK_CREATE_BASE_PLOT;
  BEGIN
    rPlot.nType:=PLOT_STACKED_BARCHART;
    RETURN rPlot;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_LEGEND(i_rPlot IN tPlot)
  RETURN tLegend IS
    rLegend tLegend;
  BEGIN
    rLegend.rPlot        :=i_rPlot;
    rLegend.vcPosition    :=POSITION_BOTTOM;
    rLegend.vcFont       :=PK_JRXML2PDF_TYPES.ARIAL;
    rLegend.vcFontStyle  :=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rLegend.nFontSize   :=12;
    rLegend.vcFontColor  :=PK_JRXML2PDF_TYPES.BLACK;
    rLegend.vcBgColor    :=NULL;
    rLegend.vcBorderColor:=PK_JRXML2PDF_TYPES.BLACK;
    rLegend.rOuterPaddings.nLeft  :=5;
    rLegend.rOuterPaddings.nTop   :=5;
    rLegend.rOuterPaddings.nRight :=5;
    rLegend.rOuterPaddings.nBottom:=5;
    rLegend.rInnerPaddings.nLeft  :=5;
    rLegend.rInnerPaddings.nTop   :=5;
    rLegend.rInnerPaddings.nRight :=5;
    rLegend.rInnerPaddings.nBottom:=5;
    rLegend.nEntryXSpacing:=4;
    rLegend.nEntryYSpacing:=4;
    rLegend.nScaleToWidthPercentage:=0.8;
    RETURN rLegend;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_DARKER(i_vcColor IN PK_JRXML2PDF_TYPES.tColor)
  RETURN PK_JRXML2PDF_TYPES.tColor IS
    vcR PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 1, 2);
    vcG PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 3, 2);
    vcB PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 5, 2);
  BEGIN
    vcR:=TO_CHAR(GREATEST(TO_NUMBER(vcR, 'XX')-40, 0), 'FM0X');
    vcG:=TO_CHAR(GREATEST(TO_NUMBER(vcG, 'XX')-40, 0), 'FM0X');
    vcB:=TO_CHAR(GREATEST(TO_NUMBER(vcB, 'XX')-40, 0), 'FM0X');
    RETURN vcR || vcG || vcB;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_LIGHTER(i_vcColor IN PK_JRXML2PDF_TYPES.tColor)
  RETURN PK_JRXML2PDF_TYPES.tColor IS
    vcR PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 1, 2);
    vcG PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 3, 2);
    vcB PK_JRXML2PDF_TYPES.tColor:=SUBSTR(REPLACE(i_vcColor, '#', ''), 5, 2);
  BEGIN
    vcR:=TO_CHAR(LEAST(TO_NUMBER(vcR, 'XX')+40, 255), 'FM0X');
    vcG:=TO_CHAR(LEAST(TO_NUMBER(vcG, 'XX')+40, 255), 'FM0X');
    vcB:=TO_CHAR(LEAST(TO_NUMBER(vcB, 'XX')+40, 255), 'FM0X');
    RETURN vcR || vcG || vcB;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_VALUE_AXIS
  RETURN tAxis IS
    rValueAxis tAxis;
  BEGIN
    rValueAxis.nAxisType:=AXIS_TYPE_VALUE;
    rValueAxis.nValueType:=AXIS_VALUE;
    rValueAxis.nPosition:=POSITION_TOP_OR_LEFT;
    rValueAxis.nOrientation:=ORIENTATION_VERTICAL;
    rValueAxis.nMinValueType:=VALUE_AUTOMATIC;
    rValueAxis.vcInclude0Value:=PK_JRXML2PDF_TYPES.YES;
    rValueAxis.nMaxValueType:=VALUE_AUTOMATIC;
    rValueAxis.nAdditionalPercent:=5;
    rValueAxis.nWidthType:=VALUE_AUTOMATIC;
    rValueAxis.vcDotLineColor:=MID_GRAY;
    rValueAxis.vcShowTickLabels    :=PK_JRXML2PDF_TYPES.YES;
    rValueAxis.vcShowTickMarks     :=PK_JRXML2PDF_TYPES.YES;
    rValueAxis.vcShowDotlines      :=PK_JRXML2PDF_TYPES.YES;
    rValueAxis.vcLabelColor        :=PK_JRXML2PDF_TYPES.BLACK;
    rValueAxis.vcLabelFont         :=PK_JRXML2PDF_TYPES.ARIAL;
    rValueAxis.vcLabelFontStyle    :=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rValueAxis.nLabelFontSize      :=10;
    rValueAxis.vcTickLabelColor    :=PK_JRXML2PDF_TYPES.BLACK;
    rValueAxis.vcTickLabelFont     :=PK_JRXML2PDF_TYPES.ARIAL;
    rValueAxis.vcTickLabelFontStyle:=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rValueAxis.nTickLabelFontSize  :=10;
    rValueAxis.vcLineColor         :=PK_JRXML2PDF_TYPES.BLACK;
    rValueAxis.nAxisLineWidth      :=1;
    rValueAxis.nStartOffset        :=0;
    rValueAxis.nEndOffset          :=10;
    rValueAxis.nDataOffset         :=0;
    rValueAxis.nTickLabelOffset    :=0;
    rValueAxis.nTickSpace          :=60;
    RETURN rValueAxis;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_RANGE_AXIS(i_nAxisType IN tAxisType)
  RETURN tAxis IS
    rRangeAxis tAxis;
  BEGIN
    rRangeAxis.nAxisType:=AXIS_TYPE_RANGE;
    rRangeAxis.nValueType:=i_nAxisType;
    rRangeAxis.nPosition:=POSITION_BOTTOM_OR_RIGHT;
    rRangeAxis.nOrientation:=ORIENTATION_HORIZONTAL;
    rRangeAxis.nMinValueType:=VALUE_AUTOMATIC;
    rRangeAxis.nMaxValueType:=VALUE_AUTOMATIC;
    rRangeAxis.nAdditionalPercent:=5;
    rRangeAxis.vcInclude0Value:=PK_JRXML2PDF_TYPES.YES;
    rRangeAxis.nHeightType:=VALUE_AUTOMATIC;
    rRangeAxis.vcShowTickLabels:=PK_JRXML2PDF_TYPES.YES;
    IF i_nAxisType!=AXIS_CATEGORY THEN
      rRangeAxis.vcDotLineColor:=MID_GRAY;
      rRangeAxis.vcShowTickMarks   :=PK_JRXML2PDF_TYPES.YES;
      rRangeAxis.nStartOffset      :=0;
    ELSE
      rRangeAxis.vcShowTickMarks   :=PK_JRXML2PDF_TYPES.NO;
      -- Starting offset
      rRangeAxis.nStartOffset      :=10;
    END IF;
    rRangeAxis.vcShowDotlines      :=PK_JRXML2PDF_TYPES.YES;
    rRangeAxis.vcLabelColor        :=PK_JRXML2PDF_TYPES.BLACK;
    rRangeAxis.vcLabelFont         :=PK_JRXML2PDF_TYPES.ARIAL;
    rRangeAxis.vcLabelFontStyle    :=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rRangeAxis.nLabelFontSize      :=10;
    rRangeAxis.vcTickLabelColor    :=PK_JRXML2PDF_TYPES.BLACK;
    rRangeAxis.vcTickLabelFont     :=PK_JRXML2PDF_TYPES.ARIAL;
    rRangeAxis.vcTickLabelFontStyle:=PK_JRXML2PDF_TYPES.FONT_NORMAL;
    rRangeAxis.nTickLabelFontSize  :=10;
    rRangeAxis.vcLineColor         :=PK_JRXML2PDF_TYPES.BLACK;
    rRangeAxis.nAxisLineWidth      :=1;
    rRangeAxis.nEndOffset          :=10;
    rRangeAxis.nDataOffset         :=0;
    rRangeAxis.nTickLabelOffset    :=0;
    rRangeAxis.nTickSpace          :=60;
    RETURN rRangeAxis;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_SERIES(i_vcName IN VARCHAR2)
  RETURN tSeries IS
    rSeries tSeries;
  BEGIN
    rSeries.vcName:=i_vcName;
    RETURn rSeries;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_XY_DATASET
  RETURN tDataset IS
    rDataset tDataset;
  BEGIN
    rDataset.vcValueType:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    rDataset.vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    rDataset.nGroup:=1;
    rDataset.nValueAxis:=1;
    rDataset.nRangeAxis:=1;
    RETURN rDataset;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_CATEGORY_DATASET
  RETURN tDataset IS
    rDataset tDataset;
  BEGIN
    rDataset.vcValueType:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    rDataset.vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_VARCHAR;
    rDataset.nGroup:=1;
    rDataset.nValueAxis:=1;
    rDataset.nRangeAxis:=1;
    RETURN rDataset;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_TIMESERIES_DATASET
  RETURN tDataset IS
    rDataset tDataset;
  BEGIN
    rDataset.vcValueType:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
    rDataset.vcRangeType:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
    rDataset.nGroup:=1;
    rDataset.nValueAxis:=1;
    rDataset.nRangeAxis:=1;
    RETURN rDataset;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_SERIES(io_rChart IN OUT NOCOPY tChart, i_rSeries IN tSeries) IS
    iSeries PLS_INTEGER:=io_rChart.lSeries.COUNT+1;
  BEGIN
    io_rChart.lSeries(iSeries):=i_rSeries;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_DATASET(io_rChart IN OUT NOCOPY tChart, i_nPlotIndex IN NUMBER, i_rDataSet IN tDataset, i_vcSeries IN VARCHAR2 DEFAULT NULL) IS
    iPos PLS_INTEGER:=io_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT+1;
    iSeries PLS_INTEGER:=io_rChart.lSeries.COUNT+1;
  BEGIN
    IF i_vcSeries IS NULL THEN
      iSeries:=NULL;
    ELSE
      FOR i IN 1..io_rChart.lSeries.COUNT LOOP
        IF io_rChart.lSeries(i).vcName=i_vcSeries THEN
          iSeries:=i;
          EXIT;
        END IF;
      END LOOP;
      IF iSeries>io_rChart.lSeries.COUNT THEN
        io_rChart.lSeries(iSeries):=FK_CREATE_SERIES(i_vcSeries);
      END IF;
    END IF;
    io_rChart.lPlots(i_nPlotIndex).lDataSets(iPos):=i_rDataSet;
    io_rChart.lPlots(i_nPlotIndex).lDataSets(iPos).nSeries:=iSeries;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_ADD_DATASET(io_rChart IN OUT NOCOPY tChart, i_nPlotIndex IN NUMBER, i_rDataSet IN tDataset, i_nSeries IN NUMBER) IS
    iPos PLS_INTEGER:=io_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT+1;
  BEGIN
    IF io_rChart.lSeries.EXISTS(i_nSeries) THEN
      io_rChart.lPlots(i_nPlotIndex).lDataSets(iPos):=i_rDataSet;
      io_rChart.lPlots(i_nPlotIndex).lDataSets(iPos).nSeries:=i_nSeries;
    ELSE
      RAISE NO_DATA_FOUND;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_BASE_CHART
  RETURN tChart IS
    rChart tChart;
  BEGIN
    rChart.vcTitleFont        :=PK_JRXML2PDF_TYPES.ARIAL;
    rChart.vcTitleFontStyle   :=PK_JRXML2PDF_TYPES.FONT_BOLD;
    rChart.nTitleFontSize     :=16;
    rChart.vcTitleColor       :=PK_JRXML2PDF_TYPES.BLACK;
    rChart.vcSubTitleFont     :=PK_JRXML2PDF_TYPES.ARIAL;
    rChart.vcSubTitleFontStyle:=PK_JRXML2PDF_TYPES.FONT_BOLD;
    rChart.nSubTitleFontSize  :=14;
    rChart.vcSubTitleColor    :=PK_JRXML2PDF_TYPES.BLACK;
    rChart.vcTitlePosition    :=POSITION_TOP;
    rChart.rInnerPaddings.nLeft:=10;
    rChart.rInnerPaddings.nTop:=0;
    rChart.rInnerPaddings.nRight:=10;
    rChart.rInnerPaddings.nBottom:=0;
    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_CATEGORY_BAR_CHART(i_nWidth  IN NUMBER,
                                        i_nHeight IN NUMBER
                                       )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_CATEGORY_BAR_PLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_CATEGORY);

    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_CATEGORY_LINE_CHART(i_nWidth  IN NUMBER,
                                         i_nHeight IN NUMBER
                                        )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_CATEGORY_LINE_PLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_CATEGORY);

    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_PIE_CHART(i_nWidth  IN NUMBER,
                               i_nHeight IN NUMBER
                              )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_PIECHART_PLOT;
    rChart.lPlots(1).vcKeyFormat:='{0}';
    rChart.lPlots(1).vcLabelFormat:='{0}';
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    -- By default, donÄt show legend
    rChart.rLegend.vcPosition:=NULL;
    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_XY_BAR_CHART(i_nWidth  IN NUMBER,
                                  i_nHeight IN NUMBER
                                 )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_CATEGORY_BAR_PLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_VALUE);

    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_XY_LINE_CHART(i_nWidth  IN NUMBER,
                                   i_nHeight IN NUMBER
                                  )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_XY_LINE_PLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_VALUE);
    rChart.lRangeAxis(1).vcInclude0Value:=PK_JRXML2PDF_TYPES.NO;

    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_TIME_LINE_CHART(i_nWidth  IN NUMBER,
                                     i_nHeight IN NUMBER
                                    )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_TIMESERIES_LINE_PLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).vcInclude0Value:=PK_JRXML2PDF_TYPES.NO;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_VALUE);
    rChart.lRangeAxis(1).vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_CREATE_STACKED_BAR_CHART(i_nWidth  IN NUMBER,
                                       i_nHeight IN NUMBER
                                      )
  RETURN tChart IS
    rChart tChart:=FK_CREATE_BASE_CHART;
  BEGIN
    rChart.nWidth:=i_nWidth;
    rChart.nHeight:=i_nHeight;
    rChart.lPlots(1):=FK_CREATE_STACKED_BARPLOT;
    rChart.rLegend:=FK_CREATE_LEGEND(rChart.lPlots(1));
    rChart.lValueAxis(1):=FK_CREATE_VALUE_AXIS;
    rChart.lValueAxis(1).nTickSpace:=30;
    rChart.lRangeAxis(1):=FK_CREATE_RANGE_AXIS(AXIS_CATEGORY);
    RETURN rChart;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_RANGE_CATEGORY_KEY(i_rDataset In tDataset, i_iIdx IN PLS_INTEGER)
  RETURN PK_JRXML2PDF_TYPES.tMaxVarchar2 IS
    vcKey PK_JRXML2PDF_TYPES.tMaxVarchar2;
  BEGIN
    IF i_rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
      vcKey:=TO_CHAR(i_rDataset.lRangeDataEntries(i_iIdx).nValue, 'FM00000000000000000000D00000000000000000');
    ELSIF i_rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
      vcKey:=TO_CHAR(i_rDataset.lRangeDataEntries(i_iIdx).dtValue, 'YYYYMMDDHH24MISS');
    ELSE
      vcKey:=i_rDataset.lRangeDataEntries(i_iIdx).vcValue;
    END IF;
    RETURN vcKey;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CALC_AXIS_MEASURES(i_rChart     IN tChart,
                                  io_rAxis IN OUT NOCOPY tAxis,
                                  i_nAxisIndex IN NUMBER) IS
    nMaxLen     NUMBER:=0;
    vcText      PK_JRXML2PDF_TYPES.tMaxVarchar2;
    rDataSet    tDataSet;
    vcDataType  PK_JRXML2PDF_TYPES.tDatatype;
    nMinValue   NUMBER:=MAX_NUMBER;
    nMaxValue   NUMBER:=MIN_NUMBER;
    dtMinValue  DATE:=MAX_DATE;
    dtMaxValue  DATE:=MIN_DATE;
    vcPattern   PK_JRXML2PDF_TYPES.tPattern;
    nXSpace     NUMBER;
    nYSpace     NUMBER;
    bStacked    BOOLEAN;
    lMinStacked tMinMaxList;
    lMaxStacked tMinMaxList;
    
    PROCEDURE PR_CALC_VALUE_MINMAX(i IN PLS_INTEGER, j IN PLS_INTEGER) IS
      vcKey PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      FOR t IN 1..i_rChart.lPlots(i).lDatasets(j).lValueDataEntries.COUNT LOOP
        -- Initialize min and max if stacked
        IF bStacked THEN
          nMinValue :=MAX_NUMBER;
          nMaxValue :=MIN_NUMBER;
          dtMinValue:=MAX_DATE;
          dtMaxValue:=MIN_DATE;
          vcKey:=FK_GET_RANGE_CATEGORY_KEY(i_rChart.lPlots(i).lDatasets(j), t);
        END IF;
        IF rDataset.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nMinValue:=LEAST(nMinValue, i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(t).nValue);
          nMaxValue:=GREATEST(nMaxValue, i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(t).nValue);
        ELSIF rDataset.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          dtMinValue:=LEAST(dtMinValue, i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(t).dtValue);
          dtMaxValue:=GREATEST(dtMaxValue, i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(t).dtValue);
        ELSE
          vcText:=i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(t).vcValue;
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
        -- store min and max back
        IF     bStacked 
           AND vcKey IS NOT NULL THEN
          -- get value for accordin range-entry
          IF vcKey IS NOT NULL THEN
            IF lMinStacked.EXISTS(vcKey) THEN
              lMinStacked(vcKey):=lMinStacked(vcKey)+nMinValue;
            ELSE
              lMinStacked(vcKey):=nMinValue;
            END IF;
            IF lMaxStacked.EXISTS(vcKey) THEN
              lMaxStacked(vcKey):=lMaxStacked(vcKey)+nMaxValue;
            ELSE
              lMaxStacked(vcKey):=nMaxValue;
            END IF;
          END IF;
        END IF;
      END LOOP;

      -- for stacked charts, now get min and max bycategory 
      IF bStacked THEN
        nMinValue :=MAX_NUMBER;
        nMaxValue :=MIN_NUMBER;
        vcKey:=lMinStacked.FIRST;
        WHILE vcKey IS NOT NULL LOOP
          nMinValue:=LEAST(nMinValue, lMinStacked(vcKey));
          nMaxValue:=GREATEST(nMaxValue, lMaxStacked(vcKey));
          vcKey:=lMinStacked.NEXT(vcKey);
        END LOOP;
      END IF;
      IF rDataset.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
        IF nMaxValue!=nMinValue THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(nMaxValue-nMinValue));
        ELSIF nMaxValue!=0 THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(nMaxValue));
        ELSE
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(100));
        END IF;
        IF i_rChart.lPlots(i).lDatasets(j).lValueDataEntries.COUNT>0 THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(i_rChart.lPlots(i).lDatasets(j).lValueDataEntries.COUNT).nValue, vcPattern));
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
      ELSIF rDataset.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
        IF dtMinValue!=dtMaxValue THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_DATE_PATTERN_FOR_VALUE(dtMaxValue-dtMinValue));
        ELSE
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_DATE_PATTERN_FOR_VALUE(1));
        END IF;
        IF i_rChart.lPlots(i).lDatasets(j).lValueDataEntries.COUNT>0 THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lValueDataEntries(i_rChart.lPlots(i).lDatasets(j).lValueDataEntries.COUNT).dtValue,vcPattern));
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
      END IF;
    END;
    
    PROCEDURE PR_CALC_RANGE_MINMAX(i IN PLS_INTEGER, j IN PLS_INTEGER) IS
    BEGIN
      FOR t IN 1..i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT LOOP
        IF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).nValue, io_rAxis.vcTickLabelPattern));
          nMinValue:=LEAST(nMinValue, i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).nValue);
          nMaxValue:=GREATEST(nMaxValue, i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).nValue);
        ELSIF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).dtValue, io_rAxis.vcTickLabelPattern));
          dtMinValue:=LEAST(dtMinValue, i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).dtValue);
          dtMaxValue:=GREATEST(dtMaxValue, i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).dtValue);
        ELSE
          vcText:=i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).vcValue;
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
      END LOOP;
      IF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
        IF nMaxValue!=nMinValue THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(nMaxValue-nMinValue));
        ELSIF nMaxValue!=0 THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(nMaxValue));
        ELSE
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_NUM_PATTERN_FOR_VALUE(100));
        END IF;
        IF i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT>0 THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT).nValue, vcPattern));
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
      ELSIF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
        IF dtMinValue!=dtMaxValue THEN
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_DATE_PATTERN_FOR_VALUE(dtMaxValue-dtMinValue));
        ELSE
          vcPattern:=NVL(io_rAxis.vcTickLabelPattern, FK_GET_DATE_PATTERN_FOR_VALUE(1));
        END IF;
        IF i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT>0 THEN
          vcText:=TRIM(TO_CHAR(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT).dtValue,vcPattern));
          nMaxLen:=GREATEST(nMaxLen, AS_PDF3_MOD.str_len(vcText));
        END IF;
      END IF;
    END;
  BEGIN
    -- change rotation
    IF NVL(io_rAxis.nTicklabelRotation,0)!=0 THEN
      io_rAxis.nTicklabelRotation:=360-io_rAxis.nTicklabelRotation;
    END IF;
    FOR i IN 1..i_rChart.lPlots.COUNT LOOP
      bStacked:=i_rChart.lPlots(i).nType IN(PLOT_STACKED_BARCHART);
      -- initialize aggregats
      nMaxLen   :=0;
      nMinValue :=MAX_NUMBER;
      nMaxValue :=MIN_NUMBER;
      dtMinValue:=MAX_DATE;
      dtMaxValue:=MIN_DATE;
      lMinStacked.DELETE;
      lMaxStacked.DELETE;

      FOR j IN 1..i_rChart.lPlots(i).lDatasets.COUNT LOOP
        IF    (    i_rChart.lPlots(i).lDatasets(j).nValueAxis=i_nAxisIndex
               AND io_rAxis.nAxisType=AXIS_TYPE_VALUE
              )
           OR (    i_rChart.lPlots(i).lDatasets(j).nRangeAxis=i_nAxisIndex
               AND io_rAxis.nAxisType=AXIS_TYPE_RANGE
              ) THEN
          rDataSet:=i_rChart.lPlots(i).lDatasets(j);
          IF vcDataType IS NULL THEN
            IF io_rAxis.nAxisType=AXIS_TYPE_VALUE THEN
              vcDatatype:=rDataset.vcValueType;
            ELSE
              vcDatatype:=rDataset.vcRangeType;
            END IF;
          END IF;
          IF io_rAxis.nAxisType=AXIS_TYPE_VALUE THEN
            PR_CALC_VALUE_MINMAX(i, j);
          ELSE
            PR_CALC_RANGE_MINMAX(i, j);
          END IF;
        END IF;
      END LOOP;
    END LOOP;
    IF io_rAxis.vcLabelText IS NOT NULL THEN
      -- set font for measuring
      PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcLabelFont,
                                    i_vcStyle    =>io_rAxis.vcLabelFontStyle,
                                    i_nSize      =>io_rAxis.nLabelFontSize
                                   );
    END IF;
    -- set measures
    -- set font for measuring
    PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcTickLabelFont,
                                  i_vcStyle    =>io_rAxis.vcTickLabelFontStyle,
                                  i_nSize      =>io_rAxis.nTickLabelFontSize
                                 );

    IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
      IF io_rAxis.nWidthType=VALUE_AUTOMATIC THEN
        IF NVL(io_rAxis.nTicklabelRotation,0) IN (90,270) THEN
          nXSpace:=AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
        ELSE
          nXSpace:=nMaxLen*cos(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0)));
        END IF;
        io_rAxis.nWidth:=GREATEST(ABS(nXSpace)+5, 5);
      END IF;
      -- Additional space if there is an axis-label
      IF io_rAxis.vcLabelText IS NOT NULL THEN
        io_rAxis.nWidth:=io_rAxis.nWidth+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
      END IF;
      -- Offsets for Data and Ticklabel
      io_rAxis.nWidth:=io_rAxis.nWidth+io_rAxis.nDataOffset+io_rAxis.nTickLabelOffset;
    ELSE
      IF io_rAxis.nHeightType=VALUE_AUTOMATIC THEN
        IF NVL(io_rAxis.nTicklabelRotation,0) IN (0,180) THEN
          nYSpace:=AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+2;
        ELSE
          nYSpace:=nMaxLen*sin(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0)));
        END IF;
        io_rAxis.nHeight:=GREATEST(ABS(nYSpace)+10, 10);
      END IF;
      -- Additional space if there is an axis-label
      IF io_rAxis.vcLabelText IS NOT NULL THEN
        io_rAxis.nHeight:=io_rAxis.nHeight+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
      END IF;
      -- Offsets for Data and Ticklabel
      io_rAxis.nHeight:=io_rAxis.nHeight+io_rAxis.nDataOffset+io_rAxis.nTickLabelOffset;
    END IF;
    -- set min and max-values
    IF vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
      io_rAxis.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER;
      IF io_rAxis.nMinValueType=VALUE_AUTOMATIC THEN
        IF nMinValue>0 AND io_rAxis.vcInclude0Value=PK_JRXML2PDF_TYPES.YES THEN
          nMinValue:=0;
        ELSE
          nMinValue:=nMinValue-ABS(nMinValue)*io_rAxis.nAdditionalPercent/100;
        END IF;
        io_rAxis.rMinValueDataEntry.nValue:=nMinValue;
      END IF;
      IF io_rAxis.nMaxValueType=VALUE_AUTOMATIC THEN
        nMaxValue:=nMaxValue+(nMaxValue-nMinValue)*io_rAxis.nAdditionalPercent/100;
        io_rAxis.rMaxValueDataEntry.nValue:=nMaxValue;
      ELSE
        io_rAxis.rMaxValueDataEntry.nValue:=io_rAxis.rMaxValueDataEntry.nValue+ABS(io_rAxis.rMaxValueDataEntry.nValue)*io_rAxis.nAdditionalPercent/100;
      END IF;
      io_rAxis.nMinMaxDiff:=io_rAxis.rMaxValueDataEntry.nValue-io_rAxis.rMinValueDataEntry.nValue;
    ELSIF vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
      io_rAxis.vcDatatype:=PK_JRXML2PDF_TYPES.DATATYPE_DATE;
      IF io_rAxis.nMinValueType=VALUE_AUTOMATIC THEN
        io_rAxis.rMinValueDataEntry.dtValue:=dtMinValue;
      END IF;
      IF io_rAxis.nMinValueType=VALUE_AUTOMATIC THEN
        dtMaxValue:=dtMaxValue+(dtMaxValue-dtMinValue)*io_rAxis.nAdditionalPercent/100;
        io_rAxis.rMaxValueDataEntry.dtValue:=dtMaxValue;
      END IF;
      io_rAxis.nMinMaxDiff:=io_rAxis.rMaxValueDataEntry.dtValue-io_rAxis.rMinValueDataEntry.dtValue;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_VALUE_ON_AXIS(i_rAxis       IN tAxis,
                                i_bSnapToEdge IN BOOLEAN,
                                i_nValue      IN NUMBER)
  RETURN NUMBER IS
    nReturn NUMBER;
  BEGIN
    IF i_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
      IF i_nValue<i_rAxis.rMinValueDataEntry.nValue THEN
        IF i_bSnapToEdge THEN
          nReturn:=i_rAxis.rMinValueDataEntry.nValue;
        END IF;
      ELSIF i_nValue>i_rAxis.rMaxValueDataEntry.nValue THEN
        IF i_bSnapToEdge THEN
          nReturn:=i_rAxis.rMaxValueDataEntry.nValue;
        END IF;
      ELSE
        nReturn :=i_nValue;
      END IF;
      IF nReturn IS NOT NULL THEN
        nReturn:=i_rAxis.nStartOffset+(nReturn-i_rAxis.rMinValueDataEntry.nValue)*i_rAxis.nPixelPerUnit;
      END IF;
    END IF;
    RETURN nReturn;
  END;

  -- ---------------------------------------------------------------------------

  FUNCTION FK_GET_VALUE_ON_AXIS(i_rAxis       IN tAxis,
                                i_bSnapToEdge IN BOOLEAN,
                                i_dtValue     IN DATE)
  RETURN NUMBER IS
    dtValue DATE;
    nReturn NUMBER;

  BEGIN
    IF i_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
      IF i_dtValue<i_rAxis.rMinValueDataEntry.dtValue THEN
        IF i_bSnapToEdge THEN
          dtValue:=i_rAxis.rMinValueDataEntry.dtValue;
        END IF;
      ELSIF i_dtValue>i_rAxis.rMaxValueDataEntry.dtValue THEN
        IF i_bSnapToEdge THEN
          dtValue:=i_rAxis.rMaxValueDataEntry.dtValue;
        END IF;
      ELSE
        dtValue:=i_dtValue;
      END IF;
      IF dtValue IS NOT NULL THEN
        nReturn:=i_rAxis.nStartOffset+(dtValue-i_rAxis.rMinValueDataEntry.dtValue)*i_rAxis.nPixelPerUnit;
      END IF;
    END IF;
    RETURN nReturn;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_AXIS(i_rChart           IN tChart,
                           io_rAxis           IN OUT NOCOPY tAxis,
                           i_nAxisIndex       IN NUMBER,
                           i_rChartRect       IN tRect) IS
    nTicks         NUMBER;
    nTickOffset    NUMBER;
    vcText         PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nTickValueDiff NUMBER;
    nX             NUMBER;
    nY             NUMBER;
    nValue         NUMBER;
    dtValue        DATE;
    vcIndex        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    lX             AS_PDF3_MOD.tVertices;
    lY             AS_PDF3_MOD.tVertices;
    nXBaseLabel    NUMBER;
    nYBaseLabel    NUMBER;
    nXAngle        NUMBER;
    nYAngle        NUMBER;

    PROCEDURE PR_RENDER_VALUE_AXIS IS
      iPos PLS_INTEGER;

      PROCEDURE PR_CALC_TICKS IS
      BEGIN
        IF io_rAxis.lTickEntries.COUNT>0 THEN
          vcIndex:=io_rAxis.lTickEntries.FIRST;
          WHILE vcIndex IS NOT NULL LOOP
            IF io_rAxis.lTickEntries(vcIndex).vcPattern IS NULL THEN
              io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
            END IF;
            vcIndex:=io_rAxis.lTickEntries.NEXT(vcIndex);
          END LOOP;
        ELSIF io_rAxis.nTickOffset IS NOT NULL THEN
          IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
            WHILE nValue<=io_rAxis.rMaxValueDataEntry.nValue LOOP
              vcIndex:=TO_CHAR(nValue, 'FM00000000000000000000D00000000000000000');
              io_rAxis.lTickEntries(vcIndex).rDataEntry.nValue:=nValue;
              io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
              nValue:=nValue+io_rAxis.nTickOffset;
            END LOOP;
          ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
            WHILE dtValue<=io_rAxis.rMaxValueDataEntry.dtValue LOOP
              vcIndex:=TO_CHAR(dtValue, 'YYYYMMDDHH24MISS');
              io_rAxis.lTickEntries(vcIndex).rDataEntry.dtValue:=dtValue;
              io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
              dtValue:=dtValue+io_rAxis.nTickOffset;
            END LOOP;
          END IF;
        ELSE
          -- automatic ticks
          IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
            IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
              io_rAxis.lTickEntries:=FK_CALC_NUM_TICK_ENTRIES(i_nMinValue =>io_rAxis.rMinValueDataEntry.nValue,
                                                              i_nMaxValue =>io_rAxis.rMaxValueDataEntry.nValue,
                                                              i_nSpace    =>i_rChartRect.nHeight,
                                                              i_nTickSpace=>io_rAxis.nTickSpace,
                                                              i_vcPattern =>io_rAxis.vcTickLabelPattern
                                                             );
            ELSE
              io_rAxis.lTickEntries:=FK_CALC_NUM_TICK_ENTRIES(i_nMinValue =>io_rAxis.rMinValueDataEntry.nValue,
                                                              i_nMaxValue =>io_rAxis.rMaxValueDataEntry.nValue,
                                                              i_nSpace    =>i_rChartRect.nWidth,
                                                              i_nTickSpace=>io_rAxis.nTickSpace,
                                                              i_vcPattern =>io_rAxis.vcTickLabelPattern
                                                             );
            END IF;
          ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
            IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
              io_rAxis.lTickEntries:=FK_CALC_DATE_TICK_ENTRIES(i_dtMinValue =>io_rAxis.rMinValueDataEntry.dtValue,
                                                               i_dtMaxValue =>io_rAxis.rMaxValueDataEntry.dtValue,
                                                               i_nSpace     =>i_rChartRect.nHeight,
                                                               i_nTickSpace=>io_rAxis.nTickSpace,
                                                               i_vcPattern  =>io_rAxis.vcTickLabelPattern
                                                              );
            ELSE
              io_rAxis.lTickEntries:=FK_CALC_DATE_TICK_ENTRIES(i_dtMinValue =>io_rAxis.rMinValueDataEntry.dtValue,
                                                               i_dtMaxValue =>io_rAxis.rMaxValueDataEntry.dtValue,
                                                               i_nSpace     =>i_rChartRect.nWidth,
                                                               i_nTickSpace=>io_rAxis.nTickSpace,
                                                               i_vcPattern  =>io_rAxis.vcTickLabelPattern
                                                              );
            END IF;
          END IF;
        END IF;
      END;

      PROCEDURE PR_VERTICAL_TICKS IS
        nXSpace NUMBER;
        nYSpace NUMBER;
      BEGIN
        IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>io_rAxis,
                                                   i_bSnapToEdge =>FALSE,
                                                   i_nValue      =>io_rAxis.lTickEntries(vcIndex).rdataEntry.nValue);
        END IF;
        IF nY IS NOT NULL THEN
          IF io_rAxis.vcShowTickMarks=PK_JRXML2PDF_TYPES.YES THEN
            AS_PDF3_MOD.horizontal_line(p_x          =>CASE WHEN io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                                                         nTickOffset-io_rAxis.nDataOffset
                                                       ELSE
                                                         nTickOffset+io_rAxis.nDataOffset
                                                       END,
                                        p_y          =>nY,
                                        p_width      =>2,
                                        p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                                        p_line_color =>io_rAxis.vcLineColor
                                     );
          END IF;
          IF io_rAxis.vcShowDotlines=PK_JRXML2PDF_TYPES.YES THEN
            IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
              IF io_rAxis.vcDotlineColor IS NOT NULL THEN
                -- dotted lines
                AS_PDF3_MOD.pr_line      (i_nX1        =>i_rChartRect.nX,
                                          i_nY1        =>nY,
                                          i_nX2        =>i_rChartRect.nX+10.5,
                                          i_nY2        =>nY+10.5,
                                          i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                          i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                          i_vcStroke   =>'[3] 0'
                                         );
                -- dotted line
                AS_PDF3_MOD.pr_line      (i_nX1        =>i_rChartRect.nX+10.5,
                                          i_nY1        =>nY+10.5,
                                          i_nX2        =>i_rChartRect.nX+i_rChartRect.nWidth+10.5,
                                          i_nY2        =>nY+10.5,
                                          i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                          i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                          i_vcStroke   =>'[3] 0'
                                         );
              END IF;
            ELSE
              IF     io_rAxis.vcDotlineColor IS NOT NULL THEN
                -- dotted line
                AS_PDF3_MOD.pr_line      (i_nX1        =>i_rChartRect.nX,
                                          i_nY1        =>nY,
                                          i_nX2        =>i_rChartRect.nX+i_rChartRect.nWidth,
                                          i_nY2        =>nY,
                                          i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                          i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                          i_vcStroke   =>'[3] 0'
                                         );
              END IF;
            END IF;
          END IF;
          IF io_rAxis.vcShowTickLabels=PK_JRXML2PDF_TYPES.YES THEN
            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcTickLabelFont,
                                          i_vcStyle    =>io_rAxis.vcTickLabelFontStyle,
                                          i_nSize      =>io_rAxis.nTickLabelFontSize
                                         );


            IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
              vcText:=TRIM(TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.nValue, io_rAxis.lTickEntries(vcIndex).vcPattern, i_rChart.rLocaleData.vcNumericChars));
            ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
              vcText:=TRIM(TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.dtValue, io_rAxis.lTickEntries(vcIndex).vcPattern, i_rChart.rLocaleData.vcDateLanguage));
            END IF;
            IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
              nx:=nTickOffset-AS_PDF3_MOD.str_len(vcText)-2;
            ELSE
              nx:=nTickOffset+3;
            END IF;

            AS_PDF3_MOD.set_color(io_rAxis.vcTickLabelColor);
            -- calculate end-point when rotation is set
            IF NVL(io_rAxis.nTicklabelRotation,0)=0 THEN
              nXBaseLabel:=nx;
              nYBaseLabel:=nY-(AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2);
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=180 THEN
              nXBaseLabel:=nx+AS_PDF3_MOD.str_len(vcText);
              nYBaseLabel:=nY+(AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2*0.8);
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=90 THEN
              IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                nXBaseLabel:=nTickOffset-2-io_rAxis.nTickLabelOffset;
              ELSE
                nXBaseLabel:=nTickOffset+3+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+io_rAxis.nTickLabelOffset;
              END IF;
              nYBaseLabel:=nY-AS_PDF3_MOD.str_len(vcText)/2;
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=270 THEN
              IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                nXBaseLabel:=nTickOffset-2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)-io_rAxis.nTickLabelOffset;
              ELSE
                nXBaseLabel:=nTickOffset+3+io_rAxis.nTickLabelOffset;
              END IF;
              nYBaseLabel:=nY+AS_PDF3_MOD.str_len(vcText)/2;
            ELSE
              -- calculate space of rotated text
              nXSpace:=GREATEST(AS_PDF3_MOD.str_len(vcText)*cos(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0))), AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize));
              nYSpace:=GREATEST(AS_PDF3_MOD.str_len(vcText)*sin(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0))), AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize));

              IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                nX:=nTickOffset-2-nXSpace-io_rAxis.nTickLabelOffset;
                nXBaseLabel:=nx;
                nYBaseLabel:=ny;

                IF    io_rAxis.nTicklabelRotation <90 THEN
                  nXBaseLabel:=nTickOffset-2;
                  nYBaseLabel:=nY;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 90 AND 180 THEN
                  nXBaseLabel:=nx;
                  nYBaseLabel:=nY+nYSpace;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 180 AND 270 THEN
                  nXBaseLabel:=nTickOffset-5;
                  nYBaseLabel:=nY+5;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 270 AND 360 THEN
                  nXBaseLabel:=nx;
                  nYBaseLabel:=nY+nYSpace;
                END IF;
              ELSE
                nX:=nTickOffset+nXSpace+io_rAxis.nTickLabelOffset;
                nXBaseLabel:=nx;
                nYBaseLabel:=ny;
                IF    io_rAxis.nTicklabelRotation <90 THEN
                  nXBaseLabel:=nX+4;
                  nYBaseLabel:=nY+nYSpace-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 90 AND 180 THEN
                  nXBaseLabel:=nx-nXSpace+4;
                  nYBaseLabel:=nY+4;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 180 AND 270 THEN
                  nXBaseLabel:=nX+4;
                  nYBaseLabel:=nY+nYSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                ELSIF io_rAxis.nTicklabelRotation BETWEEN 270 AND 360 THEN
                  nXBaseLabel:=nx-nXSpace;
                  nYBaseLabel:=nY;
                END IF;

              END IF;
              nXAngle:=nXBaseLabel+AS_PDF3_MOD.str_len(vcText)*cos(FK_RADIANS(io_rAxis.nTicklabelRotation));
              nYAngle:=nYBaseLabel+AS_PDF3_MOD.str_len(vcText)*sin(FK_RADIANS(io_rAxis.nTicklabelRotation));
              IF    io_rAxis.nTicklabelRotation <180 THEN
                nXBaseLabel:=nXBaseLabel-(nXAngle-nXBaseLabel);
                nYBaseLabel:=nYBaseLabel-(nYAngle-nYBaseLabel);
              END IF;
            END IF;
            AS_PDF3_MOD.put_txt(p_x               =>CASE WHEN io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                                                      nXBaseLabel-io_rAxis.nDataOffset-io_rAxis.nTickLabelOffset
                                                    ELSE
                                                      nXBaseLabel+io_rAxis.nDataOffset+io_rAxis.nTickLabelOffset
                                                    END,
                                p_y               =>nYBaseLabel,
                                p_txt             =>vcText,
                                p_degrees_rotation=>io_rAxis.nTicklabelRotation);
          END IF;
        END IF;
      END;

      PROCEDURE PR_HORIZONTAL_TICKS IS
        nXSpace NUMBER;
        nYSpace NUMBER;
      BEGIN
        IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nX:=i_rChartRect.nX+FK_GET_VALUE_ON_AXIS(i_rAxis       =>io_rAxis,
                                                   i_bSnapToEdge =>FALSE,
                                                   i_nValue      =>io_rAxis.lTickEntries(vcIndex).rdataEntry.nValue);
        ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nX:=i_rChartRect.nX+FK_GET_VALUE_ON_AXIS(i_rAxis       =>io_rAxis,
                                                   i_bSnapToEdge =>FALSE,
                                                   i_dtValue     =>io_rAxis.lTickEntries(vcIndex).rdataEntry.dtValue);
        END IF;
        IF io_rAxis.vcShowTickMarks=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.vertical_line(p_x          =>nX,
                                    p_y          =>nY,
                                    p_height     =>CASE WHEN io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
                                                     -2
                                                   ELSE
                                                     2
                                                   END,
                                    p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                                    p_line_color =>io_rAxis.vcLineColor
                                   );
        END IF;
        IF io_rAxis.vcShowDotlines=PK_JRXML2PDF_TYPES.YES THEN
          IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
            IF io_rAxis.vcDotlineColor IS NOT NULL THEN
              -- dotted lines
              AS_PDF3_MOD.pr_line      (i_nX1        =>nX,
                                        i_nY1        =>i_rChartRect.nY,
                                        i_nX2        =>nX+10.5,
                                        i_nY2        =>i_rChartRect.nY+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke   =>'[3] 0'
                                       );
              -- dotted line
              AS_PDF3_MOD.pr_line      (i_nX1        =>nX+10.5,
                                        i_nY1        =>i_rChartRect.nY+10.5,
                                        i_nX2        =>nX+10.5,
                                        i_nY2        =>i_rChartRect.nY+i_rChartRect.nHeight+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke   =>'[3] 0'
                                       );
            END IF;
          ELSE
            IF     io_rAxis.vcDotlineColor IS NOT NULL THEN
              -- dotted line
              AS_PDF3_MOD.pr_line      (i_nX1        =>nX,
                                        i_nY1        =>i_rChartRect.nY,
                                        i_nX2        =>nX,
                                        i_nY2        =>i_rChartRect.nY+i_rChartRect.nHeight,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor=>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke   =>'[3] 0'
                                       );
            END IF;
          END IF;
        END IF;
        IF io_rAxis.vcShowTickLabels=PK_JRXML2PDF_TYPES.YES THEN
          PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcTickLabelFont,
                                        i_vcStyle    =>io_rAxis.vcTickLabelFontStyle,
                                        i_nSize      =>io_rAxis.nTickLabelFontSize
                                       );

          IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
            vcText:=TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.nValue, io_rAxis.lTickEntries(vcIndex).vcPattern, i_rChart.rLocaleData.vcNumericChars);
          ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
            vcText:=TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.dtValue, io_rAxis.lTickEntries(vcIndex).vcPattern, i_rChart.rLocaleData.vcDateLanguage);
          END IF;

          IF NVL(io_rAxis.nTicklabelRotation,0)=0 THEN
            nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-(AS_PDF3_MOD.str_len(vcText)/2);
            IF io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
              nYBaseLabel:=nY-2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            ELSE
              nYBaseLabel:=nY-2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            END IF;
          ELSIF NVL(io_rAxis.nTicklabelRotation,0)=180 THEN
            nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+(AS_PDF3_MOD.str_len(vcText)/2);
            IF io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
              nYBaseLabel:=nY+2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            ELSE
              nYBaseLabel:=nY+2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            END IF;
          ELSIF NVL(io_rAxis.nTicklabelRotation,0)=90 THEN
            nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
            IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
              nYBaseLabel:=nY+3;
            ELSE
              nYBaseLabel:=nY-AS_PDF3_MOD.str_len(vcText)-3;
            END IF;
          ELSIF NVL(io_rAxis.nTicklabelRotation,0)=270 THEN
            nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/4;
            IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
              nYBaseLabel:=nY+AS_PDF3_MOD.str_len(vcText)+2;
            ELSE
              nYBaseLabel:=nY-3;
            END IF;
          ELSE
            -- calculate space of rotated text
            nXSpace:=GREATEST(AS_PDF3_MOD.str_len(vcText)*cos(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0))), AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize));
            nYSpace:=GREATEST(AS_PDF3_MOD.str_len(vcText)*sin(FK_RADIANS(NVL(io_rAxis.nTicklabelRotation,0))), AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize));

            IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
              IF    io_rAxis.nTicklabelRotation <90 THEN
                nXBaseLabel:=nx+nXSpace;
                nYBaseLabel:=nY+nYSpace+2;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 90 AND 180 THEN
                nXBaseLabel:=nx-nXSpace+2;
                nYBaseLabel:=nY+nYSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 180 AND 270 THEN
                nXBaseLabel:=nx+nXSpace;
                nYBaseLabel:=nY+nySpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+2;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 270 AND 360 THEN
                nXBaseLabel:=nx-nXSpace;
                nYBaseLabel:=nY+nySpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
              END IF;
            ELSE
              IF    io_rAxis.nTicklabelRotation <90 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                nYBaseLabel:=nY-8;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 90 AND 180 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2;
                nYBaseLabel:=nY-5;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 180 AND 270 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2;
                nYBaseLabel:=nY-5;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 270 AND 360 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                nYBaseLabel:=nY-8;
              END IF;
            END IF;
            nXAngle:=nXBaseLabel+AS_PDF3_MOD.str_len(vcText)*cos(FK_RADIANS(io_rAxis.nTicklabelRotation));
            nYAngle:=nYBaseLabel+AS_PDF3_MOD.str_len(vcText)*sin(FK_RADIANS(io_rAxis.nTicklabelRotation));
            IF    io_rAxis.nTicklabelRotation <180 THEN
              nXBaseLabel:=nXBaseLabel-(nXAngle-nXBaseLabel);
              nYBaseLabel:=nYBaseLabel-(nYAngle-nYBaseLabel);
            END IF;
          END IF;

          AS_PDF3_MOD.set_color(io_rAxis.vcTickLabelColor);

          AS_PDF3_MOD.put_txt(p_x               =>nXBaseLabel,
                              p_y               =>CASE WHEN io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
                                                    nYBaseLabel-io_rAxis.nTickLabelOffset
                                                  ELSE
                                                    nYBaseLabel+io_rAxis.nTickLabelOffset
                                                  END,
                              p_txt             =>vcText,
                              p_degrees_rotation=>io_rAxis.nTicklabelRotation);
        END IF;
      END;
    BEGIN
      PR_CALC_TICKS;
      vcIndex:=io_rAxis.lTickEntries.FIRST;
      iPos:=1;
      WHILE vcIndex IS NOT NULL LOOP
        IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
          PR_VERTICAL_TICKS;
        ELSE
          PR_HORIZONTAL_TICKS;
        END IF;
        vcIndex:=io_rAxis.lTickEntries.NEXT(vcIndex);
        iPos:=iPos+1;
      END LOOP;
    END;

    PROCEDURE PR_RENDER_CATEGORY_AXIS IS
      rDataSet tDataset;
      rEntry   tDataEntry;
      iPos     PLS_INTEGER;
    BEGIN
      -- Render one ticket for each value
      FOR i IN 1..i_rChart.lPlots.COUNT LOOP
        FOR j IN 1..i_rChart.lPlots(i).lDatasets.COUNT LOOP
          IF    (    i_rChart.lPlots(i).lDatasets(j).nValueAxis=i_nAxisIndex
                 AND io_rAxis.nAxisType=AXIS_TYPE_VALUE
                )
             OR (    i_rChart.lPlots(i).lDatasets(j).nRangeAxis=i_nAxisIndex
                 AND io_rAxis.nAxisType=AXIS_TYPE_RANGE
                ) THEN
            rDataSet:=i_rChart.lPlots(i).lDatasets(j);
            FOR t IN 1..i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries.COUNT LOOP
              rEntry:=i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t);
              IF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
                vcIndex:=TO_CHAR(rEntry.nValue, 'FM00000000000000000000D00000000000000000');
                io_rAxis.lTickEntries(vcIndex).rDataEntry.nValue:=rEntry.nValue;
                io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
              ELSIF rDataset.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
                vcIndex:=TO_CHAR(rEntry.dtValue, 'YYYYMMDDHH24MISS');
                io_rAxis.lTickEntries(vcIndex).rDataEntry.dtValue:=rEntry.dtValue;
                io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
              ELSE
                -- sort by appearence
                -- check for existance
                vcIndex:=io_rAxis.lTickEntries.FIRST;
                WHILE vcIndex IS NOT NULL LOOP
                  IF io_rAxis.lTickEntries(vcIndex).rDataEntry.vcValue=i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).vcValue THEN
                    EXIT;
                  END IF;
                  vcIndex:=io_rAxis.lTickEntries.NEXT(vcIndex);
                END LOOP;
                IF vcIndex IS NULL THEN
                  vcIndex:=TO_CHAR(io_rAxis.lTickEntries.COUNT+1, 'FM00000000000000000000');
                  io_rAxis.lTickEntries(vcIndex).rDataEntry.vcValue:=i_rChart.lPlots(i).lDatasets(j).lRangeDataEntries(t).vcValue;
                  io_rAxis.lTickEntries(vcIndex).vcPattern:=io_rAxis.vcTickLabelPattern;
                END IF;
              END IF;
            END LOOP;
          END IF;
        END LOOP;
      END LOOP;

      vcIndex:=io_rAxis.lTickEntries.FIRST;
      iPos:=1;
      WHILE vcIndex IS NOT NULL LOOP
        IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
          NULL;
        ELSE
          io_rAxis.nPixelPerUnit:=(i_rChartRect.nWidth-20)/io_rAxis.lTickEntries.COUNT;
          nX:=i_rChartRect.nX+10+iPos*io_rAxis.nPixelPerUnit;
          IF io_rAxis.vcShowTickMarks=PK_JRXML2PDF_TYPES.YES THEN
            AS_PDF3_MOD.vertical_line(p_x          =>nX,
                                      p_y          =>nY-io_rAxis.nTickLabelOffset,
                                      p_height     =>-2,
                                      p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                                      p_line_color =>io_rAxis.vcLineColor
                                     );
          END IF;
          IF io_rAxis.vcShowDotlines=PK_JRXML2PDF_TYPES.YES THEN
            IF     io_rAxis.vcDotlineColor IS NOT NULL THEN
              -- dotted line
              AS_PDF3_MOD.pr_line      (i_nX1         =>nX,
                                        i_nY1         =>i_rChartRect.nY,
                                        i_nX2         =>nX,
                                        i_nY2         =>i_rChartRect.nY+i_rChartRect.nHeight,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor =>REPLACE(io_rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke     =>'[3] 0'
                                       );
            END IF;
          END IF;
          IF io_rAxis.vcShowTickLabels=PK_JRXML2PDF_TYPES.YES THEN
            IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
              vcText:=TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.nValue, io_rAxis.lTickEntries(vcIndex).vcPattern);
            ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
              vcText:=TO_CHAR(io_rAxis.lTickEntries(vcIndex).rdataEntry.dtValue, io_rAxis.lTickEntries(vcIndex).vcPattern);
            ELSE
              vcText:=io_rAxis.lTickEntries(vcIndex).rdataEntry.vcValue;
            END IF;

            AS_PDF3_MOD.set_color(io_rAxis.vcTickLabelColor);

            PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcTickLabelFont,
                                          i_vcStyle    =>io_rAxis.vcTickLabelFontStyle,
                                          i_nSize      =>io_rAxis.nTickLabelFontSize
                                         );

            -- calculate end-point when rotation is set
            IF NVL(io_rAxis.nTicklabelRotation,0)=0 THEN
              nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-(AS_PDF3_MOD.str_len(vcText)/2);
              nYBaseLabel:=nY-2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=180 THEN
              nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+(AS_PDF3_MOD.str_len(vcText)/2);
              nYBaseLabel:=nY-(AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2*0.8);
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=90 THEN
              nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
              IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                nYBaseLabel:=nY+3;
              ELSE
                nYBaseLabel:=nY-AS_PDF3_MOD.str_len(vcText)-3;
              END IF;
            ELSIF NVL(io_rAxis.nTicklabelRotation,0)=270 THEN
              nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
              IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
                nYBaseLabel:=nY+AS_PDF3_MOD.str_len(vcText)+3;
              ELSE
                nYBaseLabel:=nY-3;
              END IF;
            ELSE
              IF    io_rAxis.nTicklabelRotation <90 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                nYBaseLabel:=nY-8;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 90 AND 180 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2;
                nYBaseLabel:=nY-5;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 180 AND 270 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2;
                nYBaseLabel:=nY-5;
              ELSIF io_rAxis.nTicklabelRotation BETWEEN 270 AND 360 THEN
                nXBaseLabel:=nx-io_rAxis.nPixelPerUnit/2-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2;
                nYBaseLabel:=nY-8;
              END IF;
              nXAngle:=nXBaseLabel+AS_PDF3_MOD.str_len(vcText)*cos(FK_RADIANS(io_rAxis.nTicklabelRotation));
              nYAngle:=nYBaseLabel+AS_PDF3_MOD.str_len(vcText)*sin(FK_RADIANS(io_rAxis.nTicklabelRotation));
              IF    io_rAxis.nTicklabelRotation <180 THEN
                nXBaseLabel:=nXBaseLabel-(nXAngle-nXBaseLabel);
                nYBaseLabel:=nYBaseLabel-(nYAngle-nYBaseLabel);
              END IF;
            END IF;

            AS_PDF3_MOD.put_txt(p_x               =>nXBaseLabel,
                                p_y               =>nYBaseLabel-io_rAxis.nDataOffset-io_rAxis.nTickLabelOffset,
                                p_txt             =>vcText,
                                p_degrees_rotation=>io_rAxis.nTicklabelRotation);
         END IF;
        END IF;
        vcIndex:=io_rAxis.lTickEntries.NEXT(vcIndex);
        iPos:=iPos+1;
      END LOOP;
    END;

    PROCEDURE PR_RENDER_VERTICAL IS
    BEGIN
      io_rAxis.nPixelPerUnit:=(i_rChartRect.nHeight-io_rAxis.nStartOffset-io_rAxis.nEndOffset)/io_rAxis.nMinMaxDiff;
      nY:=i_rChartRect.nY;
      -- Render axis-label, if set
      IF io_rAxis.vcLabelText IS NOT NULL THEN

        -- set font for measuring
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcLabelFont,
                                      i_vcStyle    =>io_rAxis.vcLabelFontStyle,
                                      i_nSize      =>io_rAxis.nLabelFontSize
                                     );
        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          nX:=io_rAxis.nX+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
        ELSE
          nX:=io_rAxis.nX+io_rAxis.nWidth-5;
        END IF;

        AS_PDF3_MOD.set_color(io_rAxis.vcLabelColor);
        AS_PDF3_MOD.put_txt(p_x               =>nX,
                            p_y               =>nY+(i_rChartRect.nHeight-AS_PDF3_MOD.str_len(io_rAxis.vcLabelText))/2,
                            p_txt             =>io_rAxis.vcLabelText,
                            p_degrees_rotation=>90);
      END IF;

      IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
        nX:=io_rAxis.nX+io_rAxis.nWidth;
        nTickOffset:=io_rAxis.nX+io_rAxis.nWidth-2;
      ELSE
        nX:=io_rAxis.nX;
        nTickOffset:=io_rAxis.nX;
      END IF;

      IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
        -- buildd up polygon
        lX(1):=nX+.5;     lY(1):=nY+.5;
        lX(2):=nX+10.5;   lY(2):=nY+10.5;
        lX(3):=nX+10.5;   lY(3):=nY+10.5+i_rChartRect.nHeight;
        lX(4):=nX+.5;     lY(4):=nY+.5+i_rChartRect.nHeight;
        lX(5):=nX+.5;     lY(5):=nY+.5;
        AS_PDF3_MOD.pr_polygon( lX,
                                lY,
                                'EEEEEE',
                                'EEEEEE'
                              );
        AS_PDF3_MOD.vertical_line(p_x          =>nX+10.5,
                                  p_y          =>nY+10.5,
                                  p_height     =>i_rChartRect.nHeight,
                                  p_line_width =>1,
                                  p_line_color =>REPLACE('C0C0C0', '#', '')
                                 );

        AS_PDF3_MOD.pr_line(i_nX1        =>nX,
                            i_nY1        =>nY,
                            i_nX2        =>nX+10.5,
                            i_nY2        =>nY+10.5,
                            i_nLineWidth =>1,
                            i_vcLineColor=>REPLACE('C0C0C0', '#', '')
                           );


      ELSE
        -- Axis line only when not 3D
        AS_PDF3_MOD.vertical_line(p_x          =>nX,
                                  p_y          =>nY,
                                  p_height     =>i_rChartRect.nHeight,
                                  p_line_width =>io_rAxis.nAxisLineWidth,
                                  p_line_color =>CASE WHEN io_rAxis.nDataOffset>0 THEN
                                                   REPLACE('C0C0C0', '#', '')
                                                 ELSE
                                                   io_rAxis.vcLineColor
                                                 END
                                 );
      END IF;
      IF io_rAxis.nDataOffset>0 THEN
        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          nX:=nX-io_rAxis.nDataOffset;
        ELSE
          nX:=nX+io_rAxis.nDataOffset;
        END IF;
        -- Axis line only when not 3D
        AS_PDF3_MOD.vertical_line(p_x          =>nX,
                                  p_y          =>nY,
                                  p_height     =>i_rChartRect.nHeight,
                                  p_line_width =>io_rAxis.nAxisLineWidth,
                                  p_line_color =>io_rAxis.vcLineColor
                                 );
      END IF;
      nTicks:=TRUNC(i_rChartRect.nHeight/PIXEL_PER_TICK);
    END;

    PROCEDURE PR_RENDER_HORIZONTAL IS
    BEGIN
      io_rAxis.nPixelPerUnit:=(i_rChartRect.nWidth-io_rAxis.nStartOffset-io_rAxis.nEndOffset)/io_rAxis.nMinMaxDiff;
      nX:=i_rChartRect.nX;

      -- Render axis-label, if set
      IF io_rAxis.vcLabelText IS NOT NULL THEN

        -- set font for measuring
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>io_rAxis.vcLabelFont,
                                      i_vcStyle    =>io_rAxis.vcLabelFontStyle,
                                      i_nSize      =>io_rAxis.nLabelFontSize
                                     );

        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          ny:=io_rAxis.nY-5-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
        ELSE
          ny:=io_rAxis.nY-io_rAxis.nHeight+10;
        END IF;

        AS_PDF3_MOD.set_color(io_rAxis.vcLabelColor);
        AS_PDF3_MOD.put_txt(p_x               =>nX+(i_rChartRect.nWidth-AS_PDF3_MOD.str_len(io_rAxis.vcLabelText))/2,
                            p_y               =>nY,
                            p_txt             =>io_rAxis.vcLabelText);
      END IF;

      IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
        nY:=io_rAxis.nY-io_rAxis.nHeight;
      ELSE
        nY:=io_rAxis.nY;
      END IF;
      IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
        -- buildd up polygon
        lX(1):=nX+.5;                         lY(1):=nY+.5;
        lX(2):=nX+10.5;                       lY(2):=nY+10.5;
        lX(3):=nX+10.5+i_rChartRect.nWidth;   lY(3):=nY+10.5;
        lX(4):=nX+.5+i_rChartRect.nWidth;     lY(4):=nY+.5;
        lX(5):=nX+.5;                         lY(5):=nY+.5;
        AS_PDF3_MOD.pr_polygon( lX,
                                lY,
                                'EEEEEE',
                                'EEEEEE'
                              );
        AS_PDF3_MOD.horizontal_line(p_x          =>nX+10.5,
                                    p_y          =>nY+10.5,
                                    p_width      =>i_rChartRect.nWidth,
                                    p_line_width =>1,
                                    p_line_color =>REPLACE('C0C0C0', '#', '')
                                   );

        AS_PDF3_MOD.pr_line(i_nX1          =>nX,
                            i_nY1          =>nY,
                            i_nX2          =>nX+10.5,
                            i_nY2          =>nY+10.5,
                            i_nLineWidth =>1,
                            i_vcLineColor =>REPLACE('C0C0C0', '#', '')
                           );
      ELSE
        -- render axis-line only in 2D
        AS_PDF3_MOD.horizontal_line(p_x          =>nX,
                                    p_y          =>nY,
                                    p_width      =>i_rChartRect.nWidth,
                                    p_line_width =>io_rAxis.nAxisLineWidth,
                                    p_line_color =>CASE WHEN io_rAxis.nDataOffset>0 THEN
                                                     REPLACE('C0C0C0', '#', '')
                                                   ELSE
                                                     REPLACE(io_rAxis.vcLineColor, '#', '')
                                                   END
                                 );
      END IF;
      IF (io_rAxis.nDataOffset>0) THEN
        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          nY:=nY+io_rAxis.nDataOffset;
        ELSE
          nY:=nY-io_rAxis.nDataOffset;
        END IF;
        -- render axis-line only in 2D
        AS_PDF3_MOD.horizontal_line(p_x          =>nX,
                                    p_y          =>nY,
                                    p_width      =>i_rChartRect.nWidth,
                                    p_line_width =>io_rAxis.nAxisLineWidth,
                                    p_line_color =>REPLACE(io_rAxis.vcLineColor, '#', '')
                                 );
      END IF;
      nTicks:=TRUNC(i_rChartRect.nWidth/PIXEL_PER_TICK);
    END;
  BEGIN
    IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
      PR_RENDER_VERTICAL;
    ELSE
      PR_RENDER_HORIZONTAL;
    END IF;
    IF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
      nTickValueDiff:=(io_rAxis.rMaxValueDataEntry.nValue-io_rAxis.rMinValueDataEntry.nValue)/nTicks;
      nValue:=io_rAxis.rMinValueDataEntry.nValue;
    ELSIF io_rAxis.vcDatatype=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
      nTickValueDiff:=(io_rAxis.rMaxValueDataEntry.dtValue-io_rAxis.rMinValueDataEntry.dtValue)/nTicks;
      dtValue:=io_rAxis.rMinValueDataEntry.dtValue;
    END IF;
    -- font for axis-values
    PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>PK_JRXML2PDF_TYPES.ARIAL,
                                  i_vcStyle    =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                  i_nSize      =>11
                                 );
    IF io_rAxis.nValueType=AXIS_VALUE THEN
      PR_RENDER_VALUE_AXIS;
    ELSE
      PR_RENDER_CATEGORY_AXIS;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_CATEGORY_LINE_PLOT(i_rChart IN tChart,
                                         i_nPlotIndex IN NUMBER,
                                         i_rChartRect IN tRect
                                        ) IS
    nXLast         NUMBER;
    nYLast         NUMBER;
    lSeriesIndex   PK_JRXML2PDF_TYPES.tNumList;
    iSeries        PLS_INTEGER;
    nRangeAxis     NUMBER;
    vcIndex        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nSeriesNum     NUMBER;
    iRangeIndex    NUMBER;
    nY             NUMBER;
    rAxis          tAxis;
    lX             AS_PDF3_MOD.tVertices;
    lY             AS_PDF3_MOD.tVertices;
    nCategorySpace NUMBER;
    nSeriesSpace   NUMBER;
    PROCEDURE PR_RENDER_LINE(i_rDataset        IN tDataset,
                             i_rRangeTickEntry IN tTickEntry,
                             i_nRangeIndex     IN NUMBER,
                             i_nSeriesNum      IN NUMBER,
                             i_nSeriesCount    IN NUMBER) IS
      nX       NUMBER;
      nXFactor NUMBER;
      nY1      NUMBER;
      nY2      NUMBER;
      nSwap    NUMBER;
      nYFactor NUMBER;
      vcLineColor        PK_JRXML2PDF_TYPES.tColor;
      vcLineColorDark    PK_JRXML2PDF_TYPES.tColor;
      vcLineColorLight   PK_JRXML2PDF_TYPES.tColor;
      vcColor            PK_JRXML2PDF_TYPES.tColor;
      vcIndex            PK_JRXML2PDF_TYPES.tMaxVarchar2;
      iPos               PLS_INTEGER;
      lX                 AS_PDF3_MOD.tVertices;
      lY                 AS_PDF3_MOD.tVertices;
      vcText             PK_JRXML2PDF_TYPES.tMaxVarchar2;
      nXText             NUMBER;
      nYText             NUMBER;
      iDatasetIndex      PLS_INTEGER;
    BEGIN
      nXFactor:=i_rChart.lValueAxis(i_rDataset.nValueAxis).nPixelPerUnit;
      nYFactor:=i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit;
      vcLineColor:=i_rChart.lSeries(i_rDataset.nSeries).vcColor;
      vcLineColorDark:=FK_DARKER(vcLineColor);
      vcLineColorLight:=FK_LIGHTER(vcLineColor);
      -- Find entry in Range
      -- calc y-value and check Index in TickEntries
      FOR i IN 1..i_rDataset.lRangeDataEntries.COUNT LOOP
        IF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF i_rRangeTickEntry.rDataEntry.nValue=i_rDataset.lRangeDataEntries(i).nValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        ELSIF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          IF i_rRangeTickEntry.rDataEntry.dtValue=i_rDataset.lRangeDataEntries(i).dtValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        ELSE
          IF i_rRangeTickEntry.rDataEntry.vcValue=i_rDataset.lRangeDataEntries(i).vcValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        END IF;
        EXIT WHEN iDatasetIndex >0;
      END LOOP;

      IF iDatasetIndex>0 THEN
        -- calc x-value
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY1:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>i_rDataset.lValueDataEntries(iDatasetIndex).nValue
                                                  );

          nY2:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>0
                                                  );
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nY1:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(iDatasetIndex).dtValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.dtValue)*nXFactor;
        END IF;
      ELSE
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY1:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>0
                                                  );
          nY2:=nY1;
        END IF;
      END IF;
      IF i_nRangeIndex IS NOT NULL THEN
        nX:= i_rChartRect.nx
            +i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nStartOffset
            +(i_nRangeIndex-1)*i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit
            +i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit/2;

        IF i_rChart.lPlots(i_nPlotIndex).vcShowShapes=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.pr_path(i_lPath     =>FK_TRANSFORM_PATH(i_nX   =>nX,
                                                              i_nY   =>nY1,
                                                              i_lPath=>i_rChart.lSeries(i_rDataset.nSeries).lShapePath),
                              i_vcLineColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_vcFillColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                             );
        END IF;
        IF     nxLast IS NOT NULL
           AND i_rChart.lPlots(i_nPlotIndex).vcShowLines=PK_JRXML2PDF_TYPES.YES THEN
          IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
            lX.DELETE;
            lY.DELETE;
            lX(1):=nXLast+.5;        lY(1):=nYLast+.5;
            lX(2):=nXLast+10.5;      lY(2):=nYLast+10.5;
            lX(3):=nX+10.5;          lY(3):=nY1+10.5;
            lX(4):=nX+.5;            lY(4):=nY1+.5;
            lX(5):=nXLast+.5;        lY(5):=nYLast+.5;
            IF nY1!=nYLast THEN
              IF (nx-nXLast)/(nY1-nYLast)>1 THEN
                vcColor:=vcLineColorLight;
              ELSIF (nx-nXLast)/(nY1-nYLast) BETWEEN 0 AND 1 THEN
                vcColor:=vcLineColorDark;
              ELSIF (nx-nXLast)/(nY1-nYLast) BETWEEN 0 AND -1 THEN
                vcColor:=vcLineColorDark;
              ELSE
                vcColor:=vcLineColorLight;
              END IF;
            ELSE
              vcColor:=vcLineColorLight;
            END IF;

            as_pdf3_mod.pr_polygon(lX,
                                   lY,
                                   vcColor,
                                   vcColor
                                  );
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nxLast+10.5,
                                nyLast+10.5,
                                i_vcLineColor=>vcLineColor);
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny1,
                                i_vcLineColor=>vcLineColor);

          ELSE
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny1,
                                i_vcLineColor=>vcLineColor);
          END IF;
        END IF;
        nxLast:=nX;
        nYLast:=ny1;
      END IF;
      IF nY1>nY2 THEN
        nSwap:=nY2;
        nY2:=nY1;
        nY1:=nSwap;
      END IF;
      IF i_rChart.lPlots(i_nPlotIndex).nLabelPosition IN (VALUE_POSITION_INSIDE, VALUE_POSITION_OUTSIDE) THEN
        AS_PDF3_MOD.set_color(i_rChart.lPlots(i_nPlotIndex).vcLabelColor);
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>i_rChart.lPlots(i_nPlotIndex).vcLabelFont,
                                      i_vcStyle    =>i_rChart.lPlots(i_nPlotIndex).vcLabelFontStyle,
                                      i_nSize      =>i_rChart.lPlots(i_nPlotIndex).nLabelFontSize
                                     );

        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          vcText:=TO_CHAR(i_rDataset.lValueDataEntries(iDatasetIndex).nValue, i_rChart.lPlots(i_nPlotIndex).vcLabelPattern);
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          vcText:=TO_CHAR(i_rDataset.lValueDataEntries(iDatasetIndex).dtValue, i_rChart.lPlots(i_nPlotIndex).vcLabelPattern);
        END IF;
        IF i_rChart.lPlots(i_nPlotIndex).nLabelPosition=VALUE_POSITION_INSIDE THEN
          nYText:=(nY2-nY1)/2;
        ELSE
          IF nSwap IS NULL THEN
            nYText:=nY1-3-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
          ELSE
            -- swapped, then place text beneath bar
            nYText:=nY2+3;
          END IF;
        END IF;
        nxText:=nx+nCategorySpace/2-AS_PDF3_MOD.str_len(vcText)/2;
        IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES
           AND i_rChart.lPlots(i_nPlotIndex).nLabelPosition=VALUE_POSITION_OUTSIDE THEN
          nxText:=nxText+5;
        END IF;
        AS_PDF3_MOD.put_txt(p_x=>nXText,
                            p_y=>nYText,
                            p_txt=>vcText
                           );
      END IF;
    END;

  BEGIN
    FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
      IF i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries>0 THEN
        nRangeAxis:=i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nrangeAxis;
        lSeriesIndex(i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries):=i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries;
      END IF;
    END LOOP;
    IF lSeriesIndex.COUNT>0 THEN
      -- if the plot is 3D, render a bar-base at X-position 0
      IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
        -- find X-axis
        rAxis:=i_rChart.lValueaxis(i_rChart.lPlots(i_nPlotIndex).lDatasets(1).nValueAxis);
        nY:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>rAxis,
                                                 i_bSnapToEdge =>FALSE,
                                                 i_nValue      =>0
                                                );
        -- render bar-base
        IF rAxis.rMinValueDataEntry.nValue<0 AND rAxis.rMaxValueDataEntry.nValue>0 THEN
          lX(1):=i_rChartRect.nX+.5;                         lY(1):=nY+.5;
          lX(2):=i_rChartRect.nX+10.5;                       lY(2):=nY+10.5;
          lX(3):=i_rChartRect.nX+10.5+i_rChartRect.nWidth;   lY(3):=nY+10.5;
          lX(4):=i_rChartRect.nX+.5+i_rChartRect.nWidth;     lY(4):=nY+.5;
          lX(5):=i_rChartRect.nX+.5;                         lY(5):=nY+.5;
          AS_PDF3_MOD.pr_polygon( lX,
                                  lY,
                                  'EEEEEE',
                                  'EEEEEE'
                                );
          IF rAxis.vcShowDotlines=PK_JRXML2PDF_TYPES.YES THEN
            IF rAxis.vcDotlineColor IS NOT NULL THEN
              -- dotted lines
              AS_PDF3_MOD.pr_line      (i_nX1         =>i_rChartRect.nX,
                                        i_nY1         =>nY,
                                        i_nX2         =>i_rChartRect.nX+10.5,
                                        i_nY2         =>nY+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor =>REPLACE(rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke     =>'[3] 0'
                                       );
              -- dotted line
              AS_PDF3_MOD.pr_line      (i_nX1        =>i_rChartRect.nX+10.5,
                                        i_nY1        =>nY+10.5,
                                        i_nX2        =>i_rChartRect.nX+i_rChartRect.nWidth+10.5,
                                        i_nY2        =>nY+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor=>REPLACE(rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke   =>'[3] 0'
                                       );
            END IF;
          END IF;
        END IF;
      END IF;

      -- if spaces for category and series are not explicitly set, calculate percentage of space
      IF i_rChart.lPlots(i_nPlotIndex).nCategorySpace IS NULL THEN
        nCategorySpace:=i_rChart.lRangeAxis(nRangeAxis).nPixelPerUnit*0.20;
      ELSE
        nCategorySpace:=i_rChart.lPlots(i_nPlotIndex).nCategorySpace;
      END IF;
      IF i_rChart.lPlots(i_nPlotIndex).nSeriesSpace IS NULL THEN
        nSeriesSpace:=i_rChart.lRangeAxis(nRangeAxis).nPixelPerUnit*0.05;
      ELSE
        nSeriesSpace:=i_rChart.lPlots(i_nPlotIndex).nSeriesSpace;
      END IF;
      FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
        nXLast:=NULL;
        nYLast:=NULL;
        iRangeIndex:=1;
        vcIndex:=i_rChart.lRangeAxis(nRangeAxis).lTickEntries.FIRST;
        WHILE vcIndex IS NOT NULL LOOP
          nSeriesNum:=1;
          iSeries:=lSeriesIndex.FIRST;
          WHILE iSeries IS NOT NULL LOOP
            IF i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries=iSeries THEN
              PR_RENDER_LINE(i_rChart.lPlots(i_nPlotIndex).lDatasets(i),
                             i_rChart.lRangeAxis(nRangeAxis).lTickEntries(vcIndex),
                             iRangeIndex,
                             nSeriesNum,
                             lSeriesIndex.COUNT
                            );
            END IF;
            nSeriesNum:=nSeriesNum+1;
            iSeries:=lSeriesIndex.NEXT(iSeries);
          END LOOP;
          iRangeIndex:=iRangeIndex+1;
          vcIndex:=i_rChart.lRangeAxis(nRangeAxis).lTickEntries.NEXT(vcIndex);
        END LOOP;
      END LOOP;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_CATEGORY_BAR_PLOT(i_rChart IN tChart,
                                        i_nPlotIndex IN NUMBER,
                                        i_rChartRect IN tRect
                                       ) IS

    lSeriesIndex   PK_JRXML2PDF_TYPES.tNumList;
    iSeries        PLS_INTEGER;
    nRangeAxis     NUMBER;
    vcIndex        PK_JRXML2PDF_TYPES.tMaxVarchar2;
    nSeriesNum     NUMBER;
    iRangeIndex    NUMBER;
    nY             NUMBER;
    rAxis          tAxis;
    lX             AS_PDF3_MOD.tVertices;
    lY             AS_PDF3_MOD.tVertices;
    nCategorySpace NUMBER;
    nSeriesSpace   NUMBER;
    nSeriesCount   NUMBER;
    lLastValues    PK_JRXML2PDF_TYPES.tNumList;
    
    PROCEDURE PR_RENDER_BAR(i_rDataset        IN tDataset,
                            i_rRangeTickEntry IN tTickEntry,
                            i_nRangeIndex     IN NUMBER,
                            i_nSeriesNum      IN NUMBER,
                            i_nSeriesCount    IN NUMBER) IS
      nX                 NUMBER;
      nXFactor           NUMBER;
      nY1                NUMBER;
      nY2                NUMBER;
      nSwap              NUMBER;
      nYFactor           NUMBER;
      nxLast             NUMBER;
      nYLast             NUMBER;
      vcBarColor         PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcDarkerBarColor   PK_JRXML2PDF_TYPES.tColor;
      vcVeryDarkBarColor PK_JRXML2PDF_TYPES.tColor;
      vcIndex            PK_JRXML2PDF_TYPES.tMaxVarchar2;
      iPos               PLS_INTEGER;
      nBarWidth          NUMBER;
      lX                 AS_PDF3_MOD.tVertices;
      lY                 AS_PDF3_MOD.tVertices;
      vcText             PK_JRXML2PDF_TYPES.tMaxVarchar2;
      nXText             NUMBER;
      nYText             NUMBER;
      iDatasetIndex      PLS_INTEGER;
      nValue             NUMBER;
      nMinValue          NUMBER;
    BEGIN
      nXFactor:=i_rChart.lValueAxis(i_rDataset.nValueAxis).nPixelPerUnit;
      nYFactor:=i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit;
      vcBarColor:=i_rChart.lSeries(i_rDataset.nSeries).vcColor;
      vcDarkerBarColor:=FK_DARKER(vcBarColor);
      vcVeryDarkBarColor:=FK_DARKER(vcDarkerBarColor);
      -- Find entry in Range
      -- calc y-value and check Index in TickEntries
      FOR i IN 1..i_rDataset.lRangeDataEntries.COUNT LOOP
        IF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          IF i_rRangeTickEntry.rDataEntry.nValue=i_rDataset.lRangeDataEntries(i).nValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        ELSIF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          IF i_rRangeTickEntry.rDataEntry.dtValue=i_rDataset.lRangeDataEntries(i).dtValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        ELSE
          IF i_rRangeTickEntry.rDataEntry.vcValue=i_rDataset.lRangeDataEntries(i).vcValue THEN
            iDatasetIndex:=i;
            EXIT;
          END IF;
        END IF;
        EXIT WHEN iDatasetIndex >0;
      END LOOP;

      IF iDatasetIndex>0 THEN
        -- calc x-value
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
        -- for stacked bars, get the last value
          IF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_STACKED_BARCHART THEN
            IF lLastValues.EXISTS(iDatasetIndex) THEN
              nValue:=lLastValues(iDatasetIndex)+i_rDataset.lValueDataEntries(iDatasetIndex).nValue;
              nMinValue:=lLastValues(iDatasetIndex);
              lLastValues(iDatasetIndex):=lLastValues(iDatasetIndex)+i_rDataset.lValueDataEntries(iDatasetIndex).nValue;
            ELSE
              nValue:=i_rDataset.lValueDataEntries(iDatasetIndex).nValue;
              nMinValue:=0;
              lLastValues(iDatasetIndex):=i_rDataset.lValueDataEntries(iDatasetIndex).nValue;
            END IF;
          ELSE
            nValue:=i_rDataset.lValueDataEntries(iDatasetIndex).nValue;
            nMinValue:=0;
          END IF;

          nY1:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>nValue
                                                  );

          nY2:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>nMinValue
                                                  );
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nY1:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(iDatasetIndex).dtValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.dtValue)*nXFactor;
        END IF;
      ELSE
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY1:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>i_rChart.lValueAxis(i_rDataset.nValueAxis),
                                                    i_bSnapToEdge =>TRUE,
                                                    i_nValue      =>0
                                                  );
          nY2:=nY1;
        END IF;
      END IF;
      IF nY1>nY2 THEN
        nSwap:=nY2;
        nY2:=nY1;
        nY1:=nSwap;
      END IF;

      IF i_nRangeIndex IS NOT NULL THEN
        IF i_nSeriesCount> 1 THEN
          nBarWidth:=( i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit
                      -nCategorySpace
                      -(i_nSeriesCount-1)*nSeriesSpace)/i_nSeriesCount;
          nX:= i_rChartRect.nx
              +i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nStartOffset
              +(i_nRangeIndex-1)*i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit
              +nCategorySpace/2+(i_nSeriesNum-1)*(nSeriesSpace/2+nBarWidth);
        ELSE
          nBarWidth:= i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit
                     -nCategorySpace;
          nX:= i_rChartRect.nx
              +i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nStartOffset
              +(i_nRangeIndex-1)*i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit
              +nCategorySpace/2;
        END IF;
        IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.NO THEN
        AS_PDF3_MOD.rect(p_x         =>nX,
                         p_y         =>nY1,
                         p_width     =>nBarWidth,
                         p_height    =>nY2-nY1,
                         p_line_color=>i_rChart.lSeries(i_rDataset.nSeries).vcBorderColor,
                         p_fill_color=>vcBarColor,
                         p_line_width=>PK_JRXML2PDF_TYPES.THIN
                        );
        ELSE
          lX.DELETE;
          lY.DELETE;
          lX(1):=nx;              lY(1):=nY1;
          lX(2):=nx;              lY(2):=nY2;
          lX(3):=nx+nBarWidth;    lY(3):=nY2;
          lX(4):=nx+nBarWidth;    lY(4):=nY1;
          lX(5):=nx;              lY(5):=nY1;
          AS_PDF3_MOD.PR_POLYGON(i_lXs =>lX,
                                 i_lYs =>lY,
                                 i_vcLineColor=>NVL(i_rChart.lSeries(i_rDataset.nSeries).vcBorderColor, vcBarColor),
                                 i_vcFillColor=>vcBarColor,
                                 i_nLineWidth=>PK_JRXML2PDF_TYPES.THIN
                                );

          -- Apply 3d-effect
          -- build upper box
          lX.DELETE;
          lY.DELETE;
          lX(1):=nx-0.45;         lY(1):=nY2+0.45;
          lX(2):=nx+10;           lY(2):=nY2+10;
          lX(3):=nx+10+nBarWidth+0.45; lY(3):=nY2+10;
          lX(4):=nx+nBarWidth+0.45;    lY(4):=nY2+0.45;
          lX(5):=nx-0.45;         lY(5):=nY2+0.45;
          AS_PDF3_MOD.PR_POLYGON(i_lXs =>lX,
                                 i_lYs =>lY,
                                 i_vcLineColor=>i_rChart.lSeries(i_rDataset.nSeries).vcBorderColor,
                                 i_vcFillColor=>vcDarkerBarColor,
                                 i_nLineWidth=>PK_JRXML2PDF_TYPES.THIN
                                );
          -- build right box
          lX.DELETE;
          lY.DELETE;
          lX(1):=nx+nBarWidth+0.45;    lY(1):=nY2+0.45;
          lX(2):=nx+nBarWidth+10+0.45;      lY(2):=nY2+10;
          lX(3):=nx+nBarWidth+10+0.45;      lY(3):=nY1+10;
          lX(4):=nx+nBarWidth+0.45;    lY(4):=nY1-0.45;
          lX(5):=nx+nBarWidth+0.45;    lY(5):=nY2+0.45;
          AS_PDF3_MOD.PR_POLYGON(i_lXs        =>lX,
                                 i_lYs        =>lY,
                                 i_vcLineColor=>i_rChart.lSeries(i_rDataset.nSeries).vcBorderColor,
                                 i_vcFillColor=>vcVeryDarkBarColor,
                                 i_nLineWidth=>PK_JRXML2PDF_TYPES.THIN
                                );
        END IF;
      END IF;
      IF i_rChart.lPlots(i_nPlotIndex).nLabelPosition IN (VALUE_POSITION_INSIDE, VALUE_POSITION_OUTSIDE) THEN
        AS_PDF3_MOD.set_color(i_rChart.lPlots(i_nPlotIndex).vcLabelColor);
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>i_rChart.lPlots(i_nPlotIndex).vcLabelFont,
                                      i_vcStyle    =>i_rChart.lPlots(i_nPlotIndex).vcLabelFontStyle,
                                      i_nSize      =>i_rChart.lPlots(i_nPlotIndex).nLabelFontSize
                                     );

        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          vcText:=TO_CHAR(i_rDataset.lValueDataEntries(iDatasetIndex).nValue, i_rChart.lPlots(i_nPlotIndex).vcLabelPattern, i_rChart.rLocaleData.vcNumericChars);
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          vcText:=TO_CHAR(i_rDataset.lValueDataEntries(iDatasetIndex).dtValue, i_rChart.lPlots(i_nPlotIndex).vcLabelPattern, i_rChart.rLocaleData.vcDateLanguage);
        END IF;
        IF i_rChart.lPlots(i_nPlotIndex).nLabelPosition=VALUE_POSITION_INSIDE THEN
          nYText:=(nY2-nY1)/2;
        ELSE
          IF nSwap IS NULL THEN
            nYText:=nY1-3-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
          ELSE
            -- swapped, then place text beneath bar
            nYText:=nY2+3;
          END IF;
        END IF;
        nxText:=nx+nBarWidth/2-AS_PDF3_MOD.str_len(vcText)/2;
        IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES
           AND i_rChart.lPlots(i_nPlotIndex).nLabelPosition=VALUE_POSITION_OUTSIDE THEN
          nxText:=nxText+5;
        END IF;
        AS_PDF3_MOD.put_txt(p_x=>nXText,
                            p_y=>nYText,
                            p_txt=>vcText
                           );
      END IF;
    END;

  BEGIN
    FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
      IF i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries>0 THEN
        nRangeAxis:=i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nrangeAxis;
        lSeriesIndex(i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries):=i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries;
      END IF;
    END LOOP;
    IF lSeriesIndex.COUNT>0 THEN
      IF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_STACKED_BARCHART THEN
        nSeriesCount:=1;
      ELSE
        nSeriesCount:=lSeriesIndex.COUNT;
      END IF;
      -- if the plot is 3D, render a bar-base at X-position 0
      IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
        -- find X-axis
        rAxis:=i_rChart.lValueaxis(i_rChart.lPlots(i_nPlotIndex).lDatasets(1).nValueAxis);
        nY:=i_rChartRect.nY+FK_GET_VALUE_ON_AXIS(i_rAxis       =>rAxis,
                                                 i_bSnapToEdge =>FALSE,
                                                 i_nValue      =>0
                                                );
        -- render bar-base
        IF rAxis.rMinValueDataEntry.nValue<0 AND rAxis.rMaxValueDataEntry.nValue>0 THEN
          lX(1):=i_rChartRect.nX+.5;                         lY(1):=nY+.5;
          lX(2):=i_rChartRect.nX+10.5;                       lY(2):=nY+10.5;
          lX(3):=i_rChartRect.nX+10.5+i_rChartRect.nWidth;   lY(3):=nY+10.5;
          lX(4):=i_rChartRect.nX+.5+i_rChartRect.nWidth;     lY(4):=nY+.5;
          lX(5):=i_rChartRect.nX+.5;                         lY(5):=nY+.5;
          AS_PDF3_MOD.pr_polygon( lX,
                                  lY,
                                  'EEEEEE',
                                  'EEEEEE'
                                );
          IF rAxis.vcShowDotlines=PK_JRXML2PDF_TYPES.YES THEN
            IF rAxis.vcDotlineColor IS NOT NULL THEN
              -- dotted lines
              AS_PDF3_MOD.pr_line      (i_nX1         =>i_rChartRect.nX,
                                        i_nY1         =>nY,
                                        i_nX2         =>i_rChartRect.nX+10.5,
                                        i_nY2         =>nY+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor =>REPLACE(rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke     =>'[3] 0'
                                       );
              -- dotted line
              AS_PDF3_MOD.pr_line      (i_nX1        =>i_rChartRect.nX+10.5,
                                        i_nY1        =>nY+10.5,
                                        i_nX2        =>i_rChartRect.nX+i_rChartRect.nWidth+10.5,
                                        i_nY2        =>nY+10.5,
                                        i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN,
                                        i_vcLineColor=>REPLACE(rAxis.vcDotlineColor, '#', ''),
                                        i_vcStroke   =>'[3] 0'
                                       );
            END IF;
          END IF;
        END IF;
      END IF;

      -- if spaces for category and series are not explicitly set, calculate percentage of space
      IF i_rChart.lPlots(i_nPlotIndex).nCategorySpace IS NULL THEN
        nCategorySpace:=i_rChart.lRangeAxis(nRangeAxis).nPixelPerUnit*0.20;
      ELSE
        nCategorySpace:=i_rChart.lPlots(i_nPlotIndex).nCategorySpace;
      END IF;
      IF i_rChart.lPlots(i_nPlotIndex).nSeriesSpace IS NULL THEN
        IF lSeriesIndex.COUNT>0 THEN
          nSeriesSpace:=i_rChart.lRangeAxis(nRangeAxis).nPixelPerUnit/nSeriesCount*0.4;
        ELSE
          nSeriesSpace:=i_rChart.lRangeAxis(nRangeAxis).nPixelPerUnit*0.1;
        END IF;
      ELSE
        nSeriesSpace:=i_rChart.lPlots(i_nPlotIndex).nSeriesSpace;
      END IF;

      iRangeIndex:=1;
      vcIndex:=i_rChart.lRangeAxis(nRangeAxis).lTickEntries.FIRST;
      WHILE vcIndex IS NOT NULL LOOP
        nSeriesNum:=1;
        iSeries:=lSeriesIndex.FIRST;
        WHILE iSeries IS NOT NULL LOOP
          FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
            IF i_rChart.lPlots(i_nPlotIndex).lDatasets(i).nSeries=iSeries THEN
              PR_RENDER_BAR(i_rChart.lPlots(i_nPlotIndex).lDatasets(i),
                            i_rChart.lRangeAxis(nRangeAxis).lTickEntries(vcIndex),
                            iRangeIndex,
                            nSeriesNum,
                            nSeriesCount
                           );
            END IF;
          END LOOP;
          nSeriesNum:=nSeriesNum+1;
          iSeries:=lSeriesIndex.NEXT(iSeries);
        END LOOP;
        iRangeIndex:=iRangeIndex+1;
        vcIndex:=i_rChart.lRangeAxis(nRangeAxis).lTickEntries.NEXT(vcIndex);
      END LOOP;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_PLOT(i_rChart IN tChart,
                           i_nPlotIndex IN NUMBER,
                           i_rChartRect IN tRect
                          ) IS
    PROCEDURE PR_RENDER_XY_LINE_DATASET(i_rDataset IN tDataset) IS

      nX       NUMBER;
      nXFactor NUMBER;
      nY       NUMBER;
      nYFactor NUMBER;
      nxLast   NUMBER;
      nYLast   NUMBER;
      vcLineColor PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcLineColorDark PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcLineColorLight PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcColor          PK_JRXML2PDF_TYPES.tColor;
      lX       AS_PDF3_MOD.tVertices;
      lY       AS_PDF3_MOD.tVertices;
    BEGIN
      nXFactor:=i_rChart.lValueAxis(i_rDataset.nValueAxis).nPixelPerUnit;
      nYFactor:=i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit;
      IF i_rDataset.nSeries>0 THEN
        vcLineColor:=i_rChart.lSeries(i_rDataset.nSeries).vcColor;
        vcLineColorDark:=FK_DARKER(vcLineColor);
        vcLineColorLight:=FK_LIGHTER(vcLineColor);
      END IF;
      FOR i IN 1..i_rDataset.lValueDataEntries.COUNT LOOP
        -- calc x-value
        IF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nX:=i_rChartRect.nX+(i_rDataset.lRangeDataEntries(i).nValue-i_rChart.lRangeAxis(i_rDataset.nRangeAxis).rMinValueDataEntry.nValue)*nYFactor;

        ELSIF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nX:=i_rChartRect.nX+(i_rDataset.lRangeDataEntries(i).dtValue-i_rChart.lRangeAxis(i_rDataset.nRangeAxis).rMinValueDataEntry.dtValue)*nYFactor;
        END IF;
        -- starting offset
        nx:=nx+i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nStartOffset;
        -- calc y-value
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(i).nValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.nValue)*nXFactor;
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nY:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(i).dtValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.dtValue)*nXFactor;
        END IF;
        -- starting offset
        ny:=ny+i_rChart.lValueAxis(i_rDataset.nValueAxis).nStartOffset;
        IF i_rChart.lPlots(i_nPlotIndex).vcShowShapes=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.pr_path(i_lPath     =>FK_TRANSFORM_PATH(i_nX   =>nX,
                                                              i_nY   =>nY,
                                                              i_lPath=>i_rChart.lSeries(i_rDataset.nSeries).lShapePath),
                              i_vcLineColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_vcFillColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                             );
        END IF;

        IF     nxLast IS NOT NULL
           AND i_rChart.lPlots(i_nPlotIndex).vcShowLines=PK_JRXML2PDF_TYPES.YES THEN
          IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
            lX.DELETE;
            lY.DELETE;
            lX(1):=nXLast+.5;        lY(1):=nYLast+.5;
            lX(2):=nXLast+10.5;     lY(2):=nYLast+10.5;
            lX(3):=nX+10.5;         lY(3):=nY+10.5;
            lX(4):=nX+.5;            lY(4):=nY+.5;
            lX(5):=nXLast+.5;        lY(5):=nYLast+.5;
            IF nY!=nYLast THEN
              IF (nx-nXLast)/(nY-nYLast)>1 THEN
                vcColor:=vcLineColorLight;
              ELSIF (nx-nXLast)/(nY-nYLast) BETWEEN 0 AND 1 THEN
                vcColor:=vcLineColorDark;
              ELSIF (nx-nXLast)/(nY-nYLast) BETWEEN 0 AND -1 THEN
                vcColor:=vcLineColorDark;
              ELSE
                vcColor:=vcLineColorLight;
              END IF;
            ELSE
              vcColor:=vcLineColorLight;
            END IF;

            as_pdf3_mod.pr_polygon(lX,
                                   lY,
                                   vcColor,
                                   vcColor
                                  );
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nxLast+10.5,
                                nyLast+10.5,
                                i_vcLineColor=>vcLineColor);
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny,
                                i_vcLineColor=>vcLineColor);

          ELSE
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny,
                                i_vcLineColor=>vcLineColor);
          END IF;
        END IF;
        nxLast:=nX;
        nYLast:=ny;
      END LOOP;
    END;

    PROCEDURE PR_RENDER_TIME_LINE_DATASET(i_rDataset IN tDataset) IS

      nX       NUMBER;
      nXFactor NUMBER;
      nY       NUMBER;
      nYFactor NUMBER;
      nxLast   NUMBER;
      nYLast   NUMBER;
      vcLineColor PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcLineColorDark PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcLineColorLight PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      vcColor          PK_JRXML2PDF_TYPES.tColor;
      lX       AS_PDF3_MOD.tVertices;
      lY       AS_PDF3_MOD.tVertices;
    BEGIN
      nXFactor:=i_rChart.lValueAxis(i_rDataset.nValueAxis).nPixelPerUnit;
      nYFactor:=i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nPixelPerUnit;
      IF i_rDataset.nSeries>0 THEN
        vcLineColor:=i_rChart.lSeries(i_rDataset.nSeries).vcColor;
        vcLineColorDark:=FK_DARKER(vcLineColor);
        vcLineColorLight:=FK_LIGHTER(vcLineColor);
      END IF;
      FOR i IN 1..i_rDataset.lValueDataEntries.COUNT LOOP
        -- calc x-value
        IF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nX:=i_rChartRect.nX+(i_rDataset.lRangeDataEntries(i).nValue-i_rChart.lRangeAxis(i_rDataset.nRangeAxis).rMinValueDataEntry.nValue)*nYFactor;

        ELSIF i_rDataSet.vcRangeType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nX:=i_rChartRect.nX+(i_rDataset.lRangeDataEntries(i).dtValue-i_rChart.lRangeAxis(i_rDataset.nRangeAxis).rMinValueDataEntry.dtValue)*nYFactor;
        END IF;
        -- starting offset
        nx:=nx+i_rChart.lRangeAxis(i_rDataset.nRangeAxis).nStartOffset;

        -- calc y-value
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nY:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(i).nValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.nValue)*nXFactor;
        ELSIF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_DATE THEN
          nY:=i_rChartRect.nY+(i_rDataset.lValueDataEntries(i).dtValue-i_rChart.lValueAxis(i_rDataset.nValueAxis).rMinValueDataEntry.dtValue)*nXFactor;
        END IF;
        -- starting offset
        ny:=ny+i_rChart.lValueAxis(i_rDataset.nValueAxis).nStartOffset;

        IF i_rChart.lPlots(i_nPlotIndex).vcShowShapes=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.pr_path(i_lPath     =>FK_TRANSFORM_PATH(i_nX   =>nX,
                                                              i_nY   =>nY,
                                                              i_lPath=>i_rChart.lSeries(i_rDataset.nSeries).lShapePath),
                              i_vcLineColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_vcFillColor=>REPLACE(i_rChart.lSeries(i_rDataset.nSeries).vcColor, '#', ''),
                              i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                             );
        END IF;

        IF     nxLast IS NOT NULL
           AND i_rChart.lPlots(i_nPlotIndex).vcShowLines=PK_JRXML2PDF_TYPES.YES THEN
          IF i_rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
            lX.DELETE;
            lY.DELETE;
            lX(1):=nXLast+.5;        lY(1):=nYLast+.5;
            lX(2):=nXLast+10.5;     lY(2):=nYLast+10.5;
            lX(3):=nX+10.5;         lY(3):=nY+10.5;
            lX(4):=nX+.5;            lY(4):=nY+.5;
            lX(5):=nXLast+.5;        lY(5):=nYLast+.5;
            IF nY!=nYLast THEN
              IF (nx-nXLast)/(nY-nYLast)>1 THEN
                vcColor:=vcLineColorLight;
              ELSIF (nx-nXLast)/(nY-nYLast) BETWEEN 0 AND 1 THEN
                vcColor:=vcLineColorDark;
              ELSIF (nx-nXLast)/(nY-nYLast) BETWEEN 0 AND -1 THEN
                vcColor:=vcLineColorDark;
              ELSE
                vcColor:=vcLineColorLight;
              END IF;
            ELSE
              vcColor:=vcLineColorLight;
            END IF;

            as_pdf3_mod.pr_polygon(lX,
                                   lY,
                                   vcColor,
                                   vcColor
                                  );
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nxLast+10.5,
                                nyLast+10.5,
                                i_vcLineColor=>vcLineColor);
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny,
                                i_vcLineColor=>vcLineColor);

          ELSE
            as_pdf3_mod.pr_line(nxLast,
                                nylast,
                                nx,
                                ny,
                                i_vcLineColor=>vcLineColor);
          END IF;
        END IF;
        nxLast:=nX;
        nYLast:=ny;
      END LOOP;
    END;

    PROCEDURE PR_RENDER_PIE_DATASET(i_rDataset IN tDataset) IS

      nX             NUMBER;
      nXFactor       NUMBER;
      nY             NUMBER;
      nYFactor       NUMBER;
      nxLast         NUMBER;
      nYLast         NUMBER;
      vcLineColor    PK_JRXML2PDF_TYPES.tColor:=PK_JRXML2PDF_TYPES.BLACK;
      nTotal         NUMBER:=0;
      nStartAngle    NUMBER:=0;
      nAngle         NUMBER;
      nCenterX       NUMBER;
      nCenterY       NUMBER;
      lPath          AS_PDF3_MOD.tPath;
      nRadius        NUMBER;
      bFirst         BOOLEAN:=TRUE;
      vcColor        PK_JRXML2PDF_TYPES.tColor;
      lAngle         PK_JRXML2PDF_TYPES.tNumList;
      bToLeft        BOOLEAN;
      nMaxLabelWidth NUMBER;

      PROCEDURE PR_PIE_LABEL(i_nValue     IN NUMBER,
                             i_vcCategory IN VARCHAR2,
                             i_nPercent   IN NUMBER,
                             i_nX         IN NUMBER,
                             i_nY         IN NUMBER,
                             i_bToLeft    IN BOOLEAN
                            ) IS
        vcText PK_JRXML2PDF_TYPES.tMaxVarchar2;
        nTextLength NUMBER;
        nTextHeight NUMBER:=0;
        nX          NUMBER;
        nY          NUMBER;
        lText       PK_JRXML2PDF_TYPES.tTextPieceList;
        nDummy      NUMBER:=0;
        nGroup      NUMBER:=0;
      BEGIN

        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>PK_JRXML2PDF_TYPES.ARIAL,
                                      i_vcStyle    =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                      i_nSize      =>9
                                     );
        vcText:=PK_JRXML2PDF_UTIL.FK_TEXT_MESSAGE(i_rChart.lPlots(i_nPlotIndex).vcLabelFormat,
                                                  i_vcCategory,
                                                  TO_CHAR(ROUND(i_nValue, 1), 'FM99999999990D0', i_rChart.rLocaleData.vcNumericChars),
                                                  TO_CHAR(ROUND(i_nPercent*100,1), 'FM99999999990D0', i_rChart.rLocaleData.vcNumericChars) || '%' );

        -- calculate the textpieces
        PK_JRXML2PDF_UTIL.PR_GET_TEXT_PIECES(io_lText          =>lText,
                                             io_nCurrentHeight =>nTextheight,
                                             io_nX             =>nDummy,
                                             i_vcText          =>vcText,
                                             i_nMaxHeight      =>AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)*4,
                                             io_nGroup         =>nGroup,
                                             i_nLineSpacing    =>PK_JRXML2PDF_TYPES.MIN_LINE_SPACING,
                                             i_nXStart         =>0,
                                             i_nMaxWidth       =>nMaxLabelWidth
                                            );
        nTextLength:=0;
        FOR i IN 1..lText.COUNT LOOP
          nTextlength:=GREATEST(nTextLength,AS_PDF3_MOD.str_len(lText(i).vcText));
        END LOOP;

        IF i_bToLeft THEN
          nX:=i_nX-4-nTextLength;
        ELSE
          nx:=i_nx+2;
        END IF;
        -- Rectangle around label
        AS_PDF3_MOD.rect(nx,
                         i_nY+3+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize),
                         nTextLength+4,
                         -nTextHeight-4,
                         PK_JRXML2PDF_TYPES.BLACK,
                         PK_JRXML2PDF_TYPES.WHITE
                        );
        nY:=i_nY+2;
        FOR i IN 1..lText.COUNT LOOP
          AS_PDF3_MOD.put_txt(nX+2, nY, lText(i).vcText);
          nY:=nY-lText(i).nHeight;
        END LOOP;
      END;
    BEGIN
      -- calculate total sum of the dataset
      FOR i IN 1..i_rDataset.lValueDataEntries.COUNT LOOP
        -- calc x-value
        IF i_rDataSet.vcValueType=PK_JRXML2PDF_TYPES.DATATYPE_NUMBER THEN
          nTotal:=nTotal+i_rDataset.lValueDataEntries(i).nValue;
        END IF;
      END LOOP;
      IF nTotal!=0 THEN
        nCenterX:=i_rChartRect.nX+i_rChartRect.nWidth/2;
        nCenterY:=i_rChartRect.nY+i_rChartRect.nHeight/2;

        -- Leave 50px room at a minimum left and right of pie
        nRadius:=LEAST(i_rChartRect.nWidth*0.8/2, i_rChartRect.nHeight*0.8/2);
        IF i_rChartRect.nWidth-2*nRadius<100 THEN
          nRadius:=(i_rChartRect.nWidth-100)/2;
        END IF;
        nMaxLabelWidth:=(i_rChartRect.nWidth-2*nRadius)/2-10;
        FOR i IN 1..i_rDataset.lValueDataEntries.COUNT LOOP
          nAngle:=i_rDataset.lValueDataEntries(i).nValue/nTotal*360;
          IF bFirst THEN
            nStartAngle:=90;
            bFirst:=FALSE;
          END IF;
          -- Draw Pie-piece
          lPath:=FK_CALC_PIESLICE(i_nCX=>nCenterX,
                                  i_nCY=>nCenterY,
                                  i_nA=>nRadius,
                                  i_nB=>nRadius,
                                  i_nLambda2=>FK_RADIANS(nStartAngle),
                                  i_nLambda1=>FK_RADIANS(nStartAngle-nAngle),
                                  i_bIsPieSlice=>TRUE
                                 );
          IF i_rChart.lSeries.EXISTS(i) THEN
            vcColor:=i_rChart.lSeries(i).vcColor;
          ELSE
            vcColor:='FF0000';
          END IF;
          AS_PDF3_MOD.PR_PATH(lPath,
                              PK_JRXML2PDF_TYPES.BLACK,
                              vcColor,
                              0.5
                             );
          lPath.DELETE;
          -- calculate the line and text
          lAngle(i):=MOD(nStartAngle-nAngle/2, 360);
          IF lAngle(i)<0 THEN
            lAngle(i):=360+lAngle(i);
          END IF;
          nStartAngle:=nStartAngle-nAngle;
        END LOOP;
        IF i_rChart.lPlots(i_nPlotIndex).vcIsDonut=PK_JRXML2PDF_TYPES.YES THEN
          -- Donut, cut-out inner circle
          lPath:=FK_CALC_PIESLICE(i_nCX=>nCenterX,
                                   i_nCY=>nCenterY,
                                   i_nA=>nRadius/4,
                                   i_nB=>nRadius/4,
                                   i_nLambda1=>FK_RADIANS(0),
                                   i_nLambda2=>FK_RADIANS(360),
                                   i_bIsPieSlice=>FALSE
                                  );
          AS_PDF3_MOD.PR_PATH(lPath,
                              PK_JRXML2PDF_TYPES.BLACK,
                              NVL(i_rChart.vcBgColor, PK_JRXML2PDF_TYPES.WHITE),
                              0.5
                             );
        END IF;
        FOR i IN 1..i_rDataset.lValueDataEntries.COUNT LOOP
          nXLast:= nCenterX + nRadius * 0.95 * cos(FK_RADIANS(lAngle(i)));
          nYLast:= nCentery + nRadius * 0.95 * sin(FK_RADIANS(lAngle(i)));
          nX:= nCenterX + nRadius * 1.05 * cos(FK_RADIANS(lAngle(i)));
          nY:= nCentery + nRadius * 1.05 * sin(FK_RADIANS(lAngle(i)));
          AS_PDF3_MOD.PR_LINE(nxLast,
                              nyLast,
                              nx,
                              ny,
                              PK_JRXML2PDF_TYPES.BLACK,
                              0.5
                             );
          IF    lAngle(i)<90
             OR lAngle(i)>270 THEN
            nXLast:=i_rChartRect.nx+i_rChartRect.nWidth/2+nRadius;
            bToLeft:=FALSE;
          ELSE
            nXLast:=i_rChartRect.nx+i_rChartRect.nWidth/2-nRadius;
            bToLeft:=TRUE;
          END IF;
          AS_PDF3_MOD.PR_LINE(nxLast,
                              ny,
                              nx,
                              ny,
                              PK_JRXML2PDF_TYPES.BLACK,
                              0.5
                             );
          PR_PIE_LABEL(i_rDataset.lValueDataEntries(i).nValue,
                       i_rDataset.lRangeDataEntries(i).vcValue,
                       i_rDataset.lValueDataEntries(i).nValue/nTotal,
                       nxLast,
                       ny,
                       bToLeft
                      );
        END LOOP;
      END IF;
    END;

  BEGIN
    IF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_XY_LINECHART THEN
      -- Render all datasets, measure by the corresponding axis
      FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
        PR_RENDER_XY_LINE_DATASET(i_rChart.lPlots(i_nPlotIndex).lDatasets(i));
      END LOOP;
    ELSIF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_TIMESERIES_LINECHART THEN
      -- Render all datasets, measure by the corresponding axis
      FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
        PR_RENDER_TIME_LINE_DATASET(i_rChart.lPlots(i_nPlotIndex).lDatasets(i));
      END LOOP;
    ELSIF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_CATEGORY_BARCHART THEN
      PR_RENDER_CATEGORY_BAR_PLOT(i_rChart, i_nPlotIndex, i_rChartRect);
    ELSIF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_STACKED_BARCHART THEN
      PR_RENDER_CATEGORY_BAR_PLOT(i_rChart, i_nPlotIndex, i_rChartRect);
    ELSIF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_CATEGORY_LINECHART THEN
      PR_RENDER_CATEGORY_LINE_PLOT(i_rChart, i_nPlotIndex, i_rChartRect);
    ELSIF i_rChart.lPlots(i_nPlotIndex).nType=PLOT_PIECHART THEN
      -- Render all datasets, measure by the corresponding axis
      FOR i IN 1..i_rChart.lPlots(i_nPlotIndex).lDatasets.COUNT LOOP
        PR_RENDER_PIE_DATASET(i_rChart.lPlots(i_nPlotIndex).lDatasets(i));
      END LOOP;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_RENDER_LEGEND(i_rChart IN tChart) IS
    nX            NUMBER:=i_rChart.rLegend.nX+i_rChart.rLegend.rOuterPaddings.nLeft+i_rChart.rLegend.rInnerPaddings.nLeft;
    nY            NUMBER:=i_rChart.rLegend.nY-i_rChart.rLegend.rOuterPaddings.nTop-i_rChart.rLegend.rInnerPaddings.nTop;
    bFirst        BOOLEAN:=TRUE;
    nWidth        NUMBER;
    nLegendHeight NUMBER;
    PROCEDURE PR_RENDER_MARKER(i_nSeries IN NUMBER) IS
      lPath    AS_PDF3_MOD.tPath;
      nType    NUMBER;
      vcLines  PK_JRXML2PDF_TYPES.tYesNo;
      vcShapes PK_JRXML2PDF_TYPES.tYesNo;
      PROCEDURE PR_GET_SERIES_TYPE(i_nSeries IN NUMBER,
                                  o_nType    OUT NUMBER,
                                  o_vcLines  OUT PK_JRXML2PDF_TYPES.tYesNo,
                                  o_vcShapes OUT PK_JRXML2PDF_TYPES.tYesNo
                                  ) IS
      BEGIN
        IF i_rChart.lPlots(1).nType=PLOT_PIECHART THEN
          o_nType:=PLOT_PIECHART;
        ELSE
          FOR i IN 1..i_rChart.lPlots.COUNT LOOP
            FOR j IN 1..i_rChart.lPlots(i).lDatasets.COUNT LOOP
              IF i_rChart.lPlots(i).lDatasets(j).nSeries=i_nSeries THEN
                o_nType:=i_rChart.lPlots(i).nType;
                o_vcLines:=i_rChart.lPlots(i).vcShowLines;
                o_vcShapes:=i_rChart.lPlots(i).vcShowShapes;
                EXIT;
              END IF;
            END LOOP;
            EXIT WHEN nType IS NOT NULL;
          END LOOP;
        END IF;
      END;
    BEGIN
      PR_GET_SERIES_TYPE(i_nSeries, nType, vcLines, vcShapes);

      IF nType IN (PLOT_XY_LINECHART, PLOT_TIMESERIES_LINECHART) THEN
        -- Render piece of line if lines are shown
        IF vcLines=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.horizontal_line(p_x          =>nX+3,
                                      p_y          =>nY-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2-1,
                                      p_width      =>10,
                                      p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                                      p_line_color =>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', '')
                                     );
        END IF;
        IF vcShapes=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.pr_path(i_lPath     =>FK_TRANSFORM_PATH(i_nX   =>nX+8,
                                                              i_nY   =>nY-6,
                                                              i_lPath=>i_rChart.lSeries(i_nSeries).lShapePath),
                              i_vcLineColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                              i_vcFillColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                              i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                             );
        END IF;
      ELSIF nType=PLOT_CATEGORY_LINECHART THEN
        -- Render piece of line
        IF vcLines=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.horizontal_line(p_x          =>nX+3,
                                      p_y          =>nY-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)/2-1,
                                      p_width      =>10,
                                      p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                                      p_line_color =>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', '')
                                     );
        END IF;
        IF vcShapes=PK_JRXML2PDF_TYPES.YES THEN
          AS_PDF3_MOD.pr_path(i_lPath     =>FK_TRANSFORM_PATH(i_nX   =>nX+8,
                                                              i_nY   =>nY-6,
                                                              i_lPath=>i_rChart.lSeries(i_nSeries).lShapePath),
                              i_vcLineColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                              i_vcFillColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                              i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                             );
        END IF;
      ELSIF nType=PLOT_PIECHART THEN
        -- Render circle
        lPath:=FK_CALC_PIESLICE(i_nCX =>nX+7,
                                i_nCY =>nY-7,
                                i_nA  =>4,
                                i_nB =>4,
                                i_nLambda1=>0,
                                i_nLambda2=>360,
                                i_bIsPieSlice =>FALSE);

        AS_PDF3_MOD.pr_path(i_lPath     =>lPath,
                            i_vcLineColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                            i_vcFillColor=>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                            i_nLineWidth =>PK_JRXML2PDF_TYPES.THIN
                           );
      ELSIF nType IN (PLOT_CATEGORY_BARCHART, PLOT_STACKED_BARCHART) THEN
        -- Render square
        AS_PDF3_MOD.rect(p_x          =>nX+3,
                         p_y          =>nY-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize),
                         p_width      =>8,
                         p_height     =>8,
                         p_line_width =>PK_JRXML2PDF_TYPES.THIN,
                         p_line_color =>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', ''),
                         p_fill_color =>REPLACE(i_rChart.lSeries(i_nSeries).vcColor, '#', '')
                        );
      END IF;
    END;
  BEGIN
    IF i_rChart.rLegend.vcPosition!=POSITION_NONE THEN
      IF i_rChart.rLegend.vcBGColor IS NOT NULL THEN
        AS_PDF3_MOD.rect(p_x =>i_rChart.rLegend.nX+i_rChart.rLegend.rOuterPaddings.nLeft,
                         p_y =>i_rChart.rLegend.nY-i_rChart.rLegend.rOuterPaddings.nTop,
                         p_width=>i_rChart.rLegend.nWidth-i_rChart.rLegend.rOuterPaddings.nLeft-i_rChart.rLegend.rOuterPaddings.nRight,
                         p_height=>-(i_rChart.rLegend.nHeight-i_rChart.rLegend.rOuterPaddings.nTop-i_rChart.rLegend.rOuterPaddings.nBottom),
                         p_line_color =>i_rChart.rLegend.vcBGColor,
                         p_fill_color =>i_rChart.rLegend.vcBGColor,
                         p_line_width =>PK_JRXML2PDF_TYPES.THIN
                        );
      END IF;

      PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>i_rChart.rLegend.vcFont,
                                    i_vcStyle    =>i_rChart.rLegend.vcFontStyle,
                                    i_nSize      =>i_rChart.rLegend.nFontSize
                                   );
      AS_PDF3_MOD.set_color(i_rChart.rLegend.vcFontColor);

      FOR i IN 1..i_rChart.lSeries.COUNT LOOP
        IF i_rChart.rLegend.vcPosition IN (POSITION_TOP, POSITION_BOTTOM) THEN
          nWidth:=i_rChart.rLegend.nEntryXSpacing+10+2+AS_PDF3_MOD.str_len(i_rChart.lSeries(i).vcName);
          IF NOT bFirst THEN
            IF nx+nWidth> i_rChart.rLegend.nWidth
                         +i_rChart.rLegend.nX
                         -i_rChart.rLegend.rOuterPaddings.nRight
                         -i_rChart.rLegend.rInnerPaddings.nRight THEN
              -- skip to next line
              nX:=i_rChart.rLegend.nX+i_rChart.rLegend.rOuterPaddings.nLeft+i_rChart.rLegend.rInnerPaddings.nLeft;
              nY:=nY-i_rChart.rLegend.nEntryYSpacing-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            END IF;
          ELSE
            bFirst:=FALSE;
          END IF;
        ELSE
          -- skip to next line
          nX:=i_rChart.rLegend.nX+i_rChart.rLegend.rOuterPaddings.nLeft+i_rChart.rLegend.rInnerPaddings.nLeft;
          IF NOT bFirst THEN
            nY:=nY-i_rChart.rLegend.nEntryYSpacing-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
          ELSE
            bFirst:=FALSE;
          END IF;
        END IF;
        PR_RENDER_MARKER(i);
        AS_PDF3_MOD.put_txt(p_x               =>nx+3+10+3,
                            p_y               =>nY-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize),
                            p_txt             =>i_rChart.lSeries(i).vcName);
        nx:=nX+nWidth;
      END LOOP;
      -- Render box
      IF i_rChart.rLegend.vcBorderColor IS NOT NULL THEN
        AS_PDF3_MOD.rect(p_x =>i_rChart.rLegend.nX+i_rChart.rLegend.rOuterPaddings.nLeft,
                         p_y =>i_rChart.rLegend.nY-i_rChart.rLegend.rOuterPaddings.nTop,
                         p_width=>i_rChart.rLegend.nWidth-i_rChart.rLegend.rOuterPaddings.nLeft-i_rChart.rLegend.rOuterPaddings.nRight,
                         p_height=>-(i_rChart.rLegend.nHeight-i_rChart.rLegend.rOuterPaddings.nTop-i_rChart.rLegend.rOuterPaddings.nBottom),
                         p_line_color=>i_rChart.rLegend.vcBorderColor,
                         p_line_width =>PK_JRXML2PDF_TYPES.THIN
                        );

      END IF;
    END IF;
  END;

  -- ---------------------------------------------------------------------------

  PROCEDURE PR_CHART_TO_PDF(i_rChart  IN tChart,
                            i_nX      IN NUMBER,
                            i_nY      IN NUMBER) IS
    rChart        tChart:=i_rChart;
    nLeftSpace    NUMBER:=i_rChart.rInnerPaddings.nLeft;
    nRightSpace   NUMBER:=i_rChart.rInnerPaddings.nRight;
    nTopSpace     NUMBER:=i_rChart.rInnerPaddings.nTop;
    nBottomSpace  NUMBER:=i_rChart.rInnerPaddings.nBottom;
    rChartRect    tRect;
    iColor        PLS_INTEGER:=1;
    iShape        PLS_INTEGER:=1;

    PROCEDURE PR_CALC_AXIS_SPACE(io_rAxis IN OUT NOCOPY tAxis) IS
    BEGIN
      IF io_rAxis.nOrientation=ORIENTATION_VERTICAL THEN
        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          io_rAxis.nX:=i_nX+nLeftSpace;
          nLeftSpace:=nLeftSpace+io_rAxis.nWidth;
        ELSIF io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
          nRightSpace:=nRightSpace+io_rAxis.nWidth;
          io_rAxis.nX:=i_nX+rChart.nWidth-nRightSpace;
        END IF;
      ELSE
        IF io_rAxis.nPosition=POSITION_TOP_OR_LEFT THEN
          io_rAxis.nY:=i_nY-nTopSpace;
          nTopSpace:=nTopSpace+io_rAxis.nHeight;
        ELSIF io_rAxis.nPosition=POSITION_BOTTOM_OR_RIGHT THEN
          nBottomSpace:=nBottomSpace+io_rAxis.nHeight;
          io_rAxis.nY:=i_nY-rChart.nHeight+nBottomSpace;
        END IF;
      END IF;
    END;

    PROCEDURE PR_CALC_LEGEND_MEASURE IS
      nResult   NUMBER:=0;
      nWidth    NUMBER;
      bFirst    BOOLEAN:=TRUE;
      nMaxWidth NUMBER:=0;
      nX        NUMBER:=0;
      nTotal    NUMBER:=0;
      vcText    PK_JRXML2PDF_TYPES.tMaxVarchar2;
    BEGIN
      IF rChart.rLegend.vcPosition!=POSITION_NONE THEN
        -- If the chart is a pie-chart, the series-entries are the different Keys from the dataset
        IF rChart.lPlots(1).nType=PLOT_PIECHART THEN
          -- calculate the total and percentages
          FOR i IN 1..rChart.lPlots(1).lDataSets(1).lValueDataEntries.COUNT LOOP
            nTotal:=nTotal+rChart.lPlots(1).lDataSets(1).lValueDataEntries(i).nValue;
          END LOOP;
          FOR i IN 1..rChart.lPlots(1).lDataSets(1).lRangeDataEntries.COUNT LOOP
            vcText:=PK_JRXML2PDF_UTIL.FK_TEXT_MESSAGE(i_rChart.lPlots(1).vcKeyFormat,
                                                      rChart.lPlots(1).lDataSets(1).lRangeDataEntries(i).vcValue,
                                                      TO_CHAR(ROUND(rChart.lPlots(1).lDataSets(1).lValueDataEntries(i).nValue, 1), 'FM99999999990D0', i_rChart.rLocaleData.vcNumericChars),
                                                      TO_CHAR(ROUND(rChart.lPlots(1).lDataSets(1).lValueDataEntries(i).nValue/nTotal*100,1), 'FM99999999990D0', i_rChart.rLocaleData.vcNumericChars) || '%' );

            IF rChart.lSeries.EXISTS(i) THEN
              rChart.lSeries(i).vcName:=vcText;
            ELSE
              rChart.lSeries(i):=FK_CREATE_SERIES(vcText);
              IF lDefaultColors.EXISTS(i) THEN
                rChart.lSeries(i).vcColor:=lDefaultColors(i);
                rChart.lSeries(i).vcBorderColor:=lDefaultColors(i);
              END IF;
            END IF;
          END LOOP;
        END IF;
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>rChart.rLegend.vcFont,
                                      i_vcStyle    =>rChart.rLegend.vcFontStyle,
                                      i_nSize      =>rChart.rLegend.nFontSize
                                     );
        IF rChart.rLegend.vcPosition IN (POSITION_TOP, POSITION_BOTTOM) THEN
          nResult:=2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
          FOR i IN 1..rChart.lSeries.COUNT LOOP
            nWidth:=rChart.rLegend.nEntryXSpacing+10+2+AS_PDF3_MOD.str_len(rChart.lSeries(i).vcName);
            IF NOT bFirst THEN
              IF nX+nWidth>i_rChart.nWidth-rChart.rLegend.rOuterPaddings.nLeft
                                          -rChart.rLegend.rOuterPaddings.nRight
                                          -rChart.rLegend.rInnerPaddings.nLeft
                                          -rChart.rLegend.rInnerPaddings.nRight
                                          -nLeftSpace
                                          -nRightSpace THEN
                -- skip to next line
                nX:=0;
                nResult:=nResult+rChart.rLegend.nEntryYSpacing+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
              END IF;
            ELSE
              bFirst:=FALSE;
            END IF;
            nx:=nX+nWidth;
            nMaxWidth:=GREATEST(nMaxWidth, nX);
          END LOOP;

          nMaxWidth:=LEAST(nMaxWidth, i_rChart.nWidth);
          nResult:=nResult
                   +rChart.rLegend.rOuterPaddings.nTop
                   +rChart.rLegend.rOuterPaddings.nBottom
                   +rChart.rLegend.rInnerPaddings.nTop
                   +rChart.rLegend.rInnerPaddings.nBottom;
          IF rChart.rLegend.vcPosition=POSITION_TOP THEN
            rChart.rLegend.nY:=i_nY-nTopSpace;
            nTopSpace:=nTopSpace+nResult;
          ELSE
            rChart.rLegend.nY:=i_nY-rChart.nHeight+nBottomSpace+nResult;
            nBottomSpace:=nBottomSpace+nResult;
          END IF;
          rChart.rLegend.nHeight:=nResult;
          -- Check the width of the legend, if its more than the defined percentage of the whole
          -- area, put it to the maximum size
          nMaxWidth:= nMaxWidth
                     +rChart.rLegend.rInnerPaddings.nLeft
                     +rChart.rLegend.rInnerPaddings.nRight
                     +rChart.rLegend.rOuterPaddings.nLeft
                     +rChart.rLegend.rOuterPaddings.nRight;
          IF  nMaxWidth<(rChart.nWidth-nLeftSpace-nRightSpace)*rChart.rLegend.nScaleToWidthPercentage THEN
            rChart.rLegend.nWidth:=nMaxWidth;
            -- adjust x-position to center
            rChart.rLegend.nX:=i_nX+nLeftSpace+(rChart.nWidth-nLeftSpace-nRightSpace-nMaxWidth)/2;

          ELSE
            rChart.rLegend.nX:=i_nX+nLeftSpace;
            rChart.rLegend.nWidth:=rChart.nWidth-nLeftSpace-nRightSpace;
          END IF;
        ELSIF rChart.rLegend.vcPosition IN (POSITION_LEFT, POSITION_RIGHT) THEN
          nResult:=2+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
          FOR i IN 1..rChart.lSeries.COUNT LOOP
            nWidth:=rChart.rLegend.nEntryXSpacing+10+2+AS_PDF3_MOD.str_len(rChart.lSeries(i).vcName);
            IF NOT bFirst THEN
              nResult:=nResult+rChart.rLegend.nEntryYSpacing+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize);
            ELSE
              bFirst:=FALSE;
            END IF;
            nMaxWidth:=GREATEST(nMaxWidth, nWidth);
          END LOOP;
          nResult:=nResult
                   +rChart.rLegend.rOuterPaddings.nTop
                   +rChart.rLegend.rOuterPaddings.nBottom
                   +rChart.rLegend.rInnerPaddings.nTop
                   +rChart.rLegend.rInnerPaddings.nBottom;
          IF rChart.rLegend.vcPosition=POSITION_LEFT THEN
            rChart.rLegend.nX:=i_nX+nLeftSpace;
            nLeftSpace:=nLeftSpace+nMaxWidth;
          ELSE
            rChart.rLegend.nX:=i_nX+rChart.nWidth-nRightSpace-nMaxWidth;
            nRightSpace:=nRightSpace+nMaxWidth;
          END IF;
          rChart.rLegend.nY:=i_nY-(rChart.nHeight+nBottomSpace-nResult)/2;
          rChart.rLegend.nHeight:=nResult;
          rChart.rLegend.nWidth:= nMaxWidth
                                 +rChart.rLegend.rOuterPaddings.nLeft
                                 +rChart.rLegend.rOuterPaddings.nRight
                                 +rChart.rLegend.rInnerPaddings.nLeft
                                 +rChart.rLegend.rInnerPaddings.nRight;
        END IF;
      END IF;
    END;

    PROCEDURE PR_RENDER_TITLE IS
    BEGIN
      -- Render title
      IF rChart.vcTitle IS NOT NULL THEN
        AS_PDF3_MOD.set_color(rChart.vcTitleColor);
        -- title is centered
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>rChart.vcTitleFont,
                                      i_vcStyle    =>rChart.vcTitleFontStyle,
                                      i_nSize      =>rChart.nTitleFontSize
                                     );

        IF rChart.vcTitlePosition=POSITION_TOP THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+i_rChart.nWidth/2-AS_PDF3_MOD.str_len(rChart.vcTitle)/2,
                              p_y               =>i_nY-3-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize),
                              p_txt             =>rChart.vcTitle);
          nTopSpace:=nTopSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_BOTTOM THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+i_rChart.nWidth/2-AS_PDF3_MOD.str_len(rChart.vcTitle)/2,
                              p_y               =>i_nY+6-rChart.nHeight+nBottomSpace,
                              p_txt             =>rChart.vcTitle);
          nBottomSpace:=nBottomSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_LEFT THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+3+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize),
                              p_y               =>i_nY-(rChart.nHeight-nTopSpace-nBottomSpace)/2-AS_PDF3_MOD.str_len(rChart.vcTitle)/2,
                              p_txt             =>rChart.vcTitle,
                              p_degrees_rotation=>90
                             );
          nLeftSpace:=nLeftSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_RIGHT THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+rChart.nWidth-nRightSpace,
                              p_y               =>i_nY-(rChart.nHeight-nTopSpace-nBottomSpace)/2-AS_PDF3_MOD.str_len(rChart.vcTitle)/2,
                              p_txt             =>rChart.vcTitle,
                              p_degrees_rotation=>90
                             );
          nRightSpace:=nRightSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        END IF;
      END IF;
    END;

    PROCEDURE PR_RENDER_SUBTITLE IS
    BEGIN
      -- Render subtitle
      IF rChart.vcSubTitle IS NOT NULL THEN
        AS_PDF3_MOD.set_color(rChart.vcSubTitleColor);
        -- title is centered
        PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>rChart.vcSubTitleFont,
                                      i_vcStyle    =>rChart.vcSubTitleFontStyle,
                                      i_nSize      =>rChart.nSubTitleFontSize
                                     );

        IF rChart.vcTitlePosition=POSITION_TOP THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+i_rChart.nWidth/2-AS_PDF3_MOD.str_len(rChart.vcSubTitle)/2,
                              p_y               =>i_nY-3-AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)-nTopSpace,
                              p_txt             =>rChart.vcSubTitle);
          nTopSpace:=nTopSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_BOTTOM THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+i_rChart.nWidth/2-AS_PDF3_MOD.str_len(rChart.vcSubTitle)/2,
                              p_y               =>i_nY+6-rChart.nHeight+nBottomSpace,
                              p_txt             =>rChart.vcSubTitle);
          nBottomSpace:=nBottomSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_LEFT THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+3+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+nLeftSpace,
                              p_y               =>i_nY-(rChart.nHeight-nTopSpace-nBottomSpace)/2-AS_PDF3_MOD.str_len(rChart.vcSubTitle)/2,
                              p_txt             =>rChart.vcSubTitle,
                              p_degrees_rotation=>90
                             );
          nLeftSpace:=nLeftSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        ELSIF rChart.vcTitlePosition=POSITION_RIGHT THEN
          AS_PDF3_MOD.put_txt(p_x               =>i_nX+rChart.nWidth-nRightSpace,
                              p_y               =>i_nY-(rChart.nHeight-nTopSpace-nBottomSpace)/2-AS_PDF3_MOD.str_len(rChart.vcSubTitle)/2,
                              p_txt             =>rChart.vcSubTitle,
                              p_degrees_rotation=>90
                             );
          nRightSpace:=nRightSpace+AS_PDF3_MOD.get(AS_PDF3_MOD.c_get_fontsize)+10;
        END IF;
      END IF;
    END;

  BEGIN
    PK_JRXML2PDF_UTIL.PR_SET_FONT(i_vcFont     =>PK_JRXML2PDF_TYPES.ARIAL,
                                  i_vcStyle    =>PK_JRXML2PDF_TYPES.FONT_NORMAL,
                                  i_nSize      =>10
                                 );
    -- calculate measures
    FOR i IN 1..rChart.lValueAxis.COUNT LOOP
      PR_CALC_AXIS_MEASURES(i_rChart     =>rChart,
                            io_rAxis     =>rChart.lValueAxis(i),
                            i_nAxisIndex =>i
                           );
    END LOOP;
    FOR i IN 1..rChart.lRangeAxis.COUNT LOOP
      PR_CALC_AXIS_MEASURES(i_rChart     =>rChart,
                            io_rAxis     =>rChart.lRangeAxis(i),
                            i_nAxisIndex =>i
                           );
    END LOOP;
    IF    rChart.vcBorderColor IS NOT NULL
       OR rChart.vcBgColor IS NOT NULL THEN
      -- render background
      AS_PDF3_MOD.rect(p_x          =>i_nX,
                       p_y          =>i_nY-rChart.nHeight,
                       p_width      =>rChart.nWidth,
                       p_height     =>rChart.nHeight,
                       p_line_color =>REPLACE(rChart.vcBorderColor, '#', ''),
                       p_fill_color =>REPLACE(rChart.vcBgColor, '#', ''),
                       p_line_width =>COALESCE(rChart.nBorderWidth, PK_JRXML2PDF_TYPES.THIN)
                      );
    END IF;
    PR_RENDER_TITLE;

    PR_CALC_LEGEND_MEASURE;

    PR_RENDER_SUBTITLE;

    FOR i IN 1..rChart.lValueAxis.COUNT LOOP
      PR_CALC_AXIS_SPACE(rChart.lValueAxis(i));
    END LOOP;
    FOR i IN 1..rChart.lRangeAxis.COUNT LOOP
      PR_CALC_AXIS_SPACE(rChart.lRangeAxis(i));
    END LOOP;


    rChartRect.nX:=i_nX+nLeftSpace;
    rChartRect.nY:=i_nY-rChart.nHeight+nBottomSpace;
    IF rChart.vcIs3D=PK_JRXML2PDF_TYPES.YES THEN
      rChartRect.nWidth:=rChart.nWidth-nLeftSpace-nRightSpace-10;
      rChartRect.nHeight:=rChart.nHeight-nTopSpace-nBottomSpace-10;
    ELSE
      rChartRect.nWidth:=rChart.nWidth-nLeftSpace-nRightSpace;
      rChartRect.nHeight:=rChart.nHeight-nTopSpace-nBottomSpace;
    END IF;

    -- set empty series-colour
    FOR i IN 1..rChart.lSeries.COUNT LOOP
      IF rChart.lSeries(i).vcColor IS NULL THEN
        rChart.lSeries(i).vcColor:=lDefaultColors(iColor);
        IF iColor<lDefaultColors.COUNT THEN
          iColor:=iColor+1;
        ELSE
          iColor:=1;
        END IF;
      END IF;
    END LOOP;

    --set empty series paths
    FOR i IN 1..rChart.lSeries.COUNT LOOP
      IF rChart.lSeries(i).lShapePath.COUNT=0 THEN
        rChart.lSeries(i).lShapePath:=lDefaultShapes(iShape);
        IF iShape<lDefaultShapes.COUNT THEN
          iShape:=iShape+1;
        ELSE
          iShape:=1;
        END IF;
      END IF;
    END LOOP;
    -- now render value-axis and range
    FOR i IN 1..rChart.lValueAxis.COUNT LOOP
      PR_RENDER_AXIS(rChart, rChart.lValueAxis(i), i, rChartRect);
    END LOOP;

    FOR i IN 1..rChart.lRangeAxis.COUNT LOOP
      PR_RENDER_AXIS(rChart, rChart.lRangeAxis(i), i, rChartRect);
    END LOOP;

    FOR i IN 1..rChart.lPlots.COUNT LOOP
      PR_RENDER_PLOT(rChart,
                     i,
                     rChartRect
                    );
    END LOOP;

    PR_RENDER_LEGEND(rChart);
  END;


BEGIN
  lDefaultColors(1):='FF5555';
  lDefaultColors(2):='5555FF';
  lDefaultColors(3):='55FF55';
  lDefaultColors(4):='FFFF55';
  lDefaultColors(5):='FF55FF';
  lDefaultColors(6):='55FFFF';
  lDefaultColors(7):='FFAFAF';
  lDefaultColors(8):='808080';
  lDefaultColors(9):='C00000';
  lDefaultColors(10):='0000C0';
  lDefaultColors(11):='00C000';
  lDefaultColors(12):='C0C000';
  lDefaultColors(13):='C000C0';
  lDefaultColors(14):='00C0C0';
  lDefaultColors(15):='404040';

  PR_DEFAULT_SHAPES;

  PR_INIT_DEFAULT_NUM_TICKS;
  PR_INIT_DEFAULT_DATE_TICKS;

END;
/
