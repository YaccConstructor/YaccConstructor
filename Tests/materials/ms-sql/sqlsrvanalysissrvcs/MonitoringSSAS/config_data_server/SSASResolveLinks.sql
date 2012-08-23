USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASResolveLinks]    Script Date: 12/23/2008 15:00:57 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASResolveLinks]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[SSASResolveLinks]
GO

USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASResolveLinks]    Script Date: 12/23/2008 15:00:57 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [ssas].[SSASResolveLinks]
AS

UPDATE wt
SET 
	P_RowNumber_Query = (SELECT MIN(RowNumber) 
						FROM [SSAS].SSASWorkTraceTable qe 
						WHERE qe.RowNumber >= wt.RowNumber 
							AND qe.EventClass = 'Query End' AND qe.EventSubclass = 'MDXQuery'
							AND qe.SessionID = wt.SessionID
							AND qe.ServerName = wt.ServerName)
	, P_RowNumber_Subcube = CASE WHEN EventClass = 'Query End' THEN NULL
							ELSE
	
								(SELECT MIN(RowNumber) 
								FROM [SSAS].SSASWorkTraceTable qs 
								WHERE qs.RowNumber >= wt.RowNumber 
									AND qs.EventClass = 'Query Subcube' AND qs.EventSubclass IN ('Cache data','Non-cache data')
									AND qs.SessionID = wt.SessionID
									AND qs.ServerName = wt.ServerName)
							END
	, P_ExecutionID = (SELECT COUNT(*) 
								FROM [SSAS].SSASWorkTraceTable qei 
								WHERE qei.RowNumber < wt.RowNumber 
								AND qei.EventClass = 'Query End' AND qei.EventSubClass = 'MDXQuery')
	
	
FROM [SSAS].SSASWorkTraceTable wt WITH (TABLOCKX)



--SELECT rownumber, P_RowNumber_FirstSubCube,cubename FROM [SSAS].[SSASWorkTable_Transform]
/* find any subcube that belongs to a query */
UPDATE wt
SET P_RowNumber_FirstSubCube = (SELECT MIN(RowNumber) 
								FROM [SSAS].SSASWorkTraceTable sc 
								WHERE sc.P_RowNumber_Query = wt.RowNumber
									AND sc.EventClass = 'Query Subcube' AND sc.EventSubclass IN ('Cache data','Non-cache data'))
FROM [SSAS].SSASWorkTraceTable wt WITH (TABLOCKX)

UPDATE wt1 SET CubeName = 
                ISNULL(
                                CASE WHEN wt1.P_RowNumber_Query IS NULL AND wt1.ObjectPath IS NULL THEN  'FE_Cache' 
                                ELSE (SELECT TOP 1 wt2.CubeName
                                                                FROM [SSAS].SSASWorkTracetable AS wt2
                                                                WHERE RowNumber < wt1.RowNumber 
                                                                AND EventClass = 'Query Subcube' 
                                                                AND P_RowNumber_Query = wt1.P_RowNumber_Query
                                                                AND SessionID = wt1.SessionID)
                                END, 'FE_Cache') 
                FROM [SSAS].SSASWorkTracetable AS wt1 WITH (TABLOCKX)
                WHERE EventClass = 'Query End'


			



GO


