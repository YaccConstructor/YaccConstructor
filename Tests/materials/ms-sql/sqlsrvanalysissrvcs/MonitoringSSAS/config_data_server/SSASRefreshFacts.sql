USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASRefreshFacts]    Script Date: 12/30/2008 19:24:10 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASRefreshFacts]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[SSASRefreshFacts]
GO

USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASRefreshFacts]    Script Date: 12/30/2008 19:24:10 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [ssas].[SSASRefreshFacts]
AS

	--partition events
BEGIN TRAN
INSERT INTO [SSAS].[SSASQueryPartitionEvents]
           ([PartitionID]
           ,[AggregationID]
           ,[SubCubeID]
           ,[PartitionDuration]
           ,[DW_RowNumber]
           ,[DW_BatchID])
SELECT 
	pt.PartitionID
	, ag.AggregationID
	, sc.SubCubeID
	, swt.Duration
	, swt.RowNumber
	, swt.P_BatchID
FROM [SSAS].[SSASWorkTable_Transform] AS swt
JOIN [SSAS].[SSASSubCube] AS sc
	ON swt.P_RowNumber_SubCube = sc.DW_RowNumber_SubCube AND swt.P_BatchID = sc.DW_BatchID
	JOIN [SSAS].[SSASInstance] AS i
		ON swt.ServerName = i.InstanceName
	JOIN [SSAS].[SSASDatabase] AS db
		ON swt.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
	JOIN [SSAS].[SSASCube] AS c
		ON swt.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
	JOIN [SSAS].[SSASMeasureGroup] AS mg
		ON swt.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
	JOIN [SSAS].[SSASPartition] AS pt
		ON swt.PartitionName = pt.PartitionName AND pt.MeasureGroupID = mg.MeasureGroupID
	LEFT JOIN [SSAS].SSASAggregation AS ag
		ON swt.AggregationName = ag.AggregationName AND pt.PartitionID = ag.PartitionID 
WHERE swt.EventClass = 'Progress Report End' AND EventSubClass = 'Query' AND P_RowNumber_Query IS NOT NULL
		AND swt.P_BatchID NOT IN (SELECT DISTINCT DW_BatchID FROM [SSAS].SSASQueryPartitionEvents WHERE DW_BatchID IS NOT NULL)
--select * from [SSAS].[SSASQueryPartitionEvents]
--subcube events
INSERT INTO [SSAS].[SSASQuerySubCubeEvents]
			   ([SubCubeID]
			   ,[Cached]
			   ,[QueryExecutionID]
			   ,[MeasureGroupID]
			   ,[StorageEngineDuration]
			   ,[NumberPartitions]
			   ,[NumberAggregations]
			   ,[DW_RowNumber]
			   ,[DW_BatchID])
SELECT 
		sc.SubCubeID
		,Case WHEN swt.EventSubclass = 'Cache data' THEN 0 ELSE 1 END as cached
		,qe.QueryExecutionID 
		, mg.MeasureGroupID
		,swt.Duration
		,ISNULL (qpe.NumberPartitions,0) AS NumberPartitions
		,ISNULL (qpe.NumberAggregations,0) AS NumberAggregations
		,swt.RowNumber
		,swt.P_BatchID
	FROM [SSAS].[SSASWorkTable_Transform] AS swt
	JOIN [SSAS].[SSASInstance] AS i
		ON swt.ServerName = i.InstanceName
	JOIN [SSAS].[SSASDatabase] AS db
		ON swt.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
	JOIN [SSAS].[SSASCube] AS c
		ON swt.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
	JOIN [SSAS].[SSASMeasureGroup] AS mg
		ON swt.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
	JOIN [SSAS].[SSASSubCube] AS sc
		ON swt.SubCubeName = sc.SubCubeName AND swt.P_RowNumber_SubCube = sc.DW_RowNumber_SubCube AND sc.MeasureGroupID = mg.MeasureGroupID
	left JOIN [SSAS].[SSASQueryExecution] AS qe
		ON swt.P_ExecutionID = qe.DW_ExecutionID and SWT.P_BatchID = qe.DW_BatchID
	LEFT JOIN
		(
		SELECT 
			SubCubeID
			, COUNT(PartitionID) AS NumberPartitions
			,Count (AggregationID) as NumberAggregations
		FROM [SSAS].SSASQueryPartitionEvents qpe
		GROUP BY SubCubeID
		) AS qpe
		ON qpe.SubCubeID=sc.SubCubeID	
	
	WHERE swt.EventClass = 'Query Subcube' AND P_RowNumber_Query IS NOT NULL
		AND swt.p_BatchID NOT IN (SELECT DISTINCT DW_BatchID FROM [SSAS].[SSASQuerySubCubeEvents] WHERE DW_BatchID IS NOT NULL) AND ((qpe.NumberPartitions>0 or qpe.NumberAggregations>0) OR swt.EventSubclass = 'cache data')
--	 SELECT * FROM [SSAS].[SSASQuerySubCubeEvents] 

--QueryExecutionEvents
INSERT INTO [SSAS].[SSASQueryExecutionEvents]
			   ([QueryExecutionID]
			   ,[QueryID]
			   ,[Duration]
			   ,[CPUTime]  
			   ,[StorageEngineDuration]
			   ,[FormulaEngineDuration]
			   ,[NumberCachedSubcubes]
			   ,[NumberNonCachedSubcubes]
			   ,[NumberPartitions]
			   ,[NumberAggregations]
			   ,[NumberMeasureGroups]
			   ,[EndDateTime]
			   ,[EndDateTime_WithGMTOffset]
			   ,[StartDateTime]
			   ,[StartDateTime_WithGMTOffset]
			   ,[EndDate]
			   ,[StartDate]
			   ,[EndTime]
			   ,[StartTime]
			   ,[DW_RowNumber]
			   ,[DW_BatchID]
		 )
	SELECT 
		  qe.QueryExecutionID
		  ,qe.QueryID
		  ,swt.Duration
		  ,swt.CPUTime
		  ,ISNULL(qse.StorageEngineDuration,0) AS StorageEngineDuration
		  ,swt.Duration - ISNULL(qse.StorageEngineDuration,0) AS FormulaEngineDuration
 		  ,ISNULL (qse.NumberCachedSubcubes,0) AS NumberCachedSubcubes
		  ,ISNULL (qse.NumberNonCachedSubcubes,0) AS NumberNonCachedSubcubes
		  ,ISNULL (qse.NumberPartitions, 0) AS NumberPartitions
		  ,ISNULL (qse.NumberAggregations, 0) AS NumberAggregations
		  ,ISNULL (qse.NumberMeasureGroups, 0) AS NumberMeasureGroups
		  ,swt.EndTime
		  ,swt.EndTime_WithGMTOffset
		  ,swt.StartTime
		  ,swt.StartTime_WithGMTOffset
		  , CAST(swt.EndTime AS Date) AS EndDate
		  , CAST(swt.StartTime AS Date) AS StartDate
		  , CAST(swt.EndTime AS Time) AS EndTime
		  , CAST(swt.StartTime AS Time) AS StartTime
		  , swt.RowNumber
		  , swt.P_BatchID
	FROM [SSAS].[SSASWorkTable_Transform] AS swt
	JOIN [SSAS].[SSASQueryExecution] qe
		ON qe.DW_ExecutionID = swt.P_ExecutionID AND qe.DW_BatchID = swt.P_BatchID
	LEFT JOIN
			(
			SELECT 
				  QueryExecutionID
				  , SUM(NumberPartitions) AS NumberPartitions
				  , SUM(NumberAggregations) as NumberAggregations
				, COUNT (DISTINCT MeasureGroupID) AS NumberMeasureGroups
				  , SUM(CASE WHEN Cached = 1 THEN 1 ELSE 0 END) AS NumberNonCachedSubcubes
				  , SUM(CASE WHEN Cached = 0 THEN 1 ELSE 0 END) AS NumberCachedSubcubes
				  , SUM(StorageEngineDuration) AS StorageEngineDuration
			FROM [SSAS].[SSASQuerySubCubeEvents] qse
			GROUP BY QueryExecutionID 
			) AS qse
		   ON qse.QueryExecutionID=qe.QueryExecutionID     
	WHERE swt.EventClass = 'Query End' AND P_RowNumber_Query IS NOT NULL
		  AND swt.p_BatchID NOT IN (SELECT DISTINCT DW_BatchID FROM [SSAS].[SSASQueryExecutionEvents] WHERE DW_BatchID IS NOT NULL)

--select * from [SSAS].[SSASQueryExecutionEvents]
--rollback tran
commit tran



GO

