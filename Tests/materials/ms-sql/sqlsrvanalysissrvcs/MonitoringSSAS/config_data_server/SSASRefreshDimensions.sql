USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASRefreshDimensions]    Script Date: 12/23/2008 15:01:43 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASRefreshDimensions]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[SSASRefreshDimensions]
GO

USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASRefreshDimensions]    Script Date: 12/23/2008 15:01:43 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [ssas].[SSASRefreshDimensions]
AS
BEGIN
	-- ==============================================================
	-- User 
	-- ==============================================================
	INSERT INTO [SSAS].[SSASUser] (NTCanonicalUserName, WindowsDomainName, WindowsUserName, DW_BatchID)
		SELECT ISNULL(s.NTCanonicalUserName,'Unknown') as NTCanonicalUserName, ISNULL(s.WindowsDomainName,'Unknown') WindowsDomainName, ISNULL(s.WindowsUserName,'Unknown') as WindowsUserName , MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		LEFT JOIN [SSAS].[SSASUser] AS d
			ON ISNULL(s.NTCanonicalUserName,'Unknown') = d.NTCanonicalUserName
		WHERE d.SSASUserID IS NULL
		GROUP BY s.NTCanonicalUserName, s.WindowsDomainName, s.WindowsUserName
	-- SELECT * FROM [SSAS].[SSASUser]

	-- ==============================================================
	-- Instance 
	-- ==============================================================
	INSERT INTO [SSAS].[SSASInstance] (InstanceName, DW_BatchID)
		SELECT s.ServerName, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		LEFT JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		WHERE i.InstanceID IS NULL
		GROUP BY s.ServerName
	-- SELECT * FROM [SSAS].[SSASInstance]
	
	-- ==============================================================
	-- Database 
	-- ==============================================================
	INSERT INTO [SSAS].[SSASDatabase] (DatabaseName, InstanceID, DW_BatchID)
		SELECT s.DatabaseName, i.InstanceID, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		LEFT JOIN [SSAS].[SSASDatabase] AS d
			ON s.DatabaseName = d.DatabaseName
		WHERE d.DatabaseID IS NULL
		GROUP BY s.DatabaseName, i.InstanceID
	-- SELECT * FROM [SSAS].[SSASDatabase]
	
	-- ==============================================================
	-- Cube 
	-- ==============================================================
	INSERT INTO [SSAS].[SSASCube] (CubeName, DatabaseID, DW_BatchID)
		SELECT s.CubeName, db.DatabaseID ,MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		LEFT JOIN [SSAS].[SSASCube] AS d
			ON s.CubeName = d.CubeName AND d.DatabaseID = db.DatabaseID
		WHERE d.CubeName IS NULL -- EventClass 'Query End' not populated with cube name when resolved from FE
		GROUP BY s.CubeName, db.DatabaseID
	-- SELECT * FROM [SSAS].[SSASCube]

	-- ==============================================================
	-- MeasureGroup
	-- ==============================================================
	INSERT INTO [SSAS].[SSASMeasureGroup] (MeasureGroupName, CubeID, DW_BatchID)
		SELECT s.MeasureGroupName, c.CubeID,  MIN(s.P_BatchID )
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		JOIN [SSAS].[SSASCube] AS c
			ON s.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
		LEFT JOIN [SSAS].[SSASMeasureGroup] AS mg
			ON s.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
		WHERE s.MeasureGroupName IS NOT NULL AND s.EventClass = 'Query Subcube'
			AND mg.CubeID IS NULL
		GROUP BY s.MeasureGroupName , c.CubeID
		-- SELECT * FROM [SSAS].[SSASMeasureGroup]

	-- ==============================================================
	-- Subcube
	-- ==============================================================
	
	INSERT INTO [SSAS].[SSASSubCube] (SubCubeName, MeasureGroupID, DW_RowNumber_SubCube, DW_BatchID)
		SELECT s.SubcubeName , mg.MeasureGroupID, s.P_RowNumber_SubCube, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		JOIN [SSAS].[SSASCube] AS c
			ON s.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
		JOIN [SSAS].[SSASMeasureGroup] AS mg
			ON s.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
		LEFT JOIN [SSAS].[SSASSubCube] AS sc
			ON s.SubCubeName = sc.SubCubeName AND s.P_BatchID = sc.DW_BatchID
		WHERE sc.SubcubeID IS NULL and s.EventClass = 'Query Subcube' --AND [P_RowNumber_Query] IS NOT NULL
		GROUP BY s.SubCubeName, mg.MeasureGroupID, s.P_RowNumber_SubCube
	-- SELECT * FROM [SSAS].[SSASSubCube] 

	-- ==============================================================
	-- Partition
	-- ==============================================================
	INSERT INTO [SSAS].[SSASPartition] (PartitionName, MeasureGroupID, DW_BatchID)
		SELECT s.PartitionName, mg.MeasureGroupID, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		JOIN [SSAS].[SSASCube] AS c
			ON s.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
		JOIN [SSAS].[SSASMeasureGroup] AS mg
			ON s.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
		LEFT JOIN [SSAS].[SSASPartition] AS pt
			ON s.PartitionName = pt.PartitionName AND pt.MeasureGroupID = mg.MeasureGroupID
		WHERE pt.PartitionID IS NULL AND EventClass = 'Progress Report End' 
		GROUP BY s.PartitionName, mg.MeasureGroupID
	-- SELECT * FROM [SSAS].[SSASPartition]


	-- ==============================================================
	-- Aggregation
	-- ==============================================================
	INSERT INTO [SSAS].[SSASAggregation] (AggregationName, PartitionID, DW_BatchID)
		SELECT s.AggregationName, pt.PartitionID, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		JOIN [SSAS].[SSASCube] AS c
			ON s.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
		JOIN [SSAS].[SSASMeasureGroup] AS mg
			ON s.MeasureGroupName = mg.MeasureGroupName AND mg.CubeID = c.CubeID
		JOIN [SSAS].[SSASPartition] AS pt
			ON s.PartitionName = pt.PartitionName AND pt.MeasureGroupID = mg.MeasureGroupID
		LEFT JOIN [SSAS].SSASAggregation AS a
			ON s.AggregationName = a.AggregationName
		WHERE s.EventClass = 'Progress Report End' AND s.AggregationName IS NOT NULL AND a.AggregationName IS NULL
		GROUP BY s.AggregationName, pt.PartitionID
	-- SELECT * FROM [SSAS].SSASAggregation

	-- ==============================================================
	-- Query
	-- ==============================================================
	INSERT INTO [SSAS].[SSASQuery] (QueryString, NK_QueryDataLength, NK_QueryCheckSum, DW_BatchID)
		SELECT s.QueryString, s.QueryDataLength, s.QueryCheckSum, MIN(s.P_BatchID ) 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		LEFT JOIN [SSAS].[SSASQuery] AS q
			ON s.QueryDataLength = q.NK_QueryDataLength AND s.QueryCheckSum = q.NK_QueryCheckSum
		WHERE q.QueryID IS NULL and EventClass = 'Query End' and EventSubclass = 'MDXQuery'
		GROUP BY s.QueryString, s.QueryDataLength, s.QueryCheckSum
	-- SELECT * FROM [SSAS].[SSASQuery]

	-- ==============================================================
	-- Query Execution
	-- ==============================================================
	INSERT INTO [SSAS].[SSASQueryExecution] (QueryID, SSASUserID, SessionID, ConnectionID, SPID, CubeID, DW_RowNumber, DW_ExecutionID, DW_BatchID)
		SELECT q.QueryID
			, u.SSASUserID
			, s.SessionID
			, s.ConnectionID
			, s.SPID
			, c.CubeID
			, s.RowNumber
			, s.P_ExecutionID
			, s.P_BatchID 
		FROM [SSAS].[SSASWorkTable_Transform] AS s
		JOIN [SSAS].[SSASQuery] AS q
			ON s.QueryDataLength = q.NK_QueryDataLength AND s.QueryCheckSum = q.NK_QueryCheckSum
		JOIN [SSAS].[SSASUser] AS u
			ON s.WindowsUserName = u.WindowsUserName AND s.WindowsDomainName = u.WindowsDomainName
		JOIN [SSAS].[SSASInstance] AS i
			ON s.ServerName = i.InstanceName
		JOIN [SSAS].[SSASDatabase] AS db
			ON s.DatabaseName = db.DatabaseName AND db.InstanceID = i.InstanceID
		JOIN [SSAS].[SSASCube] AS c
			ON s.CubeName = c.CubeName AND c.DatabaseID = db.DatabaseID
		LEFT JOIN [SSAS].[SSASQueryExecution] AS qe
			ON s.P_ExecutionID = qe.DW_ExecutionID AND s.P_BatchID = qe.DW_BatchID
		WHERE EventClass = 'Query End' and EventSubclass = 'MDXQuery'
			AND qe.QueryExecutionID IS NULL AND  s.P_BatchID NOT IN (SELECT DISTINCT DW_BatchID FROM [SSAS].[SSASQueryExecution] WHERE DW_BatchID IS NOT NULL)
		GROUP BY s.P_BatchID, s.P_ExecutionID, q.QueryID, u.SSASUserID, c.CubeID, s.SessionID, s.ConnectionID, s.SPID, s.RowNumber
	-- SELECT * FROM [SSAS].[SSASQueryExecution]
END
RETURN



GO


