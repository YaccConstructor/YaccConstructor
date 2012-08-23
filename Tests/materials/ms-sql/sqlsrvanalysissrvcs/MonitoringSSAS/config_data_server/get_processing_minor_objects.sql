USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_minor_objects]    Script Date: 12/23/2008 17:11:36 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_processing_minor_objects]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_processing_minor_objects]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_minor_objects]    Script Date: 12/23/2008 17:11:36 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROC [ssas].[get_processing_minor_objects] (
      @ConnectionID int 
    , @ServerName nvarchar (256) 
    , @DatabaseName nvarchar (256) 
    , @BeginTime datetime 
    , @EndTime datetime 
    , @ObjectType int 
    , @DimensionName nvarchar (256) = NULL
    , @CubeName nvarchar (256) = NULL
    , @MeasureGroupName nvarchar (256) = NULL
    , @mdw_database_name nvarchar(128) = NULL
    , @workload_name nvarchar(128) = NULL
    , @workload_description nvarchar(128) = NULL
    , @Debug bit = 0
    )
AS
BEGIN
-- Declare variables
    DECLARE @SQLExecString nvarchar(4000)


	-- =============================================================================
	-- Check Parameters
	-- =============================================================================
	IF @ObjectType NOT IN (100006, 100016)
	BEGIN
		RAISERROR('[SSAS].[GetProcessingMinorObjects]: ObjectType must be 100006 or 100016', 16, 1)
		RETURN
	END
	IF @ObjectType = 100006 AND @DimensionName IS NULL
	BEGIN
		RAISERROR('[SSAS].[GetProcessingMinorObjects]: ObjectType 100006 must have a DimensionName', 16, 1)
		RETURN
	END
	IF @ObjectType = 100016 AND (@MeasureGroupName IS NULL OR @CubeName IS NULL)
	BEGIN
		RAISERROR('[SSAS].GetProcessingMinorObjects: ObjectType 100016 must have both a CubeID and MeasureGroupName', 16, 1)
		RETURN
	END
	-- Parameters are OK
	IF @ObjectType = 100006
	BEGIN
	    -- Construct query
	    	    SET @SQLExecString = 

N'	
		SELECT
			''' + @workload_name + ''' AS workload_name
			, ''' + @workload_description + ''' AS workload_description
			, ConnectionID
			, ' + CAST(@ObjectType AS nvarchar(20)) + ' AS ParentObjectType
			, JobID 
			, SessionID
			, TextData
			, ObjectType
			, (SELECT MIN(StartTime) FROM mdw.[SSAS].[SSASProcessingArchive] AS B 
					WHERE B.ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + ' 
					AND B.ServerName = ''' + @ServerName + '''
					AND B.DatabaseName = ''' + @DatabaseName + '''
					AND B.StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + '''
					AND B.EndTime <= ''' + CONVERT(varchar(30), @EndTime, 109) + '''
					AND B.ObjectType IN(100007, 100008)
					AND B.DimensionName = ''' + @DimensionName + '''
					AND B.AttributeName = A.AttributeName) AS StartTime
			, (SELECT MAX(EndTime) FROM mdw.[SSAS].[SSASProcessingArchive] AS B
					WHERE B.ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + ' 
					AND B.ServerName = ''' + @ServerName + '''
					AND B.DatabaseName = ''' + @DatabaseName + '''
					AND B.StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + '''
					AND B.EndTime <= ''' + CONVERT(varchar(30), @EndTime, 109) + '''
					AND B.ObjectType IN(100007, 100008)
					AND B.DimensionName = ''' + @DimensionName + '''
					AND B.AttributeName = A.AttributeName) AS EndTime
			, DimensionName
			, AttributeName
			, AttributeName as ObjectName
			, CubeName
			, MeasureGroupName
			, ''' + @workload_name + ''' AS workload_name
			,''' + @workload_description + ''' AS workload_description
		    ,''' + @mdw_database_name + ''' AS mdw_database_name

		FROM ' + @mdw_database_name + '.[SSAS].[SSASProcessingArchive] AS A
		WHERE ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + '
		AND ServerName = ''' + @ServerName + '''
		AND DatabaseName = ''' + @DatabaseName + '''
		AND StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + '''
		AND B.EndTime <= ''' + CONVERT(varchar(30), @EndTime, 109) + '''
		AND ObjectType IN (100007, 100008)
		AND DimensionName = ''' + @DimensionName + '''
		AND EventSubclass = ''Process''

		'
	END
	IF @ObjectType = 100016
	BEGIN
SET @SQLExecString = 

N'			SELECT
			ConnectionID
			, ' + CAST(@ObjectType AS nvarchar(20)) + ' AS ParentObjectType
			,JobID
			,SessionID
			, TextData
			, ObjectType
			, (SELECT MIN(StartTime) FROM mdw.[SSAS].[SSASProcessingArchive] AS B 
					WHERE B.ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + ' 
					AND B.ServerName = ''' + @ServerName + '''
					AND B.DatabaseName = ''' + @DatabaseName + '''
					AND B.StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + ''' 
					AND B.EndTime <= ''' + CONVERT(varchar(30), dateadd(s,1,@EndTime), 109) + '''
					AND B.ObjectType IN(100021)
					AND B.CubeName = ''' + @CubeName + '''
					AND B.MeasureGroupName = ''' + @MeasureGroupName + '''
					AND B.PartitionName = A.PartitionName) AS StartTime
			, (SELECT MAX(EndTime) FROM mdw.[SSAS].[SSASProcessingArchive] AS B
					WHERE B.ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + '  
					AND B.ServerName = ''' + @ServerName + '''
					AND B.DatabaseName = ''' + @DatabaseName + '''
					AND B.StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + '''
					AND B.EndTime <= ''' + CONVERT(varchar(30), dateadd(s,1,@EndTime), 109) + '''
					AND B.ObjectType IN(100021)
					AND B.CubeName = ''' + @CubeName + '''
					AND B.MeasureGroupName = ''' + @MeasureGroupName + '''
					AND B.PartitionName = A.PartitionName) AS EndTime
			, DimensionName
			, DimensionName
			, AttributeName
			, CubeName
			, MeasureGroupName
			, PartitionName as ObjectName 
			, PartitionName
			, ''' + @workload_name + ''' AS workload_name
			,''' + @workload_description + ''' AS workload_description
		    ,''' + @mdw_database_name + ''' AS mdw_database_name

		FROM ' + @mdw_database_name + '.[SSAS].[SSASProcessingArchive] AS A
		WHERE ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + '
			AND ServerName = ''' + @ServerName + '''
			AND DatabaseName = ''' + @DatabaseName + '''
			AND StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + ''' 
			AND dateadd(s,-1,EndTime) <= ''' + CONVERT(varchar(30), @EndTime, 109) + '''
			AND ObjectType IN (100021)
			AND MeasureGroupName = ''' + @MeasureGroupName + '''
			AND CubeName = ''' + @CubeName + '''
			AND EventSubclass = ''Process ''
			'
	END

	IF @Debug = 1
	    PRINT @SQLExecString
	ELSE
	    EXEC (@SQLExecString)
	
END

RETURN




GO


