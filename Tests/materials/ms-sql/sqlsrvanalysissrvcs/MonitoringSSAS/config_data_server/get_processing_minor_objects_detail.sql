USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_minor_objects_detail]    Script Date: 12/23/2008 17:12:35 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_processing_minor_objects_detail]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_processing_minor_objects_detail]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_minor_objects_detail]    Script Date: 12/23/2008 17:12:35 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO




CREATE PROC [ssas].[get_processing_minor_objects_detail] (
      @ConnectionID int 
    , @ServerName nvarchar (256) 
    , @DatabaseName nvarchar (256) 
    , @BeginTime datetime 
    , @EndTime datetime 
    , @ObjectType int 
    , @DimensionName nvarchar (256) = NULL
    , @AttributeName nvarchar (256) = NULL
    , @CubeName nvarchar (256) = NULL
    , @MeasureGroupName nvarchar (256) = NULL
    , @PartitionName nvarchar (256) = NULL
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
		RAISERROR('[SSAS].[GetProcessingMinorObjectsDetail]: ObjectType must be 100006 or 1000016', 16, 1)
		RETURN
	END
	
	IF @ObjectType = 100006 AND (@DimensionName IS NULL OR @AttributeName IS NULL)
	BEGIN
		RAISERROR('[SSAS].[GetProcessingMinorObjectsDetail]: ObjectType 100006 must have a DimensionName and AttributeName', 16, 1)
		RETURN
	END
	IF @ObjectType = 100016 AND (@MeasureGroupName IS NULL OR @CubeName IS NULL OR @PartitionName IS NULL)
	BEGIN
		RAISERROR('[SSAS].[GetProcessingMinorObjectsDetail]: ObjectType 100016 must have a CubeName, MeasureGroupName and PartitionName', 16, 1)
		RETURN
	END
	-- Parameters are OK
	IF @ObjectType = 100006
	BEGIN
	    -- Construct query
	    	    SET @SQLExecString = 

N'		
		SELECT
			ConnectionID
			, ' + CAST(@ObjectType AS nvarchar(20)) + ' AS ParentObjectType
			, ObjectType
			, TextData
			, EventSubclass
			, StartTime
			, EndTime
			, CPUTime
			, Duration
			, DimensionName
			, AttributeName
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
		AND EndTime <= ''' + CONVERT(varchar(30), dateadd(s,1,@EndTime), 109) + '''
		AND ObjectType IN(100007, 100008)
		AND DimensionName = ''' + @DimensionName + '''
		AND AttributeName = ''' + @AttributeName + '''
		--AND EventSubclass <> ''Process''
		ORDER BY StartTime
		'
	END
	IF @ObjectType = 100016
	BEGIN
		    	    SET @SQLExecString = 
N'	
		SELECT
			ConnectionID
			, ServerName
			, DatabaseName
			,  ' + CAST(@ObjectType AS nvarchar(20)) + ' AS ParentObjectType
			, TextData
			, ObjectType
			, EventSubclass
			, StartTime
			, EndTime
			, CPUTime
			, Duration
			, DimensionName
			, AttributeName
			, CubeName
			, MeasureGroupName
			, PartitionName
			, ''' + @workload_name + ''' AS workload_name
			,''' + @workload_description + ''' AS workload_description
		    ,''' + @mdw_database_name + ''' AS mdw_database_name

		FROM ' + @mdw_database_name + '.[SSAS].[SSASProcessingArchive] AS A
		WHERE ConnectionID = ' + CAST(@ConnectionID AS nvarchar(20)) + '
		AND ServerName = ''' + @ServerName + '''
		AND DatabaseName = ''' + @DatabaseName + '''
		AND StartTime >= ''' + CONVERT(varchar(30), @BeginTime, 109) + ''' 
		AND EndTime <= ''' + CONVERT(varchar(30), dateadd(s,1,@EndTime), 109) + '''
		AND ObjectType IN (100021)
		AND MeasureGroupName = ''' + @MeasureGroupName + '''
		AND CubeName = ''' + @CubeName + '''
		AND PartitionName = ''' + @PartitionName + '''
		AND EventSubclass <> ''Process''
		ORDER BY StartTime
		'
	END

	IF @Debug = 1
	    PRINT @SQLExecString
	ELSE
	    EXEC (@SQLExecString)
	
END

RETURN




GO


