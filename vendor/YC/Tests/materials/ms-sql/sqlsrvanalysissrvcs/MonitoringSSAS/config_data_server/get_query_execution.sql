USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_execution]    Script Date: 12/23/2008 17:14:07 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_query_execution]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_query_execution]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_execution]    Script Date: 12/23/2008 17:14:07 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE proc [ssas].[get_query_execution] (
	@QueryID int
   , @workload_list nvarchar(max) = NULL
    , @CountThreshold int = 1
    , @DurationThreshold NUMERIC(14,4) = 0
    , @CPUTimeThreshold NUMERIC(14,4) = 0


--	,@start_time datetime = null
--    ,@end_time datetime = null  
--    ,@instance_name_filter nvarchar (128) = null
--    ,@database_name_filter nvarchar (128) = null
--    ,@cube_name_filter nvarchar (128) = null
--    ,@mdw_database_name nvarchar(128) = null
--    ,@workload_name nvarchar(128) = NULL
--    ,@workload_description nvarchar(128) = NULL
    ,@Debug bit = 0
)
AS

BEGIN
    SET NOCOUNT ON
	-- =================================================================================
	-- Test the parameters
	-- =================================================================================
	SET @workload_list = LTRIM(RTRIM(@workload_list))
	IF LEN(@workload_list) = 0
	    BEGIN
	        RAISERROR('get_query_execution: You must enter a delimited set of test runs', 16, 0)
	        RETURN
	    END
	-- =================================================================================
	-- Prepare tables and variables
	-- =================================================================================
	IF OBJECT_ID('tempdb.dbo.#T') IS NOT NULL DROP TABLE #T
	CREATE TABLE #T (
		workload_name nvarchar(128)
		, workload_list nvarchar(max)
		, workload_description nvarchar(128)
		, mdw_database_name nvarchar(128)
		, QueryExecutionID int
		, QueryID int		
		, Duration NUMERIC(14,4)
		, CPUTime NUMERIC(14,4)
		, StorageEngineDuration NUMERIC(14,4)
		, FormulaEngineDuration NUMERIC(14,4)
		, NumberCachedSubcubes int
		, NumberNonCachedSubcubes int
		, NumberPartitions int
		, NumberAggregations int
		, NumberMeasureGroups int
		, BeginTime Datetime
		, EndTime Datetime
		, InstanceName nvarchar(128)
		, DatabaseName nvarchar(128)
		, CubeName nvarchar(128)
		, WindowsUserName nvarchar(128)
		, WindowsDomainName nvarchar(128)
		, SessionID nvarchar(255)
		, ConnectionID int
		, SPID int
		)
	-- Declare variables
    DECLARE @SQLExecString nvarchar(4000)
		, @workload nvarchar(128)
		, @workload_counter int
        , @start_time datetime
		, @end_time datetime
		, @instance_name_filter nvarchar(128)
		, @mdw_database_name nvarchar(128)
		, @interval int
		, @database_name_filter nvarchar(128)
		, @workload_list_orig nvarchar(128)
		, @workload_description nvarchar(128)


	-- =================================================================================
	-- Process test runs
	-- =================================================================================
	set @workload_list_orig = @workload_list
	SET @workload_counter = 1
    SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
    SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
    SET @workload = LTRIM(@workload)
    IF @debug = 1 SELECT @workload
    -- Intialize the query parameters
	SELECT @start_time = start_time
		, @end_time = end_time
		,@instance_name_filter = instance_name_filter
		, @mdw_database_name = mdw_database_name
		, @database_name_filter = database_name_filter
		, @interval = collection_interval_perfmon 
		, @workload_description = workload_description 
	    FROM mdw.workload_runs 
	    WHERE workload_name = @workload
	    
	    
	-- Processing loop
    WHILE @workload <> N'' 
    BEGIN
	    -- Construct query
	    SET @SQLExecString = 

N'   INSERT #T
SELECT
 
	  ''' + @workload + ''' AS workload_name
	  ,	''' + @workload_list_orig + ''' AS workload_list
	  , ''' + @workload_description + ''' AS workload_description
	  , ''' + @mdw_database_name + ''' AS mdw_database_name
	  ,qev.QueryExecutionID
	  ,qe.QueryID
      ,CAST(qev.Duration/1000.0 AS NUMERIC(14,4)) AS Duration
      ,CAST(qev.CPUTime/1000.0 AS NUMERIC(14,4)) AS CPUTime
      ,CAST(qev.StorageEngineDuration/1000.0 AS NUMERIC(14,4)) AS StorageEngineDuration
      ,CAST(qev.FormulaEngineDuration/1000.0 AS NUMERIC(14,4)) AS FormulaEngineDuration
      ,CAST(qev.NumberCachedSubcubes AS NUMERIC (14,1)) AS NumberCachedSubcubes
      ,CAST(qev.NumberNonCachedSubcubes AS NUMERIC (14,1)) AS NumberNonCachedSubcubes
      ,CAST(qev.NumberPartitions AS NUMERIC (14,1)) AS NumberPartitions
      ,CAST(qev.NumberAggregations AS NUMERIC (14,1)) AS NumberAggregations
      ,CAST(qev.NumberMeasureGroups AS NUMERIC (14,1)) AS NumberMeasureGroups
      ,qev.StartDateTime AS BeginTime
      ,qev.EndDateTime AS EndTime
      ,i.InstanceName
      ,db.DatabaseName
      ,c.CubeName
      ,u.WindowsUserName
      ,u.WindowsDomainName
      ,qe.SessionID
      ,qe.ConnectionID
      ,qe.SPID
  FROM ' + @mdw_database_name + '.[SSAS].[SSASQueryExecutionEvents] qev
  JOIN ' + @mdw_database_name + '.[SSAS].[SSASQueryExecution] qe
  ON qev.QueryExecutionID = qe.QueryExecutionID
  JOIN ' + @mdw_database_name + '.[SSAS].[SSASUser] u
  ON qe.SSASUserID=u.SSASUserID
  			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASCube] AS c 
				ON c.CubeID = qe.CubeID
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASDatabase] AS db 
				ON db.DatabaseID = c.DatabaseID
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASInstance] AS i ON i.InstanceID = db.InstanceID and db.DatabaseID=c.DatabaseID
			WHERE qe.QueryID = ' + CAST(@QueryID AS nvarchar(20)) + '
			AND i.InstanceName = ''' + @instance_name_filter + '''
				AND db.DatabaseName = ''' + @database_name_filter + '''
				AND qev.StartDateTime BETWEEN COALESCE(' + '''' + Convert(varchar(30),@start_time, 109) + ''', ''1/1/1900'') 
					AND COALESCE(' + '''' + Convert(varchar(30),@end_time,109) + ''', ''12/31/9999'')  
GROUP BY qev.QueryExecutionID
		, qev.Duration
		, qev.CPUTime
		, qev.StorageEngineDuration
		, qev.FormulaEngineDuration
		, qev.NumberCachedSubcubes
		, qev.NumberNonCachedSubcubes
		, qev.NumberPartitions
		, qev.NumberAggregations
		, qev.NumberMeasureGroups
		, qev.EndDateTime
		, qev.StartDateTime
		, u.WindowsUserName
		, u.WindowsDomainName
		, qe.queryID
		,c.CubeName
		,i.InstanceName
		,db.DatabaseName
		,qe.SessionID
		,qe.ConnectionID
		,qe.SPID
		
HAVING CAST(AVG(qev.Duration/1000.0) AS NUMERIC(14,4)) >= ' + CAST(@DurationThreshold AS nvarchar(20)) + '
		AND CAST(AVG(qev.CPUTime/1000.0) AS NUMERIC(14,4)) >= ' + CAST(@CPUTimeThreshold AS nvarchar(20)) + '
ORDER BY Duration DESC
'
	IF @Debug = 1
	    PRINT @SQLExecString
	ELSE
	    EXEC (@SQLExecString)
	
        -- Reinitialize for next iteration        
        SET @workload_counter += 1
        SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
        SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
        SET @workload = LTRIM(@workload)
        -- Intialize the query parameters
	    SELECT @start_time = start_time
	    , @end_time = end_time
	    , @instance_name_filter = instance_name_filter
	    , @mdw_database_name = mdw_database_name
	    , @interval = collection_interval_perfmon 
	    , @workload_description = workload_description  
	        FROM MDW.workload_runs 
	        WHERE workload_name = @workload
	    IF @start_time IS NULL OR @end_time IS NULL OR @instance_name_filter IS NULL OR @mdw_database_name IS NULL
	        BREAK

END
	SELECT    
	   	  workload_name 
	   	,workload_list
	   	,workload_description
	   	, mdw_database_name
	   	, QueryExecutionID
		, QueryID 
		, Duration 
		, CPUTime 
		, StorageEngineDuration 
		, FormulaEngineDuration 
		, NumberCachedSubcubes 
		, NumberNonCachedSubcubes 
		, NumberPartitions 
		, NumberAggregations 
		, NumberMeasureGroups 
		, BeginTime 
		, EndTime 
		, InstanceName 
		, DatabaseName 
		, CubeName 
		, WindowsUserName
		, WindowsDomainName
		, SessionID
		, ConnectionID
		, SPID
		FROM #T
		Order By Duration desc
    RETURN
END;
 
--exec ssas.get_query_execution @QueryID=N'96',@DurationThreshold=0,@CPUTimeThreshold=0,@workload_list=N'test1, test2',@Debug=1

--exec ssas.get_query_execution @QueryID=N'96',@DurationThreshold=0,@CPUTimeThreshold=0,@workload_list=N'test1,test2',@Debug=0





GO


