USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_queries_full]    Script Date: 12/23/2008 17:13:25 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_queries_full]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_queries_full]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_queries_full]    Script Date: 12/23/2008 17:13:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO



CREATE PROCEDURE [ssas].[get_queries_full] (
    @TopCount int = 0
    , @CountThreshold int = 1
    , @DurationThreshold NUMERIC(14,4) = 0
    , @CPUTimeThreshold NUMERIC(14,4) = 0
--    , @Cubename nvarchar(128) = NULL
--    , @TraceDatabaseName nvarchar(128)
--    , @TraceInstanceName nvarchar(128)
    , @workload_list nvarchar(max) = NULL
    , @Debug bit = 0
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
	        RAISERROR('get_queries_full: You must enter a delimited set of test runs', 16, 0)
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
		, QueryID int
		, InstanceName nvarchar(128)
		, DatabaseName nvarchar(128)
		, CubeName nvarchar(128)
		, NumberExecutions int
		, AverageDuration NUMERIC(14,2)
		, TotalDuration  NUMERIC(14,1)
		, AverageCPUTime NUMERIC(14,2)
		, TotalCPUTime NUMERIC(14,1)
		, AverageStorageEngineDuration NUMERIC(14,2)
		, TotalStorageEngineDuration NUMERIC(14,2)
		, AverageFormulaEngineDuration NUMERIC(14,2)
		, TotalFormulaEngineDuration NUMERIC(14,2)
		, AverageCachedSubcubes NUMERIC(14,1)
		, TotalCachedSubcubes INT
		, AverageNonCachedSubcubes NUMERIC(14,1)
		, TotalNonCachedSubcubes INT
		, AverageNumberPartitions NUMERIC(14,1)
		, TotalNumberPartitions INT
		, AverageNumberAggregations NUMERIC(14,1)
		, TotalNumberAggregations INT
		, AverageNumberMeasureGroups NUMERIC(14,1)
		, TotalNumberMeasureGroups INT
		, BeginTime Datetime
		, EndTime Datetime
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
	' 	+ CASE WHEN @TopCount > 0 THEN  ' TOP('  + CAST(@TopCount AS nvarchar(8)) + ') ' ELSE '' END + '
		''' + @workload + ''' AS workload_name
			  ,	''' + @workload_list_orig + ''' AS workload_list
			  , ''' + @workload_description + ''' AS workload_description
			  , ''' + @mdw_database_name + ''' AS mdw_database_name
			  , qev.QueryID
				,i.InstanceName
				,db.DatabaseName
				,c.CubeName 
			  , COUNT(qev.queryexecutionid) AS NumberExecutions
			   , CAST(AVG(qev.Duration/1000.0) AS NUMERIC(14,2)) AS AverageDuration
				,CAST(SUM(qev.Duration/1000.0) AS NUMERIC(14,1)) AS TotalDuration
			   , CAST(AVG(qev.CPUTime/1000.0) AS NUMERIC(14,2)) AS AverageCPUTime
				,CAST(SUM(qev.CPUTime/1000.0) AS NUMERIC(14,1)) AS TotalCPUTime
			   , CAST(AVG(qev.StorageEngineDuration/1000.0) AS NUMERIC(14,2)) AS AverageStorageEngineDuration
			   , CAST(SUM(qev.StorageEngineDuration/1000.0) AS NUMERIC(14,2)) AS TotalStorageEngineDuration
			   , CAST(AVG(qev.FormulaEngineDuration/1000.0) AS NUMERIC(14,2)) AS AverageFormulaEngineDuration
			   , CAST(SUM(qev.FormulaEngineDuration/1000.0) AS NUMERIC(14,2)) AS TotalFormulaEngineDuration
			   , CAST(AVG(cast(qev.NumberCachedSubcubes as float)) AS NUMERIC (14,1)) AS AverageCachedSubcubes
			   , SUM(qev.NumberCachedSubcubes) AS TotalCachedSubcubes
			   , CAST(AVG(cast(qev.NumberNonCachedSubcubes as float)) AS NUMERIC (14,1)) AS AverageNonCachedSubcubes
			   , SUM(qev.NumberNonCachedSubcubes) AS TotalNonCachedSubcubes
			   , CAST(AVG(cast(qev.NumberPartitions as float)) AS NUMERIC (14,1)) AS AverageNumberPartitions
			   , SUM(qev.NumberPartitions) AS TotalNumberPartitions
			   , CAST(AVG(cast(qev.NumberAggregations as float)) AS NUMERIC (14,1)) AS AverageNumberAggregations
			   , SUM(qev.NumberAggregations) AS TotalNumberAggregations
			   , CAST(AVG(cast(qev.NumberMeasureGroups as float)) AS NUMERIC (14,1)) AS AverageNumberMeasureGroups
			   , SUM(qev.NumberMeasureGroups) AS TotalNumberMeasureGroups
			   , MIN(qev.StartDateTime) AS BeginTime
			   , MAX(qev.EndDateTime) AS EndTime
			  FROM ' + @mdw_database_name + '.[SSAS].[SSASQueryExecutionEvents]   AS qev
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASQueryExecution] as qe
				ON qev.QueryExecutionID = qe.QueryExecutionID 
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASCube] AS c 
				ON c.CubeID = qe.CubeID
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASDatabase] AS db 
				ON db.DatabaseID = c.DatabaseID
			  JOIN ' + @mdw_database_name + '.[SSAS].[SSASInstance] AS i ON i.InstanceID = db.InstanceID
			  WHERE db.DatabaseName = ''' + @database_name_filter + '''
				AND i.InstanceName =  ''' + @instance_name_filter + '''
				AND qev.StartDateTime BETWEEN COALESCE(' + '''' + CONVERT(varchar(30),@start_time, 109) + ''', ''1/1/1900'') 
				AND COALESCE(' + '''' + Convert(varchar(30), @end_time,109) + ''', ''12/31/9999'')	
				GROUP BY qev.QueryID
  				, db.DatabaseName
					, i.InstanceName
					, c.CubeName 
			HAVING COUNT(qev.QueryExecutionID) >= ' + CAST(@CountThreshold AS nvarchar(20)) + '
				AND CAST(AVG(qev.Duration/1000.0) AS NUMERIC(14,4)) >= ' + CAST(@DurationThreshold AS nvarchar(20)) + '
				AND CAST(AVG(qev.CPUTime/1000.0) AS NUMERIC(14,4)) >= ' + CAST(@CPUTimeThreshold AS nvarchar(20)) + '
			ORDER BY AverageDuration DESC  
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
		, QueryID 
		, InstanceName 
		, DatabaseName 
		, CubeName 
		, NumberExecutions 
		, AverageDuration 
		, TotalDuration
		, AverageCPUTime 
		, TotalCPUTime
		, AverageStorageEngineDuration 
		, TotalStorageEngineDuration
		, AverageFormulaEngineDuration
		, TotalFormulaEngineDuration 
		, AverageCachedSubcubes 
		, TotalCachedSubcubes
		, AverageNonCachedSubcubes 
		, TotalNonCachedSubcubes
		, AverageNumberPartitions 
		, TotalNumberPartitions
		, AverageNumberAggregations 
		, TotalNumberAggregations
		, AverageNumberMeasureGroups 
		, TotalNumberMeasureGroups
		, BeginTime 
		, EndTime 
		FROM #T
		Order By workload_name
    RETURN
END;


GO


