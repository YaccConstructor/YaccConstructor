USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[get_counter_data_points_by_config]    Script Date: 12/23/2008 17:06:30 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[mdw].[get_counter_data_points_by_config]') AND type in (N'P', N'PC'))
DROP PROCEDURE [mdw].[get_counter_data_points_by_config]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[get_counter_data_points_by_config]    Script Date: 12/23/2008 17:06:30 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [mdw].[get_counter_data_points_by_config] ( 
	 @workload_list nvarchar(max)
	, @performance_object_list nvarchar(max) = NULL
	, @debug bit = 0
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
	        RAISERROR('get_counter_data_points_by_config: You must enter a delimited set of test runs', 16, 0)
	        RETURN
	    END
	-- =================================================================================
	-- Prepare object counters (optional)
	-- =================================================================================
    IF @performance_object_list IS NOT NULL AND @performance_object_list <> ''
        EXEC MDW.set_counter_config_by_list @performance_object_list, @debug
	-- =================================================================================
	-- Prepare tables and variables
	-- =================================================================================
	IF OBJECT_ID('tempdb.dbo.#T') IS NOT NULL DROP TABLE #T
	CREATE TABLE #T (
		 workload_name nvarchar(128)
		, workload_description nvarchar(128)
		, instance_name nvarchar(128)
		, performance_object_name nvarchar(2048)
		, performance_counter_name nvarchar(2048)
		, performance_instance_name nvarchar(2048)
		, collection_time datetimeoffset(7)
		, elapsed_time int
		, adjusted_elapsed_time int
		, formatted_value float
		)
	-- Declare variables
    DECLARE @SQLExecString nvarchar(4000)
    , @workload nvarchar(128)
    , @workload_counter int
    , @start_time datetimeoffset(7)
    , @end_time datetimeoffset(7)
    , @instance_name nvarchar(128)
    , @database_name nvarchar(128)
    , @interval int
    , @workload_description nvarchar(128)

	-- =================================================================================
	-- Process test runs
	-- =================================================================================
	SET @workload_counter = 1
    SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
    SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
    SET @workload = LTRIM(@workload)
    IF @debug = 1 SELECT @workload
    -- Intialize the query parameters
	SELECT @start_time = start_time
	, @end_time = end_time
	, @instance_name = instance_name_filter
	, @database_name = mdw_database_name
	, @interval = collection_interval_perfmon 
	, @workload_description = workload_description 
	    FROM MDW.workload_runs 
	    WHERE workload_name = @workload
	-- Processing loop
    WHILE @workload <> N'' 
    BEGIN
	    -- Construct query
	    SET @SQLExecString = 
N'   INSERT #T 
	SELECT	''' + @workload + ''' AS workload_name 
		, ''' + @workload_description + ''' AS workload_description
		, s.instance_name
		, pc.performance_object_name
		, pc.performance_counter_name
		, pc.performance_instance_name
		, pc.collection_time
		, DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), pc.collection_time) AS elapsed_time
		, DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), pc.collection_time) - (DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), pc.collection_time) % ' + CAST(@interval AS nvarchar(20)) + ') AS adjusted_elapsed_time
		, pc.formatted_value 
    FROM ' + @database_name + '.snapshots.performance_counters AS pc
    INNER JOIN ' + @database_name + '.core.snapshots AS s 
		ON s.snapshot_id = pc.snapshot_id
    INNER JOIN MDW.perfmon_counter_config AS pcc
		ON pc.performance_object_name = pcc.performance_object_name AND pc.performance_counter_name = pcc.performance_counter_name AND COALESCE(pc.performance_instance_name, '''') = COALESCE(pcc.performance_instance_name, '''')
    WHERE s.instance_name = ''' + @instance_name + ''' 
    AND CAST(pc.collection_time AS datetimeoffset(7)) BETWEEN ''' + CAST(@start_time AS nvarchar(50)) + ''' AND ''' + CAST(@end_time AS nvarchar(50)) + '''
    AND pcc.config_YN = ''Y'' 
'
        IF @debug = 1 
	        PRINT @SQLExecString
        ELSE
	        EXEC(@SQLExecString)
    
        -- Reinitialize for next iteration        
        SET @workload_counter += 1
        SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
        SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
        SET @workload = LTRIM(@workload)
        -- Intialize the query parameters
	    SELECT @start_time = start_time
	    , @end_time = end_time
	    , @instance_name = instance_name_filter
	    , @database_name = mdw_database_name
	    , @interval = collection_interval_perfmon 
	    , @workload_description = workload_description 
	        FROM MDW.workload_runs 
	        WHERE workload_name = @workload
	    IF @start_time IS NULL OR @end_time IS NULL OR @instance_name IS NULL OR @database_name IS NULL
	        BREAK
    END    

	SELECT 
		workload_name
		, workload_description
		, instance_name
		, performance_object_name
		, performance_counter_name
		, performance_instance_name
		, collection_time
		, elapsed_time
		, adjusted_elapsed_time
		, formatted_value
	FROM #T
	ORDER BY workload_name, instance_name, performance_object_name, performance_counter_name, performance_instance_name, collection_time

	RETURN
END;

	
/*	Test:
-- 1. Without passing a performance object list
EXEC [MDW].[get_counter_data_points_by_config] 
	 @workload_list = 'Run1'
	 , @Debug = 0
	
EXEC [MDW].[get_counter_data_points_by_config] 
	 @workload_list = 'Run1, Run2, Run3'
	 , @Debug = 0
	 

-- 2. Now passing a performance object list
EXEC [MDW].[get_counter_data_points_by_config] 
	@workload_list = 'Run1, Run2, Run3'
    , @performance_object_list = 'Memory, LogicalDisk'
    
*/


GO


