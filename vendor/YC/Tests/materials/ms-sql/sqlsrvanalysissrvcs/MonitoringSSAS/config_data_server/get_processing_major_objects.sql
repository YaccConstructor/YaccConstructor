USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_major_objects]    Script Date: 01/05/2009 23:17:59 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_processing_major_objects]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_processing_major_objects]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_processing_major_objects]    Script Date: 01/05/2009 23:17:59 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO



CREATE PROC [ssas].[get_processing_major_objects]
     @workload_list nvarchar(max)
 --    , @TraceDatabaseName nvarchar(128)
 --    , @TraceInstanceName nvarchar(128)
     , @Debug bit = NULL
AS
BEGIN      
    SET NOCOUNT ON
	-- =================================================================================
	-- Test the parameters
	-- =================================================================================
	SET @workload_list = LTRIM(RTRIM(@workload_list))
	IF LEN(@workload_list) = 0
	    BEGIN
	        RAISERROR('GetProcessingMajorObjects: You must enter a delimited set of test runs', 16, 0)
	        RETURN
	    END
	-- =================================================================================
	-- Prepare tables and variables
	-- =================================================================================
	IF OBJECT_ID('tempdb.dbo.#T') IS NOT NULL DROP TABLE #T
	CREATE TABLE #T (
	[workload_name] nvarchar(128)
	,[workload_list] nvarchar(max)
	,[workload_description] nvarchar(128)
	,[mdw_database_name] nvarchar(128)
	,[RowNumber] bigint
	,[ConnectionID] int 
	,[EventClass] nvarchar(128)
	,[StartTime] datetime 
	,[EventSubclass] nvarchar(128)
	,[Duration] bigint
	,[CPUTime] bigint
	,[JobID] int 
	,[NTCanonicalUserName] nvarchar(128) 
	,[ObjectID] nvarchar(128)
	,[ObjectName] nvarchar(128)
	,[ObjectPath] nvarchar(2000)
	,[ObjectType] int
	,[SPID] int 
	,[SessionID] nvarchar(128)
	,[TextData] ntext
	,[EndTime] datetime
	,[Error] int 
	,[BinaryData] image
	,[DimensionName] nvarchar(128) 
	,[CubeName] nvarchar(128) 
	,[MeasureGroupName] nvarchar(128) 
	,[DatabaseName] nvarchar(128)
	,[InstanceName] nvarchar(128)
		)
	-- Declare variables
    DECLARE @SQLExecString nvarchar(4000)
		, @workload nvarchar(128)
		, @workload_counter int
    DECLARE @start_time datetime
		, @end_time datetime
		, @instance_name_filter nvarchar(128)
		, @mdw_database_name nvarchar(128)
		, @interval int
		, @database_name_filter nvarchar(128)
		, @workload_description nvarchar(128)
		, @workload_list_orig nvarchar(128)


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
	    FROM MDW.workload_runs 
	    WHERE workload_name = @workload
	-- Processing loop
    WHILE @workload <> N'' 
    BEGIN
	    -- Construct query
	    SET @SQLExecString = 

N'   INSERT #T
	SELECT     
		''' + @workload + ''' AS workload_name
		,''' + @workload_list_orig + ''' AS workload_list
		,''' + @workload_description + ''' AS workload_description
		,''' + @mdw_database_name + ''' AS mdw_database_name
		,g.RowNumber
		,g.ConnectionID
		, g.EventClass
		, g.StartTime
		, g.EventSubClass
		, g.Duration
		, g.CPUTime
		, g.JobID
		, g.NTCanonicalUserName
		, g.ObjectID
		, g.ObjectName
		, g.ObjectPath
		, g.ObjectType
		, g.SPID
		, g.SessionID
		, g.TextData
		, g.EndTime
		, g.Error
		, g.BinaryData
		, g.DimensionName
		, g.CubeName
		, g.MeasureGroupName
		,''' + @database_name_filter + ''' AS DatabaseName
		,''' + @instance_name_filter + ''' AS InstanceName
	FROM         ' + @mdw_database_name + '.[SSAS].[SSASProcessingArchive] AS g 
	WHERE StartTime BETWEEN COALESCE(' + '''' + CONVERT(varchar(30),@start_time, 109) + ''', ''1/1/1900'') 
	AND COALESCE(' + '''' + Convert(varchar(30), @end_time,109) + ''', ''12/31/9999'')
	AND DatabaseName = ''' + @database_name_filter + '''
	AND ServerName = ''' + @instance_name_filter + '''
	AND objecttype IN (100006, 100016)
	AND eventclass = ''Progress Report End''
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
	    SELECT @start_time = start_time, @end_time = end_time, @instance_name_filter = instance_name_filter, @mdw_database_name = mdw_database_name, @interval = collection_interval_perfmon  
	        FROM MDW.workload_runs 
	        WHERE workload_name = @workload
	    IF @start_time IS NULL OR @end_time IS NULL OR @instance_name_filter IS NULL OR @mdw_database_name IS NULL
	        BREAK

END
	SELECT
		workload_name
		, workload_description
		, mdw_database_name
		, RowNumber
		, ConnectionID
		, EventClass
		, StartTime
		, EventSubClass
		, Duration
		, CPUTime
		, JobID
		, NTCanonicalUserName
		, ObjectID
		, ObjectName
		, ObjectPath
		, ObjectType
		, SPID
		, SessionID
		, TextData
		, EndTime
		, Error
		, BinaryData
		, DimensionName
		, CubeName
		, MeasureGroupName
		, DatabaseName
		, InstanceName
		FROM #T
		Order By workload_name
		
RETURN
END;


/* Test
EXEC ssas.GetProcessingMajorObjects2
    @workload_list = 'Run1, Run2' 
    , @TraceDatabaseName = 'Adventure Works DW 2008'
    , @TraceInstanceName = 'sql2008'
    , @debug = 0
*/



GO

