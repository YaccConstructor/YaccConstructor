USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_session_dmv_info_by_time]    Script Date: 12/23/2008 17:16:34 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_session_dmv_info_by_time]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_session_dmv_info_by_time]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_session_dmv_info_by_time]    Script Date: 12/23/2008 17:16:34 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [ssas].[get_session_dmv_info_by_time]( 
	 @workload_list nvarchar(max)
	, @debug bit = 0
	)
	AS
BEGIN
    SET NOCOUNT ON
    DECLARE @SQLExecString varchar(8000)
		, @workload nvarchar(128)
		, @workload_counter int
		, @workload_name nvarchar(128)
		, @workload_description nvarchar(128)
		, @start_time datetimeoffset(7)
		, @end_time datetimeoffset(7)
		, @instance_name nvarchar(128)
		, @database_name nvarchar(128)
		, @interval int

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
	-- Prepare temporary table
	-- =================================================================================
	IF OBJECT_ID('tempdb.dbo.#T') IS NOT NULL DROP TABLE #T
	CREATE TABLE #T (
		[workload] [nvarchar](128) NOT NULL,
		[workload_name] nvarchar(128) NULL,
		[workload_description] nvarchar(128) NULL,
		[start_time] [datetimeoffset](7) NULL,
		[end_time] [datetimeoffset](7) NULL,
		[Connection_Date] [datetime] NULL,
		[CONNECTION_ID] [int] NULL,
		[CONNECTION_USER_NAME] [ntext] NULL,
		[CONNECTION_IMPERSONATED_USER_NAME] [ntext] NULL,
		[CONNECTION_HOST_NAME] [ntext] NULL,
		[CONNECTION_HOST_APPLICATION] [ntext] NULL,
		[CONNECTION_START_TIME] [datetime] NULL,
		[CONNECTION_ELAPSED_TIME_MS] [bigint] NULL,
		[CONNECTION_LAST_COMMAND_START_TIME] [datetime] NULL,
		[CONNECTION_LAST_COMMAND_END_TIME] [datetime] NULL,
		[CONNECTION_LAST_COMMAND_ELAPSED_TIME_MS] [bigint] NULL,
		[CONNECTION_IDLE_TIME_MS] [bigint] NULL,
		[CONNECTION_BYTES_SENT] [bigint] NULL,
		[CONNECTION_DATA_BYTES_SENT] [bigint] NULL,
		[CONNECTION_BYTES_RECEIVED] [bigint] NULL,
		[CONNECTION_DATA_BYTES_RECEIVED] [bigint] NULL,
		[Connection_Instance_Name] [nvarchar](255) NULL,
		[Session_Date] [datetime] NULL,
		[SESSION_ID] [nvarchar](255) NULL,
		[SESSION_SPID] [int] NULL,
		[SESSION_CONNECTION_ID] [int] NULL,
		[SESSION_USER_NAME] [ntext] NULL,
		[SESSION_CURRENT_DATABASE] [ntext] NULL,
		[SESSION_USED_MEMORY] [int] NULL,
		[SESSION_PROPERTIES] [ntext] NULL,
		[SESSION_START_TIME] [datetime] NULL,
		[SESSION_ELAPSED_TIME_MS] [numeric](20, 0) NULL,
		[SESSION_LAST_COMMAND_START_TIME] [datetime] NULL,
		[SESSION_LAST_COMMAND_END_TIME] [datetime] NULL,
		[SESSION_LAST_COMMAND_ELAPSED_TIME_MS] [numeric](20, 0) NULL,
		[SESSION_IDLE_TIME_MS] [numeric](20, 0) NULL,
		[SESSION_CPU_TIME_MS] [numeric](20, 0) NULL,
		[SESSION_LAST_COMMAND] [ntext] NULL,
		[SESSION_LAST_COMMAND_CPU_TIME_MS] [numeric](20, 0) NULL,
		[SESSION_STATUS] [int] NULL,
		[SESSION_READS] [numeric](20, 0) NULL,
		[SESSION_WRITES] [numeric](20, 0) NULL,
		[SESSION_READ_KB] [numeric](20, 0) NULL,
		[SESSION_WRITE_KB] [numeric](20, 0) NULL,
		[SESSION_COMMAND_COUNT] [int] NULL,
		[Session_Instance_Name] [nvarchar](255) NULL,
		[COMMANDS_DATE] [datetime] NULL,
		[COMMAND_SESSION_SPID] [int] NULL,
		[COMMAND_SESSION_COMMAND_COUNT] [int] NULL,
		[COMMAND_START_TIME] [datetime] NULL,
		[COMMAND_ELAPSED_TIME_MS] [bigint] NULL,
		[COMMAND_CPU_TIME_MS] [bigint] NULL,
		[COMMAND_READS] [bigint] NULL,
		[COMMAND_READ_KB] [bigint] NULL,
		[COMMAND_WRITES] [bigint] NULL,
		[COMMAND_WRITE_KB] [bigint] NULL,
		[COMMAND_TEXT] [ntext] NULL,
		[COMMAND_END_TIME] [datetime] NULL,
		[COMMAND_Instance_Name] [nvarchar](255) NULL
		
		
		)
	-- =================================================================================
	-- Process test runs
	-- =================================================================================
	SET @workload_counter = 1
    SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
    SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
    SET @workload = LTRIM(@workload)
    -- IF @debug = 1 PRINT @workload
    -- Intialize the query parameters
	SELECT @start_time = start_time
	, @end_time = end_time
	, @instance_name = instance_name_filter
	, @database_name = mdw_database_name 
	, @workload_name = workload_name
	, @workload_description = workload_description 
	    FROM mdw.workload_runs 
	    WHERE workload_name = @workload
	-- Processing loop
    WHILE @workload <> '' 
    BEGIN
	    -- Construct query
	    SET @SQLExecString = '
WITH 
	ssas_connections AS
	  (
	  SELECT 
			CONNECTION_ID 
			, CONNECTION_ELAPSED_TIME_MS
			, CONNECTION_LAST_COMMAND_ELAPSED_TIME_MS
			, CONNECTION_BYTES_SENT
			, CONNECTION_DATA_BYTES_SENT
			, CONNECTION_BYTES_RECEIVED
			, CONNECTION_DATA_BYTES_RECEIVED
			, INSTANCE_NAME
	  FROM ' + @database_name + '.custom_snapshots.ssas_connections 
	  WHERE CONNECTION_LAST_COMMAND_START_TIME BETWEEN CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AND CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetimeoffset(7))
	  ),
	ssas_sessions AS
	  (
			SELECT 
				   SESSION_ID
				  , SESSION_SPID
				  , SESSION_CONNECTION_ID
				  , SESSION_USER_NAME
				  , SESSION_CURRENT_DATABASE
				  , SESSION_USED_MEMORY
				  , SESSION_START_TIME
				  , SESSION_ELAPSED_TIME_MS
				  , SESSION_LAST_COMMAND
				  , SESSION_LAST_COMMAND_START_TIME
				  , SESSION_LAST_COMMAND_END_TIME
				  , SESSION_LAST_COMMAND_ELAPSED_TIME_MS
				  , SESSION_IDLE_TIME_MS
				  , SESSION_CPU_TIME_MS
				  , SESSION_READS
				  , SESSION_WRITES
				  , SESSION_READ_KB
				  , SESSION_WRITE_KB
				  , SESSION_COMMAND_COUNT
	  FROM ' + @database_name + '.custom_snapshots.ssas_sessions AS ssas_sessions
	  WHERE SESSION_LAST_COMMAND NOT LIKE ''<Subscribe%''
	  AND SESSION_LAST_COMMAND NOT LIKE ''%$system.discover_sessions%''
	  AND SESSION_LAST_COMMAND_START_TIME BETWEEN CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AND CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetimeoffset(7))
	  ),
	ssas_commands AS
	  (
			SELECT
			SESSION_SPID AS Commands_Session_SPID
			, SESSION_COMMAND_COUNT AS Commands_Session_Command_Count
			, COMMAND_START_TIME
			, COMMAND_ELAPSED_TIME_MS
			, COMMAND_CPU_TIME_MS
			, COMMAND_READS
			, COMMAND_READ_KB
			, COMMAND_WRITES
			, COMMAND_WRITE_KB
			, COMMAND_TEXT
			, COMMAND_END_TIME
	  FROM ' + @database_name + '.custom_snapshots.ssas_commands
	  WHERE COMMAND_TEXT NOT LIKE ''<Subscribe%''
	  AND COMMAND_TEXT NOT LIKE ''%$system.discover_sessions%''
	  AND command_start_time BETWEEN CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AND CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetimeoffset(7))
	  
	  )
INSERT #T 
SELECT 
	  ''' + @workload + ''' AS workload
	  ,''' + @workload_name + ''' AS workload_name
	  ,''' + @workload_description + ''' AS workload_description
	  , CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AS start_time 
	  , CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AS end_time
	  , ssas_connections.*
	  , ssas_sessions.*
	  , ssas_commands.* 
FROM ' + @database_name + '.custom_snapshots.ssas_connections
JOIN ' + @database_name + '.custom_snapshots.ssas_sessions
	ON ssas_connections.CONNECTION_ID = ssas_sessions.SESSION_CONNECTION_ID
JOIN ' + @database_name + '.custom_snapshots.ssas_commands
	ON ssas_commands.SESSION_SPID = ssas_sessions.SESSION_SPID
	AND ssas_sessions.SESSION_LAST_COMMAND_START_TIME = ssas_commands.COMMAND_START_TIME
		  WHERE CONNECTION_LAST_COMMAND_START_TIME BETWEEN CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetimeoffset(7)) AND CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetimeoffset(7))
AND SESSION_LAST_COMMAND NOT LIKE ''<Subscribe%''
	  AND SESSION_LAST_COMMAND NOT LIKE ''%$system.discover_sessions%''
	  AND COMMAND_TEXT NOT LIKE ''<Subscribe%''
	  AND COMMAND_TEXT NOT LIKE ''%$system.discover_sessions%''

;'
		IF @debug = 1 
			PRINT @SqlExecString
		ELSE
			EXEC (@SqlExecString)
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
	    , @workload_name = workload_name
		, @workload_description = workload_description  
	        FROM mdw.workload_runs 
	        WHERE workload_name = @workload
	    IF @start_time IS NULL OR @end_time IS NULL OR @instance_name IS NULL OR @database_name IS NULL
	        BREAK
    END    
	SELECT * FROM #T WHERE SESSION_READS >0
		ORDER BY SESSION_Reads
    RETURN
END

/* Test

EXEC ssas.get_session_dmv_info 'Run1, Run2',0
*/






GO


