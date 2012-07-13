USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_command_objects_by_id]    Script Date: 12/23/2008 17:08:19 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_command_objects_by_id]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_command_objects_by_id]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_command_objects_by_id]    Script Date: 12/23/2008 17:08:19 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [ssas].[get_command_objects_by_id]( 
	 @workload nvarchar(128)
	, @session_id nvarchar(128)
	, @session_spid int
	, @debug bit = 0
--	, @query_string nvarchar(4000)
	) 
	AS
BEGIN
    SET NOCOUNT ON
	-- ===============================================
	-- Get start and end time from workload table
	-- ===============================================
    DECLARE @start_time datetimeoffset(7)
		, @end_time datetimeoffset(7)
		, @database_name nvarchar(128)
		, @SqlExecString nvarchar(max)
		, @workload_name nvarchar(128)
		, @workload_description nvarchar(128)
    	SELECT @start_time = start_time, @end_time = end_time, @database_name = mdw_database_name, @workload_name=workload_name, @workload_description=workload_description
	    FROM mdw.workload_runs 
	    WHERE workload_name = @workload
	SET @SqlExecString = '
	SELECT 
		SESSION_SPID
		, MAX(SESSION_COMMAND_COUNT) AS SESSION_COMMAND_COUNT
		, MAX(OBJECT_CPU_TIME_MS) AS OBJECT_CPU_TIME_MS
		, MAX(OBJECT_READS) AS OBJECT_READS
		, MAX(OBJECT_READ_KB) AS OBJECT_READ_KB
		, MAX(OBJECT_WRITES) AS OBJECT_WRITES
		, MAX(OBJECT_WRITE_KB) AS OBJECT_WRITE_KB
		, MAX(OBJECT_ROWS_SCANNED) AS OBJECT_ROWS_SCANNED
		, MAX(OBJECT_ROWS_RETURNED) AS OBJECT_ROWS_RETURNED
		, SESSION_ID
		, OBJECT_PARENT_PATH
		, OBJECTPATH
		, OBJECT_ID
		,''' + @workload_name + ''' as workload_name
		,''' + @workload_description + ''' as workload_description
	FROM ' + @database_name + '.custom_snapshots.ssas_command_objects 
	WHERE Object_CPU_Time_MS >0 and session_spid = ' + CAST(@session_spid AS varchar(50)) + '
	AND session_id = ''' + @session_id + '''group by OBJECT_PARENT_PATH, OBJECT_ID, OBJECTPATH, SESSION_SPID, OBJECT_VERSION, OBJECT_DATA_VERSION, SESSION_ID'
	IF @debug = 1 PRINT @SqlExecString
ELSE
	EXEC(@SqlExecString)
	RETURN
END

/* Test:
select * from custom_snapshots.ssas_command_objects 
EXEC ssas.get_command_objects_by_id 'Run1a', '0C2303E3-4715-45E0-B6EB-65D7D2D35F00', 2894, 0
*/

GO


