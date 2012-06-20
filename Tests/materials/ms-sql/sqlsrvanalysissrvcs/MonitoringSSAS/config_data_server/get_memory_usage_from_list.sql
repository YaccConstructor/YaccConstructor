USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_memory_usage_from_list]    Script Date: 12/23/2008 17:09:14 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_memory_usage_from_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_memory_usage_from_list]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_memory_usage_from_list]    Script Date: 12/23/2008 17:09:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [ssas].[get_memory_usage_from_list]( 
	 @workload_list nvarchar(max)
	, @object_path_list nvarchar(max) = ''
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
	        RAISERROR('get_memory_usage_from_list: You must enter a delimited set of test runs', 16, 0)
	        RETURN
	    END
	-- =================================================================================
	-- Prepare temporary table
	-- =================================================================================
	IF OBJECT_ID('tempdb.dbo.#T') IS NOT NULL DROP TABLE #T
	CREATE TABLE #T (
		workload nvarchar(128),
		[Date] [datetime] NULL,
		[OBJECT_PARENT_PATH] [nvarchar](4000) NULL,
		[OBJECT_MEMORY_SHRINKABLE] [bigint] NULL,
		[OBJECT_MEMORY_NONSHRINKABLE] [bigint] NULL,
		[elapsed_time] int null,
		[adjusted_elapsed_time] int null,
		[OBJECT_ID] [nvarchar](255) NULL,
		[ObjectPath] [nvarchar](4000) NULL,
		)
	-- ===============================================
	-- Get start and end time from workload table
	-- ===============================================
    DECLARE @start_time datetime
		, @end_time datetime
	--	, @start_timedt datetime
	--	, @end_timedt datetime
		, @database_name nvarchar(128)
		, @SqlExecString varchar(max)
		, @workload_counter int
		, @workload varchar(128)
		, @interval int
	-- =================================================================================
	-- Process test runs
	-- =================================================================================
	SET @workload_counter = 1
    SET @workload = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, 1, CHARINDEX(N',', @workload_list, 0)-1) ELSE @workload_list END -- Get first delimited token
    SET @workload_list = CASE WHEN CHARINDEX(N',', @workload_list) > 0 THEN SUBSTRING(@workload_list, CHARINDEX(N',', @workload_list, 0)+1, LEN(@workload_list)) ELSE N'' END -- Remove token from list
    SET @workload = LTRIM(@workload)
    -- IF @debug = 1 PRINT @workload
    -- Intialize the query parameters
SELECT @start_time = start_time_local, @end_time = end_time_local, @database_name = mdw_database_name, @interval = collection_interval_dmv
		    FROM mdw.workload_runs 
	    WHERE workload_name = @workload
--	SET @start_time = SWITCHOFFSET (@start_time, '-08:00')  -- need to genericize this 
--	SET @end_time = SWITCHOFFSET (@end_time, '-08:00') -- need to genericize this
--	SELECT @start_timedt = CAST(@start_time AS datetime),@end_timedt = CAST(@end_time AS datetime)
	-- Convert comma-separated list to include quotes for IN clause, if it has any data
	SET @object_path_list = RTRIM(LTRIM(@object_path_list))
	IF DATALENGTH(@object_path_list) > 0
		BEGIN
			SET @object_path_list = '''' + REPLACE(@object_path_list, ',', ''',''') + ''''
			IF @Debug = 1 PRINT @object_path_list
		END
	-- Processing loop
    WHILE @workload <> '' 
	    BEGIN
			SET @SqlExecString = '
INSERT #T
SELECT 
	''' + @workload + ''',
	[Date] ,
	[OBJECT_PARENT_PATH],
	[OBJECT_MEMORY_SHRINKABLE],
	[OBJECT_MEMORY_NONSHRINKABLE],
	DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), date) AS elapsed_time,
	DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), date) - (DATEDIFF(ss, CAST(''' + CAST(@start_time AS nvarchar(80)) + ''' AS datetimeoffset(7)), date) % ' + CAST(@interval AS nvarchar(20)) + ') AS adjusted_elapsed_time,
	[OBJECT_ID],
	[ObjectPath]
FROM ' + @database_name + '.custom_snapshots.ssas_memory_usage
WHERE [date] BETWEEN CAST(''' + CAST(@start_time AS nvarchar(50)) + ''' AS datetime) AND CAST(''' + CAST(@end_time AS nvarchar(50)) + ''' AS datetime)'
	SET @SqlExecString = @SqlExecString + 
		CASE WHEN DATALENGTH(@object_path_list) > 0 THEN '
	AND [OBJECTPATH] IN (' + @object_path_list + ')'
		ELSE '' END
	SET @SqlExecString = @SqlExecString + '
	ORDER BY [OBJECTPATH]'
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
 SELECT @start_time = start_time_local, @end_time = end_time_local, @database_name = mdw_database_name, @interval = collection_interval_dmv
	    FROM mdw.workload_runs 
       WHERE workload_name = @workload
  	--SET @start_time = SWITCHOFFSET (@start_time, '-08:00')  -- need to genericize this 
	--SET @end_time = SWITCHOFFSET (@end_time, '-08:00') -- need to genericize this
 --	SELECT @start_timedt = CAST(@start_time AS datetime),@end_timedt = CAST(@end_time AS datetime)
 IF @start_time IS NULL OR @end_time IS NULL OR @database_name IS NULL
        BREAK
    END    
	SELECT * FROM #T
		ORDER BY adjusted_elapsed_time--workload, date, OBJECT_ID
	RETURN
END

/* Test
SELECT TOP 100 * FROM custom_snapshots.ssas_memory_usage
EXEC ssas.get_memory_usage_from_list 'test1', 'CARLR_XPSM1210.Databases.Adventure Works DW 2008.Cubes.Adventure Works.Measure Groups.Fact Reseller Sales.Partitions.Reseller_Sales_2003', 0
EXEC ssas.get_memory_usage_from_list 'Run1', 'Server.Databases.Database_Name.Cubes.CubeName.Measure Groups.MeasureGroup_Name.Partitions.Partition_Name', 0
*/

GO


