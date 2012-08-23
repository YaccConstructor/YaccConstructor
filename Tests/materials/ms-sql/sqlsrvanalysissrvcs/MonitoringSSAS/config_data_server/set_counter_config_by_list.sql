USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[set_counter_config_by_list]    Script Date: 12/23/2008 17:07:54 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[mdw].[set_counter_config_by_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [mdw].[set_counter_config_by_list]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[set_counter_config_by_list]    Script Date: 12/23/2008 17:07:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [mdw].[set_counter_config_by_list] ( 
	  @performance_object_list nvarchar(max) = NULL
	, @debug bit = 0
	)
	AS
BEGIN
    SET NOCOUNT ON
	-- =================================================================================
	-- Test the parameters
	-- =================================================================================
	SET @performance_object_list = LTRIM(RTRIM(@performance_object_list))
	IF LEN(@performance_object_list) = 0
	    BEGIN
	        RAISERROR('set_counter_config_by_list: The performance object list must not be empty', 16, 0)
	        RETURN
	    END
	UPDATE MDW.perfmon_counter_config SET config_YN = 'N'
		-- =================================================================================
	-- Prepare tables and variables
	-- =================================================================================
	-- Declare variables
    DECLARE @SQLExecString nvarchar(max), @performance_object nvarchar(2048), @performance_object_counter int
   	-- =================================================================================
	-- Parse performance object list and build WHERE clause for UPDATE statement
	-- =================================================================================
	SET @performance_object_counter = 1
    SET @performance_object = CASE WHEN CHARINDEX(N',', @performance_object_list) > 0 THEN SUBSTRING(@performance_object_list, 1, CHARINDEX(N',', @performance_object_list, 0)-1) ELSE @performance_object_list END -- Get first delimited token
    SET @performance_object_list = CASE WHEN CHARINDEX(N',', @performance_object_list) > 0 THEN SUBSTRING(@performance_object_list, CHARINDEX(N',', @performance_object_list, 0)+1, LEN(@performance_object_list)) ELSE N'' END -- Remove token from list
    SET @performance_object = LTRIM(@performance_object)
    IF @debug = 1 SELECT @performance_object
    	-- Processing loop
    WHILE @performance_object <> N'' 
    BEGIN
	    -- Construct query
	    IF @performance_object_counter = 1
	        SET @SQLExecString = N'
UPDATE MDW.perfmon_counter_config
    SET config_YN = ''Y'' 
    '
	    SET @SQLExecString = @SQLExecString + CASE WHEN @performance_object_counter = 1 THEN N'WHERE ' ELSE N' OR ' END
        SET @SQLExecString = @SQLExecString + ' performance_object_name = ''' + @performance_object + ''''
        -- Reinitialize for next iteration        
	    SET @performance_object_counter += 1
        SET @performance_object = CASE WHEN CHARINDEX(N',', @performance_object_list) > 0 THEN SUBSTRING(@performance_object_list, 1, CHARINDEX(N',', @performance_object_list, 0)-1) ELSE @performance_object_list END -- Get first delimited token
        SET @performance_object_list = CASE WHEN CHARINDEX(N',', @performance_object_list) > 0 THEN SUBSTRING(@performance_object_list, CHARINDEX(N',', @performance_object_list, 0)+1, LEN(@performance_object_list)) ELSE N'' END -- Remove token from list
        SET @performance_object = LTRIM(@performance_object)
    END

    IF @debug = 1 
        PRINT @SQLExecString
    ELSE
        EXEC(@SQLExecString)


    RETURN
END

/* Test: 
SELECT DISTINCT performance_object_name FROM MDW.perfmon_counter_config

SELECT * FROM MDW.perfmon_counter_config WHERE config_YN = 'Y'

EXEC MDW.set_counter_config_by_list 
    @performance_object_list = 'Memory'
SELECT * FROM MDW.perfmon_counter_config WHERE config_YN = 'Y'
    
EXEC MDW.set_counter_config_by_list 
    @performance_object_list = 'LogicalDisk, Memory'
SELECT * FROM MDW.perfmon_counter_config WHERE config_YN = 'Y'

EXEC MDW.set_counter_config_by_list 
    @performance_object_list = 'Process, Server Work Queues, System'
SELECT * FROM MDW.perfmon_counter_config WHERE config_YN = 'Y'

*/
GO


