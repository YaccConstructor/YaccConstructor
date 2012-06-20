USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[load_perfmon_counter_config_table]    Script Date: 02/23/2009 08:01:55 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[mdw].[load_perfmon_counter_config_table]') AND type in (N'P', N'PC'))
DROP PROCEDURE [mdw].[load_perfmon_counter_config_table]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [mdw].[load_perfmon_counter_config_table]    Script Date: 02/23/2009 08:01:55 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [mdw].[load_perfmon_counter_config_table] ( 
	@mdw_database_name nvarchar(128) = 'MDW' -- name of the MDW database
	, @Include_Counter varchar(1) = 'Y' -- sets all counters to Y
	, @instance_name_filter nvarchar(128) = '$(SSASServerComputer)' --name of the instance filter when querying the MDW database for valid counters
	)
	AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @SQLcmd nvarchar(4000);
    IF OBJECT_ID('mdw.perfmon_counter_config') IS NOT NULL DROP TABLE mdw.perfmon_counter_config
    SET @SQLcmd = '
			SELECT
				s.instance_name
				, pc.performance_object_name
				, pc.performance_counter_name
				, pc.performance_instance_name
				, ''' + @Include_Counter + ''' AS config_YN
			INTO MDW.perfmon_counter_config
			FROM ' + @mdw_database_name + '.snapshots.performance_counters AS pc
			INNER JOIN ' + @mdw_database_name + '.core.snapshots AS s ON (s.snapshot_id = pc.snapshot_id)'
	SET @SQLcmd = @SQLcmd + '
			WHERE s.instance_name = ''' + @instance_name_filter + ''''
	SET @SQLcmd = @SQLcmd + '
			GROUP BY s.instance_name, pc.performance_object_name, pc.performance_counter_name, pc.performance_instance_name
			ORDER BY 
				s.instance_name
				, pc.performance_object_name
				, pc.performance_counter_name
				, pc.performance_instance_name
'    
	EXEC(@SQLCmd);
	RETURN
END;
	
	
/*	
	
EXEC [MDW].[Load_Perfmon_Counter_Config_Table]
	@mdw_database_name = 'mdw'
	, @default_config = 'Y'
	, @instance_name_filter = 'ServerName'
go
select * from MDW.perfmon_counter_config

*/



GO


