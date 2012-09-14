USE [msdb]
GO

/****** Object:  Collection Set [SSAS Monitoring-Core]    Script Date: 11/04/2008 07:58:16 ******/
IF  EXISTS (SELECT name FROM msdb.dbo.[syscollector_collection_sets] WHERE name = N'SSAS_Monitoring_Verbose')

EXEC [msdb].[dbo].[sp_syscollector_delete_collection_set] @name='SSAS_Monitoring_Verbose'

GO
Begin Transaction
Begin Try
Declare @collection_set_id_1 int
Declare @collection_set_uid_2 uniqueidentifier
EXEC [msdb].[dbo].[sp_syscollector_create_collection_set] @name=N'SSAS_TSQL_DMVs', @collection_mode=0, @description=N'Collects top-level performance indicators for the computer and the Database Engine. Enables analysis of resource use, resource bottlenecks, and Database Engine activity.', @logging_level=0, @days_until_expiration=14, @schedule_name=N'CollectorSchedule_Every_15min', @collection_set_id=@collection_set_id_1 OUTPUT, @collection_set_uid=@collection_set_uid_2 OUTPUT
Select @collection_set_id_1, @collection_set_uid_2

Declare @collector_type_uid_3 uniqueidentifier
Select @collector_type_uid_3 = collector_type_uid From [msdb].[dbo].[syscollector_collector_types] Where name = N'Generic T-SQL Query Collector Type';
Declare @collection_item_id_4 int
EXEC [msdb].[dbo].[sp_syscollector_create_collection_item] 
	@name=N'SSAS_TSQL_DMVs'
	, @parameters=N'<ns:TSQLQueryCollector xmlns:ns="DataCollectorType">
	<Query>
	<Value>
	select * from mdw.ssas.connections
	</Value><OutputTable>ssas_connections</OutputTable>
	</Query>
	
	<Query>
	<Value>
	select * from mdw.ssas.sessions
	</Value><OutputTable>ssas_sessions</OutputTable>
	</Query>
	
	<Query>
	<Value>
	select * from mdw.ssas.commands
	</Value><OutputTable>ssas_commands</OutputTable>
	</Query>
	
	<Query>
	<Value>
	select * from mdw.ssas.command_objects
	</Value><OutputTable>ssas_command_objects</OutputTable>
	</Query>
	
	<Query>
	<Value>
	select * from mdw.ssas.object_activity
	</Value><OutputTable>ssas_object_activity</OutputTable>
	</Query>
	
	<Query>
	<Value>
	select * from mdw.ssas.memory_usage
	</Value><OutputTable>ssas_memory_usage</OutputTable>
	</Query>

	</ns:TSQLQueryCollector>'
	, @collection_item_id=@collection_item_id_4 OUTPUT
	, @frequency=60
	, @collection_set_id=@collection_set_id_1
	, @collector_type_uid=@collector_type_uid_3
Select @collection_item_id_4

Commit Transaction;
End Try
Begin Catch
Rollback Transaction;
DECLARE @ErrorMessage NVARCHAR(4000);
DECLARE @ErrorSeverity INT;
DECLARE @ErrorState INT;
DECLARE @ErrorNumber INT;
DECLARE @ErrorLine INT;
DECLARE @ErrorProcedure NVARCHAR(200);
SELECT @ErrorLine = ERROR_LINE(),
       @ErrorSeverity = ERROR_SEVERITY(),
       @ErrorState = ERROR_STATE(),
       @ErrorNumber = ERROR_NUMBER(),
       @ErrorMessage = ERROR_MESSAGE(),
       @ErrorProcedure = ISNULL(ERROR_PROCEDURE(), '-');
RAISERROR (14684, @ErrorSeverity, 1 , @ErrorNumber, @ErrorSeverity, @ErrorState, @ErrorProcedure, @ErrorLine, @ErrorMessage);

End Catch;

GO


