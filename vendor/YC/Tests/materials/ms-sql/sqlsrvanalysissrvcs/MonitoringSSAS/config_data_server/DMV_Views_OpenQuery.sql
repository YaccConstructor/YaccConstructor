USE MDW
GO
--connections
IF OBJECT_ID('[ssas].[connections]') IS NOT NULL DROP VIEW [ssas].[connections]
GO
CREATE VIEW [ssas].[connections] AS
SELECT GETDATE() as [Date] 
      ,[CONNECTION_ID]
      ,cast ([CONNECTION_USER_NAME] as nvarchar(128))  as [CONNECTION_USER_NAME]
      ,cast ([CONNECTION_IMPERSONATED_USER_NAME] as nvarchar(128)) as [CONNECTION_IMPERSONATED_USER_NAME]
      ,cast ([CONNECTION_HOST_NAME] as nvarchar(128)) as [CONNECTION_HOST_NAME]
      ,cast ([CONNECTION_HOST_APPLICATION] as nvarchar(128)) as [CONNECTION_HOST_APPLICATION]
      ,[CONNECTION_START_TIME]
      ,[CONNECTION_ELAPSED_TIME_MS]
      ,[CONNECTION_LAST_COMMAND_START_TIME]
      ,[CONNECTION_LAST_COMMAND_END_TIME]
      ,[CONNECTION_LAST_COMMAND_ELAPSED_TIME_MS]
      ,[CONNECTION_IDLE_TIME_MS]
      ,[CONNECTION_BYTES_SENT]
      ,[CONNECTION_DATA_BYTES_SENT]
      ,[CONNECTION_BYTES_RECEIVED]
      ,[CONNECTION_DATA_BYTES_RECEIVED]
  FROM
OPENQUERY(SSAS2008, 'select  * from $system.discover_connections') 
GO
--select * from ssas.connections

--sessions
IF OBJECT_ID('[ssas].[sessions]') IS NOT NULL DROP VIEW [ssas].[sessions]
GO
CREATE VIEW [ssas].[sessions] AS
SELECT GETDATE() as [Date] 
      ,cast ([SESSION_ID] as nvarchar(128)) as [SESSION_ID]
      ,[SESSION_SPID]
      ,[SESSION_CONNECTION_ID]
      ,cast ([SESSION_USER_NAME] as nvarchar(128)) as [SESSION_USER_NAME]
      ,cast ([SESSION_CURRENT_DATABASE] as nvarchar(128)) as [SESSION_CURRENT_DATABASE]
      ,[SESSION_USED_MEMORY]
      ,cast ([SESSION_PROPERTIES] as nvarchar(128)) as [SESSION_PROPERTIES]
      ,[SESSION_START_TIME]
      ,[SESSION_ELAPSED_TIME_MS]
      ,[SESSION_LAST_COMMAND_START_TIME]
      ,[SESSION_LAST_COMMAND_END_TIME]
      ,[SESSION_LAST_COMMAND_ELAPSED_TIME_MS]
      ,[SESSION_IDLE_TIME_MS]
      ,[SESSION_CPU_TIME_MS]
      ,cast ([SESSION_LAST_COMMAND] as nvarchar(128)) as [SESSION_LAST_COMMAND]
      ,[SESSION_LAST_COMMAND_CPU_TIME_MS]
      ,[SESSION_STATUS]
      ,[SESSION_READS]
      ,[SESSION_WRITES]
      ,[SESSION_READ_KB]
      ,[SESSION_WRITE_KB]
      ,[SESSION_COMMAND_COUNT]
  FROM
OPENQUERY(SSAS2008, 'select  * from $system.discover_sessions') as s
GO
--select * from ssas.SSAS_Sessions

--commands
IF OBJECT_ID('[ssas].[commands]') IS NOT NULL DROP VIEW [ssas].[commands]
GO
CREATE VIEW [ssas].[commands] as
SELECT GETDATE() as [Date] 
      ,[SESSION_SPID]
      ,[SESSION_COMMAND_COUNT]
      ,[COMMAND_START_TIME]
      ,[COMMAND_ELAPSED_TIME_MS]
      ,[COMMAND_CPU_TIME_MS]
      ,[COMMAND_READS]
      ,[COMMAND_READ_KB]
      ,[COMMAND_WRITES]
      ,[COMMAND_WRITE_KB]
      ,CAST ([COMMAND_TEXT] as nvarchar(128)) as [COMMAND_TEXT]
      ,[COMMAND_END_TIME]
  FROM
OPENQUERY(SSAS2008, 'select  * from $system.discover_commands')
GO
--select * from ssas.commands

--command_objects
IF OBJECT_ID('[ssas].[command_objects]') IS NOT NULL DROP VIEW [ssas].[command_objects]
GO
CREATE VIEW [ssas].[command_objects] as
SELECT GETDATE() as 
	[Date]
	,[SESSION_SPID]
      ,CAST ([SESSION_ID] as varchar(128)) as [SESSION_ID]
      ,[SESSION_COMMAND_COUNT]
      ,CAST ([OBJECT_PARENT_PATH] as varchar(128)) as [OBJECT_PARENT_PATH]
      ,CAST ([OBJECT_ID] as varchar(128)) as [OBJECT_ID]
      ,[OBJECT_VERSION]
      ,[OBJECT_DATA_VERSION]
      ,[OBJECT_CPU_TIME_MS]
      ,[OBJECT_READS]
      ,[OBJECT_READ_KB]
      ,[OBJECT_WRITES]
      ,[OBJECT_WRITE_KB]
      ,[OBJECT_ROWS_SCANNED]
      ,[OBJECT_ROWS_RETURNED]

 FROM
OPENQUERY(SSAS2008, 'select  * from $system.discover_command_objects')
GO
--select * from ssas.command_objects

--object_activity
IF OBJECT_ID('[ssas].[object_activity]') IS NOT NULL DROP VIEW [ssas].[object_activity]
GO
CREATE VIEW [ssas].[object_activity] as
SELECT GETDATE() as [Date] 
	,CAST ([OBJECT_PARENT_PATH] as varchar(128)) as [OBJECT_PARENT_PATH]
	,CAST ([OBJECT_ID] as varchar(128)) as [OBJECT_ID]
	,CAST ([OBJECT_PARENT_PATH] as varchar(128))  + '.'+ CAST ([OBJECT_ID] as varchar(128))  as [ObjectPath]
	,OBJECT_CPU_TIME_MS  
	,OBJECT_READS   
	,OBJECT_READ_KB   
	,OBJECT_WRITES   
	,OBJECT_WRITE_KB   
	,OBJECT_AGGREGATION_HIT  
	,OBJECT_AGGREGATION_MISS  
	,OBJECT_HIT   
	,OBJECT_MISS   
	,OBJECT_VERSION   
	,OBJECT_DATA_VERSION  
	,OBJECT_ROWS_SCANNED  
	,OBJECT_ROWS_RETURNED  

 FROM
OPENQUERY(SSAS2008, 'select  * from $system.discover_object_activity')
 where 
OBJECT_CPU_TIME_MS <> 0 or 
OBJECT_READS  <> 0  or
OBJECT_READ_KB  <> 0  or
OBJECT_WRITES  <> 0 or 
OBJECT_WRITE_KB  <> 0 or
OBJECT_AGGREGATION_HIT <> 0 or
OBJECT_AGGREGATION_MISS <> 0 or
OBJECT_HIT   <> 0 or
OBJECT_MISS   <> 0

--select * from ssas.object_activity
GO

-- memory usage
IF OBJECT_ID('[ssas].[memory_usage]') IS NOT NULL DROP VIEW [ssas].[memory_usage]
GO
CREATE VIEW [ssas].[memory_usage] as
select GETDATE() as [Date] 
      ,CAST([OBJECT_PARENT_PATH] as nvarchar(128)) as [OBJECT_PARENT_PATH]
	  ,CAST ([OBJECT_ID] as varchar(128)) as [OBJECT_ID]
	  ,CAST ([OBJECT_PARENT_PATH] as varchar(128))  + '.'+ CAST ([OBJECT_ID] as varchar(128))  as [ObjectPath]
      ,[OBJECT_MEMORY_SHRINKABLE]
      ,[OBJECT_MEMORY_NONSHRINKABLE]
      ,[OBJECT_VERSION]
      ,[OBJECT_DATA_VERSION]
      ,[OBJECT_TYPE_ID]
      ,[OBJECT_TIME_CREATED]
 
from OPENQUERY(SSAS2008, 'select  * from $system.discover_object_memory_usage')
where 
OBJECT_MEMORY_NONSHRINKABLE <> 0 or 
OBJECT_MEMORY_SHRINKABLE  <> 0

--select * from mdw.ssas.memory_usage
GO