USE [msdb]
GO

/****** Object:  Job [SSAS_Trace]    Script Date: 11/04/2008 08:02:19 ******/
IF  EXISTS (SELECT job_id FROM msdb.dbo.sysjobs_view WHERE name = N'SSAS_Trace')
EXEC msdb.dbo.sp_delete_job @job_name=N'SSAS_Trace', @delete_unused_schedule=1
GO

USE [msdb]
GO

/****** Object:  Job [SSAS_Trace]    Script Date: 11/04/2008 13:54:04 ******/
BEGIN TRANSACTION
DECLARE @ReturnCode INT
SELECT @ReturnCode = 0
/****** Object:  JobCategory [Data Collector]    Script Date: 11/04/2008 13:54:04 ******/
IF NOT EXISTS (SELECT name FROM msdb.dbo.syscategories WHERE name=N'Data Collector' AND category_class=1)
BEGIN
EXEC @ReturnCode = msdb.dbo.sp_add_category @class=N'JOB', @type=N'LOCAL', @name=N'Data Collector'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback

END

DECLARE @jobId BINARY(16)
EXEC @ReturnCode =  msdb.dbo.sp_add_job @job_name=N'SSAS_Trace', 
		@enabled=0, 
		@notify_level_eventlog=0, 
		@notify_level_email=0, 
		@notify_level_netsend=0, 
		@notify_level_page=0, 
		@delete_level=0, 
		@description=N'No description available.', 
		@category_name=N'Data Collector', 
		@owner_login_name=N'$(JobOwner)', @job_id = @jobId OUTPUT
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
/****** Object:  Step [Upload_Trace_Data]    Script Date: 11/04/2008 13:54:04 ******/
EXEC @ReturnCode = msdb.dbo.sp_add_jobstep @job_id=@jobId, @step_name=N'Upload_Trace_Data', 
		@step_id=1, 
		@cmdexec_success_code=0, 
		@on_success_action=1, 
		@on_success_step_id=0, 
		@on_fail_action=2, 
		@on_fail_step_id=0, 
		@retry_attempts=0, 
		@retry_interval=0, 
		@os_run_priority=0, @subsystem=N'CmdExec', 
		@command=N'"$(DTEXECx86_PATH)DTExec.exe" /FILE "$(Load_SSAS_Trace_Data_Path)load_SSAS_trace_data.dtsx" /SET \Package.Variables[sTraceFileName].Value;$(Trace_File_Path) /CONNECTION Mdw;"\"Data Source=$(MDWDataCollectionServerInstance);Initial Catalog=mdw;Provider=SQLNCLI10.1;Integrated Security=SSPI;Application Name=SSIS-Load SSAS Trace Data-{BCD3C1EF-5D23-4F9A-9266-F70757FEF20E}Mdw;\""  /CHECKPOINTING OFF  /REPORTING EWCDI', 
		@flags=0
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_update_job @job_id = @jobId, @start_step_id = 1
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_add_jobschedule @job_id=@jobId, @name=N'Every $(LoadTraceJobSchedule) Minutes', 
		@enabled=1, 
		@freq_type=4, 
		@freq_interval=1, 
		@freq_subday_type=4, 
		@freq_subday_interval=$(LoadTraceJobSchedule), 
		@freq_relative_interval=0, 
		@freq_recurrence_factor=0, 
		@active_start_date=20081104, 
		@active_end_date=99991231, 
		@active_start_time=0, 
		@active_end_time=235959, 
		@schedule_uid=N'02d892fb-b4fb-4bb6-9b9c-af6d39977d26'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_add_jobserver @job_id = @jobId, @server_name = N'(local)'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
COMMIT TRANSACTION
GOTO EndSave
QuitWithRollback:
    IF (@@TRANCOUNT > 0) ROLLBACK TRANSACTION
EndSave:

GO

