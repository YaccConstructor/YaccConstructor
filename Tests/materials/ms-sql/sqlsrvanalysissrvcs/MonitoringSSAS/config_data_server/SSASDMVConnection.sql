USE [mdw]
GO

/****** Object:  Table [stage].[SSASDMVConnection]    Script Date: 12/23/2008 13:54:31 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[stage].[SSASDMVConnection]') AND type in (N'U'))
DROP TABLE [stage].[SSASDMVConnection]
GO

USE [mdw]
GO

/****** Object:  Table [stage].[SSASDMVConnection]    Script Date: 12/23/2008 13:54:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [stage].[SSASDMVConnection](
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
	[Instance_Name] [nvarchar](255) NULL,
	[CurrentDate] [datetime] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

/****** Object:  Index [IX_stage.SSASDMVConnection]    Script Date: 12/23/2008 13:54:35 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[stage].[SSASDMVConnection]') AND name = N'IX_stage.SSASDMVConnection')
DROP INDEX [IX_stage.SSASDMVConnection] ON [stage].[SSASDMVConnection] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_stage.SSASDMVConnection]    Script Date: 12/23/2008 13:54:35 ******/
CREATE NONCLUSTERED INDEX [IX_stage.SSASDMVConnection] ON [stage].[SSASDMVConnection] 
(
	[CONNECTION_ID] ASC,
	[Instance_Name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO


