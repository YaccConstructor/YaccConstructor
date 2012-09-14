USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_sessions]    Script Date: 12/23/2008 15:26:19 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_sessions]') AND type in (N'U'))
DROP TABLE [custom_snapshots].[ssas_sessions]
GO

USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_sessions]    Script Date: 12/23/2008 15:26:22 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [custom_snapshots].[ssas_sessions](
	[Date] [datetime] NULL,
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
	[Instance_Name] [nvarchar](255) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

USE [mdw]
GO

/****** Object:  Index [IX_ssas_sessions]    Script Date: 12/23/2008 15:26:45 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_sessions]') AND name = N'IX_ssas_sessions')
DROP INDEX [IX_ssas_sessions] ON [custom_snapshots].[ssas_sessions] WITH ( ONLINE = OFF )
GO

USE [mdw]
GO

/****** Object:  Index [IX_ssas_sessions]    Script Date: 12/23/2008 15:26:46 ******/
CREATE CLUSTERED INDEX [IX_ssas_sessions] ON [custom_snapshots].[ssas_sessions] 
(
	[Date] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO


