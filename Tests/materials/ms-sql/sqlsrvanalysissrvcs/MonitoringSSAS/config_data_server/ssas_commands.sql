USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_commands]    Script Date: 12/23/2008 13:49:51 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_commands]') AND type in (N'U'))
DROP TABLE [custom_snapshots].[ssas_commands]
GO

/****** Object:  Table [custom_snapshots].[ssas_commands]    Script Date: 12/23/2008 13:49:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [custom_snapshots].[ssas_commands](
	[DATE] [datetime] NULL,
	[SESSION_SPID] [int] NULL,
	[SESSION_COMMAND_COUNT] [int] NULL,
	[COMMAND_START_TIME] [datetime] NULL,
	[COMMAND_ELAPSED_TIME_MS] [bigint] NULL,
	[COMMAND_CPU_TIME_MS] [bigint] NULL,
	[COMMAND_READS] [bigint] NULL,
	[COMMAND_READ_KB] [bigint] NULL,
	[COMMAND_WRITES] [bigint] NULL,
	[COMMAND_WRITE_KB] [bigint] NULL,
	[COMMAND_TEXT] [ntext] NULL,
	[COMMAND_END_TIME] [datetime] NULL,
	[Instance_Name] [nvarchar](255) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

/****** Object:  Index [IX_ssas_commands]    Script Date: 12/23/2008 13:49:57 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_commands]') AND name = N'IX_ssas_commands')
DROP INDEX [IX_ssas_commands] ON [custom_snapshots].[ssas_commands] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_ssas_commands]    Script Date: 12/23/2008 13:49:57 ******/
CREATE CLUSTERED INDEX [IX_ssas_commands] ON [custom_snapshots].[ssas_commands] 
(
	[DATE] ASC,
	[SESSION_SPID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO


