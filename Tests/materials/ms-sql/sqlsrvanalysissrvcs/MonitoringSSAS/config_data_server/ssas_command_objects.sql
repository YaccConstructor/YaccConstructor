USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_command_objects]    Script Date: 12/23/2008 13:50:56 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_command_objects]') AND type in (N'U'))
DROP TABLE [custom_snapshots].[ssas_command_objects]
GO

/****** Object:  Table [custom_snapshots].[ssas_command_objects]    Script Date: 12/23/2008 13:50:56 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [custom_snapshots].[ssas_command_objects](
	[Date] [datetime] NULL,
	[SESSION_SPID] [int] NULL,
	[SESSION_COMMAND_COUNT] [int] NULL,
	[OBJECT_VERSION] [int] NULL,
	[OBJECT_DATA_VERSION] [int] NULL,
	[OBJECT_CPU_TIME_MS] [bigint] NULL,
	[OBJECT_READS] [bigint] NULL,
	[OBJECT_READ_KB] [bigint] NULL,
	[OBJECT_WRITES] [bigint] NULL,
	[OBJECT_WRITE_KB] [bigint] NULL,
	[OBJECT_ROWS_SCANNED] [bigint] NULL,
	[OBJECT_ROWS_RETURNED] [bigint] NULL,
	[SESSION_ID] [nvarchar](50) NULL,
	[OBJECT_PARENT_PATH] [nvarchar](3800) NULL,
	[OBJECT_ID] [nvarchar](199) NULL,
	[ObjectPath] [nvarchar](4000) NULL,
	[Instance_Name] [nvarchar](255) NULL
) ON [PRIMARY]

GO


/****** Object:  Index [IX_ssas_command_objects]    Script Date: 12/23/2008 13:51:16 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_command_objects]') AND name = N'IX_ssas_command_objects')
DROP INDEX [IX_ssas_command_objects] ON [custom_snapshots].[ssas_command_objects] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_ssas_command_objects]    Script Date: 12/23/2008 13:51:16 ******/
CREATE CLUSTERED INDEX [IX_ssas_command_objects] ON [custom_snapshots].[ssas_command_objects] 
(
	[Date] ASC,
	[SESSION_SPID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

