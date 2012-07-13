USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_connections]    Script Date: 12/23/2008 13:48:51 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_connections]') AND type in (N'U'))
DROP TABLE [custom_snapshots].[ssas_connections]
GO

/****** Object:  Table [custom_snapshots].[ssas_connections]    Script Date: 12/23/2008 13:48:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [custom_snapshots].[ssas_connections](
	[Date] [datetime] NULL,
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
	[Instance_Name] [nvarchar](255) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO


/****** Object:  Index [IX_ssas_connections]    Script Date: 12/23/2008 13:48:57 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_connections]') AND name = N'IX_ssas_connections')
DROP INDEX [IX_ssas_connections] ON [custom_snapshots].[ssas_connections] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_ssas_connections]    Script Date: 12/23/2008 13:48:57 ******/
CREATE CLUSTERED INDEX [IX_ssas_connections] ON [custom_snapshots].[ssas_connections] 
(
	[CONNECTION_ID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

