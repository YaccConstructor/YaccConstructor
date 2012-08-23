USE [mdw]
GO

/****** Object:  Table [custom_snapshots].[ssas_memory_usage]    Script Date: 12/23/2008 13:46:44 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_memory_usage]') AND type in (N'U'))
DROP TABLE [custom_snapshots].[ssas_memory_usage]
GO

/****** Object:  Table [custom_snapshots].[ssas_memory_usage]    Script Date: 12/23/2008 13:46:44 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [custom_snapshots].[ssas_memory_usage](
	[Date] [datetime] NULL,
	[OBJECT_PARENT_PATH] [nvarchar](3800) NULL,
	[OBJECT_MEMORY_SHRINKABLE] [bigint] NULL,
	[OBJECT_MEMORY_NONSHRINKABLE] [bigint] NULL,
	[OBJECT_VERSION] [int] NULL,
	[OBJECT_DATA_VERSION] [int] NULL,
	[OBJECT_TYPE_ID] [int] NULL,
	[OBJECT_TIME_CREATED] [datetime] NULL,
	[OBJECT_ID] [nvarchar](199) NULL,
	[ObjectPath] [nvarchar](4000) NULL,
	[Instance_Name] [nvarchar](255) NULL
) ON [PRIMARY]

GO

/****** Object:  Index [IX_ssas_memory_usage]    Script Date: 12/23/2008 13:46:53 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[custom_snapshots].[ssas_memory_usage]') AND name = N'IX_ssas_memory_usage')
DROP INDEX [IX_ssas_memory_usage] ON [custom_snapshots].[ssas_memory_usage] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_ssas_memory_usage]    Script Date: 12/23/2008 13:46:53 ******/
CREATE CLUSTERED INDEX [IX_ssas_memory_usage] ON [custom_snapshots].[ssas_memory_usage] 
(
	[Date] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO



