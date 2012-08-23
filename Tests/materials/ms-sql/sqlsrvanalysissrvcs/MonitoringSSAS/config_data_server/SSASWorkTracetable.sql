USE [mdw]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASWorkTracetable_CubeName]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASWorkTracetable] DROP CONSTRAINT [DF_SSASWorkTracetable_CubeName]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASWorkTracetable_MeasureGroupName]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASWorkTracetable] DROP CONSTRAINT [DF_SSASWorkTracetable_MeasureGroupName]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASWorkTracetable_PartitionName]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASWorkTracetable] DROP CONSTRAINT [DF_SSASWorkTracetable_PartitionName]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASWorkTracetable_AggregationName]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASWorkTracetable] DROP CONSTRAINT [DF_SSASWorkTracetable_AggregationName]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASWorkTracetable]    Script Date: 12/23/2008 14:21:40 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASWorkTracetable]') AND type in (N'U'))
DROP TABLE [ssas].[SSASWorkTracetable]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASWorkTracetable]    Script Date: 12/23/2008 14:21:40 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [ssas].[SSASWorkTracetable](
	[RowNumber] [bigint] IDENTITY(1,1) NOT NULL,
	[LogTableRowNumber] [int] NULL,
	[EventClass] [nvarchar](64) NULL,
	[ConnectionID] [int] NULL,
	[EventSubclass] [nvarchar](64) NULL,
	[JobID] [int] NULL,
	[NTCanonicalUserName] [nvarchar](256) NULL,
	[WindowsDomainName] [nvarchar](128) NULL,
	[WindowsUserName] [nvarchar](128) NULL,
	[ObjectID] [nvarchar](128) NULL,
	[ObjectName] [nvarchar](128) NULL,
	[ObjectPath] [nvarchar](128) NULL,
	[ObjectType] [int] NULL,
	[SPID] [int] NULL,
	[SessionID] [nvarchar](50) NULL,
	[StartTime] [datetime] NULL,
	[StartTime_WithGMTOffSet] [datetimeoffset](7) NOT NULL,
	[TextData] [ntext] NULL,
	[QueryString] [varchar](max) NULL,
	[QueryDataLength] [int] NULL,
	[QueryCheckSum] [int] NULL,
	[CPUTime] [bigint] NULL,
	[Duration] [bigint] NULL,
	[EndTime] [datetime] NULL,
	[EndTime_WithGMTOffSet] [datetimeoffset](7) NOT NULL,
	[Error] [int] NULL,
	[ApplicationName] [nvarchar](128) NULL,
	[Success] [int] NULL,
	[BinaryData] [image] NULL,
	[Databasename] [nvarchar](128) NULL,
	[ServerName] [nvarchar](128) NULL,
	[CubeName] [nvarchar](128) NULL,
	[MeasureGroupName] [nvarchar](128) NULL,
	[PartitionName] [nvarchar](128) NULL,
	[AggregationName] [nvarchar](128) NULL,
	[SubcubeName] [nvarchar](2000) NULL,
	[P_BatchID] [int] NULL,
	[P_RowNumber_Query] [bigint] NULL,
	[P_RowNumber_SubCube] [bigint] NULL,
	[P_RowNumber_FirstSubCube] [bigint] NULL,
	[P_QueryChecksum] [int] NULL,
	[P_ExecutionID] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [ssas].[SSASWorkTracetable] ADD  CONSTRAINT [DF_SSASWorkTracetable_CubeName]  DEFAULT (N'No Cube Name') FOR [CubeName]
GO

ALTER TABLE [ssas].[SSASWorkTracetable] ADD  CONSTRAINT [DF_SSASWorkTracetable_MeasureGroupName]  DEFAULT (N'No Measure Group Name') FOR [MeasureGroupName]
GO

ALTER TABLE [ssas].[SSASWorkTracetable] ADD  CONSTRAINT [DF_SSASWorkTracetable_PartitionName]  DEFAULT (N'No Partition Name') FOR [PartitionName]
GO

ALTER TABLE [ssas].[SSASWorkTracetable] ADD  CONSTRAINT [DF_SSASWorkTracetable_AggregationName]  DEFAULT (N'No Aggregation Name') FOR [AggregationName]
GO


