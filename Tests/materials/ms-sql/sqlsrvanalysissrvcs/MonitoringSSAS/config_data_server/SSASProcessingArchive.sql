USE [mdw]
GO

/****** Object:  Table [ssas].[SSASProcessingArchive]    Script Date: 12/23/2008 14:17:50 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASProcessingArchive]') AND type in (N'U'))
DROP TABLE [ssas].[SSASProcessingArchive]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASProcessingArchive]    Script Date: 12/23/2008 14:17:50 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASProcessingArchive](
	[RowNumber] [bigint] IDENTITY(0,1) NOT NULL,
	[EventClass] [nvarchar](64) NULL,
	[ConnectionID] [int] NULL,
	[EventSubclass] [nvarchar](128) NULL,
	[JobID] [int] NULL,
	[NTCanonicalUserName] [nvarchar](128) NULL,
	[ObjectID] [nvarchar](128) NULL,
	[ObjectName] [nvarchar](128) NULL,
	[ObjectPath] [nvarchar](2000) NULL,
	[ObjectType] [int] NULL,
	[SPID] [int] NULL,
	[SessionID] [nvarchar](50) NULL,
	[StartTime] [datetime] NULL,
	[StartTime_WithGMTOffset] [datetimeoffset](7) NULL,
	[TextData] [ntext] NULL,
	[CPUTime] [bigint] NULL,
	[Duration] [bigint] NULL,
	[EndTime] [datetime] NULL,
	[EndTime_WithGMTOffset] [datetimeoffset](7) NULL,
	[Error] [int] NULL,
	[Success] [int] NULL,
	[ApplicationName] [nvarchar](128) NULL,
	[BinaryData] [image] NULL,
	[ServerName] [nvarchar](128) NULL,
	[DatabaseName] [nvarchar](128) NULL,
	[DimensionName] [nvarchar](128) NULL,
	[AttributeName] [nvarchar](128) NULL,
	[CubeName] [nvarchar](128) NULL,
	[MeasureGroupName] [nvarchar](128) NULL,
	[PartitionName] [nvarchar](128) NULL,
	[LogTableRowNumber] [int] NOT NULL,
	[P_BatchID] [int] NULL,
 CONSTRAINT [PK_SSASProcessingArchive] PRIMARY KEY NONCLUSTERED 
(
	[RowNumber] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

/****** Object:  Index [PK_SSASProcessingArchive]    Script Date: 12/23/2008 14:18:01 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[ssas].[SSASProcessingArchive]') AND name = N'PK_SSASProcessingArchive')
ALTER TABLE [ssas].[SSASProcessingArchive] DROP CONSTRAINT [PK_SSASProcessingArchive]
GO

/****** Object:  Index [PK_SSASProcessingArchive]    Script Date: 12/23/2008 14:18:01 ******/
ALTER TABLE [ssas].[SSASProcessingArchive] ADD  CONSTRAINT [PK_SSASProcessingArchive] PRIMARY KEY NONCLUSTERED 
(
	[RowNumber] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO


