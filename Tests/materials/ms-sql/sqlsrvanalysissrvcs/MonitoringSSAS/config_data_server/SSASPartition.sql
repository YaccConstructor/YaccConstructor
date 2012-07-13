USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASPartition_SSASMeasureGroup]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASPartition]'))
ALTER TABLE [ssas].[SSASPartition] DROP CONSTRAINT [FK_SSASPartition_SSASMeasureGroup]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASPartition_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASPartition] DROP CONSTRAINT [DF_SSASPartition_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASPartition_LastModifiedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASPartition] DROP CONSTRAINT [DF_SSASPartition_LastModifiedDateTime]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASPartition]    Script Date: 12/23/2008 14:45:01 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASPartition]') AND type in (N'U'))
DROP TABLE [ssas].[SSASPartition]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASPartition]    Script Date: 12/23/2008 14:45:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASPartition](
	[PartitionID] [int] IDENTITY(1,1) NOT NULL,
	[MeasureGroupID] [int] NOT NULL,
	[PartitionName] [nvarchar](128) NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASPartition] PRIMARY KEY CLUSTERED 
(
	[PartitionID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASPartition]  WITH CHECK ADD  CONSTRAINT [FK_SSASPartition_SSASMeasureGroup] FOREIGN KEY([MeasureGroupID])
REFERENCES [ssas].[SSASMeasureGroup] ([MeasureGroupID])
GO

ALTER TABLE [ssas].[SSASPartition] CHECK CONSTRAINT [FK_SSASPartition_SSASMeasureGroup]
GO

ALTER TABLE [ssas].[SSASPartition] ADD  CONSTRAINT [DF_SSASPartition_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASPartition] ADD  CONSTRAINT [DF_SSASPartition_LastModifiedDateTime]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


