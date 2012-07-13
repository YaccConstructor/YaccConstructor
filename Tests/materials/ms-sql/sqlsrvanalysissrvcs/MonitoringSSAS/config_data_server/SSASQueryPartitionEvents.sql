USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryPartitionEvents_Aggregation]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryPartitionEvents]'))
ALTER TABLE [ssas].[SSASQueryPartitionEvents] DROP CONSTRAINT [FK_SSASQueryPartitionEvents_Aggregation]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryPartitionEvents_SSASPartition]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryPartitionEvents]'))
ALTER TABLE [ssas].[SSASQueryPartitionEvents] DROP CONSTRAINT [FK_SSASQueryPartitionEvents_SSASPartition]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryPartitionEvents_SSASSubCube]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryPartitionEvents]'))
ALTER TABLE [ssas].[SSASQueryPartitionEvents] DROP CONSTRAINT [FK_SSASQueryPartitionEvents_SSASSubCube]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryPartitionEvents]    Script Date: 12/23/2008 14:51:56 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASQueryPartitionEvents]') AND type in (N'U'))
DROP TABLE [ssas].[SSASQueryPartitionEvents]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryPartitionEvents]    Script Date: 12/23/2008 14:51:56 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASQueryPartitionEvents](
	[PartitionID] [int] NOT NULL,
	[AggregationID] [int] NULL,
	[SubCubeID] [int] NOT NULL,
	[PartitionDuration] [int] NOT NULL,
	[DW_RowNumber] [int] NOT NULL,
	[DW_BatchID] [int] NOT NULL
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryPartitionEvents_Aggregation] FOREIGN KEY([AggregationID])
REFERENCES [ssas].[SSASAggregation] ([AggregationID])
GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents] CHECK CONSTRAINT [FK_SSASQueryPartitionEvents_Aggregation]
GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryPartitionEvents_SSASPartition] FOREIGN KEY([PartitionID])
REFERENCES [ssas].[SSASPartition] ([PartitionID])
GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents] CHECK CONSTRAINT [FK_SSASQueryPartitionEvents_SSASPartition]
GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryPartitionEvents_SSASSubCube] FOREIGN KEY([SubCubeID])
REFERENCES [ssas].[SSASSubCube] ([SubCubeID])
GO

ALTER TABLE [ssas].[SSASQueryPartitionEvents] CHECK CONSTRAINT [FK_SSASQueryPartitionEvents_SSASSubCube]
GO


