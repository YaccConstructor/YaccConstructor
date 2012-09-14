USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQuerySubCubeEvents_SSASMeasureGroup]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQuerySubCubeEvents]'))
ALTER TABLE [ssas].[SSASQuerySubCubeEvents] DROP CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASMeasureGroup]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQuerySubCubeEvents_SSASQueryExecution]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQuerySubCubeEvents]'))
ALTER TABLE [ssas].[SSASQuerySubCubeEvents] DROP CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASQueryExecution]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQuerySubCubeEvents_SSASSubCube]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQuerySubCubeEvents]'))
ALTER TABLE [ssas].[SSASQuerySubCubeEvents] DROP CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASSubCube]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQuerySubCubeEvents]    Script Date: 12/23/2008 14:51:11 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASQuerySubCubeEvents]') AND type in (N'U'))
DROP TABLE [ssas].[SSASQuerySubCubeEvents]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQuerySubCubeEvents]    Script Date: 12/23/2008 14:51:11 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASQuerySubCubeEvents](
	[SubCubeID] [int] NOT NULL,
	[Cached] [int] NOT NULL,
	[QueryExecutionID] [int] NOT NULL,
	[MeasureGroupID] [int] NOT NULL,
	[StorageEngineDuration] [int] NOT NULL,
	[NumberPartitions] [int] NOT NULL,
	[NumberAggregations] [int] NOT NULL,
	[DW_RowNumber] [int] NOT NULL,
	[DW_BatchID] [int] NOT NULL
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASMeasureGroup] FOREIGN KEY([MeasureGroupID])
REFERENCES [ssas].[SSASMeasureGroup] ([MeasureGroupID])
GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents] CHECK CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASMeasureGroup]
GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASQueryExecution] FOREIGN KEY([QueryExecutionID])
REFERENCES [ssas].[SSASQueryExecution] ([QueryExecutionID])
GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents] CHECK CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASQueryExecution]
GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASSubCube] FOREIGN KEY([SubCubeID])
REFERENCES [ssas].[SSASSubCube] ([SubCubeID])
GO

ALTER TABLE [ssas].[SSASQuerySubCubeEvents] CHECK CONSTRAINT [FK_SSASQuerySubCubeEvents_SSASSubCube]
GO


