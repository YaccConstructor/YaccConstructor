USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryExecutionEvents_SSASQueryExecution]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryExecutionEvents]'))
ALTER TABLE [ssas].[SSASQueryExecutionEvents] DROP CONSTRAINT [FK_SSASQueryExecutionEvents_SSASQueryExecution]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryExecutionEvents]    Script Date: 12/23/2008 14:50:18 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASQueryExecutionEvents]') AND type in (N'U'))
DROP TABLE [ssas].[SSASQueryExecutionEvents]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryExecutionEvents]    Script Date: 12/23/2008 14:50:18 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASQueryExecutionEvents](
	[QueryExecutionID] [int] NOT NULL,
	[QueryID] [int] NOT NULL,
	[Duration] [int] NOT NULL,
	[CPUTime] [int] NOT NULL,
	[StorageEngineDuration] [int] NOT NULL,
	[FormulaEngineDuration] [int] NOT NULL,
	[NumberCachedSubcubes] [int] NOT NULL,
	[NumberNonCachedSubcubes] [int] NOT NULL,
	[NumberPartitions] [int] NOT NULL,
	[NumberAggregations] [int] NOT NULL,
	[NumberMeasureGroups] [int] NOT NULL,
	[EndDateTime] [datetime] NOT NULL,
	[EndDateTime_WithGMTOffSet] [datetimeoffset](7) NOT NULL,
	[StartDateTime] [datetime] NOT NULL,
	[StartDateTime_WithGMTOffSet] [datetimeoffset](7) NOT NULL,
	[EndDate] [date] NOT NULL,
	[StartDate] [date] NOT NULL,
	[EndTime] [time](7) NOT NULL,
	[StartTime] [time](7) NOT NULL,
	[DW_RowNumber] [int] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASQueryExecutionEvents] PRIMARY KEY CLUSTERED 
(
	[QueryExecutionID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASQueryExecutionEvents]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryExecutionEvents_SSASQueryExecution] FOREIGN KEY([QueryExecutionID])
REFERENCES [ssas].[SSASQueryExecution] ([QueryExecutionID])
GO

ALTER TABLE [ssas].[SSASQueryExecutionEvents] CHECK CONSTRAINT [FK_SSASQueryExecutionEvents_SSASQueryExecution]
GO


