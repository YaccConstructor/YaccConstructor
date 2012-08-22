USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASAggregation_SSASPartition]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASAggregation]'))
ALTER TABLE [ssas].[SSASAggregation] DROP CONSTRAINT [FK_SSASAggregation_SSASPartition]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASAggregation_DW_Created]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASAggregation] DROP CONSTRAINT [DF_SSASAggregation_DW_Created]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASAggregation_DW_LastModified]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASAggregation] DROP CONSTRAINT [DF_SSASAggregation_DW_LastModified]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASAggregation]    Script Date: 12/23/2008 14:45:48 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASAggregation]') AND type in (N'U'))
DROP TABLE [ssas].[SSASAggregation]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASAggregation]    Script Date: 12/23/2008 14:45:48 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASAggregation](
	[AggregationID] [int] IDENTITY(1,1) NOT NULL,
	[AggregationName] [nvarchar](128) NOT NULL,
	[PartitionID] [int] NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASAggregation] PRIMARY KEY CLUSTERED 
(
	[AggregationID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASAggregation]  WITH CHECK ADD  CONSTRAINT [FK_SSASAggregation_SSASPartition] FOREIGN KEY([PartitionID])
REFERENCES [ssas].[SSASPartition] ([PartitionID])
GO

ALTER TABLE [ssas].[SSASAggregation] CHECK CONSTRAINT [FK_SSASAggregation_SSASPartition]
GO

ALTER TABLE [ssas].[SSASAggregation] ADD  CONSTRAINT [DF_SSASAggregation_DW_Created]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASAggregation] ADD  CONSTRAINT [DF_SSASAggregation_DW_LastModified]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


