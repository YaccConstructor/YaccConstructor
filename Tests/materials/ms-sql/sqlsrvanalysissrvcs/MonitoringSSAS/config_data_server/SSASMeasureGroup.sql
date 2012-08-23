USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASMeasureGroup_SSASCube]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASMeasureGroup]'))
ALTER TABLE [ssas].[SSASMeasureGroup] DROP CONSTRAINT [FK_SSASMeasureGroup_SSASCube]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASMeasureGroup_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASMeasureGroup] DROP CONSTRAINT [DF_SSASMeasureGroup_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASMeasureGroup_LastModifiedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASMeasureGroup] DROP CONSTRAINT [DF_SSASMeasureGroup_LastModifiedDateTime]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASMeasureGroup]    Script Date: 12/23/2008 14:42:46 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASMeasureGroup]') AND type in (N'U'))
DROP TABLE [ssas].[SSASMeasureGroup]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASMeasureGroup]    Script Date: 12/23/2008 14:42:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASMeasureGroup](
	[MeasureGroupID] [int] IDENTITY(1,1) NOT NULL,
	[MeasureGroupName] [nvarchar](128) NOT NULL,
	[CubeID] [int] NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASMeasureGroup] PRIMARY KEY CLUSTERED 
(
	[MeasureGroupID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASMeasureGroup]  WITH CHECK ADD  CONSTRAINT [FK_SSASMeasureGroup_SSASCube] FOREIGN KEY([CubeID])
REFERENCES [ssas].[SSASCube] ([CubeID])
GO

ALTER TABLE [ssas].[SSASMeasureGroup] CHECK CONSTRAINT [FK_SSASMeasureGroup_SSASCube]
GO

ALTER TABLE [ssas].[SSASMeasureGroup] ADD  CONSTRAINT [DF_SSASMeasureGroup_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASMeasureGroup] ADD  CONSTRAINT [DF_SSASMeasureGroup_LastModifiedDateTime]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


