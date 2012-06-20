USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASSubCube_SSASMeasureGroup]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASSubCube]'))
ALTER TABLE [ssas].[SSASSubCube] DROP CONSTRAINT [FK_SSASSubCube_SSASMeasureGroup]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASSubCube_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASSubCube] DROP CONSTRAINT [DF_SSASSubCube_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASSubCube_LastModifiedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASSubCube] DROP CONSTRAINT [DF_SSASSubCube_LastModifiedDateTime]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASSubCube]    Script Date: 12/23/2008 14:43:48 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASSubCube]') AND type in (N'U'))
DROP TABLE [ssas].[SSASSubCube]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASSubCube]    Script Date: 12/23/2008 14:43:48 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASSubCube](
	[SubCubeID] [int] IDENTITY(1,1) NOT NULL,
	[SubCubeName] [nvarchar](2000) NULL,
	[MeasureGroupID] [int] NOT NULL,
	[DW_RowNumber_SubCube] [int] NOT NULL,
	[SubCubeDescription] [nvarchar](2000) NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASSubCube] PRIMARY KEY CLUSTERED 
(
	[SubCubeID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASSubCube]  WITH CHECK ADD  CONSTRAINT [FK_SSASSubCube_SSASMeasureGroup] FOREIGN KEY([MeasureGroupID])
REFERENCES [ssas].[SSASMeasureGroup] ([MeasureGroupID])
GO

ALTER TABLE [ssas].[SSASSubCube] CHECK CONSTRAINT [FK_SSASSubCube_SSASMeasureGroup]
GO

ALTER TABLE [ssas].[SSASSubCube] ADD  CONSTRAINT [DF_SSASSubCube_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASSubCube] ADD  CONSTRAINT [DF_SSASSubCube_LastModifiedDateTime]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


