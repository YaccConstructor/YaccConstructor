USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASCube_SSASDatabase]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASCube]'))
ALTER TABLE [ssas].[SSASCube] DROP CONSTRAINT [FK_SSASCube_SSASDatabase]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASCube_DW_Created]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASCube] DROP CONSTRAINT [DF_SSASCube_DW_Created]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASCube_DW_LastModified]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASCube] DROP CONSTRAINT [DF_SSASCube_DW_LastModified]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASCube]    Script Date: 12/23/2008 14:37:42 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASCube]') AND type in (N'U'))
DROP TABLE [ssas].[SSASCube]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASCube]    Script Date: 12/23/2008 14:37:42 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASCube](
	[CubeID] [int] IDENTITY(1,1) NOT NULL,
	[CubeName] [nvarchar](128) NULL,
	[DatabaseID] [int] NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASCube] PRIMARY KEY CLUSTERED 
(
	[CubeID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASCube]  WITH CHECK ADD  CONSTRAINT [FK_SSASCube_SSASDatabase] FOREIGN KEY([DatabaseID])
REFERENCES [ssas].[SSASDatabase] ([DatabaseID])
GO

ALTER TABLE [ssas].[SSASCube] CHECK CONSTRAINT [FK_SSASCube_SSASDatabase]
GO

ALTER TABLE [ssas].[SSASCube] ADD  CONSTRAINT [DF_SSASCube_DW_Created]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASCube] ADD  CONSTRAINT [DF_SSASCube_DW_LastModified]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


