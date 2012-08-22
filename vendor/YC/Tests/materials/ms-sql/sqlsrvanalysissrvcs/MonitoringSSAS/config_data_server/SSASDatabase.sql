USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASDatabase_SSASInstance]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASDatabase]'))
ALTER TABLE [ssas].[SSASDatabase] DROP CONSTRAINT [FK_SSASDatabase_SSASInstance]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASDatabase_DW_Created]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASDatabase] DROP CONSTRAINT [DF_SSASDatabase_DW_Created]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASDatabase_DW_LastModified]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASDatabase] DROP CONSTRAINT [DF_SSASDatabase_DW_LastModified]
END

GO

/****** Object:  Table [ssas].[SSASDatabase]    Script Date: 12/23/2008 14:27:49 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASDatabase]') AND type in (N'U'))
DROP TABLE [ssas].[SSASDatabase]
GO

/****** Object:  Table [ssas].[SSASDatabase]    Script Date: 12/23/2008 14:27:49 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASDatabase](
	[DatabaseID] [int] IDENTITY(1,1) NOT NULL,
	[InstanceID] [int] NOT NULL,
	[DatabaseName] [nvarchar](128) NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASDatabase] PRIMARY KEY CLUSTERED 
(
	[DatabaseID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASDatabase]  WITH CHECK ADD  CONSTRAINT [FK_SSASDatabase_SSASInstance] FOREIGN KEY([InstanceID])
REFERENCES [ssas].[SSASInstance] ([InstanceID])
GO

ALTER TABLE [ssas].[SSASDatabase] CHECK CONSTRAINT [FK_SSASDatabase_SSASInstance]
GO

ALTER TABLE [ssas].[SSASDatabase] ADD  CONSTRAINT [DF_SSASDatabase_DW_Created]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASDatabase] ADD  CONSTRAINT [DF_SSASDatabase_DW_LastModified]  DEFAULT (getdate()) FOR [DW_LastModified]
GO

