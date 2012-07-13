USE [mdw]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASQuery_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASQuery] DROP CONSTRAINT [DF_SSASQuery_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASQuery_LastModifiedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASQuery] DROP CONSTRAINT [DF_SSASQuery_LastModifiedDateTime]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQuery]    Script Date: 12/23/2008 14:46:37 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASQuery]') AND type in (N'U'))
DROP TABLE [ssas].[SSASQuery]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQuery]    Script Date: 12/23/2008 14:46:37 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASQuery](
	[QueryID] [int] IDENTITY(1,1) NOT NULL,
	[QueryString] [nvarchar](max) NOT NULL,
	[NK_QueryDataLength] [int] NOT NULL,
	[NK_QueryChecksum] [int] NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASQuery] PRIMARY KEY CLUSTERED 
(
	[QueryID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASQuery] ADD  CONSTRAINT [DF_SSASQuery_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASQuery] ADD  CONSTRAINT [DF_SSASQuery_LastModifiedDateTime]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


