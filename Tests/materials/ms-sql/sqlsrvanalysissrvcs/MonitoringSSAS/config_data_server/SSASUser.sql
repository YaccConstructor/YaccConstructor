USE [mdw]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASUser_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASUser] DROP CONSTRAINT [DF_SSASUser_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASUser_LastModifiedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASUser] DROP CONSTRAINT [DF_SSASUser_LastModifiedDateTime]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASUser]    Script Date: 12/23/2008 14:47:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASUser]') AND type in (N'U'))
DROP TABLE [ssas].[SSASUser]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASUser]    Script Date: 12/23/2008 14:47:22 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASUser](
	[SSASUserID] [int] IDENTITY(1,1) NOT NULL,
	[NTCanonicalUserName] [nvarchar](128) NOT NULL,
	[WindowsDomainName] [nvarchar](128) NULL,
	[WindowsUserName] [nvarchar](128) NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASUser] PRIMARY KEY CLUSTERED 
(
	[SSASUserID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UIX_SSASUser] UNIQUE NONCLUSTERED 
(
	[NTCanonicalUserName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASUser] ADD  CONSTRAINT [DF_SSASUser_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASUser] ADD  CONSTRAINT [DF_SSASUser_LastModifiedDateTime]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


