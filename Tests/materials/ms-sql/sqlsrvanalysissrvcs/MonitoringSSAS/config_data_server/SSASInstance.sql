USE [mdw]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASInstance_DW_Created]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASInstance] DROP CONSTRAINT [DF_SSASInstance_DW_Created]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASInstance_DW_LastModified]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASInstance] DROP CONSTRAINT [DF_SSASInstance_DW_LastModified]
END

GO

/****** Object:  Table [ssas].[SSASInstance]    Script Date: 12/23/2008 14:24:38 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASInstance]') AND type in (N'U'))
DROP TABLE [ssas].[SSASInstance]
GO

/****** Object:  Table [ssas].[SSASInstance]    Script Date: 12/23/2008 14:24:38 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASInstance](
	[InstanceID] [int] IDENTITY(1,1) NOT NULL,
	[InstanceName] [nvarchar](128) NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [datetime] NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASInstance] PRIMARY KEY CLUSTERED 
(
	[InstanceID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UIX_SSASInstance] UNIQUE NONCLUSTERED 
(
	[InstanceName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASInstance] ADD  CONSTRAINT [DF_SSASInstance_DW_Created]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASInstance] ADD  CONSTRAINT [DF_SSASInstance_DW_LastModified]  DEFAULT (getdate()) FOR [DW_LastModified]
GO
