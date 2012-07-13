USE [mdw]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryExecution_SSASCube]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryExecution]'))
ALTER TABLE [ssas].[SSASQueryExecution] DROP CONSTRAINT [FK_SSASQueryExecution_SSASCube]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryExecution_SSASQuery]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryExecution]'))
ALTER TABLE [ssas].[SSASQueryExecution] DROP CONSTRAINT [FK_SSASQueryExecution_SSASQuery]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[ssas].[FK_SSASQueryExecution_SSASUser]') AND parent_object_id = OBJECT_ID(N'[ssas].[SSASQueryExecution]'))
ALTER TABLE [ssas].[SSASQueryExecution] DROP CONSTRAINT [FK_SSASQueryExecution_SSASUser]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASQueryExecution_CreatedDateTime]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASQueryExecution] DROP CONSTRAINT [DF_SSASQueryExecution_CreatedDateTime]
END

GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF_SSASQueryExecution_DW_LastModified]') AND type = 'D')
BEGIN
ALTER TABLE [ssas].[SSASQueryExecution] DROP CONSTRAINT [DF_SSASQueryExecution_DW_LastModified]
END

GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryExecution]    Script Date: 12/23/2008 14:48:04 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASQueryExecution]') AND type in (N'U'))
DROP TABLE [ssas].[SSASQueryExecution]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASQueryExecution]    Script Date: 12/23/2008 14:48:04 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASQueryExecution](
	[QueryExecutionID] [int] IDENTITY(1,1) NOT NULL,
	[QueryID] [int] NOT NULL,
	[SSASUserID] [int] NOT NULL,
	[SessionID] [nvarchar] (50) NOT NULL,
	[ConnectionID] [int] NULL,
	[SPID] [int] NULL,
	[CubeID] [int] NOT NULL,
	[DW_RowNumber] [int] NOT NULL,
	[DW_ExecutionID] [int] NOT NULL,
	[DW_Created] [datetime] NOT NULL,
	[DW_LastModified] [nchar](10) NOT NULL,
	[DW_BatchID] [int] NOT NULL,
 CONSTRAINT [PK_SSASQueryExecution] PRIMARY KEY CLUSTERED 
(
	[QueryExecutionID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UIX_SSASQueryExecution] UNIQUE NONCLUSTERED 
(
	[QueryID] ASC,
	[DW_ExecutionID] ASC,
	[SessionID] ASC,
	[CubeID] ASC,
	[DW_BatchID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [ssas].[SSASQueryExecution]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryExecution_SSASCube] FOREIGN KEY([CubeID])
REFERENCES [ssas].[SSASCube] ([CubeID])
GO

ALTER TABLE [ssas].[SSASQueryExecution] CHECK CONSTRAINT [FK_SSASQueryExecution_SSASCube]
GO

ALTER TABLE [ssas].[SSASQueryExecution]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryExecution_SSASQuery] FOREIGN KEY([QueryID])
REFERENCES [ssas].[SSASQuery] ([QueryID])
GO

ALTER TABLE [ssas].[SSASQueryExecution] CHECK CONSTRAINT [FK_SSASQueryExecution_SSASQuery]
GO

ALTER TABLE [ssas].[SSASQueryExecution]  WITH CHECK ADD  CONSTRAINT [FK_SSASQueryExecution_SSASUser] FOREIGN KEY([SSASUserID])
REFERENCES [ssas].[SSASUser] ([SSASUserID])
GO

ALTER TABLE [ssas].[SSASQueryExecution] CHECK CONSTRAINT [FK_SSASQueryExecution_SSASUser]
GO

ALTER TABLE [ssas].[SSASQueryExecution] ADD  CONSTRAINT [DF_SSASQueryExecution_CreatedDateTime]  DEFAULT (getdate()) FOR [DW_Created]
GO

ALTER TABLE [ssas].[SSASQueryExecution] ADD  CONSTRAINT [DF_SSASQueryExecution_DW_LastModified]  DEFAULT (getdate()) FOR [DW_LastModified]
GO


