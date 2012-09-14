USE [mdw]
GO

/****** Object:  Table [ssas].[SSASBatch]    Script Date: 12/23/2008 14:52:41 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASBatch]') AND type in (N'U'))
DROP TABLE [ssas].[SSASBatch]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASBatch]    Script Date: 12/23/2008 14:52:41 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[SSASBatch](
	[BatchID] [int] IDENTITY(1,1) NOT NULL,
	[Loaded] [datetime] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[BatchID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO


