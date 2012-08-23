USE [mdw]
GO

/****** Object:  Table [ssas].[SSASEventSubclass]    Script Date: 12/23/2008 14:54:01 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASEventSubclass]') AND type in (N'U'))
DROP TABLE [ssas].[SSASEventSubclass]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASEventSubclass]    Script Date: 12/23/2008 14:54:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING OFF
GO

CREATE TABLE [ssas].[SSASEventSubclass](
	[EventClassID] [int] NULL,
	[EventSubclassID] [int] NULL,
	[EventSubclassName] [varchar](50) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO


