USE [mdw]
GO

/****** Object:  Table [ssas].[SSASEventClass]    Script Date: 12/23/2008 14:53:30 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASEventClass]') AND type in (N'U'))
DROP TABLE [ssas].[SSASEventClass]
GO

USE [mdw]
GO

/****** Object:  Table [ssas].[SSASEventClass]    Script Date: 12/23/2008 14:53:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING OFF
GO

CREATE TABLE [ssas].[SSASEventClass](
	[EventClassID] [int] NULL,
	[EventCategoryName] [varchar](50) NULL,
	[EventClassName] [varchar](50) NULL,
	[EventClassDescription] [varchar](256) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO


