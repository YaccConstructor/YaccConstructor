USE [mdw]
GO

/****** Object:  Table [ssas].[AS_TraceData]    Script Date: 12/23/2008 14:04:49 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[AS_TraceData]') AND type in (N'U'))
DROP TABLE [ssas].[AS_TraceData]
GO

/****** Object:  Table [ssas].[AS_TraceData]    Script Date: 12/23/2008 14:04:49 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [ssas].[AS_TraceData](
	[RowNumber] [int] IDENTITY(0,1) NOT NULL,
	[EventClass] [nvarchar](64) NULL,
	[EventSubclass] [nvarchar](64) NULL,
	[StartTime] [datetime] NULL,
	[EndTime] [datetime] NULL,
	[Duration] [bigint] NULL,
	[CPUTime] [bigint] NULL,
	[JobID] [int] NULL,
	[ObjectID] [nvarchar](128) NULL,
	[ObjectType] [int] NULL,
	[ObjectName] [nvarchar](128) NULL,
	[ObjectPath] [nvarchar](128) NULL,
	[Success] [int] NULL,
	[Error] [int] NULL,
	[ConnectionID] [int] NULL,
	[DatabaseName] [nvarchar](128) NULL,
	[SessionID] [nvarchar](50) NULL,
	[NTCanonicalUserName] [nvarchar](255) NULL,
	[SPID] [int] NULL,
	[TextData] [ntext] NULL,
	[ServerName] [nvarchar](128) NULL,
	[CurrentTime] [datetime] NULL,
	[SessionType] [nvarchar](128) NULL,
	[ProgressTotal] [int] NULL,
	[IntegerData] [int] NULL,
	[ObjectReference] [nvarchar](128) NULL,
	[Severity] [int] NULL,
	[ApplicationName] [nvarchar](128) NULL,
	[BinaryData] [image] NULL,
 CONSTRAINT [PK_AS_Trace] PRIMARY KEY CLUSTERED 
(
	[RowNumber] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

/****** Object:  Index [PK_AS_Trace]    Script Date: 12/23/2008 14:05:33 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[ssas].[AS_TraceData]') AND name = N'PK_AS_Trace')
ALTER TABLE [ssas].[AS_TraceData] DROP CONSTRAINT [PK_AS_Trace]
GO

/****** Object:  Index [PK_AS_Trace]    Script Date: 12/23/2008 14:05:33 ******/
ALTER TABLE [ssas].[AS_TraceData] ADD  CONSTRAINT [PK_AS_Trace] PRIMARY KEY CLUSTERED 
(
	[RowNumber] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

