USE [mdw]
GO

/****** Object:  Table [stage].[SSASDMVCommand]    Script Date: 12/23/2008 13:53:33 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[stage].[SSASDMVCommand]') AND type in (N'U'))
DROP TABLE [stage].[SSASDMVCommand]
GO

/****** Object:  Table [stage].[SSASDMVCommand]    Script Date: 12/23/2008 13:53:33 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [stage].[SSASDMVCommand](
	[SESSION_SPID] [int] NULL,
	[SESSION_COMMAND_COUNT] [int] NULL,
	[COMMAND_START_TIME] [datetime] NULL,
	[COMMAND_ELAPSED_TIME_MS] [bigint] NULL,
	[COMMAND_CPU_TIME_MS] [bigint] NULL,
	[COMMAND_READS] [bigint] NULL,
	[COMMAND_READ_KB] [bigint] NULL,
	[COMMAND_WRITES] [bigint] NULL,
	[COMMAND_WRITE_KB] [bigint] NULL,
	[COMMAND_TEXT] [ntext] NULL,
	[COMMAND_END_TIME] [datetime] NULL,
	[Instance_Name] [nvarchar](255) NULL,
	[CurrentDate] [datetime] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

/****** Object:  Index [IX_stage.SSASDMVCommand]    Script Date: 12/23/2008 13:53:40 ******/
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[stage].[SSASDMVCommand]') AND name = N'IX_stage.SSASDMVCommand')
DROP INDEX [IX_stage.SSASDMVCommand] ON [stage].[SSASDMVCommand] WITH ( ONLINE = OFF )
GO

/****** Object:  Index [IX_stage.SSASDMVCommand]    Script Date: 12/23/2008 13:53:40 ******/
CREATE NONCLUSTERED INDEX [IX_stage.SSASDMVCommand] ON [stage].[SSASDMVCommand] 
(
	[SESSION_SPID] ASC,
	[Instance_Name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO



