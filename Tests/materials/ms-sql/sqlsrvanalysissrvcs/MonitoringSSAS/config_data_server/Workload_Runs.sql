USE [mdw_control]
GO

IF  EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[DF__test_run__collec__7E6CC920]') AND type = 'D')
BEGIN
ALTER TABLE [mdw].[workload_runs] DROP CONSTRAINT [DF__test_run__collec__7E6CC920]
END

GO

USE [mdw_control]
GO

/****** Object:  Table [mdw].[workload_runs]    Script Date: 12/23/2008 15:55:58 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[mdw].[workload_runs]') AND type in (N'U'))
DROP TABLE [mdw].[workload_runs]
GO

USE [mdw_control]
GO

/****** Object:  Table [mdw].[workload_runs]    Script Date: 12/23/2008 15:56:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [mdw].[workload_runs](
	[workload_id] [int] IDENTITY(1,1) NOT NULL,
	[workload_name] [nvarchar](128) NULL,
	[workload_description] [nvarchar](max) NULL,
	[start_time] [datetimeoffset](7) NULL,
	[start_time_local] [datetime] NULL,
	[start_time_gmt] [datetime] NULL,
	[end_time] [datetimeoffset](7) NULL,
	[end_time_local] [datetime] NULL,
	[end_time_gmt] [datetime] NULL,
	[instance_name_filter] [nvarchar](128) NULL,
	[database_name_filter] [nvarchar](128) NULL,
	[mdw_database_name] [nvarchar](128) NULL,
	[collection_interval_perfmon] [int] NOT NULL,
	[use_as_baseline_yn] [nvarchar](2) NULL,
	[collection_interval_dmv] [int] NULL,
 CONSTRAINT [PK_test_run] PRIMARY KEY CLUSTERED 
(
	[workload_id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [mdw].[workload_runs] ADD  CONSTRAINT [DF__test_run__collec__7E6CC920]  DEFAULT ((15)) FOR [collection_interval_perfmon]
GO


