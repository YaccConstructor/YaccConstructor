USE [mdw]
GO

/****** Object:  View [ssas].[SSASWorkTable_Transform]    Script Date: 12/23/2008 14:55:25 ******/
IF  EXISTS (SELECT * FROM sys.views WHERE object_id = OBJECT_ID(N'[ssas].[SSASWorkTable_Transform]'))
DROP VIEW [ssas].[SSASWorkTable_Transform]
GO

USE [mdw]
GO

/****** Object:  View [ssas].[SSASWorkTable_Transform]    Script Date: 12/23/2008 14:55:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/****** Object:  View [SSAS].[SSASWorkTable_Transform]    Script Date: 04/27/2008 20:58:41 ******/
CREATE VIEW [ssas].[SSASWorkTable_Transform]
AS

SELECT swt.RowNumber
      ,swt.EventClass
      ,swt.ConnectionID
      ,swt.EventSubclass
      ,swt.EventClass as EventDescription
      ,swt.JobID
      ,swt.NTCanonicalUserName
      ,swt.WindowsDomainName
      ,swt.WindowsUserName
      ,swt.ObjectID
      ,swt.ObjectName
      ,swt.ObjectPath
      ,swt.ObjectType
      ,swt.SPID
      ,swt.SessionID
      ,swt.StartTime
      ,swt.StartTime_WithGMTOffSet
      ,swt.TextData
      ,swt.QueryString
      ,swt.QueryCheckSum
      ,swt.QueryDataLength
      ,swt.CPUTime
      ,swt.Duration
      ,swt.EndTime
      ,swt.EndTime_WithGMTOffSet
      ,swt.Error
      ,swt.ApplicationName
      ,swt.Success
      ,swt.BinaryData
      ,swt.Databasename
      ,swt.ServerName
      ,swt.CubeName
      ,swt.MeasureGroupName
      ,swt.PartitionName
      ,swt.AggregationName
      ,swt.SubcubeName
      ,swt.P_BatchID
      ,swt.P_RowNumber_Query
      ,swt.P_RowNumber_SubCube
      ,swt.P_RowNumber_FirstSubCube
      ,swt.P_QueryChecksum
      ,swt.P_ExecutionID
   
  FROM [SSAS].SSASWorkTraceTable swt




GO


