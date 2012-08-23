USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_execution_subcube]    Script Date: 12/23/2008 17:14:56 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_query_execution_subcube]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_query_execution_subcube]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_execution_subcube]    Script Date: 12/23/2008 17:14:56 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


create proc [ssas].[get_query_execution_subcube] (
	 @QueryExecutionID int
   ,@mdw_database_name nvarchar(128)
    ,@debug int = 0
)
AS
BEGIN
   DECLARE @SQLExecString nvarchar(4000)
	    	    SET @SQLExecString = 
'

	SELECT
		sc.SubCubeName
		, qev.StorageEngineDuration
		, mg.MeasureGroupName
		, qev.NumberPartitions
		, qev.NumberAggregations
		, CASE WHEN qev.Cached =0 THEN ''True'' ELSE ''False''	END AS Cached
	FROM ' + @mdw_database_name + '.[SSAS].[SSASQuerySubCubeEvents]  qev
	JOIN ' + @mdw_database_name + '.[SSAS].[SSASSubCube] sc
		ON qev.SubCubeID = sc.SubCubeID
	JOIN ' + @mdw_database_name + '.[SSAS].[SSASMeasureGroup] mg
		ON 	mg.MeasureGroupID = qev.MeasureGroupID
	where qev.QueryExecutionID = ' + CAST(@QueryExecutionID AS nvarchar(20)) + ' 
	ORDER BY qev.StorageEngineDuration desc
	'
	IF @Debug = 1
	    PRINT @SQLExecString
	ELSE
	    EXEC (@SQLExecString)
	
END

RETURN





GO


