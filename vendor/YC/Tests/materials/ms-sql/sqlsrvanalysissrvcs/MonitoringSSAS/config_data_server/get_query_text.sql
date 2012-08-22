USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_text]    Script Date: 12/23/2008 17:15:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[get_query_text]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[get_query_text]
GO

USE [mdw_control]
GO

/****** Object:  StoredProcedure [ssas].[get_query_text]    Script Date: 12/23/2008 17:15:21 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROC [ssas].[get_query_text] (
    @QueryID int
    ,@mdw_database_name nvarchar(128)
    ,@debug int = 0
)
AS
BEGIN
-- Declare variables
    DECLARE @SQLExecString nvarchar(4000)
    ,@cr char(2)
    , @lf char(1)
-- Set variables    
	SET @lf = CHAR(10)
	SET @cr = CHAR(13) + CHAR(10)
	
	    	    SET @SQLExecString = 
'SELECT

 REPLACE(QueryString, ''' + @lf + ''', ''' + @cr + ''') AS QueryString
  FROM ' + @mdw_database_name + '.[SSAS].[SSASQuery]
 WHERE QueryID = ' + CAST(@QueryID AS nvarchar(20)) + '
 '
	IF @Debug = 1
	    PRINT @SQLExecString
	ELSE
	    EXEC (@SQLExecString)
	
END

RETURN



GO


