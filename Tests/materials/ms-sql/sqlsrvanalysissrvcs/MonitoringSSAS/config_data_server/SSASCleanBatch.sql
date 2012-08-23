USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASCleanBatch]    Script Date: 12/23/2008 15:04:14 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASCleanBatch]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[SSASCleanBatch]
GO

USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASCleanBatch]    Script Date: 12/23/2008 15:04:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [ssas].[SSASCleanBatch]
 @BatchID INT
 AS
 
 DELETE FROM [SSAS].SSASWorkTraceTable WITH (TABLOCKX)
 WHERE P_BatchID <= @BatchID	/* get rid of all batches before this, including old tails, if they have  been matched */
 AND P_RowNumber_Query IS NOT NULL	/* dont delete the "tail" of the table */
 
 UPDATE [SSAS].[SSASWorkTracetable]
   SET [P_BatchID] = NULL
      ,[P_RowNumber_Query] = NULL
      ,[P_RowNumber_SubCube] = NULL
      ,[P_RowNumber_FirstSubCube] = NULL
      ,[P_QueryChecksum] = NULL
      ,[P_ExecutionID] = NULL
 

GO


