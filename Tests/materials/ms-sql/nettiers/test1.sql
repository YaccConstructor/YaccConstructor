
USE [AdventureWorks]
GO
SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_Get_List procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_Get_List') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_Get_List
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets all records from the SalesTerritory table
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_Get_List

AS


				
				SELECT
					[TerritoryID],
					[Name],
					[CountryRegionCode],
					[Group],
					[SalesYTD],
					[SalesLastYear],
					[CostYTD],
					[CostLastYear],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesTerritory]
					
				SELECT @@ROWCOUNT
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_GetPaged procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_GetPaged') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetPaged
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records from the SalesTerritory table passing page index and page count parameters
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetPaged
(

	@WhereClause varchar (2000)  ,

	@OrderBy varchar (2000)  ,

	@PageIndex int   ,

	@PageSize int   
)
AS


				
				BEGIN
				DECLARE @PageLowerBound int
				DECLARE @PageUpperBound int
				
				-- Set the page bounds
				SET @PageLowerBound = @PageSize * @PageIndex
				SET @PageUpperBound = @PageLowerBound + @PageSize

				IF (@OrderBy IS NULL OR LEN(@OrderBy) < 1)
				BEGIN
					-- default order by to first column
					SET @OrderBy = '[TerritoryID]'
				END

				-- SQL Server 2005 Paging
				DECLARE @SQL AS nvarchar(MAX)
				SET @SQL = 'WITH PageIndex AS ('
				SET @SQL = @SQL + ' SELECT'
				IF @PageSize > 0
				BEGIN
					SET @SQL = @SQL + ' TOP ' + CONVERT(nvarchar, @PageUpperBound)
				END
				SET @SQL = @SQL + ' ROW_NUMBER() OVER (ORDER BY ' + @OrderBy + ') as RowIndex'
				SET @SQL = @SQL + ', [TerritoryID]'
				SET @SQL = @SQL + ', [Name]'
				SET @SQL = @SQL + ', [CountryRegionCode]'
				SET @SQL = @SQL + ', [Group]'
				SET @SQL = @SQL + ', [SalesYTD]'
				SET @SQL = @SQL + ', [SalesLastYear]'
				SET @SQL = @SQL + ', [CostYTD]'
				SET @SQL = @SQL + ', [CostLastYear]'
				SET @SQL = @SQL + ', [rowguid]'
				SET @SQL = @SQL + ', [ModifiedDate]'
				SET @SQL = @SQL + ' FROM [Sales].[SalesTerritory]'
				IF LEN(@WhereClause) > 0
				BEGIN
					SET @SQL = @SQL + ' WHERE ' + @WhereClause
				END
				SET @SQL = @SQL + ' ) SELECT'
				SET @SQL = @SQL + ' [TerritoryID],'
				SET @SQL = @SQL + ' [Name],'
				SET @SQL = @SQL + ' [CountryRegionCode],'
				SET @SQL = @SQL + ' [Group],'
				SET @SQL = @SQL + ' [SalesYTD],'
				SET @SQL = @SQL + ' [SalesLastYear],'
				SET @SQL = @SQL + ' [CostYTD],'
				SET @SQL = @SQL + ' [CostLastYear],'
				SET @SQL = @SQL + ' [rowguid],'
				SET @SQL = @SQL + ' [ModifiedDate]'
				SET @SQL = @SQL + ' FROM PageIndex'
				SET @SQL = @SQL + ' WHERE RowIndex > ' + CONVERT(nvarchar, @PageLowerBound)
				IF @PageSize > 0
				BEGIN
					SET @SQL = @SQL + ' AND RowIndex <= ' + CONVERT(nvarchar, @PageUpperBound)
				END
				SET @SQL = @SQL + ' ORDER BY ' + @OrderBy
				EXEC sp_executesql @SQL
				
				-- get row count
				SET @SQL = 'SELECT COUNT(*) AS TotalRowCount'
				SET @SQL = @SQL + ' FROM [Sales].[SalesTerritory]'
				IF LEN(@WhereClause) > 0
				BEGIN
					SET @SQL = @SQL + ' WHERE ' + @WhereClause
				END
				EXEC sp_executesql @SQL
			
				END
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO
