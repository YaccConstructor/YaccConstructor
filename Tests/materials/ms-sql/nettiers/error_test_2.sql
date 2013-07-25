
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

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_Insert procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_Insert') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_Insert
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Inserts a record into the SalesTerritory table
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_Insert
(

	@TerritoryId int    OUTPUT,

	@Name nvarchar (50)  ,

	@CountryRegionCode nvarchar (3)  ,

	@Group nvarchar (50)  ,

	@SalesYtd money   ,

	@SalesLastYear money   ,

	@CostYtd money   ,

	@CostLastYear money   ,

	@Rowguid uniqueidentifier    OUTPUT,

	@ModifiedDate datetime   
)
AS


				
				Declare @IdentityRowGuids table (Rowguid uniqueidentifier	)
				INSERT INTO [Sales].[SalesTerritory]
					(
					[Name]
					,[CountryRegionCode]
					,[Group]
					,[SalesYTD]
					,[SalesLastYear]
					,[CostYTD]
					,[CostLastYear]
					,[ModifiedDate]
					)
						OUTPUT INSERTED.rowguid INTO @IdentityRowGuids
					
				VALUES
					(
					@Name
					,@CountryRegionCode
					,@Group
					,@SalesYtd
					,@SalesLastYear
					,@CostYtd
					,@CostLastYear
					,@ModifiedDate
					)
				
				SELECT @Rowguid=Rowguid	 from @IdentityRowGuids
				-- Get the identity value
				SET @TerritoryId = SCOPE_IDENTITY()
									
							
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_Update procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_Update') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_Update
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Updates a record in the SalesTerritory table
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_Update
(

	@TerritoryId int   ,

	@Name nvarchar (50)  ,

	@CountryRegionCode nvarchar (3)  ,

	@Group nvarchar (50)  ,

	@SalesYtd money   ,

	@SalesLastYear money   ,

	@CostYtd money   ,

	@CostLastYear money   ,

	@Rowguid uniqueidentifier   ,

	@ModifiedDate datetime   
)
AS


				
				
				-- Modify the updatable columns
				UPDATE
					[Sales].[SalesTerritory]
				SET
					[Name] = @Name
					,[CountryRegionCode] = @CountryRegionCode
					,[Group] = @Group
					,[SalesYTD] = @SalesYtd
					,[SalesLastYear] = @SalesLastYear
					,[CostYTD] = @CostYtd
					,[CostLastYear] = @CostLastYear
					,[ModifiedDate] = @ModifiedDate
				WHERE
[TerritoryID] = @TerritoryId 
				
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_Delete procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_Delete') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_Delete
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Deletes a record in the SalesTerritory table
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_Delete
(

	@TerritoryId int   
)
AS


				DELETE FROM [Sales].[SalesTerritory] WITH (ROWLOCK) 
				WHERE
					[TerritoryID] = @TerritoryId
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_GetByName procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_GetByName') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByName
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesTerritory table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByName
(

	@Name nvarchar (50)  
)
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
				WHERE
					[Name] = @Name
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_GetByRowguid procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_GetByRowguid') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByRowguid
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesTerritory table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByRowguid
(

	@Rowguid uniqueidentifier   
)
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
				WHERE
					[rowguid] = @Rowguid
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_GetByTerritoryId procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_GetByTerritoryId') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByTerritoryId
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesTerritory table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_GetByTerritoryId
(

	@TerritoryId int   
)
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
				WHERE
					[TerritoryID] = @TerritoryId
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesTerritory_Find procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesTerritory_Find') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesTerritory_Find
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Finds records in the SalesTerritory table passing nullable parameters
-- Table Comment: Sales territory lookup table.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesTerritory_Find
(

	@SearchUsingOR bit   = null ,

	@TerritoryId int   = null ,

	@Name nvarchar (50)  = null ,

	@CountryRegionCode nvarchar (3)  = null ,

	@Group nvarchar (50)  = null ,

	@SalesYtd money   = null ,

	@SalesLastYear money   = null ,

	@CostYtd money   = null ,

	@CostLastYear money   = null ,

	@Rowguid uniqueidentifier   = null ,

	@ModifiedDate datetime   = null 
)
AS


				
  IF ISNULL(@SearchUsingOR, 0) <> 1
  BEGIN
    SELECT
	  [TerritoryID]
	, [Name]
	, [CountryRegionCode]
	, [Group]
	, [SalesYTD]
	, [SalesLastYear]
	, [CostYTD]
	, [CostLastYear]
	, [rowguid]
	, [ModifiedDate]
    FROM
	[Sales].[SalesTerritory]
    WHERE 
	 ([TerritoryID] = @TerritoryId OR @TerritoryId IS NULL)
	AND ([Name] = @Name OR @Name IS NULL)
	AND ([CountryRegionCode] = @CountryRegionCode OR @CountryRegionCode IS NULL)
	AND ([Group] = @Group OR @Group IS NULL)
	AND ([SalesYTD] = @SalesYtd OR @SalesYtd IS NULL)
	AND ([SalesLastYear] = @SalesLastYear OR @SalesLastYear IS NULL)
	AND ([CostYTD] = @CostYtd OR @CostYtd IS NULL)
	AND ([CostLastYear] = @CostLastYear OR @CostLastYear IS NULL)
	AND ([rowguid] = @Rowguid OR @Rowguid IS NULL)
	AND ([ModifiedDate] = @ModifiedDate OR @ModifiedDate IS NULL)
						
  END
  ELSE
  BEGIN
    SELECT
	  [TerritoryID]
	, [Name]
	, [CountryRegionCode]
	, [Group]
	, [SalesYTD]
	, [SalesLastYear]
	, [CostYTD]
	, [CostLastYear]
	, [rowguid]
	, [ModifiedDate]
    FROM
	[Sales].[SalesTerritory]
    WHERE 
	 ([TerritoryID] = @TerritoryId AND @TerritoryId is not null)
	OR ([Name] = @Name AND @Name is not null)
	OR ([CountryRegionCode] = @CountryRegionCode AND @CountryRegionCode is not null)
	OR ([Group] = @Group AND @Group is not null)
	OR ([SalesYTD] = @SalesYtd AND @SalesYtd is not null)
	OR ([SalesLastYear] = @SalesLastYear AND @SalesLastYear is not null)
	OR ([CostYTD] = @CostYtd AND @CostYtd is not null)
	OR ([CostLastYear] = @CostLastYear AND @CostLastYear is not null)
	OR ([rowguid] = @Rowguid AND @Rowguid is not null)
	OR ([ModifiedDate] = @ModifiedDate AND @ModifiedDate is not null)
	SELECT @@ROWCOUNT			
  END
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_Get_List procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_Get_List') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_Get_List
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets all records from the Location table
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_Get_List

AS


				
				SELECT
					[LocationID],
					[Name],
					[CostRate],
					[Availability],
					[ModifiedDate]
				FROM
					[Production].[Location]
					
				SELECT @@ROWCOUNT
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_GetPaged procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_GetPaged') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_GetPaged
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records from the Location table passing page index and page count parameters
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_GetPaged
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
					SET @OrderBy = '[LocationID]'
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
				SET @SQL = @SQL + ', [LocationID]'
				SET @SQL = @SQL + ', [Name]'
				SET @SQL = @SQL + ', [CostRate]'
				SET @SQL = @SQL + ', [Availability]'
				SET @SQL = @SQL + ', [ModifiedDate]'
				SET @SQL = @SQL + ' FROM [Production].[Location]'
				IF LEN(@WhereClause) > 0
				BEGIN
					SET @SQL = @SQL + ' WHERE ' + @WhereClause
				END
				SET @SQL = @SQL + ' ) SELECT'
				SET @SQL = @SQL + ' [LocationID],'
				SET @SQL = @SQL + ' [Name],'
				SET @SQL = @SQL + ' [CostRate],'
				SET @SQL = @SQL + ' [Availability],'
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
				SET @SQL = @SQL + ' FROM [Production].[Location]'
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

	

-- Drop the Production.usp_adwTiers_Location_Insert procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_Insert') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_Insert
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Inserts a record into the Location table
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_Insert
(

	@LocationId smallint    OUTPUT,

	@Name nvarchar (50)  ,

	@CostRate smallmoney   ,

	@Availability decimal (8, 2)  ,

	@ModifiedDate datetime   
)
AS


				
				INSERT INTO [Production].[Location]
					(
					[Name]
					,[CostRate]
					,[Availability]
					,[ModifiedDate]
					)
				VALUES
					(
					@Name
					,@CostRate
					,@Availability
					,@ModifiedDate
					)
				
				-- Get the identity value
				SET @LocationId = SCOPE_IDENTITY()
									
							
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_Update procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_Update') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_Update
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Updates a record in the Location table
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_Update
(

	@LocationId smallint   ,

	@Name nvarchar (50)  ,

	@CostRate smallmoney   ,

	@Availability decimal (8, 2)  ,

	@ModifiedDate datetime   
)
AS


				
				
				-- Modify the updatable columns
				UPDATE
					[Production].[Location]
				SET
					[Name] = @Name
					,[CostRate] = @CostRate
					,[Availability] = @Availability
					,[ModifiedDate] = @ModifiedDate
				WHERE
[LocationID] = @LocationId 
				
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_Delete procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_Delete') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_Delete
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Deletes a record in the Location table
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_Delete
(

	@LocationId smallint   
)
AS


				DELETE FROM [Production].[Location] WITH (ROWLOCK) 
				WHERE
					[LocationID] = @LocationId
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_GetByName procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_GetByName') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_GetByName
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the Location table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_GetByName
(

	@Name nvarchar (50)  
)
AS


				SELECT
					[LocationID],
					[Name],
					[CostRate],
					[Availability],
					[ModifiedDate]
				FROM
					[Production].[Location]
				WHERE
					[Name] = @Name
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_GetByLocationId procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_GetByLocationId') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_GetByLocationId
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the Location table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_GetByLocationId
(

	@LocationId smallint   
)
AS


				SELECT
					[LocationID],
					[Name],
					[CostRate],
					[Availability],
					[ModifiedDate]
				FROM
					[Production].[Location]
				WHERE
					[LocationID] = @LocationId
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_GetByProductIdFromProductInventory procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_GetByProductIdFromProductInventory') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_GetByProductIdFromProductInventory
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records through a junction table
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_GetByProductIdFromProductInventory
(

	@ProductId int   
)
AS


SELECT Production.[Location].[LocationID]
       ,Production.[Location].[Name]
       ,Production.[Location].[CostRate]
       ,Production.[Location].[Availability]
       ,Production.[Location].[ModifiedDate]
  FROM Production.[Location]
 WHERE EXISTS (SELECT 1
                 FROM Production.[ProductInventory] 
                WHERE Production.[ProductInventory].[ProductID] = @ProductId
                  AND Production.[ProductInventory].[LocationID] = Production.[Location].[LocationID]
                  )
				SELECT @@ROWCOUNT			
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Production.usp_adwTiers_Location_Find procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Production.usp_adwTiers_Location_Find') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Production.usp_adwTiers_Location_Find
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Finds records in the Location table passing nullable parameters
-- Table Comment: Product inventory and manufacturing locations.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Production.usp_adwTiers_Location_Find
(

	@SearchUsingOR bit   = null ,

	@LocationId smallint   = null ,

	@Name nvarchar (50)  = null ,

	@CostRate smallmoney   = null ,

	@Availability decimal (8, 2)  = null ,

	@ModifiedDate datetime   = null 
)
AS


				
  IF ISNULL(@SearchUsingOR, 0) <> 1
  BEGIN
    SELECT
	  [LocationID]
	, [Name]
	, [CostRate]
	, [Availability]
	, [ModifiedDate]
    FROM
	[Production].[Location]
    WHERE 
	 ([LocationID] = @LocationId OR @LocationId IS NULL)
	AND ([Name] = @Name OR @Name IS NULL)
	AND ([CostRate] = @CostRate OR @CostRate IS NULL)
	AND ([Availability] = @Availability OR @Availability IS NULL)
	AND ([ModifiedDate] = @ModifiedDate OR @ModifiedDate IS NULL)
						
  END
  ELSE
  BEGIN
    SELECT
	  [LocationID]
	, [Name]
	, [CostRate]
	, [Availability]
	, [ModifiedDate]
    FROM
	[Production].[Location]
    WHERE 
	 ([LocationID] = @LocationId AND @LocationId is not null)
	OR ([Name] = @Name AND @Name is not null)
	OR ([CostRate] = @CostRate AND @CostRate is not null)
	OR ([Availability] = @Availability AND @Availability is not null)
	OR ([ModifiedDate] = @ModifiedDate AND @ModifiedDate is not null)
	SELECT @@ROWCOUNT			
  END
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_Get_List procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_Get_List') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_Get_List
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets all records from the SalesReason table
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_Get_List

AS


				
				SELECT
					[SalesReasonID],
					[Name],
					[ReasonType],
					[ModifiedDate]
				FROM
					[Sales].[SalesReason]
					
				SELECT @@ROWCOUNT
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_GetPaged procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_GetPaged') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_GetPaged
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records from the SalesReason table passing page index and page count parameters
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_GetPaged
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
					SET @OrderBy = '[SalesReasonID]'
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
				SET @SQL = @SQL + ', [SalesReasonID]'
				SET @SQL = @SQL + ', [Name]'
				SET @SQL = @SQL + ', [ReasonType]'
				SET @SQL = @SQL + ', [ModifiedDate]'
				SET @SQL = @SQL + ' FROM [Sales].[SalesReason]'
				IF LEN(@WhereClause) > 0
				BEGIN
					SET @SQL = @SQL + ' WHERE ' + @WhereClause
				END
				SET @SQL = @SQL + ' ) SELECT'
				SET @SQL = @SQL + ' [SalesReasonID],'
				SET @SQL = @SQL + ' [Name],'
				SET @SQL = @SQL + ' [ReasonType],'
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
				SET @SQL = @SQL + ' FROM [Sales].[SalesReason]'
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

	

-- Drop the Sales.usp_adwTiers_SalesReason_Insert procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_Insert') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_Insert
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Inserts a record into the SalesReason table
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_Insert
(

	@SalesReasonId int    OUTPUT,

	@Name nvarchar (50)  ,

	@ReasonType nvarchar (50)  ,

	@ModifiedDate datetime   
)
AS


				
				INSERT INTO [Sales].[SalesReason]
					(
					[Name]
					,[ReasonType]
					,[ModifiedDate]
					)
				VALUES
					(
					@Name
					,@ReasonType
					,@ModifiedDate
					)
				
				-- Get the identity value
				SET @SalesReasonId = SCOPE_IDENTITY()
									
							
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_Update procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_Update') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_Update
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Updates a record in the SalesReason table
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_Update
(

	@SalesReasonId int   ,

	@Name nvarchar (50)  ,

	@ReasonType nvarchar (50)  ,

	@ModifiedDate datetime   
)
AS


				
				
				-- Modify the updatable columns
				UPDATE
					[Sales].[SalesReason]
				SET
					[Name] = @Name
					,[ReasonType] = @ReasonType
					,[ModifiedDate] = @ModifiedDate
				WHERE
[SalesReasonID] = @SalesReasonId 
				
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_Delete procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_Delete') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_Delete
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Deletes a record in the SalesReason table
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_Delete
(

	@SalesReasonId int   
)
AS


				DELETE FROM [Sales].[SalesReason] WITH (ROWLOCK) 
				WHERE
					[SalesReasonID] = @SalesReasonId
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_GetBySalesReasonId procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_GetBySalesReasonId') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_GetBySalesReasonId
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesReason table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_GetBySalesReasonId
(

	@SalesReasonId int   
)
AS


				SELECT
					[SalesReasonID],
					[Name],
					[ReasonType],
					[ModifiedDate]
				FROM
					[Sales].[SalesReason]
				WHERE
					[SalesReasonID] = @SalesReasonId
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_GetBySalesOrderIdFromSalesOrderHeaderSalesReason procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_GetBySalesOrderIdFromSalesOrderHeaderSalesReason') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_GetBySalesOrderIdFromSalesOrderHeaderSalesReason
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records through a junction table
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_GetBySalesOrderIdFromSalesOrderHeaderSalesReason
(

	@SalesOrderId int   
)
AS


SELECT Sales.[SalesReason].[SalesReasonID]
       ,Sales.[SalesReason].[Name]
       ,Sales.[SalesReason].[ReasonType]
       ,Sales.[SalesReason].[ModifiedDate]
  FROM Sales.[SalesReason]
 WHERE EXISTS (SELECT 1
                 FROM Sales.[SalesOrderHeaderSalesReason] 
                WHERE Sales.[SalesOrderHeaderSalesReason].[SalesOrderID] = @SalesOrderId
                  AND Sales.[SalesOrderHeaderSalesReason].[SalesReasonID] = Sales.[SalesReason].[SalesReasonID]
                  )
				SELECT @@ROWCOUNT			
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesReason_Find procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesReason_Find') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesReason_Find
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Finds records in the SalesReason table passing nullable parameters
-- Table Comment: Lookup table of customer purchase reasons.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesReason_Find
(

	@SearchUsingOR bit   = null ,

	@SalesReasonId int   = null ,

	@Name nvarchar (50)  = null ,

	@ReasonType nvarchar (50)  = null ,

	@ModifiedDate datetime   = null 
)
AS


				
  IF ISNULL(@SearchUsingOR, 0) <> 1
  BEGIN
    SELECT
	  [SalesReasonID]
	, [Name]
	, [ReasonType]
	, [ModifiedDate]
    FROM
	[Sales].[SalesReason]
    WHERE 
	 ([SalesReasonID] = @SalesReasonId OR @SalesReasonId IS NULL)
	AND ([Name] = @Name OR @Name IS NULL)
	AND ([ReasonType] = @ReasonType OR @ReasonType IS NULL)
	AND ([ModifiedDate] = @ModifiedDate OR @ModifiedDate IS NULL)
						
  END
  ELSE
  BEGIN
    SELECT
	  [SalesReasonID]
	, [Name]
	, [ReasonType]
	, [ModifiedDate]
    FROM
	[Sales].[SalesReason]
    WHERE 
	 ([SalesReasonID] = @SalesReasonId AND @SalesReasonId is not null)
	OR ([Name] = @Name AND @Name is not null)
	OR ([ReasonType] = @ReasonType AND @ReasonType is not null)
	OR ([ModifiedDate] = @ModifiedDate AND @ModifiedDate is not null)
	SELECT @@ROWCOUNT			
  END
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_Get_List procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_Get_List') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Get_List
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets all records from the SalesPersonQuotaHistory table
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Get_List

AS


				
				SELECT
					[SalesPersonID],
					[QuotaDate],
					[SalesQuota],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesPersonQuotaHistory]
					
				SELECT @@ROWCOUNT
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_GetPaged procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_GetPaged') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetPaged
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records from the SalesPersonQuotaHistory table passing page index and page count parameters
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetPaged
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
					SET @OrderBy = '[SalesPersonID]'
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
				SET @SQL = @SQL + ', [SalesPersonID]'
				SET @SQL = @SQL + ', [QuotaDate]'
				SET @SQL = @SQL + ', [SalesQuota]'
				SET @SQL = @SQL + ', [rowguid]'
				SET @SQL = @SQL + ', [ModifiedDate]'
				SET @SQL = @SQL + ' FROM [Sales].[SalesPersonQuotaHistory]'
				IF LEN(@WhereClause) > 0
				BEGIN
					SET @SQL = @SQL + ' WHERE ' + @WhereClause
				END
				SET @SQL = @SQL + ' ) SELECT'
				SET @SQL = @SQL + ' [SalesPersonID],'
				SET @SQL = @SQL + ' [QuotaDate],'
				SET @SQL = @SQL + ' [SalesQuota],'
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
				SET @SQL = @SQL + ' FROM [Sales].[SalesPersonQuotaHistory]'
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

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_Insert procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_Insert') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Insert
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Inserts a record into the SalesPersonQuotaHistory table
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Insert
(

	@SalesPersonId int   ,

	@QuotaDate datetime   ,

	@SalesQuota money   ,

	@Rowguid uniqueidentifier    OUTPUT,

	@ModifiedDate datetime   
)
AS


				
				Declare @IdentityRowGuids table (Rowguid uniqueidentifier	)
				INSERT INTO [Sales].[SalesPersonQuotaHistory]
					(
					[SalesPersonID]
					,[QuotaDate]
					,[SalesQuota]
					,[ModifiedDate]
					)
						OUTPUT INSERTED.rowguid INTO @IdentityRowGuids
					
				VALUES
					(
					@SalesPersonId
					,@QuotaDate
					,@SalesQuota
					,@ModifiedDate
					)
				
				SELECT @Rowguid=Rowguid	 from @IdentityRowGuids
									
							
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_Update procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_Update') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Update
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Updates a record in the SalesPersonQuotaHistory table
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Update
(

	@SalesPersonId int   ,

	@OriginalSalesPersonId int   ,

	@QuotaDate datetime   ,

	@OriginalQuotaDate datetime   ,

	@SalesQuota money   ,

	@Rowguid uniqueidentifier   ,

	@ModifiedDate datetime   
)
AS


				
				
				-- Modify the updatable columns
				UPDATE
					[Sales].[SalesPersonQuotaHistory]
				SET
					[SalesPersonID] = @SalesPersonId
					,[QuotaDate] = @QuotaDate
					,[SalesQuota] = @SalesQuota
					,[ModifiedDate] = @ModifiedDate
				WHERE
[SalesPersonID] = @OriginalSalesPersonId 
AND [QuotaDate] = @OriginalQuotaDate 
				
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_Delete procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_Delete') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Delete
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Deletes a record in the SalesPersonQuotaHistory table
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Delete
(

	@SalesPersonId int   ,

	@QuotaDate datetime   
)
AS


				DELETE FROM [Sales].[SalesPersonQuotaHistory] WITH (ROWLOCK) 
				WHERE
					[SalesPersonID] = @SalesPersonId
					AND [QuotaDate] = @QuotaDate
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonId procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonId') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonId
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesPersonQuotaHistory table through a foreign key
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonId
(

	@SalesPersonId int   
)
AS


				SET ANSI_NULLS OFF
				
				SELECT
					[SalesPersonID],
					[QuotaDate],
					[SalesQuota],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesPersonQuotaHistory]
				WHERE
					[SalesPersonID] = @SalesPersonId
				
				SELECT @@ROWCOUNT
				SET ANSI_NULLS ON
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_GetByRowguid procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_GetByRowguid') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetByRowguid
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesPersonQuotaHistory table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetByRowguid
(

	@Rowguid uniqueidentifier   
)
AS


				SELECT
					[SalesPersonID],
					[QuotaDate],
					[SalesQuota],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesPersonQuotaHistory]
				WHERE
					[rowguid] = @Rowguid
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonIdQuotaDate procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonIdQuotaDate') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonIdQuotaDate
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesPersonQuotaHistory table through an index
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_GetBySalesPersonIdQuotaDate
(

	@SalesPersonId int   ,

	@QuotaDate datetime   
)
AS


				SELECT
					[SalesPersonID],
					[QuotaDate],
					[SalesQuota],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesPersonQuotaHistory]
				WHERE
					[SalesPersonID] = @SalesPersonId
					AND [QuotaDate] = @QuotaDate
				SELECT @@ROWCOUNT
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesPersonQuotaHistory_Find procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesPersonQuotaHistory_Find') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Find
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Finds records in the SalesPersonQuotaHistory table passing nullable parameters
-- Table Comment: Sales performance tracking.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesPersonQuotaHistory_Find
(

	@SearchUsingOR bit   = null ,

	@SalesPersonId int   = null ,

	@QuotaDate datetime   = null ,

	@SalesQuota money   = null ,

	@Rowguid uniqueidentifier   = null ,

	@ModifiedDate datetime   = null 
)
AS


				
  IF ISNULL(@SearchUsingOR, 0)  1
  BEGIN
    SELECT
	  [SalesPersonID]
	, [QuotaDate]
	, [SalesQuota]
	, [rowguid]
	, [ModifiedDate]
    FROM
	[Sales].[SalesPersonQuotaHistory]
    WHERE 
	 ([SalesPersonID] = @SalesPersonId OR @SalesPersonId IS NULL)
	AND ([QuotaDate] = @QuotaDate OR @QuotaDate IS NULL)
	AND ([SalesQuota] = @SalesQuota OR @SalesQuota IS NULL)
	AND ([rowguid] = @Rowguid OR @Rowguid IS NULL)
	AND ([ModifiedDate] = @ModifiedDate OR @ModifiedDate IS NULL)
						
  END
  ELSE
  BEGIN
    SELECT
	  [SalesPersonID]
	, [QuotaDate]
	, [SalesQuota]
	, [rowguid]
	, [ModifiedDate]
    FROM
	[Sales].[SalesPersonQuotaHistory]
    WHERE 
	OR ([QuotaDate] = @QuotaDate AND @QuotaDate is not null)
	OR ([SalesQuota] = @SalesQuota AND @SalesQuota is not null)
	OR ([rowguid] = @Rowguid AND @Rowguid is not null)
	OR ([ModifiedDate] = @ModifiedDate AND @ModifiedDate is not null)
	SELECT @@ROWCOUNT			
  END
				

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_Get_List procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_Get_List') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Get_List
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets all records from the SalesOrderHeader table
-- Table Comment: General sales order information.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Get_List

AS


				
				SELECT
					[SalesOrderID],
					[RevisionNumber],
					[OrderDate],
					[DueDate],
					[ShipDate],
					[Status],
					[OnlineOrderFlag],
					[SalesOrderNumber],
					[PurchaseOrderNumber],
					[AccountNumber],
					[CustomerID],
					[ContactID],
					[SalesPersonID],
					[TerritoryID],
					[BillToAddressID],
					[ShipToAddressID],
					[ShipMethodID],
					[CreditCardID],
					[CreditCardApprovalCode],
					[CurrencyRateID],
					[SubTotal],
					[TaxAmt],
					[Freight],
					[TotalDue],
					[Comment],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesOrderHeader]
					
				SELECT @@ROWCOUNT
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_GetPaged procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_GetPaged') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_GetPaged
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Gets records from the SalesOrderHeader table passing page index and page count parameters
-- Table Comment: General sales order information.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_GetPaged
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
					SET @OrderBy = '[SalesOrderID]'
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
				SET @SQL = @SQL + ', [SalesOrderID]'
				SET @SQL = @SQL + ', [RevisionNumber]'
				SET @SQL = @SQL + ', [OrderDate]'
				SET @SQL = @SQL + ', [DueDate]'
				SET @SQL = @SQL + ', [ShipDate]'
				SET @SQL = @SQL + ', [Status]'
				SET @SQL = @SQL + ', [OnlineOrderFlag]'
				SET @SQL = @SQL + ', [SalesOrderNumber]'
				SET @SQL = @SQL + ', [PurchaseOrderNumber]'
				SET @SQL = @SQL + ', [AccountNumber]'
				SET @SQL = @SQL + ', [CustomerID]'
				SET @SQL = @SQL + ', [ContactID]'
				SET @SQL = @SQL + ', [SalesPersonID]'
				SET @SQL = @SQL + ', [TerritoryID]'
				SET @SQL = @SQL + ', [BillToAddressID]'
				SET @SQL = @SQL + ', [ShipToAddressID]'
				SET @SQL = @SQL + ', [ShipMethodID]'
				SET @SQL = @SQL + ', [CreditCardID]'
				SET @SQL = @SQL + ', [CreditCardApprovalCode]'
				SET @SQL = @SQL + ', [CurrencyRateID]'
				SET @SQL = @SQL + ', [SubTotal]'
				SET @SQL = @SQL + ', [TaxAmt]'
				SET @SQL = @SQL + ', [Freight]'
				SET @SQL = @SQL + ', [TotalDue]'
				SET @SQL = @SQL + ', [Comment]'
				SET @SQL = @SQL + ', [rowguid]'
				SET @SQL = @SQL + ', [ModifiedDate]'
				SET @SQL = @SQL + ' FROM [Sales].[SalesOrderHeader]'
				IF LEN(@WhereClause) > 0
				BEGIN
					
				END
				SET @SQL = @SQL + ' ) SELECT'
				SET @SQL = @SQL + ' [SalesOrderID],'
				SET @SQL = @SQL + ' [RevisionNumber],'
				SET @SQL = @SQL + ' [OrderDate],'
				SET @SQL = @SQL + ' [DueDate],'
				SET @SQL = @SQL + ' [ShipDate],'
				SET @SQL = @SQL + ' [Status],'
				SET @SQL = @SQL + ' [OnlineOrderFlag],'
				SET @SQL = @SQL + ' [SalesOrderNumber],'
				SET @SQL = @SQL + ' [PurchaseOrderNumber],'
				SET @SQL = @SQL + ' [AccountNumber],'
				SET @SQL = @SQL + ' [CustomerID],'
				SET @SQL = @SQL + ' [ContactID],'
				SET @SQL = @SQL + ' [SalesPersonID],'
				SET @SQL = @SQL + ' [TerritoryID],'
				SET @SQL = @SQL + ' [BillToAddressID],'
				SET @SQL = @SQL + ' [ShipToAddressID],'
				SET @SQL = @SQL + ' [ShipMethodID],'
				SET @SQL = @SQL + ' [CreditCardID],'
				SET @SQL = @SQL + ' [CreditCardApprovalCode],'
				SET @SQL = @SQL + ' [CurrencyRateID],'
				SET @SQL = @SQL + ' [SubTotal],'
				SET @SQL = @SQL + ' [TaxAmt],'
				SET @SQL = @SQL + ' [Freight],'
				SET @SQL = @SQL + ' [TotalDue],'
				SET @SQL = @SQL + ' [Comment],'
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
				SET @SQL = @SQL + ' FROM [Sales].[SalesOrderHeader]'
				IF LEN(@WhereClause) > 0
				BEGIN
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

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_Insert procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_Insert') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Insert
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Inserts a record into the SalesOrderHeader table
-- Table Comment: General sales order information.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Insert
(

	@SalesOrderId int    OUTPUT,

	@RevisionNumber tinyint   ,

	@OrderDate datetime   ,

	@DueDate datetime   ,

	@ShipDate datetime   ,

	@Status tinyint   ,

	@OnlineOrderFlag bit   ,

	@SalesOrderNumber nvarchar (25)   OUTPUT,

	@PurchaseOrderNumber nvarchar (25)  ,

	@AccountNumber nvarchar (15)  ,

	@CustomerId int   ,

	@ContactId int   ,

	@SalesPersonId int   ,

	@TerritoryId int   ,

	@BillToAddressId int   ,

	@ShipToAddressId int   ,

	@ShipMethodId int   ,

	@CreditCardId int   ,

	@CreditCardApprovalCode varchar (15)  ,

	@CurrencyRateId int   ,

	@SubTotal money   ,

	@TaxAmt money   ,

	@Freight money   ,

	@TotalDue money    OUTPUT,

	@Comment nvarchar (128)  ,

	@Rowguid uniqueidentifier    OUTPUT,

	@ModifiedDate datetime   
)
AS


				
				Declare @IdentityRowGuids table (Rowguid uniqueidentifier	)
				INSERT INTO [Sales].[SalesOrderHeader]
					(
					[RevisionNumber]
					,[OrderDate]
					,[DueDate]
					,[ShipDate]
					,[Status]
					,[OnlineOrderFlag]
					,[PurchaseOrderNumber]
					,[AccountNumber]
					,[CustomerID]
					,[ContactID]
					,[SalesPersonID]
					,[TerritoryID]
					,[BillToAddressID]
					,[ShipToAddressID]
					,[ShipMethodID]
					,[CreditCardID]
					,[CreditCardApprovalCode]
					,[CurrencyRateID]
					,[SubTotal]
					,[TaxAmt]
					,[Freight]
					,[Comment]
					,[ModifiedDate]
					)
						OUTPUT INSERTED.rowguid INTO @IdentityRowGuids
					
				VALUES
					(
					@RevisionNumber
					,@OrderDate
					,@DueDate
					,@ShipDate
					,@Status
					,@OnlineOrderFlag
					,@PurchaseOrderNumber
					,@AccountNumber
					,@CustomerId
					,@ContactId
					,@SalesPersonId
					,@TerritoryId
					,@BillToAddressId
					,@ShipToAddressId
					,@ShipMethodId
					,@CreditCardId
					,@CreditCardApprovalCode
					,@CurrencyRateId
					,@SubTotal
					,@TaxAmt
					,@Freight
					,@Comment
					,@ModifiedDate
					)
				
				SELECT @Rowguid=Rowguid	 from @IdentityRowGuids
				-- Get the identity value
				SET @SalesOrderId = SCOPE_IDENTITY()
									
				-- Select computed columns into output parameters
				SELECT
 @SalesOrderNumber = [SalesOrderNumber]
, @TotalDue = [TotalDue]
				FROM
					[Sales].[SalesOrderHeader]
				WHERE
[SalesOrderID] = @SalesOrderId 
							
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_Update procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_Update') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Update
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Updates a record in the SalesOrderHeader table
-- Table Comment: General sales order information.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Update
(

	@SalesOrderId int   ,

	@RevisionNumber tinyint   ,

	@OrderDate datetime   ,

	@DueDate datetime   ,

	@ShipDate datetime   ,

	@Status tinyint 

	@OnlineOrderFlag bit   ,

	@SalesOrderNumber nvarchar (25)   OUTPUT,

	@PurchaseOrderNumber nvarchar (25)  ,

	@AccountNumber nvarchar (15)  ,

	@CustomerId ,

	@ContactId int   ,

	@SalesPersonId int   ,

	@TerritoryId int   ,

	@BillToAddressId int   ,

	@ShipToAddressId int   ,

	@ShipMethodId int   ,

	@CreditCardId int   ,

	@CreditCardApprovalCode varchar (15)  ,

	@CurrencyRateId int   ,

	@SubTotal money   ,

	@TaxAmt money   ,

	@Freight money   ,

	@TotalDue money    OUTPUT,

	@Comment nvarchar (128)  ,

	@Rowguid uniqueidentifier   ,

	@ModifiedDate datetime   
)
AS


				
				
				-- Modify the updatable columns
				UPDATE
					[Sales].[SalesOrderHeader]
				SET
					[RevisionNumber] = @RevisionNumber
					,[OrderDate] = @OrderDate
					,[DueDate] = @DueDate
					,[ShipDate] = @ShipDate
					,[Status] = @Status
					,[OnlineOrderFlag] = @OnlineOrderFlag
					,[PurchaseOrderNumber] = @PurchaseOrderNumber
					,[AccountNumber] = @AccountNumber
					,[CustomerID] = @CustomerId
					,[ContactID] = @ContactId
					,[SalesPersonID] = @SalesPersonId
					,[TerritoryID] = @TerritoryId
					,[BillToAddressID] = @BillToAddressId
					,[ShipToAddressID] = @ShipToAddressId
					,[ShipMethodID] = @ShipMethodId
					,[CreditCardID] = @CreditCardId
					,[CreditCardApprovalCode] = @CreditCardApprovalCode
					,[CurrencyRateID] = @CurrencyRateId
					,[SubTotal] = @SubTotal
					,[TaxAmt] = @TaxAmt
					,[Freight] = @Freight
					,[Comment] = @Comment
					,[ModifiedDate] = @ModifiedDate
				WHERE
[SalesOrderID] = @SalesOrderId 
				
				
				-- Select computed columns into output parameters
				SELECT
 @SalesOrderNumber = [SalesOrderNumber]
, @TotalDue = [TotalDue]
				FROM
					[Sales].[SalesOrderHeader]
				WHERE
[SalesOrderID] = @SalesOrderId 
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_Delete procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_Delete') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Delete
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Deletes a record in the SalesOrderHeader table
-- Table Comment: General sales order information.
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_Delete
(

	@SalesOrderId int   
)
AS


				DELETE FROM [Sales].[SalesOrderHeader] WITH (ROWLOCK) 
				WHERE
					[SalesOrderID] = @SalesOrderId
					
			

GO
SET QUOTED_IDENTIFIER ON 
GO
SET NOCOUNT ON
GO
SET ANSI_NULLS OFF 
GO

	

-- Drop the Sales.usp_adwTiers_SalesOrderHeader_GetByBillToAddressId procedure
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'Sales.usp_adwTiers_SalesOrderHeader_GetByBillToAddressId') AND OBJECTPROPERTY(id, N'IsProcedure') = 1)
DROP PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_GetByBillToAddressId
GO

/*
----------------------------------------------------------------------------------------------------

-- Created By: Nettiers Sample (http://www.nettiers.com)
-- Purpose: Select records from the SalesOrderHeader table through a foreign key
----------------------------------------------------------------------------------------------------
*/


CREATE PROCEDURE Sales.usp_adwTiers_SalesOrderHeader_GetByBillToAddressId
(

	@BillToAddressId int   
)
AS


				SET ANSI_NULLS OFF
				
					[SalesOrderID],
					[RevisionNumber],
					[OrderDate],
					[DueDate],
					[ShipDate],
					[Status],
					[OnlineOrderFlag],
					[SalesOrderNumber],
					[PurchaseOrderNumber],
					[AccountNumber],
					[CustomerID],
					[ContactID],
					[SalesPersonID],
					[TerritoryID],
					[BillToAddressID],
					[ShipToAddressID],
					[ShipMethodID],
					[CreditCardID],
					[CreditCardApprovalCode],
					[CurrencyRateID],
					[SubTotal],
					[TaxAmt],
					[Freight],
					[TotalDue],
					[Comment],
					[rowguid],
					[ModifiedDate]
				FROM
					[Sales].[SalesOrderHeader]
				WHERE
					[BillToAddressID] = @BillToAddressId
				
				SELECT @@ROWCOUNT
				SET ANSI_NULLS ON
			

GO
SET QUOTED_IDENTIFIER ON 
GO