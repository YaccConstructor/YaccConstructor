USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASLoadFromTraceTable]    Script Date: 12/23/2008 14:57:01 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[ssas].[SSASLoadFromTraceTable]') AND type in (N'P', N'PC'))
DROP PROCEDURE [ssas].[SSASLoadFromTraceTable]
GO

USE [mdw]
GO

/****** Object:  StoredProcedure [ssas].[SSASLoadFromTraceTable]    Script Date: 12/23/2008 14:57:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [ssas].[SSASLoadFromTraceTable]
	@TraceTableDB nvarchar(128) = 'mdw'
	, @TraceTableSchema nvarchar(128) = 'dbo'
	, @TraceTableName nvarchar(128) = 'as_tracedata'
	, @debug bit = 0
AS
BEGIN
DECLARE @SourceTable NVARCHAR(MAX)
DECLARE @Sql_Query_MDXQueryEvents NVARCHAR(MAX)
DECLARE @Sql_Query_ProcessingEvents NVARCHAR(MAX)
DECLARE @BatchID INT
INSERT INTO [SSAS].[SSASBatch] (Loaded) VALUES (GETDATE())
SELECT @BatchID = SCOPE_IDENTITY()
SET @SourceTable = @TraceTableDB + '.' + @TraceTableSchema + '.' + @TraceTableName

--DECLARE @offset varchar(12)
--DECLARE @offset1 varchar(12) = '-07:00'
--DECLARE @offset2 varchar(12) = '-08:00'
DECLARE @tz int 
SELECT @tz = DATEPART(TZOffset, SYSDATETIMEOFFSET() )
--IF @tz = -420
--SET @offset = @offset1
--IF @tz = -480
--set @offset = @offset2

SET @Sql_Query_MDXQueryEvents = 
	'INSERT INTO [SSAS].[SSASWorkTracetable]
           ([LogTableRowNumber]
           ,[EventClass]
           ,[ConnectionID]
           ,[EventSubclass]
           ,[JobID]
           ,[NTCanonicalUserName]
           ,[WindowsDomainName]
           ,[WindowsUserName]
           ,[ObjectID]
           ,[ObjectName]
           ,[ObjectPath]
           ,[ObjectType]
           ,[SPID]
           ,[SessionID]
           ,[StartTime]
		   ,[StartTime_WithGMTOffset]
           ,[TextData]
           ,[QueryString]
           ,[QueryDataLength]
           ,[QueryCheckSum]
           ,[CPUTime]
           ,[Duration]
           ,[EndTime]
		   ,[EndTime_WithGMTOffset]
           ,[Error]
           ,[ApplicationName]
           ,[Success]
           ,[BinaryData]
           ,[Databasename]
           ,[ServerName]
           ,[CubeName]
           ,[MeasureGroupName]
           ,[PartitionName]
           ,[AggregationName]
           ,[SubcubeName]
           ,[P_BatchID]
)
	SELECT [RowNumber]
           ,[EventClass]
           ,[ConnectionID]
           ,[EventSubclass]
           ,[JobID]
           ,[NTCanonicalUserName]
           ,SUBSTRING(NTCanonicalUserName,1, CHARINDEX(''\'', NTCanonicalUserName, 0)-1) as WindowsDomainName
           ,SUBSTRING(NTCanonicalUserName,CHARINDEX(''\'', NTCanonicalUserName, 0)+1,128) as WindowsUserName
           ,[ObjectID]
           ,[ObjectName]
           ,[ObjectPath]
           ,[ObjectType]
           ,[SPID]
           ,[SessionID]
           ,[StartTime]
           ,TODATETIMEOFFSET(StartTime,@tz) as StartTime_WithGMTOffset
           ,[TextData]
           ,CASE WHEN EventClass = ''Query End'' AND EventSubclass = ''MDXQuery'' THEN CAST(TextData AS nvarchar(MAX)) END 
           ,CASE WHEN EventClass = ''Query End'' AND EventSubclass = ''MDXQuery'' THEN DATALENGTH(CAST(TextData AS nvarchar(MAX))) END
	   ,CASE WHEN EventClass = ''Query End'' AND EventSubclass = ''MDXQuery'' THEN CHECKSUM(CAST(TextData AS nvarchar(MAX))) END 
           ,[CPUTime]
           ,[Duration]
           ,[EndTime]
	   , TODATETIMEOFFSET(EndTime,@tz) as EndTime_WithGMTOffset
           ,[Error]
           ,[ApplicationName]
           ,[Success]
           ,[BinaryData]
           ,[Databasename]
           ,[ServerName]
		   ,CASE WHEN ObjectPath = ''''
				THEN ''''
			ELSE 
            SUBSTRING 
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1
				, CASE 
					WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1) = 0 
					THEN LEN(ObjectPath)
					ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)
					END
						-
						CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)-1
				)
				 
				 END
				 as CubeName
           ,(SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1
				, CASE 
					WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1) = 0 
					THEN LEN(ObjectPath)+1 
					ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)
					END	
						-
						(CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1) 
				) 
				)as MeasureGroupName
          ,CASE WHEN ObjectType IN (100021, 100031) THEN 
				(SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1
				, CASE 
					WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1) = 0 
					THEN LEN(ObjectPath)+1 
					ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1)
					
						-
						(CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1) 
				END) 
				)
				ELSE NULL 
				END 
				AS PartitionName
			,CASE WHEN ObjectType = 100031 THEN 
				(SELECT SUBSTRING
					(
					ObjectPath
					, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1)+1
					, CASE 
						WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1)+1) = 0 
						THEN LEN(ObjectPath)+1
						ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1)
						
							-
							CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)+1) 
						END
					) 
				)ELSE NULL END AS AggregationName				,CASE WHEN EventClass = ''Query Subcube'' THEN CAST(TextData AS NVARCHAR(2000)) END AS SubCubeName
			, ''<BatchID>'' 
	FROM <SourceTable>
	WHERE DatabaseName IS NOT NULL and SessionID in (
  Select DISTINCT SessionID from <SourceTable> 
  where EventClass = ''Query End'' and EventSubclass = ''MDXQuery'') 
  
 ORDER BY SPID, sessionid, EndTime '


	
	
	SET @Sql_Query_MDXQueryEvents = REPLACE(@Sql_Query_MDXQueryEvents, '<SourceTable>', @SourceTable)
	SET @Sql_Query_MDXQueryEvents = REPLACE(@Sql_Query_MDXQueryEvents, '<BatchID>', @BatchID)
	SET @Sql_Query_MDXQueryEvents = REPLACE(@Sql_Query_MDXQueryEvents, '@tz', @tz)

	
   IF @debug = 1 
	        PRINT @Sql_Query_MDXQueryEvents
	ELSE        
	EXEC sp_executeSql @Sql_Query_MDXQueryEvents


SET @Sql_Query_ProcessingEvents = 
	'INSERT INTO [SSAS].[SSASProcessingArchive]
           ([EventClass]
           ,[ConnectionID]
           ,[EventSubclass]
           ,[JobID]
           ,[NTCanonicalUserName]
           ,[ObjectID]
           ,[ObjectName]
           ,[ObjectPath]
           ,[ObjectType]
           ,[SPID]
           ,[SessionID]
           ,[StartTime]
           ,[StartTime_WithGMTOffset]
           ,[TextData]
           ,[CPUTime]
           ,[Duration]
           ,[EndTime]
	   ,[EndTime_WithGMTOffset]
           ,[Error]
           ,[Success]
           ,[ApplicationName]
           ,[BinaryData]
           ,[ServerName]
           ,[DatabaseName]
           ,[DimensionName]
           ,[AttributeName]
           ,[CubeName]
           ,[MeasureGroupName]
           ,[PartitionName]
           ,[LogTableRowNumber]
           ,[P_BatchID])

SELECT  
		 EventClass
		, ConnectionID
		, EventSubclass
		, JobID
		, NTCanonicalUserName
		, ObjectID
		, ObjectName
		, ObjectPath
		, ObjectType
		, SPID
		, SessionID
  	        ,[StartTime]
	 	, TODATETIMEOFFSET(StartTime,@tz)  as StartTime_WithGMTOffset
  		, TextData
		, CPUTime
		, Duration
  	        , [EndTime]
           	, TODATETIMEOFFSET(EndTime,@tz) as EndTime_WithGMTOffset
  		, Error
		, Success
		, ApplicationName
		, BinaryData
		, (SELECT SUBSTRING(	ObjectPath, 1, CHARINDEX(''.'', ObjectPath, 0)-1)) AS ServerName
		, (SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, 0)+1
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)-CHARINDEX(''.'', ObjectPath, 0)-1
				) 
			) AS DatabaseName
		, CASE WHEN ObjectType IN (100006, 100007, 100008) THEN 
			(SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1
				, CASE WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1) = 0 THEN LEN(ObjectPath) ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)-CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)-1 END
				) 
			)
			ELSE NULL END AS DimensionName
		, CASE WHEN ObjectType IN (100007, 100008) THEN ObjectID END AS AttributeName
		, CASE WHEN ObjectType IN (100016, 100021) THEN 
			(SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1
				, CASE WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1) = 0 THEN LEN(ObjectPath) ELSE CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)-CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)-1 END
				) 
			)
			ELSE NULL END AS CubeName
		, CASE WHEN ObjectType IN (100016, 100021) THEN 
			(SELECT SUBSTRING
				(
				ObjectPath
				, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1
				, CASE 
					WHEN CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1) = 0 
					THEN LEN(ObjectPath)+1
					ELSE (CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1))
					END 
					- 
					(CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, CHARINDEX(''.'', ObjectPath, 0)+1)+1)+1)
				) 
			)
			ELSE NULL END AS MeasureGroupName
		, CASE WHEN ObjectType IN (100021) THEN ObjectID END AS PartitionName
		, RowNumber
		, ''<BatchID>'' 
	FROM <SourceTable>
	WHERE SessionID IS NOT NULL	
	AND EventClass = ''Progress Report End'' and EventSubClass != ''Query''
	AND ObjectPath is not null and rtrim(ObjectPath) != ''''
	AND (
	ObjectType IN (100006, 100007, 100008)
	OR
	ObjectType IN (100016,100021)
	)  ORDER BY SPID, sessionid, EndTime	'
	
	SET @Sql_Query_ProcessingEvents = REPLACE(@Sql_Query_ProcessingEvents, '<SourceTable>', @SourceTable)
	SET @Sql_Query_ProcessingEvents = REPLACE(@Sql_Query_ProcessingEvents, '<BatchID>', @BatchID)
	SET @Sql_Query_ProcessingEvents = REPLACE(@Sql_Query_ProcessingEvents, '@tz', @tz)


IF @debug = 1 

	        PRINT @Sql_Query_ProcessingEvents
ELSE
		EXEC (@Sql_Query_ProcessingEvents)

END	

--exec [SSAS].[SSASLoadFromTraceTable]

GO


