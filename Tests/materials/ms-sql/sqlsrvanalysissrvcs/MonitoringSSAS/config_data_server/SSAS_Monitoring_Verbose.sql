Begin Transaction
Begin Try
Declare @collection_set_id_63 int
Declare @collection_set_uid_64 uniqueidentifier
EXEC [msdb].[dbo].[sp_syscollector_create_collection_set] @name=N'SSAS_Monitoring_Verbose', @collection_mode=0, @description=N'Collects SQL Server 2008 Analysis Services querying counters', @logging_level=1, @days_until_expiration=14, @schedule_name=N'CollectorSchedule_Every_15min', @collection_set_id=@collection_set_id_63 OUTPUT, @collection_set_uid=@collection_set_uid_64 OUTPUT
Select @collection_set_id_63, @collection_set_uid_64

Declare @collector_type_uid_65 uniqueidentifier
Select @collector_type_uid_65 = collector_type_uid From [msdb].[dbo].[syscollector_collector_types] Where name = N'Performance Counters Collector Type';
Declare @collection_item_id_66 int
EXEC [msdb].[dbo].[sp_syscollector_create_collection_item] @name=N'SSAS_Monitoring_Verbose', 
@parameters=N'<ns:PerformanceCountersCollector xmlns:ns="DataCollectorType">
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Current Entries" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Direct hit ratio" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="KB added/sec" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total direct hits" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total evictions" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total filtered iterator cache hits" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total filtered iterator cache misses" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total inserts" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total lookups" />
<PerformanceCounters Objects="MSAS 2008:Cache" Counters="Total misses" />

<PerformanceCounters Objects="MSAS 2008:Connection" Counters="Successes/sec" />
<PerformanceCounters Objects="MSAS 2008:Connection" Counters="Total failures" />
<PerformanceCounters Objects="MSAS 2008:Connection" Counters="Total requests" />
<PerformanceCounters Objects="MSAS 2008:Connection" Counters="Total successes" />

<PerformanceCounters Objects="MSAS 2008:Locks" Counters="Current locks" />
<PerformanceCounters Objects="MSAS 2008:Locks" Counters="Lock denials/sec" />
<PerformanceCounters Objects="MSAS 2008:Locks" Counters="Lock requests/sec" />
<PerformanceCounters Objects="MSAS 2008:Locks" Counters="Unlock requests/sec" />

<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total Autoexist" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total cells calculated" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total EXISTING" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total flat cache inserts" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total NON EMPTY" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total NON EMPTY for calculated members" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total NON EMPTY unoptimized" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total recomputes" />
<PerformanceCounters Objects="MSAS 2008:MDX" Counters="Total Sonar subcubes" />

<PerformanceCounters Objects="MSAS 2008:Memory" Counters="AggCacheKB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Aggregation Map Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Cleaner Balance/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Cleaner Current Price" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Cleaner Memory KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Dimension Index (Hash) Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Dimension Property Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Dimension String Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Fact Aggregation Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Fact Data Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Fact String Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore Clock Pages Examined/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore Clock Pages HaveRef/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore Clock Pages Valid/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore Memory Pinned KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Filestore Page Faults/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Aggregation Map File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Aggregation Map File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension Index (Hash) File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension Index (Hash) File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension Property File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension Property File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension String File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Dimension String File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact Aggregation File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact Aggregation File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact Data File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact Data File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact String File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Fact String File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Map File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Map File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Other File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="In-memory Other File KB/sec" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Map Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Other Files" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 1 Alloc KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 1 Lookaside KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 64 Alloc KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 64 Lookaside KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 8 Alloc KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Page Pool 8 Lookaside KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Aggregation Map File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Dimension Index (Hash) File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Dimension Property File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Dimension String File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Fact Aggregation File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Fact Data File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory"	Counters="Potential In-memory Fact String File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Map File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Potential In-memory Other File KB" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Quota Blocked" />
<PerformanceCounters Objects="MSAS 2008:Memory" Counters="Quota KB" />

<PerformanceCounters Objects="MSAS 2008:Proc Aggregations" Counters="Memory size bytes" />
<PerformanceCounters Objects="MSAS 2008:Proc Aggregations" Counters="Memory size rows" />
<PerformanceCounters Objects="MSAS 2008:Proc Aggregations" Counters="Rows merged/sec" />
<PerformanceCounters Objects="MSAS 2008:Proc Aggregations" Counters="Temp file rows written/sec" />
<PerformanceCounters Objects="MSAS 2008:Proc Aggregations" Counters="Total Partitions" />

<PerformanceCounters Objects="MSAS 2008:Proc Indexes" Counters="Total partitions" />
<PerformanceCounters Objects="MSAS 2008:Proc Indexes" Counters="Total rows" />


<PerformanceCounters Objects="MSAS 2008:Processing" Counters="Rows converted/sec" />
<PerformanceCounters Objects="MSAS 2008:Processing" Counters="Total rows converted" />
<PerformanceCounters Objects="MSAS 2008:Processing" Counters="Total rows read" />
<PerformanceCounters Objects="MSAS 2008:Processing" Counters="Total rows written" />

<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Current dimension queries" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Current measure group queries" />

<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Dimension cache hits/sec" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Dimension cache lookups/sec" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Dimension queries/sec" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total bytes sent" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total dimension queries." />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total measure group queries" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total network round trips" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total queries answered" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total queries from cache direct" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total queries from cache filtered" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total queries from file" />
<PerformanceCounters Objects="MSAS 2008:Storage Engine Query" Counters="Total rows sent" />

<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Long parsing busy threads" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Long parsing idle threads" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Long parsing job queue length" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Long parsing job rate" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Short parsing busy threads" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Short parsing idle threads" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Short parsing job queue length" />
<PerformanceCounters Objects="MSAS 2008:Threads" Counters="Short parsing job rate" />

</ns:PerformanceCountersCollector>', @collection_item_id=@collection_item_id_66 OUTPUT, @frequency=60, @collection_set_id=@collection_set_id_63, @collector_type_uid=@collector_type_uid_65
Select @collection_item_id_66

Commit Transaction;
End Try
Begin Catch
Rollback Transaction;
DECLARE @ErrorMessage NVARCHAR(4000);
DECLARE @ErrorSeverity INT;
DECLARE @ErrorState INT;
DECLARE @ErrorNumber INT;
DECLARE @ErrorLine INT;
DECLARE @ErrorProcedure NVARCHAR(200);
SELECT @ErrorLine = ERROR_LINE(),
       @ErrorSeverity = ERROR_SEVERITY(),
       @ErrorState = ERROR_STATE(),
       @ErrorNumber = ERROR_NUMBER(),
       @ErrorMessage = ERROR_MESSAGE(),
       @ErrorProcedure = ISNULL(ERROR_PROCEDURE(), '-');
RAISERROR (14684, @ErrorSeverity, 1 , @ErrorNumber, @ErrorSeverity, @ErrorState, @ErrorProcedure, @ErrorLine, @ErrorMessage);

End Catch;

GO


