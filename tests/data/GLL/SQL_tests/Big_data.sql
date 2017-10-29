-- Content of this file was taken from https://github.com/ktaranov/sqlserver-kit
/*
Author: Daniel Hutmacher
Original link: https://sqlsunday.com/2017/04/10/guessing-fk-constraints
*/

DECLARE @referenced TABLE (
    [object_id]        int NOT NULL,
    index_id        int NOT NULL,
    column_name        sysname NOT NULL,
    user_type_id    int NOT NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id, column_name)
);

INSERT INTO @referenced ([object_id], index_id, column_name, user_type_id)
SELECT t.[object_id], i.index_id, c.[name] AS column_name, c.user_type_id
FROM sys.tables AS t
INNER JOIN sys.indexes AS i ON t.[object_id]=i.[object_id]
INNER JOIN sys.index_columns AS ic ON i.[object_id]=ic.[object_id] AND i.index_id=ic.index_id
INNER JOIN sys.columns AS c ON ic.[object_id]=c.[object_id] AND ic.column_id=c.column_id
WHERE c.is_nullable=0 AND i.[type] IN (1, 2) AND i.is_unique=1 AND ic.key_ordinal>0;

WITH referenced AS (
    SELECT *, COUNT(*) OVER (
               PARTITION BY [object_id], index_id) AS col_count
    FROM @referenced),

     work AS (
    SELECT COUNT(*) OVER (
               PARTITION BY r.[object_id], r.index_id, t.[object_id]) AS referencing_count,
           r.col_count AS referenced_count, r.index_id,
           t.[object_id] AS referencing_tbl, c.[name] AS referencing_col,
           r.[object_id] AS referenced_tbl,  r.column_name AS referenced_col
    FROM referenced AS r
    INNER JOIN sys.tables AS t ON
        r.[object_id]!=t.[object_id]
    INNER JOIN sys.columns AS c ON
        t.[object_id]=c.[object_id] AND
        --- This is where the column naming logic
        --- can be customized:
        c.[name]=r.column_name AND
        c.user_type_id=r.user_type_id)

SELECT fk.[name] AS [Existing FK],
       'ALTER TABLE '+ts.[name]+'.'+t.[name]+
           ' ADD CONSTRAINT '+
           ISNULL(fk.[name], 'FK_'+rs.[name]+'_'+r.[name]+'_'+ts.[name]+'_'+t.[name])+
           ' FOREIGN KEY ('+x.referencing_columns+')'+
           ' REFERENCES '+rs.[name]+'.'+r.[name]+' ('+x.referenced_columns+')' AS Syntax
FROM work
INNER JOIN sys.tables AS r ON work.referenced_tbl=r.[object_id]
INNER JOIN sys.schemas AS rs ON r.[schema_id]=rs.[schema_id]
INNER JOIN sys.tables AS t ON work.referencing_tbl=t.[object_id]
INNER JOIN sys.schemas AS ts ON t.[schema_id]=ts.[schema_id]
LEFT JOIN sys.foreign_keys AS fk ON
    work.referencing_tbl=fk.parent_object_id AND
    work.referenced_tbl=fk.referenced_object_id
CROSS APPLY (
    SELECT
        SUBSTRING(CAST((
            SELECT ', '+w.referencing_col
            FROM work AS w
            WHERE w.referencing_tbl=work.referencing_tbl AND
                  w.referenced_tbl=work.referenced_tbl AND
                  w.index_id=work.index_id
            ORDER BY w.referencing_col
            FOR XML PATH(''), TYPE) AS varchar(4000)), 3, 4000),
        SUBSTRING(CAST((
            SELECT ', '+w.referenced_col
            FROM work AS w
            WHERE w.referencing_tbl=work.referencing_tbl AND
                  w.referenced_tbl=work.referenced_tbl AND
                  w.index_id=work.index_id
            ORDER BY w.referencing_col
            FOR XML PATH(''), TYPE) AS varchar(4000)), 3, 4000)
    ) AS x(referencing_columns, referenced_columns)
WHERE work.referencing_count=work.referenced_count
GROUP BY ts.[name], t.[name], rs.[name], r.[name], x.referencing_columns, x.referenced_columns, fk.[name]
ORDER BY Syntax;

GO
/*
Author: Chris Yates
Original link: http://www.sqlservercentral.com/blogs/the-sql-professor/2017/01/26/backuprestorewhats-my-status/
*/

--By checking only for the restore and backup command lines you will be able to quickly identify your session id
--and get an  approximate ETA and percentage complete. you can tinker of course with the estimations
--if you’d like or pull back more fields. This is just a simple technique in utilizing a helpful DMV to provide info quickly.
SELECT  r.session_id
      , r.command
      , r.start_time
      , r.status
      , CONVERT(NUMERIC(6, 2), r.percent_complete) AS [Percent Complete]
      , CONVERT(VARCHAR(20), DATEADD(ms, r.estimated_completion_time , GETDATE()), 20) AS [ETA Completion Time]
      , CONVERT(NUMERIC(10, 2), r.total_elapsed_time / 1000.0 / 60.0) AS [Elapsed Min]
      , CONVERT(NUMERIC(10, 2), r.total_elapsed_time / 1000.0 / 60.0 / 60.0) AS [Elapsed Hours]
      , CONVERT(NUMERIC(10, 2), r.estimated_completion_time / 1000.0 / 60.0) AS [ETA Min]
      , CONVERT(NUMERIC(10, 2), r.estimated_completion_time / 1000.0 / 60.0/ 60.0) AS [ETA Hours]
      , CONVERT(VARCHAR(1000), (
                 SELECT SUBSTRING(TEXT, r.statement_start_offset / 2, CASE
                             WHEN r.statement_end_offset = -1
                                 THEN 1000
                             ELSE (r.statement_end_offset - r.statement_start_offset) / 2
                             END)
                 FROM   sys.dm_exec_sql_text(sql_handle)
                 )) AS TSQLStatement
FROM sys.dm_exec_requests r
WHERE command IN (
        'RESTORE DATABASE'
        , 'BACKUP DATABASE'
        );

GO
/*
Author: Kenneth Fisher
Original link: https://sqlstudies.com/2017/01/18/lead-blocker/
*/
WITH Blocked_Sessions AS (
       -- Collect lead blockers
       --     Pull all blocking IDs & check which ones are not being blocked themselves
       SELECT sys.dm_exec_requests.blocking_session_id  AS lead_session_id,
             sys.dm_exec_requests.blocking_session_id  AS blocking_session_id , 0 Cnt
       FROM sys.dm_exec_requests
       WHERE blocking_session_id  <> 0
         AND blocking_session_id  NOT IN (SELECT session_id
                                          FROM sys.dm_exec_requests
                                          WHERE sys.dm_exec_requests.blocking_session_id  <> 0)
       UNION ALL
       -- Recurse through list of blocked sessions
       SELECT Blocked_Sessions.lead_session_id, sys.dm_exec_requests.session_id, 1 Cnt
       FROM sys.dm_exec_requests
       JOIN Blocked_Sessions
             ON Blocked_Sessions.blocking_session_id  = sys.dm_exec_requests.blocking_session_id 
    ),
       Blocked AS (
             -- Add up all sessions blocked for the lead blocker
             SELECT lead_session_id, SUM(Cnt) AS sessions_blocked
             FROM Blocked_Sessions
             GROUP BY lead_session_id)
SELECT Blocked.*, DATEDIFF(s, Sess.last_request_start_time, getdate()) seconds_blocking,
    ISNULL(Req.status,'sleeping') [status], SqlText.text [sql_text],
    STUFF((SELECT DISTINCT ISNULL(', ' + db.name,'')
            FROM sys.databases db
            JOIN sys.dm_tran_locks lcks
                ON db.database_id = lcks.resource_database_id
            WHERE lcks.request_session_id = Sess.session_id
            ORDER BY ISNULL(', ' + db.name,'')
            FOR XML PATH(''),TYPE)--.value('.','VARCHAR(MAX)')
        ,1,2,'') AS database_list
        , Conn.client_net_address, Sess.login_name
FROM sys.dm_exec_connections Conn
LEFT OUTER JOIN sys.dm_exec_sessions Sess
       ON Conn.session_id = Sess.session_id
JOIN Blocked
       ON Blocked.lead_session_id = Sess.session_id
CROSS APPLY sys.dm_exec_sql_text(Conn.most_recent_sql_handle) SqlText
LEFT JOIN sys.dm_exec_requests Req
       ON Req.session_id = Sess.session_id
WHERE Blocked.sessions_blocked >= 1
-- We only care if the session has been blocked for longer than 30 seconds.
-- This can obviously be modified or commented out.
  AND DATEDIFF(s, Sess.last_request_start_time, getdate()) > 30;

GO
-- MS SQL Server function LEN() does not count trailing spaces!
DECLARE @pattern varchar(20) = 'Ted ';
DECLARE @columnToSearch VARCHAR(1000) = 'Ted is Ted because Ted is awesome!';

SELECT (DATALENGTH(@columnToSearch) - DATALENGTH(REPLACE(@columnToSearch, @pattern, ''))) / DATALENGTH(@pattern) DatalengthNumberOfMatches
     , (LEN(@columnToSearch) - LEN(REPLACE(@columnToSearch, @pattern, ''))) / Len(@pattern) AS LenNumberOfMatches
     , (LEN(@columnToSearch) - LEN(REPLACE(@columnToSearch, @pattern, ''))) AS LenPattern
     , LEN(@pattern) AS Len@pattern
     , DATALENGTH(@pattern) AS Datalength@pattern;

GO
/*
Author: Benjamin Nevarez
Original link: http://sqlblog.com/blogs/ben_nevarez/archive/2009/07/26/getting-cpu-utilization-data-from-sql-server.aspx
*/


DECLARE @ts_now BIGINT;

SELECT @ts_now = cpu_ticks / CONVERT(FLOAT, cpu_ticks)
FROM   sys.dm_os_sys_info;

SELECT record_id
     , Dateadd(ms, -1 * ( @ts_now - [timestamp])
     , Getdate()) AS EventTime
     , sqlprocessutilization
     , systemidle
     , 100 - systemidle - sqlprocessutilization AS OtherProcessUtilization
FROM   (SELECT record.value('(./Record/@id)[1]', 'int') AS record_id
             , record.value('(./Record/SchedulerMonitorEvent/SystemHealth/SystemIdle)[1]', 'int') AS SystemIdle
             , record.value('(./Record/SchedulerMonitorEvent/SystemHealth/ProcessUtilization)[1]', 'int') AS SQLProcessUtilization
             , timestamp
 FROM   (SELECT timestamp
              , record
         FROM   sys.dm_os_ring_buffers
         WHERE  ring_buffer_type = N'RING_BUFFER_SCHEDULER_MONITOR'
            AND record LIKE '%<SystemHealth>%') AS x) AS y
ORDER BY record_id DESC;

GO
/*
Author: Slava Murygin
Original link: http://slavasql.blogspot.ru/2016/03/sql-server-cpu-utilization-in-graphical.html
*/

DECLARE @gc VARCHAR(MAX), @gi VARCHAR(MAX);
WITH BR_Data as (
 SELECT timestamp, record
 FROM sys.dm_os_ring_buffers
 WHERE ring_buffer_type = N'RING_BUFFER_SCHEDULER_MONITOR' and record like '%<SystemHealth>%'
), Extracted_XML as (
 SELECT timestamp, record.value('(./Record/@id)[1]', 'int') as record_id,
  record.value('(./Record/SchedulerMonitorEvent/SystemHealth/SystemIdle)[1]', 'bigint') as SystemIdle,
  record.value('(./Record/SchedulerMonitorEvent/SystemHealth/ProcessUtilization)[1]', 'bigint') as SQLCPU
 FROM BR_Data
), CPU_Data as (
 SELECT record_id, ROW_NUMBER() OVER(ORDER BY record_id) as rn,
    dateadd(ms, -1 * ((SELECT ms_ticks  FROM sys.dm_os_sys_info) - [timestamp]), GETDATE()) as EventTime,
    SQLCPU, SystemIdle, 100 - SystemIdle - SQLCPU as OtherCPU
 FROM Extracted_XML )
SELECT @gc = CAST((SELECT  CAST(d1.rn as VARCHAR) + ' ' + CAST(d1.SQLCPU as VARCHAR) + ',' FROM CPU_Data as d1 ORDER BY d1.rn FOR XML PATH('')) as VARCHAR(MAX)),
@gi = CAST((SELECT  CAST(d1.rn as VARCHAR) + ' ' + CAST(d1.OtherCPU as VARCHAR) + ',' FROM CPU_Data as d1 ORDER BY d1.rn FOR XML PATH('')) as VARCHAR(MAX))
--OPTION (RECOMPILE);

SELECT CAST('LINESTRING(' + LEFT(@gc,LEN(@gc)-1) + ')' as VARCHAR), 'SQL CPU %'
UNION ALL
SELECT CAST('LINESTRING(1 100,2 100)' as VARCHAR), ''
UNION ALL
SELECT CAST('LINESTRING(' + LEFT(@gi,LEN(@gi)-1) + ')' as VARCHAR), 'Other CPU %';

GO
/*
Author: Nagaraj
Original link: http://www.sqlservercentral.com/blogs/sql-and-sql-only/2016/08/07/current-running-queries/
*/

SELECT getdate() as dt,
ss.session_id,
db_name(sysprocesses.dbid) as dbname,
er.status as req_status,
ss.login_name,
cs.client_net_address,
ss.program_name,
sysprocesses.open_tran,
er.blocking_session_id,
ss.host_name,
ss.client_interface_name,
[eqp].[query_plan] as qplan,
SUBSTRING(est.text,(er.statement_start_offset/2)+1,
CASE WHEN er.statement_end_offset=-1 OR er.statement_end_offset=0
THEN (DATALENGTH(est.Text)-er.statement_start_offset/2)+1
ELSE (er.statement_end_offset-er.statement_start_offset)/2+1
END) as req_query_text,
er.granted_query_memory,
er.logical_reads as req_logical_reads,
er.cpu_time as req_cpu_time,
er.reads as req_physical_reads,
er.row_count as req_row_count,
er.scheduler_id,
er.total_elapsed_time as req_elapsed_time,
er.start_time as req_start_time,
er.percent_complete,
er.wait_resource as wait_resource,
er.wait_type as req_waittype,
er.wait_time as req_wait_time,
wait.wait_duration_ms as blocking_time_ms,
lock.resource_associated_entity_id,
lock.request_status as lock_request_status,
lock.request_mode as lock_mode,
er.writes as req_writes,
sysprocesses.lastwaittype,
fn_sql.text as session_query,
ss.status as session_status,
ss.cpu_time as session_cpu_time,
ss.reads as session_reads,
ss.writes as session_writes,
ss.logical_reads as session_logical_reads,
ss.memory_usage as session_memory_usage,
ss.last_request_start_time,
ss.last_request_end_time,
ss.total_scheduled_time as session_scheduled_time,
ss.total_elapsed_time as session_elpased_time,
ss.row_count as session_rowcount
FROM sys.dm_exec_sessions ss
INNER JOIN sys.dm_exec_connections cs ON ss.session_id = cs.session_id
OUTER APPLY fn_get_sql(cs.most_recent_sql_handle) as fn_sql
INNER JOIN sys.sysprocesses ON sys.sysprocesses.spid = cs.session_id
LEFT OUTER JOIN sys.dm_exec_requests [er] ON er.session_id = ss.session_id
OUTER APPLY sys.dm_exec_sql_text ([er].[sql_handle]) [est]
OUTER APPLY sys.dm_exec_query_plan ([er].[plan_handle]) [eqp]
LEFT OUTER JOIN sys.dm_os_waiting_tasks wait ON er.session_id = wait.session_id
and wait.wait_type like 'LCK%' and
er.blocking_session_id = wait.blocking_session_id
LEFT OUTER JOIN sys.dm_tran_locks lock ON lock.lock_owner_address = wait.resource_address
                                      AND lock.request_session_id = er.blocking_session_id
WHERE ss.status != 'sleeping';

GO
/*

This script shows current status and activity for currently running tasks on
the SQL Server. It returns 6 recordsets, in the following order:

1: General server-wide information
2: Database files, usage, disk space
3: Connections by SPID including CPU and I/O totals
4: Running queries, memory use/grant, query plan
5: Open transactions, type, isolation level
6: Locks by object, partition, including size, wait type, blocking info.

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source:  http://sqlsunday.com/downloads/
Version: 2016-10-20

DISCLAIMER: This script does not make any modifications to the server, except
            for creating three temporary functions in tempdb, used to format
        values in a user-friendly manner. However, the script may not be
        suitable to run in a production environment. I cannot assume any
        responsibility regarding the accuracy of the output information,
        performance impacts on your server, or any other consequence. If
        your juristiction does not allow for this kind of waiver/disclaimer,
        or if you do not accept these terms, you are NOT allowed to store,
        distribute or use this code in any way.

*/

USE tempdb;
SET TRANSACTION ISOLATION LEVEL SNAPSHOT;
SET DEADLOCK_PRIORITY LOW;
SET STATISTICS XML OFF;
SET LOCK_TIMEOUT 500;
SET NOCOUNT ON;

DECLARE @hasPermissions bit=HAS_PERMS_BY_NAME(NULL, NULL, 'VIEW SERVER STATE');
IF (@hasPermissions=0) BEGIN;
    RAISERROR('This script requires VIEW SERVER STATE permissions.', 16, 1);
    RETURN;
END;

SET @hasPermissions=HAS_PERMS_BY_NAME(NULL, 'DATABASE', 'CREATE FUNCTION')
IF (@hasPermissions=0) BEGIN;
    RAISERROR('This script requires CREATE FUNCTION permissions in tempdb.', 16, 1);
    RETURN;
END;

-------------------------------------------------------------------------------








BEGIN TRANSACTION;
BEGIN TRY;

-------------------------------------------------------------------------------
--- Temporary functions, used to beautify numbers, bits&bytes and dates.
-------------------------------------------------------------------------------

IF (NOT EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_friendly_age'))
    EXEC('
CREATE FUNCTION dbo.fn_friendly_age (
    @from	datetime,
    @to	datetime)
RETURNS varchar(20)
WITH SCHEMABINDING
AS

BEGIN
    DECLARE @age varchar(20)

    IF (@from IS NOT NULL AND @to IS NOT NULL)
        SET @age=(CASE
            WHEN DATEDIFF(ss, @from, @to)<1
            THEN ''       ''+STR(DATEDIFF(ms, @from, @to), 10, 0)+'' ms.''

            WHEN DATEDIFF(ss, @from, @to)<180
            THEN ''        ''+STR(0.001*DATEDIFF(ms, @from, @to), 10, 1)+'' s.''

            WHEN DATEDIFF(ss, @from, @to)<24*3600
            THEN ''     ''+SUBSTRING(CONVERT(varchar(20), DATEADD(ss, DATEDIFF(ss, @from, @to), 0), 120), 12, 8)

            WHEN DATEDIFF(dd, @from, @to)<3
            THEN STR(FLOOR(1.0*DATEDIFF(ss, @from, @to)/(3600*24)), 3, 0)+''d+''+
                SUBSTRING(CONVERT(varchar(20), DATEADD(ss, DATEDIFF(ss, @from, @to), 0), 120), 12, 5)+''  ''

            ELSE STR(FLOOR(1.0*DATEDIFF(ss, @from, @to)/(3600*24)), 3, 0)+''d ''+
                REPLICATE('' '', 5)+''  ''
            END)

    RETURN @age
END');

IF (NOT EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_format_number'))
    EXEC('
CREATE FUNCTION dbo.fn_format_number(
    @number		numeric(28, 8),
    @prec		tinyint)
RETURNS varchar(100)
WITH SCHEMABINDING
AS

BEGIN
    DECLARE @out varchar(100), @offset int=(CASE WHEN @prec=0 THEN 0 ELSE @prec+1 END)

    SET @out=REVERSE(LTRIM(ISNULL(STR(@number, 20, @prec), '''')))
    WHILE (@offset<LEN(@out)) BEGIN
        IF (SUBSTRING(@out, @offset, 1)!=''.'') BEGIN
            SET @out=LEFT(@out, @offset)+'' ''+SUBSTRING(@out, @offset+1, LEN(@out))
            SET @offset=@offset+1
        END
        SET @offset=@offset+3
    END
    SET @out=LTRIM(REVERSE(@out))
    IF (LEN(@out)<15)
        SET @out=NULLIF(RIGHT(REPLICATE('' '', 15+@prec)+@out, 15+@prec), '''')

    RETURN @out
END');

IF (NOT EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_friendly_size'))
    EXEC('
CREATE FUNCTION dbo.fn_friendly_size (
    @bytes	bigint)
RETURNS varchar(20)
WITH SCHEMABINDING
AS

BEGIN;
    DECLARE @size varchar(20), @k bigint=1024;

    IF (@bytes IS NOT NULL)
        SET @size=(CASE
            --WHEN @bytes<4*@k
            --THEN dbo.fn_format_number(1.0*@bytes, 0)+'' B''

            WHEN @bytes<4*@k*@k
            THEN dbo.fn_format_number(1.0*@bytes/@k, 1)+'' kB''

            WHEN @bytes<4*@k*@k*@k
            THEN dbo.fn_format_number(1.0*@bytes/@k/@k, 2)+'' MB''

            WHEN @bytes<4*@k*@k*@k*@k
            THEN dbo.fn_format_number(1.0*@bytes/@k/@k/@k, 2)+'' GB''

            ELSE dbo.fn_format_number(1.0*@bytes/@k/@k/@k/@k, 2)+'' TB''
            END);

    RETURN @size;
END;');



-------------------------------------------------------------------------------
--- Gather information on database files and objects/indexes
-------------------------------------------------------------------------------

DECLARE @sql varchar(max), @sql2 varchar(max), @sql3 varchar(max),
    @sql4 varchar(max), @name sysname, @virtual_machine_type int,
    @version varchar(4), @physical_memory bigint, @virtual_memory bigint,
    @gb bigint=1024*1024*1024, @mb bigint=1024*1024,
    @max_server_memory bigint, @min_server_memory bigint, @max_degree_of_parallelism tinyint;

SET @version=LEFT(CAST(SERVERPROPERTY('ProductVersion') AS varchar(10)), 4);
IF (@version LIKE '9%') SET @version=LEFT('0'+@version, 4);

SELECT @max_server_memory=CAST([value] AS bigint)*1024*1024 FROM sys.configurations WHERE [name]='max server memory (MB)';
SELECT @min_server_memory=CAST([value] AS bigint)*1024*1024 FROM sys.configurations WHERE [name]='min server memory (MB)';
SELECT @max_degree_of_parallelism=CAST([value] AS tinyint)  FROM sys.configurations WHERE [name]='max degree of parallelism';

IF (@version<'11.0')
    EXECUTE sp_executesql N'
        SELECT @physical_memory=physical_memory_in_bytes,
            @virtual_memory=virtual_memory_in_bytes
        FROM sys.dm_os_sys_info',
        N'@physical_memory bigint OUTPUT, @virtual_memory bigint OUTPUT',
        @physical_memory=@physical_memory OUTPUT,
        @virtual_memory=@virtual_memory OUTPUT;

IF (@version>='11.0')
    EXECUTE sp_executesql N'
        SELECT @physical_memory=physical_memory_kb*1024,
            @virtual_memory=virtual_memory_kb*1024
        FROM sys.dm_os_sys_info',
        N'@physical_memory bigint OUTPUT, @virtual_memory bigint OUTPUT',
        @physical_memory=@physical_memory OUTPUT,
        @virtual_memory=@virtual_memory OUTPUT;

BEGIN TRY;
    EXECUTE sp_executesql N'
        SELECT @virtual_machine_type=virtual_machine_type
        FROM sys.dm_os_sys_info',
        N'@virtual_machine_type int OUTPUT',
        @virtual_machine_type=@virtual_machine_type OUTPUT;
END TRY
BEGIN CATCH;
    PRINT 'This version of SQL Server does not provide virtual_machine_type.';
END CATCH;

-------------------------------------------------------------------------------

DECLARE @block_chain TABLE (
    spid				int NOT NULL,
    blocked_by			varchar(max) NOT NULL,
    PRIMARY KEY CLUSTERED (spid)
);

DECLARE @blocking TABLE (
    spid				int NOT NULL,
    blocking			varchar(max) NOT NULL,
    PRIMARY KEY CLUSTERED (spid)
);

DECLARE @locks TABLE (
    spid				int NOT NULL,
    tran_id				bigint NOT NULL,
    lock_ord			int NOT NULL,
    mode				varchar(255) NOT NULL,
    [database]			varchar(255) NOT NULL,
    [object_id]			int NOT NULL,
    [object_name]		varchar(255) NULL,
    index_id			int NOT NULL,
    index_type			tinyint NULL,
    index_name			varchar(255) NULL,
    index_filter		varchar(max) NULL,
    object_notes		varchar(255) NULL,
    obj_rows			bigint NULL,
    partition_number	 int NOT NULL,
    partition_count		int NULL,
    partition_boundary	varchar(max) NULL,
    request_owner_type	varchar(50) NOT NULL,
    wait_type			varchar(120) NULL,
    wait_time			varchar(100) NULL,
    blkd_by_spid		int NULL,
    [count]				int NULL,
    isWaiting			bit NULL,
    isBlocking			bit NULL,
    ident				int IDENTITY(1, 1) NOT NULL,
    PRIMARY KEY CLUSTERED (spid, tran_id, [object_id], index_id, partition_number, lock_ord, ident)
);

DECLARE @tran_locks TABLE (
    resource_database_id int NOT NULL,
    request_session_id	int NOT NULL,
    request_owner_id	bigint NULL,
    request_mode		nvarchar(120) NOT NULL,
    request_status		nvarchar(120) NOT NULL,
    resource_type		nvarchar(120) NOT NULL,
    request_owner_type	nvarchar(120) NULL,
    lock_owner_address	varbinary(8) NOT NULL,
    resource_associated_entity_id bigint NULL,
    ord					int NOT NULL,
    PRIMARY KEY CLUSTERED (request_session_id, ord)
);

INSERT INTO @tran_locks
SELECT resource_database_id, request_session_id, request_owner_id, request_mode, request_status,
    resource_type, request_owner_type, lock_owner_address, resource_associated_entity_id,
    ROW_NUMBER() OVER (PARTITION BY request_session_id ORDER BY lock_owner_address) AS ord
FROM sys.dm_tran_locks WITH (NOLOCK);

DECLARE @tran_session_transactions TABLE (
    transaction_id		bigint NOT NULL,
    session_id			int NOT NULL,
    is_local			bit NOT NULL,
    PRIMARY KEY CLUSTERED (transaction_id)
);

INSERT INTO @tran_session_transactions
SELECT transaction_id, session_id, is_local
FROM sys.dm_tran_session_transactions WITH (NOLOCK);

DECLARE @exec_sessions TABLE (
    session_id			int NOT NULL,
    [program_name]		nvarchar(256) NULL,
    login_name			nvarchar(256) NOT NULL,
    [status]			nvarchar(60) NOT NULL,
    last_request_start_time datetime NOT NULL,
    cpu_time			int NOT NULL,
    memory_usage		int NOT NULL,
    reads				bigint NOT NULL,
    writes				bigint NOT NULL,
    logical_reads		bigint NOT NULL,
    flags				varchar(200) NOT NULL,
    client_interface_name nvarchar(64) NULL,
    PRIMARY KEY CLUSTERED (session_id)
);

INSERT INTO @exec_sessions
SELECT session_id, [program_name], login_name, [status], last_request_start_time, cpu_time,
    memory_usage, reads, writes, logical_reads,
      'arithabort '+(CASE [arithabort] WHEN 1 THEN 'ON' ELSE 'OFF' END)+
    ', quot.id '+(CASE [quoted_identifier] WHEN 1 THEN 'ON' ELSE 'OFF' END)+
    (CASE [ansi_warnings] WHEN 0 THEN ', ansi warn OFF' ELSE '' END)+
    (CASE [ansi_padding] WHEN 0 THEN ', ansi pad OFF' ELSE '' END)+
    (CASE [ansi_nulls] WHEN 0 THEN ', nulls OFF' ELSE '' END)+
    (CASE [concat_null_yields_null] WHEN 0 THEN ', conc.null OFF' ELSE '' END) AS flags,
    LEFT(client_interface_name, CHARINDEX(' ', client_interface_name+' ')-1)
FROM sys.dm_exec_sessions WITH (NOLOCK);

DECLARE @jobs TABLE (
    job_id				int NOT NULL,
    job_name			sysname NOT NULL,
    step_id				int NOT NULL,
    step_name			sysname NOT NULL,
    search_name			varchar(max) NOT NULL,
    PRIMARY KEY CLUSTERED (job_id, step_id)
);

INSERT INTO @jobs
SELECT j.job_id, j.name AS job_name, js.step_id, js.step_name, ' 0x'+
    SUBSTRING(CAST(j.job_id AS varchar(max)),  7,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)),  5,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)),  3,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)),  1,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 12,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 10,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 17,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 15,  2)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 20,  4)+
    SUBSTRING(CAST(j.job_id AS varchar(max)), 25, 12)+'%:% '+
    CAST(js.step_id AS varchar(max))+')' AS search_name
FROM msdb.dbo.sysjobs AS j WITH (NOLOCK)
INNER JOIN msdb.dbo.sysjobsteps AS js WITH (NOLOCK) ON j.job_id=js.job_id;

DECLARE @exec_requests TABLE (
    session_id			int NOT NULL,
    request_id			int NOT NULL,
    database_id			smallint NOT NULL,
    transaction_isolation_level smallint NOT NULL,
    percent_complete	real NOT NULL,
    total_elapsed_time	int NOT NULL,
    command				varchar(32) NOT NULL,
    statement_start_offset int NULL,
    statement_end_offset int NULL,
    plan_handle			varbinary(64) NULL,
    _id                 int IDENTITY(1, 1) NOT NULL,
    PRIMARY KEY CLUSTERED (session_id, request_id, _id)
);

INSERT INTO @exec_requests (session_id, request_id, database_id, transaction_isolation_level, percent_complete,
    total_elapsed_time, command, statement_start_offset, statement_end_offset, plan_handle)
SELECT session_id, request_id, database_id, transaction_isolation_level, percent_complete,
    total_elapsed_time, command, statement_start_offset, statement_end_offset, plan_handle
FROM sys.dm_exec_requests WITH (NOLOCK);

DECLARE @exec_connections TABLE (
    session_id			int NOT NULL,
    client_net_address	varchar(48) NULL,
    client_tcp_port		int NULL,
    local_net_address	varchar(48) NULL,
    auth_scheme			nvarchar(80) NOT NULL,
    connect_time		datetime NOT NULL,
    num_reads			int NULL,
    num_writes			int NULL,
    most_recent_sql_handle varbinary(64),
    PRIMARY KEY CLUSTERED (session_id)
);

INSERT INTO @exec_connections
SELECT session_id, client_net_address, client_tcp_port, local_net_address, auth_scheme,
    connect_time, num_reads, num_writes, most_recent_sql_handle
FROM sys.dm_exec_connections WITH (NOLOCK)
WHERE endpoint_id!=0 AND session_id IS NOT NULL;

DECLARE @os_tasks TABLE (
    session_id          int NOT NULL,
    scheduler_id        int NOT NULL,
    PRIMARY KEY CLUSTERED (session_id, scheduler_id)
);

INSERT INTO @os_tasks
SELECT DISTINCT session_id, scheduler_id
FROM sys.dm_os_tasks
WHERE session_id IS NOT NULL AND
      scheduler_id IS NOT NULL;

DECLARE @tran_active_transactions TABLE (
    transaction_id		bigint NOT NULL,
    transaction_begin_time	datetime NOT NULL,
    transaction_type	int NOT NULL,
    transaction_state	int NOT NULL,
    PRIMARY KEY CLUSTERED (transaction_id)
);

INSERT INTO @tran_active_transactions
SELECT transaction_id, transaction_begin_time, transaction_type, transaction_state
FROM sys.dm_tran_active_transactions WITH (NOLOCK);

DECLARE @os_waiting_tasks TABLE (
    session_id			int NULL,
    blocking_session_id	int NULL,
    waiting_task_address varbinary(8) NOT NULL,
    wait_type			nvarchar(120) NULL,
    wait_duration_ms	bigint NULL,
    resource_address	varbinary(8) NULL,
    exec_context_id		int NULL,
    dupl				int NOT NULL,
    PRIMARY KEY CLUSTERED (waiting_task_address, dupl)
);

INSERT INTO @os_waiting_tasks
SELECT session_id, blocking_session_id, waiting_task_address,
    wait_type, wait_duration_ms, resource_address, exec_context_id,
    ROW_NUMBER() OVER (
        PARTITION BY waiting_task_address
        ORDER BY wait_duration_ms, resource_address) AS dupl
FROM sys.dm_os_waiting_tasks WITH (NOLOCK);

DECLARE @exec_query_memory_grants TABLE (
    session_id			int NOT NULL,
    request_id			int NOT NULL,
    requested_mb		numeric(16, 3) NOT NULL,
    granted_mb			numeric(16, 3) NULL,
    minimum_required_mb	numeric(16, 3) NOT NULL,
    used_mb				numeric(16, 3) NOT NULL,
    max_used_mb			numeric(16, 3) NOT NULL,
    ideal_mb			numeric(16, 3) NOT NULL,
    est_query_cost		numeric(16, 1) NOT NULL,
    PRIMARY KEY CLUSTERED (session_id, request_id)
);

INSERT INTO @exec_query_memory_grants
SELECT session_id, request_id,
    1.0*requested_memory_kb/1024 AS requested_mb,
    1.0*granted_memory_kb/1024 AS granted_mb,
    1.0*required_memory_kb/1024 AS minimum_required_mb,
    ISNULL(1.0*used_memory_kb/1024, 0.0) AS used_mb,
    ISNULL(1.0*max_used_memory_kb/1024, 0.0) AS max_used_mb,
    1.0*ideal_memory_kb/1024 AS ideal_mb,
    query_cost AS est_query_cost
FROM sys.dm_exec_query_memory_grants WITH (NOLOCK);

DECLARE @dm_server_services TABLE (
    _id                 tinyint IDENTITY(1, 1) NOT NULL,
    servicename         nvarchar(512) NOT NULL,
    startup_type_desc   nvarchar(512) NOT NULL,
    status_desc         nvarchar(512) NOT NULL,
    service_account     nvarchar(512) NOT NULL,
    cluster_nodename    nvarchar(512) NULL,
    PRIMARY KEY CLUSTERED (_id)
);

BEGIN TRY;
    INSERT INTO @dm_server_services
    SELECT servicename, startup_type_desc, status_desc, service_account, cluster_nodename
    FROM sys.dm_server_services;
END TRY
BEGIN CATCH;
    PRINT 'sys.dm_server_services is not supported on this version of SQL Server.';
END CATCH;

DECLARE @database_procedures TABLE (
    database_id			int NOT NULL,
    [object_id]			int NOT NULL,
    [name]				varchar(255) NOT NULL,
    PRIMARY KEY CLUSTERED (database_id, [object_id])
);

DECLARE @database_objects TABLE (
    database_id			int NOT NULL,
    [object_id]			int NOT NULL,
    index_id			int NOT NULL,
    hobt_id				bigint NOT NULL,
    [name]				varchar(255) NOT NULL,
    index_type			tinyint NULL,
    index_name			varchar(255) NULL,
    partition_number	int NOT NULL,
    [rows]				bigint NULL,
    [type_desc]			varchar(255) NOT NULL,
    is_clustered		bit NULL,
    index_filter		varchar(max) NULL,
    PRIMARY KEY CLUSTERED (database_id, [object_id], hobt_id)
);

DECLARE @files TABLE (
    database_id			int NOT NULL,
    [file_id]			int NOT NULL,
    filetype			varchar(20) NOT NULL,
    [name]				varchar(255) NOT NULL,
    drive				char(3) NOT NULL,
    [filegroup]			varchar(255) NULL,
    size_mb				numeric(18, 2) NOT NULL,
    autogrow			numeric(18, 2) NULL,
    is_percent_growth	bit NOT NULL,
    max_size_mb			numeric(18, 2) NULL,
    used_size_mb		numeric(18, 2) NULL,
    total_bytes			bigint NULL,
    available_bytes		bigint NULL,
    file_system_type	varchar(100) NULL,
    primary_replica     sysname NULL,
    [availability_mode] tinyint NULL,
    suspend_state       varchar(255) NULL,
    queue_kb            bigint NULL,
    rpo                 bigint NULL,
    PRIMARY KEY CLUSTERED (database_id, [name])
);

DECLARE @parts TABLE (
    database_id			int NOT NULL,
    [object_id]			int NOT NULL,
    index_id			int NOT NULL,
    hobt_id				bigint NULL,
    boundary_id			int NOT NULL,
    boundary			varchar(max) NULL,
    [rows]				int NULL,
    PRIMARY KEY CLUSTERED (database_id, [object_id], index_id, boundary_id)
);

DECLARE #cur CURSOR FOR
    SELECT name,
        --- Databases, schemas, tables, number of rows
        'SELECT '+CAST(database_id AS varchar(max))+', o.[object_id], ISNULL(ix.index_id, 0), p.hobt_id,
            s.name+''.''+o.name, ix.[type] AS index_type, ix.name AS index_name, p.partition_number, p.[rows], o.type_desc,
            (CASE WHEN cix.[object_id] IS NULL THEN 0 ELSE 1 END) AS is_clustered,
            ix.filter_definition
         FROM ['+[name]+'].sys.objects AS o WITH (NOLOCK)
         INNER JOIN ['+[name]+'].sys.partitions AS p WITH (NOLOCK) ON o.[object_id]=p.[object_id]
         INNER JOIN ['+[name]+'].sys.schemas AS s WITH (NOLOCK) ON o.[schema_id]=s.[schema_id]
         LEFT JOIN ['+[name]+'].sys.indexes AS ix WITH (NOLOCK) ON p.[object_id]=ix.[object_id] AND p.index_id=ix.index_id
         LEFT JOIN ['+[name]+'].sys.indexes AS cix WITH (NOLOCK) ON p.[object_id]=cix.[object_id] AND cix.[type]=1
        ' AS [sql],

        --- Database files
        'USE ['+[name]+'];
         SELECT '+CAST(database_id AS varchar(max))+', f.[file_id], f.type_desc AS filetype,
            f.name, UPPER(LEFT(f.physical_name, 3)) AS drive,
            ds.name AS [filegroup], 1.0*f.size*8/1024 AS size_mb,
            (CASE WHEN f.is_percent_growth=1 THEN 1.0*f.growth
                ELSE 1.0*f.growth*8/1024 END) AS autogrow,
            f.is_percent_growth, ROUND(1.0*NULLIF(f.max_size, -1)*8/1024, 1) AS max_size_mb,
            1.0*FILEPROPERTY(f.name, ''SpaceUsed'')*8/1024 AS used_size_mb, '+
            (CASE WHEN @version>'10.5' OR
                @version='10.5' AND CAST(SERVERPROPERTY('ProductLevel') AS varchar(10))>='SP1' THEN '
            v.total_bytes, v.available_bytes, v.file_system_type' ELSE '
            NULL AS total_bytes, NULL AS available_bytes, NULL AS file_system_type' END)+', NULL, NULL, NULL, NULL, NULL
         FROM ['+name+'].sys.database_files AS f WITH (NOLOCK)
         LEFT JOIN sys.data_spaces AS ds WITH (NOLOCK) ON f.data_space_id=ds.data_space_id'+
            (CASE WHEN @version>'10.5' OR
                   @version='10.5' AND CAST(SERVERPROPERTY('ProductLevel') AS varchar(10))>='SP1' THEN '
         CROSS APPLY sys.dm_os_volume_stats('+CAST(database_id AS varchar(max))+', f.[file_id]) AS v' ELSE '' END)+
                --- In-memory OLTP tables:
                (CASE WHEN @version>='12.0' THEN '
                 UNION ALL
         SELECT '+CAST(database_id AS varchar(max))+', -1 AS [file_id], ''IN-MEMORY'' AS filetype,
            '''' AS [name], ''RAM'' AS drive,
            '''' AS [filegroup], 1.0*SUM(xtp.allocated_bytes)/(1024*1024) AS size_mb,
            NULL AS autogrow,
            0 AS is_percent_growth, NULL AS max_size_mb,
            1.0*SUM(xtp.used_bytes)/(1024*1024) AS used_size_mb,
            NULL AS total_bytes, NULL AS available_bytes, NULL AS file_system_type, NULL, NULL, NULL, NULL, NULL
         FROM sys.dm_db_xtp_memory_consumers AS xtp
         HAVING COUNT(*)>0' ELSE '' END) AS sql2,

        --- Partitioning schemes, functions, boundaries
        'WITH spc (data_space_id, function_id, pf_name, boundary_id, boundary_value_on_right, [value])
         AS (	SELECT ps.data_space_id, pf.function_id, pf.name AS pf_name, val.boundary_id, pf.boundary_value_on_right,
                (CASE SQL_VARIANT_PROPERTY(val.[value], ''BaseType'')
                    WHEN ''date'' THEN LEFT(CONVERT(varchar(max), val.[value], 120), 10)
                    WHEN ''datetime'' THEN CONVERT(varchar(max), val.[value], 120)
                    ELSE CAST(val.[value] AS varchar(max))
                END) AS [value]
            FROM ['+[name]+'].sys.partition_range_values AS val WITH (NOLOCK)
            INNER JOIN ['+[name]+'].sys.partition_functions AS pf WITH (NOLOCK) ON val.function_id = pf.function_id
            INNER JOIN ['+[name]+'].sys.partition_schemes AS ps WITH (NOLOCK) ON pf.function_Id=ps.function_id),

             rng (pf_name, data_space_id, boundary_id, lower_rng, upper_rng)
         AS (	SELECT	ISNULL(a.pf_name, b.pf_name),
                ISNULL(a.data_space_id, b.data_space_id),
                ISNULL(a.boundary_id+1, b.boundary_id),
                a.[value]+(CASE
                    WHEN a.boundary_value_on_right=1 THEN ''<=''
                    WHEN a.boundary_value_on_right=0 THEN ''<'' END),
                (CASE	WHEN b.boundary_value_on_right=1 THEN ''<''+b.[value]
                    WHEN b.boundary_value_on_right=0 THEN ''<=''+b.[value] END)
            FROM spc AS a
            FULL JOIN spc AS b ON
                a.function_id=b.function_id AND
                a.boundary_id+1=b.boundary_id)
        SELECT '+CAST(database_id AS varchar(max))+', ix.[object_id], ix.index_id, p.hobt_id, ISNULL(rng.boundary_id, 0),
            ISNULL(rng.lower_rng, '''')+c.name+ISNULL(rng.upper_rng, '''') AS boundary, p.[rows]
        FROM rng
        INNER JOIN ['+[name]+'].sys.indexes AS ix WITH (NOLOCK) ON ix.data_space_id=rng.data_space_id
        INNER JOIN ['+[name]+'].sys.index_columns AS ic WITH (NOLOCK) ON ix.[object_id]=ic.[object_id] AND ix.index_id=ic.index_id AND ic.partition_ordinal>0
        INNER JOIN ['+[name]+'].sys.columns AS c WITH (NOLOCK) ON ic.[object_id]=c.[object_id] AND ic.column_id=c.column_id
        LEFT  JOIN ['+[name]+'].sys.partitions AS p WITH (NOLOCK) ON ix.[object_id]=p.[object_id] AND ix.index_id=p.index_id AND rng.boundary_id=p.partition_number
        ' AS sql3,

        --- Databases, schemas, tables
        'SELECT '+CAST(database_id AS varchar(max))+', o.[object_id], s.name+''.''+o.name
         FROM ['+[name]+'].sys.objects AS o WITH (NOLOCK)
         INNER JOIN ['+[name]+'].sys.schemas AS s WITH (NOLOCK) ON o.[schema_id]=s.[schema_id]
         WHERE o.[type] NOT IN (''U'', ''S'')
        ' AS sql4

    FROM sys.databases WITH (NOLOCK)
    WHERE state_desc='ONLINE' AND database_id IN (
        SELECT resource_database_id
        FROM @tran_locks);
OPEN #cur;

FETCH NEXT FROM #cur INTO @name, @sql, @sql2, @sql3, @sql4;
WHILE (@@FETCH_STATUS=0) BEGIN;
    BEGIN TRY;
        INSERT INTO @database_objects EXEC(@sql);
    END TRY
    BEGIN CATCH;
        PRINT @name+'/schema: '+ERROR_MESSAGE();
    END CATCH;

    BEGIN TRY;
        INSERT INTO @files EXEC(@sql2);
    END TRY
    BEGIN CATCH;
        PRINT @name+'/database files: '+ERROR_MESSAGE();
    END CATCH;

    BEGIN TRY;
        INSERT INTO @parts EXEC(@sql3);
    END TRY
    BEGIN CATCH;
        PRINT @name+'/partitions: '+ERROR_MESSAGE();
    END CATCH;

    BEGIN TRY;
        INSERT INTO @database_procedures EXEC(@sql4);
    END TRY
    BEGIN CATCH;
        PRINT @name+'/tables: '+ERROR_MESSAGE();
    END CATCH;

    FETCH NEXT FROM #cur INTO @name, @sql, @sql2, @sql3, @sql4;
END;

CLOSE #cur;
DEALLOCATE #cur;


-------------------------------------------------------------------------------
--- Availability groups stuff:

UPDATE f SET f.primary_replica=ags.primary_replica, f.[availability_mode]=ar.[availability_mode], f.suspend_state=x.suspend_state, f.queue_kb=x.queue_kb, f.rpo=x.rpo
FROM @files AS f
INNER JOIN sys.databases AS db ON f.database_id=db.database_id
INNER JOIN sys.availability_replicas AS ar ON db.replica_id=ar.replica_id
--INNER JOIN sys.availability_groups AS ag ON ar.group_id=ag.group_id
INNER JOIN sys.dm_hadr_availability_group_states AS ags ON ar.group_id=ags.group_id
LEFT JOIN (
    SELECT database_id, group_id, (CASE MAX(suspend_reason_desc)
                WHEN 'SUSPEND_FROM_USER' THEN 'A user manually suspended data movement'
                WHEN 'SUSPEND_FROM_PARTNER' THEN 'The database replica is suspended after a forced failover'
                WHEN 'SUSPEND_FROM_REDO' THEN 'An error occurred during the redo phase'
                WHEN 'SUSPEND_FROM_APPLY' THEN 'An error occurred when writing the log to file (see error log)'
                WHEN 'SUSPEND_FROM_CAPTURE' THEN 'An error occurred while capturing log on the primary replica'
                WHEN 'SUSPEND_FROM_RESTART' THEN 'The database replica was suspended before the database was restarted (see error log)'
                WHEN 'SUSPEND_FROM_UNDO' THEN 'An error occurred during the undo phase (see error log)'
                WHEN 'SUSPEND_FROM_REVALIDATION' THEN 'Log change mismatch is detected on reconnection (see error log)'
                WHEN 'SUSPEND_FROM_XRF_UPDATE' THEN 'Unable to find the common log point (see error log)'
                ELSE MAX(suspend_reason_desc)
                END) AS suspend_state, MAX(log_send_queue_size) AS queue_kb, DATEDIFF(second, MIN(last_commit_time), MAX(last_commit_time)) AS rpo
    FROM sys.dm_hadr_database_replica_states
    GROUP BY database_id, group_id) AS x ON f.database_id=x.database_id AND ar.group_id=x.group_id;

-------------------------------------------------------------------------------
--- Beautify names of temp tables:
UPDATE @database_objects
SET [name]=SUBSTRING(LEFT([name], CHARINDEX(REPLICATE('_', 8), [name]+REPLICATE('_', 8))-1), 5, LEN([name]))
WHERE database_id=(SELECT database_id FROM sys.databases WITH (NOLOCK) WHERE [name]='tempdb') AND
    REPLACE([name], '_', '.') LIKE 'dbo.#%'+REPLICATE('.', 8)+'[0-9A-F][0-9A-F]%[0-9A-F][0-9A-F]';

--- Beautify names of temp table indexes:
UPDATE @database_objects
SET index_name='Primary key'
WHERE database_id=(SELECT database_id FROM sys.databases WITH (NOLOCK) WHERE [name]='tempdb') AND
    LEFT(index_name, 12) LIKE 'PK__'+LEFT([name], 8);

UPDATE @database_objects
SET index_name=REPLACE(RTRIM(REPLACE(LEFT(index_name, LEN(index_name)-16), '_', ' ')), ' ', '_')
WHERE database_id=(SELECT database_id FROM sys.databases WITH (NOLOCK) WHERE [name]='tempdb') AND
    REPLACE(index_name, '_', '.') LIKE '%#%'+REPLICATE('[0-9A-F]', 16);



-------------------------------------------------------------------------------
--- Chains of spids that are being blocked, per spid:
WITH blocks (spid, blocked_by, ord)
AS (
    SELECT DISTINCT session_id AS spid, blocking_session_id AS blocked_by,
        DENSE_RANK() OVER (PARTITION BY blocking_session_id ORDER BY session_id) AS ord
    FROM @os_waiting_tasks
    WHERE blocking_session_id IS NOT NULL AND session_id!=blocking_session_id),

     list (spid, blocking, list, ord)
AS (
    SELECT blocked_by AS spid, spid AS blocking, CAST(spid AS varchar(max)) AS list, ord
    FROM blocks
    WHERE ord=1
    UNION ALL
    SELECT c.spid, c.blocking, CAST(c.list+', '+CAST(b.spid AS varchar(max)) AS varchar(max)) AS list, b.ord
    FROM list AS c
    INNER JOIN blocks AS b ON
        c.spid=b.blocked_by AND
        c.ord+1=b.ord)

INSERT INTO @blocking (spid, blocking)
SELECT spid, list AS blocking
FROM list AS l
WHERE ord=(SELECT MAX(ord) FROM list WHERE spid=l.spid);



--- Spids that are blocking other spids (reverse-lookup)
WITH blocks (spid, blocked_by)
AS (
    SELECT DISTINCT session_id AS spid, blocking_session_id AS blocked_by
    FROM @os_waiting_tasks
    WHERE blocking_session_id IS NOT NULL AND session_id!=blocking_session_id),

     chain (spid, blocked_by, chain, lvl)
AS (
    SELECT spid, blocked_by, CAST(blocked_by AS varchar(max)) AS chain, 1 AS lvl
    FROM blocks
    UNION ALL
    SELECT c.spid, b.blocked_by, CAST(c.chain+' <- '+CAST(b.blocked_by AS varchar(max)) AS varchar(max)) AS chain, lvl+1
    FROM chain AS c
    INNER JOIN blocks AS b ON b.spid=c.blocked_by)

INSERT INTO @block_chain (spid, blocked_by)
SELECT spid, MAX('<- '+chain)
FROM chain AS c
WHERE lvl=(SELECT MAX(lvl) FROM chain WHERE spid=c.spid)
GROUP BY spid;



--- Compile locks
INSERT INTO @locks
SELECT
    tl.request_session_id AS spid,
    NULLIF(tl.request_owner_id, 0) AS tran_id,
    DENSE_RANK() OVER (
        PARTITION BY tl.request_session_id, tl.request_owner_id, obj.[object_id], obj.index_id, obj.partition_number
        ORDER BY (CASE WHEN tl.request_mode='GRANT' THEN 1 ELSE 2 END), tl.request_mode
        ) AS lock_ord,
    ISNULL(NULLIF(tl.request_status, 'GRANT')+' ', '')+tl.request_mode+' '+(CASE tl.resource_type
        WHEN 'DATABASE' THEN 'db'
        WHEN 'FILE' THEN 'file'
        WHEN 'OBJECT' THEN ISNULL(LOWER(REPLACE(REPLACE(REPLACE(obj.type_desc, '_', ' '), 'SQL ', ''), 'USER ', '')), 'obj')
        WHEN 'PAGE' THEN 'page'
        WHEN 'KEY' THEN 'key'
        WHEN 'EXTENT' THEN 'extent'
        WHEN 'RID' THEN 'RID'
        WHEN 'APPLICATION' THEN 'app'
        WHEN 'METADATA' THEN 'metadata'
        WHEN 'HOBT' THEN 'HoBT'
        WHEN 'ALLOCATION_UNIT' THEN 'alloc unit'
        END) AS mode,
    db.name AS [database], ISNULL(obj.[object_id], 0), obj.name AS [object_name],
    ISNULL(obj.index_id, 0), obj.index_type, obj.index_name, obj.index_filter,
    (CASE	WHEN obj.type_desc='USER_TABLE' AND obj.is_clustered=1 THEN 'Clustered'
        WHEN obj.type_desc='USER_TABLE' THEN 'Heap'
        WHEN obj.type_desc='VIEW' AND obj.is_clustered=1 THEN 'Indexed view'
        ELSE '' END) AS object_notes,
    obj.[rows] AS obj_rows,
    ISNULL(obj.partition_number, 0), (SELECT MAX(partition_number)
            FROM @database_objects AS sub
            WHERE sub.database_id=obj.database_id AND
                sub.[object_id]=obj.[object_id] AND
                index_id=obj.index_id
            ) AS partition_count,
    parts.boundary AS partition_boundary,
    (CASE tl.request_owner_type
        WHEN 'TRANSACTION' THEN 'Trans'			    --- The request is owned by a transaction.
        WHEN 'CURSOR' THEN 'Cursor'			        --- The request is owned by a cursor.
        WHEN 'SESSION' THEN 'Session'			    --- The request is owned by a user session.
        WHEN 'SHARED_TRANSACTION_WORKSPACE' THEN 'Tran ws (shared)' --- The request is owned by the shared part of the transaction workspace.
        WHEN 'EXCLUSIVE_TRANSACTION_WORKSPACE' THEN 'Tran ws (excl)' --- The request is owned by the exclusive part of the transaction workspace.
        WHEN 'NOTIFICATION_OBJECT' THEN 'Internal'	--- The request is owned by an internal SQL Server component. This component has requested the lock manager to notify it when another component is waiting to take the lock. The FileTable feature is a component that uses this value.
        END) AS request_owner_type,
    wt.wait_type,
    dbo.fn_friendly_age(DATEADD(ms, 0-wt.wait_duration_ms, GETDATE()), GETDATE()) AS wait_time,
    wt.blocking_session_id AS blkd_by_spid,
    COUNT(*) AS [count],
    (CASE WHEN tl.request_status='WAIT' THEN 1 ELSE 0 END) AS isWaiting,
    0 AS isBlocking
FROM @tran_locks AS tl
INNER HASH JOIN sys.databases AS db WITH (NOLOCK) ON tl.resource_database_id=db.database_id
INNER HASH JOIN @tran_session_transactions AS st ON tl.request_owner_id=st.transaction_id
LEFT HASH JOIN @os_waiting_tasks AS wt ON tl.lock_owner_address=wt.resource_address
LEFT HASH JOIN @database_objects AS obj ON tl.resource_database_id=obj.database_id AND tl.resource_associated_entity_id IN (obj.[object_id], obj.hobt_id)
LEFT HASH JOIN @parts AS parts ON obj.database_id=parts.database_id AND parts.hobt_id=obj.hobt_id
WHERE tl.request_session_id!=@@SPID
GROUP BY tl.request_session_id, tl.request_owner_id, tl.request_status, tl.request_mode, tl.resource_type, db.name, obj.[object_id], obj.index_id, obj.index_type,
    obj.index_name, obj.index_filter, obj.type_desc, obj.[object_id], obj.database_id, obj.is_clustered, obj.name, obj.[rows],
    obj.partition_number, parts.boundary, tl.request_owner_type, wt.wait_type, wt.wait_duration_ms, wt.blocking_session_id;


--- Update @locks.isBlocking column.
UPDATE blocking
SET blocking.isBlocking=1
FROM @locks AS blocked
INNER JOIN @locks AS blocking ON
    blocked.[database]=blocking.[database] AND
    ISNULL(blocked.[object_id], -1)=ISNULL(blocking.[object_id], -1) AND
    ISNULL(blocked.index_id, -1)=ISNULL(blocking.index_id, -1) AND
    ISNULL(blocked.partition_number, -1)=ISNULL(blocking.partition_number, -1) AND
    blocked.tran_id!=blocking.tran_id AND
    blocked.spid!=blocking.spid AND
    blocked.isWaiting=1;



SET LOCK_TIMEOUT -1;

-------------------------------------------------------------------------------
--- 1. Server properties:
-------------------------------------------------------------------------------

SELECT
    CAST(SERVERPROPERTY('ComputerNamePhysicalNetBIOS') AS varchar(128)) AS [Physical name],
    CAST(SERVERPROPERTY('ServerName') AS varchar(128)) AS [Instance name],
    (CASE CAST(SERVERPROPERTY('IsClustered') AS bit) WHEN 1 THEN ISNULL('Cluster node '+sqlsrv.cluster_nodename, 'Clustered') ELSE 'Stand-alone' END)+
    (CASE WHEN SERVERPROPERTY('IsHadrEnabled')=1 THEN ' with Availability Groups' ELSE '' END)+
    (CASE @virtual_machine_type
        WHEN 1 THEN ' on Hypervisor'
        WHEN 2 THEN ' on virtual machine'
        ELSE '' END) AS [Configuration],
    'SQL Server '+(CASE @version
        WHEN '09.0' THEN '2005'
        WHEN '10.0' THEN '2008'
        WHEN '10.5' THEN '2008 R2'
        WHEN '11.0' THEN '2012'
        WHEN '12.0' THEN '2014'
        WHEN '13.0' THEN '2016' ELSE @version END)+' '+
        REPLACE(REPLACE(REPLACE(
            CAST(SERVERPROPERTY('Edition') AS varchar(128)), ' Edition', ''),
            'Standard', 'Std'), 'Enterprise', 'Ent') AS [Product, edition],
    CAST(SERVERPROPERTY('ProductLevel') AS varchar(128))+ISNULL(', '+
        CAST(SERVERPROPERTY('ProductUpdateLevel') AS varchar(128)), '')+' ('+
        CAST(SERVERPROPERTY('ProductVersion') AS varchar(128))+')' AS [Level, CU],
    (CASE WHEN @max_server_memory/(1024*1024)>2000000 THEN ''
          ELSE
            (CASE WHEN @min_server_memory<@max_server_memory AND @min_server_memory>0
                THEN LTRIM(dbo.fn_friendly_size(@min_server_memory))+' - '
                ELSE '' END)+
            LTRIM(dbo.fn_friendly_size(@max_server_memory))+
            (CASE WHEN @max_server_memory!=@physical_memory THEN ' / ' ELSE '' END) END)+
        LTRIM(dbo.fn_friendly_size(@physical_memory)) AS [Min - max / physical mem],
--	dbo.fn_friendly_size(@virtual_memory) AS [Virtual mem],
    CAST(sysinfo.cpu_count/sysinfo.hyperthread_ratio AS varchar(10))+
        ISNULL('x'+CAST(NULLIF(sysinfo.hyperthread_ratio, 1) AS varchar(10)), '')+
        ISNULL(' ('+CAST((SELECT NULLIF(MAX(memory_node_id), 0)+1
                          FROM sys.dm_os_memory_clerks
                          WHERE memory_node_id<64) AS varchar(10))+' NUMA)', '') AS [Core count],
    ISNULL(CAST(NULLIF(@max_degree_of_parallelism, 0) AS varchar(10)), '-') AS [MaxDOP],
    dbo.fn_friendly_age(sysinfo.sqlserver_start_time, GETDATE()) AS Uptime,
    CAST(SERVERPROPERTY('Collation') AS varchar(128)) AS [Server collation],
    sqlsrv.service_account AS [Service acct],
--  sqlagt.service_account AS [Agent svc acct],
    sqlagt.status_desc+' ('+sqlagt.startup_type_desc+')' AS [SQL Server Agent]
FROM sys.dm_os_sys_info AS sysinfo WITH (NOLOCK)
LEFT JOIN @dm_server_services AS sqlsrv ON sqlsrv.servicename LIKE 'SQL Server (%'
LEFT JOIN @dm_server_services AS sqlagt ON sqlagt.servicename LIKE 'SQL Server Agent (%';



-------------------------------------------------------------------------------
--- 2. Display database file usages:
-------------------------------------------------------------------------------

SELECT
    db.name AS [Database],
    UPPER(LEFT(f.filetype, 1))+LOWER(SUBSTRING(f.filetype, 2, 100)) AS [Type],
    COALESCE(
        (CASE WHEN f.primary_replica=@@SERVERNAME THEN 'Primary AG replica' WHEN f.primary_replica IS NOT NULL THEN (CASE f.[availability_mode] WHEN 1 THEN 'Synchronous AG replica' WHEN 0 THEN 'Async AG replica' ELSE '?' END) END),
        LEFT(m.mirroring_role_desc, 1)+LOWER(SUBSTRING(m.mirroring_role_desc, 2, 100)+
        ', '+REPLACE(m.mirroring_state_desc, '_', ' '))+
        (CASE m.mirroring_safety_level WHEN 1 THEN ' (async)' WHEN 2 THEN '(sync)' ELSE '(unknown)' END), '') AS [Mirror/AG],
    SUBSTRING(
        ISNULL(', '+f.suspend_state, '')+
        ISNULL(', '+LTRIM(dbo.fn_friendly_size(NULLIF(f.queue_kb, 0)*1024))+' queue', '')+
        ISNULL(', '+LTRIM(dbo.fn_friendly_age('00:00:00', DATEADD(second, NULLIF(f.rpo, 0), '00:00:00')))+' RPO', ''),
        3, 1000) [AG state],
    (CASE WHEN f.filetype='LOG' AND db.log_reuse_wait_desc!='NOTHING' THEN LEFT(db.log_reuse_wait_desc, 1)+LOWER(SUBSTRING(REPLACE(db.log_reuse_wait_desc, '_', ' '), 2, 1000)) ELSE '' END) AS [Log wait],
    UPPER(LEFT(db.recovery_model_desc, 1))+LOWER(SUBSTRING(db.recovery_model_desc, 2, 100)) AS [Recovery model],
    f.name AS [File name],
    dbo.fn_friendly_size(f.size_mb*@mb) AS [Allocated size], ISNULL((CASE
                WHEN f.filetype='IN-MEMORY' THEN 'n/a'
                WHEN f.is_percent_growth=1 THEN dbo.fn_format_number(NULLIF(f.autogrow, 0), 1)+'%'
        ELSE dbo.fn_friendly_size(NULLIF(f.autogrow, 0)*1.0*@mb) END), 'None') AS [Autogrow],
    (CASE WHEN f.max_size_mb=0 THEN ''
        WHEN f.max_size_mb IS NULL AND f.filetype='Rows' AND CAST(SERVERPROPERTY('Edition') AS varchar(128)) LIKE '%Express edition%' AND @version>='10.50' THEN dbo.fn_friendly_size(10*@gb)+' (Express ed.)'
        WHEN f.max_size_mb IS NULL AND f.filetype='Rows' AND CAST(SERVERPROPERTY('Edition') AS varchar(128)) LIKE '%Express edition%' THEN dbo.fn_friendly_size(4*@gb)+' (Express ed.)'
        WHEN f.max_size_mb IS NULL AND f.filetype='IN-MEMORY' THEN 'n/a'
                WHEN f.max_size_mb IS NULL THEN '('+ISNULL(f.file_system_type, 'OS')+' max)'
        WHEN f.max_size_mb=268435456/128 THEN '(Log max, 2 TB)'
        ELSE ISNULL(dbo.fn_friendly_size(f.max_size_mb*@mb), '') END) AS [Max file size],
    ISNULL(dbo.fn_friendly_size(f.used_size_mb*@mb), '') AS [File usage],
    ISNULL(dbo.fn_format_number(100.0*f.used_size_mb/f.size_mb, 2)+'%', '') AS [File usage %],
    f.drive AS [Drive],
    ISNULL(LTRIM(dbo.fn_friendly_size(1.0*f.available_bytes))+' of '+
           LTRIM(dbo.fn_friendly_size(1.0*f.total_bytes)), '') AS [Free disk space]
FROM @files AS f
INNER JOIN master.sys.databases AS db WITH (NOLOCK) ON db.database_id=f.database_id
INNER JOIN master.sys.database_mirroring AS m WITH (NOLOCK) ON db.database_id=m.database_id
WHERE f.database_id IN (
    SELECT database_id
    FROM @exec_requests
    UNION
    SELECT DISTINCT resource_database_id
    FROM @tran_locks
    WHERE request_session_id IN (
        SELECT session_id
        FROM @exec_sessions
        WHERE login_name=SUSER_SNAME()))
ORDER BY db.name, (CASE f.filetype
    WHEN 'ROWS' THEN 0
    WHEN 'LOG' THEN 1
    WHEN 'FILESTREAM' THEN 2
    WHEN 'IN-MEMORY' THEN 3 ELSE 0 END), f.name;



-------------------------------------------------------------------------------
--- 3. Display all sessions:
-------------------------------------------------------------------------------

SELECT
    xc.session_id AS SPID,
    ISNULL('"'+job.job_name+'", step '+CAST(job.step_id AS varchar(max))+' "'+job.step_name+'"',
        (CASE xs.[program_name]
            WHEN 'Microsoft SQL Server Management Studio - Query' THEN 'SSMS Query'
            WHEN 'Microsoft SQL Server Management Studio' THEN 'SSMS'
            ELSE xs.[program_name]
            END)) AS [Application],
--	xs.client_interface_name AS [Interface],
--	ISNULL(NULLIF(xc.client_net_address, xc.local_net_address), 'local')+ISNULL(':'+CAST(xc.client_tcp_port AS varchar(max)), '') AS [Client addr],
    xs.login_name+ISNULL(' ('+xc.auth_scheme+')', '') AS [Login name (auth)],
    dbo.fn_friendly_age(xc.connect_time, GETDATE()) AS [Conn. age],
    ISNULL(dbo.fn_friendly_size(NULLIF(xs.logical_reads*8192, 0)), '') AS [Logical reads],
    ISNULL(dbo.fn_friendly_size(NULLIF(xs.reads*8192, 0)), '') AS [Reads],
    ISNULL(dbo.fn_friendly_size(NULLIF(xs.writes*8192, 0)), '') AS [Writes],
    ISNULL(dbo.fn_friendly_age(0, DATEADD(ms, NULLIF(xs.cpu_time, 0), 0)), '') AS [CPU time],
    ISNULL(NULLIF(xs.[status], 'sleeping'), '') AS [State],
    ISNULL(CAST(NULLIF(trn.[count], 0) AS varchar(10)), '') AS [Tran count],
    ISNULL(blocking.blocking, '') AS [Blocking],
    ISNULL(chain.blocked_by, '') AS [Blocked by],
    ISNULL(SUBSTRING(cur.x.value('.', 'varchar(1000)'), 3, 1000), '') AS [Cursors],
    ISNULL(CAST(NULLIF((SELECT COUNT(*)
        FROM @os_tasks AS t
        WHERE t.session_id=xc.session_id), 0) AS varchar(10)), '') AS [Schedulers (DOP)],
    (CASE WHEN ISNULL(trn.[count], 0)>0 AND xs.[status]='sleeping' THEN dbo.fn_friendly_age(xs.last_request_start_time, GETDATE()) ELSE '' END) AS [Abandoned?],
    xs.flags AS [Connection/ANSI flags]
FROM @exec_connections AS xc
LEFT JOIN @exec_sessions AS xs ON xc.session_id=xs.session_id
LEFT JOIN @jobs AS job ON xs.[program_name] LIKE '%'+job.search_name+'%'
LEFT JOIN @block_chain AS chain ON chain.spid=xc.session_id
LEFT JOIN @blocking AS blocking ON blocking.spid=xc.session_id
LEFT JOIN (
    SELECT session_id, COUNT(*) AS [count]
    FROM @tran_session_transactions
    GROUP BY session_id) AS trn ON xc.session_id=trn.session_id
CROSS APPLY (
    SELECT ', '+[name]
    FROM sys.dm_exec_cursors(xc.session_id)
    FOR XML PATH(''), TYPE) AS cur(x)
WHERE xc.session_id!=@@SPID
ORDER BY (CASE
    WHEN xs.[status] NOT IN ('sleeping', 'dormant') OR ISNULL(trn.[count], 0)>0 THEN 1
    ELSE 2 END), xc.session_id;



-------------------------------------------------------------------------------
--- 4. Running queries:
-------------------------------------------------------------------------------

SELECT
    xc.session_id AS SPID,
    ISNULL('"'+job.job_name+'", step '+CAST(job.step_id AS varchar(max))+' "'+job.step_name+'"',
        (CASE xs.[program_name]
            WHEN 'Microsoft SQL Server Management Studio - Query' THEN 'SSMS Query'
            WHEN 'Microsoft SQL Server Management Studio' THEN 'SSMS'
            ELSE xs.[program_name]
            END)) AS [Application],
    ISNULL(dbobj.name, '') AS [Proc],
    (CASE WHEN xs.[status] NOT IN ('sleeping', 'dormant') THEN (CASE WHEN cmd.[encrypted]=1
        THEN xr.command+' (encrypted)'
        ELSE COALESCE(
            SUBSTRING(cmd.[text], xr.statement_start_offset/2, NULLIF(xr.statement_end_offset, -1)/2-xr.statement_start_offset/2),
            cmd.[text],
            xr.command COLLATE database_default,
            '') END) ELSE '('+xs.[status]+')' END) AS [Command/T-SQL],
    (CASE WHEN xs.[status]!='sleeping' THEN dbo.fn_friendly_age(xs.last_request_start_time, GETDATE()) ELSE '' END) AS [Stmt age],
    ISNULL(dbo.fn_friendly_size(mem.max_used_mb)+(CASE WHEN mem.max_used_mb<mem.granted_mb
        THEN ' of '+LTRIM(dbo.fn_friendly_size(mem.granted_mb*@mb))
        ELSE '' END)+(CASE WHEN mem.granted_mb<mem.requested_mb
        THEN ' ('+LTRIM(dbo.fn_friendly_size(mem.requested_mb*@mb))+' requested)'
        ELSE '' END), '') AS [Max used/granted memory],
    ISNULL(dbo.fn_friendly_size(mem.used_mb), '') AS [Used mem],
    ISNULL(dbo.fn_friendly_size(NULLIF(mem.ideal_mb, mem.granted_mb)), '') AS [Ideal mem],
    ISNULL(dbo.fn_format_number(mem.est_query_cost, 2), '') AS [Est. cost],
    ISNULL(dbo.fn_format_number(NULLIF(xr.percent_complete, 0.0), 1)+'%', '') AS [Progress],
    ISNULL(dbo.fn_friendly_age(GETDATE(), DATEADD(ms, 100.0*xr.total_elapsed_time/NULLIF(xr.percent_complete, 0.0), xs.last_request_start_time)), '') AS [Est remaining],
    [plan].query_plan AS [Query plan]
FROM @exec_connections AS xc
CROSS APPLY sys.dm_exec_sql_text(xc.most_recent_sql_handle) AS cmd
LEFT JOIN @exec_sessions AS xs ON xc.session_id=xs.session_id
LEFT JOIN @jobs AS job ON xs.[program_name] LIKE '%'+job.search_name+'%'
LEFT JOIN @exec_requests AS xr ON xc.session_id=xr.session_id
OUTER APPLY sys.dm_exec_query_plan(xr.plan_handle) AS [plan]
LEFT JOIN @block_chain AS chain ON chain.spid=xc.session_id
LEFT JOIN @blocking AS blocking ON blocking.spid=xc.session_id
LEFT JOIN @exec_query_memory_grants AS mem ON xc.session_id=mem.session_id
LEFT JOIN @database_procedures AS dbobj ON dbobj.database_id=cmd.dbid AND dbobj.[object_id]=cmd.objectid
WHERE xc.session_id!=@@SPID AND (
    xs.[status]!='sleeping' OR
    xc.session_id IN (SELECT session_id FROM @tran_session_transactions))
ORDER BY xc.session_id



-------------------------------------------------------------------------------
--- 5. Display open transactions:
-------------------------------------------------------------------------------

SELECT
    st.session_id AS SPID,
    act.transaction_id AS [Transaction],
    dbo.fn_friendly_age(act.transaction_begin_time, GETDATE()) AS [Transaction age],
    REPLACE((CASE st.is_local
            WHEN 0 THEN 'Distr '
            WHEN 1 THEN 'Local ' END)+
        (CASE act.transaction_type
            WHEN 1 THEN 'r/w'
            WHEN 2 THEN 'read'
            WHEN 3 THEN 'sys'
            WHEN 4 THEN 'distr'
            END), 'Distr distr', 'Distr') AS [Transaction type],
    (CASE act.transaction_state
        WHEN 0 THEN 'None'	        --- The transaction has not been completely initialized yet.
        WHEN 1 THEN 'Init'	        --- The transaction has been initialized but has not started.
        WHEN 2 THEN 'Active'	    --- The transaction is active.
        WHEN 3 THEN 'Ended'	        --- The transaction has ended. This is used for read-only transactions.
        WHEN 4 THEN 'Distr commit'  --- The commit process has been initiated on the distributed transaction. This is for distributed transactions only. The distributed transaction is still active but further processing cannot take place.
        WHEN 5 THEN 'Prepared'	    --- The transaction is in a prepared state and waiting resolution.
        WHEN 6 THEN 'Committed'	    --- The transaction has been committed.
        WHEN 7 THEN 'Rolling back'  --- The transaction is being rolled back.
        WHEN 8 THEN 'Rolled back'   --- The transaction has been rolled back.
        END) AS [State],
    (CASE xr.transaction_isolation_level
        WHEN 0 THEN '?'
        WHEN 1 THEN 'Read uncommitted'
        WHEN 2 THEN 'Read committed'
        WHEN 3 THEN 'Repeatable read'
        WHEN 4 THEN 'Serializable'
        WHEN 5 THEN 'Snapshot'
        ELSE ISNULL(CAST(xr.transaction_isolation_level AS varchar(10)), '')
        END) AS [Isolation level],
    ISNULL(CAST(dbo.fn_friendly_age(0, DATEADD(ms, (
        SELECT AVG(wt.wait_duration_ms)
        FROM @os_waiting_tasks AS wt
        WHERE wt.session_id=wt.blocking_session_id AND
            wt.session_id=st.session_id AND
            wt.wait_type='CXPACKET'), 0)) AS varchar(20)), 'None') AS [Avg cxPkt wait]
FROM @tran_session_transactions AS st
INNER JOIN @tran_active_transactions AS act ON st.transaction_id=act.transaction_id
LEFT JOIN @exec_requests AS xr ON st.session_id=xr.session_id
WHERE st.session_id!=@@SPID
ORDER BY st.session_id, act.transaction_id;



-------------------------------------------------------------------------------
--- 6. Display all locks in all sessions and databases:
-------------------------------------------------------------------------------

WITH rcte (spid, tran_id, lock_ord, mode, [count], [database], [object_id], [object_name], index_id, index_type, index_name, index_filter,
       partition_number, partition_count, partition_boundary, obj_rows, request_owner_type, wait_type, wait_time, isBlocking, isWaiting, lock_descr)
AS (
    SELECT spid, tran_id, lock_ord, mode, [count], [database], [object_id], [object_name], index_id, index_type, index_name, index_filter,
        partition_number, partition_count, partition_boundary, obj_rows, request_owner_type, wait_type, wait_time, isBlocking, isWaiting,
        CAST((CASE WHEN mode LIKE '%table' THEN '' ELSE LTRIM(ISNULL(dbo.fn_format_number(NULLIF([count], 1), 0)+' ', '')) END)+mode AS varchar(max))
    FROM @locks
    WHERE lock_ord=1
    UNION ALL
    SELECT rcte.spid, rcte.tran_id, lck.lock_ord, lck.mode, lck.[count], rcte.[database], rcte.[object_id], rcte.[object_name], rcte.index_id, rcte.index_type, rcte.index_name, rcte.index_filter,
        rcte.partition_number, rcte.partition_count, rcte.partition_boundary, rcte.obj_rows, rcte.request_owner_type, rcte.wait_type, rcte.wait_time,
        CAST((CASE WHEN 1 IN (rcte.isBlocking, lck.isBlocking) THEN 1 ELSE 0 END) AS bit),
        CAST((CASE WHEN 1 IN (rcte.isWaiting, lck.isWaiting) THEN 1 ELSE 0 END) AS bit),
        CAST(rcte.lock_descr+', '+(CASE WHEN lck.mode LIKE '%table' THEN '' ELSE LTRIM(ISNULL(dbo.fn_format_number(NULLIF(lck.[count], 1), 0)+' ', '')) END)+lck.mode AS varchar(max))
    FROM @locks AS lck
    INNER JOIN rcte ON
        rcte.lock_ord+1=lck.lock_ord AND
        rcte.spid=lck.spid AND
        rcte.tran_id=lck.tran_id AND
        rcte.[database]=lck.[database] AND
        rcte.[object_id]=lck.[object_id] AND
        ISNULL(rcte.index_id, -1)=ISNULL(lck.index_id, -1) AND
        rcte.partition_number=lck.partition_number)

SELECT spid AS SPID, tran_id AS [Transaction], lock_descr AS [Lock types],
    [database] AS [Database],
    ISNULL([object_name], '') AS [Object],
    ISNULL((CASE
        WHEN index_name='Primary key' AND index_id=1 THEN 'Primary key, clustered'
        WHEN index_type=5 THEN index_name+' (clustered colstore)'
        WHEN index_type=6 THEN index_name+' (colstore)'
        WHEN index_type=7 THEN index_name+' (hash)'
        WHEN index_id=1 THEN index_name+' (clustered)'
        WHEN index_id=0 OR index_id IS NULL THEN index_name+' (heap)' ELSE index_name END)+ISNULL(' '+index_filter, ''), '') AS [Index],
    ISNULL(STR(NULLIF(partition_number, 0), 4, 0)+'/'+CAST(NULLIF(partition_count, 1) AS varchar(10)), '') AS [Part'n],
    ISNULL(partition_boundary, '') AS [..bounds],
    ISNULL(dbo.fn_format_number(obj_rows, 0), '') AS [Est. object rows],
    request_owner_type AS [Req/trans owned by],
    ISNULL((CASE wait_type
        WHEN 'LCK_M_SCH_S' THEN 'Schema stability'
        WHEN 'LCK_M_SCH_M' THEN 'Schema modification'
        WHEN 'LCK_M_S' THEN	    'Share'
        WHEN 'LCK_M_U' THEN	    'Update'
        WHEN 'LCK_M_X' THEN	    'Exclusive'
        WHEN 'LCK_M_IS' THEN    'Intent-Share'
        WHEN 'LCK_M_IU' THEN    'Intent-Update'
        WHEN 'LCK_M_IX' THEN    'Intent-Exclusive'
        WHEN 'LCK_M_SIU' THEN   'Shared intent to update'
        WHEN 'LCK_M_SIX' THEN   'Share-Intent-Exclusive'
        WHEN 'LCK_M_UIX' THEN   'Update-Intent-Exclusive'
        WHEN 'LCK_M_BU' THEN    'Bulk Update'
        WHEN 'LCK_M_RS_S' THEN  'Range-share-share'
        WHEN 'LCK_M_RS_U' THEN  'Range-share-Update'
        WHEN 'LCK_M_RI_NL' THEN 'Range-Insert-NULL'
        WHEN 'LCK_M_RI_S' THEN  'Range-Insert-Shared'
        WHEN 'LCK_M_RI_U' THEN  'Range-Insert-Update'
        WHEN 'LCK_M_RI_X' THEN  'Range-Insert-Exclusive'
        WHEN 'LCK_M_RX_S' THEN  'Range-exclusive-Shared'
        WHEN 'LCK_M_RX_U' THEN  'Range-exclusive-update'
        WHEN 'LCK_M_RX_X' THEN  'Range-exclusive-exclusive'
        ELSE wait_type END), '') AS [Wait type],
    ISNULL(wait_time, '') AS [Wait time],
    (CASE WHEN isBlocking=1 THEN 'Blocking' ELSE '' END)+
    (CASE WHEN isBlocking=1 AND isWaiting=1 THEN ', ' ELSE '' END)+
    (CASE WHEN isWaiting=1 THEN 'Blocked' ELSE '' END) AS [Blocking/blocked]
FROM rcte
WHERE lock_ord=(SELECT MAX(lock_ord) FROM rcte AS x
        WHERE x.spid=rcte.spid AND x.tran_id=rcte.tran_id AND
            x.[database]=rcte.[database] AND
            ISNULL(x.[object_id], -1)=ISNULL(rcte.[object_id], -1) AND
            ISNULL(x.index_id, -1)=ISNULL(rcte.index_id, -1) AND
            ISNULL(x.partition_number, -1)=ISNULL(rcte.partition_number, -1))
ORDER BY spid, tran_id, [database], [object_name], index_id, partition_number, mode;



-------------------------------------------------------------------------------
--- Clean up:
-------------------------------------------------------------------------------

IF (XACT_STATE()!=-1 AND EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_friendly_size'))
    DROP FUNCTION dbo.fn_friendly_size;

IF (XACT_STATE()!=-1 AND EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_format_number'))
    DROP FUNCTION dbo.fn_format_number;

IF (XACT_STATE()!=-1 AND EXISTS (SELECT [object_id] FROM sys.objects WHERE [name]='fn_friendly_age'))
    DROP FUNCTION dbo.fn_friendly_age;




-------------------------------------------------------------------------------
--- CATCH
-------------------------------------------------------------------------------

END TRY
BEGIN CATCH;

    IF (EXISTS (SELECT session_id FROM sys.dm_exec_cursors(@@SPID))) BEGIN;
        CLOSE #cur;
        DEALLOCATE #cur;
    END;

    THROW;

    IF (@@TRANCOUNT!=0)
        ROLLBACK TRANSACTION;
    RETURN;

END CATCH;

ROLLBACK TRANSACTION;

GO
/*
Author: Tim Ford
Original link: http://sqlmag.com/database-administration/how-set-sql-server-database-mail-one-easy-script
*/

--====================================
-- DATABASE MAIL CONFIGURATION
--================================================================
--==========================================================
-- Create a Database Mail account
--==========================================================
EXECUTE msdb.dbo.sysmail_add_account_sp
    @account_name = '<account_name, DBM account name, Database Mail Primary Account>',
    @description = '<description, , SQL Server Notification Service>',
    @email_address = '<email_address, email address for DBM. Does not need a valid mail account ,>',
    @replyto_address = '<replyto_address, reply email address for DBM. Does not need a valid mail account ,>',
    @display_name = '<display_name, friendly name for emails sent via DBM, Database Mail Account>',
    @mailserver_name = '<mailserver_name, smtp mail server name,>',
        @port = '<port_number, port number of the mailserver, 25>';



--==========================================================
-- Create a Database Mail Profile
--==========================================================
DECLARE @profile_id INT, @profile_description sysname;
SELECT @profile_id = COALESCE(MAX(profile_id),1) FROM msdb.dbo.sysmail_profile
SELECT @profile_description = 'Database Mail Profile for ' + @@servername 


EXECUTE msdb.dbo.sysmail_add_profile_sp
    @profile_name = '<profile_name, DBM profile name, Database Mail Primary Profile>',
    @description = @profile_description;

-- Add the account to the profile
EXECUTE msdb.dbo.sysmail_add_profileaccount_sp
    @profile_name = '<profile_name, DBM profile name, Database Mail Primary Profile>',
    @account_name = '<account_name, DBM account name, Database Mail Primary Account>',
    @sequence_number = @profile_id;

-- Grant access to the profile to the DBMailUsers role
EXECUTE msdb.dbo.sysmail_add_principalprofile_sp
    @profile_name = '<profile_name, DBM profile name, Database Mail Primary Profile>',
    @principal_id = 0,
    @is_default = 1 ;


--==========================================================
-- Enable Database Mail
--==========================================================
USE master;
GO

sp_CONFIGURE 'show advanced', 1
GO
RECONFIGURE
GO
sp_CONFIGURE 'Database Mail XPs', 1
GO
RECONFIGURE
GO 


--EXEC master.dbo.xp_instance_regwrite N'HKEY_LOCAL_MACHINE', N'SOFTWARE\Microsoft\MSSQLServer\SQLServerAgent', N'DatabaseMailProfile', N'REG_SZ', N''
--EXEC master.dbo.xp_instance_regwrite N'HKEY_LOCAL_MACHINE', N'SOFTWARE\Microsoft\MSSQLServer\SQLServerAgent', N'UseDatabaseMail', N'REG_DWORD', 1
--GO

EXEC msdb.dbo.sp_set_sqlagent_properties @email_save_in_sent_folder = 0
GO


--==========================================================
-- Review Outcomes
--==========================================================
SELECT * FROM msdb.dbo.sysmail_profile;
SELECT * FROM msdb.dbo.sysmail_account;
GO


--==========================================================
-- Test Database Mail
--==========================================================
DECLARE @sub VARCHAR(100)
DECLARE @body_text NVARCHAR(MAX)
SELECT @sub = 'Test from New SQL install on ' + @@servername
SELECT @body_text = N'This is a test of Database Mail.' + CHAR(13) + CHAR(13) + 'SQL Server Version Info: ' + CAST(@@version AS VARCHAR(500))

EXEC msdb.dbo.[sp_send_dbmail] 
    @profile_name = '<profile_name, DBM profile name, Database Mail Primary Profile>'
  , @recipients = '<test_email_address, email address to send test email,>'
  , @subject = @sub
  , @body = @body_text





--================================================================
-- SQL Agent Properties Configuration
--================================================================
EXEC msdb.dbo.sp_set_sqlagent_properties 
        @databasemail_profile = '<profile_name, DBM profile name, Database Mail Primary Profile>'
        , @use_databasemail=1
GO
GO
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
SET NOCOUNT ON
SET ANSI_WARNINGS OFF
SET ARITHABORT OFF
SET ARITHIGNORE ON
SET TEXTSIZE 2147483647


-----------------------------------------------------------------------------------------------------------------------------
--	Script Details: Listing Of Standard Details Related To The Script
-----------------------------------------------------------------------------------------------------------------------------

--	Purpose: Date Calendar Cross-Reference Table
--	Create Date (MM/DD/YYYY): 10/29/2009
--	Developer: Sean Smith (s.smith.sql AT gmail DOT com)
--	Additional Notes: N/A


-----------------------------------------------------------------------------------------------------------------------------
--	Modification History: Listing Of All Modifications Since Original Implementation
-----------------------------------------------------------------------------------------------------------------------------

--	Description: Fixed Bug Affecting "month_weekdays_remaining" And "quarter_weekdays_remaining" Columns
--	Date (MM/DD/YYYY): 07/02/2014
--	Developer: Sean Smith (s.smith.sql AT gmail DOT com)
--	Additional Notes: N/A


-----------------------------------------------------------------------------------------------------------------------------
--	Declarations / Sets: Declare And Set Variables
-----------------------------------------------------------------------------------------------------------------------------

DECLARE
	 @Date_Start AS DATETIME
	,@Date_End AS DATETIME


SET @Date_Start = '20000101'


SET @Date_End = '20501231'


-----------------------------------------------------------------------------------------------------------------------------
--	Error Trapping: Check If Permanent Table(s) Already Exist(s) And Drop If Applicable
-----------------------------------------------------------------------------------------------------------------------------

IF OBJECT_ID (N'dbo.date_calendar', N'U') IS NOT NULL
BEGIN

	DROP TABLE dbo.date_calendar

END


-----------------------------------------------------------------------------------------------------------------------------
--	Permanent Table: Create Date Xref Table
-----------------------------------------------------------------------------------------------------------------------------

CREATE TABLE dbo.date_calendar

	(
		 calendar_date DATETIME NOT NULL CONSTRAINT PK_date_calendar_calendar_date PRIMARY KEY CLUSTERED
		,calendar_year SMALLINT NULL
		,calendar_month TINYINT NULL
		,calendar_day TINYINT NULL
		,calendar_quarter TINYINT NULL
		,first_day_in_week DATETIME NULL
		,last_day_in_week DATETIME NULL
		,is_week_in_same_month INT NULL
		,first_day_in_month DATETIME NULL
		,last_day_in_month DATETIME NULL
		,is_last_day_in_month INT NULL
		,first_day_in_quarter DATETIME NULL
		,last_day_in_quarter DATETIME NULL
		,is_last_day_in_quarter INT NULL
		,day_of_week TINYINT NULL
		,week_of_month TINYINT NULL
		,week_of_quarter TINYINT NULL
		,week_of_year TINYINT NULL
		,days_in_month TINYINT NULL
		,month_days_remaining TINYINT NULL
		,weekdays_in_month TINYINT NULL
		,month_weekdays_remaining TINYINT NULL
		,month_weekdays_completed TINYINT NULL
		,days_in_quarter TINYINT NULL
		,quarter_days_remaining TINYINT NULL
		,quarter_days_completed TINYINT NULL
		,weekdays_in_quarter TINYINT NULL
		,quarter_weekdays_remaining TINYINT NULL
		,quarter_weekdays_completed TINYINT NULL
		,day_of_year SMALLINT NULL
		,year_days_remaining SMALLINT NULL
		,is_weekday INT NULL
		,is_leap_year INT NULL
		,day_name VARCHAR (10) NULL
		,month_day_name_instance TINYINT NULL
		,quarter_day_name_instance TINYINT NULL
		,year_day_name_instance TINYINT NULL
		,month_name VARCHAR (10) NULL
		,year_week CHAR (6) NULL
		,year_month CHAR (6) NULL
		,year_quarter CHAR (6) NULL
	)


-----------------------------------------------------------------------------------------------------------------------------
--	Table Insert: Populate Base Date Values Into Permanent Table Using Common Table Expression (CTE)
-----------------------------------------------------------------------------------------------------------------------------

;WITH CTE_Date_Base_Table AS

	(
		SELECT
			@Date_Start AS calendar_date

		UNION ALL

		SELECT
			DATEADD (DAY, 1, cDBT.calendar_date)
		FROM
			CTE_Date_Base_Table cDBT
		WHERE
			DATEADD (DAY, 1, cDBT.calendar_date) <= @Date_End
	)

INSERT INTO dbo.date_calendar

	(
		calendar_date
	)

SELECT
	cDBT.calendar_date
FROM
	CTE_Date_Base_Table cDBT
OPTION
	(MAXRECURSION 0)


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update I: Populate Additional Date Xref Table Fields (Pass I)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	dbo.date_calendar
SET
	 calendar_year = DATEPART (YEAR, calendar_date)
	,calendar_month = DATEPART (MONTH, calendar_date)
	,calendar_day = DATEPART (DAY, calendar_date)
	,calendar_quarter = DATEPART (QUARTER, calendar_date)
	,first_day_in_week = DATEADD (DAY, -DATEPART (WEEKDAY, calendar_date) + 1, calendar_date)
	,first_day_in_month = CONVERT (VARCHAR (6), calendar_date, 112) + '01'
	,day_of_week = DATEPART (WEEKDAY, calendar_date)
	,week_of_year = DATEPART (WEEK, calendar_date)
	,day_of_year = DATEPART (DAYOFYEAR, calendar_date)
	,is_weekday = (CASE
						WHEN ((@@DATEFIRST - 1) + (DATEPART (WEEKDAY, calendar_date) - 1)) % 7 NOT IN (5, 6) THEN 1
						ELSE 0
						END)
	,day_name = DATENAME (WEEKDAY, calendar_date)
	,month_name = DATENAME (MONTH, calendar_date)


ALTER TABLE dbo.date_calendar ALTER COLUMN calendar_year INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN calendar_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN calendar_day INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN calendar_quarter INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN first_day_in_week DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN first_day_in_month DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN day_of_week INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN week_of_year INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN day_of_year INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN is_weekday INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN day_name VARCHAR (10) NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN month_name VARCHAR (10) NOT NULL


CREATE NONCLUSTERED INDEX IX_date_calendar_calendar_year ON dbo.date_calendar (calendar_year)


CREATE NONCLUSTERED INDEX IX_date_calendar_calendar_month ON dbo.date_calendar (calendar_month)


CREATE NONCLUSTERED INDEX IX_date_calendar_calendar_quarter ON dbo.date_calendar (calendar_quarter)


CREATE NONCLUSTERED INDEX IX_date_calendar_first_day_in_week ON dbo.date_calendar (first_day_in_week)


CREATE NONCLUSTERED INDEX IX_date_calendar_day_of_week ON dbo.date_calendar (day_of_week)


CREATE NONCLUSTERED INDEX IX_date_calendar_is_weekday ON dbo.date_calendar (is_weekday)


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update II: Populate Additional Date Xref Table Fields (Pass II)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	DC
SET
	 DC.last_day_in_week = DC.first_day_in_week + 6
	,DC.last_day_in_month = DATEADD (MONTH, 1, DC.first_day_in_month) - 1
	,DC.first_day_in_quarter = sqDC.first_day_in_quarter
	,DC.last_day_in_quarter = sqDC.last_day_in_quarter
	,DC.week_of_month = DATEDIFF (WEEK, DC.first_day_in_month, DC.calendar_date) + 1
	,DC.week_of_quarter = (DC.week_of_year - sqDC.min_week_of_year_in_quarter) + 1
	,DC.is_leap_year = (CASE
							WHEN DC.calendar_year % 400 = 0 THEN 1
							WHEN DC.calendar_year % 100 = 0 THEN 0
							WHEN DC.calendar_year % 4 = 0 THEN 1
							ELSE 0
							END)
	,DC.year_week = CONVERT (VARCHAR (4), DC.calendar_year) + RIGHT ('0' + CONVERT (VARCHAR (2), DC.week_of_year), 2)
	,DC.year_month = CONVERT (VARCHAR (4), DC.calendar_year) + RIGHT ('0' + CONVERT (VARCHAR (2), DC.calendar_month), 2)
	,DC.year_quarter = CONVERT (VARCHAR (4), DC.calendar_year) + 'Q' + CONVERT (VARCHAR (1), DC.calendar_quarter)
FROM
	dbo.date_calendar DC
	INNER JOIN

		(
			SELECT
				 DC.calendar_year
				,DC.calendar_quarter
				,MIN (DC.calendar_date) AS first_day_in_quarter
				,MAX (DC.calendar_date) AS last_day_in_quarter
				,MIN (DC.week_of_year) AS min_week_of_year_in_quarter
			FROM
				dbo.date_calendar DC
			GROUP BY
				 DC.calendar_year
				,DC.calendar_quarter
		) sqDC ON sqDC.calendar_year = DC.calendar_year AND sqDC.calendar_quarter = DC.calendar_quarter


ALTER TABLE dbo.date_calendar ALTER COLUMN last_day_in_week DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN last_day_in_month DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN first_day_in_quarter DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN last_day_in_quarter DATETIME NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN week_of_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN week_of_quarter INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN is_leap_year INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN year_week VARCHAR (6) NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN year_month VARCHAR (6) NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN year_quarter VARCHAR (6) NOT NULL


CREATE NONCLUSTERED INDEX IX_date_calendar_last_day_in_week ON dbo.date_calendar (last_day_in_week)


CREATE NONCLUSTERED INDEX IX_date_calendar_year_month ON dbo.date_calendar (year_month)


CREATE NONCLUSTERED INDEX IX_date_calendar_year_quarter ON dbo.date_calendar (year_quarter)


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update III: Populate Additional Date Xref Table Fields (Pass III)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	DC
SET
	 DC.is_last_day_in_month = (CASE
									WHEN DC.last_day_in_month = DC.calendar_date THEN 1
									ELSE 0
									END)
	,DC.is_last_day_in_quarter = (CASE
									WHEN DC.last_day_in_quarter = DC.calendar_date THEN 1
									ELSE 0
									END)
	,DC.days_in_month = DATEPART (DAY, DC.last_day_in_month)
	,DC.weekdays_in_month = sqDC1.weekdays_in_month
	,DC.days_in_quarter = DATEDIFF (DAY, DC.first_day_in_quarter, DC.last_day_in_quarter) + 1
	,DC.quarter_days_remaining = DATEDIFF (DAY, DC.calendar_date, DC.last_day_in_quarter)
	,DC.weekdays_in_quarter = sqDC2.weekdays_in_quarter
	,DC.year_days_remaining = (365 + DC.is_leap_year) - DC.day_of_year
FROM
	dbo.date_calendar DC
	INNER JOIN

		(
			SELECT
				 DC.year_month
				,SUM (DC.is_weekday) AS weekdays_in_month
			FROM
				dbo.date_calendar DC
			GROUP BY
				DC.year_month
		) sqDC1 ON sqDC1.year_month = DC.year_month

	INNER JOIN

		(
			SELECT
				 DC.year_quarter
				,SUM (DC.is_weekday) AS weekdays_in_quarter
			FROM
				dbo.date_calendar DC
			GROUP BY
				DC.year_quarter
		 ) sqDC2 ON sqDC2.year_quarter = DC.year_quarter


ALTER TABLE dbo.date_calendar ALTER COLUMN is_last_day_in_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN is_last_day_in_quarter INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN days_in_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN weekdays_in_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN days_in_quarter INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN quarter_days_remaining INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN weekdays_in_quarter INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN year_days_remaining INT NOT NULL


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update IV: Populate Additional Date Xref Table Fields (Pass IV)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	DC
SET
	 DC.month_weekdays_remaining = DC.weekdays_in_month - sqDC.month_weekdays_remaining_subtraction
	,DC.quarter_weekdays_remaining = DC.weekdays_in_quarter - sqDC.quarter_weekdays_remaining_subtraction
FROM
	dbo.date_calendar DC
	INNER JOIN

		(
			SELECT
				 DC.calendar_date
				,ROW_NUMBER () OVER
									(
										PARTITION BY
											DC.year_month
										ORDER BY
											DC.calendar_date
									) AS month_weekdays_remaining_subtraction
				,ROW_NUMBER () OVER
									(
										PARTITION BY
											DC.year_quarter
										ORDER BY
											DC.calendar_date
									) AS quarter_weekdays_remaining_subtraction
			FROM
				dbo.date_calendar DC
			WHERE
				DC.is_weekday = 1
		) sqDC ON sqDC.calendar_date = DC.calendar_date


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update V: Populate Additional Date Xref Table Fields (Pass V)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	DC
SET
	 DC.month_weekdays_remaining = (CASE
										WHEN DC1.calendar_month = DC.calendar_month AND DC1.month_weekdays_remaining IS NOT NULL THEN DC1.month_weekdays_remaining
										WHEN DC2.calendar_month = DC.calendar_month AND DC2.month_weekdays_remaining IS NOT NULL THEN DC2.month_weekdays_remaining
										ELSE DC.weekdays_in_month
										END)
	,DC.quarter_weekdays_remaining = (CASE
										WHEN DC1.calendar_quarter = DC.calendar_quarter AND DC1.quarter_weekdays_remaining IS NOT NULL THEN DC1.quarter_weekdays_remaining
										WHEN DC2.calendar_quarter = DC.calendar_quarter AND DC2.quarter_weekdays_remaining IS NOT NULL THEN DC2.quarter_weekdays_remaining
										ELSE DC.weekdays_in_quarter
										END)
FROM
	dbo.date_calendar DC
	LEFT JOIN dbo.date_calendar DC1 ON DATEADD (DAY, 1, DC1.calendar_date) = DC.calendar_date
	LEFT JOIN dbo.date_calendar DC2 ON DATEADD (DAY, 2, DC2.calendar_date) = DC.calendar_date
WHERE
	DC.month_weekdays_remaining IS NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN month_weekdays_remaining INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN quarter_weekdays_remaining INT NOT NULL


-----------------------------------------------------------------------------------------------------------------------------
--	Table Update VI: Populate Additional Date Xref Table Fields (Pass VI)
-----------------------------------------------------------------------------------------------------------------------------

UPDATE
	DC
SET
	 DC.is_week_in_same_month = sqDC.is_week_in_same_month
	,DC.month_days_remaining = DC.days_in_month - DC.calendar_day
	,DC.month_weekdays_completed = DC.weekdays_in_month - DC.month_weekdays_remaining
	,DC.quarter_days_completed = DC.days_in_quarter - DC.quarter_days_remaining
	,DC.quarter_weekdays_completed = DC.weekdays_in_quarter - DC.quarter_weekdays_remaining
	,DC.month_day_name_instance = sqDC.month_day_name_instance
	,DC.quarter_day_name_instance = sqDC.quarter_day_name_instance
	,DC.year_day_name_instance = sqDC.year_day_name_instance
FROM
	dbo.date_calendar DC
	INNER JOIN

		(
			SELECT
				 DC.calendar_date
				,(CASE
					WHEN DATEDIFF (MONTH, DC.first_day_in_week, DC.last_day_in_week) = 0 THEN 1
					ELSE 0
					END) AS is_week_in_same_month
				,ROW_NUMBER () OVER
									(
										PARTITION BY
											 DC.year_month
											,DC.day_name
										ORDER BY
											DC.calendar_date
									) AS month_day_name_instance
				,ROW_NUMBER () OVER
									(
										PARTITION BY
											 DC.year_quarter
											,DC.day_name
										ORDER BY
											DC.calendar_date
									) AS quarter_day_name_instance
				,ROW_NUMBER () OVER
									(
										PARTITION BY
											 DC.calendar_year
											,DC.day_name
										ORDER BY
											DC.calendar_date
									) AS year_day_name_instance
			FROM
				dbo.date_calendar DC
		) sqDC ON sqDC.calendar_date = DC.calendar_date


ALTER TABLE dbo.date_calendar ALTER COLUMN is_week_in_same_month INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN month_days_remaining INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN month_weekdays_completed INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN quarter_days_completed INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN quarter_weekdays_completed INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN month_day_name_instance INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN quarter_day_name_instance INT NOT NULL


ALTER TABLE dbo.date_calendar ALTER COLUMN year_day_name_instance INT NOT NULL


-----------------------------------------------------------------------------------------------------------------------------
--	Main Query: Final Display / Output
-----------------------------------------------------------------------------------------------------------------------------

SELECT
	DC.*
FROM
	dbo.date_calendar DC
ORDER BY
	DC.calendar_date

GO
/*
Author: Aaron Bertrand
Original links: https://www.mssqltips.com/sqlservertip/4052/build-a-cheat-sheet-for-sql-server-date-and-time-formats/
*/

SET NOCOUNT ON;

DECLARE @sql NVARCHAR(MAX), @v VARCHAR(30), @d DATETIME2(7), @sqlServerMajorVersion TINYINT;
SET @sql = N'';

-- a random date/time, making sure no single digits for any
-- date parts as these can truncate length of output:
SET @d = '2015-12-31T22:25:59.7901245';

SET @sqlServerMajorVersion = CAST(SERVERPROPERTY ('ProductMajorVersion') AS TINYINT);

CREATE TABLE #s(style VARCHAR(3));

-- for SQL Server < 2012
IF @sqlServerMajorVersion < 12
BEGIN

DECLARE @s INT = 0;
WHILE @s <= 255
BEGIN
  BEGIN TRY
    SET @sql = N'SELECT @v = CONVERT(VARCHAR(30), @d, ' + RTRIM(@s) + ');';
    EXEC sys.sp_executesql @sql, N'@v VARCHAR(30), @d DATETIME2(7)', @v, @d;
    INSERT #s(style) VALUES(@s);
  END TRY
  BEGIN CATCH
    SET @sql = N'';
  END CATCH
  SET @s = @s + 1;
END
END

-- for SQL Server >= 2012
ELSE
BEGIN
SET @sql = N'';

WITH x(rn) AS
(
  SELECT TOP (256) CONVERT(VARCHAR(3), ROW_NUMBER() OVER (ORDER BY name) - 1)
  FROM sys.all_objects ORDER BY name
)
SELECT @sql = @sql + N'INSERT #s SELECT ' + rn + ' FROM 
  (SELECT n = TRY_CONVERT(VARCHAR(30),@d,' + rn + ')) AS x
  WHERE n IS NOT NULL;' FROM x;

EXEC sys.sp_executesql @sql, N'@d DATETIME2(7)', @d;
END


SET @sql = N'';

SELECT @sql = @sql + N' UNION ALL SELECT [style #] = '
  + style + ', expression = N''CONVERT(CHAR(''
    +RTRIM(LEN(CONVERT(VARCHAR(30), @d, ' + style + ')))
    +''), @d, ' + style + ')'',
    [output] = CONVERT(VARCHAR(30), @d, ' + style + ')'
FROM #s;

SET @sql = STUFF(@sql, 1, 11, N'') + N';';

EXEC sys.sp_executesql @sql, N'@d DATETIME2(7)', @d;

DROP TABLE #s;

GO
/*
Author: Daniel Hutmacher
Original link: https://sqlsunday.com/2013/03/24/decrypting-sql-objects/

-- Enable Dedicated Administrator Connection
EXEC sp_configure 'remote admin connections', 1;
GO
RECONFIGURE
GO

-- Who using the Dedicated Admin Connection
SELECT
    CASE
    WHEN ses.session_id= @@SPID THEN 'It''s me! '
    ELSE '' END
    + coalesce(ses.login_name,'???') as WhosGotTheDAC,
    ses.session_id,
    ses.login_time,
    ses.status,
    ses.original_login_name
FROM sys.endpoints as en
INNER JOIN sys.dm_exec_sessions ses ON en.endpoint_id=ses.endpoint_id
WHERE en.name='Dedicated Admin Connection';
*/


SET NOCOUNT ON
DECLARE @owner sysname='dbo', @name sysname='sp_someprocedure';

-----------------------------------------------------------
--- Declarations:

DECLARE @offset            int=1;
DECLARE @datalength        int;
DECLARE @encrypted_object  nvarchar(max);
DECLARE @decrypted_object  nvarchar(max)=N'';
DECLARE @fake_object       nvarchar(max);
DECLARE @fake_encrypted_object nvarchar(max);
DECLARE @lf                nvarchar(max)=NCHAR(13)+NCHAR(10);
DECLARE @type              varchar(128);
DECLARE @object_id         int=OBJECT_ID('['+@owner+'].['+@name+']');
DECLARE @a int, @b int, @c int;

--- Check that the object exists
IF (@object_id IS NULL) BEGIN;
    RAISERROR('Object does not exist.', 16, 1);
    RETURN;
END;

--- Check that the object really is encrypted.
IF (NOT EXISTS (SELECT TOP 1 * FROM sys.sql_modules
        WHERE [object_id]=@object_id AND [definition] IS NULL)) BEGIN;
    RAISERROR('Object is not encrypted.', 16, 1);
    RETURN;
END;

--- Store the SQL type name of the object in @type
SELECT @type=(CASE [type]
    WHEN 'P' THEN 'PROCEDURE'
    WHEN 'TR' THEN 'TRIGGER'
    WHEN 'V' THEN 'VIEW'
    ELSE 'FUNCTION' END)
FROM sys.objects
WHERE [object_id]=@object_id;

--- @encrypted_object is the encrypted, binary, version of the code:
SELECT TOP 1 @encrypted_object=imageval
FROM sys.sysobjvalues
WHERE [objid]=@object_id AND valclass=1 and subobjid=1;

SET @datalength=DATALENGTH(@encrypted_object)/2;


--- We're going to ALTER the existing object to a "known plaintext"
--- with encryption. That way, we can reverse-engineer the encryption
--- key, using the new encrypted object.
--- All of this is done in a transaction that we'll roll back when
--- we're done with it.
SET @fake_object=N'ALTER '+@type+N' ['+@owner+N'].['+@name+N']
WITH ENCRYPTION AS
';

--- Fill the fake object with dashes ("-") until it's as long as
--- the encrypted object.
WHILE (DATALENGTH(@fake_object)/2<@datalength) BEGIN;
    IF (DATALENGTH(@fake_object)/2+4000<@datalength)
        SET @fake_object=@fake_object+REPLICATE(N'-', 4000);
    ELSE
        SET @fake_object=@fake_object+REPLICATE(N'-',
            @datalength-(DATALENGTH(@fake_object)/2));
END;

BEGIN TRANSACTION;
    --- Implement the fake encrypted object:
    EXEC(@fake_object);

    --- Retrieve the encrypted version of the "known plaintext".
    SELECT TOP 1 @fake_encrypted_object=imageval
    FROM sys.sysobjvalues
    WHERE [objid]=@object_id AND valclass=1 and subobjid=1;

    --- Now that we have the encrypted fake object, roll back
    --- the transaction, so we don't break the original object.
ROLLBACK TRANSACTION;


--- Change the @fake_object from ALTER to CREATE (because this is
--- how the encrypted objects are stored in the database!)
SET @fake_object='CREATE'+SUBSTRING(@fake_object, 6, LEN(@fake_object));

-----------------------------------------------------------
--- Perform decryption using the three versions: the encrypted
--- code, the plaintext fake code and the encrypted fake code.

WHILE (@offset<=@datalength) BEGIN;

    SELECT
        @a=UNICODE(SUBSTRING(@encrypted_object,      @offset, 1)),
        @b=UNICODE(SUBSTRING(@fake_object,           @offset, 1)),
        @c=UNICODE(SUBSTRING(@fake_encrypted_object, @offset, 1));

    SELECT @decrypted_object=@decrypted_object+NCHAR(@a^(@b^@c)),
        @offset=@offset+1;
END;


-----------------------------------------------------------
--- Print the results:

WHILE (@decrypted_object IS NOT NULL) BEGIN;
    PRINT LEFT(@decrypted_object,
        CHARINDEX(@lf, @decrypted_object+@lf)-1);

    SET @decrypted_object=NULLIF(
        SUBSTRING(@decrypted_object,
        CHARINDEX(@lf, @decrypted_object+@lf)+LEN(@lf),
            LEN(@decrypted_object)), '');
END;
GO

GO
/*
Author: BradC
Original link: https://dba.stackexchange.com/questions/7917/how-to-determine-used-free-space-within-sql-database-files
Desctiption: Determine used/free space within SQL database files
*/


--This one works for me and seems to be consistent on SQL 2000 to SQL Server 2012 CTP3:
SELECT RTRIM(name) AS [Segment Name], groupid AS [Group Id], filename AS [File Name],
   CAST(size/128.0 AS DECIMAL(10,2)) AS [Allocated Size in MB],
   CAST(FILEPROPERTY(name, 'SpaceUsed')/128.0 AS DECIMAL(10,2)) AS [Space Used in MB],
   CAST([maxsize]/128.0 AS DECIMAL(10,2)) AS [Max in MB],
   CAST([maxsize]/128.0-(FILEPROPERTY(name, 'SpaceUsed')/128.0) AS DECIMAL(10,2)) AS [Available Space in MB],
   CAST((CAST(FILEPROPERTY(name, 'SpaceUsed')/128.0 AS DECIMAL(10,2))/CAST([maxsize]/128.0 AS DECIMAL(10,2)))*100 AS DECIMAL(10,2)) AS [Percent Used]
FROM sysfiles
ORDER BY groupid DESC


--An alternative (not compatible with SQL Server 200) that provides more information, suggested by Tri Effendi SS:
USE [database name]
GO
SELECT 
    [TYPE] = A.TYPE_DESC
    ,[FILE_Name] = A.name
    ,[FILEGROUP_NAME] = fg.name
    ,[File_Location] = A.PHYSICAL_NAME
    ,[FILESIZE_MB] = CONVERT(DECIMAL(10,2),A.SIZE/128.0)
    ,[USEDSPACE_MB] = CONVERT(DECIMAL(10,2),A.SIZE/128.0 - ((SIZE/128.0) - CAST(FILEPROPERTY(A.NAME, 'SPACEUSED') AS INT)/128.0))
    ,[FREESPACE_MB] = CONVERT(DECIMAL(10,2),A.SIZE/128.0 - CAST(FILEPROPERTY(A.NAME, 'SPACEUSED') AS INT)/128.0)
    ,[FREESPACE_%] = CONVERT(DECIMAL(10,2),((A.SIZE/128.0 - CAST(FILEPROPERTY(A.NAME, 'SPACEUSED') AS INT)/128.0)/(A.SIZE/128.0))*100)
    ,[AutoGrow] = 'By ' + CASE is_percent_growth WHEN 0 THEN CAST(growth/128 AS VARCHAR(10)) + ' MB -' 
        WHEN 1 THEN CAST(growth AS VARCHAR(10)) + '% -' ELSE '' END 
        + CASE max_size WHEN 0 THEN 'DISABLED' WHEN -1 THEN ' Unrestricted' 
            ELSE ' Restricted to ' + CAST(max_size/(128*1024) AS VARCHAR(10)) + ' GB' END 
        + CASE is_percent_growth WHEN 1 THEN ' [autogrowth by percent, BAD setting!]' ELSE '' END
FROM sys.database_files A LEFT JOIN sys.filegroups fg ON A.data_space_id = fg.data_space_id 
order by A.TYPE desc, A.NAME; 
GO
/*
Author: Kenneth Fisher
Original link: https://sqlstudies.com/2017/06/21/query-to-run-command-line-dma-on-each-database
*/
-- Dynamically generate the command line DMA statement for each database

SELECT name, '"C:\Program Files\Microsoft Data Migration Assistant\DmaCmd.exe" ' + 
    '/AssessmentName="DMA_Output" ' +
    '/AssessmentDatabases="Server=' + @@ServerName +
    ';Initial Catalog=' + sys.databases.name +
    ';Integrated Security=true" ' + 
    '/AssessmentEvaluateCompatibilityIssues /AssessmentOverwriteResult ' + 
    '/AssessmentResultCSV="\\PathToSaveTo\'+REPLACE(@@ServerName,'\','_')+'\'+sys.databases.name+'.CSV"' +
    ' > "\\PathToSaveTo\'+REPLACE(@@ServerName,'\','_')+'\'+sys.databases.name+'.LOG"'
FROM sys.databases WHERE state <> 6 -- exclude offline databases
  and database_id > 4 -- Exclude system databases
  
GO
-- https://social.msdn.microsoft.com/Forums/en-US/1d5c04c7-157f-4955-a14b-41d912d50a64/how-to-fix-error-the-microsoftaceoledb120-provider-is-not-registered-on-the-local-machine

USE master;

EXEC sp_configure 'show advanced options', 1;
RECONFIGURE;
EXEC sp_configure 'Ad Hoc Distributed Queries', 1;
RECONFIGURE;
GO
EXEC sp_MSset_oledb_prop N'Microsoft.ACE.OLEDB.12.0', N'AllowInProcess', 1
GO 
EXEC sp_MSset_oledb_prop N'Microsoft.ACE.OLEDB.12.0', N'DynamicParameters', 1
GO

GO
/*
Author: Tim Ford
Original link: http://sqlmag.com/database-administration/estimate-when-long-running-sql-processes-will-finish
*/

SET NOCOUNT ON;

SELECT R.session_id
     , R.percent_complete
     , R.total_elapsed_time/1000 AS elapsed_seconds
     , R.wait_type
     , R.wait_time
     , R.last_wait_type
     , DATEADD(s,100/((R.percent_complete)/ (R.total_elapsed_time/1000)), R.start_time) AS est_complete_time
     , ST.text AS batch_text
     , CAST(SUBSTRING(ST.text, R.statement_start_offset / 2,
             (
               CASE WHEN R.statement_end_offset = -1 THEN DATALENGTH(ST.text)
               ELSE R.statement_end_offset
               END - R.statement_start_offset 
             ) / 2 
     ) AS varchar(1024)) AS statement_executing
FROM sys.dm_exec_requests AS R
        CROSS APPLY sys.dm_exec_sql_text(R.sql_handle) AS ST
WHERE R.percent_complete > 0
  AND R.session_id <> @@spid;

GO
/*
Author: Michael J Swart (@MJSwart)
Original link: https://sqlperformance.com/2017/07/sql-performance/find-database-connection-leaks
Desctiption: Find Database Connection Leaks in Your Application
*/

--Finding Your Connection Leaks

select count(*) as sessions,
         s.host_name,
         s.host_process_id,
         s.program_name,
         db_name(s.database_id) as database_name
   from sys.dm_exec_sessions s
   where is_user_process = 1
   group by host_name, host_process_id, program_name, database_id
   order by count(*) desc;
   
   
--Given a pool, are there sessions that have been sleeping for a while and, if so, 
--how long have they been sleeping and what was the last SQL statement they executed?

declare @host_process_id int = 1508;
  declare @host_name sysname = N'SERV4102';
  declare @database_name sysname = N'My_Database';
 
  select datediff(minute, s.last_request_end_time, getdate()) as minutes_asleep,
         s.session_id,
         db_name(s.database_id) as database_name,
         s.host_name,
         s.host_process_id,
         t.text as last_sql,
         s.program_name
    from sys.dm_exec_connections c
    join sys.dm_exec_sessions s
         on c.session_id = s.session_id
   cross apply sys.dm_exec_sql_text(c.most_recent_sql_handle) t
   where s.is_user_process = 1
         and s.status = 'sleeping'
         and db_name(s.database_id) = @database_name
         and s.host_process_id = @host_process_id
         and s.host_name = @host_name
         and datediff(second, s.last_request_end_time, getdate()) > 60
   order by s.last_request_end_time;

GO
/*
Author: Jonathan Kehayias
Original link: https://www.sqlskills.com/blogs/jonathan/finding-implicit-column-conversions-in-the-plan-cache
Desctiption: Finding Implicit Column Conversions in the Plan Cache
*/


SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED

DECLARE @dbname SYSNAME
SET @dbname = QUOTENAME(DB_NAME());

WITH XMLNAMESPACES
   (DEFAULT 'http://schemas.microsoft.com/sqlserver/2004/07/showplan')
SELECT
   stmt.value('(@StatementText)[1]', 'varchar(max)'),
   t.value('(ScalarOperator/Identifier/ColumnReference/@Schema)[1]', 'varchar(128)'),
   t.value('(ScalarOperator/Identifier/ColumnReference/@Table)[1]', 'varchar(128)'),
   t.value('(ScalarOperator/Identifier/ColumnReference/@Column)[1]', 'varchar(128)'),
   ic.DATA_TYPE AS ConvertFrom,
   ic.CHARACTER_MAXIMUM_LENGTH AS ConvertFromLength,
   t.value('(@DataType)[1]', 'varchar(128)') AS ConvertTo,
   t.value('(@Length)[1]', 'int') AS ConvertToLength,
   query_plan
FROM sys.dm_exec_cached_plans AS cp
CROSS APPLY sys.dm_exec_query_plan(plan_handle) AS qp
CROSS APPLY query_plan.nodes('/ShowPlanXML/BatchSequence/Batch/Statements/StmtSimple') AS batch(stmt)
CROSS APPLY stmt.nodes('.//Convert[@Implicit="1"]') AS n(t)
JOIN INFORMATION_SCHEMA.COLUMNS AS ic
   ON QUOTENAME(ic.TABLE_SCHEMA) = t.value('(ScalarOperator/Identifier/ColumnReference/@Schema)[1]', 'varchar(128)')
   AND QUOTENAME(ic.TABLE_NAME) = t.value('(ScalarOperator/Identifier/ColumnReference/@Table)[1]', 'varchar(128)')
   AND ic.COLUMN_NAME = t.value('(ScalarOperator/Identifier/ColumnReference/@Column)[1]', 'varchar(128)')
WHERE t.exist('ScalarOperator/Identifier/ColumnReference[@Database=sql:variable("@dbname")][@Schema!="[sys]"]') = 1

GO
/*
Original link: http://www.sqlservercentral.com/blogs/john-morehouse-sqlruscom/2017/04/25/quick-script-finding-specific-columns
*/

/***************************************************************
  Author: John Morehouse
  Summary: interrogate each database looking for a specific column name
 
  You may alter this code for your own purposes. You may republish altered code as long as you give due credit. 
 
  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
***************************************************************/
DECLARE @dbs AS TABLE ( name sysname, processed BIT)
DECLARE @x INT = 1
DECLARE @sql VARCHAR (2000)
DECLARE @dbName VARCHAR (50)

IF object_id ('tempdb..#results') IS NOT NULL
BEGIN
DROP TABLE #results
END

CREATE TABLE #results ( dbName sysname
, tableName VARCHAR (100)
, columnName VARCHAR (100)
, DataType VARCHAR (100)
, MaxDataLength INT
, theRowCount INT )

INSERT INTO @dbs ( name, processed )
SELECT name, 0 FROM sys.databases
WHERE database_id > 6
AND [state] = 0 --online

WHILE @x <= (SELECT COUNT( 1) FROM @dbs WHERE processed = 0 )
BEGIN
SELECT TOP 1 @dbName = name FROM @dbs WHERE processed = 0

SET @sql =
'INSERT #results (dbName, tableName, columnName, DataType, MaxDataLength, theRowCount)
SELECT ''' + @dbName + ''',t.name,c.name,st.name, c.max_length,p.rows
FROM [' + @dbName + '].sys.columns c
INNER JOIN [' + @dbName + '].sys.tables t ON c.object_id = t.object_id
INNER JOIN [' + @dbName + '].sys.systypes st ON c.user_type_ID = st.xusertype
INNER JOIN [' + @dbName + '].sys.partitions p ON t.object_id = p.object_ID
INNER JOIN [' + @dbName + '].sys.allocation_units au ON au.container_ID =
CASE
WHEN au.TYPE IN (1,3) THEN p.hobt_id
WHEN au.type IN (2) THEN p.partition_id
END
WHERE (c.name LIKE ''TIN%''
OR c.name LIKE ''TIN_TX%''
OR c.name LIKE ''%SSN%'') -- looking for specific column name
AND c.OBJECT_ID > 100 -- exluded system objects
AND st.name IN (''varchar'', ''nvarchar'') -- due to leading zeros, should be n/varchar data type
AND p.index_id IN (0,1) -- Heap or Clustered Index
AND au.type = 1 --IN_ROW_DATA only'

--PRINT @sql
BEGIN TRY
EXEC (@sql)
END TRY
BEGIN CATCH
SELECT ERROR_LINE () AS [Error Line]
, ERROR_MESSAGE () AS [Error Message]
, ERROR_NUMBER () AS [Error Number]
, @dbName AS [Database]
END CATCH

UPDATE @dbs
SET processed = 1
WHERE name = @dbName
END

SELECT * FROM #results
GO

GO
--Finding Blocking Queries
--Author:  Steve Stedman
--One of the common problems with when diagnosing what appears to be a slow scenario where there may be blocking involved is determining what is blocking on SQL Server
--This script works on SQL Server 2008, 2008R2, 2012, 2014, and 2016
--If there is no blocking occurring then this query will return nothing

SELECT * 
INTO #runningQueries
 FROM master.sysprocesses WITH (NOLOCK);
 
 
;WITH BlockingCTE as
(
 SELECT q1.blocked as spid
 FROM #runningQueries q1
 WHERE q1.blocked != 0 
 AND q1.blocked not in (SELECT spid FROM #runningQueries q2 WHERE q2.blocked != 0)
)
, recursiveBlocking AS
(
 SELECT b.spid, cast(0 as SMALLINT) as blocked, 
 cast(b.spid as varchar(1000)) as treePath, 0 as [level],
 sp1.sql_handle, b.spid as topBlock
 FROM BlockingCTE b
 INNER JOIN #runningQueries sp1 on sp1.spid = b.spid 
 
 UNION ALL
 
 SELECT sp.spid, rb.spid as blocked, 
 cast(rb.treePath + '-&gt;' + cast(sp.spid as varchar(1000)) as VARCHAR(1000)) as treePath, 
 [level] + 1 as [level], sp.sql_handle, topBlock
 FROM #runningQueries sp 
 INNER JOIN recursiveBlocking as rb ON rb.spid = sp.blocked
)
, topBlockCount AS
(
 SELECT *, count(1) over(partition by topBlock) as NumBlocked 
 FROM recursiveBlocking
)
SELECT DISTINCT
 tb.SPID,
 tb.blocked,
 tb.treePath as blockingChain,
 tb.[level],
 tb.topBlock,
 tb.NumBlocked, 
 LTRIM(REPLACE(REPLACE(st.text, char(10), ' '), char(13), ' ')) as theQuery
 FROM topBlockCount tb
 CROSS APPLY sys.dm_exec_sql_text(tb.sql_handle) AS st
 ORDER BY NumBlocked DESC, treePath
 OPTION (RECOMPILE);
 
 
DROP TABLE #runningQueries;

GO
/*
Author: Thomas LaRock
Original link: https://www.mssqltips.com/sqlservertip/4936/how-to-find-long-running-job-steps-in-microsoft-sql-server
Desctiption: How to Find Long Running Job Steps in Microsoft SQL Server
*/

/*============================================= 
  Variables: 
    @MinHistExecutions - Minimum number of job step executions we want to consider 
    @MinAvgSecsDuration - Threshold for minimum job step duration we care to monitor 
    @HistoryStartDate - Start date for historical average 
    @HistoryEndDate - End date for historical average 
  
  These variables allow for us to control a couple of factors. First 
  we can focus on job steps that are running long enough on average for 
  us to be concerned with (say, 30 seconds or more). Second, we can 
  avoid being alerted by job steps that have run so few times that the 
  average and standard deviations are not quite stable yet. This script 
  leaves these variables at 1.0, but I would advise you alter them 
  upwards after testing. 
  
  Returns: One result set containing a list of job steps that 
  are currently running and are running longer than two standard deviations 
  away from their historical average. The "Min Threshold" column 
  represents the average plus two standard deviations. 
  
  note [1] - comment this line and note [2] line if you want to report on all history for job steps 
  note [2] - comment just this line is you want to report on running and non-running job steps 
 =============================================*/ 
  
DECLARE   @HistoryStartDate datetime 
  ,@HistoryEndDate datetime  
  ,@MinHistExecutions int   
  ,@MinAvgSecsDuration int  
  
SET @HistoryStartDate = '19000101' 
SET @HistoryEndDate = GETDATE() 
SET @MinHistExecutions = 1.0 
SET @MinAvgSecsDuration = 1.0 
  
DECLARE @currently_running_jobs TABLE ( 
    job_id UNIQUEIDENTIFIER NOT NULL 
    ,last_run_date INT NOT NULL 
    ,last_run_time INT NOT NULL 
    ,next_run_date INT NOT NULL 
    ,next_run_time INT NOT NULL 
    ,next_run_schedule_id INT NOT NULL 
    ,requested_to_run INT NOT NULL 
    ,request_source INT NOT NULL 
    ,request_source_id SYSNAME NULL 
    ,running INT NOT NULL 
    ,current_step INT NOT NULL 
    ,current_retry_attempt INT NOT NULL 
    ,job_state INT NOT NULL 
    ) 
  
--capture details on jobs 
INSERT INTO @currently_running_jobs 
EXECUTE master.dbo.xp_sqlagent_enum_jobs 1,'' 
  
;WITH JobStepsHistData AS 
( 
  SELECT job_id, step_id 
,date_executed=msdb.dbo.agent_datetime(run_date, run_time) 
,secs_duration=run_duration/10000*3600 
                      +run_duration%10000/100*60 
                      +run_duration%100 
  FROM msdb.dbo.sysjobhistory 
  WHERE run_status = 1 -- Succeeded 
) 
,JobHistStats AS 
( 
  SELECT job_id, step_id 
        ,AvgDuration = AVG(secs_duration*1.) 
        ,AvgPlus2StDev = AVG(secs_duration*1.) + 2*stdevp(secs_duration) 
  FROM JobStepsHistData 
  WHERE date_executed >= DATEADD(day, DATEDIFF(day,'19000101',@HistoryStartDate),'19000101') 
  AND date_executed < DATEADD(day, 1 + DATEDIFF(day,'19000101',@HistoryEndDate),'19000101')   
  GROUP BY job_id, step_id 
  HAVING COUNT(*) >= @MinHistExecutions 
  AND AVG(secs_duration*1.) >= @MinAvgSecsDuration 
) 
-- need to select from the CTE's, and join to msdb for final result 
SELECT jd.job_id 
      ,j.name AS [JobName] 
      ,sjs.step_id 
    ,sjs.step_name 
      ,MAX(act.start_execution_date) AS [ExecutionDate] 
      ,AvgDuration AS [Historical Avg Duration (secs)] 
      ,AvgPlus2StDev AS [Min Threshhold (secs)] 
FROM JobStepsHistData jd 
JOIN JobHistStats jhs on jd.job_id = jhs.job_id AND jd.step_id = jhs.step_id 
JOIN msdb.sysjobs j on jd.job_id = j.job_id 
JOIN msdb.sysjobsteps sjs on jd.job_id = sjs.job_id AND jd.step_id = sjs.step_id 
JOIN @currently_running_jobs crj ON crj.job_id = jd.job_id --see note [1] above 
JOIN msdb.sysjobactivity AS act ON act.job_id = jd.job_id 
AND act.stop_execution_date IS NULL 
AND act.start_execution_date IS NOT NULL 
WHERE DATEDIFF(SS, act.start_execution_date, GETDATE()) > AvgPlus2StDev 
AND crj.job_state = 1 --see note [2] above 
GROUP BY jd.job_id, j.name, sjs.step_id, sjs.step_name, AvgDuration, AvgPlus2StDev

GO
/*
Author: Jana Sattainathan
Original link: https://sqljana.wordpress.com/2017/03/31/sql-server-find-tables-with-similar-table-structure
*/

WITH ColCountsByTable
AS
(
      SELECT
            c.TABLE_CATALOG,
            c.TABLE_SCHEMA,
            c.TABLE_NAME,
            COUNT(1) AS Column_Count
      FROM INFORMATION_SCHEMA.COLUMNS c
      /*
      --Plug in the schema and table name to get similar views/tables just for one or more tables
      WHERE
            c.TABLE_SCHEMA = 'TransactionPreProcessing'
            AND c.TABLE_NAME IN ('PreStagingTransaction')
      */
      GROUP BY
            c.TABLE_CATALOG,
            c.TABLE_SCHEMA,
            c.TABLE_NAME
)
SELECT
      100 * COUNT(c2.COLUMN_NAME) /*Matching_Column_Count*/ / MIN(ColCountsByTable.Column_Count) /*Column_Count*/ AS Match_Percent,
      c.TABLE_CATALOG,
      c.TABLE_SCHEMA,
      c.TABLE_NAME,
      DENSE_RANK() OVER(ORDER BY c.TABLE_CATALOG, c.TABLE_SCHEMA, c.TABLE_NAME) Table_Rank,
      MIN(ColCountsByTable.Column_Count) AS Column_Count,
      c2.TABLE_CATALOG AS Matching_Catalog,
      c2.TABLE_SCHEMA AS Matching_Schema,
      c2.TABLE_NAME AS Matching_Table,
      COUNT(c2.COLUMN_NAME) AS Matching_Column_Count
FROM INFORMATION_SCHEMA.TABLES t
      INNER JOIN INFORMATION_SCHEMA.COLUMNS c
            ON t.TABLE_CATALOG = c.TABLE_CATALOG
                  AND t.TABLE_SCHEMA = c.TABLE_SCHEMA
                  AND t.TABLE_NAME = c.TABLE_NAME
      INNER JOIN ColCountsByTable
            ON t.TABLE_CATALOG = ColCountsByTable.TABLE_CATALOG
                  AND t.TABLE_SCHEMA = ColCountsByTable.TABLE_SCHEMA
                  AND t.TABLE_NAME = ColCountsByTable.TABLE_NAME
      LEFT OUTER JOIN INFORMATION_SCHEMA.COLUMNS c2
            ON t.TABLE_NAME != c2.TABLE_NAME
                  AND c.COLUMN_NAME = c2.COLUMN_NAME
GROUP BY
      c.TABLE_CATALOG,
      c.TABLE_SCHEMA,
      c.TABLE_NAME,
      c2.TABLE_CATALOG,
      c2.TABLE_SCHEMA,
      c2.TABLE_NAME
--Use the below HAVING clause if you want to restrict results to only matches above a certain percent
--HAVING
--    /*Match_Percent*/
--    (100 * COUNT(c2.COLUMN_NAME) /*Matching_Column_Count*/) / MIN(ColCountsByTable.Column_Count) /*Column_Count*/
--          >= 50       --Require atleast 50% of the columns to match
ORDER BY
      Match_Percent DESC
GO
/*
Author: Bert Wagner
Source link: https://blog.bertwagner.com/how-to-search-and-destroy-non-sargable-queries-on-your-server-ff9f57c7268e
*/

SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

DECLARE @dbname SYSNAME;
SET @dbname = QUOTENAME(DB_NAME());

--WITH XMLNAMESPACES (DEFAULT 'http://schemas.microsoft.com/sqlserver/2004/07/showplan')

SELECT
   stmt.value('(@StatementText)[1]', 'varchar(max)') AS [Query],
   query_plan AS [QueryPlan],
   sc.value('(.//Identifier/ColumnReference/@Schema)[1]', 'varchar(128)') AS [Schema]
   , sc.value('(.//Identifier/ColumnReference/@Table)[1]', 'varchar(128)') AS [Table]
   , sc.value('(.//Identifier/ColumnReference/@Column)[1]', 'varchar(128)') AS [Column]
   , CASE WHEN s.exist('.//TableScan') = 1 THEN 'TableScan' ELSE 'IndexScan' END AS [ScanType],
   sc.value('(@ScalarString)[1]', 'varchar(128)') AS [ScalarString]
FROM 
    sys.dm_exec_cached_plans AS cp
    CROSS APPLY sys.dm_exec_query_plan(cp.plan_handle) AS qp
    CROSS APPLY query_plan.nodes('/ShowPlanXML/BatchSequence/Batch/Statements/StmtSimple') AS batch(stmt)
    CROSS APPLY stmt.nodes('.//RelOp[TableScan or IndexScan]') AS scan(s)
    CROSS APPLY s.nodes('.//ScalarOperator') AS scalar(sc)
WHERE
    s.exist('.//ScalarOperator[@ScalarString]!=""') = 1 
    AND sc.exist('.//Identifier/ColumnReference[@Database=sql:variable("@dbname")][@Schema!="[sys]"]') = 1
    AND sc.value('(@ScalarString)[1]', 'varchar(128)') IS NOT NULL;

GO
/*
Author: Wes Henriksen
Source: http://www.sqlservercentral.com/scripts/Foreign+Keys+%28FK%29/97811/
*/
DECLARE @SchemaName VARCHAR(50) = NULL;
DECLARE @TableName VARCHAR(250) = NULL;
DECLARE @ColumnName VARCHAR(250) = NULL;
DECLARE @SQLDrop VARCHAR(MAX) = '';
DECLARE @SQLAdd VARCHAR(MAX) = '';
DECLARE @SQLEnable VARCHAR(MAX) = '';

WITH  cte
        AS ( SELECT SQLDrop = 'IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = Object_ID(N''[' + SCHEMA_NAME(FK.schema_id) + '].[' + FK.name + ']'') AND parent_object_id =OBJECT_ID(N''[' + SCHEMA_NAME(FK.schema_id) + '].[' + OBJECT_NAME(FK.parent_object_id) + ']'') ' + ') 
BEGIN 
    ALTER TABLE [' + SCHEMA_NAME(FK.schema_id) + '].[' + OBJECT_NAME(FK.parent_object_id) + '] DROP CONSTRAINT [' + FK.name + ']' + CHAR(13) + CHAR(10) + 'END' + CHAR(13) + CHAR(10)
                  , SQLAdd = 'ALTER TABLE [' + SCHEMA_NAME(FK.schema_id) + '].[' + OBJECT_NAME(FK.parent_object_id) + '] WITH NOCHECK ADD CONSTRAINT [FK_' + OBJECT_NAME(FK.parent_object_id) + '_' + KeyOnC.name + '_' + SCHEMA_NAME(ReferencedT.schema_id) + ReferencedT.name --+ '_' + ReferencedC.name
                    + '] FOREIGN KEY(' + KeyOnC.name + ') REFERENCES ' + SCHEMA_NAME(ReferencedT.schema_id) + '.' + ReferencedT.name + '(' + ReferencedC.name + ')' 
         --+ ' ON DELETE CASCADE' 
                  , LenConstraintName = LEN('FK_' + OBJECT_NAME(FK.parent_object_id) + '_' + KeyOnC.name + '_' + SCHEMA_NAME(ReferencedT.schema_id) + ReferencedT.name)
                  , SQLEnable = 'ALTER TABLE [' + SCHEMA_NAME(FK.schema_id) + '].[' + OBJECT_NAME(FK.parent_object_id) + '] WITH CHECK CHECK CONSTRAINT [FK_' + OBJECT_NAME(FK.parent_object_id) + '_' + KeyOnC.name + '_' + SCHEMA_NAME(ReferencedT.schema_id) + ReferencedT.name + ']' --+ '_' + ReferencedC.name
                  , NumRank = ROW_NUMBER() OVER ( ORDER BY SCHEMA_NAME(FK.SCHEMA_ID), OBJECT_NAME(FK.Parent_object_id) )
             FROM   sys.foreign_keys AS FK
             INNER JOIN sys.foreign_key_columns AS FKC
                    ON FK.object_id = FKC.constraint_object_id
             INNER JOIN sys.columns AS KeyOnC
                    ON FKC.parent_object_id = KeyOnC.object_id
                       AND FKC.parent_column_id = KeyOnC.column_id
             INNER JOIN sys.columns AS ReferencedC
                    ON FKC.referenced_object_id = ReferencedC.object_id
                       AND FKC.referenced_column_id = ReferencedC.column_id
             INNER JOIN sys.objects AS KeyOnT
                    ON KeyOnC.object_id = KeyOnT.object_id
             INNER JOIN sys.objects AS ReferencedT
                    ON ReferencedC.object_id = ReferencedT.object_id
             WHERE  ( SCHEMA_NAME(KeyOnT.schema_id) = @SchemaName
                      OR @SchemaName IS NULL
                    )
                    AND ( KeyOnT.name LIKE '%' + @TableName + '%'
                          OR ReferencedT.name LIKE '%' + @TableName + '%'
                          OR @TableName IS NULL
                        )
                    AND ( KeyOnC.name = @ColumnName
                          OR ReferencedC.name = @ColumnName
                          OR @ColumnName IS NULL
                        )
                    AND NOT SCHEMA_NAME(KeyOnT.schema_id) = 'ref'
)
SELECT  SQLDrop
      , SQLAdd
      , LenConstraintName
      , SQLEnable
FROM    cte
ORDER BY NumRank;
GO
GO
/*
Author: Atom
Original link: http://billfellows.blogspot.ru/2017/07/generate-tsql-time-slices.html
Desctiption: Generate TSQL time slices
*/


SELECT
    D.Slice AS SliceStart
,   LEAD
    (
        D.Slice
    ,   1
        -- Default to midnight
    ,   TIMEFROMPARTS(0,0,0,0,0)
    )
    OVER (ORDER BY D.Slice) AS SliceStop
,   ROW_NUMBER() OVER (ORDER BY D.Slice) AS SliceLabel
FROM
(
    -- Generate 15 second time slices
    SELECT 
        TIMEFROMPARTS(A.rn, B.rn, C.rn, 0, 0) AS Slice
    FROM
        (SELECT TOP (24) -1 + ROW_NUMBER() OVER (ORDER BY(SELECT NULL)) FROM sys.all_objects AS AO) AS A(rn)
        CROSS APPLY (SELECT TOP (60) (-1 + ROW_NUMBER() OVER (ORDER BY(SELECT NULL))) FROM sys.all_objects AS AO) AS B(rn)
        -- 4 values since we'll aggregate to 15 seconds
        CROSS APPLY (SELECT TOP (4) (-1 + ROW_NUMBER() OVER (ORDER BY(SELECT NULL))) * 15  FROM sys.all_objects AS AO) AS C(rn)
) D


GO
/*
Author:  Greg Robidoux
Original link: https://www.mssqltips.com/sqlservertip/1584/auto-generate-sql-server-restore-script-from-backup-files-in-a-directory/
*/

USE Master;
GO
SET NOCOUNT ON;

-- 1 - Variable declaration
DECLARE @dbName sysname;
DECLARE @backupPath NVARCHAR(500);
DECLARE @cmd NVARCHAR(500);
DECLARE @fileList TABLE (backupFile NVARCHAR(255));
DECLARE @lastFullBackup NVARCHAR(500);
DECLARE @lastDiffBackup NVARCHAR(500);
DECLARE @backupFile NVARCHAR(500);

-- 2 - Initialize variables
SET @dbName = 'Customer';
SET @backupPath = 'D:\SQLBackups\';

-- 3 - get list of files
SET @cmd = 'DIR /b ' + @backupPath;

INSERT INTO @fileList(backupFile)
EXEC master.sys.xp_cmdshell @cmd;

-- 4 - Find latest full backup
SELECT @lastFullBackup = MAX(backupFile)
FROM @fileList
WHERE backupFile LIKE '%.BAK'
   AND backupFile LIKE @dbName + '%';

SET @cmd = 'RESTORE DATABASE ' + @dbName + ' FROM DISK = '''
       + @backupPath + @lastFullBackup + ''' WITH NORECOVERY, REPLACE';
PRINT @cmd;

-- 4 - Find latest diff backup
SELECT @lastDiffBackup = MAX(backupFile)
FROM @fileList
WHERE backupFile LIKE '%.DIF'
   AND backupFile LIKE @dbName + '%'
   AND backupFile > @lastFullBackup;

-- check to make sure there is a diff backup
IF @lastDiffBackup IS NOT NULL
BEGIN
   SET @cmd = 'RESTORE DATABASE ' + @dbName + ' FROM DISK = '''
       + @backupPath + @lastDiffBackup + ''' WITH NORECOVERY';
   PRINT @cmd;
   SET @lastFullBackup = @lastDiffBackup;
END

-- 5 - check for log backups
DECLARE backupFiles CURSOR FOR
   SELECT backupFile
   FROM @fileList
   WHERE backupFile LIKE '%.TRN'
   AND backupFile LIKE @dbName + '%'
   AND backupFile > @lastFullBackup;

OPEN backupFiles

-- Loop through all the files for the database
FETCH NEXT FROM backupFiles INTO @backupFile

WHILE @@FETCH_STATUS = 0
BEGIN
   SET @cmd = 'RESTORE LOG ' + @dbName + ' FROM DISK = '''
       + @backupPath + @backupFile + ''' WITH NORECOVERY';
   PRINT @cmd;
   FETCH NEXT FROM backupFiles INTO @backupFile;
END

CLOSE backupFiles;
DEALLOCATE backupFiles;

-- 6 - put database in a useable state
SET @cmd = 'RESTORE DATABASE ' + @dbName + ' WITH RECOVERY';
PRINT @cmd;
GO
/*
Author: SQL Undercover
Source link: https://sqlundercover.com/2017/08/21/undercover-toolbox-generate-a-temporary-table-definition-to-match-the-resultset-of-a-query/
*/

SET NOCOUNT ON;
 
DECLARE @Query VARCHAR(MAX) = 'select * from sys.databases;';
DECLARE @TempTableName VARCHAR(128) = '#temptable';
DECLARE @ColumnList VARCHAR(MAX);
 
SELECT @ColumnList = STUFF((SELECT ',' + name + ' ' + system_type_name + ' ' +
CASE is_nullable WHEN 0 THEN 'NOT NULL' ELSE 'NULL' END
+ CHAR(10)
FROM sys.dm_exec_describe_first_result_set(@Query, NULL, 0)
FOR XML PATH('')) ,1,1,'');
 
PRINT 'CREATE TABLE ' + @TempTableName + '('
PRINT @ColumnList;
PRINT(')');
GO
-- Get statistics details on SQL Server 2008 R2 and higher
-- Author: Kendra Little
-- "This works with SQL Server 2008 R2 SP2+ / SQL Server 2012 SP1+ / All higher versions"
    SELECT 
        stat.auto_created,
        stat.name as stats_name,
        STUFF((SELECT ', ' + cols.name
            FROM sys.stats_columns AS statcols
            JOIN sys.columns AS cols ON
                statcols.column_id=cols.column_id
                AND statcols.object_id=cols.object_id
            WHERE statcols.stats_id = stat.stats_id and
                statcols.object_id=stat.object_id
            ORDER BY statcols.stats_column_id
            FOR XML PATH(''), TYPE
        ), 1, 2, '')  as stat_cols,
        stat.filter_definition,
        stat.is_temporary,
        stat.no_recompute,
        sp.last_updated,
        sp.modification_counter,
        sp.rows_,
        sp.rows_sampled
    FROM sys.stats as stat
    CROSS APPLY sys.dm_db_stats_properties (stat.object_id, stat.stats_id) AS sp
    JOIN sys.objects as so on 
        stat.object_id=so.object_id
    JOIN sys.schemas as sc on
        so.schema_id=sc.schema_id
    WHERE 
        sc.name= 'Warehouse'
        and so.name='StockItemTransactions'
    ORDER BY 1, 2;
    GO
    
GO
/*
Author: Alexandros Pappas
Original link: http://www.codeproject.com/Articles/1118722/SQL-Table-Hierarchy
*/

DECLARE @fkcolumns TABLE(name SYSNAME PRIMARY KEY, referencedtable SYSNAME, parenttable SYSNAME, referencedcolumns varchar(MAX), parentcolumns varchar(MAX))
INSERT @fkcolumns
SELECT 
    a.name,
    b.name,
    c.name,
    STUFF((
        SELECT ',' + c.name
        FROM sys.foreign_key_columns b
        INNER JOIN sys.columns c ON b.referenced_object_id = c.object_id
        AND b.referenced_column_id = c.column_id
        WHERE a.object_id = b.constraint_object_id
        FOR XML PATH('')), 1, 1, '') parentcolumns,
    STUFF((
        SELECT ',' + c.name
        FROM sys.foreign_key_columns b
        INNER JOIN sys.columns c ON b.parent_object_id = c.object_id
        AND b.parent_column_id = c.column_id
        WHERE a.object_id = b.constraint_object_id
        FOR XML PATH('')), 1, 1, '') childcolumns
FROM sys.foreign_keys a
INNER JOIN sys.tables b ON a.referenced_object_id = b.object_id
INNER JOIN sys.tables c ON a.parent_object_id = c.object_id;

DECLARE @fkrefs TABLE(referencedtable SYSNAME, parenttable SYSNAME, referencedcolumns varchar(MAX), parentcolumns varchar(MAX))
INSERT @fkrefs
SELECT *, 
    (SELECT TOP 1 b.referencedcolumns
     FROM @fkcolumns b
     WHERE a.referencedtable = b.referencedtable and a.parenttable = b.parenttable),
    STUFF((
        SELECT ';' + b.parentcolumns
        FROM @fkcolumns b
        WHERE a.referencedtable = b.referencedtable and a.parenttable = b.parenttable
        FOR XML PATH('')), 1, 1, '')
FROM (
    SELECT referencedtable, parenttable
    FROM @fkcolumns a
    GROUP BY referencedtable, parenttable
) a;

WITH fks(treelevel, treepath, tablename, referencedcolumns, parentcolumns) AS (
SELECT 1,
    CAST(a.name AS VARCHAR(MAX)),
    a.name,
    CAST('' AS VARCHAR(MAX)),
    CAST('' AS VARCHAR(MAX))
    FROM sys.tables a
    LEFT JOIN @fkrefs c ON a.name = c.parenttable AND c.referencedtable <> c.parenttable
    WHERE c.referencedtable IS NULL
    UNION ALL 
    SELECT treelevel + 1,
        CAST(a.treepath + '_' + b.parenttable AS varchar(MAX)),
        b.parenttable,
        b.referencedcolumns,
        b.parentcolumns
    FROM fks a
    INNER JOIN @fkrefs b ON a.tablename = b.referencedtable
    WHERE treelevel < 10)
SELECT treelevel,
    treepath,
    REPLICATE('|---- ', treelevel) + tablename tablename,
    referencedcolumns,
    parentcolumns
FROM fks
ORDER BY treepath;

GO
/*
Author: Theo Ekelmans
Original link: http://www.sqlservercentral.com/articles/Agent+jobs/127346/
*/

--****************************************************************************************
-- This script returns a (graphical) timeline for all SQL jobs using google graph
--****************************************************************************************
-- Version: 1.1
-- Author:	Theo Ekelmans
-- Email:	theo@ekelmans.com
-- Date:	2015-06-24

-- Version: 1.2
-- Author:	Theo Ekelmans
-- Email:	theo@ekelmans.com
-- Date:	2015-07-28
-- Change:	Updated using feedback from the SqlServerCentralCommunity
--****************************************************************************************
set nocount on 

declare @DT datetime 
declare @StartDT datetime 
declare @EndDT datetime 
declare @MinRuntimeInSec int
declare @SendMail int
declare @ReturnRecocordset int
declare @Emailprofilename varchar(50)
declare @EmailRecipients varchar(50)

--***************************************************************************************
-- Set variables
--***************************************************************************************
set @StartDT = getdate() - 1
set @EndDT = getdate()
set @MinRuntimeInSec = 60 --Ignore jobs with runtime smaller then this

set @ReturnRecocordset = 0 
set @SendMail = 1
set @Emailprofilename = '<ProfileName>'
set @EmailRecipients = '<email>'

--***************************************************************************************
-- Pre-run cleanup (just in case)
--***************************************************************************************
IF OBJECT_ID('tempdb..#JobRuntime') IS NOT NULL DROP TABLE #JobRuntime;
IF OBJECT_ID('tempdb..##GoogleGraph') IS NOT NULL DROP TABLE ##GoogleGraph;

--***************************************************************************************
-- Create a table for HTML assembly
--***************************************************************************************
create table ##GoogleGraph ([ID] int IDENTITY(1,1) NOT NULL,
							[HTML] varchar(8000) NULL)

--***************************************************************************************
-- Create the Job Runtime information table
--***************************************************************************************
select	job.name as JobName
		,cat.name as CatName
		,CONVERT(DATETIME, CONVERT(CHAR(8), run_date, 112) + ' ' + STUFF(STUFF(RIGHT('000000' + CONVERT(VARCHAR(8), run_time), 6), 5, 0, ':'), 3, 0, ':'), 120) as SDT
		,dateadd(	s,
					((run_duration / 10000) % 100 * 3600) + ((run_duration / 100) % 100 * 60) + run_duration % 100 ,
					CONVERT(DATETIME, CONVERT(CHAR(8), run_date, 112) + ' ' + STUFF(STUFF(RIGHT('000000' + CONVERT(VARCHAR(8), run_time), 6), 5, 0, ':'), 3, 0, ':'), 120) 
				) as EDT
into	#JobRuntime
FROM	msdb.dbo.sysjobs job 
			left JOIN msdb.dbo.sysjobhistory his
				ON his.job_id = job.job_id
			INNER JOIN msdb.dbo.syscategories cat
				ON job.category_id = cat.category_id
where	CONVERT(DATETIME, CONVERT(CHAR(8), run_date, 112) + ' ' + STUFF(STUFF(RIGHT('000000' + CONVERT(VARCHAR(8), run_time), 6), 5, 0, ':'), 3, 0, ':'), 120) between @StartDT and @EndDT
and		step_id = 0 -- step_id = 0 is the entire job, step_id > 0 is actual step number
and		((run_duration / 10000) % 100 * 3600) + ((run_duration / 100) % 100 * 60) + run_duration % 100 > @MinRuntimeInSec  -- Ignore trivial runtimes
order by SDT

--if not exists (select 1 from #JobRuntime) 
--	goto NothingToDo

--***************************************************************************************
-- Format for google graph - Header 
-- (Split into multiple inserts because the default text result setting is 256 chars)
--***************************************************************************************
insert into ##GoogleGraph (HTML) 
select '<html>
	<head>
	<!--<META HTTP-EQUIV="refresh" CONTENT="3">-->
	<script type="text/javascript" src="https://www.google.com/jsapi?autoload={''modules'':[{''name'':''visualization'', ''version'':''1'',''packages'':[''timeline'']}]}"></script>'
insert into ##GoogleGraph (HTML) 
select '    <script type="text/javascript">
	google.setOnLoadCallback(drawChart);
	function drawChart() {'
insert into ##GoogleGraph (HTML) 
select '	var container = document.getElementById(''JobTimeline'');
	var chart = new google.visualization.Timeline(container);
	var dataTable = new google.visualization.DataTable();'
insert into ##GoogleGraph (HTML) 
select '	dataTable.addColumn({ type: ''string'', id: ''Position'' });
	dataTable.addColumn({ type: ''string'', id: ''Name'' });
	dataTable.addColumn({ type: ''date'', id: ''Start'' });
	dataTable.addColumn({ type: ''date'', id: ''End'' });
	dataTable.addRows([
'

--***************************************************************************************
-- Format for google graph - Data
--***************************************************************************************
--insert into ##GoogleGraph (HTML) 
SELECT  '		[ ' 
		+'''' + CatName  + ''', '
		+'''' + JobName  + ''', '
		+'new Date('
		+     cast(DATEPART(year ,  SDT) as varchar(4))
		+', '+cast(DATEPART(month,  SDT) -1 as varchar(4)) --Java months count from 0
		+', '+cast(DATEPART(day,    SDT) as varchar(4))
		+', '+cast(DATEPART(hour,   SDT) as varchar(4))
		+', '+cast(DATEPART(minute, SDT) as varchar(4))
		+', '+cast(DATEPART(second, SDT) as varchar(4)) 
		+'), '

		+'new Date('
		+     cast(DATEPART(year,   EDT) as varchar(4))
		+', '+cast(DATEPART(month,  EDT) -1 as varchar(4)) --Java months count from 0
		+', '+cast(DATEPART(day,    EDT) as varchar(4))
		+', '+cast(DATEPART(hour,   EDT) as varchar(4))
		+', '+cast(DATEPART(minute, EDT) as varchar(4))
		+', '+cast(DATEPART(second, EDT) as varchar(4)) 
		+ ') ],' --+ char(10)
from	#JobRuntime 

--***************************************************************************************
-- Format for google graph - Footer
--***************************************************************************************
insert into ##GoogleGraph (HTML) 
select '	]);

	var options = 
	{
		timeline: 	{ 
					groupByRowLabel: true,
					colorByRowLabel: false,
					singleColor: false,
					rowLabelStyle: {fontName: ''Helvetica'', fontSize: 14 },
					barLabelStyle: {fontName: ''Helvetica'', fontSize: 14 }					
					}
	};

	chart.draw(dataTable, options);

}'
insert into ##GoogleGraph (HTML) 
select '
	</script>
	</head>
	<body>'
	+'<font face="Helvetica" size="3" >'
	+'Job timeline on: '+@@servername
	+' from '+convert(varchar(20), @StartDT, 120)
	+' until '+convert(varchar(20), @EndDT, 120)
	+case when @MinRuntimeInSec = 0 then '' else ' (hiding jobs with runtime < '+cast(@MinRuntimeInSec as varchar(10))+' seconds)' end
	+'</font>
		<div id="JobTimeline" style="width: 1885px; height: 900px;"></div>
	</body>
</html>'

--***************************************************************************************
-- Output HTML page - copy output & paste to a .HTML file and open with google chrome
--***************************************************************************************
if @ReturnRecocordset = 1 
	select html from ##GoogleGraph order by ID

--***************************************************************************************
-- Send Email - 
--***************************************************************************************
if @SendMail = 1 
	execute msdb.dbo.sp_send_dbmail	
		 @profile_name = @Emailprofilename
		,@recipients = @EmailRecipients
		,@subject = 'JobTimeline'
		,@body = 'See attachment for JobTimeline, open with Google Chrome!' 
		,@body_format = 'HTML' -- or TEXT
		,@importance = 'Normal' --Low Normal High
		,@sensitivity = 'Normal' --Normal Personal Private Confidential
		,@execute_query_database = 'master'
		,@query_result_header = 1
		,@query = 'set nocount on; SELECT HTML FROM ##GoogleGraph'
		,@query_result_no_padding = 1  -- prevent SQL adding padding spaces in the result
		--,@query_no_truncate= 1       -- mutually exclusive with @query_result_no_padding 
		,@attach_query_result_as_file = 1
		,@query_attachment_filename= 'JobTimeline.HTML'


--goto Cleanup

--***************************************************************************************
-- Just in case....
--***************************************************************************************
--NothingToDo:

print 'No job runtime info found....'

--***************************************************************************************
-- Cleanup
--***************************************************************************************
--Cleanup:
IF OBJECT_ID('tempdb..#JobRuntime') IS NOT NULL DROP TABLE #JobRuntime;
IF OBJECT_ID('tempdb..##GoogleGraph') IS NOT NULL DROP TABLE ##GoogleGraph;

GO
/*
Author: Tim Ford
Original link: http://sqlmag.com/database-backup-and-recovery/how-old-are-your-backups
*/
WITH full_backups AS (
SELECT ROW_NUMBER() OVER(PARTITION BY BS.database_name,
                                      BS.type
                             ORDER BY BS.database_name ASC,
                                      BS.backup_finish_date DESC
                        ) AS [Row Number],
        D.name AS [database_name],
        BS.backup_set_id,
        BS.type AS backup_type,
        BS.backup_finish_date,
        D.recovery_model_desc
FROM master.sys.databases AS D
        LEFT JOIN msdb.dbo.[backupset] AS BS
                ON D.name = BS.database_name
/* FILTERING OPTIONS*/
--WHERE BS.[type] = '<backup_type,,D>'
--WHERE BS.[name] = '<database_name,,Foo_DB>'
)
SELECT FB.database_name,
       CASE FB.backup_type
               WHEN 'D' THEN 'Data'
               WHEN 'I' THEN 'Differential'
               WHEN 'L' THEN 'Transaction Log'
       END AS backup_type_desc,
       FB.recovery_model_desc,
       FB.backup_finish_date,
       BMF.physical_device_name,
       DATEDIFF(hour, FB.backup_finish_date, GETDATE()) AS backup_hours,
       DATEDIFF(minute, FB.backup_finish_date, GETDATE()) AS backup_minutes
FROM full_backups FB
 LEFT JOIN msdb.dbo.[backupset] BS ON FB.backup_set_id = BS.backup_set_id
 LEFT JOIN msdb.dbo.backupmediafamily BMF ON BS.media_set_id = BMF.media_set_id
 WHERE FB.[Row Number] = 1
ORDER BY FB.database_name, FB.[Row Number], FB.backup_type;

GO
/*
Author: Kendra Little
Original link: https://www.littlekendra.com/2017/01/24/how-to-find-queries-using-an-index-and-queries-using-index-hints/
*/

--Search for queries in the execution plan cache
--Simply plug the name of the index you’re looking for into this query. If you have multiple databases with the same index name,
--you’ll need to add additional criteria to get just the database you’re looking for.
SELECT 
    querystats.plan_handle,
    querystats.query_hash,
    SUBSTRING(sqltext.text, (querystats.statement_start_offset / 2) + 1, 
                (CASE querystats.statement_end_offset 
                    WHEN -1 THEN DATALENGTH(sqltext.text) 
                    ELSE querystats.statement_end_offset 
                END - querystats.statement_start_offset) / 2 + 1) AS sqltext, 
    querystats.execution_count,
    querystats.total_logical_reads,
    querystats.total_logical_writes,
    querystats.creation_time,
    querystats.last_execution_time,
    CAST(query_plan AS xml) as plan_xml
FROM sys.dm_exec_query_stats as querystats
CROSS APPLY sys.dm_exec_text_query_plan
    (querystats.plan_handle, querystats.statement_start_offset, querystats.statement_end_offset) 
    as textplan
CROSS APPLY sys.dm_exec_sql_text(querystats.sql_handle) AS sqltext 
WHERE 
    textplan.query_plan like '%PK_Sales_Invoices%'
ORDER BY querystats.last_execution_time DESC
OPTION (RECOMPILE);
GO


--Find queries using the index in Query Store
--If you’ve enabled the SQL Server 2016+ Query Store on your databases, you’ve got something better to search than the plan cache
SELECT
    qsq.query_id,
    qsq.query_hash,
    (SELECT TOP 1 qsqt.query_sql_text FROM sys.query_store_query_text qsqt
        WHERE qsqt.query_text_id = MAX(qsq.query_text_id)) AS sqltext,    
    SUM(qrs.count_executions) AS execution_count,
    SUM(qrs.count_executions) * AVG(qrs.avg_logical_io_reads) as est_logical_reads,
    SUM(qrs.count_executions) * AVG(qrs.avg_logical_io_writes) as est_writes,
    MIN(qrs.last_execution_time AT TIME ZONE 'Pacific Standard Time') as min_execution_time_PST,
    MAX(qrs.last_execution_time AT TIME ZONE 'Pacific Standard Time') as last_execution_time_PST,
    SUM(qsq.count_compiles) AS sum_compiles,
    TRY_CONVERT(XML, (SELECT TOP 1 qsp2.query_plan from sys.query_store_plan qsp2
        WHERE qsp2.query_id=qsq.query_id
        ORDER BY qsp2.plan_id DESC)) AS query_plan
FROM sys.query_store_query qsq
JOIN sys.query_store_plan qsp on qsq.query_id=qsp.query_id
CROSS APPLY (SELECT TRY_CONVERT(XML, qsp.query_plan) AS query_plan_xml) AS qpx
JOIN sys.query_store_runtime_stats qrs on qsp.plan_id = qrs.plan_id
JOIN sys.query_store_runtime_stats_interval qsrsi on qrs.runtime_stats_interval_id=qsrsi.runtime_stats_interval_id
WHERE    
    qsp.query_plan like N'%PK_Sales_Invoices%'
    AND qsp.query_plan not like '%query_store_runtime_stats%' /* Not a query store query */
    AND qsp.query_plan not like '%dm_exec_sql_text%' /* Not a query searching the plan cache */
GROUP BY 
    qsq.query_id, qsq.query_hash
ORDER BY est_logical_reads DESC
OPTION (RECOMPILE);
GO


--Search the execution plan cache for index hints
--To find forced indexes in the plan cache, look for plans that contain ‘%ForcedIndex=”1″%’
SELECT 
    querystats.plan_handle,
    querystats.query_hash,
    SUBSTRING(sqltext.text, (querystats.statement_start_offset / 2) + 1, 
                (CASE querystats.statement_end_offset 
                    WHEN -1 THEN DATALENGTH(sqltext.text) 
                    ELSE querystats.statement_end_offset 
                END - querystats.statement_start_offset) / 2 + 1) AS sqltext, 
    querystats.execution_count,
    querystats.total_logical_reads,
    querystats.total_logical_writes,
    querystats.creation_time,
    querystats.last_execution_time,
    CAST(query_plan AS xml) as plan_xml
FROM sys.dm_exec_query_stats as querystats
CROSS APPLY sys.dm_exec_text_query_plan
    (querystats.plan_handle, querystats.statement_start_offset, querystats.statement_end_offset) 
    as textplan
CROSS APPLY sys.dm_exec_sql_text(querystats.sql_handle) AS sqltext 
WHERE 
    textplan.query_plan like N'%ForcedIndex="1"%'
    and UPPER(sqltext.text) like N'%INDEX%'
OPTION (RECOMPILE);
GO


--Find index hints in Query Store
--If you’ve enabled the SQL Server 2016+ Query Store on your databases
SELECT
    qsq.query_id,
    qsq.query_hash,
    (SELECT TOP 1 qsqt.query_sql_text FROM sys.query_store_query_text qsqt
        WHERE qsqt.query_text_id = MAX(qsq.query_text_id)) AS sqltext,    
    SUM(qrs.count_executions) AS execution_count,
    SUM(qrs.count_executions) * AVG(qrs.avg_logical_io_reads) as est_logical_reads,
    SUM(qrs.count_executions) * AVG(qrs.avg_logical_io_writes) as est_writes,
    MIN(qrs.last_execution_time AT TIME ZONE 'Pacific Standard Time') as min_execution_time_PST,
    MAX(qrs.last_execution_time AT TIME ZONE 'Pacific Standard Time') as last_execution_time_PST,
    SUM(qsq.count_compiles) AS sum_compiles,
    TRY_CONVERT(XML, (SELECT TOP 1 qsp2.query_plan from sys.query_store_plan qsp2
        WHERE qsp2.query_id=qsq.query_id
        ORDER BY qsp2.plan_id DESC)) AS query_plan
FROM sys.query_store_query qsq
JOIN sys.query_store_plan qsp on qsq.query_id=qsp.query_id
CROSS APPLY (SELECT TRY_CONVERT(XML, qsp.query_plan) AS query_plan_xml) AS qpx
JOIN sys.query_store_runtime_stats qrs on qsp.plan_id = qrs.plan_id
JOIN sys.query_store_runtime_stats_interval qsrsi on qrs.runtime_stats_interval_id=qsrsi.runtime_stats_interval_id
WHERE    
    qsp.query_plan like N'%ForcedIndex="1"%'
GROUP BY 
    qsq.query_id, qsq.query_hash
ORDER BY est_logical_reads DESC
OPTION (RECOMPILE);
GO

GO
/*
Author: Frank Gill
Original link: https://skreebydba.com/2016/07/18/identifying-in-memory-objects-when-querying-the-transaction-log/
*/


SELECT 
f.[Current LSN],
f.Operation,
f.Context,
f.[Transaction ID],
f.operation_desc,
f.tx_end_timestamp,
f.total_size,
OBJECT_NAME(m.object_id) AS ObjectName,
f.newrow_identity,
f.newrow_data,
f.newrow_datasize,
f.oldrow_begin_timestamp,
f.oldrow_identity,
f.oldrow_key_data,
f.oldrow_key_datasize,
f.xtp_description,
f.[Log Record Fixed Length],
f.[Log Record Length],
f.[Flag Bits],
f.[Log Reserve],
f.[Log Record]
FROM fn_dblog_xtp(NULL,NULL) AS f
INNER JOIN sys.memory_optimized_tables_internal_attributes m
ON (f.xtp_object_id - 2147483648) = (m.xtp_object_id + 2147483648) 
WHERE Operation LIKE '%HK%';
GO
GO
/*
Author: skreebydba
Original link: https://skreebydba.com/2016/08/09/identifying-object-name-for-create-and-alter-using-fn_dblog
*/

USE YourDatabase;
 
/* Declare local variables and drop temp table if it exists. */
 
IF CHARINDEX('2016',@@VERSION) &amp--;gt; 0
BEGIN
 
    DROP TABLE IF EXISTS #logrecords;
 
END
ELSE
BEGIN
 
    IF OBJECT_ID('tempdb..#logrecords') IS NOT NULL
    BEGIN
 
        DROP TABLE #logrecords;
 
    END
END
 
/* Declare local variables */
DECLARE @tranname NVARCHAR(66);
DECLARE @tranid NVARCHAR(28);
DECLARE @loopcount INT = 1;
DECLARE @looplimit INT;
 
/* Set @tranname to the value you are looking for
   This works for CREATE/ALTER VIEW, CREATE TABLE, and ALTER TABLE
   Currently researching other possibilities */
SELECT @tranname = 'ALTER TABLE';
 
/* Get all log records associated with the transaction name specified
   The results contain a row number per transaction, so all occurrences
   of the transaction name will be found */
SELECT  ROW_NUMBER() OVER(PARTITION BY [Transaction ID] ORDER BY [Current LSN]) AS [Row],
    [Current LSN], [Transaction ID], [Transaction Name], operation, Context, AllocUnitName, AllocUnitId, PartitionId, [Lock Information]
INTO #logrecords
FROM fn_dblog(NULL,NULL)
WHERE [Transaction ID] IN
    (SELECT [Transaction ID]
    FROM fn_dblog(NULL,NULL)
    WHERE [Transaction Name] = @tranname);
 
SELECT @looplimit = COUNT(*) FROM #logrecords
WHERE [Transaction Name] = @tranname;
 
/* The object id for the object affected is contained in the [Lock Information] column of the second log record of the transaction
   This WHILE loop finds the second row for each transaction and does lots of string manipulation magic to return the object id
   from a string like this:
   HoBt 0:ACQUIRE_LOCK_SCH_M OBJECT: 9:146099561:0
   Once it finds it, it returns the object name */
WHILE @loopcount &amp--;lt;= @looplimit
BEGIN
 
    SELECT TOP 1 @tranid = [Transaction ID]
    FROM #logrecords
 
    DECLARE @lockinfo NVARCHAR(300);
    DECLARE @startingposition INT;
    DECLARE @endingposition INT;
    SELECT @lockinfo = REVERSE([Lock Information]), @startingposition = (CHARINDEX(':',REVERSE([Lock Information])) + 1), @endingposition = CHARINDEX(':',REVERSE([Lock Information]),(CHARINDEX(':',REVERSE([Lock Information])) + 1))
    FROM #logrecords
    WHERE [Row] = 2
    AND [Transaction ID] = @tranid; 
 
    SELECT OBJECT_NAME(REVERSE(SUBSTRING(@lockinfo,(@startingposition),(@endingposition - @startingposition)))) AS ObjectName;
 
    DELETE FROM #logrecords
    WHERE [Transaction ID] = @tranid;
 
    SELECT @loopcount += 1;
 
END

GO
/*
Author: Tim Ford
Original link: http://sqlmag.com/security/identifying-sql-server-logins-without-default-database-users
*/
--============================================================
--IDENTIFY ALL LOGINS WITH NO ASSOCIATED DEFAULT DATABASE USER
--============================================================
SET NOCOUNT ON;

DECLARE @sql_text nvarchar(max);
DECLARE @database_name sysname;

IF EXISTS(SELECT name FROM tempdb.sys.tables WHERE name LIKE '#default_db_users_table%')
        BEGIN
                DROP TABLE #default_db_users_table;
        END

CREATE TABLE #default_db_users_table 
        (
                server_login_name sysname NOT NULL
                , database_user_name sysname NOT NULL
                , default_database_name sysname NOT NULL
        );      

DECLARE cur_default_databases CURSOR FORWARD_ONLY STATIC FOR --FORWARD_ONLY STATIC CURSOR FOR
        SELECT name 
        FROM sys.databases 
        ORDER BY name

OPEN cur_default_databases

FETCH NEXT FROM cur_default_databases INTO @database_name

WHILE @@FETCH_STATUS = 0
        BEGIN

                SELECT @sql_text = 
                'INSERT INTO #default_db_users_table(server_login_name, database_user_name, default_database_name)
                SELECT SP.name AS [server_login_name], DP.name AS [database_user_name], SP.default_database_name
                FROM sys.server_principals AS SP
                        INNER JOIN ' + @database_name + '.sys.database_principals AS DP
                                ON SP.sid = DP.sid
                WHERE SP.default_database_name = ''' + @database_name + ''';'

                EXEC sys.sp_sqlexec @sql_text;

                FETCH NEXT FROM cur_default_databases INTO @database_name
        END 

CLOSE cur_default_databases;
DEALLOCATE cur_default_databases;

--===================================================
-- LISTING OF LOGINS MISSING USER IN DEFAULT DATABASE
--===================================================
SELECT SP.name AS login_name
        , SP.default_database_name
        , 'USE [' + SP.default_database_name + ']; CREATE USER [' + SP.name + '] FOR LOGIN [' + SP.name + '];' AS user_create_stmt
FROM sys.server_principals AS SP
        LEFT JOIN #default_db_users_table AS DDUT
                ON SP.name = DDUT.server_login_name
WHERE SP.type_desc IN ('SQL_LOGIN', 'WINDOWS_LOGIN')
        AND SP.name NOT LIKE 'NT %'
        AND SP.name NOT LIKE '##%'
        AND SP.default_database_name IS NOT NULL
        AND DDUT.server_login_name IS NULL
ORDER BY SP.name;


--===================================================
-- CLEANUP
--===================================================
IF EXISTS(SELECT name FROM tempdb.sys.tables WHERE name LIKE '#missing_default_db_users_table%')
        BEGIN
                DROP TABLE #missing_default_db_users_table;
        END;

GO
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
SET NOCOUNT ON
SET ANSI_WARNINGS OFF
SET ARITHABORT OFF
SET ARITHIGNORE ON
SET TEXTSIZE 2147483647


-----------------------------------------------------------------------------------------------------------------------------
--	Script Details: Listing Of Standard Details Related To The Script
-----------------------------------------------------------------------------------------------------------------------------

--	Purpose: Breakdown Of All Indexes Contained Within A Database
--	Create Date (MM/DD/YYYY): 12/23/2013
--	Developer: Sean Smith (s.smith.sql AT gmail DOT com)
--	Additional Notes: N/A


-----------------------------------------------------------------------------------------------------------------------------
--	Modification History: Listing Of All Modifications Since Original Implementation
-----------------------------------------------------------------------------------------------------------------------------

--	Description: Added "@Report_Style" Variable
--	Date (MM/DD/YYYY): 11/09/2015
--	Developer: Sean Smith (s.smith.sql AT gmail DOT com)
--	Additional Notes: N/A


-----------------------------------------------------------------------------------------------------------------------------
--	Declarations / Sets: Declare And Set Variables
-----------------------------------------------------------------------------------------------------------------------------

DECLARE
	 @Database_ID AS SMALLINT
	,@Report_Style AS BIT


DECLARE @Index_Filters AS TABLE

	(
		 [object_id] INT NOT NULL
		,index_id INT NOT NULL
		,filter_definition NVARCHAR (MAX) NOT NULL
	)


SET @Database_ID = DB_ID ()


SET @Report_Style = 1


-----------------------------------------------------------------------------------------------------------------------------
--	Error Trapping: Check If Temp Table(s) Already Exist(s) And Drop If Applicable
-----------------------------------------------------------------------------------------------------------------------------

IF OBJECT_ID (N'tempdb.dbo.#temp_index_breakdown_keys_filters', N'U') IS NOT NULL
BEGIN

	DROP TABLE dbo.#temp_index_breakdown_keys_filters

END


IF OBJECT_ID (N'tempdb.dbo.#temp_index_breakdown_size_info', N'U') IS NOT NULL
BEGIN

	DROP TABLE dbo.#temp_index_breakdown_size_info

END


-----------------------------------------------------------------------------------------------------------------------------
--	Table Insert: Insert Filtered Index Data Into Temp Table
-----------------------------------------------------------------------------------------------------------------------------

IF EXISTS (SELECT * FROM sys.all_columns AC WHERE AC.[object_id] = OBJECT_ID (N'sys.indexes', N'V') AND AC.name = N'has_filter')
BEGIN

	INSERT INTO @Index_Filters

		(
			 [object_id]
			,index_id
			,filter_definition
		)

	EXECUTE

		(
			N'
				SELECT
					 I.[object_id]
					,I.index_id
					,I.filter_definition
				FROM
					sys.indexes I
				WHERE
					I.has_filter = 1
			 '
		)

END


-----------------------------------------------------------------------------------------------------------------------------
--	Table Insert: Insert Index Key, Include Key, And Filter Definition Values Into Temp Table
-----------------------------------------------------------------------------------------------------------------------------

SELECT
	 sqI.[object_id]
	,sqI.index_id
	,STUFF (CONVERT (NVARCHAR (MAX), sqI.index_key), 1, 2, N'') AS index_key
	,STUFF (CONVERT (NVARCHAR (MAX), sqI.include_key), 1, 2, N'') AS include_key
	,sqI.filter_definition
	,RANK () OVER
				(
					ORDER BY
						 sqI.[object_id]
						,CONVERT (NVARCHAR (MAX), sqI.index_key)
						,sqI.filter_definition
				) AS dupe_rank
INTO
	dbo.#temp_index_breakdown_keys_filters
FROM

	(
		SELECT
			 I.[object_id]
			,I.index_id
			,(
				SELECT
					  N', '
					+ C.name
					+ N' • '
					+ LOWER (TYPE_NAME (C.user_type_id)
					+ ISNULL ((N': [ ' + (CASE
											WHEN C.system_type_id <> C.user_type_id THEN TYPE_NAME (C.system_type_id)
											END) + N' ]'), N''))
					+ N' '
					+ (CASE
							WHEN LOWER (TYPE_NAME (C.system_type_id)) IN (N'nchar', N'ntext', N'nvarchar') THEN CONVERT (VARCHAR (6), C.max_length / 2)
							WHEN LOWER (TYPE_NAME (C.system_type_id)) NOT IN (N'bigint', N'bit', N'date', N'datetime', N'datetime2', N'datetimeoffset', N'decimal', N'float', N'int', N'money', N'numeric', N'real', N'smalldatetime', N'smallint', N'smallmoney', N'time', N'tinyint') THEN CONVERT (VARCHAR (6), C.max_length)
							ELSE CONVERT (VARCHAR (6), C.max_length) + ' (' + CONVERT (VARCHAR (11), COLUMNPROPERTY (C.[object_id], C.name, 'Precision')) + ',' + ISNULL (CONVERT (VARCHAR (11), COLUMNPROPERTY (C.[object_id], C.name, 'Scale')), 0) + ')'
							END)
					+ N' '
					+ (CASE
							WHEN IC.is_descending_key = 0 THEN N'[A]'
							WHEN IC.is_descending_key = 1 THEN N'[D]'
							ELSE N'[N/A]'
							END) AS [text()]
				FROM
					sys.index_columns IC
					INNER JOIN sys.columns C ON C.[object_id] = IC.[object_id]
						AND C.column_id = IC.column_id
					INNER JOIN sys.types TY ON TY.user_type_id = C.user_type_id
				WHERE
					IC.is_included_column = 0
					AND IC.[object_id] = I.[object_id]
					AND IC.index_id = I.index_id
				ORDER BY
					IC.key_ordinal
				FOR
					 XML PATH ('')
					,TYPE
			 ) AS index_key
			,(
				SELECT
					  N', '
					+ C.name
					+ N' • '
					+ TYPE_NAME (C.user_type_id)
					+ ISNULL ((N': [ ' + (CASE
											WHEN C.system_type_id <> C.user_type_id THEN LOWER (TYPE_NAME (C.system_type_id))
											END) + N' ]'), N'')
					+ N' '
					+ (CASE
							WHEN TY.name NOT IN (N'bigint', N'bit', N'date', N'datetime', N'datetime2', N'datetimeoffset', N'decimal', N'float', N'int', N'money', N'numeric', N'real', N'smalldatetime', N'smallint', N'smallmoney', N'time', N'tinyint') THEN CONVERT (NVARCHAR (30), C.max_length)
							ELSE CONVERT (NVARCHAR (30), C.max_length) + N' (' + CONVERT (NVARCHAR (30), COLUMNPROPERTY (C.[object_id], C.name, 'Precision')) + N',' + ISNULL (CONVERT (NVARCHAR (30), COLUMNPROPERTY (C.[object_id], C.name, 'Scale')), 0) + N')'
							END) AS [text()]
				FROM
					sys.index_columns IC
					INNER JOIN sys.columns C ON C.[object_id] = IC.[object_id]
						AND C.column_id = IC.column_id
					INNER JOIN sys.types TY ON TY.user_type_id = C.user_type_id
				WHERE
					IC.is_included_column = 1
					AND IC.[object_id] = I.[object_id]
					AND IC.index_id = I.index_id
				ORDER BY
					IC.key_ordinal
				FOR
					 XML PATH ('')
					,TYPE
			 ) AS include_key
			,tvIF.filter_definition
		FROM
			sys.indexes I
			LEFT JOIN @Index_Filters tvIF ON tvIF.[object_id] = I.[object_id]
				AND tvIF.index_id = I.index_id
	) sqI


-----------------------------------------------------------------------------------------------------------------------------
--	Table Insert: Insert Size Values Into Temp Table
-----------------------------------------------------------------------------------------------------------------------------

SELECT
	 DDPS.[object_id]
	,DDPS.index_id
	,SUM (CASE
			WHEN DDPS.index_id < 2 THEN DDPS.row_count
			END) AS [rows]
	,SUM (DDPS.reserved_page_count) AS total_pages
	,SUM (DDPS.used_page_count) AS used_pages
	,SUM (CASE
			WHEN DDPS.index_id < 2 THEN DDPS.in_row_data_page_count + DDPS.lob_used_page_count + DDPS.row_overflow_used_page_count
			ELSE DDPS.lob_used_page_count + DDPS.row_overflow_used_page_count
			END) AS data_pages
INTO
	dbo.#temp_index_breakdown_size_info
FROM
	sys.dm_db_partition_stats DDPS
GROUP BY
	 DDPS.[object_id]
	,DDPS.index_id


-----------------------------------------------------------------------------------------------------------------------------
--	Main Query: Final Display / Output
-----------------------------------------------------------------------------------------------------------------------------

SELECT
	 (CASE
		WHEN sqBAQ.row_filter = 1 THEN sqBAQ.[type]
		ELSE ''
		END) AS object_type
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN DB_NAME ()
		ELSE ''
		END) AS [database_name]
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN SCHEMA_NAME (sqBAQ.[schema_id])
		ELSE ''
		END) AS [schema_name]
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN OBJECT_NAME (sqBAQ.[object_id])
		ELSE ''
		END) AS [object_name]
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN sqBAQ.create_date
		ELSE ''
		END) AS create_date
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN caMDKL.modify_date
		ELSE ''
		END) AS modify_date
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN REVERSE (SUBSTRING (REVERSE (CONVERT (VARCHAR (23), CONVERT (MONEY, sqBAQ.[rows]), 1)), 4, 23))
		ELSE ''
		END) AS [rows]
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (23), CONVERT (MONEY, (sqBAQ.total_pages * 8) / 1024.0), 1)
		ELSE ''
		END) AS total_mb
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (23), CONVERT (MONEY, (sqBAQ.used_pages * 8) / 1024.0), 1)
		ELSE ''
		END) AS used_mb
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (23), CONVERT (MONEY, (sqBAQ.unused_pages * 8) / 1024.0), 1)
		ELSE ''
		END) AS unused_mb
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (23), CONVERT (MONEY, (sqBAQ.data_pages * 8) / 1024.0), 1)
		ELSE ''
		END) AS data_mb
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (23), CONVERT (MONEY, (sqBAQ.index_pages * 8) / 1024.0), 1)
		ELSE ''
		END) AS index_mb
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (6), CONVERT (DECIMAL (5, 2), ISNULL (((sqBAQ.data_pages + .0) / sqBAQ.used_pages) * 100, 0)))
		ELSE ''
		END) AS pct_data
	,(CASE
		WHEN sqBAQ.row_filter = 1 THEN CONVERT (VARCHAR (6), CONVERT (DECIMAL (5, 2), ISNULL (((sqBAQ.index_pages + .0) / sqBAQ.used_pages) * 100, 0)))
		ELSE ''
		END) AS pct_index
	,sqBAQ.type_desc AS index_type
	,ISNULL (sqBAQ.index_name, '') AS index_name
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		WHEN sqBAQ.is_primary_key = 0 AND sqBAQ.is_unique = 0 THEN REPLICATE ('.', 6)
		WHEN sqBAQ.is_system_named = 0 THEN 'No'
		WHEN sqBAQ.is_system_named = 1 THEN 'Yes'
		ELSE ''
		END) AS system_named
	,(CASE
		WHEN sqBAQ.is_primary_key = 1 THEN 'Yes'
		ELSE ''
		END) AS is_pk
	,(CASE
		WHEN sqBAQ.is_unique_constraint = 1 THEN 'C'
		WHEN sqBAQ.is_unique = 1 THEN 'I'
		ELSE ''
		END) AS [unique]
	,(CASE
		WHEN sqBAQ.is_disabled = 1 THEN 'Yes'
		ELSE ''
		END) AS [disabled]
	,(CASE
		WHEN sqBAQ.is_hypothetical = 1 THEN 'Yes'
		ELSE ''
		END) AS hypothetical
	,REVERSE (SUBSTRING (REVERSE (CONVERT (VARCHAR (23), CONVERT (MONEY, sqCC.total_columns), 1)), 4, 23)) AS total_columns
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE REVERSE (SUBSTRING (REVERSE (CONVERT (VARCHAR (23), CONVERT (MONEY, caMDKL.[index_columns]), 1)), 4, 23))
		END) AS [index_columns]
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE REVERSE (SUBSTRING (REVERSE (CONVERT (VARCHAR (23), CONVERT (MONEY, caMDKL.include_columns), 1)), 4, 23))
		END) AS include_columns
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE CONVERT (VARCHAR (23), CONVERT (MONEY, (caMDKL.[index_columns] / sqCC.total_columns) * 100), 1)
		END) AS index_pct_of_columns
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE CONVERT (VARCHAR (23), CONVERT (MONEY, (caMDKL.include_columns / sqCC.total_columns) * 100), 1)
		END) AS include_pct_of_columns
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE CONVERT (VARCHAR (23), CONVERT (MONEY, ((caMDKL.[index_columns] + caMDKL.include_columns) / sqCC.total_columns) * 100), 1)
		END) AS total_pct_of_columns
	,CONVERT (VARCHAR (23), CONVERT (MONEY, (ISNULL (sqBAQ.individual_index_pages, 0) * 8) / 1024.0), 1) AS key_mb
	,CONVERT (VARCHAR (6), CONVERT (DECIMAL (5, 2), ISNULL (((sqBAQ.individual_index_pages + .0) / sqBAQ.index_pages) * 100, 0))) AS key_mb_pct
	,(CASE
		WHEN sqBAQ.type_desc = N'HEAP' THEN ''
		ELSE REVERSE (SUBSTRING (REVERSE (CONVERT (VARCHAR (23), CONVERT (MONEY, sqKL.total_max_length), 1)), 4, 23))
		END) AS max_key_size_bytes
	,(CASE
		WHEN sqKL.total_max_length > 900 THEN 'Yes'
		ELSE ''
		END) AS over_key_size_limit
	,ISNULL (ttIBKF.index_key, N'') AS index_key
	,ISNULL (ttIBKF.include_key, N'') AS include_key
	,ISNULL (ttIBKF.filter_definition, N'') AS filter_definition
	,(CASE
		WHEN sqED02.dupe_id IS NOT NULL THEN CONVERT (VARCHAR (20), sqED02.dupe_id) + ' - [' + CONVERT (VARCHAR (11), sqED02.total_dupes) + ']'
		ELSE ''
		END) AS dupe_id
	,sqBAQ.is_unused AS unused
	,ISNULL (CONVERT (VARCHAR (10), STATS_DATE (sqBAQ.[object_id], sqBAQ.index_id), 23), '') AS statistics_date
	,(CASE
		WHEN sqBAQ.[allow_row_locks] = 0 THEN 'No'
		WHEN sqBAQ.[allow_row_locks] = 1 THEN 'Yes'
		ELSE ''
		END) AS row_locks
	,(CASE
		WHEN sqBAQ.[allow_page_locks] = 0 THEN 'No'
		WHEN sqBAQ.[allow_page_locks] = 1 THEN 'Yes'
		ELSE ''
		END) AS page_locks
	,(CASE
		WHEN sqBAQ.[ignore_dup_key] = 0 THEN 'No'
		WHEN sqBAQ.[ignore_dup_key] = 1 THEN 'Yes'
		ELSE ''
		END) AS ignore_dupes
	,(CASE
		WHEN sqBAQ.no_recompute = 0 THEN 'Yes'
		WHEN sqBAQ.no_recompute = 1 THEN 'No'
		ELSE ''
		END) AS auto_stats
	,(CASE
		WHEN sqBAQ.is_padded = 0 THEN 'No'
		WHEN sqBAQ.is_padded = 1 THEN 'Yes'
		ELSE ''
		END) AS padded
	,(CASE
		WHEN sqBAQ.fill_factor = 0 THEN 100
		ELSE sqBAQ.fill_factor
		END) AS fill_factor
	,(CASE
		WHEN sqBAQ.user_seeks > 0 THEN CONVERT (VARCHAR (20), sqBAQ.user_seeks)
		ELSE ''
		END) AS user_seeks
	,(CASE
		WHEN sqBAQ.user_scans > 0 THEN CONVERT (VARCHAR (20), sqBAQ.user_scans)
		ELSE ''
		END) AS user_scans
	,(CASE
		WHEN sqBAQ.user_lookups > 0 THEN CONVERT (VARCHAR (20), sqBAQ.user_lookups)
		ELSE ''
		END) AS user_lookups
	,(CASE
		WHEN sqBAQ.user_updates > 0 THEN CONVERT (VARCHAR (20), sqBAQ.user_updates)
		ELSE ''
		END) AS user_updates
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_user_seek, 23), '') AS last_user_seek
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_user_scan, 23), '') AS last_user_scan
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_user_lookup, 23), '') AS last_user_lookup
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_user_update, 23), '') AS last_user_update
	,(CASE
		WHEN sqBAQ.system_seeks > 0 THEN CONVERT (VARCHAR (20), sqBAQ.system_seeks)
		ELSE ''
		END) AS system_seeks
	,(CASE
		WHEN sqBAQ.system_scans > 0 THEN CONVERT (VARCHAR (20), sqBAQ.system_scans)
		ELSE ''
		END) AS system_scans
	,(CASE
		WHEN sqBAQ.system_lookups > 0 THEN CONVERT (VARCHAR (20), sqBAQ.system_lookups)
		ELSE ''
		END) AS system_lookups
	,(CASE
		WHEN sqBAQ.system_updates > 0 THEN CONVERT (VARCHAR (20), sqBAQ.system_updates)
		ELSE ''
		END) AS system_updates
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_system_seek, 23), '') AS last_system_seek
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_system_scan, 23), '') AS last_system_scan
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_system_lookup, 23), '') AS last_system_lookup
	,ISNULL (CONVERT (VARCHAR (10), sqBAQ.last_system_update, 23), '') AS last_system_update
FROM

	(
		SELECT
			 O.[type]
			,O.[schema_id]
			,O.[object_id]
			,CONVERT (VARCHAR (10), O.create_date, 23) AS create_date
			,CONVERT (VARCHAR (10), O.modify_date, 23) AS modify_date
			,sqDDPS.[rows]
			,sqDDPS.total_pages
			,sqDDPS.used_pages
			,(CASE
				WHEN sqDDPS.total_pages > sqDDPS.used_pages THEN sqDDPS.total_pages - sqDDPS.used_pages
				ELSE 0
				END) AS unused_pages
			,sqDDPS.data_pages
			,(CASE
				WHEN sqDDPS.used_pages > sqDDPS.data_pages THEN sqDDPS.used_pages - sqDDPS.data_pages
				ELSE 0
				END) AS index_pages
			,sqI.type_desc
			,sqI.name AS index_name
			,sqI.is_system_named
			,sqI.is_primary_key
			,sqI.is_unique
			,sqI.is_disabled
			,sqI.is_hypothetical
			,sqI.individual_index_pages
			,sqI.is_unused
			,sqI.[allow_row_locks]
			,sqI.[allow_page_locks]
			,sqI.[ignore_dup_key]
			,sqI.no_recompute
			,sqI.is_padded
			,sqI.fill_factor
			,sqI.user_seeks
			,sqI.user_scans
			,sqI.user_lookups
			,sqI.user_updates
			,sqI.last_user_seek
			,sqI.last_user_scan
			,sqI.last_user_lookup
			,sqI.last_user_update
			,sqI.system_seeks
			,sqI.system_scans
			,sqI.system_lookups
			,sqI.system_updates
			,sqI.last_system_seek
			,sqI.last_system_scan
			,sqI.last_system_lookup
			,sqI.last_system_update
			,sqI.is_unique_constraint
			,sqI.index_id
			,sqI.row_filter
		FROM
			sys.objects O
			INNER JOIN

				(
					SELECT
						 ttIBSI.[object_id]
						,SUM (ttIBSI.[rows]) AS [rows]
						,SUM (ttIBSI.total_pages) AS total_pages
						,SUM (ttIBSI.used_pages) AS used_pages
						,SUM (ttIBSI.data_pages) AS data_pages
					FROM
						dbo.#temp_index_breakdown_size_info ttIBSI
					GROUP BY
						ttIBSI.[object_id]
				) sqDDPS ON sqDDPS.[object_id] = O.[object_id]

			INNER JOIN

				(
					SELECT
						 I.[object_id]
						,I.type_desc
						,I.name
						,KC.is_system_named
						,I.is_primary_key
						,I.is_unique
						,I.is_disabled
						,I.is_hypothetical
						,(CASE
							WHEN ttIBSI.used_pages > ttIBSI.data_pages THEN ttIBSI.used_pages - ttIBSI.data_pages
							END) AS individual_index_pages
						,(CASE
							WHEN I.[type] = 0 THEN ''
							WHEN I.[type] = 1 THEN REPLICATE ('.', 6)
							WHEN I.is_primary_key = 1 THEN REPLICATE ('.', 6)
							WHEN I.is_unique = 1 THEN REPLICATE ('.', 6)
							WHEN EXISTS

								(
									SELECT
										*
									FROM
										sys.index_columns IC
										INNER JOIN sys.foreign_key_columns FKC ON FKC.parent_object_id = IC.[object_id]
											AND FKC.parent_column_id = IC.column_id
									WHERE
										IC.[object_id] = I.[object_id]
										AND IC.index_id = I.index_id
								) THEN REPLICATE ('.', 6)

							WHEN DDIUS.[object_id] IS NOT NULL THEN (CASE
																		WHEN DDIUS.user_seeks + DDIUS.user_scans + DDIUS.user_lookups + DDIUS.user_updates = 0 THEN 'Y/N'
																		ELSE 'No'
																		END)
							ELSE 'Yes'
							END) AS is_unused
						,I.[allow_row_locks]
						,I.[allow_page_locks]
						,I.[ignore_dup_key]
						,S.no_recompute
						,I.is_padded
						,I.fill_factor
						,DDIUS.user_seeks
						,DDIUS.user_scans
						,DDIUS.user_lookups
						,DDIUS.user_updates
						,DDIUS.last_user_seek
						,DDIUS.last_user_scan
						,DDIUS.last_user_lookup
						,DDIUS.last_user_update
						,DDIUS.system_seeks
						,DDIUS.system_scans
						,DDIUS.system_lookups
						,DDIUS.system_updates
						,DDIUS.last_system_seek
						,DDIUS.last_system_scan
						,DDIUS.last_system_lookup
						,DDIUS.last_system_update
						,I.is_unique_constraint
						,I.index_id
						,(CASE
							WHEN @Report_Style = 0 THEN 1
							ELSE ROW_NUMBER () OVER
													(
														PARTITION BY
															I.[object_id]
														ORDER BY
															 I.is_primary_key DESC
															,(CASE
																WHEN I.[type] = 0 THEN 'Z'
																ELSE 'A'
																END)
															,I.[type]
															,I.name
													)
							END) AS row_filter
					FROM
						sys.indexes I
						LEFT JOIN dbo.#temp_index_breakdown_size_info ttIBSI ON ttIBSI.[object_id] = I.[object_id]
							AND ttIBSI.index_id = I.index_id
						LEFT JOIN sys.key_constraints KC ON KC.parent_object_id = I.[object_id]
							AND KC.unique_index_id = I.index_id
						LEFT JOIN sys.stats S ON S.[object_id] = I.[object_id]
							AND S.stats_id = I.index_id
						LEFT JOIN master.sys.dm_db_index_usage_stats DDIUS ON DDIUS.[object_id] = I.[object_id]
							AND DDIUS.index_id = I.index_id
							AND DDIUS.database_id = @Database_ID
				) sqI ON sqI.[object_id] = O.[object_id]

		WHERE
			O.[type] IN ('U', 'V')
			AND O.is_ms_shipped = 0
			AND NOT

				(
					SCHEMA_NAME (O.[schema_id]) = N'dbo'
					AND O.name = N'sysdiagrams'
					AND O.[type] = 'U'
				)

	) sqBAQ

	INNER JOIN

		(
			SELECT
				 C.[object_id]
				,COUNT (*) + .0 AS total_columns
			FROM
				sys.columns C
			GROUP BY
				C.[object_id]
		) sqCC ON sqCC.[object_id] = sqBAQ.[object_id]

	LEFT JOIN dbo.#temp_index_breakdown_keys_filters ttIBKF ON ttIBKF.[object_id] = sqBAQ.[object_id]
		AND ttIBKF.index_id = sqBAQ.index_id
	LEFT JOIN

		(
			SELECT
				 IC.[object_id]
				,IC.index_id
				,SUM (C.max_length) AS total_max_length
			FROM
				sys.index_columns IC
				INNER JOIN sys.columns C ON C.[object_id] = IC.[object_id]
					AND C.column_id = IC.column_id
			WHERE
				IC.is_included_column = 0
			GROUP BY
				 IC.[object_id]
				,IC.index_id
		) sqKL ON sqKL.[object_id] = sqBAQ.[object_id] AND sqKL.index_id = sqBAQ.index_id

	LEFT JOIN

		(
			SELECT
				 sqED01.dupe_rank
				,sqED01.total_dupes
				,ROW_NUMBER () OVER
									(
										ORDER BY
											(SELECT NULL)
									) AS dupe_id
			FROM

				(
					SELECT
						 ttIBKF.dupe_rank
						,COUNT (*) AS total_dupes
					FROM
						dbo.#temp_index_breakdown_keys_filters ttIBKF
					GROUP BY
						ttIBKF.dupe_rank
					HAVING
						COUNT (*) > 1
				) sqED01

		) sqED02 ON sqED02.dupe_rank = ttIBKF.dupe_rank

	CROSS APPLY

		(
			SELECT
				 (CASE
					WHEN sqBAQ.modify_date = sqBAQ.create_date THEN REPLICATE ('.', 18)
					ELSE sqBAQ.modify_date
					END) AS modify_date
				,LEN (ttIBKF.index_key) - LEN (REPLACE (ttIBKF.index_key, '•', '')) AS [index_columns]
				,ISNULL (LEN (ttIBKF.include_key) - LEN (REPLACE (ttIBKF.include_key, '•', '')), 0) AS include_columns
		) caMDKL

ORDER BY
	 sqBAQ.[type]
	,SCHEMA_NAME (sqBAQ.[schema_id])
	,OBJECT_NAME (sqBAQ.[object_id])
	,sqBAQ.row_filter


-----------------------------------------------------------------------------------------------------------------------------
--	Cleanup: Drop Any Remaining Temp Tables
-----------------------------------------------------------------------------------------------------------------------------

IF OBJECT_ID (N'tempdb.dbo.#temp_index_breakdown_keys_filters', N'U') IS NOT NULL
BEGIN

	DROP TABLE dbo.#temp_index_breakdown_keys_filters

END


IF OBJECT_ID (N'tempdb.dbo.#temp_index_breakdown_size_info', N'U') IS NOT NULL
BEGIN

	DROP TABLE dbo.#temp_index_breakdown_size_info

END
GO
/*
Author: Kendra Little
Original link: http://www.littlekendra.com/2016/05/05/how-to-script-out-indexes-from-sql-server/
*/
SELECT 
    DB_NAME() AS database_name,
    sc.name + N'.' + t.name AS table_name,
    (SELECT MAX(user_reads) 
        FROM (VALUES (last_user_seek), (last_user_scan), (last_user_lookup)) AS value) AS last_user_read,
    last_user_update,
    CASE si.index_id WHEN 0 THEN N'/* No create statement (Heap) */'
    ELSE 
        CASE is_primary_key WHEN 1 THEN
            N'ALTER TABLE ' + QUOTENAME(sc.name) + N'.' + QUOTENAME(t.name) + N' ADD CONSTRAINT ' + QUOTENAME(si.name) + N' PRIMARY KEY ' +
                CASE WHEN si.index_id > 1 THEN N'NON' ELSE N'' END + N'CLUSTERED '
            ELSE N'CREATE ' + 
                CASE WHEN si.is_unique = 1 then N'UNIQUE ' ELSE N'' END +
                CASE WHEN si.index_id > 1 THEN N'NON' ELSE N'' END + N'CLUSTERED ' +
                N'INDEX ' + QUOTENAME(si.name) + N' ON ' + QUOTENAME(sc.name) + N'.' + QUOTENAME(t.name) + N' '
        END +
        /* key def */ N'(' + key_definition + N')' +
        /* includes */ CASE WHEN include_definition IS NOT NULL THEN 
            N' INCLUDE (' + include_definition + N')'
            ELSE N''
        END +
        /* filters */ CASE WHEN filter_definition IS NOT NULL THEN 
            N' WHERE ' + filter_definition ELSE N''
        END +
        /* with clause - compression goes here */
        CASE WHEN row_compression_partition_list IS NOT NULL OR page_compression_partition_list IS NOT NULL 
            THEN N' WITH (' +
                CASE WHEN row_compression_partition_list IS NOT NULL THEN
                    N'DATA_COMPRESSION = ROW ' + CASE WHEN psc.name IS NULL THEN N'' ELSE + N' ON PARTITIONS (' + row_compression_partition_list + N')' END
                ELSE N'' END +
                CASE WHEN row_compression_partition_list IS NOT NULL AND page_compression_partition_list IS NOT NULL THEN N', ' ELSE N'' END +
                CASE WHEN page_compression_partition_list IS NOT NULL THEN
                    N'DATA_COMPRESSION = PAGE ' + CASE WHEN psc.name IS NULL THEN N'' ELSE + N' ON PARTITIONS (' + page_compression_partition_list + N')' END
                ELSE N'' END
            + N')'
            ELSE N''
        END +
        /* ON where? filegroup? partition scheme? */
        ' ON ' + CASE WHEN psc.name is null 
            THEN ISNULL(QUOTENAME(fg.name),N'')
            ELSE psc.name + N' (' + partitioning_column.column_name + N')' 
            END
        + N';'
    END AS index_create_statement,
    si.index_id,
    si.name AS index_name,
    partition_sums.reserved_in_row_GB,
    partition_sums.reserved_LOB_GB,
    partition_sums.row_count,
    stat.user_seeks,
    stat.user_scans,
    stat.user_lookups,
    user_updates AS queries_that_modified,
    partition_sums.partition_count,
    si.[allow_page_locks],
    si.[allow_row_locks],
    si.is_hypothetical,
    si.has_filter,
    si.fill_factor,
    si.is_unique,
    ISNULL(pf.name, '/* Not partitioned */') AS partition_function,
    ISNULL(psc.name, fg.name) AS partition_scheme_or_filegroup,
    t.create_date AS table_created_date,
    t.modify_date AS table_modify_date
FROM sys.indexes AS si
JOIN sys.tables AS t ON si.object_id=t.object_id
JOIN sys.schemas AS sc ON t.schema_id=sc.schema_id
LEFT JOIN sys.dm_db_index_usage_stats AS stat ON 
    stat.database_id = DB_ID() 
    and si.object_id=stat.object_id 
    and si.index_id=stat.index_id
LEFT JOIN sys.partition_schemes AS psc ON si.data_space_id=psc.data_space_id
LEFT JOIN sys.partition_functions AS pf ON psc.function_id=pf.function_id
LEFT JOIN sys.filegroups AS fg ON si.data_space_id=fg.data_space_id
/* Key list */ OUTER APPLY ( SELECT STUFF (
    (SELECT N', ' + QUOTENAME(c.name) +
        CASE ic.is_descending_key WHEN 1 then N' DESC' ELSE N'' END
    FROM sys.index_columns AS ic 
    JOIN sys.columns AS c ON 
        ic.column_id=c.column_id  
        and ic.object_id=c.object_id
    WHERE ic.object_id = si.object_id
        and ic.index_id=si.index_id
        and ic.key_ordinal > 0
    ORDER BY ic.key_ordinal FOR XML PATH(''), TYPE),1,2,'')) AS keys
/* Partitioning Ordinal */ OUTER APPLY (
    SELECT MAX(QUOTENAME(c.name)) AS column_name
    FROM sys.index_columns AS ic 
    JOIN sys.columns AS c ON 
        ic.column_id=c.column_id  
        and ic.object_id=c.object_id
    WHERE ic.object_id = si.object_id
        and ic.index_id=si.index_id
        and ic.partition_ordinal = 1) AS partitioning_column
/* Include list */ OUTER APPLY ( SELECT STUFF (
    (SELECT N', ' + QUOTENAME(c.name)
    FROM sys.index_columns AS ic 
    JOIN sys.columns AS c ON 
        ic.column_id=c.column_id  
        and ic.object_id=c.object_id
    WHERE ic.object_id = si.object_id
        and ic.index_id=si.index_id
        and ic.is_included_column = 1
    ORDER BY c.name FOR XML PATH(''), TYPE),1,2,'')) AS includes
/* Partitions */ OUTER APPLY ( 
    SELECT 
        COUNT(*) AS partition_count,
        CAST(SUM(ps.in_row_reserved_page_count)*8./1024./1024. AS NUMERIC(32,1)) AS reserved_in_row_GB,
        CAST(SUM(ps.lob_reserved_page_count)*8./1024./1024. AS NUMERIC(32,1)) AS reserved_LOB_GB,
        SUM(ps.row_count) AS row_count
    FROM sys.partitions_ AS p
    JOIN sys.dm_db_partition_stats AS ps ON
        p.partition_id=ps.partition_id
    WHERE p.object_id = si.object_id
        and p.index_id=si.index_id
    ) AS partition_sums
/* row compression list by partition */ OUTER APPLY ( SELECT STUFF (
    (SELECT N', ' + CAST(p.partition_number AS VARCHAR(32))
    FROM sys.partitions_ AS p
    WHERE p.object_id = si.object_id
        and p.index_id=si.index_id
        and p.data_compression_ = 1
    ORDER BY p.partition_number FOR XML PATH(''), TYPE),1,2,'')) AS row_compression_clause
/* data compression list by partition */ OUTER APPLY ( SELECT STUFF (
    (SELECT N', ' + CAST(p.partition_number AS VARCHAR(32))
    FROM sys.partitions_ AS p
    WHERE p.object_id = si.object_id
        and p.index_id=si.index_id
        and p.data_compression_ = 2
    ORDER BY p.partition_number FOR XML PATH(''), TYPE),1,2,'')) AS page_compression_clause
WHERE 
    si.type IN (0,1,2) /* heap, clustered, nonclustered */
ORDER BY table_name, si.index_id
    OPTION (RECOMPILE);
GO

GO
/*
Author: Paul S. Randal
Original link: http://www.sqlskills.com/blogs/paul/most-common-latch-classes-and-what-they-mean/
*/
WITH [Latches] AS
    (SELECT
        [latch_class],
        [wait_time_ms] / 1000.0 AS [WaitS],
        [waiting_requests_count] AS [WaitCount],
        100.0 * [wait_time_ms] / SUM ([wait_time_ms]) OVER() AS [Percentage],
        ROW_NUMBER() OVER(ORDER BY [wait_time_ms] DESC) AS [RowNum]
    FROM sys.dm_os_latch_stats
    WHERE [latch_class] NOT IN (
        N'BUFFER')
    AND [wait_time_ms] > 0
)
SELECT
    MAX ([W1].[latch_class]) AS [LatchClass], 
    CAST (MAX ([W1].[WaitS]) AS DECIMAL(14, 2)) AS [Wait_S],
    MAX ([W1].[WaitCount]) AS [WaitCount],
    CAST (MAX ([W1].[Percentage]) AS DECIMAL(14, 2)) AS [Percentage],
    CAST ((MAX ([W1].[WaitS]) / MAX ([W1].[WaitCount])) AS DECIMAL (14, 4)) AS [AvgWait_S]
FROM [Latches] AS [W1]
INNER JOIN [Latches] AS [W2]
    ON [W2].[RowNum] <= [W1].[RowNum]
GROUP BY [W1].[RowNum]
HAVING SUM ([W2].[Percentage]) - MAX ([W1].[Percentage]) < 95; -- percentage threshold
GO

GO
/*
Author: Perry Whittle
Original link: http://sqlstudies.com/2013/03/01/script-to-clean-up-windows-logins-no-longer-in-ad/
*/

SELECT * FROM
(
 SELECT JobName, RunStart, DATEADD(second, RunSeconds, RunStart) RunEnd, RunSeconds
 FROM
 (
  SELECT j.name AS 'JobName',
    msdb.dbo.agent_datetime(run_date, run_time) AS 'RunStart',
    ((jh.run_duration/1000000)*86400) 
    + (((jh.run_duration-((jh.run_duration/1000000)*1000000))/10000)*3600) 
    + (((jh.run_duration-((jh.run_duration/10000)*10000))/100)*60) 
    + (jh.run_duration-(jh.run_duration/100)*100) RunSeconds
  FROM msdb.dbo.sysjobs j 
  INNER JOIN msdb.dbo.sysjobhistory jh ON j.job_id = jh.job_id 
  WHERE jh.step_id=0 --The Summary Step
 ) AS H
) AS H2
WHERE '2016-05-19 10:16:10' BETWEEN RunStart AND RunEnd
ORDER BY JobName, RunEnd;

GO
/* Use coreinfo (utility by sysinternals) as this will give you
a. Logical to Physical Processor Map
b. Logical Processor to Socket Map
c. Logical Processor to NUMA Node Map as below :
Logical to Physical Processor Map:
**----------------------  Physical Processor 0 (Hyperthreaded)
--**--------------------  Physical Processor 1 (Hyperthreaded)
----**------------------  Physical Processor 2 (Hyperthreaded)
------**----------------  Physical Processor 3 (Hyperthreaded)
--------**--------------  Physical Processor 4 (Hyperthreaded)
----------**------------  Physical Processor 5 (Hyperthreaded)
------------**----------  Physical Processor 6 (Hyperthreaded)
--------------**--------  Physical Processor 7 (Hyperthreaded)
----------------**------  Physical Processor 8 (Hyperthreaded)
------------------**----  Physical Processor 9 (Hyperthreaded)
--------------------**--  Physical Processor 10 (Hyperthreaded)
----------------------**  Physical Processor 11 (Hyperthreaded)
Logical Processor to Socket Map:
************------------  Socket 0
------------************  Socket 1
Logical Processor to NUMA Node Map:
************------------  NUMA Node 0
------------************  NUMA Node 1
Now, based on the above info, the Ideal MaxDop setting should be calculated as
a.  It has 12 CPU’s which are hyper threaded giving us 24 CPUs.
b.  It has 2 NUMA node [Node 0 and 1] each having 12 CPU’s with Hyperthreading ON.
c.  Number of sockets are 2 [socket 0 and 1] which are housing 12 CPU’s each.
Considering all above factors, the max degree of Parallelism should be set to 6 which is ideal value for server with above configuration.
So the answer is -- "it depends" on your processor footprint and the NUMA configuration and below table will summarize what I explained above:
8 or less processors    ===> 0 to N (where N= no. of processors)
More than 8 processors  ===> 8
NUMA configured         ===> MAXDOP should not exceed no of CPU’s assigned to each 
                                 NUMA node with max value capped to 8
Hyper threading Enabled ===> Should not exceed the number of physical processors.
Below is a quick and dirty TSQL script to generate Recommendation for MAXDOP setting

Author          :   Kin Shah
Purpose         :   Recommend MaxDop settings for the server instance
Tested RDBMS    :   SQL Server 2008R2 */

declare @hyperthreadingRatio bit
declare @logicalCPUs int
declare @HTEnabled int
declare @physicalCPU int
declare @SOCKET int
declare @logicalCPUPerNuma int
declare @NoOfNUMA int

select @logicalCPUs = cpu_count -- [Logical CPU Count]
    ,@hyperthreadingRatio = hyperthread_ratio --  [Hyperthread Ratio]
    ,@physicalCPU = cpu_count / hyperthread_ratio -- [Physical CPU Count]
    ,@HTEnabled = case 
        when cpu_count > hyperthread_ratio
            then 1
        else 0
        end -- HTEnabled
from sys.dm_os_sys_info
option (recompile);

select @logicalCPUPerNuma = COUNT(parent_node_id) -- [NumberOfLogicalProcessorsPerNuma]
from sys.dm_os_schedulers
where [status] = 'VISIBLE ONLINE'
    and parent_node_id < 64
group by parent_node_id
option (recompile);

select @NoOfNUMA = count(distinct parent_node_id)
from sys.dm_os_schedulers -- find NO OF NUMA Nodes 
where [status] = 'VISIBLE ONLINE'
    and parent_node_id < 64

-- Report the recommendations ....
select
    --- 8 or less processors and NO HT enabled
    case 
        when @logicalCPUs < 8
            and @HTEnabled = 0
            then 'MAXDOP setting should be : ' + CAST(@logicalCPUs as varchar(3))
                --- 8 or more processors and NO HT enabled
        when @logicalCPUs >= 8
            and @HTEnabled = 0
            then 'MAXDOP setting should be : 8'
                --- 8 or more processors and HT enabled and NO NUMA
        when @logicalCPUs >= 8
            and @HTEnabled = 1
            and @NoofNUMA = 1
            then 'MaxDop setting should be : ' + CAST(@logicalCPUPerNuma / @physicalCPU as varchar(3))
                --- 8 or more processors and HT enabled and NUMA
        when @logicalCPUs >= 8
            and @HTEnabled = 1
            and @NoofNUMA > 1
            then 'MaxDop setting should be : ' + CAST(@logicalCPUPerNuma / @physicalCPU as varchar(3))
        else ''
        end as Recommendations
GO
/*
    Max Server Memory Calculator
    https://bornsql.ca/memory/
    Copyright (c) BornSQL.ca
    Written by Randolph West, released under the MIT License
    Last updated: 9 November 2016
    Based on an original algorithm by Jonathan Kehayias:
    https://www.sqlskills.com/blogs/jonathan/how-much-memory-does-my-sql-server-actually-need/
    Max Worker Thread Stack calculation based on Tiger Toolbox Maintenance Solution.
    Copyright (c) Microsoft Corporation. All rights reserved.
    https://github.com/Microsoft/tigertoolbox/tree/master/MaintenanceSolution
    SQL Server, on a standalone instance, requires the following reserved RAM for a server:
    - 1 GB of RAM for the OS
    - plus 1 GB for each 4 GB of RAM installed from 4 - 16 GB
    - plus 1 GB for every 8 GB RAM installed above 16 GB RAM
    
    Memory for the Thread Stack can also be taken into account:
    - 32-bit, reserve 512KB per thread * Max Worker Threads
    - 64-bit, reserve 2MB per thread * Max Worker Threads
    - 128-bit, reserve 4MB per thread * Max Worker Threads
    
    Thanks to @sqlEmt and @sqlstudent144 for testing.
    Thanks to the Tiger Team for version number and thread stack calculations.
v1.0 - 2016-08-19 - Initial release.
v1.1 - 2016-11-22 - Thread stack reservation; NUMA affinity; new version check.
*/

-- Set this to 1 if you want to configure NUMA Node Affinity
DECLARE @configureNumaNodeAffinity BIT = 0;

DECLARE @physicalMemorySource DECIMAL(20, 4);
DECLARE @physicalMemory DECIMAL(20, 4);
DECLARE @recommendedMemory DECIMAL(20, 4);
DECLARE @overheadMemory DECIMAL(20, 4);

DECLARE @cpuArchitecture DECIMAL(20, 4);
DECLARE @numaNodes INT;
DECLARE @numaNodesAfinned TINYINT;
DECLARE @maxWorkerThreadCount INT;
DECLARE @threadStack DECIMAL(20, 4);

SELECT @cpuArchitecture = CASE WHEN @@VERSION LIKE '%<X64>%' THEN 2 WHEN @@VERSION LIKE '%<IA64>%' THEN 4 ELSE 0.5 END FROM sys.dm_os_windows_info WITH (NOLOCK);
SELECT @numaNodes = COUNT(DISTINCT parent_node_id) FROM sys.dm_os_schedulers WHERE scheduler_id < 255 AND parent_node_id < 64;
SELECT @numaNodesAfinned = COUNT (DISTINCT parent_node_id) FROM sys.dm_os_schedulers WHERE scheduler_id < 255 AND parent_node_id < 64 AND is_online = 1;
SELECT @maxWorkerThreadCount = max_workers_count FROM sys.dm_os_sys_info;
SELECT @threadStack = @maxWorkerThreadCount * @cpuArchitecture / 1024.0;

-- Get physical RAM on server
SELECT @physicalMemorySource = CAST(total_physical_memory_kb AS DECIMAL(20, 4)) / CAST((1024.0) AS DECIMAL(20, 4))
FROM sys.dm_os_sys_memory;

-- Convert to nearest GB
SELECT @physicalMemory = CEILING(@physicalMemorySource / CAST(1024.0 AS DECIMAL(20, 4)));

IF (@physicalMemory <= 2.0)
BEGIN
	SELECT @overheadMemory = 0.5;
END;

IF (@physicalMemory > 2.0
	AND @physicalMemory < 4.0)
BEGIN
	SELECT @overheadMemory = 2.0;
END;

IF (@physicalMemory >= 4.0
	AND @physicalMemory <= 16.0)
BEGIN
	SELECT @overheadMemory = 1.0 /* Operating System minimum */
		+ (@physicalMemory / 4.0);
END;

IF (@physicalMemory > 16.0)
BEGIN
	SELECT @overheadMemory = 1.0 /* Operating System minimum */ + 4.0 /* add in reserved for <= 16GB */
		+ ((@physicalMemory - 16.0) / 8.0);
END;

-- Add in the Max Worker Threads Overhead
SELECT @overheadMemory = @overheadMemory + @threadStack;

DECLARE @editionId BIGINT = CAST(SERVERPROPERTY('EditionID') AS BIGINT);
DECLARE @enterprise BIT = 0;
DECLARE @developer BIT = 0;
DECLARE @override BIT = 0;

IF (
		@editionId IN (
			1804890536,
			1872460670,
			610778273
			)
		)
BEGIN
	SELECT @enterprise = 1;
END;

IF (@editionId = - 2117995310)
	SELECT @developer = 1;

-- Check for Standard Edition Limitations
IF (
		@enterprise = 0
		AND @developer = 0
		)
BEGIN
	DECLARE @ProductVersion INT = CONVERT(INT, (@@MICROSOFTVERSION / 0x1000000) & 0xff);

	IF (@ProductVersion >= 11)
		AND (@physicalMemory > 128)
	BEGIN
		SELECT @overheadMemory = 1.0 + 4.0 + ((128 - 16.0) / 8.0);

		-- Set the memory value to the max allowed, if there is enough headroom
		IF (@physicalMemory - @overheadMemory >= 128)
			SELECT @recommendedMemory = 128,
				@overheadMemory = 0,
				@override = 1;
	END;

	IF (@ProductVersion < 11)
		AND (@physicalMemory > 64)
	BEGIN
		SELECT @overheadMemory = 1.0 + 4.0 + ((64 - 16.0) / 8.0);

		-- Set the memory value to the max allowed, if there is enough headroom
		IF (@physicalMemory - @overheadMemory >= 64)
			SELECT @recommendedMemory = 64,
				@overheadMemory = 0,
				@override = 1;
	END;
END;

IF (@override = 0)
	SELECT @recommendedMemory = @physicalMemory - @overheadMemory;

-- Configure NUMA Affinity
IF (@configureNumaNodeAffinity = 1)
BEGIN
	SELECT @recommendedMemory = (@recommendedMemory / @numaNodes) * @numaNodesAfinned;
END;

SELECT @@VERSION AS [Version],
	CASE 
		WHEN (@enterprise = 1)
			THEN 'Enterprise Edition'
		WHEN (@developer = 1)
			THEN 'Developer Edition'
		ELSE 'Non-Enterprise Edition'
		END AS [Edition],
	CAST(@physicalMemorySource AS INT) AS [Physical RAM (MB)],
	--CAST(@physicalMemory AS INT) AS [Physical RAM (GB)],
	c.[value] AS [Configured Value (MB)],
	c.[value_in_use] AS [Running Value (MB)],
	CAST(@recommendedMemory * 1024 AS INT) AS [Recommended Value (MB)],
	N'EXEC sp_configure ''show advanced options'', 1; RECONFIGURE WITH OVERRIDE; EXEC sp_configure ''max server memory (MB)'', '
		+ CAST(CAST(@recommendedMemory * 1024 AS INT) AS NVARCHAR(20))
		+ '; EXEC sp_configure ''show advanced options'', 0; RECONFIGURE WITH OVERRIDE;' AS [Script]
FROM sys.configurations c
WHERE [c].[name] = N'max server memory (MB)'
OPTION (RECOMPILE);
GO
GO
/*
Author: Kendra Little
Original link: https://sqlworkbooks.com/2017/06/top-5-misleading-sql-server-performance-counters
Desctiption: Top 5 Misleading SQL Server Performance Counters
*/


SELECT TOP 20
	(SELECT CAST(SUBSTRING(st.text, (qs.statement_start_offset/2)+1,   
		((CASE qs.statement_end_offset  
			WHEN -1 THEN DATALENGTH(st.text)  
			ELSE qs.statement_end_offset  
			END
		- qs.statement_start_offset)/2) + 1) AS NVARCHAR(MAX)) FOR XML PATH(''),TYPE) AS [TSQL],
    qs.execution_count AS [#],
    qs.total_logical_reads as [logical reads],
	CASE WHEN execution_count = 0 THEN 0 ELSE
		CAST(qs.total_logical_reads / execution_count AS numeric(30,1))
	END AS [avg logical reads],
    CAST(qs.total_worker_time/1000./1000. AS numeric(30,1)) AS [cpu sec],
	CASE WHEN execution_count = 0 THEN 0 ELSE
		CAST(qs.total_worker_time / execution_count / 1000. / 1000. AS numeric(30,1))
		END AS [avg cpu sec],
    CAST(qs.total_elapsed_time/1000./1000. AS numeric(30,1)) AS [elapsed sec],
	CASE WHEN execution_count = 0 THEN 0 ELSE
		CAST(qs.total_elapsed_time / execution_count / 1000. / 1000. AS numeric(30,1))
		END AS [avg elapsed sec],
    qp.query_plan AS [query execution plan]
FROM sys.dm_exec_query_stats AS qs
OUTER APPLY sys.dm_exec_sql_text (plan_handle) as st
OUTER APPLY sys.dm_exec_query_plan (plan_handle) AS qp
ORDER BY qs.total_logical_reads DESC
OPTION (RECOMPILE);
GO

GO
/*
Original link: http://michaeljswart.com/2016/01/monitor_deadlocks/
Author: Michael J. Swart
*/

/*
Create The Session
 - has five rollover files so that a couple server restarts don�t lose any recent deadlock graphs
 - uses an asynchronous_file_target which I prefer over the ring buffer
 - and it cleans itself up over time.

*/
DECLARE @ExtendedEventsTargetPath sysname = 'Change this string to something like "D:\XEvents\Traces"';
DECLARE @SQL nvarchar(max) = N'
CREATE EVENT SESSION [capture_deadlocks] ON SERVER
ADD EVENT sqlserver.xml_deadlock_report( ACTION(sqlserver.database_name) )
ADD TARGET package0.asynchronous_file_target(
  SET filename = ''' + @ExtendedEventsTargetPath + N'\capture_deadlocks.xel'',
      max_file_size = 10,
      max_rollover_files = 5)
WITH (
    STARTUP_STATE=ON,
    EVENT_RETENTION_MODE=ALLOW_SINGLE_EVENT_LOSS,
    MAX_DISPATCH_LATENCY=15 SECONDS,
    TRACK_CAUSALITY=OFF
    );

ALTER EVENT SESSION [capture_deadlocks] ON SERVER
    STATE=START';

EXEC sp_executesql @SQL;


-- Query the Results
DECLARE @filenamePattern sysname;
 
SELECT @filenamePattern = REPLACE( CAST(field.value AS sysname), '.xel', '*xel' )
FROM sys.server_event_sessions AS [session]
JOIN sys.server_event_session_targets AS [target]
  ON [session].event_session_id = [target].event_session_id
JOIN sys.server_event_session_fields AS field 
  ON field.event_session_id = [target].event_session_id
  AND field.object_id = [target].target_id	
WHERE
    field.name = 'filename'
    and [session].name= N'capture_deadlocks';

SELECT deadlockData.*
FROM sys.fn_xe_file_target_read_file ( @filenamePattern, null, null, null) 
    as event_file_value
CROSS APPLY ( SELECT CAST(event_file_value.[event_data] as xml) ) 
    as event_file_value_xml ([xml])
CROSS APPLY (
    SELECT 
        event_file_value_xml.[xml].value('(event/@name)[1]', 'varchar(100)') as eventName,
        event_file_value_xml.[xml].value('(event/@timestamp)[1]', 'datetime') as eventDate,
        event_file_value_xml.[xml].query('//event/data/value/deadlock') as deadlock	
  ) as deadlockData
WHERE deadlockData.eventName = 'xml_deadlock_report'
ORDER BY eventDate;

GO
/*
Author: Tim Ford
Original link: http://sqlmag.com/database-security/identifying-logins-default-database-connectivity-issues-sql-server
*/
--============================================================
--IDENTIFY ALL LOGINS WITH NO ASSOCIATED DEFAULT DATABASE USER
--============================================================
---------------------------------------------------
/*
DYNAMICALLY SCRIPT NEW DEFAULT DATABASE ASSIGNMENT
FOR ANY LOGIN MISSING THEIR DEFAULT DATABASE
*/
---------------------------------------------------
SET NOCOUNT ON;
DECLARE @sql_text nvarchar(max);
DECLARE @database_name sysname;

IF EXISTS(SELECT name FROM tempdb.sys.tables WHERE name LIKE '#existing_db_users_table%')
        BEGIN
                DROP TABLE #existing_db_users_table;
        END

CREATE TABLE #existing_db_users_table 
        (
                server_login_name sysname NOT NULL
                , database_user_name sysname NOT NULL
                , default_database_name sysname NOT NULL
        );      

DECLARE cur_default_databases CURSOR FORWARD_ONLY STATIC FOR
        SELECT name 
        FROM sys.databases 
        ORDER BY name

OPEN cur_default_databases

FETCH NEXT FROM cur_default_databases INTO @database_name

WHILE @@FETCH_STATUS = 0
        BEGIN

                SELECT @sql_text = 
                'INSERT INTO #existing_db_users_table(server_login_name, database_user_name, default_database_name)
                SELECT SP.name AS [server_login_name], DP.name AS [database_user_name], ' + '''' + @database_name + '''' 
                + ' FROM sys.server_principals AS SP
                        INNER JOIN ' + @database_name + '.sys.database_principals AS DP
                                ON SP.sid = DP.sid
                WHERE SP.default_database_name != ''' + @database_name + ''';'

                EXEC sys.sp_sqlexec @sql_text;

                FETCH NEXT FROM cur_default_databases INTO @database_name
        END 

CLOSE cur_default_databases;
DEALLOCATE cur_default_databases;

--===================================================
-- LISTING OF POTENTIAL DEFAULT DATABASE ASSIGNMENTS
--===================================================
SELECT SP.name AS login_name
        , SP.default_database_name
        , DDUT.default_database_name AS potential_default_database_name
        , CASE 
                WHEN DDUT.default_database_name IS NULL THEN 'Login has no other DB user associations.'
                ELSE 'ALTER LOGIN [' + SP.name + '] WITH DEFAULT_DATABASE=[' + DDUT.default_database_name + '];'
        END AS potential_default_database_command
FROM sys.server_principals AS SP
        LEFT JOIN #existing_db_users_table AS DDUT
                ON SP.name = DDUT.server_login_name
        LEFT JOIN sys.databases AS D
                ON SP.default_database_name = D.name
WHERE SP.type_desc IN ('SQL_LOGIN', 'WINDOWS_LOGIN')
        AND SP.name NOT LIKE 'NT %'
        AND SP.name NOT LIKE '##%'
        AND SP.default_database_name IS NOT NULL
        --AND DDUT.server_login_name IS NOT NULL
        AND D.name IS NULL
ORDER BY SP.name, DDUT.default_database_name;


--===================================================
-- CLEANUP
--===================================================
IF EXISTS(SELECT name FROM tempdb.sys.tables WHERE name LIKE '#missing_existing_db_users_table%')
        BEGIN
                DROP TABLE #missing_existing_db_users_table;
        END
GO

GO
/*
Author: @DBA_ANDY
Original link: http://nebraskasql.blogspot.ru/2017/07/toolbox-which-tables-are-using-all-of.html
Desctiption: Which Tables are Using All of My Space? 
*/

/*
Object Sizes
Modified from http://stackoverflow.com/questions/15896564/get-table-and-index-storage-size-in-sql-server
*/
SELECT TOP 50
 @@SERVERNAME as InstanceName
 , DB_NAME() as DatabaseName
 , s.NAME AS SchemaName
 , t.NAME  AS TableName
 , SUM(p.rows_) AS RowCounts
 --, SUM(a.total_pages) * 8 AS TotalSpaceKB
 , SUM(a.total_pages) * 8/1024.0 AS TotalSpaceMB
 , SUM(a.total_pages) * 8/1024.0/1024.0 AS TotalSpaceGB
 , SUM(a.used_pages) * 8/1024.0 AS UsedSpaceMB
 , (SUM(a.total_pages) - SUM(a.used_pages)) * 8/1024.0 AS UnusedSpaceMB
FROM sys.tables t
INNER JOIN sys.schemas s
ON s.schema_id = t.schema_id
INNER JOIN sys.indexes i
ON t.OBJECT_ID = i.object_id
INNER JOIN sys.partitions_ p
ON i.object_id = p.OBJECT_ID AND i.index_id = p.index_id
INNER JOIN sys.allocation_units a
ON p.partition_id = a.container_id
WHERE t.NAME NOT LIKE 'dt%'    -- filter out system tables for diagramming
AND t.is_ms_shipped = 0
AND i.OBJECT_ID > 255
GROUP BY t.Name
, s.Name
ORDER BY TotalSpaceMB DESC

GO
/*
Author: bornsql
Original link: https://github.com/bornsql/scripts/blob/master/power_saving_check.sql
Desctiption: Create power saving check
*/

DECLARE @isCmdShellEnabled BIT;
DECLARE @isShowAdvanced BIT;

SELECT
	@isCmdShellEnabled = CAST(value AS BIT)
FROM
	sys.configurations
WHERE
	name = 'xp_cmdshell';

SELECT
	@isShowAdvanced = CAST(value AS BIT)
FROM
	sys.configurations
WHERE
	name = 'show advanced options';

IF (@isShowAdvanced = 0)
BEGIN
	EXEC sp_configure 'show advanced options', 1;
	RECONFIGURE;
END;

IF (@isCmdShellEnabled = 0)
BEGIN
	EXEC sp_configure 'xp_cmdshell', 1;
	RECONFIGURE;
END;

--Run xp_cmdshell to get power settings
EXEC xp_cmdshell 'powercfg /list';

--Turn off 'xp_cmdshell'
IF (@isCmdShellEnabled = 0)
BEGIN
	EXEC sp_configure 'xp_cmdshell', 0;
	RECONFIGURE;
END;

--Turn off 'show advanced options'
IF (@isShowAdvanced = 0)
BEGIN
	EXEC sp_configure 'show advanced options', 0;
	RECONFIGURE;
END;
GO
/*
Author: Bert Wagner
Source link: https://blog.bertwagner.com/warning-are-your-queries-vulnerable-to-sql-injection-db914fb39668
*/

SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

SELECT
      ROUTINE_CATALOG
    , ROUTINE_SCHEMA
    , ROUTINE_NAME
    , ROUTINE_TYPE
    , ROUTINE_DEFINITION
FROM
    INFORMATION_SCHEMA.ROUTINES
WHERE
    REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(ROUTINE_DEFINITION,CHAR(0),''),CHAR(9),''),CHAR(10),''),CHAR(11),''),CHAR(12),''),CHAR(13),''),CHAR(14),''),CHAR(160),''),' ','')
        LIKE '%+@%'
    AND 
    ( -- Only if executes a dynamic string
        ROUTINE_DEFINITION LIKE '%EXEC(%'
        OR ROUTINE_DEFINITION LIKE '%EXECUTE%'
        OR ROUTINE_DEFINITION LIKE '%sp_executesql%'
    );

GO
/*
Author: Ryan Adams
Original link: http://www.ryanjadams.com/2016/03/query-active-directory-sql-server/
*/

--There are 2 ways to query AD from SQL Server.&amp;nbsp; The first is using OPENROWSET and the second is using OPENQUERY which requires a linked server.

/*** OPENROWSET METHOD ***/
--You have to enable Ad Hoc Distributed Queries to use OPENROWSET.&amp;nbsp; Note the OPENQUERY does NOT require this to be enabled since it uses Linked servers.

sp_configure 'show advanced options', 1;
RECONFIGURE;
GO
sp_configure 'Ad Hoc Distributed Queries', 1;
RECONFIGURE;
GO
sp_configure 'show advanced options', 0;
RECONFIGURE;
GO

SELECT DisplayName
FROM OPENROWSET('ADSDSOOBJECT','adsdatasource','SELECT displayName
FROM ''LDAP://mydomainFQDN.com/ou=mySubOU,ou=myTopOU,dc=mychilddomain,dc=myTLdomain,dc=com''
WHERE objectClass =&amp;nbsp;''User'' ')

/*** OPENQUERY METHOD ***/
--Here is where we create our Linked Server connection to AD
EXEC master.dbo.sp_addlinkedserver @server = N'AD', @srvproduct=N'Active Directory Services Interface', @provider=N'ADsDSOObject', @datasrc=N'adsdatasource'
GO

EXEC master.dbo.sp_addlinkedsrvlogin @rmtsrvname = N'AD', @locallogin = NULL , @useself = N'True'
GO

--Here is the query using the above created Linked server
SELECT displayName FROM OpenQuery (AD,
'SELECT displayName
FROM ''LDAP://mydomainFQDN.com/ou=mySubOU,ou=myTopOU,dc=mychilddomain,dc=myTLdomain,dc=com''
WHERE objectClass =&amp;nbsp;''User'' ')
GO

--Here we delete our Linked Server
EXEC master.dbo.sp_dropserver @server=N'AD', @droplogins='droplogins'
GO

GO
/*
Author: Bill Fellows 
Original link: http://billfellows.blogspot.ru/2017/06/rename-default-constraints.html
*/
-- Rename default constraints

DECLARE @query nvarchar(4000);
DECLARE
    CSR CURSOR
FAST_FORWARD
FOR
SELECT
    CONCAT('ALTER TABLE ', QUOTENAME(S.name), '.', QUOTENAME(T.name), ' DROP CONSTRAINT [', DC.name, '];', CHAR(10)
    , 'ALTER TABLE ', QUOTENAME(S.name), '.', QUOTENAME(T.name)
    , ' ADD CONSTRAINT [', 'DF__', (S.name), '_', (T.name), '_', C.name, ']'
    , ' DEFAULT ', DC.definition, ' FOR ', QUOTENAME(C.name)) AS Query
FROM
    sys.schemas AS S
    INNER JOIN
        sys.tables AS T
        ON T.schema_id = S.schema_id
    INNER JOIN
        sys.columns AS C
        ON C.object_id = T.object_id
    INNER JOIN
        sys.default_constraints AS DC
        ON DC.parent_object_id = T.object_id
        AND DC.object_id = C.default_object_id
WHERE
    DC.name LIKE 'DF__%'
    AND DC.name <> CONCAT('DF__', (S.name), '_', (T.name), '_', C.name);

OPEN CSR
FETCH NEXT FROM CSR INTO @query;
WHILE @@FETCH_STATUS = 0
BEGIN
    BEGIN TRY
        EXECUTE sys.sp_executesql @query, N'';
    END TRY
    BEGIN CATCH
        PRINT ERROR_MESSAGE()
        PRINT @query;
    END CATCH
    FETCH NEXT FROM CSR INTO @query;
END
CLOSE CSR;
DEALLOCATE CSR;
GO
/*
Author: Joe O'Connor
Original link: http://www.sqlservercentral.com/scripts/Restore/140540/
*/

/*
Description:
T-SQL Script to generate a restore script for a database backed up to disk using Ola Hallengren's
maintenance solution.  The script is based solely on the contents of a directory, taking into 
account the order of full backups, differential backups and transaction log backups.

Maintenance Script Credit: Ola Hallengren
https://ola.hallengren.com/

Original Restore Script Credit: Greg Robidoux
https://www.mssqltips.com/sqlservertip/1584/auto-generate-sql-server-restore-script-from-backup-files-in-a-directory/

Modified Restore Script Credit: Jason Carter
http://jason-carter.net/professional/restore-script-from-backup-directory-modified.html

Reason for change:
Ola's script uses a .BAK extension for differentials, and stores FULL, DIFF and LOG backups in a sub-folder
heirarchy that matches @backupPath\@@SERVERNAME\@dbName\[FULL|DIFF|LOG]\ and the filename also contains what
type of backup file it is within the filename, making filename comparison for order of restore impossible.

ChangeLog: 
	2/24/2016 - Joe O'Connor (thirtybird@gmail.com)
Allow backup paths to have spaces in them by encapsulating path in the command in quotes
Fixed DIR command - /O D to /O:D to guarantee order by date to ensure transaction logs are restored in proper order
Added wrapper to enable and disable xp_cmdshell and re-set the "show advanced options" setting to whatever it was
	If you have xp_cmdshell enabled in your environment, take this part out or it will get disabled!
Took out extra @backupPath in each RESTORE command - the full path is output into the file list and was duplicated in the output
Added backupTime to the local table variable to be used for comparison as comparing filenames doesn't work with Ola's filenames
	This involves substring parsing (and is ugly).  

	2/25/2016 - Joe O'Connor (thirtybird@gmail.com)
Added logic to check to see if xp_cmdshell needs to be enabled or not.  Only enables and disables if it needs to.
Verified it does not disable xp_cmdshell if it was enabled to start with.

	3/16/2016 - Joe O'Connor (thirtybird@gmail.com)
Added a RESTORE FILELISTONLY as the first output command.

	4/20/2016 - Joe O'Connor (thirtybird@gmail.com)
Converted code to utilize xp_dirtree instead of xp_cmdshell
Re-ordered some code to get the variables that need to be set closer to the top of the script
Fixed problem with compatibility with named instances (replacing '\' with '$' in @@SERVERNAME)

Tested against: SQL 2005 - 2014
*/

USE Master;
GO 
SET NOCOUNT ON

/**
	Variable declaration
**/
DECLARE @dbName sysname
DECLARE @backupPath NVARCHAR(500)

/**
	Initialize variables
**/
SET @dbName = 'Test'
SET @backupPath = 'Y:\Microsoft SQL Server\Backup'


/**
	Convert the variables to match that of Olas maintenance script
**/
IF RIGHT (@backupPath,1) = '\' SET @backupPath = SUBSTRING (@backupPath, 1, LEN (@backupPath)-1)
SET @backupPath = @backupPath + '\' + REPLACE(@@SERVERNAME,'\','$') + '\' + @dbName + '\'


/**
	Get List of Files
**/
IF OBJECT_ID('tempdb..#DirectoryTree')IS NOT NULL
	  DROP TABLE #DirectoryTree;

CREATE TABLE #DirectoryTree (
	    backupFile nvarchar(255) NOT NULL
	  , depth int
	  , isfile bit
	  , backupTime NVARCHAR(20));


/**
	Create a clustered index to keep everything in order by filename.
**/
ALTER TABLE #DirectoryTree
ADD CONSTRAINT PK_DirectoryTree PRIMARY KEY CLUSTERED (backupFile);

INSERT #DirectoryTree (backupFile,depth,isfile)
EXEC xp_dirtree @backupPath,2,1;


/**
	Get rid of the directories
**/
DELETE FROM #DirectoryTree WHERE isfile=0


/**
    Figure out the backup time for comparison since file names cannot be compare as all DIFF and LOG backups will be > the full
    Also append the @backupPath to the backup File name in the table as well
**/
-- Update the FULL's
UPDATE #DirectoryTree
SET backupTime = 
SUBSTRING(backupfile, CHARINDEX (@dbName+'_FULL_',backupFile) +LEN(@dbName+'_FULL_'), (LEN(backupfile) - CHARINDEX ('.',REVERSE(backupFile))) + 1 - (CHARINDEX (@dbName+'_FULL_',backupFile) +LEN(@dbName+'_FULL_')))
, backupfile = @backupPath + 'FULL\' + backupfile
FROM #DirectoryTree
WHERE CHARINDEX (@dbName+'_FULL_',backupFile) > 0

-- Update the DIFF's
UPDATE #DirectoryTree
SET backupTime = 
SUBSTRING(backupfile, CHARINDEX (@dbName+'_DIFF_',backupFile) +LEN(@dbName+'_DIFF_'), (LEN(backupfile) - CHARINDEX ('.',REVERSE(backupFile))) + 1 - (CHARINDEX (@dbName+'_DIFF_',backupFile) +LEN(@dbName+'_DIFF_')))
, backupfile = @backupPath + 'DIFF\' + backupfile
FROM #DirectoryTree
WHERE CHARINDEX (@dbName+'_DIFF_',backupFile) > 0

-- Update the LOGs
UPDATE #DirectoryTree
SET backupTime = 
SUBSTRING(backupfile, CHARINDEX (@dbName+'_LOG_',backupFile) +LEN(@dbName+'_LOG_'), (LEN(backupfile) - CHARINDEX ('.',REVERSE(backupFile))) + 1 - (CHARINDEX (@dbName+'_LOG_',backupFile) +LEN(@dbName+'_LOG_')))
, backupfile = @backupPath + 'LOG\' + backupfile
FROM #DirectoryTree
WHERE CHARINDEX (@dbName+'_LOG_',backupFile) > 0

/**
	Find latest full backup
**/
DECLARE           @cmd NVARCHAR(500)
		, @lastFullBackup NVARCHAR(500)
		, @lastDiffBackup NVARCHAR(500)
		, @backupFile NVARCHAR(500)
		, @lastFullBackupTime NVARCHAR(20)
		, @lastDiffBackupTime NVARCHAR(20)

SELECT TOP 1 @lastFullBackup = backupFile
, @lastFullBackupTime = backupTime
FROM #DirectoryTree 
WHERE backupFile LIKE '%' + REPLACE(@@SERVERNAME,'\','$') + '_' + @dbName + '_FULL_%.bak'
ORDER BY backupTime DESC

SET @cmd = 'RESTORE FILELISTONLY FROM DISK = '''  
       + @lastFullBackup + ''' WITH FILE = 1' 
PRINT @cmd 

SET @cmd = 'RESTORE DATABASE [' + @dbName + '] FROM DISK = ''' 
       + @lastFullBackup + ''' WITH NORECOVERY, REPLACE'
PRINT @cmd

/**
	Find latest diff backup
**/
SELECT TOP 1 @lastDiffBackup = backupFile
, @lastDiffBackupTime = backupTime
FROM #DirectoryTree 
WHERE 
	backupFile  LIKE '%' + REPLACE(@@SERVERNAME,'\','$') + '_' + @dbName + '_DIFF_%.bak'
	AND backupTime > @lastFullBackupTime
	ORDER BY backupTime DESC

/**
	check to make sure there is a diff backup
**/
IF @lastDiffBackup IS NOT NULL
	BEGIN
	   SET @cmd = 'RESTORE DATABASE [' + @dbName + '] FROM DISK = ''' 
		   + @lastDiffBackup + ''' WITH NORECOVERY'
	   PRINT @cmd
	   SET @lastFullBackupTime = @lastDiffBackupTime
	END

/**
	check for log backups
**/
DECLARE backupFiles CURSOR FOR 
   SELECT backupFile 
   FROM #DirectoryTree
   WHERE 
	backupFile LIKE  '%' + REPLACE(@@SERVERNAME,'\','$') + '_' + @dbName + '_LOG_%.trn'
	AND backupTime > @lastFullBackupTime

OPEN backupFiles 

/**
	Loop through all the files for the database 
**/
FETCH NEXT FROM backupFiles INTO @backupFile 

WHILE @@FETCH_STATUS = 0 
	BEGIN 
	   SET @cmd = 'RESTORE LOG [' + @dbName + '] FROM DISK = ''' 
		   + @backupFile + ''' WITH NORECOVERY'
	   PRINT @cmd
	   FETCH NEXT FROM backupFiles INTO @backupFile 
	END

CLOSE backupFiles 
DEALLOCATE backupFiles 

/**
	put database in a useable state
**/
SET @cmd = 'RESTORE DATABASE [' + @dbName + '] WITH RECOVERY'
PRINT @cmd


/**
    Cleanup our temp table
**/
IF OBJECT_ID('tempdb..#DirectoryTree')IS NOT NULL
      DROP TABLE #DirectoryTree;
GO

GO
/*
Author: Dmitriy Ivanov
Original link: https://t.me/sqlcom
*/

WITH RingBufferXML
AS(SELECT CAST(Record AS XML) AS RBR FROM sys .dm_os_ring_buffers
   WHERE ring_buffer_type = 'RING_BUFFER_RESOURCE_MONITOR'
  )
SELECT DISTINCT 'Problems' =
          CASE
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]','tinyint')  = 0 AND
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]','tinyint')   = 2 
                    THEN 'Insufficient physical memory for the system'
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]','tinyint')  = 0 AND 
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]','tinyint')   = 4 
                    THEN 'Insufficient virtual memory for the system' 
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]', 'tinyint') = 2 AND 
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]','tinyint')   = 0 
                    THEN'Insufficient physical memory for queries'
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]', 'tinyint') = 4 AND 
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]', 'tinyint')  = 4
                    THEN 'Insufficient virtual memory for queries and system'
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]','tinyint')  = 2 AND 
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]','tinyint')   = 4 
                    THEN 'Insufficient virtual memory for the system and physical for queries'
                    WHEN XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]', 'tinyint') = 2 AND 
                         XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]', 'tinyint')  = 2 
                    THEN 'There is not enough physical memory for the system and requests'
         END
FROM        RingBufferXML
CROSS APPLY RingBufferXML.RBR.nodes ('Record') Record (XMLRecord)
WHERE       XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]','tinyint') IN (0,2,4) AND
            XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]' ,'tinyint') IN (0,2,4) AND
            XMLRecord.value('(ResourceMonitor/IndicatorsProcess)[1]','tinyint') +
            XMLRecord.value('(ResourceMonitor/IndicatorsSystem)[1]' ,'tinyint') > 0;

GO
/*
Author: Ed Pollack
Original link: http://www.sqlshack.com/searching-sql-server-made-easy-building-the-perfect-search-script/
*/

SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
SET NOCOUNT ON;
----------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------Configure Search Here-----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------
DECLARE @search_string NVARCHAR(MAX) = 'Department';
DECLARE @search_SSRS BIT = 1;
DECLARE @search_SSIS_MSDB BIT = 1;
DECLARE @search_SSIS_disk BIT = 0;
DECLARE @pkg_directory NVARCHAR(MAX) = 'C:\SSIS_Packages';
----------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------
SET @search_string = '%' + @search_string + '%';
DECLARE @sql_command NVARCHAR(MAX);
DECLARE @database_name NVARCHAR(MAX);
DECLARE @database TABLE
    (database_name NVARCHAR(MAX), is_online BIT);

IF OBJECT_ID('tempdb..#object_data', 'U') IS NOT NULL
BEGIN
    DROP TABLE #object_data;
    PRINT 'Table #object_data droped'
END

CREATE TABLE #object_data
(    server_name NVARCHAR(MAX) NULL,
    database_name NVARCHAR(MAX) NULL,
    schema_name NVARCHAR(MAX) NULL,
    table_name SYSNAME NULL,
    column_name SYSNAME NULL,
    objectname SYSNAME NULL,
    step_name NVARCHAR(MAX) NULL,
    object_description NVARCHAR(MAX) NULL,
    object_definition NVARCHAR(MAX) NULL,
    key_column_list NVARCHAR(MAX) NULL,
    include_column_list NVARCHAR(MAX) NULL,
    xml_content XML NULL,
    text_content NVARCHAR(MAX) NULL,
    enabled BIT NULL,
    status NVARCHAR(MAX) NULL,
    object_type NVARCHAR(50) NULL);

INSERT INTO @database
    (database_name, is_online)
SELECT
    databases.name,
    CASE WHEN databases.state = 0 THEN 1 ELSE 0 END AS is_online
FROM sys.databases;

-- Jobs
INSERT INTO #object_data
    (server_name, objectname, object_description, enabled, object_type)
SELECT
    sysservers.srvname AS server_name,
    sysjobs.name AS objectname,
    sysjobs.description AS object_description,
    sysjobs.enabled,
    'SQL Agent Job' AS object_type
FROM msdb.dbo.sysjobs
INNER JOIN master.dbo.sysservers
ON srvid = originating_server_id
WHERE sysjobs.name LIKE @search_string
OR sysjobs.description LIKE @search_string;

-- Job Steps
INSERT INTO #object_data
    (server_name, objectname, step_name, object_description, object_definition, enabled, object_type)
SELECT
    s.srvname AS server_name,
    sysjobs.name AS objectname,
    sysjobsteps.step_name,
    sysjobs.description AS object_description,
    sysjobsteps.command AS object_definition,
    sysjobs.enabled,
    'SQL Agent Job Step'
FROM msdb.dbo.sysjobs
INNER JOIN msdb.dbo.sysjobsteps
ON sysjobsteps.job_id = sysjobs.job_id
INNER JOIN master.dbo.sysservers s
ON s.srvid = sysjobs.originating_server_id
WHERE sysjobsteps.command LIKE @search_string
OR sysjobsteps.step_name LIKE @search_string;


-- Databases
INSERT INTO #object_data
    ( objectname, object_type)
SELECT
    databases.name AS objectname,
    'Database' AS object_type
FROM sys.databases
WHERE databases.name LIKE @search_string;

-- Logins
INSERT INTO #object_data
    (objectname, object_type)
SELECT
    syslogins.name AS objectname,
    'Server Login' AS object_type
FROM sys.syslogins
WHERE syslogins.name LIKE @search_string;

-- Linked Servers
INSERT INTO #object_data
    (objectname, object_definition, object_type)
SELECT
    servers.name AS objectname,
    servers.data_source AS object_definition,
    'Linked Server' AS object_type
FROM sys.servers
WHERE servers.name LIKE @search_string
OR servers.data_source LIKE @search_string;

-- Server Triggers
INSERT INTO #object_data
    (objectname, object_description, object_definition, object_type)
SELECT
    server_triggers.name AS objectname,
    parent_class_desc AS object_description,
    server_sql_modules.definition AS object_definition,
    'Server Trigger' AS object_type
FROM sys.server_triggers
INNER JOIN sys.server_sql_modules
ON server_triggers.object_id = server_sql_modules.object_id
WHERE server_triggers.name LIKE @search_string
OR server_sql_modules.definition LIKE @search_string;

-- Reporting Services
IF EXISTS (SELECT * FROM sys.databases WHERE databases.name = 'ReportServer')
AND @search_SSRS = 1
BEGIN
    INSERT INTO #object_data
        (objectname, object_definition, xml_content, text_content, object_type)
    SELECT
        Catalog.Name AS objectname,
        Catalog.Path_ AS object_definition,
        CONVERT(XML, CONVERT(VARBINARY(MAX), Catalog.content)) AS xml_content,
        CONVERT(NVARCHAR(MAX), CONVERT(XML, CONVERT(VARBINARY(MAX), Catalog.content))) AS text_content,
        CASE Catalog.Type
            WHEN 1 THEN 'SSRS Folder'
            WHEN 2 THEN 'SSRS Report'
            WHEN 3 THEN 'SSRS Resource'
            WHEN 4 THEN 'SSRS Linked Report'
            WHEN 5 THEN 'SSRS Data Source'
            WHEN 6 THEN 'SSRS Report Model'
            WHEN 7 THEN 'SSRS Report Part'
            WHEN 8 THEN 'SSRS Shared Dataset'
            ELSE 'SSRS Unknown'
        END AS object_type
    FROM reportserver.dbo.Catalog
    LEFT JOIN ReportServer.dbo.Subscriptions
    ON Subscriptions.Report_OID = Catalog.ItemID
    WHERE Catalog.Path_ LIKE @search_string
    OR Catalog.Name LIKE @search_string
    OR CONVERT(NVARCHAR(MAX), CONVERT(XML, CONVERT(VARBINARY(MAX), Catalog.content))) LIKE @search_string
    OR Subscriptions.DataSettings LIKE @search_string
    OR Subscriptions.ExtensionSettings LIKE @search_string
    OR Subscriptions.Description LIKE @search_string;
    
    INSERT INTO #object_data
        (object_description, object_definition, text_content, status, object_type)
    SELECT
        Subscriptions.Description AS object_description,
        Subscriptions.ExtensionSettings AS object_definition,
        Subscriptions.DeliveryExtension AS text_content,
        Subscriptions.LastStatus AS status,
        'SSRS Subscription' AS object_type
    FROM ReportServer.dbo.Subscriptions
    WHERE Subscriptions.ExtensionSettings LIKE @search_string
    OR Subscriptions.Description LIKE @search_string
    OR Subscriptions.DataSettings LIKE @search_string;
END

IF @search_SSIS_MSDB = 1
BEGIN
    WITH CTE_SSIS AS (
    SELECT
        pf.foldername + '\'+ p.name AS full_path,
        CONVERT(XML,CONVERT(VARBINARY(MAX),packagedata)) AS package_details_XML,
        CONVERT(NVARCHAR(max), CONVERT(XML,CONVERT(VARBINARY(MAX),packagedata))) AS package_details_text,
        'SSIS Package (MSDB)' AS object_type
    FROM msdb.dbo.sysssispackages p
    INNER JOIN msdb.dbo.sysssispackagefolders pf
    ON p.folderid = pf.folderid)
    INSERT INTO #object_data
        (object_description, xml_content, text_content, object_type)
    SELECT
        full_path AS object_description,
        package_details_XML AS xml_content,
        package_details_text AS text_content,
        object_type
    FROM CTE_SSIS
    WHERE CTE_SSIS.package_details_text LIKE @search_string
    OR CTE_SSIS.full_path LIKE @search_string;
END
/*
IF @search_SSIS_disk = 1
BEGIN
    CREATE TABLE ##ssis_data
      (  full_path              NVARCHAR(MAX),
         package_details_XML  XML,
         package_details_text NVARCHAR(MAX) );

    DECLARE @ssis_package_name NVARCHAR(MAX);

    SELECT @sql_command = 'dir "' + @pkg_directory + '" /A-D /B /S ';

    INSERT INTO ##ssis_data
        (full_path)
    EXEC Xp_cmdshell @sql_command;

    DECLARE SSIS_CURSOR CURSOR FOR
    SELECT full_path FROM ##ssis_data;

    OPEN SSIS_CURSOR;

    FETCH NEXT FROM SSIS_CURSOR INTO @ssis_package_name;

    WHILE @@FETCH_STATUS = 0
      BEGIN
          SELECT @sql_command = 'WITH CTE_SSIS_PACKAGES AS (
                            SELECT
                                ''' + @ssis_package_name + ''' AS full_path,
                                CONVERT(XML, SSIS_PACKAGE.bulkcolumn) AS package_details_XML
                            FROM OPENROWSET(BULK ''' + @ssis_package_name + ''', single_blob) AS SSIS_PACKAGE)
                          UPDATE SSIS_DATA 
                                SET package_details_XML = CTE_SSIS_PACKAGES.package_details_XML
                          FROM CTE_SSIS_PACKAGES
                          INNER JOIN ##ssis_data SSIS_DATA
                          ON CTE_SSIS_PACKAGES.full_path = SSIS_DATA.full_path;'
          FROM ##ssis_data;

          EXEC (@sql_command);
          FETCH NEXT FROM SSIS_CURSOR INTO @ssis_package_name;
      END

    CLOSE SSIS_CURSOR;
    DEALLOCATE SSIS_CURSOR;

    UPDATE SSIS_DATA
    SET    package_details_text = CONVERT(NVARCHAR(MAX), SSIS_DATA.package_details_XML)
    FROM   ##ssis_data SSIS_DATA;

    INSERT INTO #object_data
        (object_description, xml_content, text_content, object_type)
    SELECT
        full_path AS object_description,
        package_details_XML AS xml_content,
        package_details_text AS text_content,
        'SSIS Package (File System)' AS object_type
    FROM ##ssis_data SSIS_DATA
    WHERE SSIS_DATA.package_details_text LIKE @search_string
    OR SSIS_DATA.full_path LIKE @search_string;

    DROP TABLE ##ssis_data;
END;


-- Iterate through databases to retrieve database object metadata
DECLARE DBCURSOR CURSOR FOR
SELECT database_name FROM @database WHERE is_online = 1;
OPEN DBCURSOR;
FETCH NEXT FROM DBCURSOR INTO @database_name;

WHILE @@FETCH_STATUS = 0
BEGIN
    SELECT @sql_command = '
    USE [' + @database_name + '];
    -- Tables
    INSERT INTO #object_data
        (database_name, schema_name, table_name, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        tables.name AS table_name,
        ''Table'' AS object_type
    FROM sys.tables
    INNER JOIN sys.schemas
    ON schemas.schema_id = tables.schema_id
    WHERE tables.name LIKE ''' + @search_string + ''';
    -- Columns
    INSERT INTO #object_data
        (database_name, schema_name, table_name, column_name, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        tables.name AS table_name,
        columns.name AS column_name,
        ''Column'' AS object_type
    FROM sys.tables
    INNER JOIN sys.columns
    ON tables.object_id = columns.object_id
    INNER JOIN sys.schemas
    ON schemas.schema_id = tables.schema_id
    WHERE columns.name LIKE ''' + @search_string + ''';
    -- Schemas
    INSERT INTO #object_data
        (database_name, schema_name, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        ''Schema'' AS object_type
    FROM sys.schemas
    WHERE schemas.name LIKE ''' + @search_string + ''';
    -- Synonyms
    INSERT INTO #object_data
        (database_name, objectname, object_definition, object_type)
    SELECT
        db_name() AS database_name,
        synonyms.name AS objectname,
        synonyms.base_object_name AS object_definition,
        ''Synonym'' AS object_type
    FROM sys.synonyms
    WHERE synonyms.name LIKE ''' + @search_string + '''
    OR synonyms.base_object_name LIKE ''' + @search_string + ''';
    -- Indexes
    INSERT INTO #object_data
        (database_name, table_name, objectname, object_type)
    SELECT
        db_name() AS database_name,
        tables.name AS table_name,
        indexes.name AS objectname,
        ''Index'' AS object_type
    FROM sys.indexes
    INNER JOIN sys.tables
    ON tables.object_id = indexes.object_id
    WHERE indexes.name LIKE ''' + @search_string + ''';
    -- Index Columns
    WITH CTE_INDEX_COLUMNS AS (
        SELECT
            db_name() AS database_name,
            TABLE_DATA.name AS table_name,
            INDEX_DATA.name AS index_name,
            STUFF(( SELECT '', '' + columns.name
                    FROM sys.tables
                    INNER JOIN sys.indexes
                    ON tables.object_id = indexes.object_id
                    INNER JOIN sys.index_columns
                    ON indexes.object_id = index_columns.object_id
                    AND indexes.index_id = index_columns.index_id
                    INNER JOIN sys.columns
                    ON tables.object_id = columns.object_id
                    AND index_columns.column_id = columns.column_id
                    WHERE INDEX_DATA.object_id = indexes.object_id
                    AND INDEX_DATA.index_id = indexes.index_id
                    AND index_columns.is_included_column = 0
                    ORDER BY index_columns.key_ordinal
                FOR XML PATH('''')), 1, 2, '''') AS key_column_list,
                STUFF(( SELECT '', '' + columns.name
                    FROM sys.tables
                    INNER JOIN sys.indexes
                    ON tables.object_id = indexes.object_id
                    INNER JOIN sys.index_columns
                    ON indexes.object_id = index_columns.object_id
                    AND indexes.index_id = index_columns.index_id
                    INNER JOIN sys.columns
                    ON tables.object_id = columns.object_id
                    AND index_columns.column_id = columns.column_id
                    WHERE INDEX_DATA.object_id = indexes.object_id
                    AND INDEX_DATA.index_id = indexes.index_id
                    AND index_columns.is_included_column = 1
                    ORDER BY index_columns.key_ordinal
                FOR XML PATH('''')), 1, 2, '''') AS include_column_list,
                ''Index Column'' AS object_type
        FROM sys.indexes INDEX_DATA
        INNER JOIN sys.tables TABLE_DATA
        ON TABLE_DATA.object_id = INDEX_DATA.object_id)
    INSERT INTO #object_data
        (database_name, table_name, objectname, key_column_list, include_column_list, object_type)
    SELECT
        database_name,
        table_name,
        index_name,
        key_column_list,
        ISNULL(include_column_list, '''') AS include_column_list,
        object_type
    FROM CTE_INDEX_COLUMNS
    WHERE CTE_INDEX_COLUMNS.key_column_list LIKE ''' + @search_string + '''
    OR CTE_INDEX_COLUMNS.include_column_list LIKE ''' + @search_string + ''';
    -- Service Broker Queues
    INSERT INTO #object_data
        (database_name, objectname, object_type)
    SELECT
        db_name() AS database_name,
        name AS objectname,
        ''Queue'' AS object_type
    FROM sys.service_queues
    WHERE service_queues.name LIKE ''' + @search_string + ''';
    -- Foreign Keys
    INSERT INTO #object_data
        (database_name, schema_name, table_name, objectname, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        objects.name AS table_name,
        foreign_keys.name AS objectname,
        ''Foreign Key'' AS object_type
    FROM sys.foreign_keys
    INNER JOIN sys.schemas
    ON foreign_keys.schema_id = schemas.schema_id
    INNER JOIN sys.objects
    ON objects.object_id = foreign_keys.parent_object_id
    WHERE foreign_keys.name LIKE ''' + @search_string + ''';
    -- Foreign Key Columns
    WITH CTE_FOREIGN_KEY_COLUMNS AS (
        SELECT
            parent_schema.name AS parent_schema,
            parent_table.name AS parent_table,
            referenced_schema.name AS referenced_schema,
            referenced_table.name AS referenced_table,
            foreign_keys.name AS foreign_key_name,
            STUFF(( SELECT '', '' + referencing_column.name
                    FROM sys.foreign_key_columns
                    INNER JOIN sys.objects
                    ON objects.object_id = foreign_key_columns.constraint_object_id
                    INNER JOIN sys.tables parent_table
                    ON foreign_key_columns.parent_object_id = parent_table.object_id
                    INNER JOIN sys.schemas parent_schema
                    ON parent_schema.schema_id = parent_table.schema_id
                    INNER JOIN sys.columns referencing_column
                    ON foreign_key_columns.parent_object_id = referencing_column.object_id 
                    AND foreign_key_columns.parent_column_id = referencing_column.column_id
                    INNER JOIN sys.columns referenced_column
                    ON foreign_key_columns.referenced_object_id = referenced_column.object_id
                    AND foreign_key_columns.referenced_column_id = referenced_column.column_id
                    INNER JOIN sys.tables referenced_table
                    ON referenced_table.object_id = foreign_key_columns.referenced_object_id
                    INNER JOIN sys.schemas referenced_schema
                    ON referenced_schema.schema_id = referenced_table.schema_id
                    WHERE objects.object_id = foreign_keys.object_id
                    ORDER BY foreign_key_columns.constraint_column_id ASC
                FOR XML PATH('''')), 1, 2, '''') AS foreign_key_column_list,
            STUFF(( SELECT '', '' + referenced_column.name
                    FROM sys.foreign_key_columns
                    INNER JOIN sys.objects
                    ON objects.object_id = foreign_key_columns.constraint_object_id
                    INNER JOIN sys.tables parent_table
                    ON foreign_key_columns.parent_object_id = parent_table.object_id
                    INNER JOIN sys.schemas parent_schema
                    ON parent_schema.schema_id = parent_table.schema_id
                    INNER JOIN sys.columns referencing_column
                    ON foreign_key_columns.parent_object_id = referencing_column.object_id 
                    AND foreign_key_columns.parent_column_id = referencing_column.column_id
                    INNER JOIN sys.columns referenced_column
                    ON foreign_key_columns.referenced_object_id = referenced_column.object_id
                    AND foreign_key_columns.referenced_column_id = referenced_column.column_id
                    INNER JOIN sys.tables referenced_table
                    ON referenced_table.object_id = foreign_key_columns.referenced_object_id
                    INNER JOIN sys.schemas referenced_schema
                    ON referenced_schema.schema_id = referenced_table.schema_id
                    WHERE objects.object_id = foreign_keys.object_id
                    ORDER BY foreign_key_columns.constraint_column_id ASC
                FOR XML PATH('''')), 1, 2, '''') AS referenced_column_list,
                ''Foreign Key Column'' AS object_type
        FROM sys.foreign_keys
        INNER JOIN sys.tables parent_table
        ON foreign_keys.parent_object_id = parent_table.object_id
        INNER JOIN sys.schemas parent_schema
        ON parent_schema.schema_id = parent_table.schema_id
        INNER JOIN sys.tables referenced_table
        ON referenced_table.object_id = foreign_keys.referenced_object_id
        INNER JOIN sys.schemas referenced_schema
        ON referenced_schema.schema_id = referenced_table.schema_id)
    INSERT INTO #object_data
        (database_name, schema_name, table_name, objectname, key_column_list, object_type)
    SELECT
        db_name() AS database_name,
        parent_schema + '' --&gt; '' + referenced_schema,
        parent_table + '' --&gt; '' + referenced_table,
        foreign_key_name AS objectname,
        foreign_key_column_list + '' --&gt; '' + referenced_column_list AS key_column_list,
        object_type
    FROM CTE_FOREIGN_KEY_COLUMNS
    WHERE CTE_FOREIGN_KEY_COLUMNS.foreign_key_column_list LIKE ''' + @search_string + '''
    OR CTE_FOREIGN_KEY_COLUMNS.referenced_column_list LIKE ''' + @search_string + ''';
    -- Default Constraints
    INSERT INTO #object_data
        (database_name, schema_name, table_name, column_name, objectname, object_definition, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        objects.name AS table_name,
        columns.name AS column_name,
        default_constraints.name AS objectname,
        default_constraints.definition AS object_definition,
        ''Default Constraint'' AS object_type
    FROM sys.default_constraints
    INNER JOIN sys.objects
    ON objects.object_id = default_constraints.parent_object_id
    INNER JOIN sys.schemas
    ON objects.schema_id = schemas.schema_id
    INNER JOIN sys.columns
    ON columns.object_id = objects.object_id
    AND columns.column_id = default_constraints.parent_column_id
    WHERE default_constraints.name LIKE ''' + @search_string + '''
    OR default_constraints.definition LIKE ''' + @search_string + ''';
    -- Check Constraints
    INSERT INTO #object_data
        (database_name, schema_name, table_name, objectname, object_definition, object_type)
    SELECT
        db_name() AS database_name,
        schemas.name AS schema_name,
        objects.name AS table_name,
        check_constraints.name AS objectname,
        check_constraints.definition AS object_definition,
        ''Check Constraint'' AS object_type
    FROM sys.check_constraints
    INNER JOIN sys.objects
    ON objects.object_id = check_constraints.parent_object_id
    INNER JOIN sys.schemas
    ON objects.schema_id = schemas.schema_id
    WHERE check_constraints.name LIKE ''' + @search_string + '''
    OR check_constraints.definition LIKE ''' + @search_string + ''';
    -- Database DDL Triggers
    INSERT INTO #object_data
        (database_name, objectname, object_description, object_definition, object_type)
    SELECT
        db_name() AS database_name,
        triggers.name AS objectname,
        triggers.parent_class_desc AS object_description,
        sql_modules.definition AS object_definition,
        ''Database DDL Trigger'' AS object_type
    FROM sys.triggers
    INNER JOIN sys.sql_modules
    ON triggers.object_id = sys.sql_modules.object_id
    WHERE parent_class_desc = ''DATABASE''
    AND (triggers.name LIKE ''' + @search_string + ''' OR sql_modules.definition LIKE ''' + @search_string + ''');
    -- P (stored proc), RF (replication-filter-procedure), V (view), TR (DML trigger), FN (scalar function), IF (inline table-valued function), TF (SQL table-valued function), and R (rule)
    INSERT INTO #object_data
        (database_name, schema_name, table_name, objectname, object_definition, object_type)
    SELECT
        db_name() AS database_name,
        parent_schema.name AS schema_name,
        parent_object.name AS table_name,
        child_object.name AS objectname,
        sql_modules.definition AS object_definition,
        CASE child_object.type 
            WHEN ''P'' THEN ''Stored Procedure''
            WHEN ''RF'' THEN ''Replication Filter Procedure''
            WHEN ''V'' THEN ''View''
            WHEN ''TR'' THEN ''DML Trigger''
            WHEN ''FN'' THEN ''Scalar Function''
            WHEN ''IF'' THEN ''Inline Table Valued Function''
            WHEN ''TF'' THEN ''SQL Table Valued Function''
            WHEN ''R'' THEN ''Rule''
        END    AS object_type
    FROM sys.sql_modules
    INNER JOIN sys.objects child_object
    ON sql_modules.object_id = child_object.object_id
    LEFT JOIN sys.objects parent_object
    ON parent_object.object_id = child_object.parent_object_id
    LEFT JOIN sys.schemas parent_schema
    ON parent_object.schema_id = parent_schema.schema_id
    WHERE child_object.name LIKE ''' + @search_string + '''
    OR sql_modules.definition LIKE ''' + @search_string + ''';

    EXEC sp_executesql @sql_command;

    FETCH NEXT FROM DBCURSOR INTO @database_name;
END

CLOSE DBCURSOR;
DEALLOCATE DBCURSOR;

SELECT
    object_type,
    server_name,
    database_name,
    schema_name,
    table_name,
    column_name,
    objectname,
    step_name,
    object_description,
    object_definition,
    key_column_list,
    include_column_list,
    xml_content,
    text_content,
    enabled BIT,
    status,
    object_type
FROM #object_data;

DROP TABLE #object_data;
GO
-- */
GO
/*
 * Query that simulates running sp_spaceused on every applicable object in a database and gathering it all into a single result set
 * This set-based approach is more efficient then actually doing that.
 * The logic is derived strait from the source of sp_spaceused, so the numerical values should be a 1-to-1 match.
 * Three changes have been made to the result set:
 *     (1) The object's schema and type are included.
 *     (2) Actual numbers are used in the result instead of strings with ' KB' appended to the end.
 *     (3) The reserved, data, index_size, and unused columns are renamed with a postfix of '_kb'.
 * Compatibility: 2005+
 * Released by Greg Drake on 2013-06-03
 */


SELECT
	'schema'         = schema_name(so.schema_id)
	,'name'          = so.name
	,'type'          = so.type
	,'type_desc'     = so.type_desc
	,'rows'          = partition_stats.row_count
	,'reserved_kb'   = (calc.reserved_page_count * 8)
	,'data_kb'       = (partition_stats.page_count * 8)
	,'index_size_kb' = (CASE WHEN (calc.used_page_count > partition_stats.page_count) THEN (calc.used_page_count - partition_stats.page_count) ELSE 0 END * 8)
	,'unused_kb'     = (CASE WHEN calc.reserved_page_count > calc.used_page_count THEN (calc.reserved_page_count - calc.used_page_count) ELSE 0 END * 8)
FROM
	sys.objects so
	INNER JOIN
	(
		SELECT
			'object_id'            = sddps.[object_id]
			,'row_count'           = sum(
				CASE
					WHEN (sddps.index_id < 2) THEN sddps.row_count
					ELSE 0
				END
			)
			,'page_count'          = sum(
				CASE
					WHEN (sddps.index_id < 2) THEN (sddps.in_row_data_page_count + sddps.lob_used_page_count + sddps.row_overflow_used_page_count)
					ELSE sddps.lob_used_page_count + sddps.row_overflow_used_page_count
				END
			)
			,'used_page_count'     = sum(sddps.used_page_count)
			,'reserved_page_count' = sum(sddps.reserved_page_count)
		FROM
			sys.dm_db_partition_stats sddps
		GROUP BY
			sddps.[object_id]
	) partition_stats ON (so.[object_id] = partition_stats.[object_id])
	LEFT OUTER JOIN
	(
		SELECT
			sit.parent_object_id
			,'used_page_count'     = sum(sddps2.used_page_count)
			,'reserved_page_count' = sum(sddps2.reserved_page_count)
		FROM
			sys.internal_tables sit
			INNER JOIN
			sys.dm_db_partition_stats sddps2 ON (sit.[object_id] = sddps2.[object_id])
		WHERE
			sit.internal_type IN (202,204,211,212,213,214,215,216)
		GROUP BY
			sit.parent_object_id
	) summary_data ON (so.[object_id] = summary_data.parent_object_id)
	CROSS APPLY
	(
		SELECT
			'reserved_page_count'  = (partition_stats.reserved_page_count + isnull(summary_data.reserved_page_count, 0))
			,'used_page_count'     = (partition_stats.used_page_count + isnull(summary_data.used_page_count, 0))
	) calc
WHERE
	so.[type] IN ('U ','V ','S ','SQ','IT')
ORDER BY
	schema_name(so.schema_id)
	,so.name;

GO
/*
Author: Greg Drake
Original link: http://www.sqlservercentral.com/scripts/99716/
*/
 /*
 * Query that simulates running sp_spaceused on every applicable object in a database and gathering it all into a single result set
 * This set-based approach is more efficient then actually doing that.
 * The logic is derived strait from the source of sp_spaceused, so the numerical values should be a 1-to-1 match.
 * Three changes have been made to the result set:
 *     (1) The object's schema and type are included.
 *     (2) Actual numbers are used in the result instead of strings with ' KB' appended to the end.
 *     (3) The reserved, data, index_size, and unused columns are renamed with a postfix of '_kb'.
 * Compatibility: 2005+
 * Released by Greg Drake on 2013-06-03
 */


SELECT 'schema'         = schema_name(so.schema_id)
     , 'name'          = so.name
     , 'type'          = so.type
     , 'type_desc'     = so.type_desc
     , 'rows'          = partition_stats.row_count
     , 'reserved_kb'   = (calc.reserved_page_count * 8)
     , 'data_kb'       = (partition_stats.page_count * 8)
     , 'index_size_kb' = (CASE WHEN (calc.used_page_count > partition_stats.page_count) THEN (calc.used_page_count - partition_stats.page_count) ELSE 0 END * 8)
     , 'unused_kb'     = (CASE WHEN calc.reserved_page_count > calc.used_page_count THEN (calc.reserved_page_count - calc.used_page_count) ELSE 0 END * 8)
FROM
    sys.objects so
    INNER JOIN
    (
        SELECT
            'object_id'            = sddps.[object_id]
            ,'row_count'           = sum(
                CASE
                    WHEN (sddps.index_id < 2) THEN sddps.row_count
                    ELSE 0
                END
            )
            ,'page_count'          = sum(
                CASE
                    WHEN (sddps.index_id < 2) THEN (sddps.in_row_data_page_count + sddps.lob_used_page_count + sddps.row_overflow_used_page_count)
                    ELSE sddps.lob_used_page_count + sddps.row_overflow_used_page_count
                END
            )
            ,'used_page_count'     = sum(sddps.used_page_count)
            ,'reserved_page_count' = sum(sddps.reserved_page_count)
        FROM
            sys.dm_db_partition_stats sddps
        GROUP BY
            sddps.[object_id]
    ) partition_stats ON (so.[object_id] = partition_stats.[object_id])
    LEFT OUTER JOIN
    (
        SELECT
            sit.parent_object_id
            ,'used_page_count'     = sum(sddps2.used_page_count)
            ,'reserved_page_count' = sum(sddps2.reserved_page_count)
        FROM
            sys.internal_tables sit
            INNER JOIN
            sys.dm_db_partition_stats sddps2 ON (sit.[object_id] = sddps2.[object_id])
        WHERE
            sit.internal_type IN (202,204,211,212,213,214,215,216)
        GROUP BY
            sit.parent_object_id
    ) summary_data ON (so.[object_id] = summary_data.parent_object_id)
    CROSS APPLY
    (
        SELECT
            'reserved_page_count'  = (partition_stats.reserved_page_count + isnull(summary_data.reserved_page_count, 0))
            ,'used_page_count'     = (partition_stats.used_page_count + isnull(summary_data.used_page_count, 0))
    ) calc
WHERE
    so.[type] IN ('U ','V ','S ','SQ','IT')
ORDER BY schema_name(so.schema_id)
       , so.name;

GO
--SQL Server Fixed Role Permissions
--Author: Jason Brimhall
--In this script, I have taken the results from each of the stored procedures and dumped them into a temp table.
--Using this temp table, I can now join to this table to get a more complete list of the permissions in effect for various principals.

CREATE TABLE #role_permission
	(
		DBFixedRole VARCHAR(128)
		, Permission VARCHAR(128)
	)
INSERT INTO #role_permission
		( DBFixedRole, Permission )
		EXECUTE sp_dbfixedrolepermission
GO
 
INSERT INTO #role_permission
		( DBFixedRole, Permission )
		EXECUTE sp_srvrolepermission
GO
 
SELECT *
	FROM #role_permission r;
 
DROP TABLE #role_permission;
GO
/*
Author: Allen McGuire
Source link: http://allen-mcguire.blogspot.ru/2017/08/monitor-and-stop-long-running-sql.html
*/

DECLARE @Duration_secs INT;
DECLARE @JobName SYSNAME;

DECLARE @LongRunningJobs TABLE(
      DurationSecs  INT     NOT NULL
    , JobName       SYSNAME NOT NULL
    , TSQLStatement NVARCHAR(300) NOT NULL
);

INSERT INTO @LongRunningJobs(DurationSecs, JobName, TSQLStatement)
SELECT DATEDIFF(ss, ja.start_execution_date, GETDATE()) AS Duration_secs
     , jobs.name AS JobName
     , 'EXEC msdb.dbo.sp_stop_job N''' + jobs.name + '''' AS TSQLStatement
FROM   msdb.dbo.sysjobs jobs
       LEFT JOIN msdb.dbo.sysjobactivity ja ON ja.job_id = jobs.job_id
            AND ja.start_execution_date IS NOT NULL
WHERE  -- jobs.name = 'Distribution clean up: distribution'
       -- AND
       stop_execution_date IS NULL;

SELECT * FROM @LongRunningJobs;

/*
USE [msdb]
GO
DECLARE @jobId BINARY(16)
EXEC  msdb.dbo.sp_add_job @job_name=N'Test', 
        @enabled=1, 
        @notify_level_eventlog=0, 
        @notify_level_email=2, 
        @notify_level_page=2, 
        @delete_level=0, 
        @category_name=N'[Uncategorized (Local)]', 
        @owner_login_name=N'sa', @job_id = @jobId OUTPUT
select @jobId
GO
EXEC msdb.dbo.sp_add_jobserver @job_name=N'Test', @server_name = @@SERVERNAME;
GO
USE [msdb]
GO
EXEC msdb.dbo.sp_add_jobstep @job_name=N'Test', @step_name=N'wait_20_seconds', 
        @step_id=1, 
        @cmdexec_success_code=0, 
        @on_success_action=1, 
        @on_fail_action=2, 
        @retry_attempts=0, 
        @retry_interval=0, 
        @os_run_priority=0, @subsystem=N'TSQL', 
        @command=N'WAITFOR DELAY ''00:00:59'';', 
        @database_name=N'master', 
        @flags=0
GO

DECLARE @schedule_id int
EXEC msdb.dbo.sp_add_jobschedule @job_id=N'f0cc44dc-4718-4b66-8d47-bd6f86c9c513', @name=N'Test_schedule_every_minute', 
        @enabled=1, 
        @freq_type=4, 
        @freq_interval=1, 
        @freq_subday_type=4, 
        @freq_subday_interval=1, 
        @freq_relative_interval=0, 
        @freq_recurrence_factor=1, 
        @active_start_date=20170904, 
        @active_end_date=99991231, 
        @active_start_time=0, 
        @active_end_time=235959, @schedule_id = @schedule_id OUTPUT
SELECT @schedule_id;
GO

EXEC msdb.dbo.sp_delete_job @job_name = N'Test';
*/
GO
/*
Author: Ben Snaidero
Original link: https://www.mssqltips.com/sqlservertip/4166/automate-alerting-for-sql-server-suspect-database-pages/
*/

SELECT sp.database_id AS DatabaseID
  , d.name            AS DatabaseName
  , sp.file_id        AS FileID
  , mf.physical_name  AS FileName
  , sp.page_id        AS PageID
  , CASE
        WHEN sp.event_type = 1
        THEN '823 or 824 error other than a bad checksum or a torn page'
        WHEN sp.event_type = 2
        THEN 'Bad checksum'
        WHEN sp.event_type = 3
        THEN 'Torn Page'
        WHEN sp.event_type = 4
        THEN 'Restored (The page was restored after it was marked bad)'
        WHEN sp.event_type = 5
        THEN 'Repaired (DBCC repaired the page)'
        WHEN sp.event_type = 7
        THEN 'Deallocated by DBCC'
    END                     AS EventDesc
  , sp.error_count          AS ErrorCount
  , sp.last_update_date     AS LastUpdated
FROM msdb.dbo.suspect_pages AS sp
INNER JOIN sys.databases    AS d  ON d.database_id  = sp.database_id
INNER JOIN sys.master_files AS mf ON mf.database_id = sp.database_id AND mf.file_id = sp.file_id;

GO
/*
Author: SSMS
*/

exec sp_executesql N'
        CREATE TABLE #tmp_extended_remote_data_archive_tables
        (object_id int not null, remote_table_name nvarchar(128) null, filter_predicate nvarchar(max) null, migration_state tinyint null)

        IF EXISTS(SELECT 1 FROM master.sys.syscolumns WHERE Name = N''remote_data_archive_migration_state'' AND ID = Object_ID(N''sys.tables''))
          EXECUTE(N''INSERT INTO #tmp_extended_remote_data_archive_tables SELECT rdat.object_id, rdat.remote_table_name, 
            SUBSTRING(rdat.filter_predicate, 2, LEN(rdat.filter_predicate) - 2) as filter_predicate, 
            CASE
            WHEN tbl.remote_data_archive_migration_state_desc = N''''PAUSED'''' THEN 1
            WHEN tbl.remote_data_archive_migration_state_desc = N''''OUTBOUND'''' THEN 3
            WHEN tbl.remote_data_archive_migration_state_desc = N''''INBOUND'''' THEN 4
            WHEN tbl.remote_data_archive_migration_state_desc = N''''DISABLED'''' THEN 0
            ELSE 0
            END AS migration_state
            FROM sys.tables tbl LEFT JOIN sys.remote_data_archive_tables rdat ON rdat.object_id = tbl.object_id
            WHERE rdat.object_id IS NOT NULL'')
        ELSE
          EXECUTE(N''INSERT INTO #tmp_extended_remote_data_archive_tables SELECT rdat.object_id, rdat.remote_table_name, 
            SUBSTRING(rdat.filter_predicate, 2, LEN(rdat.filter_predicate) - 2) as filter_predicate, 
            CASE
            WHEN rdat.is_migration_paused = 1 AND rdat.migration_direction_desc = N''''OUTBOUND'''' THEN 1
            WHEN rdat.is_migration_paused = 1 AND rdat.migration_direction_desc = N''''INBOUND'''' THEN 2
            WHEN rdat.is_migration_paused = 0 AND rdat.migration_direction_desc = N''''OUTBOUND'''' THEN 3
            WHEN rdat.is_migration_paused = 0 AND rdat.migration_direction_desc = N''''INBOUND'''' THEN 4
            ELSE 0
            END AS migration_state
            FROM sys.tables tbl LEFT JOIN sys.remote_data_archive_tables rdat ON rdat.object_id = tbl.object_id
            WHERE rdat.object_id IS NOT NULL'')
      


SELECT
tbl.name AS [Name],
tbl.object_id AS [ID],
tbl.create_date AS [CreateDate],
tbl.modify_date AS [DateLastModified],
ISNULL(stbl.name, N'''') AS [Owner],
CAST(case when tbl.principal_id is null then 1 else 0 end AS bit) AS [IsSchemaOwned],
SCHEMA_NAME(tbl.schema_id) AS [Schema],
CAST(
 case 
    when tbl.is_ms_shipped = 1 then 1
    when (
        select 
            major_id 
        from 
            sys.extended_properties 
        where 
            major_id = tbl.object_id and 
            minor_id = 0 and 
            class = 1 and 
            name = N''microsoft_database_tools_support'') 
        is not null then 1
    else 0
end          
             AS bit) AS [IsSystemObject],
CAST(OBJECTPROPERTY(tbl.object_id, N''HasAfterTrigger'') AS bit) AS [HasAfterTrigger],
CAST(OBJECTPROPERTY(tbl.object_id, N''HasInsertTrigger'') AS bit) AS [HasInsertTrigger],
CAST(OBJECTPROPERTY(tbl.object_id, N''HasDeleteTrigger'') AS bit) AS [HasDeleteTrigger],
CAST(OBJECTPROPERTY(tbl.object_id, N''HasInsteadOfTrigger'') AS bit) AS [HasInsteadOfTrigger],
CAST(OBJECTPROPERTY(tbl.object_id, N''HasUpdateTrigger'') AS bit) AS [HasUpdateTrigger],
CAST(OBJECTPROPERTY(tbl.object_id, N''IsIndexed'') AS bit) AS [HasIndex],
CAST(OBJECTPROPERTY(tbl.object_id, N''IsIndexable'') AS bit) AS [IsIndexable],
CAST(CASE idx.index_id WHEN 1 THEN 1 ELSE 0 END AS bit) AS [HasClusteredIndex],
CAST(CASE idx.type WHEN 0 THEN 1 ELSE 0 END AS bit) AS [HasHeapIndex],
tbl.uses_ansi_nulls AS [AnsiNullsStatus],
CAST(ISNULL(OBJECTPROPERTY(tbl.object_id,N''IsQuotedIdentOn''),0) AS bit) AS [QuotedIdentifierStatus],
CAST(0 AS bit) AS [FakeSystemTable],
ISNULL(dstext.name,N'''') AS [TextFileGroup],
CAST(tbl.is_memory_optimized AS bit) AS [IsMemoryOptimized],
case when (tbl.durability=1) then 0 else 1 end AS [Durability],
tbl.is_replicated AS [Replicated],
tbl.lock_escalation AS [LockEscalation],
CAST(case when ctt.object_id is null then 0 else 1  end AS bit) AS [ChangeTrackingEnabled],
CAST(ISNULL(ctt.is_track_columns_updated_on,0) AS bit) AS [TrackColumnsUpdatedEnabled],
tbl.is_filetable AS [IsFileTable],
ISNULL(ft.directory_name,N'''') AS [FileTableDirectoryName],
ISNULL(ft.filename_collation_name,N'''') AS [FileTableNameColumnCollation],
CAST(ISNULL(ft.is_enabled,0) AS bit) AS [FileTableNamespaceEnabled],
CASE WHEN ''PS''=dsidx.type THEN dsidx.name ELSE N'''' END AS [PartitionScheme],
CAST(CASE WHEN ''PS''=dsidx.type THEN 1 ELSE 0 END AS bit) AS [IsPartitioned],
CASE WHEN ''FD''=dstbl.type THEN dstbl.name ELSE N'''' END AS [FileStreamFileGroup],
CASE WHEN ''PS''=dstbl.type THEN dstbl.name ELSE N'''' END AS [FileStreamPartitionScheme],
CAST(CASE idx.type WHEN 5 THEN 1 ELSE 0 END AS bit) AS [HasClusteredColumnStoreIndex],
CAST(CASE tbl.temporal_type WHEN 2 THEN 1 ELSE 0 END AS bit) AS [IsSystemVersioned],
CAST(ISNULL(historyTable.name, N'''') AS sysname) AS [HistoryTableName],
CAST(ISNULL(SCHEMA_NAME(historyTable.schema_id), N'''') AS sysname) AS [HistoryTableSchema],
CAST(ISNULL(historyTable.object_id, 0) AS int) AS [HistoryTableID],
CAST(CASE WHEN periods.start_column_id IS NULL THEN 0 ELSE 1 END AS bit) AS [HasSystemTimePeriod],
CAST(
      ISNULL((SELECT cols.name
      FROM sys.columns cols
      WHERE periods.object_id = tbl.object_id
      AND cols.object_id = tbl.object_id
      AND cols.column_id = periods.start_column_id), N'''')
     AS sysname) AS [SystemTimePeriodStartColumn],
CAST(
      ISNULL((SELECT cols.name
      FROM sys.columns cols
      WHERE periods.object_id = tbl.object_id
      AND cols.object_id = tbl.object_id
      AND cols.column_id = periods.end_column_id), N'''')
     AS sysname) AS [SystemTimePeriodEndColumn],
tbl.temporal_type AS [TemporalType],
CAST(tbl.is_remote_data_archive_enabled AS bit) AS [RemoteDataArchiveEnabled],
CAST(
      ISNULL(rdat.migration_state, 0)
     AS tinyint) AS [RemoteDataArchiveDataMigrationState],
CAST(rdat.filter_predicate AS varchar(4000)) AS [RemoteDataArchiveFilterPredicate],
CAST(rdat.remote_table_name AS sysname) AS [RemoteTableName],
CAST(CASE WHEN rdat.remote_table_name IS NULL THEN 0 ELSE 1 END AS bit) AS [RemoteTableProvisioned],
CAST(tbl.is_external AS bit) AS [IsExternal],
eds.name AS [DataSourceName],
ISNULL(eff.name,N'''') AS [FileFormatName],
ISNULL(et.location,N'''') AS [Location],

      CASE et.reject_type
      WHEN ''VALUE''      THEN 0
      WHEN ''PERCENTAGE'' THEN 1
      ELSE -1
      END
     AS [RejectType],
ISNULL(et.reject_value,0) AS [RejectValue],
ISNULL(et.reject_sample_value,-1) AS [RejectSampleValue]
FROM
sys.tables AS tbl
LEFT OUTER JOIN sys.periods as periods ON periods.object_id = tbl.object_id
LEFT OUTER JOIN sys.tables as historyTable ON historyTable.object_id = tbl.history_table_id
LEFT OUTER JOIN sys.database_principals AS stbl ON stbl.principal_id = ISNULL(tbl.principal_id, (OBJECTPROPERTY(tbl.object_id, ''OwnerId'')))
INNER JOIN sys.indexes AS idx ON 
        idx.object_id = tbl.object_id and (idx.index_id < @_msparam_0  or (tbl.is_memory_optimized = 1 and idx.index_id = (select min(index_id) from sys.indexes where object_id = tbl.object_id)))
      
LEFT OUTER JOIN sys.data_spaces AS dstext  ON tbl.lob_data_space_id = dstext.data_space_id
LEFT OUTER JOIN sys.change_tracking_tables AS ctt ON ctt.object_id = tbl.object_id 
LEFT OUTER JOIN sys.filetables AS ft ON ft.object_id = tbl.object_id 
LEFT OUTER JOIN sys.data_spaces AS dsidx ON dsidx.data_space_id = idx.data_space_id
LEFT OUTER JOIN sys.tables AS t ON t.object_id = idx.object_id
LEFT OUTER JOIN sys.data_spaces AS dstbl ON dstbl.data_space_id = t.Filestream_data_space_id and (idx.index_id < 2 or (idx.type = 7 and idx.index_id < 3))
LEFT OUTER JOIN #tmp_extended_remote_data_archive_tables AS rdat ON rdat.object_id = tbl.object_id
LEFT OUTER JOIN sys.external_tables AS et ON et.object_id = tbl.object_id
LEFT OUTER JOIN sys.external_data_sources AS eds ON eds.data_source_id = et.data_source_id
LEFT OUTER JOIN sys.external_file_formats AS eff ON eff.file_format_id = et.file_format_id
WHERE
(tbl.name=@_msparam_1 and SCHEMA_NAME(tbl.schema_id)=@_msparam_2)

        DROP TABLE #tmp_extended_remote_data_archive_tables
      
',N'@_msparam_0 nvarchar(4000),@_msparam_1 nvarchar(4000),@_msparam_2 nvarchar(4000)',@_msparam_0=N'2',@_msparam_1=N'Node',@_msparam_2=N'dictionary';

GO
/*
Author:
Link:
*/

WITH fk_tables AS 
    (SELECT s1.name        AS from_schema
      , o1.name            AS from_table
      , s2.name            AS to_schema
      , o2.name            AS to_table
    FROM sys.foreign_keys     fk
    INNER JOIN sys.objects    o1 ON fk.parent_object_id     = o1.object_id
    INNER JOIN sys.schemas    s1 ON o1.schema_id            = s1.schema_id
    INNER JOIN sys.objects    o2 ON fk.referenced_object_id = o2.object_id
    INNER JOIN sys.schemas    s2 ON o2.schema_id            = s2.schema_id
    INNER JOIN (SELECT object_id, SUM(row_count) AS row_count
              FROM sys.dm_db_partition_stats
              WHERE index_id < 2
              GROUP BY object_id
              ) AS rc ON o1.object_id            = rc.object_id
        /*For the purposes of finding dependency hierarchy
        we're not worried about self-referencing tables*/
    WHERE NOT 
        (s1.name        = s2.name
            AND o1.name = o2.name 
        )
    )
  , ordered_tables AS
    (SELECT s.name AS schemaName
      , t.name     AS tableName
      , 0          AS TableLevel
    FROM(SELECT    *
        FROM sys.tables
        )                     t
    INNER JOIN sys.schemas    s  ON t.schema_id = s.schema_id
    LEFT OUTER JOIN fk_tables fk ON s.name      = fk.from_schema AND t.name = fk.from_table
    WHERE fk.from_schema                  IS NULL
    UNION ALL
    SELECT fk.from_schema
      , fk.from_table
      , ot.TableLevel +       1
    FROM fk_tables            fk
    INNER JOIN ordered_tables ot ON fk.to_schema = ot.schemaName AND fk.to_table = ot.tableName
    )
  , final AS
    (SELECT    DISTINCT ot.schemaName
      , ot.tableName
      , ot.TableLevel
    FROM ordered_tables ot
    INNER JOIN 
        (SELECT schemaName
          , tableName
          , MAX(TableLevel) maxTableLevel
        FROM ordered_tables
        GROUP BY schemaName 
          , tableName
        ) mx ON ot.schemaName = mx.schemaName AND ot.tableName = mx.tableName AND mx.maxTableLevel = ot.TableLevel
    )
SELECT QUOTENAME(final.schemaName) + '.' + QUOTENAME(final.tableName)
     , TableLevel
FROM final
WHERE(
        -- exclude some schemes by IN
        final.schemaName NOT IN('dbo' 
                              , 'consumption')
        -- exclude some tables by IN
        AND final.tableName NOT IN('TP6'
                                 , 'WorldScenarioResult')
        -- exclude some tables by mask
        AND final.tableName NOT LIKE '%Temp'
    )
    -- add some tables by mask and with IN
    OR final.tableName IN('IPAdmin')
    OR final.tableName LIKE 'AspNet%'
    OR final.tableName LIKE 'Application%'
ORDER BY TableLevel;
GO
/*
Author: Paul Randal
Original link: http://www.sqlskills.com/blogs/paul/the-accidental-dba-day-27-of-30-troubleshooting-tempdb-contention/
*/

SELECT
    [owt].[session_id],
    [owt].[exec_context_id],
    [owt].[wait_duration_ms],
    [owt].[wait_type],
    [owt].[blocking_session_id],
    [owt].[resource_description],
    CASE [owt].[wait_type]
        WHEN N'CXPACKET' THEN
            RIGHT ([owt].[resource_description],
            CHARINDEX (N'=', REVERSE ([owt].[resource_description])) - 1)
        ELSE NULL
    END AS [Node ID],
    [es].[program_name],
    [est].text,
    [er].[database_id],
    [eqp].[query_plan],
    [er].[cpu_time]
FROM sys.dm_os_waiting_tasks [owt]
INNER JOIN sys.dm_exec_sessions [es] ON
    [owt].[session_id] = [es].[session_id]
INNER JOIN sys.dm_exec_requests [er] ON
    [es].[session_id] = [er].[session_id]
OUTER APPLY sys.dm_exec_sql_text ([er].[sql_handle]) [est]
OUTER APPLY sys.dm_exec_query_plan ([er].[plan_handle]) [eqp]
WHERE
    [es].[is_user_process] = 1
ORDER BY
    [owt].[session_id],
    [owt].[exec_context_id];
GO
GO
/*
Author: Paul Randal
Original link: http://www.sqlskills.com/blogs/paul/wait-statistics-or-please-tell-me-where-it-hurts/
*/

WITH [Waits] AS
    (SELECT
        [wait_type],
        [wait_time_ms] / 1000.0 AS [WaitS],
        ([wait_time_ms] - [signal_wait_time_ms]) / 1000.0 AS [ResourceS],
        [signal_wait_time_ms] / 1000.0 AS [SignalS],
        [waiting_tasks_count] AS [WaitCount],
        100.0 * [wait_time_ms] / SUM ([wait_time_ms]) OVER() AS [Percentage],
        ROW_NUMBER() OVER(ORDER BY [wait_time_ms] DESC) AS [RowNum]
    FROM sys.dm_os_wait_stats
    WHERE [wait_type] NOT IN (
        N'BROKER_EVENTHANDLER',             N'BROKER_RECEIVE_WAITFOR',
        N'BROKER_TASK_STOP',                N'BROKER_TO_FLUSH',
        N'BROKER_TRANSMITTER',              N'CHECKPOINT_QUEUE',
        N'CHKPT',                           N'CLR_AUTO_EVENT',
        N'CLR_MANUAL_EVENT',                N'CLR_SEMAPHORE',
        N'DBMIRROR_DBM_EVENT',              N'DBMIRROR_EVENTS_QUEUE',
        N'DBMIRROR_WORKER_QUEUE',           N'DBMIRRORING_CMD',
        N'DIRTY_PAGE_POLL',                 N'DISPATCHER_QUEUE_SEMAPHORE',
        N'EXECSYNC',                        N'FSAGENT',
        N'FT_IFTS_SCHEDULER_IDLE_WAIT',     N'FT_IFTSHC_MUTEX',
        N'HADR_CLUSAPI_CALL',               N'HADR_FILESTREAM_IOMGR_IOCOMPLETION',
        N'HADR_LOGCAPTURE_WAIT',            N'HADR_NOTIFICATION_DEQUEUE',
        N'HADR_TIMER_TASK',                 N'HADR_WORK_QUEUE',
        N'KSOURCE_WAKEUP',                  N'LAZYWRITER_SLEEP',
        N'LOGMGR_QUEUE',                    N'ONDEMAND_TASK_QUEUE',
        N'PWAIT_ALL_COMPONENTS_INITIALIZED',
        N'QDS_PERSIST_TASK_MAIN_LOOP_SLEEP',
        N'QDS_SHUTDOWN_QUEUE',
        N'QDS_CLEANUP_STALE_QUERIES_TASK_MAIN_LOOP_SLEEP',
        N'REQUEST_FOR_DEADLOCK_SEARCH',     N'RESOURCE_QUEUE',
        N'SERVER_IDLE_CHECK',               N'SLEEP_BPOOL_FLUSH',
        N'SLEEP_DBSTARTUP',                 N'SLEEP_DCOMSTARTUP',
        N'SLEEP_MASTERDBREADY',             N'SLEEP_MASTERMDREADY',
        N'SLEEP_MASTERUPGRADED',            N'SLEEP_MSDBSTARTUP',
        N'SLEEP_SYSTEMTASK',                N'SLEEP_TASK',
        N'SLEEP_TEMPDBSTARTUP',             N'SNI_HTTP_ACCEPT',
        N'SP_SERVER_DIAGNOSTICS_SLEEP',     N'SQLTRACE_BUFFER_FLUSH',
        N'SQLTRACE_INCREMENTAL_FLUSH_SLEEP',
        N'SQLTRACE_WAIT_ENTRIES',           N'WAIT_FOR_RESULTS',
        N'WAITFOR',                         N'WAITFOR_TASKSHUTDOWN',
        N'WAIT_XTP_HOST_WAIT',              N'WAIT_XTP_OFFLINE_CKPT_NEW_LOG',
        N'WAIT_XTP_CKPT_CLOSE',             N'XE_DISPATCHER_JOIN',
        N'XE_DISPATCHER_WAIT',              N'XE_TIMER_EVENT')
    AND [waiting_tasks_count] > 0
 )
SELECT
    MAX ([W1].[wait_type]) AS [WaitType],
    CAST (MAX ([W1].[WaitS]) AS DECIMAL (16,2)) AS [Wait_S],
    CAST (MAX ([W1].[ResourceS]) AS DECIMAL (16,2)) AS [Resource_S],
    CAST (MAX ([W1].[SignalS]) AS DECIMAL (16,2)) AS [Signal_S],
    MAX ([W1].[WaitCount]) AS [WaitCount],
    CAST (MAX ([W1].[Percentage]) AS DECIMAL (5,2)) AS [Percentage],
    CAST ((MAX ([W1].[WaitS]) / MAX ([W1].[WaitCount])) AS DECIMAL (16,4)) AS [AvgWait_S],
    CAST ((MAX ([W1].[ResourceS]) / MAX ([W1].[WaitCount])) AS DECIMAL (16,4)) AS [AvgRes_S],
    CAST ((MAX ([W1].[SignalS]) / MAX ([W1].[WaitCount])) AS DECIMAL (16,4)) AS [AvgSig_S]
FROM [Waits] AS [W1]
INNER JOIN [Waits] AS [W2]
    ON [W2].[RowNum] <= [W1].[RowNum]
GROUP BY [W1].[RowNum]
HAVING SUM ([W2].[Percentage]) - MAX ([W1].[Percentage]) < 95; -- percentage threshold
GO

GO
/*
Author: Buck Woody
Original link: https://thelonedba.wordpress.com/2016/07/18/which-databases-were-backed-up-in-which-backup-task/
*/


DECLARE @SQL VARCHAR(MAX);
DECLARE @RunStartedList VARCHAR(MAX);
DECLARE @NumDays INT;
SELECT  @NumDays = 10; -- not doing this as a single line declare-define, as we might be running on older versions.
 
IF @NumDays > 0
    BEGIN
        SELECT  @NumDays = @NumDays * ( -1 );
    END;
 
SELECT  @RunStartedList = STUFF(( SELECT    ', ' + QUOTENAME(CONVERT(VARCHAR(20), msdb.dbo.agent_datetime(jh.run_date, jh.run_time), 120))
                                  FROM      msdb.dbo.sysjobhistory jh
                                            LEFT JOIN msdb.dbo.sysjobs j ON j.job_id = jh.job_id
                                  WHERE     LOWER(j.name) LIKE '%backup%'
                                            AND LOWER(j.name) LIKE '%full%'
                                            AND jh.step_id = 0
                                            AND msdb.dbo.agent_datetime(jh.run_date, jh.run_time) >= DATEADD(DAY, @NumDays, GETDATE())
                                  ORDER BY  msdb.dbo.agent_datetime(jh.run_date, jh.run_time) DESC
                                FOR
                                  XML PATH('')
                                ), 1, 2, '');
 
SELECT  @SQL = '
WITH    JobRuns
          AS ( SELECT   jh.instance_id ,
                        msdb.dbo.agent_datetime(jh.run_date, jh.run_time) AS RunStartDateTime ,
                        DATEADD(SECOND, CONVERT(INT, RIGHT(''000000'' + CONVERT(VARCHAR(20), jh.run_duration), 2)),
                                DATEADD(MINUTE, CONVERT(INT, SUBSTRING(RIGHT(''000000'' + CONVERT(VARCHAR(20), jh.run_duration), 6), 3, 2)),
                                        DATEADD(HOUR, CONVERT(INT, LEFT(RIGHT(''000000'' + CONVERT(VARCHAR(20), jh.run_duration), 6), 2)),
                                                DATEADD(DAY, jh.run_duration / 1000000, msdb.dbo.agent_datetime(jh.run_date, jh.run_time))))) AS RunFinishDateTime
               FROM     msdb.dbo.sysjobhistory jh
                        LEFT JOIN msdb.dbo.sysjobs j ON j.job_id = jh.job_id
               WHERE    LOWER(j.name) LIKE ''%backup%''
                        AND LOWER(j.name) LIKE ''%full%''
                        AND jh.step_id = 0
                                         AND msdb.dbo.agent_datetime(jh.run_date, jh.run_time) >= DATEADD(DAY, ' + CONVERT(NVARCHAR(8), @NumDays) + ', GETDATE())
             ),
        BackupsTaken
          AS ( SELECT   backup_start_date ,
                        backup_finish_date ,
                        database_name AS DBName
               FROM     msdb.dbo.backupset
               WHERE    type = ''D''
             ),
              BackupPivot
                AS ( SELECT DBName, ' + @RunStartedList + '
                        FROM (
                                  SELECT  CONVERT(VARCHAR(20), RunStartDateTime, 120) AS RunStarted,
                                                BackupsTaken.DBName
                                  FROM    JobRuns
                                                LEFT JOIN BackupsTaken ON JobRuns.RunStartDateTime <= BackupsTaken.backup_start_date
                                                                                           AND BackupsTaken.backup_finish_date <= JobRuns.RunFinishDateTime
                                  WHERE   JobRuns.RunStartDateTime >= DATEADD(DAY, ' + CONVERT(NVARCHAR(8), @NumDays) + ', GETDATE())
                                  ) AS src
                           PIVOT
                           ( COUNT(RunStarted) FOR RunStarted IN (' + @RunStartedList + ') ) AS pvt)
SELECT  sd.name as [Sys.Databases.Name],
        bp.*
FROM    BackupPivot bp
        FULL OUTER JOIN master.sys.databases sd ON bp.DBName = sd.name
ORDER BY sd.name ,
        bp.DBName
       ;
       ';
--SELECT  @SQL;
EXEC (@SQL);

GO
/*
Author: Greg Larsen
Original links: http://www.databasejournal.com/features/mssql/which-indexes-are-not-used.html
*/

SELECT o.name Object_Name,
       SCHEMA_NAME(o.schema_id) Schema_name,
       i.name Index_name,
       i.Type_Desc,
       CASE WHEN (s.user_seeks > 0
               OR s.user_scans > 0
               OR s.user_lookups > 0)
              AND s.user_updates > 0
            THEN 'USED AND UPDATED'
            WHEN (s.user_seeks > 0
               OR s.user_scans > 0
               OR s.user_lookups > 0)
              AND s.user_updates = 0
            THEN 'USED AND NOT UPDATED'
            WHEN  s.user_seeks IS NULL
              AND s.user_scans IS NULL
              AND s.user_lookups IS NULL
              AND s.user_updates IS NULL
            THEN 'NOT USED AND NOT UPDATED'
            WHEN (s.user_seeks = 0
              AND s.user_scans = 0
              AND s.user_lookups = 0)
              AND s.user_updates > 0
            THEN 'NOT USED AND UPDATED'
            ELSE 'NONE OF THE ABOVE'
            END AS Usage_Info,
       COALESCE(s.user_seeks,0) AS user_seeks,
       COALESCE(s.user_scans,0) AS user_scans,
       COALESCE(s.user_lookups,0) AS user_lookups,
       COALESCE(s.user_updates,0) AS user_updates  
 FROM sys.objects AS o
     JOIN sys.indexes AS i
 ON o.object_id = i.object_id
     LEFT OUTER JOIN
  sys.dm_db_index_usage_stats AS s   
 ON i.object_id = s.object_id  
  AND i.index_id = s.index_id
 WHERE o.type = 'U'
 -- Clustered and Non-Clustered indexes
  AND i.type IN (1, 2)
ORDER BY user_seeks, user_scans, user_lookups, user_updates ASC;
GO
