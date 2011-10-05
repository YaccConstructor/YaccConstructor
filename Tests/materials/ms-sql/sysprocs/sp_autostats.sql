/****** Object:  StoredProcedure [sys].[sp_autostats]    Script Date: 09/27/2011 19:04:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER procedure [sys].[sp_autostats]
	@tblname 	nvarchar(776),
	@flagc		varchar(10)=null,
	@indname	sysname=null
as
begin
	declare	@flag		bit,	-- no recompute
			@permit		bit,	-- locked after permission check?
			@tabid		int,
			@objtype	varchar(2)

	--  Check flag
	set @flag = (case lower(@flagc)
		when 'on' 	then 0
		when 'off' 	then 1
		end)
	if @flag is null AND @flagc is NOT null
    begin
		raiserror(17000,-1,-1)
       	return (1)
    end

	-- set NORECOMPUTE mask
	select @permit = 1

	-- Check we are executing in the correct database
	declare @db sysname
	select @db = parsename(@tblname, 3)

	if (@db is NOT null AND @db <> db_name())
	begin
		raiserror(15387,-1,-1)
		return (1)
	end

	-- VERIFY WE HAVE A USER-TABLE/INDEXED-VIEW BY THIS NAME IN THE DATABASE
	select @tabid = object_id, @objtype = type from sys.objects
		where object_id = object_id(@tblname, 'local') and (type = 'U' or type = 'V' or type = 'IT')
	if (@tabid is null) OR
		(
			@objtype = 'V' AND
			(ObjectProperty(@tabid, 'IsIndexed') = 0 OR
			ObjectProperty(@tabid, 'IsMSShipped') = 1)
		)
	begin
		raiserror(15390,-1,-1,@tblname)
		return @@error
	end
	
	-- PRINT or UPDATE status?
	if (@flag is null)
	begin
		-- Display global settings (sp_dboption)
		--
		PRINT 'Global statistics settings for ' + quotename(db_name(), '[') + ':'
		PRINT '  Automatic update statistics: ' + (case when DatabaseProperty(db_name(), 'IsAutoUpdateStatistics') = 1 then 'ON' else 'OFF' end)
		PRINT '  Automatic create statistics: ' + (case when DatabaseProperty(db_name(), 'IsAutoCreateStatistics') = 1 then 'ON' else 'OFF' end)
		PRINT ''

		-- Display the current status of the index(s)
		--
		PRINT 'settings for table ' + quotename(@tblname, '[')
		PRINT ''
		select 'Index Name' = quotename(s.name, '['),
		       'AUTOSTATS' = case s.no_recompute
				when 1 then 'OFF'
				else 'ON'
			end,
		       'Last Updated' = stats_date(@tabid, s.stats_id)
		from sys.stats s
		where s.object_id = @tabid AND		-- Table
			case 					-- Match name
				when @indname is null then 1
				when @indname = s.name then 1
				else 0
			end = 1
	end
	else
	begin
		BEGIN TRANSACTION

		-- Lock the table schema and check permissions
		EXEC %%Object(MultiName = @tblname).LockMatchID(ID = @tabid, Exclusive = 1, BindInternal = 1)
		if (@@error <> 0)
		begin
			COMMIT TRANSACTION
			raiserror(15165,-1,-1,@tblname)
			return @@error
		end
	
		-- Flip the status bits
		if (@indname is null)	-- Match all index
		begin
			declare ms_crs_autostat cursor local static for
				select	s.name
				from sys.stats s left outer join sys.indexes i on s.[object_id] = i.[object_id] and s.[stats_id] = i.[index_id]
				where s.[object_id] = @tabid and isnull(i.is_hypothetical, 0) = 0
			
			open ms_crs_autostat

			fetch next from ms_crs_autostat into @indname

			while @@fetch_status = 0
			begin
				EXEC %%StatisticsEx(ObjectID = @tabid, Name = @indname).SetNoRecompute(Value = @flag)
				fetch next from ms_crs_autostat into @indname
			end
			deallocate ms_crs_autostat
		end
		else
		begin
			if exists(select *
				from sys.stats
				where object_id = @tabid and name = @indname)
				EXEC %%StatisticsEx(ObjectID = @tabid, Name = @indname).SetNoRecompute(Value = @flag)
			else
			begin
				COMMIT TRANSACTION
				raiserror(15323,-1,-1,@tblname)
				return @@error
			end
		end

		COMMIT TRANSACTION

	end

	return(0) -- sp_autostats
end
