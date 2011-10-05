/****** Object:  StoredProcedure [sys].[sp_help]    Script Date: 09/27/2011 19:06:03 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_help]
        @objname nvarchar(776) = NULL           -- object name we're after
as
        -- PRELIMINARY
        set nocount on
        declare @dbname sysname
                ,@no varchar(35), @yes varchar(35), @none varchar(35)
        select @no = 'no', @yes = 'yes', @none = 'none'

        -- If no @objname given, give a little info about all objects.
        if @objname is null
        begin
                -- DISPLAY ALL SYSOBJECTS --
        select
            'Name'          = o.name,
            'Owner'         = user_name(ObjectProperty( object_id, 'ownerid')),
            'Object_type'   = substring(v.name,5,31)
        from sys.all_objects o, master.dbo.spt_values v
        where o.type = substring(v.name,1,2) collate database_default and v.type = 'O9T'
        order by [Owner] asc, Object_type desc, Name asc

                print ' '

                -- DISPLAY ALL USER TYPES
                select
                        'User_type'     = name,
                        'Storage_type'  = type_name(system_type_id),
                        'Length'                = max_length,
                        'Prec'          = Convert(int,TypePropertyEx(user_type_id, 'precision')),
                        'Scale'         = Convert(int,TypePropertyEx(user_type_id, 'scale')),
                        'Nullable'              = case when is_nullable = 1 then @yes else @no end,
                        'Default_name'  = isnull(object_name(default_object_id), @none),
                        'Rule_name'             = isnull(object_name(rule_object_id), @none),
                        'Collation'             = collation_name
                from sys.types
                where user_type_id > 256
                order by name

                return(0)
        end

        -- Make sure the @objname is local to the current database.
        select @dbname = parsename(@objname,3)
        if @dbname is null
                select @dbname = db_name()
        else if @dbname <> db_name()
                begin
                        raiserror(15250,-1,-1)
                        return(1)
                end

        -- @objname must be either sysobjects or systypes: first look in sysobjects
        declare @objid int
        declare @sysobj_type char(2)
        select @objid = object_id, @sysobj_type = type from sys.all_objects where object_id = object_id(@objname)

        -- IF NOT IN SYSOBJECTS, TRY SYSTYPES --
        if @objid is null
        begin
                -- UNDONE: SHOULD CHECK FOR AND DISALLOW MULTI-PART NAME
                select @objid = type_id(@objname)

                -- IF NOT IN SYSTYPES, GIVE UP
                if @objid is null
                begin
                        raiserror(15009,-1,-1,@objname,@dbname)
                        return(1)
                end

                -- DATA TYPE HELP (prec/scale only valid for numerics)
                select
                        'Type_name'     = name,
                        'Storage_type'  = type_name(system_type_id),
                        'Length'                = max_length,
                        'Prec'                  = Convert(int,TypePropertyEx(user_type_id, 'precision')),
                        'Scale'                 = Convert(int,TypePropertyEx(user_type_id, 'scale')),
                        'Nullable'                      = case when is_nullable=1 then @yes else @no end,
                        'Default_name'  = isnull(object_name(default_object_id), @none),
                        'Rule_name'             = isnull(object_name(rule_object_id), @none),
                        'Collation'             = collation_name
                from sys.types
                where user_type_id = @objid

                return(0)
        end

        -- FOUND IT IN SYSOBJECT, SO GIVE OBJECT INFO
        select
                'Name'                          = o.name,
                'Owner'                         = user_name(ObjectProperty( object_id, 'ownerid')),
        'Type'              = substring(v.name,5,31),
                'Created_datetime'      = o.create_date
        from sys.all_objects o, master.dbo.spt_values v
        where o.object_id = @objid and o.type = substring(v.name,1,2) collate database_default and v.type = 'O9T'

        print ' '

        -- DISPLAY COLUMN IF TABLE / VIEW
        if exists (select * from sys.all_columns where object_id = @objid)
        begin

                -- SET UP NUMERIC TYPES: THESE WILL HAVE NON-BLANK PREC/SCALE
                -- There must be a ',' immediately after each type name (including last one),
                -- because that's what we'll search for in charindex later.
                declare @precscaletypes nvarchar(150)
                select @precscaletypes = N'tinyint,smallint,decimal,int,bigint,real,money,float,numeric,smallmoney,date,time,datetime2,datetimeoffset,'

                -- INFO FOR EACH COLUMN
                print ' '
                select
                        'Column_name'                   = name,
                        'Type'                                  = type_name(user_type_id),
                        'Computed'                              = case when ColumnProperty(object_id, name, 'IsComputed') = 0 then @no else @yes end,
                        'Length'                                        = convert(int, max_length),
                        -- for prec/scale, only show for those types that have valid precision/scale
                        -- Search for type name + ',', because 'datetime' is actually a substring of 'datetime2' and 'datetimeoffset'
                        'Prec'                                  = case when charindex(type_name(system_type_id) + ',', @precscaletypes) > 0
                                                                                then convert(char(5),ColumnProperty(object_id, name, 'precision'))
                                                                                else '     ' end,
                        'Scale'                                 = case when charindex(type_name(system_type_id) + ',', @precscaletypes) > 0
                                                                                then convert(char(5),OdbcScale(system_type_id,scale))
                                                                                else '     ' end,
                        'Nullable'                              = case when is_nullable = 0 then @no else @yes end,
                        'TrimTrailingBlanks'    = case ColumnProperty(object_id, name, 'UsesAnsiTrim')
                                                                                when 1 then @no
                                                                                when 0 then @yes
                                                                                else '(n/a)' end,
                        'FixedLenNullInSource'  = case
                                                when type_name(system_type_id) not in ('varbinary','varchar','binary','char')
                                                        then '(n/a)'
                                                when is_nullable = 0 then @no else @yes end,
                        'Collation'             = collation_name
                from sys.all_columns where object_id = @objid

                -- IDENTITY COLUMN?
                if @sysobj_type in ('S ','U ','V ','TF') and @objid > 0
                begin
                        print ' '
                        declare @colname sysname
                        select @colname = col_name(@objid, column_id) from sys.identity_columns where object_id = @objid
                        select
                                'Identity'                              = isnull(@colname,'No identity column defined.'),
                                'Seed'                          = ident_seed(@objname),
                                'Increment'                     = ident_incr(@objname),
                                'Not For Replication'   = ColumnProperty(@objid, @colname, 'IsIDNotForRepl')
                        -- ROWGUIDCOL?
                        print ' '
                        select @colname = null
                        select @colname = name from sys.columns where object_id = @objid and is_rowguidcol = 1
                        select 'RowGuidCol' = isnull(@colname,'No rowguidcol column defined.')
                end
        end

        -- DISPLAY ANY PARAMS
        if exists (select * from sys.all_parameters where object_id = @objid)
        begin
                -- INFO ON PROC PARAMS
                print ' '
                select
                        'Parameter_name'        = name,
                        'Type'                  = type_name(user_type_id),
                        'Length'                        = max_length,
                        'Prec'                  = case when type_name(system_type_id) = 'uniqueidentifier' then precision
                                                                else OdbcPrec(system_type_id, max_length, precision) end,
                        'Scale'                 = OdbcScale(system_type_id, scale),
                        'Param_order'           = parameter_id,
                        'Collation'                     = convert(sysname, case when system_type_id in (35, 99, 167, 175, 231, 239)
                                                then ServerProperty('collation') end)

                from sys.all_parameters where object_id = @objid
        end

        -- DISPLAY TABLE INDEXES & CONSTRAINTS
        if @sysobj_type in ('S ','U ')
        begin
                print ' '
                EXEC sys.sp_objectfilegroup @objid
                print ' '
                EXEC sys.sp_helpindex @objname
                print ' '
                EXEC sys.sp_helpconstraint @objname,'nomsg'
                if (select count(*) from sysdepends where depid = @objid and deptype = 1) = 0
                begin
                        raiserror(15647,-1,-1,@objname) -- No views with schemabinding reference table '%ls'.
                end
                else
                begin
            select distinct 'Table is referenced by views' = obj.name from sys.objects obj, sysdepends deps
                                where obj.type ='V' and obj.object_id = deps.id and deps.depid = @objid
                                        and deps.deptype = 1 group by obj.name

                end
        end
        else if @sysobj_type in ('V ') and @objid > 0
        begin
                -- VIEWS DONT HAVE CONSTRAINTS, BUT PRINT THESE MESSAGES BECAUSE 6.5 DID
                print ' '
                raiserror(15469,-1,-1,@objname) -- No constraints defined
                print ' '
                raiserror(15470,-1,-1,@objname) -- No foreign keys reference table '%ls'.
                EXEC sys.sp_helpindex @objname
        end

        return (0) -- sp_help
