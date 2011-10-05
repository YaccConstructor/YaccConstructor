/****** Object:  StoredProcedure [sys].[sp_tables_ex]    Script Date: 09/27/2011 19:10:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER procedure [sys].[sp_tables_ex]
(
    @table_server       sysname,
    @table_name         sysname = null,
    @table_schema       sysname = null,
    @table_catalog      sysname = null,
    @table_type         sysname = null,
    @fUsePattern        bit = 1 -- To allow users to explicitly disable all pattern matching.
)
as
    if (@fUsePattern = 1) -- Does the user want it?
    begin
        if ((isnull(charindex('%', @table_name),0) = 0) and
            (isnull(charindex('[', @table_name),0) = 0) and
            (isnull(charindex('_', @table_name),0) = 0) and
            (isnull(charindex('%', @table_schema),0) = 0) and
            (isnull(charindex('[', @table_schema),0) = 0) and
            (isnull(charindex('_', @table_schema),0) = 0) and
            (isnull(charindex('%', @table_catalog),0) = 0) and
            (isnull(charindex('[', @table_catalog),0) = 0) and
            (isnull(charindex('_', @table_catalog),0) = 0))
        begin
            select @fUsePattern = 0 -- not a single wild char, so go the fast way.
        end
    end

    if @fUsePattern = 0
    begin
        /* -- Debug output, do not remove it.
        print '*************'
        print 'No pattern matching.'
        print @fUsePattern
        print isnull(@table_server, '@table_server = null')
        print isnull(@table_name, '@table_name = null')
        print isnull(@table_schema, '@table_schema = null')
        print isnull(@table_catalog, '@table_catalog = null')
        print isnull(@table_type, '@table_type = null')
        print '*************'
        */
        select
            TABLE_CAT   = rt.TABLE_CATALOG,
            TABLE_SCHEM = rt.TABLE_SCHEMA,
            TABLE_NAME  = rt.TABLE_NAME,
            TABLE_TYPE  = rt.TABLE_TYPE,
            REMARKS     = convert(nvarchar(255),rt.DESCRIPTION)
        from
            -- We can not pass @table_type directly here, because in ODBC we enclose
            -- it in '', which is not acceptable for OLEDB SProc.
            sys.fn_remote_tables(@table_server,
                                 @table_catalog,
                                 @table_schema,
                                 @table_name,
                                 NULL) rt
        where
            (charindex ('' + TABLE_TYPE + '', @table_type) <> 0 or @table_type is NULL)
        order by 4, 1, 2, 3
    end
    else
    begin
        /* -- Debug output, do not remove it.
        print '*************'
        print 'THERE IS pattern matching!'
        print @fUsePattern
        print isnull(@table_server, '@table_server = null')
        print isnull(@table_name, '@table_name = null')
        print isnull(@table_schema, '@table_schema = null')
        print isnull(@table_catalog, '@table_catalog = null')
        print isnull(@table_type, '@table_type = null')
        print '*************'
        */
        select
            TABLE_CAT   = TABLE_CATALOG,
            TABLE_SCHEM = TABLE_SCHEMA,
            TABLE_NAME  = TABLE_NAME,
            TABLE_TYPE  = TABLE_TYPE,
            REMARKS     = convert(nvarchar(255),DESCRIPTION)
        from
            sys.fn_remote_tables(@table_server,
                                 @table_catalog,
                                 NULL,
                                 NULL,
                                 NULL)
        where
            (TABLE_SCHEMA like @table_schema or
             @table_schema is NULL or
             (TABLE_SCHEMA is NULL and @table_schema = N'%')) and
            (TABLE_CATALOG like @table_catalog or
             @table_catalog is NULL or
             (TABLE_CATALOG is NULL and @table_catalog = N'%')) and
            (charindex ('' + TABLE_TYPE + '', @table_type) <> 0 or @table_type is NULL) and
            (TABLE_NAME like @table_name or
             @table_name is NULL)
        order by 4, 1, 2, 3
    end
