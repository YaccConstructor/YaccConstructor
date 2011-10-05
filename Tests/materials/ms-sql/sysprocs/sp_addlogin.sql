/****** Object:  StoredProcedure [sys].[sp_addlogin]    Script Date: 09/27/2011 18:56:44 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER procedure [sys].[sp_addlogin]
    @loginame		sysname
   ,@passwd         sysname = Null
   ,@defdb          sysname = 'master'      -- UNDONE: DEFAULT CONFIGURABLE???
   ,@deflanguage    sysname = Null
   ,@sid			varbinary(16) = Null
   ,@encryptopt		varchar(20) = Null
AS
    -- SETUP RUNTIME OPTIONS / DECLARE VARIABLES --
	set nocount on
	declare @exec_stmt nvarchar(4000)
	declare @hextext varchar(256)
	declare @ret int

    -- DISALLOW USER TRANSACTION --
	set implicit_transactions off
	IF (@@trancount > 0)
	begin
		raiserror(15002,-1,-1,'sys.sp_addlogin')
		return (1)
	end

    -- VALIDATE LOGIN NAME:
	execute @ret = sys.sp_validname @loginame
	if (@ret <> 0)
        return (1)

	set @exec_stmt = 'create login ' + quotename(@loginame)

    if @passwd is null
        select @passwd = ''

	if (@encryptopt is null)
		set @exec_stmt = @exec_stmt + ' with password = ' + quotename(@passwd, '''')
	else
	begin
		declare @passwdbin varbinary(256)
		set @passwdbin = convert(varbinary(256), @passwd)
		execute sys.sp_hexadecimal @passwdbin, @hextext OUT
		set @exec_stmt = @exec_stmt + ' with password = ' + @hextext

		if (@encryptopt = 'skip_encryption_old')
			set @exec_stmt = @exec_stmt + ' hashed '
		else if (@encryptopt = 'skip_encryption')
			set @exec_stmt = @exec_stmt + ' hashed '
		else
		begin
			raiserror(15600,-1,-1,'sys.sp_addlogin')
			return 1
		end
	end
	
    if (@defdb is not null)
		set @exec_stmt = @exec_stmt + ', default_database = ' + quotename(@defdb)

	if (@deflanguage is not null)
		set @exec_stmt = @exec_stmt + ', default_language = ' + quotename(@deflanguage)

	if (@sid is not null)
	begin
		execute sys.sp_hexadecimal @sid, @hextext OUT
		set @exec_stmt = @exec_stmt + ', sid = ' + @hextext
	end

	exec (@exec_stmt)

	if @@error <> 0
		return (1)

    -- RETURN SUCCESS --
	return  (0)	-- sp_addlogin
