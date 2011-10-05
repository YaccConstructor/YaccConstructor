/****** Object:  StoredProcedure [sys].[sp_password]    Script Date: 09/27/2011 19:09:16 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_password]
    @old sysname = NULL,        -- the old (current) password
    @new sysname,               -- the new password
    @loginame sysname = NULL    -- user to change password on
as
    -- SETUP RUNTIME OPTIONS / DECLARE VARIABLES --
        set nocount on
        declare @exec_stmt nvarchar(4000)

    -- RESOLVE LOGIN NAME
    if @loginame is null
        select @loginame = suser_sname()

    if @new is null
        select @new = ''

    -- DISALLOW USER TRANSACTION --
        set implicit_transactions off
        IF (@@trancount > 0)
        begin
                raiserror(15002,-1,-1,'sys.sp_password')
                return (1)
        end

        -- CHECK IT'S A SQL LOGIN --
    if not exists (select * from master.dbo.syslogins where
                   loginname = @loginame and isntname = 0)
        begin
                raiserror(15007,-1,-1,@loginame)
                return (1)
        end

        if @old is null
                set @exec_stmt = 'alter login ' + quotename(@loginame) +
                        ' with password = ' + quotename(@new, '''')
        else
                set @exec_stmt = 'alter login ' + quotename(@loginame) +
                        ' with password = ' + quotename(@new, '''') + ' old_password = ' + quotename(@old, '''')

        exec (@exec_stmt)       

        if @@error <> 0
                return (1)

    -- RETURN SUCCESS --
        return  (0)     -- sp_password
