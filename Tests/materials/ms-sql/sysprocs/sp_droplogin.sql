/****** Object:  StoredProcedure [sys].[sp_droplogin]    Script Date: 09/27/2011 19:08:15 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_droplogin]
        @loginame sysname
as
    -- SETUP RUNTIME OPTIONS / DECLARE VARIABLES --
        set nocount on
        declare @exec_stmt nvarchar(4000)
        declare @ret int

    -- DISALLOW USER TRANSACTION --
        set implicit_transactions off
        IF (@@trancount > 0)
        begin
                raiserror(15002,-1,-1,'sys.sp_droplogin')
                return (1)
        end

    -- VALIDATE LOGIN NAME:
        execute @ret = sys.sp_validname @loginame
        if (@ret <> 0)
        return (1)

        -- CHECK IT'S A SQL LOGIN --
        if (not exists (select * from master.dbo.syslogins where
                                        loginname = @loginame and isntname = 0))
        begin
                raiserror(15007,-1,-1,@loginame)
                return(1)
        end

        set @exec_stmt = 'drop login ' + quotename(@loginame)

        exec (@exec_stmt)       

        if @@error <> 0
                return (1)

    -- SUCCESS MESSAGE --
        return (0)      -- sp_droplogin
