/****** Object:  StoredProcedure [sys].[sp_adduser]    Script Date: 09/27/2011 18:58:12 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_adduser]
        @loginame       sysname,            -- user's login name in syslogins
        @name_in_db     sysname = NULL, -- user's name to add to current db
        @grpname                sysname = NULL  -- role to which user should be added.
as
    -- SETUP RUNTIME OPTIONS / DECLARE VARIABLES --
        set nocount on
        declare @ret        int

    -- LIMIT TO SQL/NT USERS IN SYSLOGINS (BCKWRD COMPAT ONLY!)
        if not exists (select * from master.dbo.syslogins where loginname = @loginame
                        and (isntuser = 1 or isntname = 0))
        and @loginame <> 'guest'
    begin
        raiserror(15007,-1,-1,@loginame)
        return (1)
    end

        -- VALIDATE THE ROLENAME --
    if @grpname is not null and
           not exists (select * from sysusers where name = @grpname and issqlrole = 1)
    begin
            raiserror(15014,-1,-1,@grpname)
            return (1)
    end

    if @name_in_db is null
        select @name_in_db = @loginame

        -- In Hydra only the user dbo can do this --
    if (not is_member('dbo') = 1)
        begin
            -- AUDIT FAILED SECURITY CHECK --
        dbcc auditevent (109, 1, 0, @loginame, @name_in_db, @grpname , NULL, NULL, NULL, NULL)
                raiserror(15247,-1,-1)
                return (1)
        end

    -- ADD THE USER TO THE DATABASE --
    EXEC @ret = sys.sp_grantdbaccess @loginame, @name_in_db OUT
    if (@ret <> 0)
        return (1)

    -- ADD USER TO ROLE IF GIVEN. NOP FOR 'public' --
    if (@grpname is not null) and (@grpname <> 'public')
    begin
        EXEC @ret = sys.sp_addrolemember @grpname, @name_in_db
        if @ret <> 0
                begin
                        -- "ROLLBACK" THE ABOVE sp_grantdbaccess --
                        exec sys.sp_revokedbaccess @name_in_db
            return (1)
                end
    end

    -- RETURN SUCCESS --
    return (0) -- sp_adduser
