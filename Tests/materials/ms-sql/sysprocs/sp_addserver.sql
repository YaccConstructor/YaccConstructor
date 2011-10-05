/****** Object:  StoredProcedure [sys].[sp_addserver]    Script Date: 09/27/2011 19:07:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_addserver]
        @server                 sysname,                        -- server name
        @local                  varchar(10) = NULL,     -- NULL or 'local'
        @duplicate_ok   varchar(13) = NULL      -- NULL or 'duplicate_ok'
as
        -- VARS
        DECLARE @localentry     bit,
                        @dup_ok         bit,
                        @retcode        int
        select @duplicate_ok = LOWER (@duplicate_ok collate Latin1_General_CI_AS)

        -- VALIDATE PARAMETERS/OPTIONS
        SELECT  @localentry = CASE WHEN @local IS NULL THEN 0
                                                WHEN lower(@local) = 'local' THEN 1
                                                ELSE NULL END,
                        @dup_ok = CASE WHEN @duplicate_ok IS NULL THEN 0
                                                WHEN @duplicate_ok = 'duplicate_ok' THEN 1
                                                ELSE NULL END
        IF @localentry IS NULL OR @dup_ok IS NULL
        BEGIN
                raiserror(15600,-1,-1,'sys.sp_addserver')
                return (1)
        END

        -- DISALLOW USER TRANSACTION
        set implicit_transactions off
        if @@trancount > 0
        begin
                raiserror(15002,-1,-1,'sys.sp_addlinkedserver')
                return (1)
        end

        BEGIN TRANSACTION

        -- ADD THE SERVER (CHECKS PERMISSIONS, ETC)
        EXEC @retcode = sys.sp_MSaddserver_internal @server,
                                NULL, NULL, NULL, NULL, NULL, NULL, -- @srvproduct ... @catalog
                                0,                      -- @linkedstyle
                                @localentry

        if( @retcode = 0)
        begin
                if (@localentry = 1)
                begin
                        -- EMDEventType(x_eet_Alter_Instance), EMDUniversalClass(x_eunc_Server), src major id, src minor id, src name
                        -- -1 means ignore target stuff, target major id, target minor id, target name,
                        -- # of parameters, 5 parameters
                        EXEC %%System().FireTrigger(ID = 214, ID = 100, ID = 0, ID = 0, Value = NULL,
                                ID = -1, ID = 0, ID = 0, Value = NULL, 
                                ID = 3, Value = @server, Value = @local, Value = @duplicate_ok, Value = NULL, Value = NULL, Value = NULL, Value = NULL)
                end
                else
                begin
                        -- EMDEventType(x_eet_Create_Remote_Server), EMDUniversalClass(x_eunc_Server), src major id, src minor id, src name
                        -- -1 means ignore target stuff, target major id, target minor id, target name,
                        -- # of parameters, 5 parameters
                        EXEC %%System().FireTrigger(ID = 230, ID = 100, ID = 0, ID = 0, Value = @server,
                                ID = -1, ID = 0, ID = 0, Value = NULL, 
                                ID = 3, Value = @server, Value = @local, Value = @duplicate_ok, Value = NULL, Value = NULL, Value = NULL, Value = NULL)
                end

                COMMIT TRANSACTION
                -- SUCCESS
                return (0) -- sp_addserver
        end
        else if( @retcode = 2 )
        begin
                ROLLBACK

                if @dup_ok = 1
                        return (0)
                else
                        begin
                                raiserror(15028,-1,-1,@server);
                                return (1);
                        end;
        end
        else 
        begin
                ROLLBACK
                return (1)
        end
