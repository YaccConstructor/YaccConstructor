CREATE procedure TestProc
        @server                 sysname,                        -- server name
        @local                  varchar(10) = NULL,     -- NULL or 'local'
        @duplicate_ok   varchar(13) = NULL      -- NULL or 'duplicate_ok'
as

        DECLARE @localentry     bit,
                @dup_ok         bit,
                @retcode        int
        DECLARE @test varchar(200)

GO
