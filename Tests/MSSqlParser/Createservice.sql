CREATE procedure testProc
        @server                 sysname,                        -- server name
        @local                  varchar(10) = NULL,     -- NULL or 'local'
        @duplicate_ok   varchar(13) = NULL      -- NULL or 'duplicate_ok'
as
        CREATE SERVICE [//Adventure-Works.com/Expenses] ON QUEUE ExpenseQueue ;

GO
