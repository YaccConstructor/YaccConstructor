CREATE procedure testProc
        @server                 sysname,                        -- server name
        @local                  varchar(10) = NULL,     -- NULL or 'local'
        @duplicate_ok   varchar(13) = NULL      -- NULL or 'duplicate_ok'
as
        CREATE MESSAGE TYPE
    	[//Adventure-Works.com/Expenses/SubmitExpense]         
    	VALIDATION = WELL_FORMED_XML ;         

	CREATE MESSAGE TYPE
    	[//Adventure-Works.com/Expenses/ExpenseApprovedOrDenied]         
    	VALIDATION = WELL_FORMED_XML ;         

	CREATE MESSAGE TYPE         
    	[//Adventure-Works.com/Expenses/ExpenseReimbursed]         
    	VALIDATION= WELL_FORMED_XML ;         

	CREATE CONTRACT          
    	[//Adventure-Works.com/Expenses/ExpenseSubmission]         
    	( [//Adventure-Works.com/Expenses/SubmitExpense]         
          SENT BY INITIATOR,         
     	[//Adventure-Works.com/Expenses/ExpenseApprovedOrDenied]         
          SENT BY TARGET,         
      	[//Adventure-Works.com/Expenses/ExpenseReimbursed]         
          SENT BY TARGET         
    	) ;

GO