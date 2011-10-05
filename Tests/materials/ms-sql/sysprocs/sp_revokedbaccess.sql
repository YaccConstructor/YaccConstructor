/****** Object:  StoredProcedure [sys].[sp_revokedbaccess]    Script Date: 09/27/2011 19:09:38 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER procedure [sys].[sp_revokedbaccess]
	@name_in_db     sysname
as
    -- SETUP RUNTIME OPTIONS / DECLARE VARIABLES --
	set nocount on
	declare @ret        int,
			@stmtU		nvarchar(4000)

    -- DISALLOW USER TRANSACTION --
	set implicit_transactions off
	IF (@@trancount > 0)
	begin
		raiserror(15002,-1,-1,'sys.sp_revokedbaccess')
		return (1)
	end
	
	exec @ret = sys.sp_validname @name_in_db
	if @ret <> 0
		return(1)

	BEGIN TRANSACTION
	-- drop user
        -- for guest disable its access to database, this way guest schema is not dropped
	declare @guestname sysname
	set @guestname='guest'

    if lower(@name_in_db) = @guestname 
	begin 
		if db_id() in (1,2)
		begin
			ROLLBACK TRANSACTION
			raiserror(15182,-1,-1)			
			return (1)		
		end

		-- don't let disable guest twice
		if not exists (select * from sys.database_permissions p where p.class = 0 and p.major_id = 0 and p.minor_id = 0
			and p.grantee_principal_id = 2 and p.type = 'CO' and p.state in ('G','W')
			and not exists (select * from sys.database_permissions d where d.class = 0 and d.major_id = 0 and d.minor_id = 0
				and d.grantee_principal_id = p.grantee_principal_id and d.type = 'CO' and d.state = 'D'))
		begin
			ROLLBACK TRANSACTION
			raiserror(15539,-1,-1,@name_in_db)
			return (1)
		end
	end
	else
	begin
		-- this will drop the schema owned by the user that has the same name as the user,
		-- if such schema exists
		EXEC %%Owner(Name = @name_in_db).DropSchema(OwnerType = 1)
		if @@error <> 0
		begin
			ROLLBACK TRANSACTION
			-- error message comes from inside the invoke
			return (1)
		end
	end

	set @stmtU = 'drop user ' + quotename(@name_in_db, ']')

	-- drop the owner
	exec (@stmtU)
	if @@error <> 0
	begin
		ROLLBACK TRANSACTION
		-- error message comes from inside the statement
		return (1)
	end
    
	COMMIT TRANSACTION

    -- RETURN SUCCESS --
    return (0)	-- sp_revokedbaccess
