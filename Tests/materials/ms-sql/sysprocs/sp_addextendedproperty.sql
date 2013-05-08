/****** Object:  StoredProcedure [sys].[sp_addextendedproperty]    Script Date: 09/27/2011 19:11:33 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [sys].[sp_addextendedproperty]
        @name sysname,
        @value sql_variant                      = NULL,
        @level0type     varchar(128)    = NULL,
        @level0name     sysname                 = NULL,
        @level1type     varchar(128)    = NULL,
        @level1name     sysname                 = NULL,
        @level2type     varchar(128)    = NULL,
        @level2name     sysname                 = NULL
as

        declare @ret int

        if datalength(@value) > 7500
        begin
                raiserror(15097,-1,-1)
                return 1
        end
        
        if @name is null
        begin
                raiserror(15600,-1,-1,'sp_addextendedproperty')
                return (1)
        end

        execute @ret = sys.sp_validname @name
        if (@ret <> 0)
        begin
                raiserror(15600,-1,-1,'sp_addextendedproperty')
                return (1)
        end
        

        BEGIN TRANSACTION
        
        begin
                EXEC %%ExtendedPropertySet ().AddValue(Name = @name, Value = @value, Level0type = @level0type, Level0name = @level0name, Level1type = @level1type, Level1name = @level1name, Level2type = @level2type, Level2name = @level2name)
                IF @@error <> 0
                begin
                        COMMIT TRANSACTION
                        return (1)
                end
        end
        
        COMMIT TRANSACTION
        return (0)
