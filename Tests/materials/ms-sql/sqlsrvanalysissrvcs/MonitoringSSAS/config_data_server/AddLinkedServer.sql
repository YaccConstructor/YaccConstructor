EXEC master.dbo.sp_addlinkedserver
   @server = N'SSAS2008' -- linked server friendly name
 , @srvproduct=N'MSOLAP'
 , @provider=N'MSOLAP'
 , @datasrc=N'SSASServer' -- Server Name
 , @catalog=N'YourDatabaseNameHere' -- Database Name
 go
 
 EXEC master.dbo.sp_addlinkedsrvlogin
   @rmtsrvname=N'SSAS2008'
 , @useself=N'False'
 , @locallogin=NULL
 , @rmtuser=NULL
 , @rmtpassword=NULL
GO

