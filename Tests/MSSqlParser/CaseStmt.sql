CREATE procedure testProc
as
  case when DatabaseProperty(db_name(), 'IsAutoUpdateStatistics') = 1 then 'ON' else 'OFF' end
GO