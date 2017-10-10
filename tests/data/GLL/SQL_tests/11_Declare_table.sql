DECLARE @table TABLE (
    [object_id]   int NOT NULL,
    index_id      int NOT NULL,
    object_name   char(30) NOT NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id)
);