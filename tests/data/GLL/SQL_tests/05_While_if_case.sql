DECLARE @Counter int=1

DECLARE @Description int
SELECT @Description=MAX(ID)
FROM SalesLT.DemoTable

WHILE @Counter <5
BEGIN
	INSERT SalesLT.DemoTable(Description)
	VALUES ('ROW '+CONVERT(varchar(5),@Description))
	SET @Description=@Description+1
	SET @Counter=@Counter+1
END

SELECT Description FROM SalesLT.DemoTable
GO

IF @@ROWCOUNT<1
BEGIN
	PRINT 'Product was not found'
END
ELSE
BEGIN
	PRINT 'Product Updated'
END
GO

SELECT Name,
		CASE Size
			WHEN 'S' THEN 'Small'
			WHEN 'M' THEN 'Medium'
			WHEN 'L' THEN 'Large'
			WHEN 'XL' THEN 'Extra-Large'
			ELSE ISNULL(Size, 'n/a')
		END AS ProductSize
FROM SalesLT.Product;
GO