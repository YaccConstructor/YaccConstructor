USE AdventureWorks
GO
DECLARE contact_cursor CURSOR FOR
SELECT LastName FROM Person.Contact
WHERE LastName LIKE 'B%'
ORDER BY LastName

OPEN contact_cursor

-- Perform the first fetch.
FETCH NEXT FROM contact_cursor

-- Check @@FETCH_STATUS to see if there are any more rows to fetch.
WHILE @@FETCH_STATUS = 0
BEGIN
   -- This is executed as long as the previous fetch succeeds.
   FETCH NEXT FROM contact_cursor
END

CLOSE contact_cursor
DEALLOCATE contact_cursor
GO