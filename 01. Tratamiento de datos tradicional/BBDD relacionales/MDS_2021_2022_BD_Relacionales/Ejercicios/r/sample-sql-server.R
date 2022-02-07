# Load required packages
library(odbc)

# Create a connection to the store database
conn <- dbConnect(odbc(), 
                  Driver = "SQL Server",
                  Server = "localhost\\SQLExpress",
                  Database = "Store",
#                  UID = "sa",
#                  PWD = "SQL",
                  Trusted_Connection = "True",
                  Port = 1433)

# Prepare a query to retrieve the number of suppliers per country
sql <- 'SELECT Country AS country , COUNT(*) num_suppliers
        FROM Supplier
        GROUP BY Country'

# Execute the prepared query
res <- dbSendQuery(conn, sql)

# Fetch the results of the query
results <- dbFetch(res)

# Close/release the query results
dbClearResult(res)

# Transform results to a named matrix
rownames(results) <- results$country
results <- results[, 'num_suppliers', drop=F]
results <- t(as.matrix(results))

# Draw a plot of retrieved data
par(mai = c(1.3, 0.7, 1, 0.3))
barplot(results, main = 'Suppliers by country', las = 2)

# Prepare a query to retrieve the number of orders per year
sql <- 'SELECT YEAR(OrderDate) AS order_year, COUNT(*) as num_orders
        FROM [Order]
        GROUP BY YEAR(OrderDate)
        ORDER BY YEAR(OrderDate)'

# Execute the prepare query
res <- dbSendQuery(conn, sql)

# Fetch the results of the query
results <- dbFetch(res)

# Close/release the query results
dbClearResult(res)

# Prepare the results
results <- results[results$order_year != 0, ]

# Draw a plot of retrieved data
par(mai = c(1, 1, 1, 0.3))
plot(results, type = "n", main = 'Orders per year')
lines(results)

# Close the connection to the database
dbDisconnect(conn)
