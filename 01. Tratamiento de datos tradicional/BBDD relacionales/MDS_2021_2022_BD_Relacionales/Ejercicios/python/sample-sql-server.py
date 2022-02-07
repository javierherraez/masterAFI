#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Import requried packages
import pandas as pd
import pyodbc

# Create a connection to the store database
conn = pyodbc.connect('Driver={SQL Server};'
                      'Server=localhost\\SQLExpress;'
                      'Database=Store;'
#                      'user=sa'
#                      'password=SQL'
                      'Trusted_Connection=yes;')

# Prepare a query to retrieve the number of suppliers per country
sql = 'SELECT Country AS country , COUNT(*) num_suppliers \
        FROM Supplier \
        GROUP BY Country'

# Fetch the results of the query
results = pd.read_sql_query(sql, conn)

# Draw a plot of retrieved data
results.plot(x = 'country', y = 'num_suppliers', kind = 'bar')

# Prepare a query to retrieve the number of orders per year
sql = 'SELECT YEAR(OrderDate) AS order_year, COUNT(*) as num_orders \
       FROM [Order] \
       GROUP BY YEAR(OrderDate) \
       ORDER BY YEAR(OrderDate)'
       
# Fetch the results of the query
results = pd.read_sql_query(sql, conn)

# Prepare the results
results = results.loc[results.order_year != 0, :]

# Draw a plot of the retrieved data
results.plot(x = 'order_year', y = 'num_orders')

# Close the connection to the database
conn.close()