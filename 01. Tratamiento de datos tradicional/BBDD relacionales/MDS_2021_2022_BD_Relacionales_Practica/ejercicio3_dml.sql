BULK INSERT f1_results
FROM 'C:\Users\jherraez\Documents\BBDD relacionales\MDS_2021_2022_BD_Relacionales_Practica\f1_results.csv'
WITH (
	FIRSTROW = 2,
	FIELDTERMINATOR = ';',
	ROWTERMINATOR = '\n',
	FORMAT = 'CSV',
	CODEPAGE = '65001'
);