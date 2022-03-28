# Read the contents of a text file
text_file = sc.textFile('/home/mdsuser/spark/README.md')
print(f'Número de filas: { text_file.count() }')

# Filter the contents of the file
filtered = text_file.filter(lambda line: 'Spark' in line)
print(f'Número de filas con "Spark": { filtered.count() }')

