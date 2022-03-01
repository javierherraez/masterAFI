# -*- coding: utf-8 -*-

# Importamos los módulos necesarios
from cassandra.cluster import Cluster

# Abrimos una conexión al servidor local de Cassandra
cluster = Cluster()
session = cluster.connect()

# Seleccionamos el keyspace que queremos utilizae
session.execute('use test;')

# Preparamos un set de datos ficticio
data = [
    (0, 'mcorella', True),
    (1, 'jsmith', False),
    (2, 'jdoe', True)
]

# Procesamos todos los datos haciendo las inserciones necesarias
for user in data:
    sql = 'INSERT INTO tabla_prueba (id, nombre, activo) VALUES ('
    sql += str(user[0]) + ', '
    sql += '\'' + user[1] + '\', '
    sql += str(user[2]) + ');'
    session.execute(sql)


# Hacemos una query a la tabla de prueba
results = session.execute('SELECT * FROM tabla_prueba')

# Procesamos los resultados
for row in results:
    current_id = row.id
    current_name = row.nombre
    current_active = row.activo
    print('Identificador: ' + str(current_id))
    print('Nombre: ' + str(current_name))
    print('Activo: ' + str(current_active))


# Creamos un índice temporal para poder hacer consultas por nombre
session.execute('CREATE INDEX index_nombre ON tabla_prueba (nombre)')

# Hacemos una query por nombre
nombre = 'mcorella'
results = session.execute('SELECT id, activo FROM tabla_prueba WHERE nombre = \'' + nombre + '\';')

# Procesamos los resultados
for row in results:
    current_id = row.id
    current_active = row.activo
    print('Identificador: ' + str(current_id))
    print('Activo: ' + str(current_active))

# Eliminamos el índice creado
session.execute('DROP INDEX index_nombre')

# Cerramos la conexión a Cassandra
session.shutdown()

