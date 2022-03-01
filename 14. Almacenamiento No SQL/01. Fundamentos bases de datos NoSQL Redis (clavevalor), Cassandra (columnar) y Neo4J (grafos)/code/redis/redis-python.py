# -*- coding: utf-8 -*-

# Importamos los módulos necesarios
import redis

# Abrimos una conexión al servidor de redis
r = redis.Redis()

# Establecimiento el valor de una clave de tipo String
r.set('mensaje', 'hola')
r.set('prueba', 1)
r.set('usuario:1', 'mcorella')
r.set('usuario:2', 'jsmith')
r.mset({'usuario:3':'jdoe', 'usuario:4':'cmartin'})
r.setnx('mensaje', 'hola')

# Recueración de valor de clave
valor = r.get('usuario:1')
valor = r.mget(['usuario:2', 'usuario:3'])

# Incremento de valor de una clave "numérica"
r.incr('prueba')
valor = r.get('prueba')
r.incrby('prueba', 10)
valor = r.get('prueba')

# Decremento de valor de una clave "numérica"
r.decr('prueba')
valor = r.get('prueba')
r.decrby('prueba', 10)
valor = r.get('prueba')

# Eliminación de claves
r.delete('prueba')
r.delete('mensaje')

# Chequeo sobre la existencia de una clave
r.exists('usuario:1')

# Establecimiento de un tiempo de expiración a una clave
r.set('contador', 0)
r.expire('contador', 10)
r.ttl('contador')

# Recuperación de todas las claves disponibles (en un patrón)
r.keys()
r.keys('usuario:*')

# Recuperación del tipo de una clave
r.type('usuario:1')
