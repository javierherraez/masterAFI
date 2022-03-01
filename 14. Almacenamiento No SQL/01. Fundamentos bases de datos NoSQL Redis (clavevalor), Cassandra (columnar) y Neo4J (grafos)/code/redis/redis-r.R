# Cargamos la libraría de redis
if(!('rredis' %in% installed.packages())) {
  install.packages('rredis')
}
require(rredis)

# Abrimos una conexión a redis
redisConnect()

# Establecimiento el valor de una clave de tipo String
redisSet('mensaje', 'hola')
redisSet('prueba', charToRaw('1'))
redisSet('usuario:1', 'mcorella')
redisSet('usuario:2', 'jsmith')
redisMSet(list('usuario:3'='jdoe', 'usuario:4'='cmartin'))
redisSet('mensaje', 'hola', NX = T)

# Recueración de valor de clave
valor <- redisGet('usuario:1')
valor <- redisMGet(c('usuario:2', 'usuario:3'))

# Incremento de valor de una clave "numérica"
redisIncr('prueba')
valor <- redisGet('prueba')
redisIncrBy('prueba', 10)
valor <- redisGet('prueba')

# Decremento de valor de una clave "numérica"
redisDecr('prueba')
valor <- redisGet('prueba')
redisDecrBy('prueba', 10)
valor <- redisGet('prueba')

# Eliminación de claves
redisDelete('prueba')
redisDelete('mensaje')

# Chequeo sobre la existencia de una clave
redisExists('usuario:1')

# Establecimiento de un tiempo de expiración a una clave
redisSet('contador', 0)
redisExpire('contador', 10)
redisTTL('contador')

# Recuperación de todas las claves disponibles (en un patrón)
redisKeys()
redisKeys('usuario:*')

# Recuperación del tipo de una clave
redisType('usuario:1')

# Cerramos la conexión al servidor
redisClose()
