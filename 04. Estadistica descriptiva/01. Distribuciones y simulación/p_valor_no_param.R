# ##############################################  
( alfa =1- nSig )
# Calculo del estadistico del contraste
( Estadistico =( xbar -mu0)/(s/ sqrt (n)))
################################################
# Funcion para el calculo del p- valor
pValor = function ( EstadCon , tipoCon ){
if( tipoCon ==1) {
(pV =1- pnorm ( EstadCon ))
}
if( tipoCon ==2) {
(pV= pnorm ( EstadCon ))
}
if( tipoCon ==3) {
pV =2*(1- pnorm ( abs( EstadCon )))
}
return ( paste ("El p- Valor es ",pV , sep="",collapse =""))
} 
##########################################################
# Funcion para el calculo del limite de la region de rechazo
RegionRechazo = function (alfa , tipoCon ){
if( tipoCon ==1) {
( regionRech = paste (" Valores del Estadistico mayores que ",qnorm (1-
alfa )) )
}
if( tipoCon ==2) {
( regionRech = paste (" Valores del Estadistico menores que ",qnorm ( alfa
)) )
}
if( tipoCon ==3) {
( regionRech = paste (" Valores del Estadistico mas alejados del origen
que ",qnorm (1- alfa /2)) )
}
regionRech = paste ("La region de rechazo la forman los ",regionRech ,sep
="",collapse ="")
return ( regionRech )
}
############################################################
# Se aplican ambas funciones para mostrar los resultados
pValor ( Estadistico , TipoContraste )
Estadistico
RegionRechazo (alfa , TipoContraste )

####################
#EJEMPLOS
####################

datos=c(10.1, 9.5, 6.5, 8.0, 8.8, 12, 7.2, 10.5, 8.2, 9.3)
mu0=8
t.test(datos,mu=mu0,alternative="two.sided",conf.level = 0.95)
prop.test(x=1004,n=2499,conf.level=0.95,correct=FALSE)

#EJEMPLOS
####################
pValor ( ( mean(datos) -mu0)/(sd(datos)/ sqrt(length(datos))), 3 )
RegionRechazo (0.05 , 3 )

#Comparación de dos distribuciones KS
####################
x <- rnorm(50)
y <- runif(30)
# Proceden x e y de la misma distribucion?
ks.test(x, y)

