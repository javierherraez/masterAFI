#TCL  
TamanoMuestra=1000;NumMuestras=100;
muestra=array(0,c(TamanoMuestra,NumMuestras)) 
SumaMuestra=rep(0,TamanoMuestra)# Definir vector con 0's TamanoMuestra veces


for (i in 1:NumMuestras) {
  muestra[,i]=runif(TamanoMuestra,2,4) #llenar columna i con num. aleat. unif.
  SumaMuestra=SumaMuestra+muestra[,i]
}

hist(SumaMuestra)
qqnorm(SumaMuestra)
