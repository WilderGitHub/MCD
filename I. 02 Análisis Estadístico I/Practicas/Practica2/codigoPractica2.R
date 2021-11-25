#Ejercicio 22
# a)
A<-c("E1","E2")
B<-c("E3","E4")
C<-c("E2","E3","E5")
A_n<-2
B_n<-2
C_n<-3
n<-5
pA<-A_n/n
pB<-B_n/n
pC<-C_n/n
pAiBc<-2/n
pBiC<-1/n
Encabezados<-c("P(A)","P(B)","P(C)")
Valores<-c(pA,pB,pC)
Respuesta <-data.frame(Encabezados,Valores)
Respuesta
# b)
pA+pB
# c)
1-pA
1-pC
# d)
pA+(1-pB)-pAiBc
# e)
pB+pC-pBiC


