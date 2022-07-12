install.packages("BSDA")
library(BSDA)
horas<-c(1.5,2.2,0.9,1.3,2,1.6,1.8,1.5,2,1.2,1.7)
auto<-c(9,12,18,14,12,14,12,10,16,11,9,11,13,11,13,15,13,14)
SIGN.test(auto,md=12, alternative="two.sided", conf.level = 0.98)

##ejemplo 16.1  prueba de rangos de signo de Wilcoxon
library(MASS)
horas<-c(1.5,2.2,0.9,1.3,2.0,1.6,1.8,1.5,2.0,1.2,1.7)
wilcox.test(horas,mu=1.8,exact=T,alternative="t", conf.int=0.95)

### otra forma de hacer prueba de rangos de signo de Wilcoxon

install.packages("exactRankTests")
library(exactRankTests)
wilcox.exact(horas,mu=1.8,exact=T,alternative="t", conf.int=0.95)

####ejemplo 16.4  con y sin problemas

conproblem<-c(531,621,663,579,451,660,591,719,543,575)
sinproblem<-c(509,540,688,502,424,683,568,748,530,524)
wilcox.test(conproblem,sinproblem,mu=50,paired=T,exact=T,alternative="l",conf.int=0.95)

###prueba de U de Mann Whitney-Wilcoxon

campo<-c(14.8,10.6,7.3,12.5,5.6,12.9,6.3,16.1,9.0,11.4,4.2,2.7)
ciudad<-c(12.7,16.9,7.6,2.4,6.2,9.9,14.2,7.9,11.3,6.4,6.1,10.6,12.6,16.0,8.3,9.1,15.3,
          14.8,2.1,10.6,6.7,6.7,10.6,5.0,17.7,5.6,3.6,18.6,1.8,2.6,11.8,5.6,1.0,3.2,5.9,4.0)

wilcox.test(campo,ciudad,alternative="greater",paried=FALSE)

####prueba de kruskal - Wallis
s1<-c(24,16.7,22.8,19.8)
s2<-c(23.2,19.8,18.1,17.6,20.2,17.8)
s3<-c(18.4,19.1,17.3,17.3,19.7,18.9,18.8,19.3)
kruskal.test(list(s1,s2,s3))
