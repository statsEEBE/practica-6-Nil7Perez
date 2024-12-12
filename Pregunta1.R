
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
x
xbar<-mean(x)
xbar
sigma<-sqrt(25)
n<-length(x)
z005<-qnorm(0.95) # alpha igual a 0.1 confianza igual a 0.9
m<-z005*sigma/sqrt(n)
xbar-m
xbar+m

install.packages("BSDA")
library("BSDA")
z.test(x,sigma.x=sigma,conf.level=0.9)
# Provar la ipotesis que las cajas tienen un peso diferente a 500gr
#H0:mu=500
#H1: mu !=500 (mu0=500)
zc<-qnorm(0.95)
zc
muzero<-500
zobs<-(xbar-muzero)/(sigma/sqrt(n))
zobs
z.test(x,sigma.x=sigma,conf.level=0.9,mu=muzero)

#criterio 3 si el pvalor>alpha H0 se cumolple sino no
pvalor<-2*pnorm(-zobs)# negativo porque la observacion esta por encima de mu0, si estubiera por debajo seria en positivo. 
pvalor
#para cola superior
z.test(x,sigma.x=sigma,conf.level=0.9,alternative="greater")


# Preg 3 Varianza desconocida
n<-length(x)
xbar<-mean(x)
t005<-qt(0.995,n-1)
s<-sd(x)
m<-t005*s/sqrt(n)
c(xbar-m,xbar+m)
t.test(x,conf.level = 0.99)

# Hipotesis 
#H0:mu=mu0=500
#H1:mu!=mu0=500

t.test(x,conf.level = 0.99,mu=500,alternative="greater") # tobs=t


