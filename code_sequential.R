# Densité de X1 dans groupe 0
d0x1 = function(x) density(D_SXY0[,v1], from=x, to=x, n=1)$y
# CDF de X1 dans groupe 0 (en cumulant densité puis normalisant pour avoir proba)
FD0x1 = function(x) sum(Vectorize(d0x1)(seq(-10,x,by=.0025)))/sum(Vectorize(d0x1)(seq(-10,10,by=.0025)))
# Permet de calculer la fonction inverse d'une autre fonction
inverse = function (f, lower = -100, upper = 100) {
  function (y) as.numeric(uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1])
}
# Fonction quantile de X1 dans groupe 1
QD1x1 = inverse(FD1x1, -10, 10)

# Poids autour de a1 gaussiens
w0 = dnorm(D_SXY0[,v1], a1)
# Normalisation des poids
w0 = w0/sum(w0)
# Estimation de la densité de X2|X1 en mettant des points autour de la valeur souhaitée pour X1
d0x2 = function(x) density(D_SXY0[,v2],
                           weights=w0,
                           from=x, to=x,n=1)$y

rm(list=ls())
# Simulation de lois normales
M1=c(-1,-1)
M2=c(1.5,1.5)
S1=matrix(c(1,.5,.5,1)*1.2^2,2,2)
S2=matrix(c(1,-.4,-.4,1)*.9^2,2,2)
# 800 obs. au total
n = 400
X1 = mnormt::rmnorm(n, M1, 4*S1)
X2 = mnormt::rmnorm(n, M2, 4*S2)
# Dataset pour groupe 0
D_SXY_0 = data.frame(S = 0,
                     X1 = X1[,1],
                     X2 = X1[,2])
# Dataset pour groupe 1
D_SXY_1 = data.frame(S = 1,
                     X1 = X2[,1],
                     X2 = X2[,2])
# Simulation de variable binaire Y dans {0,1} avec loi logistique
eta_0 = (D_SXY_0$X1*1.2+D_SXY_0$X2/2*.8)/2
eta_1 = (D_SXY_1$X1*.8+D_SXY_1$X2/2*1.2)/2
p_0 = exp(eta_0)/(1+exp(eta_0))
p_1 = exp(eta_1)/(1+exp(eta_1))
set.seed(123)
D_SXY_0$Y = rbinom(n, size=1, prob = p_0)
D_SXY_1$Y = rbinom(n, size=1, prob = p_1)
# Dataset entier
D_SXY = rbind(D_SXY_0,D_SXY_1)

# estimation du modèle pour Y
library(mgcv)
reg_0 = gam(Y ~ s(X1,X2), data=D_SXY_0, family=binomial)
reg_1 = gam(Y ~ s(X1,X2), data=D_SXY_1, family=binomial)

logistique_reg = function(x1,x2,s){
  nd = data.frame(X1=x1,X2=x2)
  p = predict(reg_0, newdata=nd, type="response")*(s==0)+
    predict(reg_1, newdata=nd, type="response")*(s==1)
  return(p)
}
logistique_reg(0,0,0)
logistique_reg(0,0,1)

# transport
D_SXY0 = D_SXY[D_SXY$S == 0,]
D_SXY1 = D_SXY[D_SXY$S == 1,]

# Notre individu est dans le groupe 0 initialement
a10 = -2 # x1
a20 = .5 # x2

#### variable 1

v1 = "X1"
bdw1 = 0.7
d0x1 = function(x) density(D_SXY0[,v1],bw = bdw1,
                           weights=rep(1,length(D_SXY0[,v1]))/length(D_SXY0[,v1]),
                           from=x, to=x,n=1)$y
d1x1 = function(x) density(D_SXY1[,v1],bw = bdw1,
                           weights=rep(1,length(D_SXY1[,v1]))/length(D_SXY1[,v1]),
                           from=x, to=x,n=1)$y

FD0x1 = function(x) sum(Vectorize(d0x1)(seq(-10,x,by=.0025)))/sum(Vectorize(d0x1)(seq(-10,10,by=.0025)))
FD1x1 = function(x) sum(Vectorize(d1x1)(seq(-10,x,by=.0025)))/sum(Vectorize(d1x1)(seq(-10,10,by=.0025)))

inverse = function (f, lower = -100, upper = 100) {
  function (y) as.numeric(uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1])
}
QD1x1 = inverse(FD1x1, -10, 10)

T1 = function(x) as.numeric(QD1x1(FD0x1(x)))

### variable 2

Ta10 = T1(a10) # Transport de X1 = a10 (du groupe 0) dans le groupe 1
Ta10

v2 = "X2"
bdw2 = 0.7
# Valeurs de densité d'une loi normale de moyenne a10 et d'écart-type bdw1
# évaluée aux points D_SXY0[,v1] qui sont les valeurs de X1 du groupe 0
w0 = dnorm(D_SXY0[,v1], a10, bdw1)
# Valeurs de densité d'une loi normale de moyenne T(a10) et d'écart-type bdw1
# évaluée aux points D_SXY0[,v1] qui sont les valeurs de X1 du groupe 0
w1 = dnorm(D_SXY0[,v1], as.numeric(Ta10), bdw1) # pourquoi ce n'est pas D_SXY1
w0 = w0/sum(w0)
w1 = w1/sum(w1)
d0x2 = function(x) density(D_SXY0[,v2],bw = bdw2,
                           weights=w0, # poids plus grands autour de a10
                           from=x, to=x,n=1)$y
d1x2 = function(x) density(D_SXY1[,v2],bw = bdw2,
                           weights=w1, # poids plus grands autour de T(a10)
                           from=x, to=x,n=1)$y

FD0x2 = function(x) sum(Vectorize(d0x2)(seq(-10,x,by=.0025)))/sum(Vectorize(d0x2)(seq(-10,10,by=.0025)))
FD1x2 = function(x) sum(Vectorize(d1x2)(seq(-10,x,by=.0025)))/sum(Vectorize(d1x2)(seq(-10,10,by=.0025)))

QD1x2 = inverse(FD1x2, -10, 10)

T2 = function(x) as.numeric(QD1x2(FD0x2(x)))

Ta20 = T2(a20)


start_a = c(a10,a20,0)
end_a = c(Ta10,Ta20,1)

logistique_reg(a10,a20,0)
logistique_reg(a10,a20,1)
logistique_reg(Ta10,a20,0)
logistique_reg(Ta10,Ta20,1)