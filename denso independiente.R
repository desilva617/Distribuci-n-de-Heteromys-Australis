##5 Ejercicios 2
##Crecimiento geométrico
##En los años 1996 a 2005, los tamaños de población de lirio son:
years<-c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005)
N<-c(150, 100, 125, 200, 225, 150, 100, 175, 100, 150)
##grafica de poblacion de lirio en el tiempo
plot(N, main="Tamaño de población de lirio a través del tiempo", type="b", pch=19)
##grafica de lambda de cada año
obs.R <- N[-1]/N[-length(N)]
plot(years[-1], obs.R, pch=19)
abline(h = 1, lty = 3)
##media artimetica
lamM <- N[2:10]/N[1:9]
rM <- log(lamM)
mean(rM)
##media geometrica de las tasas de crecimiento (r)
lamM <- N[2:10]/N[1:9]
lamM
rM <- log(lamM)
rMx <- prod(rM)^1/9
rMx
##Hacemos una función de crecimiento exponencial
crecE <- function(N0,r,t ){
  N <- N0*exp(r*t)
  return(N)
}
##¿Cuál sería el tamaño de la población se espera en 2025? 
##¿Cuál sería el tamaño estimado de la población, 
##si ha utilizado la media inapropiada?
Nt29<-150*2.718^(rMx*29)
Nt29
marCrecE <- crecE(N0=150, r = rMx, t = 1:29)
plot(marCrecE, type="l")
##desarrollamos 1000 simulaciones con la funcin popsim
tiempo<- 14
set.seed(12)
sim.Rs <- sample(x = obs.R, size = tiempo, 
                 replace = TRUE)
sim1 <- numeric(tiempo + 1)
sim1[1] <- N[years == max(years)]
for (t in 1:tiempo) { 
  sim1[t + 1] <- sim1[t] * sim.Rs[t] }
sims <- 10
sim.RM <- matrix(sample(obs.R, sims * tiempo, 
                        replace = TRUE), 
                 nrow = tiempo, ncol = sims)

sim1[1] <- N[years == max(years)]
outmat <- sapply(1:sims, function(i) { 
  for (t in 1:tiempo) sim1[t + 1] <- 
      sim1[t] * sim.RM[t, i]
  sim1
})
##1000 simulaciones
PopSim <- function(Rs, N0, tiempo = 14, sims = 10) { 
  sim.RM = matrix(sample(Rs, size = sims * tiempo, replace = TRUE), 
                  nrow = tiempo, ncol = sims)
  output <- numeric(tiempo + 1) 
  output[1] <- N0 
  outmat <- sapply(1:sims, function(i) {
    for (t in 1:tiempo) output[t + 1] <- round(output[t] * 
                                                sim.RM[t, i], 0)
    output
  })  
  return(outmat) 
}
matplot(0:tiempo, outmat, type = "l", log = "y") 
par(mfcol=c(2,1), mar=c(2,4,1,1))
output <- PopSim(Rs = obs.R, N0 = 13, sims = 1000)
matplot(output/1000, type="l", ylab="Número/1000")
matplot(output/1000, type="l", ylim=c(0,10), ylab="Número/1000")

##Crecimiento de la población Humana
library(readxl)
poblacion <- read_excel("poblacion.xlsx")
View(poblacion)
str(poblacion)
##Obtenga la tasa intrínseca de crecimiento
#obtenemos lambda
lamM <- poblacion$poblacion[2:59]/poblacion$poblacion[1:58]
lamM
#convertimos lambda en r
rM <- log(lamM)
rM
mean(rM)
#obtenemos la media aritmética de r
rMx <- prod(rM)^1/58
rMx
#proyectamos la población
##Hacemos una función de crecimiento exponencial
crecE <- function(N0,r,t ){
  N <- N0*exp(r*t)
  return(N)
}
marCrecE <- crecE(N0=4543666, r = rMx, t = 1:90)
plot(marCrecE, type="l")
plot(poblacion$tiempo, poblacion$poblacion,type = "o")

##Representa gráficamente el modelo del tamaño de la población
#humana de la fecha más antigua
#que dispongas a 2050.
years<- 90
set.seed(12)
obs.R <- poblacion$poblacion[-1]/poblacion$poblacion[-length(poblacion$poblacion)]
sim.Rs <- sample(x = obs.R, size = years, 
                 replace = TRUE)
sim1 <- numeric(years + 1)
sim1[1] <- poblacion$poblacion[poblacion$tiempo == max(poblacion$tiempo)]
for (t in 1:years) { 
  sim1[t + 1] <- sim1[t] * sim.Rs[t] }
plot(0:years, sim1, type="l", xlab="1960-2050",ylab = "tamaño de población")
which(poblacion$poblacion>poblacion$poblacion[1]*2)[51]
abline(26,51)
?abline
Nt58<-4543666*2.718^(r = 0.02277295*26)
Nt58
which(poblacion$poblacion>poblacion$poblacion[1]*2)[1]
crecE <- function(N0,r,t ){
  N <- N0*exp(r*t)
  return(N)
}
marCrecE <- crecE(N0=4543666, r = 0.02277295, t = 90)
plot(marCrecE, type="l", xlab="1960-2050",ylab = "tamaño de población")
abline(v=26)
CRE<-c(4543666,4674172,4809201,4948986,5093854)
tasa<-poblacion$poblacion[2:5]/poblacion$poblacion[1:4] #1.028723 1.028888 1.029066 1.029272
tasa #1.029
log(1.029) #0.02858746=r


rd=0.02277295*100
rd
t=70/2.277295
t
dup

?which

marCrecE <- crecE(N0=4543666, r = 0.02277295, t = 1:90)
plot(marCrecE, type="l",xlab="1960-2050",ylab = "tamaño de población")
