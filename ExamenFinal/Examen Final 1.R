directorio <- '/home/alfie-gonzalez/Documentos/Maestría/Primer Semestre/Modelos Lineales Generalizados/'
setwd(directorio)

library(tidyverse)
library(R2OpenBUGS)

prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

datos <- read_csv('mexico.csv') %>% 
  mutate(suicide_rate = suicides_no/population) %>% 
  mutate_at(vars(public_health_exp_gdp,starts_with("share")), function(x) x/100)

datos %>% names

datos %>% dim
datos %>% complete.cases() %>% sum()

y <- datos$suicides_no
n <- length(y)
#x <- datos %>% select(-c(suicides_no, suicides.100k.pop, suicide_rate, 
#                         HDI.for.year))

x <- datos %>% select(c(6:10,13))
x <- datos[,c(6,7)]
x <- datos %>% select(public_health_exp_gdp,
                      life_expectancy_years,
                      share_with_alcohol_disorders, 
                      share_with_mental_disorders,
                      share_with_anxiety_disorders,
                      population) %>% 
  mutate(population=population/10^6)

x %>% names()
x %>% names() %>% length()

data.frame(1:length(colnames(x)), colnames(x))

nominal_to_num <- function(x){
  n <- length(x)
  X <- data.frame(id = 1:length(unique(x)), y = unique(x))
  x1 <- numeric(n)
  for(i in 1:n){x1[i] <- X[which(x[i] == X$y),1]}
  return(x1)
}

k<-nrow(x)

data<-list("n"=n,"k"=k,"y"=y,"x"=as.matrix(x))
inits<-function(){list(alpha=0,
                       beta=rep(0,k),
                       yf=rep(0,n))}
parameters<-c("alpha",
              "beta",
              "yf")

#data<-list("n"=n,"y"=y,"x"=as.matrix(1:length(y)))
#inits<-function(){list(alpha=0,beta=0)}
#parameters<-c("alpha","beta","yf","phy")

sim<-bugs(data,inits,parameters,model.file="Examen Final 1.txt",
          n.iter=10000,n.chains=2,n.burnin=1000,n.thin=2)

library(BRugs)
archivo <- "Examen Final 1.txt"
modelCheck(archivo)

out<-sim$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
par(mfrow=c(1,1))

dic<-sim$DIC
dic

out.sum<-sim$summary

yf<-out.sum[grep("yf",rownames(out.sum)),] 

plot(1:n,yf[,1],col='red',ylim=c(0,1.1*max(c(y,yf[7]))),type='l')
segments(1:n,yf[,3],1:n,yf[,7],col='red')
lines(1:n,y,type='l')



Modelo<-"model
{
#Likelihood
for (i in 1:n) {
  y[i] ~ dpois(mu[i])
#  log(mu[i])<-alpha+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]
  log(mu[i])<-alpha+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]+
  beta[4]*x[i,4]+beta[5]*x[i,5]+beta[6]
}

#Priors 
alpha ~ dnorm(0, 0.001)
for(i in 1:k){beta[i] ~ dnorm(0, 0.001)}

#Prediction 1
for (i in 1:n) { yf[i] ~ dpois(mu[i]) }

}"

Modelo<-"model
{
#Likelihood
for (i in 1:n) {
y[i] ~ dbin(p[i],ne[i])
mu[i]<-ne[i]*p[i]
#  log(mu[i])<-alpha+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]
log(mu[i])<-alpha+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]
}

#Priors 
alpha ~ dnorm(0, 0.001)
for(i in 1:k){beta[i] ~ dnorm(0, 0.001)}

#Prediction 1
for (i in 1:n) { yf[i] ~ dpois(mu[i]) }

}"
  
archivo <- "Examen Final 1.txt"
writeLines(Modelo, con = archivo)
modelCheck(archivo)
