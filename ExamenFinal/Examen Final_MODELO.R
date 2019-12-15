#directorio <- '/home/alfie-gonzalez/Documentos/Maestría/Primer Semestre/Modelos Lineales Generalizados/'
setwd(directorio)

library(tidyverse)
library(R2OpenBUGS)

prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

datos <- read.csv('master.csv', header = TRUE) %>% 
  mutate(suicide_rate = suicides_no/population)

colnames(datos)[c(10,11)] <- c('gdp_for_year', 
                                              'gdp_per_capita')

datos %>% dim
datos %>% complete.cases() %>% sum()
datos %>% select(-HDI.for.year) %>% complete.cases() %>% sum()

y <- datos$suicides_no
n <- length(y)
x <- datos %>% select(-c(suicides_no, suicides.100k.pop, suicide_rate, 
                         HDI.for.year))
x <- datos %>% select(-c(suicides_no, suicides.100k.pop, suicide_rate, 
                         HDI.for.year, year, country.year))

x %>% names()
x %>% names() %>% length()

data.frame(1:length(colnames(x)), colnames(x))

data<-list("n"=n,"y"=y,"x"=as.matrix(x))
inits<-function(){list(country=0,sex=0,age=0,population=0,
                       gdp_for_year=0,gdp_per_capita=0,
                       generation=0,yf=rep(0,n),theta=rep(0,n),phy=1)}
parameters<-c("alpha.est","country.est","sex.est","age","population",
              "gdp_for_year","gdp_per_capita","generation.est",
              "theta","yf","phy")

sim<-bugs(data,inits,parameters,model.file="Examen Final.txt",
               n.iter=1000,n.chains=2,n.burnin=100)

library(BRugs)
archivo <- "Examen Final.txt"
modelCheck(archivo)
