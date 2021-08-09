# Packages
library(zoo); library(sandwich); library(gmm); library(data.table); library(stargazer)
library(ggplot2); library(ggthemes)
# Data set contains daily returns of twenty selected stocks from January 1993 to February 2009, the risk-free rate and the three factors of Fama and French 
data(Finance)
df<-Finance
head(df)
setDT(df, keep.rownames = "Date")[]
df<-df[,c(1,22,23,2,3,4,5)]
#1####
# Portafolio con 25% de retorno de mercado, 25%WMK, 20%UIS, 20%ORB y 10%MAT.
pf<-df[,c(3:7)]
weights1<-rbind(.25, .25, .2, .2, .1)
pf_m<-data.table(as.matrix(pf)%*%weights1)
pf_m<-data.table(df[,1], er = pf_m, rm = df$rm, rf = df$rf)

TSR<- lm(er.V1-rf~ I(rm-rf), data = pf_m)
stargazer(TSR, type = 'text')

ggplot(pf_m, aes(I(rm-rf),er.V1-rf))+
  geom_point()+geom_abline(slope = coef(TSR)[[2]], intercept = coef(TSR)[[1]], color="firebrick")+
  theme_minimal() +
  ggtitle("") + xlab("Exceso de retorno esperado del mercado") + ylab("Exceso de retorno esperado")



#2####
