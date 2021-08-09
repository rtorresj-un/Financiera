# Packages
library(zoo); library(sandwich); library(gmm); library(data.table)
# Data set contains daily returns of twenty selected stocks from January 1993 to February 2009, the risk-free rate and the three factors of Fama and French 
data(Finance)
df<-Finance
head(df)
setDT(df, keep.rownames = "Date")[]

#1####
# Portafolio con 25% de retorno de mercado, 25%WMK, 20%UIS, 20%ORB y 10%MAT.

weights<-c(.25, .25, .2, .2, .1)



#2####
