# Packages
library(zoo); library(sandwich); library(gmm); library(data.table); library(stargazer)
library(ggplot2); library(ggthemes); library(PerformanceAnalytics); library(dplyr); library(plm)
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
#Regresión de serie de tiempo
TSR<- lm(er.V1-rf~ I(rm-rf), data = pf_m)
summary(TSR)
stargazer(TSR, type = 'text') #beta significativo y alpha no significativo.

ggplot(pf_m, aes(x = I(rm-rf),y = er.V1-rf))+
  geom_point()+geom_abline(slope = coef(TSR)[[2]], intercept = coef(TSR)[[1]], color="firebrick")+
  theme_minimal() +
  ggtitle("") + xlab("Exceso de retorno esperado del mercado") + ylab("Exceso de retorno esperado")

#Regresiones de corte transversal
df_2<-df
df_2[,1]=year(df$Date)

GroupData <- df_2 %>%
  group_by(Date)

group_er<-data.table(as.matrix(GroupData[,3:7])%*%weights1)
GroupData<-GroupData[,1:3]
GroupData["er"]<-group_er

er_bar<- NULL
beta_t<-NULL
for(i in 1:16){
    er_temp<-filter(GroupData, Date==1992+i)[,4]
    er_bar[i]<-mean(as.matrix(er_temp))
    temp_reg<-lm(er-rf ~ I(rm-rf), data = GroupData%>%filter(Date==1992+i))
    beta_t[i]<-coef(temp_reg)[[2]]
}

data_2step<-data.table(Date= seq(1993,1, to = 2009), er_bar,beta_t)
data_2step$Date<-as.factor(data_2step$Date)
reg_2step<-lm(er_bar~beta_t, data = data_2step)
summary(reg_2step)
stargazer(reg_2step)

ggplot(data_2step, aes(x = beta_t,y = er_bar, label=Date))+
  geom_point()+geom_abline(slope = coef(reg_2step)[[2]], intercept = coef(reg_2step)[[1]], color="firebrick")+
  theme_minimal() + geom_text(aes(label=Date),hjust=0, vjust=0) +
  ggtitle("") + xlab("Beta estimado") + ylab("Retorno promedio")

#2####




