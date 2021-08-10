# Packages
library(zoo); library(sandwich); library(gmm); library(data.table); library(stargazer)
library(ggplot2); library(PerformanceAnalytics); library(dplyr); library(plm)
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
  xlab("Exceso de retorno esperado del mercado") + ylab("Exceso de retorno esperado")

#Regresiones de corte transversal

#cs_df<-df[,3:ncol(df)]
#cs_df<-apply(cs_df, 2, function(x) x-df$rf)
#colnames(cs_df)<- c('zm','zWMK','zUIS','zORB','zMAT')
#
#alpha_i<- NULL; alp_sd_i<-NULL
#beta_i<-NULL; bet_sd_i<-NULL
#for(j in 2:ncol(cs_df)){
#  temp_reg<-lm(cs_df[,j]~cs_df[,1])
#  alpha_i[j]<-coef(temp_reg)[[1]]
#  alp_sd_i[j]<- sqrt(diag(vcov(temp_reg)))[1]
#  beta_i[j]<-coef(temp_reg)[[2]]
#  bet_sd_i[j]<- sqrt(diag(vcov(temp_reg)))[2]
#}
#
#er_aver<-NULL
#for (k in 2:ncol(cs_df)) {
#  er_aver[k]<-mean(cs_df[,k])
#}
#
#df_2step<-na.omit(data.frame(er_aver, beta_i))
#rownames(df_2step)<-c('zWMK','zUIS','zORB','zMAT')
#regr_2step<-lm(er_aver~beta_i, data = df_2step)
#summary(regr_2step)
#stargazer(regr_2step)
#
#ggplot(df_2step, aes(x = beta_i,y = er_aver))+
#  geom_point()+#geom_abline(slope = coef(reg_2step)[[2]], intercept = coef(reg_2step)[[1]], color="firebrick")+
#  theme_minimal() + #geom_text(aes(label=Date),hjust=0, vjust=0) +
#  xlab("Beta estimado") + ylab("Retorno promedio")

df_2<-df
df_2[,1]=year(df$Date)

GroupData <- df_2[1:2000,] %>%
  group_by(Date)

er_bar<- NULL
beta_t<-NULL
for(i in 1:100){
  rand<-sample(50:99,5, replace = T)
  sum_rand<-sum(rand)
  weights_temp<-(rand/sum_rand)
  stockXw<-as.matrix(GroupData[,3:7])%*%weights_temp
  group_er<-data.table(stockXw)
  GroupData_temp<-GroupData[,1:3]
  GroupData_temp["er"]<-group_er
  er_bar[i]<-mean(as.matrix(GroupData_temp[,4]))
  temp_reg<-lm(er-rf ~ I(rm-rf), data = GroupData_temp)
  beta_t[i]<-coef(temp_reg)[[2]]
}
summary(GroupData[,3:7])
data_2step<-NULL
data_2step<-data.table(er_bar, beta_t)
reg_2step<-lm(er_bar~beta_t, data = data_2step)
summary(reg_2step)
stargazer(reg_2step)

ggplot(data_2step, aes(x = beta_t,y = er_bar))+
  geom_point()+geom_abline(slope = coef(reg_2step)[[2]], intercept = coef(reg_2step)[[1]], color="firebrick")+
  theme_minimal() +
  xlab("Beta estimado") + ylab("Retorno promedio")

#2####




