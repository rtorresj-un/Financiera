# Packages
library(zoo); library(sandwich); library(gmm); library(data.table); library(stargazer)
library(ggplot2); library(PerformanceAnalytics); library(dplyr); library(plm); library(xts)
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
  rand<-rnorm(5,5,5)
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
data_2step<-NULL
data_2step<-data.table(er_bar, beta_t)
reg_2step<-lm(er_bar~beta_t, data = data_2step)

rf<-GroupData_temp$rf
reg_22step<-lm(er_bar-rf~beta_t, data = data_2step)
summary(reg_2step)
stargazer(reg_2step)

ggplot(data_2step, aes(x = beta_t,y = er_bar))+
  geom_point()+geom_abline(slope = coef(reg_2step)[[2]], intercept = coef(reg_2step)[[1]], color="firebrick")+
  theme_minimal() +
  xlab("Beta estimado") + ylab("Retorno promedio")

df_2<-df
df_2[,1]=year(df$Date)

GroupData <- df_2 %>%
  group_by(Date)
group_er<-data.table(as.matrix(GroupData[,3:7])%*%weights1)
GroupData<-GroupData[,1:3]
GroupData["er"]<-group_er

er_bar<- NULL
er_rm<-NULL
er_f<- NULL
beta_t<-NULL
for(i in 1:16){
  er_temp<-filter(GroupData, Date==1992+i)[,4]
  er_temp_m<-filter(GroupData, Date==1992+i)[,3]
  er_temp_f<-filter(GroupData, Date==1992+i)[,2]
  
  er_bar[i]<-mean(as.matrix(er_temp))
  er_rm[i]<-mean(as.matrix(er_temp_m))
  er_f[i]<-mean(as.matrix(er_temp_f))
  
  temp_reg<-lm(er-rf ~ I(rm-rf), data = GroupData%>%filter(Date==1992+i))
  beta_t[i]<-coef(temp_reg)[[2]]
}

data_2step<-data.table(Date= seq(1993,1, to = 2009),er_rm,er_f, er_bar,beta_t)
data_2step$Date<-as.factor(data_2step$Date)

reg_1step<-lm(er_bar-er_f~I(er_rm-er_f), data = data_2step)
reg_2step<-lm(er_bar~beta_t, data = data_2step)

summary(reg_2step)

stargazer(reg_2step)

ggplot(data_2step, aes(x = beta_t,y = er_bar, label=Date))+
geom_point()+geom_abline(slope = coef(reg_2step)[[2]], intercept = coef(reg_2step)[[1]], color="firebrick")+
theme_minimal() + geom_text(aes(label=Date),hjust=0, vjust=0) +
ggtitle("") + xlab("Beta estimado") + ylab("Retorno promedio")

#3####
df_3<-xts(Finance[,c(21,22,2,3,4,5)], order.by = as.Date(row.names(Finance), format = "%Y-%m-%d"))
df_3<-period.apply(df_3,endpoints(df_3,on = 'years'),mean)

er_bar3<- NULL
beta_t3<-NULL
for(i in 1:100){
  rand<-rnorm(5,5,5)
  sum_rand<-sum(rand)
  weights_temp<-(rand/sum_rand)
  stockXw<-as.matrix(df_3[,2:6])%*%weights_temp
  group_er<-xts(stockXw, order.by = index(df_3))
  GroupData_temp<-df_3[,1:2]
  GroupData_temp<-merge(GroupData_temp,group_er)
  er_bar3[i]<-mean(GroupData_temp[,3])
  temp_reg<-lm(group_er-rf ~ I(rm-rf), data = GroupData_temp)
  beta_t3[i]<-coef(temp_reg)[[2]]
}

data_2step3<-data.table(er_bar3,beta_t3)
data_2step3$Date<-as.factor(data_2step3$Date)
reg_2step3<-lm(er_bar3~beta_t3, data = data_2step3)

summary(reg_2step3)

stargazer(reg_2step3)

ggplot(data_2step3, aes(x = beta_t3,y = er_bar3))+
  geom_point()+geom_abline(slope = coef(reg_2step3)[[2]], intercept = coef(reg_2step3)[[1]], color="firebrick")+
  theme_minimal() + xlab("Beta estimado") + ylab("Retorno promedio")

#df_annual<-NULL
#for(i in 1993:2009){
#  df_annual_temp<-NULL
#  df_annual_temp<-Return.portfolio(df_3[as.character(c(i)),], weights = weights, rebalance_on="year")
#  df_annual<-rbind(df_annual,df_annual_temp)
#}

#weights<-NULL
#for (i in 1992:2008) {
#  weights_temp<-NULL
#  weights_temp = xts(matrix(c(.25, .25, .2, .2, .1),1), as.Date(paste0(i, "-12-31")))
#  weights<-rbind(weights,weights_temp)
#}
#colnames(weights) = colnames(df_3[,-1])


