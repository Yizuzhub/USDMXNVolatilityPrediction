library(lubridate)
library(xts)
library(purrr)
library(ggplot2)
library(MLmetrics)
library(scales)
library(ggpubr)
library(gridExtra)
library(parallel)
library(forecast)
library(rugarch)
library(tseries)
library(FinTS)
library(aTSA)

library(keras)
library(tensorflow)
library(tfruns)

#use_implementation(implementation = c("keras"))
#use_bagarchckend(backend = c("tensorflow"))
#set.seed(10)
#use_session_with_seed(10, disable_gpu = T, disable_parallel_cpu = F)
setwd("C:/Users/jo/Desktop/tesis/Codigos")

#### Funciones ####

source(file="functions.R")

#### Variables ####

par(cex.axis=1.2,cex.main=1.5,cex.lab=1.4)
#par(cex.axis=1.8,cex.main=2,cex.lab=1.8)
c.blue<-"#1121d6" #azul
c.purple<-"#7300e6" #morado
c.lpurple<-"#9933ff"
c.gold<-"#999900"
c.llblue<-"#14ceb5"
c.green<-"#00b300" #verde
c.lblue<-"#1f8ac4" #light blue
c.orange<-"#f96c00" #orange
c.red<-"#ff0f0f" #red
c1<- c.blue
c11<-"#271fba" #"#0097f3"   # "#5061fc" #azul
c12<-"#002f7a" #azul
c2<-c.orange #"#ffa200"  #"#ff8223" #naranja
c3<-c.green  #"#03c924" #"#03bc28" #verde
c4<-c.purple #morado
l1 <- 1
l2 <- 2
ggt<-theme_bw()+theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                      axis.text=element_text(size=14),
                      axis.title.x = element_text(size = rel(1.5), angle = 0),
                      axis.title.y =  element_text(size = rel(1.5), angle = 90),
                      plot.title = element_text(size = rel(1.5), hjust = 0.5, angle = 0, face="bold"))

garch.models<-list()
garch.coefs<-list()
garch.crit<-list()
garch.aic<-c()
garch.bic<-c()
garch.error<-c()
garch.sigma<-c()
egarch.models<-list()
egarch.coefs<-list()
egarch.crit<-list()
egarch.aic<-c()
egarch.bic<-c()
egarch.error<-c()
egarch.sigma<-c()
models.names<-c()
means<-c()
volatilities<-c()
xi<-c()
lstm.error<-c()
gru.error<-c()
garch.rnn.error<-c()
egarch.rnn.error<-c()

#### Analisis ####

data <- read.csv("USD-MXN.csv",colClasses=c("Fecha"="character"))
data$Fecha<-as.Date(data[,1],format = "%d/%m/%Y")
length(data[,2])
data<-data[diff(data$Cierre)!=0,]
data<-data[which(wday(data$Fecha)!=7),]
length(data[,2])
datazoo <- with(data, zoo(data$Cierre, data$Fecha))
db<-as.ts(data$Cierre)
summary(db)

p<-ggplot(data, aes(x = data$Fecha, y = data$Cierre))+
  ggt + geom_line(color = c1, size = 1.25,lty=1) +
  scale_color_manual(values = c1) +
  labs(title="USD/MXN diario al cierre", y="USD/MXN", x="Tiempo")+
  scale_y_continuous(breaks=seq(8,22,3))+
  scale_x_date(date_breaks = "3 years",
               labels=date_format("%Y"),
               limits = as.Date(c(min(data$Fecha),max(data$Fecha))))
p
ggexport(p, filename = "USD-MXN.pdf",width=10,height=5)
#plot(datazoo,lwd=l2,col=c1,ylab="USD MXN", xlab="DIAS",main="USD/MXN diario al cierre")

#### Rendimientos ####

rend<-as.ts(rendiments(db))
db.rend<-xts(rend,order.by = data$Fecha[-1])

p<-ggplot(db.rend, aes(x = index(db.rend), y = db.rend))+
  ggt + geom_line(color = c1, size = 1.25,lty=1) +
  scale_color_manual(values = c1) +
  labs(title="Rendimientos diarios del USD/MXN al cierre", y="USD/MXN", x="Tiempo")+
  scale_y_continuous(breaks=seq(-0.07,0.09,0.04))+
  scale_x_date(date_breaks = "3 years",
               labels=date_format("%Y"),
               limits = as.Date(c(min(data$Fecha),max(data$Fecha))))
p
ggexport(p, filename = "Rend-USD-MXN.pdf",width=10,height=5)

rend<-rend[which(rend!=0)]
tsdisplay(abs(diff(rend)),lwd=l1,col=c1,main="Rendimientos Diarios")
tsdisplay(diff(rend)^2,lwd=l1,col=c1, main="Rendimientos Cuadraticos Diarios")

summary(rend)
hist(rend,col = c1,probability = T,breaks = 7.5e1,ylab="Densidad", main = "Rendimientos vs Normal",xlab="Rendimientos")
t1<-mean(rend)
t2<-sd(rend)
lines(density(rnorm(1e4,mean=t1,sd=t2)),col = c2,lw=4)
nortest::ad.test(rend)

# Num. rend > umbral
cbind(seq(0,0.05,0.01),sapply(0:5,function(x) length(rend[abs(rend)>0.01*x])))

alpha<-c(0.05,0.025,0.95,0.975,0.995,0.9975)
cbind(sapply(alpha,function(x) quantile(abs(rend),x)),#cuantiles
      sapply(alpha,function(x) sum(abs(rend)>quantile(abs(rend),x))),#No. rend>cuantil
      sapply(alpha,function(x) sum(abs(rend)>quantile(abs(rend),x))/length(rend))#porcentaje rend>cuantil
      )
umbral<-1.5e-2

#umbral<-5.6e-3 #plot(rend,lwd=l1,col=ifelse(abs(rend)<umbral,"blue","red"))
rend.umbral<-rend[which(abs(rend)<umbral)]
1-length(rend.umbral)/length(rend)

tsdisplay(rend.umbral,lwd=l1,col=c1,main="Rendimientos Diarios sin atípicos")
tsdisplay(abs(rend.umbral),lwd=l1,col=c1,main="Rendimientos Absolutos Diarios sin atípicos")
tsdisplay(rend.umbral^2,lwd=l1,col=c1, main="Rendimientos Cuadráticos Diarios sin atípicos")

tsdisplay(rend,lwd=l1,col=c1,main="Rendimientos Diarios")
tsdisplay(abs(rend),lwd=l1,col=c1,main="Rendimientos Absolutos Diarios")
tsdisplay(rend^2,lwd=l1,col=c1, main="Rendimientos Cuadráticos Diarios")

windows<-c(5,10,22)
for(i in 1:length(windows)){
  volatilities[[i]]<-as.ts(vol.estimate(rend,windows[i]))
  means[[i]]<-as.ts(mean.estimate(rend,windows[i]))
}

alpha<-c(0.005,0.025,0.95,0.975,0.995,0.9975)
cbind(sapply(alpha,function(x) quantile(abs(volatilities[[1]]),x)),
      sapply(alpha,function(x) sum(abs(volatilities[[1]])>quantile(abs(volatilities[[1]]),x))),
      sapply(alpha,function(x) sum(abs(volatilities[[1]])>quantile(abs(volatilities[[1]]),x))/length(volatilities[[1]]))
      )

pdf("RendDaily.pdf")
tsdisplay(rend,lwd=l1,col=c1,main="Rendimientos Diarios")
dev.off()
pdf("Rend2Daily.pdf")
tsdisplay(rend^2,lwd=l1,col=c1, main="Rendimientos Cuadráticos Diarios")
dev.off()

pdf("RendDailyUmbral.pdf")
tsdisplay(rend.umbral,lwd=l1,col=c1,main="Rendimientos Diarios")
dev.off()
pdf("Rend2DailyUmbral.pdf")
tsdisplay(rend.umbral^2,lwd=l1,col=c1, main="Rendimientos Cuadráticos Diarios")
dev.off()

adf.test(rend)
#Los rendimientos son estacionarios

temp<-matrix(0,ncol = 2,nrow = 20)
for(i in 1:20){
  temp[i,]<-c(i,Box.test(rend, lag = i, type = "Ljung")$p.value)
}
temp[which(temp[,2]>=0.05),]

m<-arima(rend,order=c(1,0,0))
temp<-matrix(0,ncol = 2,nrow = 30)
for(i in 1:30){
  temp[i,]<-c(i,Box.test(a@fit$residuals, lag = i, type = "Ljung")$p.value)
}
print(temp)
temp[which(temp[,2]>=0.05),]
#Los rendimientos no son independientes

#### Acercamiento ####

(model<-auto.arima(rend))
(model1<-arima(rend,order=c(1,0,0),include.mean = F))
(model2<-arima(rend,order=c(0,0,1),include.mean = F))
(model3<-arima(rend,order=c(1,0,1),include.mean = F))

par(mfrow=c(1,2))
acf(model1$residuals,lwd=l1,col=c1, main="Residuales AR(1)")
pacf(model1$residuals,lwd=l1,col=c1, main="Residuales AR(1)")
acf(model1$residuals^2,lwd=l1,col=c1, main="Residuales Cuadráticos AR(1)")
pacf(model1$residuals^2,lwd=l1,col=c1, main="Residuales Cuadráticos AR(1)")
acf(abs(model1$residuals),lwd=l1,col=c1, main="Residuales Absolutos AR(1)")
pacf(abs(model1$residuals),lwd=l1,col=c1, main="Residuales Absolutos AR(1)")

tsdisplay(model1$residuals,lwd=l1,col=c1, main="Residuales AR(1)")
tsdisplay(model2$residuals,lwd=l1,col=c1, main="Residuales MA(1)")
tsdisplay(model3$residuals,lwd=l1,col=c1, main="Residuales ARMA(1,1)")

tsdisplay(model$residuals,lwd=l1,col=c1, main="Residuales ARMA(2,3) con Tendencia")
tsdisplay(model$residuals^2,lwd=l1,col=c1, main="Residuales Cuadráticos ARMA(2,3) con Tendencia")
tsdisplay(abs(model$residuals),lwd=l1,col=c1, main="Residuales Absolutos ARMA(2,3) con Tendencia")

par(mfrow=c(1,2))
acf(abs(model$residuals),lwd=l1,col=c1, main="Residuales Absolutos ARMA(2,3)")
pacf(abs(model$residuals),lwd=l1,col=c1, main="Residuales Absolutos ARMA(2,3)")

acf(model$residuals^2,lwd=l1,col=c1, main="Residuales Cuadráticos ARMA(2,3)")
pacf(model$residuals^2,lwd=l1,col=c1, main="Residuales Cuadráticos ARMA(2,3)")

tsdisplay(model1$residuals,lwd=l1,col=c1, main="Residuales AR(1)")
tsdisplay(model2$residuals,lwd=l1,col=c1, main="Residuales MA(1)")
tsdisplay(model3$residuals,lwd=l1,col=c1, main="Residuales ARMA(1,1)")

temp<-matrix(0,ncol = 2,nrow = 35)
for(i in 1:35){
  temp[i,]<-c(i,Box.test(a@fit$residuals, lag = i, type = "Ljung")$p.value)
}
temp[which(temp[,2]>=0.05),]
#Los residuales son independientes

temp<-matrix(0,ncol = 2,nrow = 12)
for(i in 1:12){
  temp[i,]<-c(i,ArchTest(model3$residuals^2, lag = i)$p.value)
}
temp[which(temp[,2]<0.05),]
#Hay efectos efectos ARCH

nortest::ad.test(model$residuals)
#Los residuales no siguen una distribucion normal

adf.test(rend)
#El proceso no tiene raíces unitarias

#### Graficas ####

source(file="plots.R")

#### Particion de la muestra ####

timesteps <- 3
outputs <- 1
portion.val<-3/10
portion.test<-3/10
batch_size <- 84
source("partition.R",echo=F)

#### GARCH #####

#### Ajuste ####

#(garch, arma)#
garch.models<-list()
garch.order<-list()
arma.order<-list()
garch.crit<-list()
garch.aic<-list()
garch.bic<-list()
key<-1
set.seed(10)
for(i in 1:2){
  for(j in 1:2){
    for(h in 0:1){
      for(t in 0:1){
        if(h+t==0) next
        garch.spec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(i,j)),
                                 mean.model=list(armaOrder=c(h,t), include.mean=F),distribution="sged")
        garch.models[[key]]<-ugarchfit(garch.spec, rend[train.indexes],solver = "hybrid")
        garch.coefs<-garch.models[[key]]@fit$matcoef
        garch.crit[[key]]<-infocriteria(garch.models[[key]])
        garch.aic[[key]]<-garch.crit[[key]][1]
        garch.bic[[key]]<-garch.crit[[key]][2]
        garch.order[[key]]<-c(i,j)
        arma.order[[key]]<-c(h,t)
        if(length(garch.coefs[which(garch.coefs[,4]<0.05),4])>=i+j+h+t+3){
          print(paste(key,"(",i,j,") - (",h,t,")"))
        }
        key<-key+1
      }
    }
  }
}

h<-2
t<-3
set.seed(10)
for(i in 1:2){
  for(j in 1:2){
    garch.spec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(i,j)),
                             mean.model=list(armaOrder=c(h,t), include.mean=F),distribution="sged")
    garch.models[[key]]<-ugarchfit(garch.spec, rend[train.indexes],solver = "hybrid")
    garch.coefs<-garch.models[[key]]@fit$matcoef
    garch.crit[[key]]<-infocriteria(garch.models[[key]])
    garch.aic[[key]]<-garch.crit[[key]][1]
    garch.bic[[key]]<-garch.crit[[key]][2]
    garch.order[[key]]<-c(i,j)
    arma.order[[key]]<-c(h,t)
    if(length(garch.coefs[which(garch.coefs[,4]<0.05),4])>=i+j+h+t+3){
      print(paste(key,"(",i,j,") - (",h,t,")"))
    }
    key<-key+1
  }
}

key<-key-1

#### Seleccion ####

k<-windows[1]:(train.uindex.vec[[1]])
k2<-1:(train.uindex.vec[[1]]-windows[1]+1)
k3<-train.indexes
temp<-as.data.frame(cbind(1:length(garch.aic),garch.aic,garch.bic,
                    sapply(unlist(garch.models),function(x)
                          MAE(sigma(x)[k],volatilities[[1]][k2])),
                    sapply(unlist(garch.models),function(x)
                          MAPE(y_pred=sigma(x)[k],y_true=volatilities[[1]][k2]))))
colnames(temp)<-c("key","AIC","BIC","MAE", "MAPE")
a<-temp[order(unlist(temp$AIC)),]
b<-temp[order(unlist(temp$BIC)),]
print(head(a,10))
print(head(b,10))
print(head(temp[order(unlist(temp$MAE)),],10))

selected<-sort(c(1,2,3))
cbind(selected,
      paste(sapply(selected, function(x) arma.order[[x]][1]),"-",
            sapply(selected, function(x) arma.order[[x]][2])),
      paste(sapply(selected, function(x) garch.order[[x]][2]),"-",
            sapply(selected, function(x) garch.order[[x]][1])))
temp[selected,]
for(i in selected){
  print(garch.models[[i]]@fit$matcoef)
}

# Validacion
garch.error<-array(NA,dim=c(length(windows),length(selected),4))
garch.error.val<-list()
set.seed(10)
for(w in 1:length(windows)){
  for(i in 1:length(selected)){
    sigmas<-c()
    j<-selected[i]
    sigmas<-prediction.sigma(garch.models[[j]],val=T,w=w,t=length(val.indexes.vec[[w]]))
    garch.error[w,i,1]<-j
    garch.error[w,i,2]<-MAE(sigmas,volatilities[[w]][val.indexes.vec[[w]]])
    garch.error[w,i,3]<-RMSE(sigmas,volatilities[[w]][val.indexes.vec[[w]]])
    garch.error[w,i,4]<-MAPE(y_pred=sigmas,y_true=volatilities[[w]][val.indexes.vec[[w]]])
  }
  garch.error.val[[w]]<-as.data.frame(garch.error[w,,])
  colnames(garch.error.val[[w]])<-c("Selected","MAE","RMSE","MAPE")
  print(paste("Window:",windows[w]))
  print(head(garch.error.val[[w]][order(garch.error.val[[w]]$MAE),],10))
}

#### Modelo seleccionado ####

garch.model<-garch.models[[1]]#5
garch.model@fit$matcoef

#### Seleccion - Backtesting* ####

cl = makePSOCKcluster(5)
spec = getspec(garch.models[[1]])
roll = ugarchroll(spec, rend, forecast.length = 500, refit.every = 22, refit.window = 'moving', window.size = 100, calculate.VaR = T, keep.coef = TRUE, cluster = cl, solver="hybrid")
report(roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(garch.models[[1]])


s<-25
spec <- getspec(egarch.model)
specf <- spec
setfixed(specf) <- as.list(coef(fit))
filt = ugarchfilter(specf, data = rend[train.indexes.ts], n.old = train.uindex.ts - s)
forc1 = ugarchforecast(specf, n.ahead = 1, n.roll = s, data = rend[train.indexes.ts], out.sample = s)
filts = tail(sigma(filt), s)
colnames(filts) = 'filter'
forcs1 = xts(sigma(forc1)[1, ], move(as.Date(names(sigma(forc1)[1, ])), by = 1))
colnames(forcs1) = 'fit2forecast'
ftest = cbind(filts, forcs1, tail(volatilities[[1]][train.indexes],s+1))
print(round(ftest, 6))

set.seed(55)
forc4 = ugarchforecast(fit, n.ahead = s, n.sim = 1e5)
sigmaDF = forc4@forecast$sigmaFor
meansig = sqrt(exp(rowMeans(log(sigmaDF^2))))
boxplot(t(sigmaDF), main = '25-ahead volatility forecast (GARCH)', col = 'orange')
points(as.numeric(meansig), col = 'green')

T = train.uindex.ts - s
sim1 = ugarchsim(fit, n.sim = 1000, m.sim = 1, n.start = 0, startMethod = 'sample', rseed = 12)
print(cbind(head(sigma(sim1),s), tail(volatilities[[1]][1:train.uindex],s)))

#### Predicciones - modelo GARCH####

# Prueba
garch.error<-matrix(NA,nrow=length(windows),ncol = 4)
for(w in 1:length(windows)){
  sigmas<-c()
  spec = getspec(garch.model)
  sigmas<-prediction.sigma(garch.model,val=F,w=w,length(test.indexes.vec[[w]]))
  garch.error[w,1]<-windows[w]
  garch.error[w,2]<-MAE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  garch.error[w,3]<-RMSE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  garch.error[w,4]<-MAPE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
}
garch.error.test<-as.data.frame(garch.error)
colnames(garch.error.test)<-c("Ventana","MAE","RMSE","MAPE")
garch.error.test

# 1d
garch.error<-matrix(NA,nrow=length(windows),ncol = 4)
for(w in 1:length(windows)){
  sigmas<-c()
  spec = getspec(garch.model)
  sigmas<-prediction.sigma.1d(garch.model,val=F,w=w,length(test.indexes.vec[[w]]))
  garch.error[w,1]<-windows[w]
  garch.error[w,2]<-MAE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  garch.error[w,3]<-RMSE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  garch.error[w,4]<-MAPE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
}
garch.error.test<-as.data.frame(garch.error)
colnames(garch.error.test)<-c("Ventana","MAE","RMSE","MAPE")
garch.error.test

#### EGARCH #####

#### Ajuste ####

set.seed(10)
arma.order<-list()
egarch.order<-list()
egarch.models<-list()
egarch.crit<-list()
egarch.aic<-list()
egarch.bic<-list()
candidates<-c()
candidates.key<-0
key<-1
#(garch, arma)#
for(i in 0:2){
  for(j in 1:2){
    for(h in 0:1){
      for(t in 0:1){
        if(h+t==0) next
        egarch.spec <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(i,j)),
                                  mean.model=list(armaOrder=c(h,t), include.mean=F), distribution.model = "sged")
        egarch.models[[key]]<-ugarchfit(egarch.spec, rend[train.indexes],solver = "hybrid")
        egarch.coefs<-egarch.models[[key]]@fit$matcoef
        egarch.crit[[key]]<-infocriteria(egarch.models[[key]])
        egarch.aic[[key]]<-egarch.crit[[key]][1]
        egarch.bic[[key]]<-egarch.crit[[key]][2]
        egarch.order[[key]]<-c(i,j)
        arma.order[[key]]<-c(h,t)
        if(length(egarch.coefs[which(egarch.coefs[,4]<0.05),4])>=i*2+j+h+t+3){
          candidates.key<-candidates.key+1
          candidates[candidates.key]<-key
          print(paste(key,"(",i,j,") - (",h,t,")"))
        }
        key<-key+1
      }
    }
  }
}
h<-2
t<-3
set.seed(10)
for(i in 0:2){
  for(j in 1:2){
    egarch.spec <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(i,j)),
                              mean.model=list(armaOrder=c(h,t), include.mean=F), distribution.model = "sged")
    egarch.models[[key]]<-ugarchfit(egarch.spec, rend[train.indexes],solver = "hybrid")
    egarch.coefs<-egarch.models[[key]]@fit$matcoef
    egarch.crit[[key]]<-infocriteria(egarch.models[[key]])
    egarch.aic[[key]]<-egarch.crit[[key]][1]
    egarch.bic[[key]]<-egarch.crit[[key]][2]
    egarch.order[[key]]<-c(i,j)
    arma.order[[key]]<-c(h,t)
    if(length(egarch.coefs[which(egarch.coefs[,4]<0.05),4])>=i*2+j+h+t+3){
      candidates.key<-candidates.key+1
      candidates[candidates.key]<-key
      print(paste(key,"(",i,j,") - (",h,t,")"))
    }
    key<-key+1
  }
}
key<-key-1

#### Seleccion ####

k<-windows[1]:(train.uindex.vec[[1]])
k2<-1:(train.uindex.vec[[1]]-windows[1]+1)
k3<-train.indexes
temp<-as.data.frame(cbind(1:length(egarch.aic),egarch.aic,egarch.bic,
                          sapply(unlist(egarch.models),function(x)
                            MAE(sigma(x)[k],volatilities[[1]][k2])),
                          sapply(unlist(egarch.models),function(x)
                            MAPE(y_pred=sigma(x)[k],y_true=volatilities[[1]][k2]))))
colnames(temp)<-c("key","AIC","BIC","MAE", "MAPE")
a<-temp[order(unlist(temp$AIC)),]
b<-temp[order(unlist(temp$BIC)),]
print(head(a,10))
print(head(b,10))
print(head(temp[order(unlist(temp$MAE)),],10))
candidates

selected<-sort(c(7,8,13))#c(14,13,8,7,17,15,16,21))

selected %in% candidates
cbind(selected,
      paste(sapply(selected, function(x) arma.order[[x]][1]),"-",
            sapply(selected, function(x) arma.order[[x]][2])),
      paste(sapply(selected, function(x) egarch.order[[x]][2]),"-",
            sapply(selected, function(x) egarch.order[[x]][1])))
temp[selected,]
for(i in c(14,8)){ #18,19
  print(egarch.models[[i]]@fit$matcoef)
}

# Validacion
egarch.error<-array(NA,dim=c(length(windows),length(selected),4))
egarch.error.val<-list()
set.seed(10)
for(w in 1:length(windows)){
  for(i in 1:length(selected)){
    j<-selected[i]
    sigmas-c()
    sigmas<-prediction.sigma(egarch.models[[j]],val=T,w,t=length(val.indexes.vec[[w]]))
    egarch.error[w,i,1]<-j
    egarch.error[w,i,2]<-MAE(sigmas,volatilities[[w]][val.indexes.vec[[w]]])
    egarch.error[w,i,3]<-RMSE(sigmas,volatilities[[w]][val.indexes.vec[[w]]])
    egarch.error[w,i,4]<-MAPE(y_pred=sigmas,y_true=volatilities[[w]][val.indexes.vec[[w]]])
  }
  egarch.error.val[[w]]<-as.data.frame(egarch.error[w,,])
  colnames(egarch.error.val[[w]])<-c("Selected","MAE","RMSE","MAPE")
  print(paste("Window:",windows[w]))
  print(head(egarch.error.val[[w]][order(egarch.error.val[[w]]$MAE),],10))
}

#### Modelo seleccionado ####

egarch.model<-egarch.models[[7]] #8
egarch.model@fit$matcoef

#### Seleccion - Backtesting* ####

cl = makePSOCKcluster(5)
spec = getspec(egarch.models[[1]])
roll = ugarchroll(spec, rend, forecast.length = 500, refit.every = 22, refit.window = 'moving', window.size = 100, calculate.VaR = T, keep.coef = TRUE, cluster = cl, solver="hybrid")
report(roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(egarch.models[[1]])

#### Predicciones - modelo EGARCH####

# Prueba
egarch.error<-matrix(NA,nrow=length(windows),ncol = 4)
for(w in 1:length(windows)){
  sigmas<-c()
  sigmas<-prediction.sigma(egarch.model,val=F,w=w,t=length(test.indexes.vec[[w]]))
  egarch.error[w,1]<-windows[w]
  egarch.error[w,2]<-MAE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  egarch.error[w,3]<-RMSE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  egarch.error[w,4]<-MAPE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
}
egarch.error.test<-as.data.frame(egarch.error)
colnames(egarch.error.test)<-c("Ventana","MAE","RMSE","MAPE")
egarch.error.test

# 1d
egarch.error<-matrix(NA,nrow=length(windows),ncol = 4)
for(w in 1:length(windows)){
  sigmas<-c()
  sigmas<-prediction.sigma.1d(egarch.model,val=F,w=w,t=length(test.indexes.vec[[w]]))
  egarch.error[w,1]<-windows[w]
  egarch.error[w,2]<-MAE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  egarch.error[w,3]<-RMSE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
  egarch.error[w,4]<-MAPE(sigmas,volatilities[[w]][test.indexes.vec[[w]]])
}
egarch.error.test<-as.data.frame(egarch.error)
colnames(egarch.error.test)<-c("Ventana","MAE","RMSE","MAPE")
egarch.error.test

#### Backtesting* ####

cl = makePSOCKcluster(5)
spec1 = getspec(garch.model)
spec2 = getspec(egarch.model)
roll1 = ugarchroll(spec1, rend[train.indexes.ts], forecast.length = 500, solver = 'hybrid', refit.every = s, refit.window = 'recursive', cluster = cl)
roll2 = ugarchroll(spec2, rend[train.indexes.ts], forecast.length = 500, refit.every = s, refit.window = 'recursive', cluster = cl)
report(roll1)
report(roll2)
plot(as.xts(as.data.frame(roll1)[, 'Sigma', drop = FALSE]), main = 'GARCH vs eGARCH\n(out-of-sample volatility forecast)',  auto.grid = FALSE, minor.ticks = FALSE)
lines(as.xts(as.data.frame(roll2)[, 'Sigma', drop = FALSE]), col = 2)
legend('topleft', c('GARCH', 'eGARCH'), col = 1:2, lty = c(1, 1), bty = 'n')
grid()

#### RNN ####

epochs <- 2e2
w<-which(windows==min(windows))
validation<-F

#### Individuales ####

isHybrid<-F
#name<-models.names[2]
#source(file="ErrorsRNN.R")

## Antes de ejecutar rnn.plots es importante que los valores de
## isLSTM, isHybrid e isGARCH sean los adecuados.

isLSTM<-T
training_run("RNN.R",echo=T)
lstm.error.train<-error.train
lstm.error.val<-error.val
lstm.error.test<-error.test
models.names[1]<-name
#rnn.plots(models.names[1])

isLSTM<-F
training_run("RNN.R",echo=T)
gru.error.train<-error.train
gru.error.val<-error.val
gru.error.test<-error.test
compare("t2")
models.names[2]<-name
#rnn.plots(models.names[2])
# Correlacion entre variables de entrada modelos híbrido GARCH-EGARCH-RNN
# q<-as.data.frame(matrix(c(xi[[1]],xi[[2]],xi[[3]]),ncol=3,byrow=F))
# cor(q)

#### Hibridos ####

a<-prediction.sigma.1d(garch.model,val=T,w=1,t=length(val.indexes.vec[[1]]))
b<-prediction.sigma.1d(garch.model,val=F,w=1,t=length(rend)-val.uindex.ts)
sigmas.garch<-c(as.double(sigma(garch.model)),a,b)
a<-prediction.sigma.1d(egarch.model,val=T,w=1,t=length(val.indexes.vec[[1]]))
b<-prediction.sigma.1d(egarch.model,val=F,w=1,t=length(rend)-val.uindex.ts)
sigmas.egarch<-c(as.double(sigma(egarch.model)),a,b)
a1<-1 #egarch.model@fit$matcoef[3,1]
g1<-egarch.model@fit$matcoef[5,1]

#a<-prediction.fitted.1d(garch.model,val=T,w=1,t=length(val.indexes.vec[[1]]))
#b<-prediction.fitted.1d(garch.model,val=F,w=1,t=length(rend)-val.uindex.ts)
#fitteds.garch<-c(as.double(fitted(garch.model)),a,b)
#a<-prediction.fitted.1d(egarch.model,val=T,w=1,t=length(val.indexes.vec[[1]]))
#b<-prediction.fitted.1d(egarch.model,val=F,w=1,t=length(rend)-val.uindex.ts)
#fitteds.egarch<-c(as.double(fitted(egarch.model)),a,b)

isHybrid<-T
isLSTM<-F
isComb<-F

isGARCH<-T
training_run("RNN.R",echo=T)
garch.rnn.error.train<-error.train
garch.rnn.error.val<-error.val
garch.rnn.error.test<-error.test
models.names[3]<-name
#rnn.plots(models.names[3])

isGARCH<-F
training_run("RNN.R",echo=T)
egarch.rnn.error.train<-error.train
egarch.rnn.error.val<-error.val
egarch.rnn.error.test<-error.test
models.names[4]<-name
#rnn.plots(models.names[4])

isComb<-T
training_run("RNN.R",echo=T)
e.g.rnn.error.train<-error.train
e.g.rnn.error.val<-error.val
e.g.rnn.error.test<-error.test
compare("t5")
models.names[5]<-name
#rnn.plots(models.names[5])

isLSTM<-T
isComb<-F

isGARCH<-T
training_run("RNN.R",echo=T)
garch.lstm.error.train<-error.train
garch.lstm.error.val<-error.val
garch.lstm.error.test<-error.test
models.names[6]<-name
#rnn.plots(models.names[6])

isGARCH<-F
training_run("RNN.R",echo=T)
egarch.lstm.error.train<-error.train
egarch.lstm.error.val<-error.val
egarch.lstm.error.test<-error.test
models.names[7]<-name
#rnn.plots(models.names[7])

isComb<-T
training_run("RNN.R",echo=T)
e.g.lstm.error.train<-error.train
e.g.lstm.error.val<-error.val
e.g.lstm.error.test<-error.test
models.names[8]<-name
#rnn.plots(models.names[8])

#### Correlación de inputs
q<-matrix(0,nrow=length(xi[[1]]),ncol=3)
q[,1]<-xi[[1]]
q[,2]<-xi[[2]]
q[,3]<-xi[[3]]
q<-as.data.frame(q)
colnames(q)<-c("GARCH","EGARCH","Vol")
corrplot::corrplot(q)

#### PLOTS ####

isHybrid<-F
isComb<-F
isGARCH<-F
isLSTM<-T
rnn.plots(models.names[1])
isLSTM<-F
rnn.plots(models.names[2])
isHybrid<-T
isGARCH<-T
rnn.plots(models.names[3])
isGARCH<-F
rnn.plots(models.names[4])
isComb<-T
rnn.plots(models.names[5])
isLSTM<-T
isComb<-F
isGARCH<-T
rnn.plots(models.names[6])
isGARCH<-F
rnn.plots(models.names[7])
isComb<-T
rnn.plots(models.names[8])

#### Errores ####

isHybrid<-F
isComb<-F
isGARCH<-F
isLSTM<-T
model<-load_model_hdf5(models.names[1])
source(file="ErrorsRNN.R")
lstm.error.train<-error.train
lstm.error.val<-error.val
lstm.error.test<-error.test
isLSTM<-F
model<-load_model_hdf5(models.names[2])
source(file="ErrorsRNN.R")
gru.error.train<-error.train
gru.error.val<-error.val
gru.error.test<-error.test
isHybrid<-T
isLSTM<-F
isComb<-F
isGARCH<-T
model<-load_model_hdf5(models.names[3])
source(file="ErrorsRNN.R")
garch.rnn.error.train<-error.train
garch.rnn.error.val<-error.val
garch.rnn.error.test<-error.test
isGARCH<-F
model<-load_model_hdf5(models.names[4])
source(file="ErrorsRNN.R")
egarch.rnn.error.train<-error.train
egarch.rnn.error.val<-error.val
egarch.rnn.error.test<-error.test
isComb<-T
model<-load_model_hdf5(models.names[5])
source(file="ErrorsRNN.R")
e.g.rnn.error.train<-error.train
e.g.rnn.error.val<-error.val
e.g.rnn.error.test<-error.test
isLSTM<-T
isComb<-F
isGARCH<-T
model<-load_model_hdf5(models.names[6])
source(file="ErrorsRNN.R")
garch.lstm.error.train<-error.train
garch.lstm.error.val<-error.val
garch.lstm.error.test<-error.test
isGARCH<-F
model<-load_model_hdf5(models.names[7])
source(file="ErrorsRNN.R")
egarch.lstm.error.train<-error.train
egarch.lstm.error.val<-error.val
egarch.lstm.error.test<-error.test
isComb<-T
model<-load_model_hdf5(models.names[8])
source(file="ErrorsRNN.R")
e.g.lstm.error.train<-error.train
e.g.lstm.error.val<-error.val
e.g.lstm.error.test<-error.test

#### Comparaciones #####

op<-"t1"
temp<-compare(op)
temp

#### bkt ####
#model <- load_model_hdf5("model-LSTM.h5")
