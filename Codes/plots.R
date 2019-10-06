library(lubridate)
library(xts)
library(purrr)
library(ggplot2)
library(MLmetrics)
library(scales)
library(ggpubr)

setwd("C:/Users/jo/Desktop/tesis/Codigos")
#c1<-"#0097f3"   # "#5061fc" #azul
#c2<-"#ffa200"  #"#ff8223" #naranja
#c3<-"#03c924"  #"#03bc28" #verde
source(file="functions.R")
data2 <- read.csv("USD-MXN.csv",colClasses=c("Fecha"="character"))
data2$Fecha<-as.Date(data2[,1],format = "%d/%m/%Y")
db2<-as.ts(data2$Cierre)
rend2<-as.ts(rendiments(db2))
windows2<-c(5,10,22)
volatilities2<-list()
for(i in 1:length(windows2)){
  volatilities2[[i]]<-as.ts(vol.estimate(rend2,windows2[i]))
}

ggt<-theme_bw()+theme(legend.text=element_text(size=16),
  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
  axis.text=element_text(size=14),
  axis.title.x = element_text(size = rel(1.5), angle = 0),
  axis.title.y =  element_text(size = rel(1.5), angle = 90),
  plot.title = element_text(size = rel(1.5), hjust = 0.5, angle = 0, face="bold"))

df<-data2[-1,1:2]
df$Cierre<-rend2
colnames(df)<-c("Fecha","Rend")
p <- ggplot(df, aes(x=df$Fecha, y = df$Rend))+ggt + geom_line(color = c1, size = 1.0) +
  scale_color_manual(values = c.blue)+
  labs(title="Rendimientos Diarios",
       y="", x="")+scale_y_continuous(breaks=c(-0.05,-0.025,0,.025,0.05,0.075))
ggexport(p, filename = "ggRendDaily.pdf")

df<-data2[(windows[1]+1):length(data2[,1]),1:2]
df[,2]<-volatilities2[[1]]
colnames(df)<-c("Fecha","Vol")
p <- ggplot(df, aes(x=df$Fecha, y = df$Vol))+ggt + geom_line(color = c1, size = 1.0) +
  scale_color_manual(values = c.blue)+
  labs(title="Volatilidad Histórica del USD/MXN\n(10/01/2000 - 03/05/2019)",
       subtitle = "Volatilidad estimada mediante la desviación estándar\nde los rendimientos de los últimos 5 días",
       y="", x="")+scale_y_continuous(breaks=seq(0,max(volatilities[[1]]),0.01))+
  scale_x_date(date_breaks = "3 years", labels=date_format("%Y"))
p
ggexport(p, filename = "ggVolDaily.pdf")

d<-as.Date(data2[,1])
inicio<-max(which(year(d)==2000))+1
fin<-min(which(year(d)==2019))-1
y<-fin-inicio+1
df<-data2[inicio:fin,1:4]
df[,2]<-volatilities2[[1]][(inicio-windows[1]):(fin-windows[1])]
df[,3]<-volatilities2[[2]][(inicio-windows[2]):(fin-windows[2])]
df[,4]<-volatilities2[[3]][(inicio-windows[3]):(fin-windows[3])]
colnames(df)<-c("Fecha","v1","v2","v3")
p <- ggplot(df)+ggt+
  labs(title="Volatilidad Histórica del USD/MXN\n(2017-2018)", y="", x="",color = "")+
  theme(legend.position="top")+ylim(0,0.02)+
  scale_x_date(date_breaks = "4 months",
               labels=date_format("%m/%Y"),
               limits = as.Date(c('2016-12-31','2019-01-01')))+
  geom_line(data=df,aes(x=df$Fecha, y = df$v1,color="5 días"),size=1.25)+#,linetype="longdash")+
  geom_line(data=df,aes(x=df$Fecha, y = df$v2,color="10 días"),size=1.25)+
  geom_line(data=df,aes(x=df$Fecha, y = df$v3,color="22 días"),size=1.25)+
  #scale_linetype_manual(values=c("twodash", "dotted", "dotted"))+
  scale_color_manual(values = c(c.blue,c.orange,c.green))
p
ggexport(p, filename = "ggHistoricVol.pdf")

sigmoid<-function(x) 1/(1+exp(-x))
p <- ggplot(data.frame(x = c(-10,10)), aes(x))+ggt +
  stat_function(fun = sigmoid, colour = c.blue,size=2)+
  scale_y_continuous(breaks=seq(0,1,0.2),name=expression(sigma(x)))+
  scale_x_continuous(limits=c(-12,12),name="x",breaks=seq(-12,12,4))
p
ggexport(p, filename = "Logistic.pdf",width=4,height = 4)

p<-ggtsdisplay(model$residuals,theme=ggt+theme(legend.text=element_text(size=16),
                                               plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                                               axis.text=element_text(size=14),
                                               axis.title.x = element_text(size = rel(1.5), angle = 0),
                                               axis.title.y =  element_text(size = rel(1.5), angle = 90),
                                               plot.title = element_text(size = rel(1.5), hjust = 0.5, angle = 0, face="bold")),
               main='Rendimientos Diarios')
#RendTSdisp.pdf

p<-ggtsdisplay(model$residuals,theme=ggt+theme(legend.text=element_text(size=16),
                                               plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                                               axis.text=element_text(size=14),
                                               axis.title.x = element_text(size = rel(1.5), angle = 0),
                                               axis.title.y =  element_text(size = rel(1.5), angle = 90),
                                               plot.title = element_text(size = rel(1.5), hjust = 0.5, angle = 0, face="bold")),
               main='Residuales de Rendimientos ARMA(2,3)')
#ResidARIMATSdisp.pdf

#"HistRendNormality.pdf"
par(mfrow=c(1,2),mar=c(5,5,5,5))
hist(rend,col = c11,probability = T,breaks = 1e2,ylab="Densidad",
     main = "Rendimientos vs Normal",xlab="Rendimientos", axes=F,border=c12)
lines(density(rnorm(1e5,mean=mean(rend),sd=sd(rend))),col = c.orange,lw=3)
box()
axis(1, at=seq(-0.05,0.09,0.025), labels=seq(-0.05,0.09,0.025))
axis(2, at=seq(0,80,20), labels=seq(0,80,20))
qqnorm(rend,col = c.blue, main="Cuantiles Normal vs Observados",ylab = "Cuantiles Observados",xlab = "Cuantiles Teóricos")
qqline(rend,col = c11,lw=2)

for(i in 1:length(means)){
  ylab <- bquote(mu[t]~ - ~ ventana:~ .(windows[i]))
  pdf(cat(c("Drift","W",windows[i],".pdf"),sep = ""))
  ts.plot(means[[i]],lwd=l1,col=c1,gpars=list(ylab=ylab, xlab="DIAS"))
  dev.off()
  ts.plot(means[[i]],lwd=l1,col=c1,gpars=list(ylab=ylab, xlab="DIAS"))
}

pdf("Rend-ACF-PACF.pdf")
par(mfrow=c(1,2))
acf(rend,main="ACF")
pacf(rend,main="PACF")
dev.off()

pdf("Rend-ACF-PACF.pdf")
par(mfrow=c(1,2))
acf(rend^2,main="ACF")
pacf(rend^2,main="PACF")
dev.off()

pdf("RendAbs-ACF-PACF.pdf")
par(mfrow=c(1,2))
acf(abs(rend),main="ACF")
pacf(abs(rend),main="PACF")
dev.off()

