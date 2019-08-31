escalar<-15
w<-1
predict.train<-list()
predict.val<-list()
predict.test<-list()
model.rnn<-list()
yval<-list()
ytest<-list()
ytrain<-list()
fechas.val<-as.Date(data$Fecha[(train.uindex.vec[w]+windows[w]+1):(val.uindex.vec[w]+windows[w])])
fechas.test<-as.Date(data$Fecha[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])])
fechas.train<-as.Date(data$Fecha[(windows[w]+timesteps):(train.uindex.vec[w]+windows[w])])
for(i in 1:8){
  model.rnn[[i]]<-load_model_hdf5(models.names[i])
}

isHybrid<-F
isComb<-F
isGARCH<-F
isLSTM<-T
j<-1
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isLSTM<-F
j<-2
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isHybrid<-T
isGARCH<-T
j<-3
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isGARCH<-F
j<-4
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isComb<-T
j<-5
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isLSTM<-T
isComb<-F
isGARCH<-T
j<-6
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isGARCH<-F
j<-7
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
isComb<-T
j<-8
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
s.g<-sigmas.garch[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]
s.e<-sigmas.egarch[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]

m1<-1
m2<-2
m3<-3

pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
pred3<-as.data.frame(cbind(fechas.test,ytest[[m3]]/escalar,predict.test[[m3]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
#pred4<-sigmas.egarch[(train.uindex.vec[w]+windows[w]+1):(val.uindex.vec[w]+windows[w])]
pred4<-sigmas.egarch[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]

s<-14
m<-12
size<-2
th<-ggt+theme(
  plot.title = element_text(size=s, face="bold"),
  axis.title.x = element_text(size=s, face="bold"),
  axis.title.y = element_text(size=s, face="bold"),
  axis.text.x = element_text(face="bold", size=m),
  axis.text.y = element_text(face="bold", size=m),
  legend.title=element_text(face="bold",size=m),
  legend.text=element_text(size=m))



m1<-2
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1[,1:2],s.g,s.e))
colnames(a)<-c("Fecha","Obs","GARCH","EGARCH")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$GARCH, col = "MA(1)-GARCH(1,1)"),size=0.8)+
  geom_line(aes(y = a$EGARCH, col = "MA(1)-EGARCH(1,1)"),size=0.8)+
  labs(title="Volatilidad USD/MXN de 22 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.blue))
p1
ggexport(p1, filename = "TestCompareTS.pdf",width = 8, height = 4)

m1<-1
m2<-2
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,sigmas.egarch))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3]))
colnames(a)<-c("Fecha","Obs","LSTM","GRU")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$LSTM, col = "LSTM"),size=0.8)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.8)+
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.blue,c.orange,c.green))
p1
ggexport(p1, filename = "TestCompareRNN.pdf",width = 8, height = 4)



m1<-1
m2<-2
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3]))
colnames(a)<-c("Fecha","Obs","LSTM","GRU")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$LSTM, col = "LSTM"),size=0.8)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.8)+
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.blue,c.orange,c.green))
p1
ggexport(p1, filename = "TestCompareRNN.pdf",width = 8, height = 4)



m1<-6
m2<-7
m3<-8
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
pred3<-as.data.frame(cbind(fechas.test,ytest[[m3]]/escalar,predict.test[[m3]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3],pred3[,3]))
colnames(a)<-c("Fecha","Obs","G","E","GE")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$G, col = "G-LSTM"),size=0.8)+
  geom_line(aes(y = a$E, col = "E-LSTM"),size=0.8)+
  geom_line(aes(y = a$GE, col = "GE-LSTM"),size=0.8)+  
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.llblue,c.green,c.blue))
#subtitle = paste("De",date_format("%m/%Y")(as.Date(min(a$Fecha))),"a",date_format("%m/%Y")(as.Date(max(a$Fecha)))))
p1
ggexport(p1, filename = "TestCompareHybridLSTM.pdf",width = 8, height = 4)


m1<-6
m2<-7
m3<-8
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
pred3<-as.data.frame(cbind(fechas.test,ytest[[m3]]/escalar,predict.test[[m3]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3],pred3[,3]))
colnames(a)<-c("Fecha","Obs","G","E","GE")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$G, col = "G-LSTM"),size=0.8)+
  geom_line(aes(y = a$E, col = "E-LSTM"),size=0.8)+
  geom_line(aes(y = a$GE, col = "G-E-LSTM"),size=0.8)+  
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.llblue,c.green,c.blue))
p1
ggexport(p1, filename = "ValCompareHybridLSTM.pdf",width = 8, height = 4)



m1<-3
m2<-4
m3<-5
pred1<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
pred3<-as.data.frame(cbind(fechas.test,ytest[[m3]]/escalar,predict.test[[m3]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3],pred3[,3]))
colnames(a)<-c("Fecha","Obs","G","E","GE")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$G, col = "G-GRU"),size=0.8)+
  geom_line(aes(y = a$E, col = "E-GRU"),size=0.8)+
  geom_line(aes(y = a$GE, col = "G-E-GRU"),size=0.8)+  
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.llblue,c.green,c.blue))
p1
ggexport(p1, filename = "TestCompareHybridGRU.pdf",width = 8, height = 4)

m1<-3
m2<-4
m3<-5
pred1<-as.data.frame(cbind(fechas.val,yval[[m1]]/escalar,predict.val[[m1]]/escalar))
pred2<-as.data.frame(cbind(fechas.val,yval[[m2]]/escalar,predict.val[[m2]]/escalar))
pred3<-as.data.frame(cbind(fechas.val,yval[[m3]]/escalar,predict.val[[m3]]/escalar))
colnames(pred1)<-c("Fecha","Observada","Estimada")
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred1,pred2[,3],pred3[,3],pred4))
colnames(a)<-c("Fecha","Obs","G","E","GE")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$G, col = "G-GRU"),size=0.8)+
  geom_line(aes(y = a$E, col = "E-GRU"),size=0.8)+
  geom_line(aes(y = a$GE, col = "G-E-GRU"),size=0.8)+  
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.llblue,c.green,c.blue))
p1
ggexport(p1, filename = "ValCompareHybridGRU.pdf",width = 8, height = 4)



m1<-2
m2<-5
m3<-8
pred1<-s.e
pred2<-as.data.frame(cbind(fechas.test,ytest[[m1]]/escalar,predict.test[[m1]]/escalar))
pred3<-as.data.frame(cbind(fechas.test,ytest[[m2]]/escalar,predict.test[[m2]]/escalar))
pred4<-as.data.frame(cbind(fechas.test,ytest[[m3]]/escalar,predict.test[[m3]]/escalar))
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
colnames(pred4)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred2[,1:2],pred1,pred2[,3],pred3[,3],pred4[,3]))
colnames(a)<-c("Fecha","Obs","E","GRU","EGL","EGG")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=1) +
  geom_line(aes(y = a$E, col = "EGARCH"),size=0.7)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.7)+  
  geom_line(aes(y = a$EGL, col = "G-E-LSTM"),size=0.7)+ 
  geom_line(aes(y = a$EGG, col = "G-E-GRU"),size=0.7)+ 
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.llblue,c.blue,c.lpurple))
p1
ggexport(p1, filename = "TestCompareModels5.pdf",width = 8, height = 4)

p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=0.5) +
  geom_line(aes(y = a$E, col = "EGARCH"),size=0.5)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.5)+  
  geom_line(aes(y = a$EGL, col = "G-E-LSTM"),size=0.5)+ 
  geom_line(aes(y = a$EGG, col = "G-E-GRU"),size=0.5)+ 
  labs(title="Volatilidad USD/MXN de 10 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.llblue,c.blue,c.lpurple))
p1
ggexport(p1, filename = "TestCompareModels10.pdf",width = 8, height = 4)

p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=0.3) +
  geom_line(aes(y = a$E, col = "EGARCH"),size=0.3)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.3)+  
  geom_line(aes(y = a$EGL, col = "G-E-LSTM"),size=0.3)+ 
  geom_line(aes(y = a$EGG, col = "G-E-GRU"),size=0.3)+ 
  labs(title="Volatilidad USD/MXN de 22 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.llblue,c.blue,c.lpurple))
p1
ggexport(p1, filename = "TestCompareModels22.pdf",width = 8, height = 4)


a<-as.data.frame(cbind(pred1,pred2[,3],pred3[,3]))
colnames(a)<-c("Fecha","Obs","GE-GRU","E-LSTM","AR-EG")
umbral<-0.02
b<-a[a$Obs>umbral,]
sum(b$G>b$E)
sum(b$G<b$E)

w<-1
source(file="InputData.R")
length(y.train)
length(y.val)
length(y.test)
w<-2
source(file="InputData.R")
length(y.train)
length(y.val)
length(y.test)
w<-3
source(file="InputData.R")
length(y.train)
length(y.val)
length(y.test)
