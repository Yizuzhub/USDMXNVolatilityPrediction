w<-1
fechas.val<-as.Date(data$Fecha[(train.uindex.vec[w]+windows[w]+1):(val.uindex.vec[w]+windows[w])])
fechas.test<-as.Date(data$Fecha[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])])
fechas.train<-as.Date(data$Fecha[(windows[w]+timesteps):(train.uindex.vec[w]+windows[w])])

isHybrid<-F
isComb<-F
isGARCH<-F
isLSTM<-T
j<-1
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isLSTM<-F
j<-2
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isHybrid<-T
isGARCH<-T
j<-3
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isGARCH<-F
j<-4
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isComb<-T
j<-5
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isLSTM<-T
isComb<-F
isGARCH<-T
j<-6
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isGARCH<-F
j<-7
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

isComb<-T
j<-8
source(file="InputData.R")
predict.train[[j]] <- (model.rnn[[j]] %>% predict(x.train, batch_size = batch_size))/escalar
predict.val[[j]] <- (model.rnn[[j]] %>% predict(x.val, batch_size = batch_size))/escalar
predict.test[[j]] <- (model.rnn[[j]] %>% predict(x.test, batch_size = batch_size))/escalar

y.train<-y.train/escalar
y.val<-y.val/escalar
y.test<-y.test/escalar

s.g<-prediction.sigma.1d(garch.model,val=F,w=w,t=length(test.indexes.vec[[w]]))
s.e<-prediction.sigma.1d(egarch.model,val=F,w=w,t=length(test.indexes.vec[[w]]))

m1<-2
m2<-5
m3<-8
pred1<-s.g
pred2<-as.data.frame(cbind(fechas.test,y.test,predict.test[[m1]]))
pred3<-as.data.frame(cbind(fechas.test,y.test,predict.test[[m2]]))
pred4<-as.data.frame(cbind(fechas.test,y.test,predict.test[[m3]]))
colnames(pred2)<-c("Fecha","Observada","Estimada")
colnames(pred3)<-c("Fecha","Observada","Estimada")
colnames(pred4)<-c("Fecha","Observada","Estimada")
a<-as.data.frame(cbind(pred2[,1:2],pred1,pred2[,3],pred3[,3],pred4[,3]))
colnames(a)<-c("Fecha","Obs","G","GRU","EGL","EGG")
p1<-ggplot(a, aes(x=as.Date(a$Fecha), y = value, color = variable))+th+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(a$Fecha),max(a$Fecha)))) + 
  geom_line(aes(y = a$Obs, col = "Observada"),size=0.5) +
  geom_line(aes(y = a$G, col = "MA-GARCH"),size=0.5)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.5)+  
  geom_line(aes(y = a$EGL, col = "G-E-LSTM"),size=0.5)+ 
  geom_line(aes(y = a$EGG, col = "G-E-GRU"),size=0.5)+ 
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.llblue,c.blue,c.lpurple))
p1
ggexport(p1, filename = "TestCompareModels5.pdf",width = 8, height = 4)
