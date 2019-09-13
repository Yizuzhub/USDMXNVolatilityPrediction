w<-1

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
ytrain[[j]]<-y.train
isLSTM<-F
j<-2
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
isHybrid<-T
isGARCH<-T
j<-3
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
isGARCH<-F
j<-4
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
isComb<-T
j<-5
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
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
ytrain[[j]]<-y.train
isGARCH<-F
j<-7
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
isComb<-T
j<-8
source(file="InputData.R")
predict.train[[j]] <- model.rnn[[j]] %>% predict(x.train, batch_size = batch_size)
predict.val[[j]] <- model.rnn[[j]] %>% predict(x.val, batch_size = batch_size)
predict.test[[j]] <- model.rnn[[j]] %>% predict(x.test, batch_size = batch_size)
yval[[j]]<-y.val
ytest[[j]]<-y.test
ytrain[[j]]<-y.train
a<-prediction.sigma.1d(garch.model,val=T,w=w,t=length(val.indexes.vec[[w]]))
b<-prediction.sigma.1d(garch.model,val=F,w=w,t=length(rend)-val.uindex.ts)
sigmas.garch<-c(as.double(sigma(garch.model)),a,b)
a<-prediction.sigma.1d(egarch.model,val=T,w=w,t=length(val.indexes.vec[[w]]))
b<-prediction.sigma.1d(egarch.model,val=F,w=w,t=length(rend)-val.uindex.ts)
sigmas.egarch<-c(as.double(sigma(egarch.model)),a,b)
s.g<-sigmas.garch[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]
s.e<-sigmas.egarch[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]

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
  geom_line(aes(y = a$Obs, col = "Observada"),size=0.5) +
  geom_line(aes(y = a$E, col = "EGARCH"),size=0.5)+
  geom_line(aes(y = a$GRU, col = "GRU"),size=0.5)+  
  geom_line(aes(y = a$EGL, col = "G-E-LSTM"),size=0.5)+ 
  geom_line(aes(y = a$EGG, col = "G-E-GRU"),size=0.5)+ 
  labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
  scale_color_manual(values=c(c.red,c.green,c.llblue,c.blue,c.lpurple))
p1
ggexport(p1, filename = "TestCompareModels5.pdf",width = 8, height = 4)
