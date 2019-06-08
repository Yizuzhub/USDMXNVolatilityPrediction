error.train<-list()
error.val<-list()
error.test<-list()

for(j in 1:length(windows)){
  error.train[[j]]<-list()
  error.val[[j]]<-list()
  error.test[[j]]<-list()
  w<-j
  source(file="InputData.R")
  predict.train <- model %>% predict(x.train, batch_size = batch_size)
  predict.val <- model %>% predict(x.val, batch_size = batch_size)
  predict.test <- model %>% predict(x.test, batch_size = batch_size)
  error.train[[j]]<-measures(predict.train/escalar,y.train/escalar)
  error.val[[j]]<-measures(predict.val/escalar,y.val/escalar)
  error.test[[j]]<-measures(predict.test/escalar,y.test/escalar)
}
w<-1
