portion.val<-(1-portion.train)/2
portion.test<-portion.val
source("partition.R",echo=F)

garch.spec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(1,0), include.mean=F),distribution="sged")
temp.garch<-ugarchfit(garch.spec, rend[train.indexes],solver = "hybrid")

egarch.spec <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(1,0), include.mean=F),distribution="sged")
temp.egarch<-ugarchfit(egarch.spec, rend[train.indexes],solver = "hybrid")

a<-prediction.sigma.1d(temp.garch,val=T,w=1,t=length(val.indexes.vec[[1]]))
b<-prediction.sigma.1d(temp.garch,val=F,w=1,t=length(rend)-val.uindex.ts)
sigmas.garch<-c(as.double(sigma(temp.garch)),a,b)
a<-prediction.sigma.1d(temp.egarch,val=T,w=1,t=length(val.indexes.vec[[1]]))
b<-prediction.sigma.1d(temp.egarch,val=F,w=1,t=length(rend)-val.uindex.ts)
sigmas.egarch<-c(as.double(sigma(temp.egarch)),a,b)

a1<-1#temp.egarch@fit$matcoef[3,1]
g1<-temp.egarch@fit$matcoef[5,1]

#### Ajuste ####

for(s in 1:scenaries){
  compare.val[[s]][[names.val.index]]<-list()
  name.temp<-NULL

  isHybrid<-F
  
  isLSTM<-T
  training_run("RNN.R",echo=T)
  lstm.error.train<-error.train
  lstm.error.val<-error.val
  lstm.error.test<-error.test
  models.index<-1
  names.val[models.index]<-name
  
  isLSTM<-F
  training_run("RNN.R",echo=T)
  gru.error.train<-error.train
  gru.error.val<-error.val
  gru.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  #### Hibridos ####
  
  isHybrid<-T
  isLSTM<-F
  isComb<-F
  
  isGARCH<-T
  training_run("RNN.R",echo=T)
  garch.rnn.error.train<-error.train
  garch.rnn.error.val<-error.val
  garch.rnn.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  isGARCH<-F
  training_run("RNN.R",echo=T)
  egarch.rnn.error.train<-error.train
  egarch.rnn.error.val<-error.val
  egarch.rnn.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  isComb<-T
  training_run("RNN.R",echo=T)
  e.g.rnn.error.train<-error.train
  e.g.rnn.error.val<-error.val
  e.g.rnn.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  isLSTM<-T
  isComb<-F
  
  isGARCH<-T
  training_run("RNN.R",echo=T)
  garch.lstm.error.train<-error.train
  garch.lstm.error.val<-error.val
  garch.lstm.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  isGARCH<-F
  training_run("RNN.R",echo=T)
  egarch.lstm.error.train<-error.train
  egarch.lstm.error.val<-error.val
  egarch.lstm.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  isComb<-T
  training_run("RNN.R",echo=T)
  e.g.lstm.error.train<-error.train
  e.g.lstm.error.val<-error.val
  e.g.lstm.error.test<-error.test
  models.index<-models.index+1
  names.val[models.index]<-name
  
  #### Guardar ####
  
  for(i in 1:8){
    name.temp<-substr(names.val[i],1,nchar(names.val[i])-3)
    compare.val[[s]][[names.val.index]][[i]]<-compare(paste("t",i,sep = ""))
    write.csv(compare.val[[s]][[names.val.index]][[i]], paste(name.temp,".csv",sep = ""), sep="\t") 
    write.table(compare.val[[s]][[names.val.index]][[i]], paste(name.temp,".txt",sep = ""), sep="\t")
  }
}