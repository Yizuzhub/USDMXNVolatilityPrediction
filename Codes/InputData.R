train.indexes<-train.indexes.vec[[w]]
val.indexes<-val.indexes.vec[[w]]
test.indexes<-test.indexes.vec[[w]]
train.uindex<-train.uindex.vec[[w]]
val.uindex<-val.uindex.vec[[w]]
test.uindex<-test.uindex.vec[[w]]
n<-tail(test.indexes,1)
nvars.x<-1
escalar<-floor(1/max(volatilities[[1]]))
xi<-list()

if(isHybrid){
  nvars.x<-2
  if(isComb){
    g1<-0.1580
    a1<-0.1495
    nvars.x<-3
    escalar<-min(escalar,floor(1/max(sigmas.garch)),floor(1/max(sigmas.egarch)))
    #xi[[1]]<-((diff(fitteds.garch)[-1])/(sigmas.garch[-c(1,2)]))[windows[w]:(windows[w]+length(volatilities[[w]])-1)]
    #xi[[2]]<-((diff(fitteds.egarch)[-1])/(sigmas.egarch[-c(1,2)]))[windows[w]:(windows[w]+length(volatilities[[w]])-1)]
    #xi[[1]]<-as.double(sigmas.garch[windows[w]:(windows[w]+length(volatilities[[w]])-1)])*escalar
    #xi[[2]]<-as.double(sigmas.egarch[windows[w]:(windows[w]+length(volatilities[[w]])-1)])*escalar
    temp<-c(0,diff(rend))
    xi[[1]]<-sapply((windows[w]):(windows[w]+length(volatilities[[w]])-1),function(x)
      temp[x]*sigmas.garch[x])*escalar*5
    xi[[2]]<-sapply((windows[w]):(windows[w]+length(volatilities[[w]])-1),function(x)
      ifelse(temp[x]<0,
             (g1-a1)*temp[x]*sigmas.egarch[x],
             (g1+a1)*temp[x]*sigmas.egarch[x]))*escalar*5
    xi[[3]]<-volatilities[[w]]*escalar
    name<-"GARCH-EGARCH"
    if(isLSTM){
      name<-paste(name,"-LSTM-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }else{
      name<-paste(name,"-GRU-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }
  }else if(isGARCH){
    escalar<-min(escalar,floor(1/max(sigmas.garch)))
    #xi[[1]]<-as.double(fitteds.garch[windows[w]:(windows[w]+length(volatilities[[w]])-1)])*escalar
    #xi[[1]]<-((diff(fitteds.garch)[-1])/(sigmas.garch[-c(1,2)]))[windows[w]:(windows[w]+length(volatilities[[w]])-1)]
    #xi[[1]]<-(diff(fitteds.garch)[-1])[windows[w]:(windows[w]+length(volatilities[[w]])-1)]*escalar
    temp<-c(0,diff(rend))
    xi[[1]]<-sapply((windows[w]):(windows[w]+length(volatilities[[w]])-1),function(x) temp[x]*sigmas.garch[x])*escalar*5
    xi[[2]]<-volatilities[[w]]*escalar
    name<-"GARCH"
    if(isLSTM){
      name<-paste(name,"-LSTM-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }else{
      name<-paste(name,"-GRU-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }
  }else{
    escalar<-min(escalar,floor(1/max(sigmas.egarch)))
    g1<-0.1580
    a1<-0.1495
    #xi[[1]]<-((diff(fitteds.egarch)[-1])/(sigmas.egarch[-c(1,2)]))[windows[w]:(windows[w]+length(volatilities[[w]])-1)]
    #xi[[1]]<-(diff(fitteds.egarch)[-1])[windows[w]:(windows[w]+length(volatilities[[w]])-1)]*escalar
    #xi[[1]]<-as.double(fitteds.egarch[windows[w]:(windows[w]+length(volatilities[[w]])-1)])*escalar
    temp<-c(0,diff(rend))
    xi[[1]]<-sapply((windows[w]):(windows[w]+length(volatilities[[w]])-1),function(x)
      ifelse(temp[x]<0,
             (g1-a1)*temp[x]*sigmas.egarch[x],
             (g1+a1)*temp[x]*sigmas.egarch[x]))*escalar*5
    xi[[2]]<-volatilities[[w]]*escalar
    name<-"EGARCH"
    if(isLSTM){
      name<-paste(name,"-LSTM-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }else{
      name<-paste(name,"-GRU-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
    }
  }
  index.y<-2
}else{
  xi[[1]]<-volatilities[[w]]*escalar
  if(isLSTM){
    name<-paste("LSTM-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
  }else{
    name<-paste("GRU-W",w,"U",units[1],"TS",timesteps,".h5",sep = "")
  }
}

nvars<-length(xi)
index.y<-nvars
temp<-array(as.numeric(unlist(xi)))
a<-matrix(temp,ncol = nvars,byrow = F)

if(nrow(a)<=n) print("Ejemplos insuficientes. Deben ser n+1")

x.train <- array(data = 0, dim = c(train.uindex-timesteps+1, timesteps, nvars.x))
x.val <- array(data = 0, dim = c(val.uindex-train.uindex, timesteps, nvars.x))
x.test <- array(data = 0, dim = c(n-val.uindex, timesteps, nvars.x))

for(i in 1:(dim(x.train)[1]))
  x.train[i,1:timesteps,]<-a[i:(i+timesteps-1),1:nvars.x]
for(i in 1:(dim(x.val)[1]))
  x.val[i,1:timesteps,]<-a[(train.uindex+i):(train.uindex+i+timesteps-1),1:nvars.x]
for(i in 1:(dim(x.test)[1]))
  x.test[i,1:timesteps,]<-a[(val.uindex+i):(val.uindex+i+timesteps-1),1:nvars.x]

y.train <- array(data = 0, dim = c(dim(x.train)[1], 1))
y.val <- array(data = 0, dim = c(dim(x.val)[1], 1))
y.test <- array(data = 0, dim = c(dim(x.test)[1], 1))
y.fin <- a[n+1,index.y]

for (i in 1:(dim(x.train)[1])){
  y.train[[i,1]] <- a[i+timesteps,index.y]  #x.train[i+1,1,2]
}#train.uindex+1
for (i in 1:(dim(x.val)[1])) {
  y.val[[i,1]] <- a[train.uindex+1+i,index.y]   #x.val[i+1,1,1]
}#val.uindex+1
for (i in 1:(dim(x.test)[1])) {
  y.test[[i,1]] <- a[val.uindex+1+i,index.y]  #x.test[i+1,1,1]
}#n-2*timesteps+3
