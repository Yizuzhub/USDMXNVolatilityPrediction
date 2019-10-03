#### Validación en secuencia ####

source(file="functions.R",echo=F)

validation<-T
scenaries<-5
sizes<-seq(0.1,0.5,0.1)
names.val<-c()
names.val.index<-0
compare.val<-list()
w<-1

for(s in 1:scenaries){
  compare.val[[s]]<-list()
}

for(p in 1:length(sizes)){
  portion.train<-sizes[p]
  names.val.index<-p
  source("val-hybrid.R",echo=T)
}

#### Promedio por porcentaje del conjunto de entrenamiento ####

metrics.val<-list()
risk.estimate<-list()
risk.estimate.mean<-list()
limit<-length(sizes)-1

for(s in 1:scenaries){
  risk.estimate[[s]]<-list()
  for(i in 1:9){#models
    risk.estimate[[s]][[i]]<-as.data.frame(matrix(0,nrow=9,ncol=5))
    colnames(risk.estimate[[s]][[i]])<-c("Set","Window","MAE","RMSE","MAPE")
    risk.estimate[[s]][[i]][,1]<-c(rep("Train",3),rep("Val",3),rep("Test",3))
    risk.estimate[[s]][[i]][,2]<-rep(c(5,10,22),3)
    
    risk.estimate.mean[[i]]<-as.data.frame(matrix(0,nrow=9,ncol=5))
    colnames(risk.estimate.mean[[i]])<-c("Set","Window","MAE","RMSE","MAPE")
    risk.estimate.mean[[i]][,1]<-c(rep("Train",3),rep("Val",3),rep("Test",3))
    risk.estimate.mean[[i]][,2]<-rep(c(5,10,22),3)
  }
}

#Promedio por porcentajes - sizes
for(s in 1:scenaries){
  for(t in 1:9){#sets
    for(i in 1:8){#models
      metrics.val[[s]]<-matrix(0,nrow=limit,ncol=3)
      for(j in 1:limit){#sizes
        metrics.val[[s]][j,]<-as.numeric(array(unlist(compare.val[[s]][[j]][[i]][t,3:5]),dim=c(3)))
      }
      risk.estimate[[s]][[i]][t,3:5]<-apply(metrics.val[[s]],2,mean)
    }
  }
}

#Promedios por escenarios
for(t in 1:9){#sets
  for(i in 1:8){#models
    metrics.val<-matrix(0,nrow=scenaries,ncol=3)
    for(s in 1:scenaries){#scenaries
      metrics.val[s,]<-as.numeric(array(unlist(risk.estimate[[s]][[i]][t,3:5]),dim=c(3)))
    }
    risk.estimate.mean[[i]][t,3:5]<-apply(metrics.val,2,mean)
  }
}

#GRU
risk.estimate.mean[[2]]
risk.estimate.mean[[3]]
risk.estimate.mean[[4]]
risk.estimate.mean[[5]]

#LSTM
risk.estimate.mean[[1]]
risk.estimate.mean[[6]]
risk.estimate.mean[[7]]
risk.estimate.mean[[8]]


q<-as.data.frame(array(0,dim=c(24,5)))
colnames(q)<-c("Set","Model","MAE","RMSE","MAPE")
q[1,1]<-"Train"
q[9,1]<-"Val"
q[17,1]<-"Test"
q[,2]<-rep(models.names,3)

for(i in 1:8){
  q[seq(i,24,8),3:5]<-risk.estimate.mean[[i]][seq(1,9,3),3:5]
}
q


#### Promedio por conjunto ####

mean.risk<-list()

for(i in 1:8){
  mean.risk[[i]]<-as.data.frame(matrix(0,nrow=3,ncol=3))
  rownames(mean.risk[[i]])<-c("Train","Val","Test")
  colnames(mean.risk[[i]])<-c("MAE","RMSE","MAPE")
  mean.risk[[i]][1,]<-apply(risk.estimate.mean[[i]][1:3,3:5],2,mean)
  mean.risk[[i]][2,]<-apply(risk.estimate.mean[[i]][4:6,3:5],2,mean)
  mean.risk[[i]][3,]<-apply(risk.estimate.mean[[i]][7:9,3:5],2,mean)
}

#LSTM
mean.risk[[1]]
mean.risk[[6]]
mean.risk[[7]]
mean.risk[[8]]

#GRU
mean.risk[[2]]
mean.risk[[3]]
mean.risk[[4]]
mean.risk[[5]]



#### Promedio de escenarios ####

metrics.val<-list()
risk.estimate<-list()
risk.estimate.mean<-list()

for(s in 1:length(sizes)){
  risk.estimate[[s]]<-list()
  for(i in 1:9){#models
    risk.estimate[[s]][[i]]<-as.data.frame(matrix(0,nrow=9,ncol=5))
    colnames(risk.estimate[[s]][[i]])<-c("Set","Window","MAE","RMSE","MAPE")
    risk.estimate[[s]][[i]][,1]<-c(rep("Train",3),rep("Val",3),rep("Test",3))
    risk.estimate[[s]][[i]][,2]<-rep(c(5,10,22),3)
    
    risk.estimate.mean[[i]]<-as.data.frame(matrix(0,nrow=9,ncol=5))
    colnames(risk.estimate.mean[[i]])<-c("Set","Window","MAE","RMSE","MAPE")
    risk.estimate.mean[[i]][,1]<-c(rep("Train",3),rep("Val",3),rep("Test",3))
    risk.estimate.mean[[i]][,2]<-rep(c(5,10,22),3)
  }
}

for(j in 1:length(sizes)){
  for(t in 1:9){#sets
    for(i in 1:8){#models
      metrics.val[[j]]<-matrix(0,nrow=scenaries,ncol=3)
      for(s in 1:scenaries){#scenaries
        metrics.val[[j]][s,]<-as.numeric(array(unlist(compare.val[[s]][[j]][[i]][t,3:5]),dim=c(3)))
      }
      risk.estimate[[j]][[i]][t,3:5]<-apply(metrics.val[[j]],2,mean)
    }
  }
}

q<-as.data.frame(array(0,dim=c(24,2+length(sizes))))
colnames(q)<-c("Set","Model",sizes)
q[1,1]<-"Train"
q[9,1]<-"Val"
q[17,1]<-"Test"
q[,2]<-rep(models.names,3)

# Promedio (para cada métrica) por pesos del conjunto de entrenamiento

#MAE

for(j in 1:length(sizes)){
  for(i in 1:8){
    q[seq(i,24,8),2+j]<-risk.estimate[[j]][[i]][seq(1,9,3),3]
  }
}
q[,1:6]

#RMSE

for(j in 1:length(sizes)){
  for(i in 1:8){
    q[seq(i,24,8),2+j]<-risk.estimate[[j]][[i]][seq(1,9,3),4]
  }
}
q[,1:6]


#MAPE

for(j in 1:length(sizes)){
  for(i in 1:8){
    q[seq(i,24,8),2+j]<-risk.estimate[[j]][[i]][seq(1,9,3),5]
  }
}
q[,1:6]






for(t in 1:9){#sets
  for(i in 1:8){#models
    metrics.val<-matrix(0,nrow=scenaries,ncol=3)
    for(s in 1:scenaries){#scenaries
      metrics.val[s,]<-as.numeric(array(unlist(risk.estimate[[s]][[i]][t,3:5]),dim=c(3)))
    }
    risk.estimate.mean[[i]][t,3:5]<-apply(metrics.val,2,mean)
  }
}




for(s in 1:scenaries){
  compare.val[[s]][[8]]<-list()
  for(i in 0:6){
    for(m in 1:8){
      compare.val[[s]][[8-i]][[m]]<-compare.val[[s]][[7-i]][[m]]
    }
  }
}
