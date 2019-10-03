n.vec<-c()
multiple<-list()
temp<-list()
train.uindex.vec<-c()
div1<-c()
div2<-c()
train.indexes.vec<-list()
val.indexes.vec<-list()
test.indexes.vec<-list()
dataDates <- read.zoo(data)

for(i in 1:length(windows)){
  n.vec[i] <- length(volatilities[[i]])
  temp[[i]] <- 1:ceiling(n.vec[i]*(1-portion.val-portion.test))
  multiple[[i]] <- sort(temp[[i]][((temp[[i]]-timesteps+1) %% batch_size) == 0],T)
  train.uindex.vec[i] <- multiple[[i]][1]
  
  temp[[i]] <- 1:ceiling(n.vec[i]*portion.val)
  multiple[[i]] <- sort(temp[[i]][((temp[[i]]) %% batch_size) == 0],T)
  div1[1] <- multiple[[i]][1]
  
  temp[[i]] <- 1:ceiling(n.vec[i]*portion.test)
  multiple[[i]] <- sort(temp[[i]][((temp[[i]]) %% batch_size) == 0],T)
  div2[1] <- multiple[[i]][1]
}

val.uindex.vec<-train.uindex.vec+div1
test.uindex.vec<-val.uindex.vec+div2

train.size.vec<-train.uindex.vec/test.uindex.vec
val.size.vec<-(val.uindex.vec-train.uindex.vec)/test.uindex.vec
test.size.vec<-1-train.size.vec-val.size.vec

train.size.vec
val.size.vec
test.size.vec

for(i in 1:length(windows)){
  train.indexes.vec[[i]]<-1:(train.uindex.vec[[i]])
  val.indexes.vec[[i]]<-(train.uindex.vec[[i]]+1):(val.uindex.vec[[i]])
  test.indexes.vec[[i]]<-(val.uindex.vec[[i]]+1):(test.uindex.vec[[i]])
  print(paste("Ventana:",windows[i]))
  print(dataDates[c(1,
                    windows[i]-1+train.uindex.vec[i],
                    windows[i]+train.uindex.vec[i],
                    windows[i]-1+val.uindex.vec[i],
                    windows[i]+val.uindex.vec[i],
                    windows[i]-1+test.uindex.vec[i])])
}

#indices para rendimientos

train.indexes<-1:(train.uindex.vec[[1]]+windows[[1]]-1)
val.indexes<-val.indexes.vec[[1]][1]:tail(val.indexes.vec[[1]])[1]+windows[[1]]-1
test.indexes<-test.indexes.vec[[1]][1]:tail(test.indexes.vec[[1]])[1]+windows[[1]]-1
train.uindex<-tail(train.uindex.vec[[1]],1)+windows[[1]]-1
val.uindex<-tail(val.uindex.vec[[1]],1)+windows[[1]]-1
test.uindex<-tail(test.uindex.vec[[1]],1)+windows[[1]]-1
n<-tail(test.indexes[[1]],1)

train.indexes.ts<-train.indexes
val.indexes.ts<-val.indexes
test.indexes.ts<-test.indexes
train.uindex.ts<-train.uindex
val.uindex.ts<-val.uindex
test.uindex.ts<-test.uindex
n.ts<-n

#train.uindex.vec almacena los limites superiores de cada ventana del conjunto de entrenamiento (dentro del vector)
#train.uindexes almacena los indices del conjunto de entrenamiento para la ventana 1 (para los rendimientos, no volatilidades)


