rendiments <- function(x){
  n<-length(x)
  rend <- log(x[2:n] / x[1:(n-1)])
  return(rend)
}

vol.estimate<-function(rend,window.size=1){
  n<-length(rend)
  vol.rend <- sapply(1:(n-window.size+1),function(i) sd(rend[i:(i+window.size-1)]))
  return(vol.rend)
}

mean.estimate<-function(rend,window.size=1){
  n<-length(rend)
  mean.rend <- unlist(lapply(1:(n-window.size+1),function(i) rep(mean(rend[i:(i+window.size-1)]),window.size)))
  return(mean.rend)
}

mean2.estimate<-function(rend,window.size=1){
  n<-length(rend)
  vol.rend <- sapply(1:(n-window.size+1),function(i) mean(rend[i:(i+window.size-1)]))
  return(vol.rend)
}

build.LSTM<-function(units=c(1,1),timesteps=1,batch_size=200, input_vars=1){
  optim<-optimizer_adam(lr = 1e-3, beta_1 = 0.9, beta_2 = 0.999,
                        epsilon = NULL, decay = 5e-4, amsgrad = T, clipnorm = NULL,
                        clipvalue = NULL)
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = units[1], return_sequences = T, stateful = T,
               batch_input_shape = c(batch_size, timesteps, input_vars)) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_lstm(units = units[2], return_sequences = F, stateful = T) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 1)
  model %>% compile(loss = 'mse', optimizer = optim) #metrics = c('accuracy'))
  
  return(model)
}

build.GRU<-function(units=c(1,1),timesteps=1,batch_size=200, input_vars=1){
  optim<-optimizer_adam(lr = 1e-3, beta_1 = 0.9, beta_2 = 0.999,
                        epsilon = NULL, decay = 5e-4, amsgrad = T, clipnorm = NULL,
                        clipvalue = NULL)
  model <- keras_model_sequential()
  model %>%
    layer_gru(units = units[1], return_sequences = T, stateful = T,
               batch_input_shape = c(batch_size, timesteps, input_vars)) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_gru(units = units[2], return_sequences = F, stateful = T) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 1)
  model %>% compile(loss = 'mse', optimizer = optim) #metrics = c('accuracy'))
  
  return(model)
}

measures<-function(est,obs){
  t<-list(
    MAE(y_pred =  est,y_true =  obs),
    RMSE(y_pred =  est,y_true =  obs),
    MAPE(y_pred =  est,y_true =  obs)
  )
  return(t)
}

rnn.error.switch<-function(op){
  temp<-c()
  switch(op,
         t1={temp<-unlist(lstm.error.train)},
         t2={temp<-unlist(gru.error.train)},
         t3={temp<-unlist(garch.rnn.error.train)},
         t4={temp<-unlist(egarch.rnn.error.train)},
         
         t11={temp<-unlist(lstm.error.val)},
         t21={temp<-unlist(gru.error.val)},
         t31={temp<-unlist(garch.rnn.error.val)},
         t41={temp<-unlist(egarch.rnn.error.val)},
         
         t12={temp<-unlist(lstm.error.test)},
         t22={temp<-unlist(gru.error.test)},
         t32={temp<-unlist(garch.rnn.error.test)},
         t42={temp<-unlist(egarch.rnn.error.test)},       
         stop("Opcion invalida!"))
  return(temp)
}

div<-function(op){
  a<-compare(op)
  return(as.double(levels(a$MAE))/as.double(levels(a$MAPE)))
}

rnn.plots<-function(name){
  model <- load_model_hdf5(name)
  s<-14
  m<-12
  size<-0.6
  th<-ggt+theme(
    plot.title = element_text(size=s, face="bold"),
    axis.title.x = element_text(size=s, face="bold"),
    axis.title.y = element_text(size=s, face="bold"),
    axis.text.x = element_text(face="bold", size=m),
    axis.text.y = element_text(face="bold", size=m),
    legend.title=element_text(face="bold",size=m),
    legend.text=element_text(size=m))
  w<-1
  source(file="InputData.R")
  
  predict.train <- model %>% predict(x.train, batch_size = batch_size)
  pred<-as.data.frame(cbind(as.Date(data$Fecha[(windows[w]+timesteps):(train.uindex.vec[w]+windows[w])]),y.train,predict.train))
  colnames(pred)<-c("Fecha",  "Observada","Estimada")
  pred$Fecha<-as.Date(pred$Fecha)
  pred$Observada<-pred$Observada/escalar
  pred$Estimada<-pred$Estimada/escalar
  p1<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value, color = variable))+th+
    scale_x_date(date_breaks = "3 years",
                 labels=date_format("%Y"),
                 limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
    geom_line(aes(y = pred$Observada, col = "Observada")) +
    geom_line(aes(y = pred$Estimada, col = "Estimada"))+
    labs(title="Volatilidad USD/MXN de 5 días",y="", x="",color = "Volatilidad")+
    scale_color_manual(values=c(c.blue,c.orange))
  ggexport(p1, filename = paste("Train",substr(name,1,nchar(name)-2),"pdf",sep = ""),width = 8, height = 4)
  
  predict.val <- model %>% predict(x.val, batch_size = batch_size)
  pred<-as.data.frame(cbind(as.Date(data$Fecha[(train.uindex.vec[w]+windows[w]+1):(val.uindex.vec[w]+windows[w])]),y.val,predict.val))
  colnames(pred)<-c("Fecha","Observada", "Estimada")
  pred$Fecha<-as.Date(pred$Fecha)
  pred$Observada<-pred$Observada/escalar
  pred$Estimada<-pred$Estimada/escalar
  p2<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value, color = variable))+th+
    scale_x_date(date_breaks = "6 months",
                 labels=date_format("%m/%Y"),
                 limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
    geom_line(aes(y = pred$Observada, col = "Observada"))+
    geom_line(aes(y = pred$Estimada, col = "Estimada")) + 
    labs(title="Volatilidad USD/MXN a 5 días",y="", x="",color = "Volatilidad")+
    scale_color_manual(values=c(c.blue,c.orange))
  ggexport(p2, filename = paste("Val",substr(name,1,nchar(name)-2),"pdf",sep = ""),width = 8, height = 4)
  
  predict.test <- model %>% predict(x.test, batch_size = batch_size)
  pred<-as.data.frame(cbind(as.Date(data$Fecha[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]),y.test,predict.test))
  colnames(pred)<-c("Fecha", "Observada","Estimada")
  pred$Fecha<-as.Date(pred$Fecha)
  pred$Observada<-pred$Observada/escalar
  pred$Estimada<-pred$Estimada/escalar
  p3<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value, color = variable))+th+
    scale_x_date(date_breaks = "6 months",
                 labels=date_format("%m/%Y"),
                 limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
    geom_line(aes(y = pred$Observada, col = "Observada")) + 
    geom_line(aes(y = pred$Estimada, col = "Estimada")) +     
    labs(title="Volatilidad USD/MXN a 5 días",y="", x="",color = "Volatilidad")+
    scale_color_manual(values=c(c.blue,c.orange))
  ggexport(p3, filename = paste("Test",substr(name,1,nchar(name)-2),"pdf",sep = ""),width = 8, height = 4)
  dev.off()
}

compare2<-function(op){
  temp<-rnn.error.switch(op)
  t<-as.data.frame(cbind(windows,
                         sapply(0:2,function(x) temp[1+x*3]), #MAE
                         sapply(0:2,function(x) temp[2+x*3]), #RMSE
                         sapply(0:2,function(x) temp[3+x*3]) #MAPE
  ))
  colnames(t)<-c(paste("Ventana-",op),"MAE","RMSE","MAPE")
  return(t)
}

compare<-function(op){
  temp<-list()
  switch(op,
         t1={
           temp<-list(lstm.error.train,lstm.error.val,lstm.error.test)
           label<-"LSTM"},
         t2={
           temp<-list(gru.error.train,gru.error.val,gru.error.test)
           label<-"GRU"},
         t3={
           temp<-list(garch.rnn.error.train,garch.rnn.error.val,garch.rnn.error.test)
           label<-"GARCH-GRU"},
         t4={
           temp<-list(egarch.rnn.error.train,egarch.rnn.error.val,egarch.rnn.error.test)
           label<-"EGARCH-GRU"},
         t5={
           temp<-list(e.g.rnn.error.train,e.g.rnn.error.val,e.g.rnn.error.test)
           label<-"G-E-GRU"},
         t6={
           temp<-list(garch.lstm.error.train,garch.lstm.error.val,garch.lstm.error.test)
           label<-"GARCH-LSTM"},
         t7={
           temp<-list(egarch.lstm.error.train,egarch.lstm.error.val,egarch.lstm.error.test)
           label<-"EGARCH-LSTM"},
         t8={
           temp<-list(e.g.lstm.error.train,e.g.lstm.error.val,e.g.lstm.error.test)
           label<-"G-E-LSTM"},
         stop(return("Opción inválida!")))
  t<-array(data=NA,dim = c(9,5))
  for(i in 1:3){
    for(w in 1:3){
      t[(i-1)*3+w,3:5]<-unlist(temp[[i]][[w]])
    }
  }
  t[1:3,1]<-rep("Train",3)
  t[4:6,1]<-rep("Val",3)
  t[7:9,1]<-rep("Test",3)
  t[c(1,4,7),2]<-rep(5,3)
  t[c(1,4,7)+1,2]<-rep(10,3)
  t[c(1,4,7)+2,2]<-rep(22,3)
  t<-as.data.frame(t)
  colnames(t)<-c(paste("Set-",label,sep = ""),"Window","MAE","RMSE","MAPE")
  return(t)
}

prediction.sigma<-function(fit,w=1,val=T,t=1){
  spec = getspec(fit)
  setfixed(spec) <- as.list(coef(fit))
  if(val){
    pred = ugarchforecast(spec, n.ahead = t, n.roll = 0, data = rend[1:train.uindex.vec[w]], out.sample = 0)
  }else{
    pred = ugarchforecast(spec, n.ahead = t, n.roll = 0, data = rend[1:val.uindex.vec[w]], out.sample = 0)
  }
  pred.sigmas = sigma(pred)
  return(pred.sigmas)
}

prediction.fitted<-function(fit,w=1,val=T,t=1){
  spec = getspec(fit)
  setfixed(spec) <- as.list(coef(fit))
  if(val){
    pred = ugarchforecast(spec, n.ahead = t, n.roll = 0, data = rend[1:train.uindex.vec[w]], out.sample = 0)
  }else{
    pred = ugarchforecast(spec, n.ahead = t, n.roll = 0, data = rend[1:val.uindex.vec[w]], out.sample = 0)
  }
  pred.fitted = fitted(pred)
  return(pred.fitted)
}

prediction.sigma.1d<-function(fit,w=1,val=T,t=1){
  spec = getspec(fit)
  setfixed(spec) <- as.list(coef(fit))
  sigmas<-c()
  if(val){
    for(i in 1:t){
      pred = ugarchforecast(spec, n.ahead = 1, n.roll = 0, data = rend[1:(train.uindex.vec[w]+i-1)], out.sample = 0)
      sigmas[i]<-sigma(pred)
    }
  }else{
    for(i in 1:t){
      pred = ugarchforecast(spec, n.ahead = 1, n.roll = 0, data = rend[1:(val.uindex.vec[w]+i-1)], out.sample = 0)
      sigmas[i]<-sigma(pred)
    }
  }
  pred.sigmas <- sigmas
  return(pred.sigmas)
}

prediction.fitted.1d<-function(fit,w=1,val=T,t=1){
  spec = getspec(fit)
  setfixed(spec) <- as.list(coef(fit))
  fitteds<-c()
  if(val){
    for(i in 1:t){
      pred = ugarchforecast(spec, n.ahead = 1, n.roll = 0, data = rend[1:(train.uindex.vec[w]+i-1)], out.sample = 0)
      fitteds[i]<-fitted(pred)
    }
  }else{
    for(i in 1:t){
      pred = ugarchforecast(spec, n.ahead = 1, n.roll = 0, data = rend[1:(val.uindex.vec[w]+i-1)], out.sample = 0)
      fitteds[i]<-fitted(pred)
    }
  }
  pred.fitteds <- fitteds
  return(pred.fitteds)
}

predict.rnn<-function(modelname,n.head,data.x,data.x.fut=NA,ts=3){
  model<-load_model_hdf5(modelname)
  forecasts<-c()
  if(is.na(data.x.fut)){
    nvars<-1
    nvars.x<-1
    index.y<-1
    data.x<-array(matrix(rep(data.x,21),ncol = 3,byrow = T),dim=c(21,ts,1))
    for(i in 1:n.head){
      data.x<-array(matrix(rep(data.x[1,,],21),ncol=3,byrow=T),dim=c(21,ts,1))
      predicted <- model %>% predict(data.x, batch_size = 21)
      data.x<-array(data = cbind(data.x[,-1,],rep(predicted[1,],21)), dim = c(21,ts, 1))
      forecasts[i]<-predicted[1,]
    }
  }else{
    nvars<-dim(data.x)[2]
    nvars.x<-nvars
    index.y<-nvars
    data.x<-array(matrix(rep(data.x,21),ncol = 3,byrow = F),dim=c(21,ts,nvars.x))
    for(i in 1:n.head){
      data.x<-array(matrix(rep(data.x[1,,],21),ncol=1,byrow=T),dim=c(21,ts,nvars.x))
      predicted <- model %>% predict(array(matrix(rep(data.x,21),nrow=3,byrow=T),dim=c(21,ts,nvars.x)), batch_size = 21)
      data.x<-array(data = cbind(data.x[,-1,],rep(predicted[1,],21)), dim = c(21,ts, 1))
      forecasts[i]<-predicted[1,]
    }
  }
  return(forecasts)
}
