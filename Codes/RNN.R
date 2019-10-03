#### Particion de la muestra ####

w<-1
units<-c(6,6)
source(file="InputData.R",echo=F)
model<-NULL

# Dimensiones de los inputs

cat('Entrenamiento:', dim(x.train), '\n')
cat('Validacion:', dim(x.val), '\n')
cat('Prueba:', dim(x.test), '\n')

# Dimensiones de los outputs

cat('Entrenamiento:', dim(y.train), '\n')
cat('Validacion:', dim(y.val), '\n')
cat('Prueba:', dim(y.test), '\n')

#### Creacion del modelo ####

if(isLSTM){
  model<-build.LSTM(units,timesteps=timesteps,batch_size=batch_size,input_vars = nvars.x)
}else{
  model<-build.GRU(units,timesteps=timesteps,batch_size=batch_size,input_vars = nvars.x)
}
reset <- callback_lambda(
  on_epoch_end = function(epoch, logs = list()) model %>% reset_states()
)

summary(model)
model$count_params()

#### Entrenamiento ####

set.seed(10)
history<-model %>%
  fit(x.train, y.train, batch_size = batch_size,
      validation_data=list(x.val,y.val), epochs = epochs,
      verbose = 2, shuffle = FALSE,
      callbacks=list(reset,
                    callback_early_stopping(monitor = "val_loss", min_delta = 0,
                    patience = 10, verbose = 0, mode = c("min"),
                    baseline = NULL, restore_best_weights = T)))

#### Predicciones ####

plot(history)

predict.train<-c()
predict.val<-c()
predict.test<-c()
p1<-NULL
p2<-NULL
p3<-NULL
size<-0.5

predict.train <- model %>% predict(x.train, batch_size = batch_size)
pred<-as.data.frame(cbind(as.Date(data$Fecha[(windows[w]+timesteps):(train.uindex.vec[w]+windows[w])]),y.train,predict.train))
colnames(pred)<-c("Fecha",  "Observada","Estimada")
pred$Fecha<-as.Date(pred$Fecha)
p1<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value, color = variable))+
  scale_x_date(date_breaks = "3 years",
               labels=date_format("%Y"),
               limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
  geom_line(aes(y = pred$Observada, col = "Observada"),size=size)+
  geom_line(aes(y = pred$Estimada, col = "Estimada"),size=size) + 
  labs(title="Volatilidad USD/MXN a 5 días (Entrenamiento)",y="", x="",color = "Volatilidad")
p1

predict.val <- model %>% predict(x.val, batch_size = batch_size)
pred<-as.data.frame(cbind(as.Date(data$Fecha[(train.uindex.vec[w]+windows[w]+1):(val.uindex.vec[w]+windows[w])]),y.val,predict.val))
colnames(pred)<-c("Fecha",  "Observada","Estimada")
pred$Fecha<-as.Date(pred$Fecha)
p2<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value))+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
  geom_line(aes(y = pred$Observada, col = "Observada"),size=size)+
  geom_line(aes(y = pred$Estimada, col = "Estimada"),size=size) + 
  labs(title="Volatilidad USD/MXN a 5 días (Validación)",y="", x="",color = "Volatilidad")
p2

predict.test <- model %>% predict(x.test, batch_size = batch_size)
pred<-as.data.frame(cbind(as.Date(data$Fecha[(val.uindex.vec[w]+windows[w]+1):(test.uindex.vec[w]+windows[w])]),y.test,predict.test))
colnames(pred)<-c("Fecha",  "Observada","Estimada")
pred$Fecha<-as.Date(pred$Fecha)
p3<-ggplot(pred, aes(x=as.Date(pred$Fecha), y = value))+
  scale_x_date(date_breaks = "1 year",
               labels=date_format("%m/%Y"),
               limits = as.Date(c(min(pred$Fecha),max(pred$Fecha)))) + 
  geom_line(aes(y = pred$Observada, col = "Observada"),size=size)+
  geom_line(aes(y = pred$Estimada, col = "Estimada"),size=size) + 
  labs(title="Volatilidad USD/MXN a 5 días (Prueba)",y="", x="",color = "Volatilidad")
p3

error.train<-list()
error.val<-list()
error.test<-list()

name.temp<-name
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
name<-name.temp
if(validation) name<-paste("s",s,"-",substring(name,1,nchar(name)-3),"-train",floor(portion.train*100),".h5",sep="")
model %>% save_model_hdf5(name)
