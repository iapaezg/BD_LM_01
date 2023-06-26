# Cargue datos -----------------------------------------------------
rm(list=ls())
if(!require(pacman)) install.packages(pacman) ; require(pacman)
require(pacman)

if(!require(stargazer)) install.packages("stargazer") ; require(stargazer)
require(stargazer)

if(!require(sjmisc)) install.packages("sjmisc") ; require(sjmisc)
require(sjmisc)

if(!require(devtools)) install.packages("devtools") ; require(devtools)
require(devtools)

require(grDevices)
install.packages("MASS")
require(MASS)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret,
       stargazer,
       ggplot2)# Classification And Regression Training

p_load(car,tidyverse,fixest, stargazer,knitr,kableExtra,jtools,ggstance,broom,broom.mixed,skimr)
df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")

----------------------------------------------------------------------------------------
## a) Muestra 70% 30%, se incluye la semilla 
set.seed(2023) 

# Número de filas en la base de datos
n_rows <- nrow(df)

# Calcular el tamaño de los conjuntos de entrenamiento y prueba
train_size <- round(n_rows * 0.7)
test_size <- n_rows - train_size

# Se crea vector de números aleatorios con esto se seleccionan las filas entrenamiento y prueba
split_indices <- sample(1:n_rows, size = n_rows, replace = FALSE)

# Dividir base de datos en entrenamiento y prueba
train_data <- df[split_indices[1:train_size], ]
test_data <- df[split_indices[(train_size + 1):n_rows], ]

# Dimensiones dataframe
dim(train_data)
dim(test_data)

# Se vuelven a estimar los modelos en el script 4-1, 4-2 y uno incial con  la constante para usar como base 

# Modelo base: modelo simple con constante.
mp0<- lm(ln_income ~ 1, data = train_data)
summary(mp0)
coef(mp0)
paste("Coef:", mean(train_data$ln_income))
test_data$mp0<-predict(mp0,newdata = test_data)

# MSE modelo base (rendimiento en la predicción)
MSE_mp0<-with(test_data,mean((ln_income-mp0)^2))

# Modelo 1
mp1<- lm(ln_income ~ age + age2, data = train_data)
summary(mp1)
coef(mp1)
paste("Coef:", mean(train_data$ln_income))
test_data$mp1<-predict(mp1,newdata = test_data)

# MSE modelo  1
MSE_mp1<-with(test_data,mean((ln_income-mp1)^2))

# Modelo  2
mp2<- lm(ln_income ~ sex, data = train_data)
summary(mp2)
coef(mp2)
paste("Coef:", mean(train_data$ln_income))
test_data$mp2<-predict(mp2,newdata = test_data)

# MSE modelo 2
MSE_mp2<-with(test_data,mean((ln_income-mp2)^2))

# Modelo 3
mp3<- lm(ln_income ~ sex + educ + estrato1 + age + age2 + exp + t_hijo + oficio 
         +relab, data = train_data)
summary(mp3)
coef(mp3)
paste("Coef:", mean(train_data$ln_income))
test_data$mp3<-predict(mp3,newdata = test_data)

# MSE modelo 3
MSE_mp3<-with(test_data,mean((ln_income-mp3)^2))

# Estimaciones de regresiones por modelo
stargazer(mp0,mp1,mp2,mp3, summary = TRUE, type = "text", out="mod_5a.txt")
----------------------------------------------------------------------------------------

## b) Planteamiento de 5 modelos nuevos con especificaciones adicionales que incluya
## no-linealidades y complejidades respecto a los anteriores
  
# Modelo nuevo 1 (genero y relacion laboral)

mn1<- lm(ln_income ~ sex + relab, data = train_data)
coef(mn1)
paste("Coef:", mean(train_data$ln_income))
test_data$mn1<-predict(mn1,newdata = test_data)

# MSE modelo nuevo 1
MSE_mn1<-with(test_data,mean((ln_income-mn1)^2))

# Modelo nuevo 2 (genero, edad, educacion, relacion laboral e Interacción entre estrato y género)
mn2<- lm(ln_income ~ sex + age + age2 + educ + relab + estrato1 + sex*estrato1, data = train_data)
coef(mn2)
paste("Coef:", mean(train_data$ln_income))
test_data$mn2<-predict(mn2,newdata = test_data)

# MSE modelo nuevo 2
MSE_mn2<-with(test_data,mean((ln_income-mn2)^2))

# Modelo nuevo 3 (modelo 1 + interaccion edad y género )
mn3<- lm(ln_income ~ sex + relab + age + sex*age, data = train_data)
coef(mn3)
paste("Coef:", mean(train_data$ln_income))
test_data$mn3<-predict(mn3,newdata = test_data)

# MSE modelo nuevo 3
MSE_mn3<-with(test_data,mean((ln_income-mn3)^2))

# Modelo nuevo 4 (genero edad , edad2, relacacion labora e Interacción entre edad y género)
mn4<- lm(ln_income ~ sex + age + age2 + educ + relab + sex*age, data = train_data)
coef(mn4)
paste("Coef:", mean(train_data$ln_income))
test_data$mn4<-predict(mn4,newdata = test_data)

# MSE modelo nuevo 4
MSE_mn4<-with(test_data,mean((ln_income-mn4)^2))

# Modelo nuevo 5 (Modelo 4, quitando edad^2)
mn5<- lm(ln_income ~ sex + age + educ + relab + sex*age, data = train_data)
coef(mn5)
paste("Coef:", mean(train_data$ln_income))
test_data$mn5<-predict(mn5,newdata = test_data)

# MSE modelo nuevo 5
MSE_mn5<-with(test_data,mean((ln_income-mn5)^2))

# Estimaciones modelos nuevos
stargazer(mn1,mn2,mn3,mn4,mn5, summary = TRUE, type = "text")

# Rendimientos de predicción para los modelos (del 1 al 5) agregados:
MSE_table<-c(MSE_mp0, MSE_mp1, MSE_mp2, MSE_mp3, MSE_mn1,MSE_mn2,MSE_mn3,MSE_mn4,MSE_mn5)
x_label<-c('Modelo 0','Modelo 1', 'Modelo 2', 'Modelo 3', 'Modelo nuevo 1','Modelo nuevo 2','Modelo nuevo 3','Modelo nuevo 4', 'Modelo nuevo 5')
MSEtabla<-data.frame(x_label,MSE_table)

#Graficas de MSE (modelos 1  al 5) para su comparación
ggplot(data=MSEtabla, aes(x = x_label, y = MSE_table, group=1)) + 
  geom_line() +  
  geom_point() +
  ggtitle("MSE modelos especificados") +
  ylab("MSE") +
  xlab ("Número modelo")

#identificar que modelo tienen el MSE más bajo
ordenMSE <- MSEtabla[order(MSEtabla$MSE_table), ]
View(ordenMSE)
      

# LOOCV -------------------------------------------------------------------
----------------------------------------------------------------------------------------
  # LOOCV para modelo 3
  GEIHSO$lnexperp <- log(GEIHSO$experp)

modelLOOCV1 <- train(ln_income ~ sex + educ + estrato1 + age + age2 + exp + t_hijo + oficio 
                     +relab, 
                     data = df,
                     method = "lm",
                     trControl = trainControl(method = "LOOCV"))

# Generación de resultados 
modelLOOCV1

RMSE_modelLOOCV1<-modelLOOCV1$results
RMSE_modelLOOCV1<-RMSE_modelLOOCV1$RMSE
RMSE_modelLOOCV1<-mean(RMSE_modelLOOCV1)

view(RMSE_modelLOOCV1)
# 

