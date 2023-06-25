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

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret,
       stargazer,
       ggplot2)# Classification And REgression Training

# Descriptiva con paquetes https://bookdown.org/wadetroberts/r-you-ready-for-r/descriptive-statistics-and-data-visualization.html
req <- substitute(require(x, character.only = TRUE))
libs<-c("psych", "tidyverse", "table1", "patchwork")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})
#

df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")

# Descriptivas PENDIENTE------------------------------------------------------------
summary(df)
skim(df)
s_descrip <- skim(df)
save()
psych::describe(df)
names(df)
summary2 <- skim(df)
class(summary2)
df_tbl <- as.data.frame(summary2)
df <- df %>% 
  table1(~urbano+estrato1+sex+oficio+maxEducLevel+posicion+relab+desempleado+formal+t_hijo+pea)
table1(~age+exp+ln_income,data=df)
df <- df %>% 
  factor(list(urbano,estrato1,sex,oficio,maxEducLevel,posicion,relab,desempleado,formal,t_hijo))

# Descriptiva -------------------------------------------------------------
boxplot(df$ln_income,ylab="Ln ingreso/hora")
boxplot(df$hora_ing,ylab="Ingreso/hora")
boxplot(df$age,ylab="Edad (años)")
boxplot(df$educ,ylab="Educación (años)")
boxplot(df$exp,ylab="Experiencia potencial (años)")
histogram(df$estrato1,xlab="Estrato",ylab="Porcentaje",col="grey")
histogram(df$maxEducLevel,xlab="Máximo nivel educativo",ylab="Porcentaje",col="grey")
histogram(df$hijos,xlab="Número de hijos",ylab="Porcentaje",col="grey")
histogram(df$relab,xlab="Relación laboral",ylab="Porcentaje",col="grey")
histogram(~hijos|sex,data=df,layout=c(2,1),col="gray",xlab="Número de hijos",ylab="Porcentaje")