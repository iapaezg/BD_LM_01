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
       ggplot2)# Classification And REgression Training

p_load(car,tidyverse,fixest, stargazer,knitr,kableExtra,jtools,ggstance,broom,broom.mixed,skimr)
df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")

## gender-wage profile --------------------------------------------------------
# 4.a Ingreso-hombre
X <- df %>% 
  select(sex)
y <- df %>% 
  select(ln_income) %>% 
  rename(y=ln_income)
dat=cbind(X,y)
mod_sex <- lm(y~.,data=dat,x=TRUE)
stargazer(mod_sex,type="text",dep.var.labels = c("Modelo ingreso hora con el género"),
          digits = 4, out="mod_sex.txt")
mod_sex_r <- rlm(y~.,data=dat)
stargazer(mod_sex_r,type="text",dep.var.labels = c("Modelo ingreso hora con el género"),
          digits = 4, out="mod_sex.txt")

# 4.b Ingreso-hombre
cols <- c("estrato1","oficio","posicion","relab")
df[cols] <- lapply(df[cols],factor)
names(df)
skim(df)
str(df)
reg_df_sex <- df %>% 
  select(ln_income,sex,estrato1,age,age2,exp,educ,t_hijo,oficio,relab)
reg_sexc <- lm(ln_income~sex+educ+estrato1+age+age2+exp+t_hijo+oficio+relab,data=reg_df_sex)
stargazer(reg_sexc,type="text",dep.var.labels=c("OLS"),digits=4)

# Existe posible colinealidad entre años de educación y años de experiencia
library(dplyr)
reg_df_sex %>% select(-ln_income) %>% cor(method="pearson") %>% round(digits=2) -> mat_cor
install.packages("corrplot")
library(corrplot)
corrplot(mat_cor, type="upper", tl.col="black", tl.srt=45)

# Test VIF
reg_sexc <- lm(ln_income~sex+educ+estrato1+age+age2+exp+t_hijo+oficio+relab,data=reg_df_sex)
car::vif(reg_sexc) # No se puede correr porque tenemos variables categóricas en el modelo

# FWL
reg_FWL_sex <- lm(ln_income~sex,data=reg_df_sex)
reg_FWL_con <- lm(ln_income~.,data=reg_df_sex)
stargazer(reg_FWL_sex,reg_FWL_con,type="text",digits=4,omit=c("educ","estrato1","age","age2","exp","t_hijo","oficio","relab"),
          dep.var.labels=c("OLS"),out="mod_sex_controles.txt")

reg_df_sex <- reg_df_sex %>% 
  mutate(res_sex_c=lm(sex~educ+estrato1+age+age2+exp+t_hijo+oficio+relab)$residuals) %>% #Residuos sex~controles
  mutate(res_ln_c=lm(ln_income~educ+estrato1+age+age2+exp+t_hijo+oficio+relab)$residuals) #Residuos ingreso~controles

reg_res <- lm(res_ln_c~res_sex_c,reg_df_sex)
stargazer(reg_FWL_sex,reg_FWL_con,reg_res,type="text",digits=4,omit=c("educ","estrato1","age","age2","exp","t_hijo","oficio","relab"),
          dep.var.labels=c("OLS"),out="mod_sex_controles.txt")

# Bootstrap FWL
set.seed(2023)
p_load("boot")
rFWL_boot <- reg_df_sex %>% 
  select(ln_income:relab)
names(regFWL_boot)

FWL_fn <-function(data,index){
  rFWL_boot <- subset (reg_df_sex, select = c(ln_income:relab))
  data<-data %>% mutate(res_sex_c=lm(sex~educ+estrato1+age+age2+exp+t_hijo+oficio+relab)$residuals) #Residuos sex~controles
  data<-data %>% mutate(res_ln_c=lm(ln_income~educ+estrato1+age+age2+exp+t_hijo+oficio+relab)$residuals) #Residuos ingreso~controles 
  coef(lm(res_ln_c~res_sex_c,data = data, subset = index))[2]  # Regreso res ing_c ~ res sex~controles, tomo b1
}
FWL_fn(rFWL_boot,1:nrow(rFWL_boot))
FWboot <- boot(rFWL_boot,FWL_fn,R=1000)
FWboot

# 4c-Plot edad-salario por género
# Hombres
set.seed(2023)
rFWL_boot <- reg_df_sex %>% 
  select(ln_income:relab)
rFWL_bootM <- rFWL_boot[rFWL_boot$sex==1,]
str(rFWL_bootM)
reg_male <- lm(ln_income~age+age2,data=rFWL_bootM)
stargazer(reg_male,type="text",digits=4,dep.var.labels = "Hombre",dep.var.caption = "Ln Ingresos")
coefM <- reg_male$coefficients
rFWL_bootM <- rFWL_bootM %>% 
  mutate(ing_m=predict(reg_male))
str(rFWL_bootM)
plot(rFWL_bootM$age,rFWL_bootM$ing_m,xlab="Edad (años)",ylab="ln ingreso/hora")

#Valor maximo
mal_max <- function(data,index){
  data <- data[index,]
  coef_mal <- (lm(ln_income~age+age2,data=data))$coefficients
  b1 <- coef_mal[2]
  b2 <- coef_mal[3]
  edad_max <- b1/(-2*b2)
  return(edad_max)
}
mal_max(rFWL_bootM,1:nrow(rFWL_bootM))
boot_male <- boot(rFWL_bootM,mal_max,R=1000)
boot_male
boot.ci(boot.out = boot_male,type=c("norm"))
plot(rFWL_bootM$age,rFWL_bootM$ing_m,xlab="Edad (años)",ylab="ln ingreso/hora")
abline(v=50.0164,col="green")

# Mujeres
set.seed(2023)
rFWL_boot <- reg_df_sex %>% 
  select(ln_income:relab)
rFWL_bootM <- rFWL_boot[rFWL_boot$sex==0,]
str(rFWL_bootM)
reg_female <- lm(ln_income~age+age2,data=rFWL_bootM)
stargazer(reg_female,type="text",digits=4,dep.var.labels = "Mujer",dep.var.caption = "Ln Ingresos")
coefM <- reg_female$coefficients
rFWL_bootM <- rFWL_bootM %>% 
  mutate(ing_m=predict(reg_female))
str(rFWL_bootM)
plot(rFWL_bootM$age,rFWL_bootM$ing_m,xlab="Edad (años)",ylab="ln ingreso/hora")

#Valor maximo
mal_max <- function(data,index){
  data <- data[index,]
  coef_mal <- (lm(ln_income~age+age2,data=data))$coefficients
  b1 <- coef_mal[2]
  b2 <- coef_mal[3]
  edad_max <- b1/(-2*b2)
  return(edad_max)
}
mal_max(rFWL_bootM,1:nrow(rFWL_bootM))
boot_male <- boot(rFWL_bootM,mal_max,R=1000)
boot_male
boot.ci(boot.out = boot_male,type=c("norm"))

plot(rFWL_bootM$age,rFWL_bootM$ing_m,xlab="Edad (años)",ylab="ln ingreso/hora")
abline(v=43.1892,col="green")

# Regresiones
skim(reg_df_sex)
stargazer(reg_male,reg_female,type="text",digits=4,dep.var.caption = "Ln Ingresos",
          out="reg_sex.txt")