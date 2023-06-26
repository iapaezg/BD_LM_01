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

p_load(car,tidyverse,fixest, stargazer,knitr,kableExtra,jtools,ggstance,broom,broom.mixed,skimr)
df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")

## gender-wage profile --------------------------------------------------------

X <- df %>% 
  select(sex)
y <- df %>% 
  select(ln_income) %>% 
  rename(y=ln_income)
dat=cbind(X,y)
mod_sex <- lm(y~.,data=dat,x=TRUE)
stargazer(mod_sex,type="text",dep.var.labels = c("Modelo ingreso hora con el género"),
          digits = 4, out="mod_sex.txt")

# Determina y gorro y residuos del modelo
y_hat <- fitted(mod_sex)
res <- resid(mod_sex)
dat_p <- cbind(dat,res,y_hat)

# Grafica el estimado de la función
ggplot(dat_p) +
  geom_point(aes(x=sex,y=y_hat)) +
  ggtitle("Relación género-ingresos") +
  labs(x="Género", y="Predicción ln ingresos/hora")

# Analisis de residuos
qqnorm(res)
qqline(res)
plot(res,dat$y,ylab = "Ln Ingreso/hora",xlab = "Residuos")
residualPlots(mod_sex)


# Bootstraping ------------------------------------------------------------
p_load(boot)
#boot(data,statistic,R)
#Funcion
set.seed(2023)
beta_fn <- function(formula,data,indices){
  d <- data[indices,]
  fit <- lm(formula,data=d)
  return(coef(fit))
  }
reps <- boot(data=dat,statistic=beta_fn,R=1000,formula=y~.)
reps

# Valor máximo
set.seed(2023)
R <- 1000
reg_sex <- rep(0,R)
for(i in 1:R) {
  sample <- sample_frac(dat,size=1,repace=TRUE)
  f <- lm(y~.,sample)
  coefs <- f$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  reg_sex[i] <- b1/(-2*b2)
}
histogram(reg_age)
max(reg_sex)


