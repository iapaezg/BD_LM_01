# Cargue datos -----------------------------------------------------
rm(list=ls())
if(!require(pacman)) install.packages(pacman) ; require(pacman)
require(pacman)
p_load(car,tidyverse,fixest, stargazer,knitr,kableExtra,jtools,ggstance,broom,broom.mixed,skimr)

df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")
df <- df %>% 
  select(estrato1:exp2)
str(df)

# Definiendo las columnas que son factores
cols <- c("estrato1","sex","oficio","maxEducLevel","posicion","relab",
          "desempleado","formal","pea","t_hijo")
df[cols] <- lapply(df[cols],factor)
names(df)
skim(df)

df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/data_final.rds")

table(df$hijos,df$sex)
# Age-wage profile --------------------------------------------------------
X <- df %>% 
  select(age,age2)
y <- df %>% 
  select(ln_income) %>% 
  rename(y=ln_income)
dat=cbind(X,y)
mod_age <- lm(y~.,data=dat,x=TRUE)
stargazer(mod_age,type="text",dep.var.labels = c("Modelo ingreso hora con la edad"),
          digits = 4, out="mod_age.txt")

# Determina y gorro y residuos del modelo
y_hat <- fitted(mod_age)
res <- resid(mod_age)
dat_p <- cbind(dat,res,y_hat)

# Grafica el estimado de la función
ggplot(dat_p) +
  geom_point(aes(x=age,y=y_hat)) +
  ggtitle("Relación edad-ingresos") +
  labs(x="Edad (años)", y="Predicción ln ingresos/hora")


# Analisis de residuos
qqnorm(res)
qqline(res)
plot(res,dat$y,ylab = "Ln Ingreso/hora",xlab = "Residuos")
residualPlots(mod_age)

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

# Intervalos de confianza




ggplot(dat, aes(y = y, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ X', method = lm, se = FALSE, 
              size = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "Edad (años)",  
       y = "Ln Ingresos",
       title = "Valores predichos de ingresos por hora dada la edad") # labels

hijo_sex <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=hijos,group=as.factor(sex),fill=as.factor(sex)))
hijo_sex
hijo_sex + scale_fill_manual(values=c("0"="red","1"="blue"),label=c("0"="Mujer","1"="Hombre"),name="Género")
ggplot(data=df,mapping=aes(x=hijos,y=sex))
ggplot(df,aes(x=hijos,color=sex)) +
  geom_histogram(fill="white")

boxplot(df$ln_income~df$sex,ylab="Ln ingreso/hora",xlab="Género",c(2,1))


stargazer(df)
ls()
summary(age_df$age)
boxplot(age_df$age)
names(df)

ARTofR::xxx_title2('your title')
ARTofR::xxx_title3('your sub-title')

df <- df %>% rename(ID_base=df[1])
#age_data<-df %>% filter(df$age>=18)
sum(is.na(df$age))
df %>% count(df$age)
skim(df$age)


###IVAN - - - - - - ## -> Ojo esto no lo corran estoy intentando unas cosas
#age_data<-data_lim %>% filter(data_lim$age>=18)
url_data <- RCurl::getURL("https://github.com/iapaezg/BD_LM_01/blob/d93cc92081ff0bd94f293e937f09b12676d6c29f/stores/df_raw.rds")
df <- load(url(url_data))

data_lim <- readRDS("df_raw.rds")
summary(data_lim$age)
age_data<-data_lim %>% subset(data_lim$age>=18)


age_data
summary(age_data$age)
var <- c("directorio","secuencia_p", "orden", "clase", "estrato1", "sex", "age" %>% 
           "p6050","oficio","age","clase","college","maxEducLevel","cotPension" %>% 
           "dsi","formal","informal","pea","pet","wap","totalHoursWor_d","y_ingLab_m" %>% 
           "y_ingLab_m_ha","y_otros_m","y_salary_m","y_salary_m_hu","y_total_m","y_total_m_ha")
df <- age_data %>% filter(var,stars_with("iof"),stars_with("ingtot"))
dsi
estrato1
fex_c
fex_dpto
formal
informal
oficio
relab
pea---poblacion economicamente activa
pet---poblacion en edad de trabjar
wap-- poblacion en edade de trabajar
ocu
iof----------todos
ingtot ----- todos
p6050
directorio
secuenciap
orden
clase
dominio
sex
totalHoursWor_d
y_ingLab_m
y_ingLab_m_ha
y_otros_m
y_salary_m
y_salary_m_hu
y_total_m
y_total_m_ha
)
df <- subset(age_data,select=)

table(dataf$dominio)
table(dataf$secuencia_p)
boxplot(dataf$age)
summary(dataf$age)
dataf %>%
  mutate(id_hogares=paste0(dataf$directorio,dataf$secuencia_p))
dataf %>%
  count(dataf$directorio,dataf$secuencia_p)
dataf %>%
  group_by(directorio,secuencia_p) %>%
  summarize(count=n())
dataf %>%
  group_by(directorio) %>%
  summarize(count=n())
summary(dataf$age)
summary(dataf$sex) # sex	=1 male, =0 female
table(dataf$sex)
age_data<-subset(dataf,dataf$age>=18)
summary(age_data$age)
str(age_data)
ls(age_data)
