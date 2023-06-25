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

# Descriptivas ------------------------------------------------------------
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
pie(df$sex)

hijo_sex <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=hijos,group=as.factor(sex),fill=as.factor(sex)))
hijo_sex
hijo_sex + scale_fill_manual(values=c("0"="red","1"="blue"),label=c("0"="Mujer","1"="Hombre"),name="Género")
ggplot(data=df,mapping=aes(x=hijos,y=sex))
ggplot(df,aes(x=hijos,color=sex)) +
  geom_histogram(fill="white")

boxplot(df$ln_income~df$sex,ylab="Ln ingreso/hora",xlab="Género",)


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
