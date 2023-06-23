# Limpieza -----------------------------------------------------
rm(list=ls())
setwd("~/OneDrive - Universidad de los andes/0. BIG DATA/repositorios/BD_LM_01/stores")
require(tidyverse)

data_lim <- readRDS("df_raw.rds")
summary(data_lim$age)
age_data<-data_lim %>% subset(data_lim$age>=18)















##### -> Ojo esto no lo corran estoy intentando unas cosas
#age_data<-data_lim %>% filter(data_lim$age>=18)
age_data
summary(age_data$age)
var <- c("directorio","secuencia_p", "orden", "clase", "estrato1", "sex", "age" %>% 
         "p6050","relab","oficio","age","clase","college","maxEducLevel","cotPension" %>% 
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