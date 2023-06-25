# Limpieza -----------------------------------------------------
rm(list=ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training
df <- import("https://github.com/iapaezg/BD_LM_01/raw/main/stores/df_raw.rds")
str(df)
df <- df %>% select((directorio:y_total_m_ha)) #Se elimina la primera columna


# Crea variable hijos y pareja -----------------------------------------------------
df <- df %>% mutate(hijo=case_when(p6050==3~1))
df <- df %>% replace_na(list(hijo=0)) # Reemplazar na por 0
df <- df %>% mutate(pareja=case_when(p6050==2~1))
df <- df %>% replace_na(list(pareja=0)) # Reemplazar na por 0
hijo_hog <- df %>%
  group_by(directorio,secuencia_p) %>% 
  summarize(hijos=sum(hijo))
skim(hijo_hog$hijos)
hijo_hog <- hijo_hog %>% mutate(t_hijo=case_when(hijos!=0~1))
hijo_hog <- hijo_hog %>% replace_na(list(t_hijo=0))

h_df <- df %>% 
  full_join(hijo_hog,by=c("directorio","secuencia_p"))
names(h_df)
h_df$hijos[h_df$p6050>=3] <- 0
h_df$t_hijo[h_df$p6050>=3] <- 0
df <- h_df
skim(df$age)

# Seleccionar mayores de 18 y variables finales -----------------------------------------------
age_df<-df %>% filter(age>=18) #Seleccion de mayores e iguales a 18
summary(age_df$age)
skim(age_df$age)

# Selección de las variables de interés
names(age_df)
var <- c("directorio","secuencia_p", "orden", "clase", "estrato1","sex",
         "age","oficio","maxEducLevel","cotPension","p6050","relab",
         "dsi","formal","pea","totalHoursWorked","y_ingLab_m",
         "y_ingLab_m_ha","y_otros_m","y_salary_m","y_salary_m_hu",
         "y_total_m","y_total_m_ha","hijos","t_hijo","p6210s1")
w_df <- age_df %>% select(var,starts_with("iof"),starts_with("ingtot"))

# Luego de revisar la definición de las variables priorizadas, debido a que la var ingtot incluye
# el ingreso observado e imputado de las fuentes:ingreso monetario primera actividad (impa), ingreso segunda actividad (isa),
# ingreso en especie (ie), ingreso monetario desocupados e inactivos (imdi) e ingresos provenientes de otras fuentes 
# no laborales (iof) (intereses, pensiones, ayudas, cesantias, arriendos y otros) .
# Inspección de variables
# Se elimnan las var cotPension dado que se incluye en formal
w_df <- w_df %>% select(-starts_with("iof"),-starts_with("y_"),-ingtotes,-ingtotob,-cotPension)
skim(w_df)
w_df <- w_df %>% rename(urbano=clase) # Se cambia el nombre de la variable
w_df <- w_df %>% rename(desempleado=dsi)
w_df <- w_df %>% rename(posicion=p6050)
w_df <- w_df %>% rename(educ=p6210s1)
w_df <- w_df %>% replace_na(list(relab=0))

# Cambiar los NA de relab, oficio (sin relab, sin oficio) + formal
w_df <- w_df %>% replace_na(list(oficio=0,formal=0)) # Reemplazar na por 0
skim(w_df)

# Crear variables de ingreso/hora -----------------------------------------
# Se eliminan datos de ingreso=0, dado que no dan información
w_df <- w_df %>% 
  filter(ingtot>0,maxEducLevel>=0)
skim(w_df)

# Ingreso de ingtot se determinó que era mensual por lo cual se procedió a calcular el ingreso/hora
w_df <- w_df %>% mutate(hora_ing=ingtot/4.28/48)
w_df <- w_df %>% mutate(hora_ingrep=ingtot/4.28/totalHoursWorked)
skim(w_df)
plot(w_df$totalHoursWorked,w_df$ingtot)
w_df <- w_df %>% select(-hora_ingrep,-totalHoursWorked)
skim(w_df)

# Crea la variable ln(ingreso) + edad2 + años de educación + experiencia
w_df <- w_df %>% mutate(ln_income=log(hora_ing))
w_df <- w_df %>% mutate(age2=age^2)
w_df <- w_df %>% mutate(exp=age-educ-5)
w_df <- w_df %>% mutate(exp2=exp^2)
skim(w_df)
boxplot(w_df$exp)

# Eliminar outliers y visualizar la variable respuesta
boxplot(w_df$ln_income)
boxplot.stats(w_df$ln_income)$out
19800-2296
w_df$z_ln <- scale(w_df$ln_income)
hist(w_df$z_ln)
summary(w_df$z_ln)
w_df <- w_df %>% 
  mutate(z_ln=abs(z_ln)) %>% 
  filter(z_ln<3) %>% 
  select(directorio:exp2)
boxplot(w_df$ln_income)
skim(w_df)
saveRDS(w_df,"data_final.rds")