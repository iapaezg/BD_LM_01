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