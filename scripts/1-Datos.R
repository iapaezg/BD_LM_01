# Scrapear ----------------------------------------------------------------
require(pacman) #Contiene p_load()
p_load(tidyverse,rvest) #tidy: ggplot -- rvest webscrapping
library(rvest)

# Extracción de datos -----------------------------------------------------
# Este loop crea un objeto para cada chunk y lo une en una lista
HTML <- function(page){
  url<-read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",page,".html")) %>%
  html_table()
  url <- url[[1]] #No sé si es un cuadrado o un paréntesis
}
lista <- lapply(1:2,HTML)
data <- do.call(rbind,lista)
data

# Forma lenta -------------------------------------------------------------
for (i in 1:10) {
  url<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  my_html<-read_html(url)
  my_table<-my_html %>% html_table()
  assign(paste0("data_",i),as.data.frame(my_table))
}
# Unir los datos
dataf_lenta<-bind_rows(data_1,data_2,data_3,data_4,data_5,data_6,data_7,data_8,data_9,data_10)
summary(dataf_lenta)

url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html"


txt <- c("Hallo", "World")

# Extracción de datos -----------------------------------------------------
# Este loop cre un objeto para cada chunk

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

# BORRADOR-NO CORRER ------------------------------------------------------
for (i in 1:10) {
  url<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  my_html<-read_html(url)
  my_table<-my_html %>% html_table()
  assign(paste0("data_",i),as.data.frame(my_table))
}

dataf<-bind_rows(data_1,data_2,data_3,data_4,data_5,)
summary(data1)
  
   url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  
  
  url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
  my_html<-read_html(url)
  class(my_html)
  View(my_html)
  my_table<-my_html %>% html_table()
  head(my_table)
  length(my_table)
  class(my_table)
  data1<-as.data.frame(my_table)
  str(data1)
}
url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
my_html<-read_html(url)
class(my_html)
View(my_html)
my_table<-my_html %>% html_table()
head(my_table)
length(my_table)
class(my_table)
data1<-as.data.frame(my_table)
str(data1)

url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
my_html<-read_html(url)
class(my_html)
my_table<-my_html %>% html_table()
head(my_table)
length(my_table)
class(my_table)
data2<-as.data.frame(my_table)
str(data2)

df<-bind_rows(data1,data2)
head(df)
vignette("rvest")
# browseURL(url)

class(my_html)
View(my_html)

a <- 5
for (i in 1:10) {
  cat(i,"\n")
  if (i==a) cat("i vale 5\n")
}

my_table<-my_html %>% html_table()
head(my_table)
length(my_table)
class(my_table)
data1<-as.data.frame(my_table)
install.packages("jsonlite")
library(jsonlite)
urljson<-"https://ignaciomsarmiento.github.io/assets/js/bootstrap.min.js"
data<-fromJSON(urljson)
my_html %>% html_element("h2")