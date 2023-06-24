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
lista <- lapply(1:10,HTML)
data_opt <- do.call(rbind,lista)
str(data_opt)
summary(data_opt)

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

# Guardar datos -----------------------------------------------------------
library(data.table)
getwd()
ls()
#saveRDS(data_opt,"df_raw.rds")
fwrite(data_opt,"df_raw.csv")
