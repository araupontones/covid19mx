library(devtools)
library(covidMex)
library(lubridate)
library(stringr)
library(tidyverse)

Sys.setlocale("LC_TIME", "Spanish")

#devtools::install_github("pablorm296/covidMex")
 
setwd("C:\\Users\\andre\\Dropbox\\Andres\\03.Dashboards\\10.Coronavirus")

data_dir = file.path("data")
app_dir = file.path("app")

#leer muertos
file_muertos = file.path(data_dir, "Muertos.xlsx")

#leer centroides
centroids = readRDS(file.path(app_dir,"centroids.rds"))

#leer muertos
muertos = readxl::read_xlsx(file_muertos) %>%
  mutate(Fecha = ymd(Fecha),
         DefuncionesMasQAyer = Defunciones - lag(Defunciones))


#leer casos
table = covidConfirmedMx()


#renombrar varibles y juntar con centroides 
table2 = table %>%
  
  #renombrar variables
  rename(Estado = ent,
         Caso= num_caso,
         Sexo = sexo,
         Fecha = fecha_corte,
         Edad = edad) %>%
  
  #remover acentos para juntar con centroides
  mutate(Estado = stringi::stri_trans_general(Estado,"Latin-ASCII"),
         Estado = str_trim(Estado),
         Estado = if_else(Estado=="Baja California\r\nSur","Baja California Sur", Estado)
         ) %>% 
  plyr::join(centroids, by="Estado", type ="left" ) %>%
 
  #crear variable por grupo de edad
  mutate(Grupo = case_when(Edad %in% c(0:9) ~ "0-9", #crear grupos de edad
                           Edad %in% c(10:19) ~ "10-19",
                           Edad %in% c(20:29) ~ "20-29",
                           Edad %in% c(30:39) ~ "30-39",
                           Edad %in% c(40:49) ~ "40-49",
                           Edad %in% c(50:59) ~ "50-59",
                           Edad %in% c(60:69) ~ "60-69",
                           Edad > 69 ~ "70 +")) %>%
  #fecha a formato lubridate 
  mutate(Fecha = ymd(Fecha)) %>%
  filter(!inconsistencia_omision==1) %>%
  select(Caso, Sexo, Fecha, Estado, Edad, centroid.lat, centroid.lon, Grupo)





#tabla de casos por dia----------------------------------------------------

#agrupar por fecha y contar
diasData = table2 %>%
  group_by(Fecha) %>%
  #casos por fecha
  summarise(Nuevos = n()) %>%
  ungroup() %>%
  
  #crear: casos cumulativos, logaritmo de casos, dias desde el primer caso reportado
  mutate(Casos = cumsum(Nuevos),
         Casoslog=log(Casos),
         Dia = Fecha - lag(Fecha),
         Dia = as.numeric(Dia),
         Dia = if_else(is.na(Dia), 1, Dia),
         Dia = cumsum(Dia),
         #Tasa de duplicacion
         tasa_dup = round(70 / 
                            (((Casos - lag(Casos)) / lag(Casos)) 
                             *100), digits = 1)
         
  )




#guardar tables
write_rds(diasData, file.path(app_dir,"diasData.rds"))
write_rds(table2, file.path(app_dir,"table.rds"))
write_rds(muertos, file.path(app_dir,"muertos.rds"))


#guardar tabla del mundo para Yamil

world <- covidWWSituation() 

write_csv(world, file.path(data_dir,"mundo.csv"))




