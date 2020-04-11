
library(CoordinateCleaner)
library(leaflet)
library(tabulizer)
library(tidyverse)
library(stringr)
library(htmltools)
library(ggplot2)
library(Hmisc)
library(lubridate)
library(rbokeh)

Sys.setlocale("LC_TIME", "Spanish")
fuente="obtenidos a través del paquete <i>covidMex</i> (Reyes,2020) y el repositario <i>covid19_mex</i> (Guzmán, 2020)"
fuente="obtenidos a través de www.gob.mx y limpiados por @AndresArau"
INEGI = "| Los datos de población INEGI. Censo de Población y Vivienda 2010"

#parameters

hora = "20:00 (CST)"
colorH="steelblue"
colorM="#652D90"

 


#options(encoding = 'UTF-8')



dfTables = read_rds("table.rds")
diasData = read_rds("diasData.rds")
muertos = read_rds("muertos.rds")
poblacion = read_rds("poblacion.rds")
shapefile = read_rds("shapefile.rds")


###############################################################################

#function to translate


english_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
spanish_months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")


to_spanish_dict <- spanish_months
names(to_spanish_dict) <- english_months

translate_date <- function(date, output_lang = "es"){
  if(output_lang == "es"){
    str_replace_all(date, to_spanish_dict)
  }
}





#---------------------------------------------
#fecha de hoy ayer
last = max(diasData$Fecha)
second_last = last - 1

fecha = paste(day(last), "de", month(last, label = T, abbr = F)) #fecha de ultimo registro
fecha = translate_date(fecha)
ayer =  paste(day(second_last), "de", month(second_last, label = T, abbr = F)) #fecha de penultimo registro
ayer = translate_date(ayer)


#boxes total casos
totalCases = nrow(dfTables)
#Percentage men and women
mujeres = paste0(round(sum(dfTables$Sexo=="F") /nrow(dfTables), digits = 2) *100,"% mujeres")
hombres = paste0(round(sum(dfTables$Sexo=="M") /nrow(dfTables), digits = 2)*100, "% hombres")

#Nuevos Casos
casosNuevos=diasData$Nuevos[nrow(diasData)]

#Dia desde inicio
DiaDesde = max(diasData$Dia)

#total muertes 

totalMuertes = max(muertos$Defunciones)

#mas muertos que ayer
muertosMasqAyer = muertos$DefuncionesMasQAyer[nrow(muertos)]


#Para mapaconfirmar que todos los nombres estan correctos
casosEstados = dfTables %>%
  group_by(Estado, centroid.lon, centroid.lat) %>%
  summarise(Casos = n()) %>%
  ungroup()





#Para chart por edad chart por grupo de edad edad------------------------------------------------------------------------------------------------------


tableGrupos2 = dfTables %>%
  group_by(Grupo, Sexo) %>%
  summarise(Casos = n()) %>%
  group_by(Sexo) %>%
  mutate(Total = sum(Casos)) %>%
  ungroup() %>%
  mutate(freq = Casos/Total,
         label = paste0(round(freq*100, digits = 0),"%"),
         label = ifelse(Grupo=="0-9", "", label),
         labelM = ifelse(Sexo=="M",label, ""),
         labelF= ifelse(Sexo=="F",label,""),
         freqM = ifelse(Sexo=="M", freq, NA),
         freqF = ifelse(Sexo=="F", freq, NA),
         Sexo = ifelse(Sexo=="F", "Mujeres", "Hombres")
  )%>%
  mutate(Grupo = factor(Grupo),
         Grupo = factor(Grupo, levels = rev(levels(Grupo)))
  ) %>%
  arrange(Grupo) 

dfmujeres = dfTables %>%
            filter(Sexo=="F") %>%
  group_by(Estado, centroid.lat, centroid.lon) %>%
  summarise(Casos = n())



#style graphs
tema_linea <- theme_minimal() +
  theme(text = element_text(family="Poppins", 
                            color = "#333333",
                            size = 14
  ),
  plot.title = element_text(size = 13.5, 
                            face = "bold", 
                            margin = margin(10,0,20,0), 
                            family="Poppins", color = "grey25"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.text.x= element_text(angle=90, hjust=1),
  axis.text = element_text(family = "Poppins")
  ) 


size_point =5
color_chart = "steelblue"
size_line = 1




