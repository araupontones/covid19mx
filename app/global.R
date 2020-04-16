
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
library(httr)
library(readxl)


##Parameters -----------------------------------------------------------------------------------------------------------



Sys.setlocale("LC_TIME", "Spanish")
#fuente="obtenidos a través del paquete <i>covidMex</i> (Reyes,2020) y el repositario <i>covid19_mex</i> (Guzmán, 2020)"
fuente="obtenidos a través de www.gob.mx y limpiados por @AndresArau"
INEGI = "| Los datos de población INEGI. Censo de Población y Vivienda 2010"

#parameters

hora = "20:00 (CST)"
colorH="steelblue"
colorM="#652D90"



##function to read rds files from github
get_rds <- function(url, rds_file) {
  
  RawData <- GET(url) #Sucess!!
  
  
  tempdir = tempdir()
  rdsfile=file.path(tempdir, rds_file)
  
  #open connection to write data in download folder
  filecon <- file(rdsfile, "wb") 
  #write data contents to download file!!
  writeBin(RawData$content, filecon) 
  #close the connection
  close(filecon)
  
  t = readRDS(rdsfile)
  
  return(t)
  
  
}


## Download data from  -------------------------------------------------------------------------------------------------

##Andres Arau : https://github.com/araupontones
## Fecha: 14 de abril (Dia de la Republica!)
##Objetivo 1: Descargar datos publicados gob.mx/datos abiertos/datos_abiertos_covid19.zip
##Objetivos 2": Leer tabla de casos en formato csv
##Objetivo 3: Leer diccionario de variables
##Objetivo final: Tener una base de datos limpia con varibles en formato factor que permitan el analisis de la tabla publicada



###1.Definir parametros para descargar, unzip y leer archivo de casos ---------------------------------------------------------

#link = "https://www.gob.mx/salud/documentos/datos-abiertos-152127"
href = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip" 
csv = "COVID19_Mexico_"
zip_ = "casos.zip"
tempdir = tempdir()
zip_dir=file.path(tempdir, zip_)
unzip_dir = file.path(tempdir, "Casos_confirmados")


##Parametros para descarga de diccionario
href_dicc = "http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"
excel = "Catalogos"
zip_dicc = "Diccionario_COVID.zip"
zip_dicc_dir = file.path(tempdir, zip_dicc)
unzip_dicc_dir = file.path(tempdir, "Diccionario")



###2.Descargar el arhivo .zip de casos ------------------------------------------------------------------------------

RawData <- GET(href) #gobtener directorio zip
filecon <- file(zip_dir, "wb")  ##oabrir conexion con directorio temporal
writeBin(RawData$content, filecon)  ###descargar directorio zip en directorio temporal!!
close(filecon) ##cerrar conexion


##unzip el directorio en directorio temporal
unzip(zip_dir, overwrite = T,
      exdir = unzip_dir)

#leer el CSV de casos confirmados
casos_df = read_csv(file.path(unzip_dir,
                              list.files(unzip_dir, pattern = ".csv")[1])
)





###3.Descargar el arhivo .zip de diccionario ---------------------------------------------------------------------------------------

RawData <- GET(href_dicc) #gobtener directorio zip
filecon <- file(zip_dicc_dir, "wb")  ##oabrir conexion con directorio temporal
writeBin(RawData$content, filecon)  ###descargar directorio zip en directorio temporal!!
close(filecon) ##cerrar conexion



##unzip el directorio en directorio temporal
unzip(zip_dicc_dir, overwrite = T,
      exdir = unzip_dicc_dir)

##capturar nombre del directorio que contiene el diccionario
diccionario = file.path(unzip_dicc_dir,
                        list.files(unzip_dicc_dir)[which(str_detect
                                                         (list.files(unzip_dicc_dir), excel)
                        )])


##tabs del archivo
tabs = diccionario %>%
  readxl::excel_sheets() 

lista_diccionarios = lapply(tabs, function(x){
  
  tabla = read_excel(diccionario, sheet = x)
  names(tabla) <- stringi::stri_trans_general(names(tabla),"Latin-ASCII")
  
  return(tabla)
  
})

##corregir nombres de los catalogos para nombrar elementos de la lista
names_tabs = str_remove_all(tabs,"Catálogo |de " )

##nombrar elementos de la lista
names(lista_diccionarios) <- names_tabs


###4. Crear look up tables para re-labear los factores de Municipio y Estado de residencia ---------------------------------------

##look up de entidades y municipios
entidades= lista_diccionarios$ENTIDADES
municipios = lista_diccionarios$MUNICIPIOS %>%
  mutate(MUN_CLAVE = paste0(CLAVE_ENTIDAD, CLAVE_MUNICIPIO)) ##crear identificacion pod Municipio



##CREAR ID DE MUNICIPIOS
casos_df = casos_df %>%
  mutate(MUNICIPIO_RES = paste0(ENTIDAD_RES,MUNICIPIO_RES)) 
#%>%
# filter(MUNICIPIO_RES!="19066") #Eliminar inconsistencia de NL (envie tweet para confirmar)


##levels y labels de entidad 
levels_entidad = entidades$CLAVE_ENTIDAD
label_entidad = entidades$ENTIDAD_FEDERATIVA


##levels and labels de municipio
label_municipios = municipios$MUNICIPIO
levels_municipos = paste0(municipios$CLAVE_ENTIDAD,municipios$CLAVE_MUNICIPIO)

##levels and labels tipo paciente
label_paciente = lista_diccionarios$TIPO_PACIENTE$DESCRIPCION
level_paciente = lista_diccionarios$TIPO_PACIENTE$CLAVE

##levels and labels tipo sector
label_sector = lista_diccionarios$SECTOR$DESCRIPCION
level_sector = lista_diccionarios$SECTOR$CLAVE

##levels and labels origen
label_origen = lista_diccionarios$ORIGEN$DESCRIPCION
level_origen = lista_diccionarios$ORIGEN$CLAVE

##levels and labels SINO
label_sino = lista_diccionarios$SI_NO$DESCRIPCION
level_sino = lista_diccionarios$SI_NO$CLAVE

##funcion para variables sino
sino = function(x){factor(x,
                          levels = level_sino,
                          labels = label_sino)}


##variables sino
sino_vars = c("INTUBADO", "NEUMONIA", "EMBARAZO", 
              "HABLA_LENGUA_INDI", "DIABETES", 
              "EPOC", "ASMA", "INMUSUPR","HIPERTENSION",
              "OTRA_CON","CARDIOVASCULAR", "OBESIDAD",
              "RENAL_CRONICA","TABAQUISMO", "OTRO_CASO",
              "MIGRANTE", "UCI")



###5. convertir variables en factores utilizando diccionario ------------------------------------------------------------------------

tabla_casos_covid = casos_df %>%
  mutate(ENTIDAD_NAC = factor(ENTIDAD_NAC,
                              levels = levels_entidad,
                              labels = label_entidad),
         ENTIDAD_UM = factor(ENTIDAD_UM,
                             levels = levels_entidad,
                             labels = label_entidad),
         ENTIDAD_RES = factor(ENTIDAD_RES,
                              levels = levels_entidad,
                              labels = label_entidad),
         MUNICIPIO_RES_ID = MUNICIPIO_RES,
         MUNICIPIO_RES = factor(MUNICIPIO_RES,
                                levels = levels_municipos,
                                labels = label_municipios),
         SEXO = factor(SEXO,
                       levels = c("1","2", "99"),
                       labels = c("MUJER", "HOMBRE", "NO ESPECIFICADO")),
         RESULTADO = factor(RESULTADO,
                            levels = c("1","2","3"),
                            labels = c("Positivo SARS-CoV-2","No positivo SARS-CoV-2","Resultado pendiente")),
         NACIONALIDAD = factor(NACIONALIDAD,
                               levels = c("1", "2", "99"),
                               labels = c("MEXICANA", "EXTRANJERA", "NO ESPECIFICADO")),
         TIPO_PACIENTE = factor(TIPO_PACIENTE,
                                levels = level_paciente,
                                labels = label_paciente),
         SECTOR = factor(SECTOR,
                         levels = level_sector,
                         labels = label_sector),
         ORIGEN = factor(ORIGEN,
                         levels = level_origen,
                         labels = label_origen)
  ) %>%
  ##variables si_no
  mutate_at(sino_vars, sino)





### 6. Remover elementos utilizados durante la limpieza y descarga de la tabla--------------------------------------------------------
rm(casos_df, entidades, lista_diccionarios, municipios, RawData)

##manterner solamente casos confirmados Y hacer tabla consistente con version previa
table = subset(tabla_casos_covid, RESULTADO =="Positivo SARS-CoV-2") %>%
  rename(Edad = EDAD,
         Sexo = SEXO,
         Fecha_reporte = FECHA_ACTUALIZACION,
         Fecha = FECHA_INGRESO,
         Estado = ENTIDAD_RES) %>%
  mutate(Sexo = if_else(Sexo=="MUJER", "F", "M"),
         Defuncion = if_else(is.na(FECHA_DEF),0,1),
         Estado = str_to_title(Estado)
         ) %>%
  arrange(Fecha) %>% ##tomando la fecha de ingreso como fecha
  mutate(Caso = row_number())



##grupos de edad
dfTables =table %>%
  mutate(Grupo = case_when(Edad %in% c(0:9) ~ "0-9", #crear grupos de edad
                           Edad %in% c(10:19) ~ "10-19",
                           Edad %in% c(20:29) ~ "20-29",
                           Edad %in% c(30:39) ~ "30-39",
                           Edad %in% c(40:49) ~ "40-49",
                           Edad %in% c(50:59) ~ "50-59",
                           Edad %in% c(60:69) ~ "60-69",
                           Edad %in% c(70:79) ~ "70-79",
                           Edad >79 ~ "80+"
  ))




## Crear diasData -----------------------------------------------------------------------------------------------------

diasData = table %>%
  group_by(Fecha) %>%
  summarise(casos_nuevos= n(),
            decesos = sum(Defuncion)) %>%
  mutate(Casos = cumsum(casos_nuevos),
         Casoslog=log(Casos),
         Dia = row_number()) %>%
  rename(Muertos = decesos,
         Nuevos = casos_nuevos) %>%
  arrange(Dia) %>%
  mutate(fecha= paste(day(Fecha), month(Fecha, label = T))
  )


muertos = diasData %>%
  select(Muertos, Fecha) %>%
  rename(DefuncionesMasQAyer= Muertos) %>%
  mutate(Defunciones = cumsum(DefuncionesMasQAyer))


## Read data from github ---------------------------------------------------------------------------------------------
#options(encoding = 'UTF-8')

##table of cases
# url ="https://github.com/araupontones/covid19mx/blob/master/app/table.rds?raw=true"
# rds_file = "table.rds"
# dfTables = get_rds(url = url, rds_file = "table.rds")

##table of days
# url ="https://github.com/araupontones/covid19mx/blob/master/app/diasData.rds?raw=true"
# rds_file = "diasData.rds"
# diasData = get_rds(url = url, rds_file = "diasData.rds")

##table of muertos 
url ="https://github.com/araupontones/covid19mx/blob/master/app/muertos.rds?raw=true"
rds_file = "muertos.rds"
muertos = get_rds(url = url, rds_file = "muertos.rds")

##table of poblacion
url ="https://github.com/araupontones/covid19mx/blob/master/app/poblacion.rds?raw=true"
rds_file = "poblacion.rds"
poblacion = get_rds(url = url, rds_file = "poblacion.rds")
                  
##sapefile

url ="https://github.com/araupontones/covid19mx/blob/master/app/shapefile.rds?raw=true"
rds_file = "shapefile.rds"
shapefile = get_rds(url = url, rds_file = "shapefile.rds")




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
last = max(dfTables$Fecha_reporte)
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
  group_by(Estado) %>%
  summarise(Casos = n(),
            Defunciones = sum(Defuncion)) %>%
  ungroup()


table(dfTables$Estado)
sum(casosEstados$Casos)


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




