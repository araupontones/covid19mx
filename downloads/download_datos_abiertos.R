##Andres Arau : https://github.com/araupontones
## Fecha: 14 de abril (Dia de la Republica!)
##Objetivo 1: Descargar datos publicados gob.mx/datos abiertos/datos_abiertos_covid19.zip
##Objetivos 2": Leer tabla de casos en formato csv
##Objetivo 3: Leer diccionario de variables
##Objetivo final: Tener una base de datos limpia con varibles en formato factor que permitan el analisis de la tabla publicada

library(httr)
library(tidyverse)
library(readxl)


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
label_paciente = lista_diccionarios$TIPO_PACIENTE$DESCRIPCIÓN
level_paciente = lista_diccionarios$TIPO_PACIENTE$CLAVE

##levels and labels tipo sector
label_sector = lista_diccionarios$SECTOR$DESCRIPCIÓN
level_sector = lista_diccionarios$SECTOR$CLAVE

##levels and labels origen
label_origen = lista_diccionarios$ORIGEN$DESCRIPCIÓN
level_origen = lista_diccionarios$ORIGEN$CLAVE

##levels and labels SINO
label_sino = lista_diccionarios$SI_NO$DESCRIPCIÓN
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
                               labels = c("MEXICANA", "EXXTRANJERA", "NO ESPECIFICADO")),
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


                      