##clean shapefiles at municipal level mexico

library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(rcartocolor)

##set paths
clean_dir= file.path("data","2.clean")
mapa_dir= file.path("graficas/4.mapas_mexico/mapa_originarios")

downloadGit = "https://github.com/araupontones/covid19mx/blob/master/downloads/download_datos_abiertos.R?raw=TRUE"



##read data
municipios = read_rds(file.path(clean_dir,"municipios.rds"))
centroids_mun = read_rds(file.path(clean_dir,"centroids_municipios.rds"))
originarios = read_rds(file.path(clean_dir,"originarios.rds"))
tabla_casos_covid =read_rds(file.path(mapa_dir, "covid22marzo.rds"))

#source(downloadGit)
#write_rds(tabla_casos_covid,(file.path(mapa_dir, "covid22marzo.rds")))



##Prepar datos de tabla de covid ----------------------------------------------------------------------------

covid = tabla_casos_covid %>%
  mutate(muertes = !is.na(FECHA_DEF) & RESULTADO=="Positivo SARS-CoV-2",
         casos = RESULTADO=="Positivo SARS-CoV-2",
         tests_ind = HABLA_LENGUA_INDI=="SI", 
         casos_ind = HABLA_LENGUA_INDI=="SI" & RESULTADO=="Positivo SARS-CoV-2",
         muertes_ind = HABLA_LENGUA_INDI=="SI" & muertes==T) %>%
  group_by(MUNICIPIO_RES_ID) %>%
  summarise(Tests = n(),
            Casos = sum(casos),
            Muertes = sum(muertes),
            Tests_ind = sum(tests_ind), ##casos de indigenas y no indigenas
            Casos_ind = sum(casos_ind),
            Muertes_ind = sum(muertes_ind),
            Tests_Noind = Tests - Tests_ind,
            Casos_Noind = Casos - Casos_ind,
            Muertes_Noind = Muertes - Muertes_ind
            )
  

# table(tabla_casos_covid$HABLA_LENGUA_INDI)
# 
# sum(covid$Casos)
# sum(covid$Muertes)
# sum(covid$Tests)
# sum(covid$Tests_ind)
# sum(covid$Casos_ind)
# sum(covid$Muertes_ind)


###preparar datos de casos de covid por proporcion de personas que hablan alguna lengua indigna
##esta tabla sirve para mapear a) polygonos por grupo y bubbles para casos por 100,000 habitantes

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}


covid_originarios = plyr:: join(originarios, covid, by="MUNICIPIO_RES_ID", type="left") %>%
  mutate_all(na.zero) %>%
  mutate(hay_casos = Casos >0, ##dentificar municipios con casos
         hay_tests = Tests >0, ## con tests
         hay_muertos = Muertes >0, ## con muertos
         grupo= if_else(orig_prop>=50,"ind", "Noind"))


##join data with shape file
originarios_poly = sf::st_sf(merge(x=covid_originarios,y=municipios, by="MUNICIPIO_RES_ID", type="left"))
originarios_centroids =sf::st_sf(merge(x=covid_originarios,y=centroids_mun, by="MUNICIPIO_RES_ID", type="left"))

##get coordinates for centroids
coordinadas = data.frame(st_coordinates(originarios_centroids))  ##latitude and longitude
names(coordinadas) <- c("long", "lat")

originarios_centroids <- data.frame(originarios_centroids)%>% ## transform to data frame for geom_point
  select(- geometry) %>%
  cbind(coordinadas)


##por grupos de municipio porcentaje de personas que hablan una lengua indigena en municipio
## esta para graficar a) Numero de personas por grupo b)% de municipios sin tests

porOriginarios = covid_originarios %>%
  group_by(grupo) %>%
  summarise(municipios=n(), ##numero de municipios por grupo
            con_casos = sum(hay_casos),##numero de municipios que tienen casos de covid
            con_tests = sum(hay_tests),
            con_muertes = sum(hay_muertos),
            Casos = sum(Casos), ## numero de casos confirmados por municipio
            Tests= sum(Tests),
            Muertes = sum(Muertes),## numero de tests por municipio
            poblacion= sum(poblacion, rm.na=T), ## poblacion total de cada grupo de municipio
            mun_sin_casos = round(1- (con_casos/municipios), digits = 2), ##% de municipios que no tienen casos
            mun_sin_tests = round(1- (con_tests/municipios), digits = 2), ##% de municipios que no tienen casos
            casos_mil = round((Casos * 100000)/poblacion,digits = 1),
            tests_mil = round((Tests * 100000)/poblacion,digits = 1),## casos por 100,000 habitantes
            muertes_mil = round((Muertes * 100000)/poblacion,digits = 1)
            )
  





####reshape datasets for charting

grupos_covid = covid %>%
  select(-Casos, - Muertes, - Tests) %>%
  gather(D, valor, -1) %>%
  mutate(Dimension = str_extract(D, "Casos|Tests|Muertes"),
         Grupo = str_extract(D, "ind|Noind")
  ) %>%
  select(-D) %>%
  group_by(Dimension,Grupo) %>%
  summarise(Totales = sum(valor)) %>%
  ungroup() %>%
  mutate(Dimension= factor(Dimension,
                           levels = c("Tests", "Casos", "Muertes")))


grupos_municipios = porOriginarios %>%
  gather(Dimension, valor, -1)

write_rds(originarios_poly,file.path(mapa_dir, "originarios_poly.rds"))
write_rds(originarios_centroids,file.path(mapa_dir, "originarios_centroids.rds"))
#write_rds(porOriginarios,file.path(mapa_dir, "grupos_municipios.rds"))
write_rds(grupos_covid,file.path(mapa_dir, "grupos_covid.rds"))
write_rds(grupos_municipios,file.path(mapa_dir, "grupos_municipios.rds"))






