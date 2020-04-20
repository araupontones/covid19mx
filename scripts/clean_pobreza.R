library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(leaflet)


raw_dir = file.path("data/1.raw")
clean_dir = file.path("data/2.clean")

##indicadores de pobreza
file = file.path(raw_dir, "pobreza2015.xlsx")

pobreza = readxl::read_excel(file, sheet = "in")


##make varnames consistent 
pobreza = pobreza %>%
  rename(MUNICIPIO_RES_ID= clave_municipio) %>%
  mutate(MUNICIPIO_RES_ID=if_else(str_count(MUNICIPIO_RES_ID)==4,
                                  paste0("0", MUNICIPIO_RES_ID),
                                  as.character(MUNICIPIO_RES_ID))
  )

##export 

write_rds(pobreza, file.path(clean_dir,"pobreza.rds"))
