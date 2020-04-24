library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(leaflet)


raw_dir = file.path("data/1.raw")
clean_dir = file.path("data/2.clean")

fuente = "Indicadores sociodemogáficos y económicos sobre la 
población indígena desagregada por municpio, 2010 de 
CDI creado el 2015-11-12 00:07"






##read raw data
originarios = readxl::read_xlsx(file.path(raw_dir,"originarios.xlsx"), sheet = "COMPARATIVO 2015" )

##clean and create indicators
originarios1 =originarios %>%
  select(INEGI, IPHLI5, TPOBTOT,MPO)%>%
  filter(MPO!="000") %>%
  rename(pob_orig= IPHLI5,
         poblacion = TPOBTOT,
         MUNICIPIO_RES_ID=INEGI) %>%
  mutate(orig_prop = round((pob_orig/poblacion)*100, digits = 1)) %>%
  select(-MPO)





write_rds(originarios1, file.path(clean_dir,"originarios.rds"))
