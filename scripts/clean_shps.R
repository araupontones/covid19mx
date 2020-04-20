##clean shapefiles at municipal level mexico

library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(leaflet)


raw_dir = file.path("data/1.raw")
clean_dir = file.path("data/2.clean")



##read data
shape_file = file.path(raw_dir,"mex_admbnda_adm2_govmex","mex_admbnda_adm2_govmex.shp")

municipios = sf::st_read(shape_file)
municipios2 = rmapshaper::ms_simplify(municipios) ##simplified versions




##clean municipios
municipios = municipios2 %>%
  mutate(MUNICIPIO_RES_ID = str_remove(ADM2_PCODE, "MX")) %>% ##make it consistent with COVEVAL'S data
  rename(Nombre = ADM2_ES,
         ENTIDAD_RES_ID = ADM1_PCODE) %>% ##rename name of District
  select(MUNICIPIO_RES_ID, Nombre, geometry, ENTIDAD_RES_ID)




centroids_mun = st_centroid(municipios) ##centroids


##export

write_rds(municipios, file.path(clean_dir,"municipios.rds"))
write_rds(centroids_mun, file.path(clean_dir,"centroids_municipios.rds"))


##test shapefile quality 
leaflet(municipios2) %>%
  addPolygons(weight = .4,
              color = "gray") %>%
  addCircleMarkers(data= centroids_mun,
                   radius = 1,
                   color = "red")




#write_rds(municipios, "covid19mx/app/municipios.rds")
#write_rds(centroids_mun, "covid19mx/app/centroids_mun.rds")

casos_municipio = tabla_casos_covid %>%
  filter(RESULTADO=="Positivo SARS-CoV-2") %>%
  group_by(MUNICIPIO_RES,MUNICIPIO_RES_ID) %>%
  summarise(Casos=n()) %>%
  rename(Municipio = MUNICIPIO_RES)











##bis por pobreza
pobreza$bins = cut(pobreza$pobreza,
                   breaks = c(0,20,40,60,80,100))


##centroids 


                   


##pobreza shape 
pobreza_shape = merge(x=municipios,y=pobreza, by="MUNICIPIO_RES_ID", type="left")



##get cound of cases by municipio

casos_municipio = tabla_casos_covid %>%
 filter(RESULTADO=="Positivo SARS-CoV-2") %>%
  group_by(MUNICIPIO_RES_ID) %>%
  summarise(Casos=n())




casos_shape = merge(x=casos_municipio,y=centroids_mun, by="MUNICIPIO_RES_ID", type="left")

##test map 
library(rcartocolor)
colores= rcartocolor::cartocolors

pal = carto_pal("Mint",n=5)

ggplot() +
  geom_sf(data = pobreza_shape,
          color = NA, ##removes borders of polygons
          aes(fill=bins)
          ) +
  
  geom_sf(data=casos_shape,
             aes(size=Casos),
          color = alpha("red",0.4),
          fill=alpha("red",0.2)) +
  
  scale_fill_brewer(palette = pal) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        
        axis.text.y = element_blank(),
        axis.text.x = element_blank())


  coord_sf(crs = " +proj=longlat +datum=WGS84 +no_defs ")

  plot(pobreza_shape["bins"])

##revisar si las variables matchean
  Sys.setlocale("LC_TIME", "Spanish")
##map de casos por mnuicipio
  ggplot() +
    geom_sf(data = pobreza_shape,
            color = "#e9ebf1",
            fill="#f9f9fb" ##removes borders of polygons
            
    ) +
    
    geom_sf(data=casos_shape,
            aes(size=Casos),
            color = alpha("red",0.4),
            fill=alpha("red",0.2),
            show.legend = "point") +
    
    scale_fill_brewer(palette = pal) +

    labs(title="Total de casos confirmados por COVID19 en cada municipio",
         caption = "Mapa: @AndresArau\n
         Datos:https://www.gob.mx/salud/documentos/datos-abiertos-152127") +
    theme_minimal() +
    theme(panel.grid.major = element_line(colour = 'transparent'),
          panel.grid.minor = element_line(colour = 'transparent'),
          
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.position = c(0.23,0.16),
          legend.direction="horizontal",
          legend.title = element_text(face = "bold"),
          text = element_text(family = "sans" ),
          plot.title = element_text(size = 20, hjust = 0.5))
  
  
table(tabla_casos_covid$UCI)
