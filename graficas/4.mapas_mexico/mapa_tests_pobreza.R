##clean shapefiles at municipal level mexico

library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(rcartocolor)

##set paths
clean_dir= file.path("data","2.clean")
downloadGit = "https://github.com/araupontones/covid19mx/blob/master/downloads/download_datos_abiertos.R?raw=TRUE"



##read data
municipios = read_rds(file.path(clean_dir,"municipios.rds"))
centroids_mun = read_rds(file.path(clean_dir,"centroids_municipios.rds"))
pobreza = read_rds(file.path(clean_dir,"pobreza.rds"))
#source(downloadGit)





##contar numero de tests por municipio
test_municipio = tabla_casos_covid %>%
  filter(!is.na(MUNICIPIO_RES)) %>%
  group_by(MUNICIPIO_RES,MUNICIPIO_RES_ID) %>%
  summarise(Tests=n()) %>%
  rename(Municipio = MUNICIPIO_RES) %>%
  ungroup() %>%
  select(-Municipio)

pobreza = pobreza %>%
  select(MUNICIPIO_RES_ID, pobreza, poblacion) %>%
  mutate(pobreza= round(as.numeric(pobreza), digits = 0)) %>%
  filter(!is.na(pobreza))


test_pobreza = left_join(pobreza, test_municipio, by="MUNICIPIO_RES_ID", type="left") %>%
  mutate(Tests=as.numeric(Tests),
         Tests = if_else(is.na(Tests),0,Tests),
         poblacion =as.numeric(poblacion),
         Test_mil = round((Tests*1000)/poblacion, digits = 1)
  )
  

##bis por pobreza
test_pobreza$bins = cut(test_pobreza$Tests,
                   breaks = c(-1,0,1,50,100, 1600))

test_pobreza$binsP = cut(test_pobreza$pobreza,
                        breaks = c(0,20,40,60,80,100))
 
str(test_pobreza)
  
table(test_pobreza$binsP)


##join data with shape file
pobreza_shape = sf::st_sf(merge(x=test_pobreza,y=municipios, by="MUNICIPIO_RES_ID", type="left"))
tests_shape =sf::st_sf(merge(x=test_pobreza,y=centroids_mun, by="MUNICIPIO_RES_ID", type="left"))




##colores
colores= rcartocolor::cartocolors

pal = carto_pal("SunsetDark",n=4)
pal = carto_pal("OrYel",n=4)
pal = carto_pal("Sunset",n=5)
pal = carto_pal("BurgYl",n=5)
pal = carto_pal("RedOr",n=5)
pal = carto_pal("Mint",n=5)
pal = carto_pal("TealGrn",n=5)




tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_blank()
  )



##porcentaje de pobreza
ggplot() +
  geom_sf(data = pobreza_shape,
          color = alpha("white",0.2), ##removes borders of polygons
          aes(fill=as.factor(binsP)),
          size =.1
  ) +
  scale_fill_manual(values = pal) +
tema +
  labs(title = "% de personas viviendo en pobreza")


##numero de testst

ggplot() +
  

  geom_sf(data = pobreza_shape,
          color = alpha("white",0.2), ##removes borders of polygons
          aes(fill=as.factor(bins)),
          size=.1
  ) +
  
  scale_fill_manual(values = pal)+
  tema +
  labs(title = "# de tests realizados") +
  xlab("") 


# 
#   
#   geom_sf(data=casos_shape,
#              aes(size=Casos),
#           color = alpha("red",0.4),
#           fill=alpha("red",0.2)) +
#   
#   scale_fill_brewer(palette = pal) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = 'transparent'),
#         panel.grid.minor = element_line(colour = 'transparent'),
#         
#         axis.text.y = element_blank(),
#         axis.text.x = element_blank())
# 
# 
#   coord_sf(crs = " +proj=longlat +datum=WGS84 +no_defs ")
# 
#   plot(pobreza_shape["bins"])
# 
# ##revisar si las variables matchean
#   Sys.setlocale("LC_TIME", "Spanish")
# ##map de casos por mnuicipio
#   ggplot() +
#     geom_sf(data = pobreza_shape,
#             color = "#e9ebf1",
#             fill="#f9f9fb" ##removes borders of polygons
#             
#     ) +
#     
#     geom_sf(data=casos_shape,
#             aes(size=Casos),
#             color = alpha("red",0.4),
#             fill=alpha("red",0.2),
#             show.legend = "point") +
#     
#     scale_fill_brewer(palette = pal) +
# 
#     labs(title="Total de casos confirmados por COVID19 en cada municipio",
#          caption = "Mapa: @AndresArau\n
#          Datos:https://www.gob.mx/salud/documentos/datos-abiertos-152127") +
#     theme_minimal() +
#     theme(panel.grid.major = element_line(colour = 'transparent'),
#           panel.grid.minor = element_line(colour = 'transparent'),
#           
#           axis.text.y = element_blank(),
#           axis.text.x = element_blank(),
#           legend.position = c(0.23,0.16),
#           legend.direction="horizontal",
#           legend.title = element_text(face = "bold"),
#           text = element_text(family = "sans" ),
#           plot.title = element_text(size = 20, hjust = 0.5))
#   
#   
# table(tabla_casos_covid$UCI)
