
library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(rcartocolor)
library(extrafont)


##set directory
mapa_dir= file.path("graficas/4.mapas_mexico/mapa_originarios")

##read data
originarios_poly <- read_rds(file.path(mapa_dir, "originarios_poly.rds"))
originarios_centroids <- read_rds(file.path(mapa_dir, "originarios_centroids.rds"))

colorind = "#1781A9"
colorNoind = "#81C7E1"
colorNotes = "#858585" 

tema <- theme(plot.title = element_text(size = 24, face = "bold"),
              plot.subtitle =  element_text(size = 20, colour = colorNotes),
              legend.position = c(.84,.84),
              legend.direction = "horizontal",
              legend.title = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 12),
              legend.title.align = 0,
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()
)



##porcentaje de pobreza
base = ggplot() +
  geom_sf(data = originarios_poly,
          color = alpha("black",0.1), ##removes borders of polygons
          aes(fill=as.factor(grupo)),
          size =.1
  ) +
  coord_sf(crs= "+proj=longlat +datum=WGS84") +

  scale_fill_manual(name="Municipios con mayoría",
                    breaks=c("Noind","ind"),
                    labels = c("No indígena", "Indígena"),
                    values= c(colorNoind, colorind)) +
   guides(fill = guide_legend(title.position = "top")) +
   tema
  


base

##puntos de municipios con por lo menos un test

  mapa = base +
    geom_point(data=originarios_centroids %>%
               filter(Tests>0),
             aes(x = long,
                 y=lat
                 ),
             size = .4,
             color = "red"
             ) +
  ##legend para municipios con un caso
  geom_point(aes(x =-107.4,
                 y= 15.96),
             color = "red",
             size = 3) +
  
  geom_text(aes(x = -112.3,
                y = 16,
                label= "Por lo menos un test realizado"),
            size = 5,
            fontface="bold") +
  
  labs(title = "Municipios en los que se ha realizado al menos un test de coronavirus",
       #subtitle = "En los municipios azules claros, 50% o más de las personas hablan alguna lengua indígena",
       caption = "@AndresArau\n21 de abril, 2020\nDatos:https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico\nIndicadores Socioeconómicos de los Pueblos Indígenas de México, 2015" )+
  tema
  
  mapa
  
  exfile = file.path(mapa_dir,"mapa_covid_originarios.png")

  ggsave(exfile, mapa, dpi = 200, width = 14, height = 9)

  
