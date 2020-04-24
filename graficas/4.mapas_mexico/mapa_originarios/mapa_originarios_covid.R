##clean shapefiles at municipal level mexico

library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(rcartocolor)
library(extrafont)

# load fonts - every session
loadfonts(device = "win", quiet = TRUE)

mapa_dir= file.path("graficas/4.mapas_mexico/mapa_originarios")

##read data
originarios_poly <- read_rds(file.path(mapa_dir, "originarios_poly.rds"))
originarios_centroids <- read_rds(file.path(mapa_dir, "originarios_centroids.rds"))
grupos_municipios <- read_rds(file.path(mapa_dir, "grupos_municipios.rds"))
grupos_covid <- read_rds(file.path(mapa_dir, "grupos_covid.rds"))




grupos_covid2 = grupos_covid %>%
  group_by(Dimension) %>%
  mutate(perc = round(Totales/sum(Totales), digits = 2) *100,
         label = paste0(perc,"%"),
         Totales = format(Totales, big.mark = ",")
  )




colorind = "#1781A9"
colorNoind = "#81C7E1"
colorNotes = "#858585"


tema <- theme(plot.title = element_text(size = 24, face = "bold"),
              plot.subtitle =  element_text(size = 20, face = "bold", colour = colorNotes),
              legend.position = c(.28,.85),
              legend.direction = "horizontal",
              legend.title = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 12),
              legend.title.align = 0,
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank()
              )


#chart1 : identificados por datos oficiales

ggplot(data = grupos_covid2,
       aes(x = reorder(Dimension, desc(Dimension)),
           y = perc,
           fill = Grupo) 
       ) +
  
  ##bars
  geom_bar(aes(y = perc),
    stat="identity",
   position = position_dodge(width = 0.6),
   width = 0.5
    
   
    ) +
  coord_flip() +
 
  ##labels (%)
  geom_text(aes(y = perc+5,label = label),
            position = position_dodge(width = 0.7),
            size = 6
            ) +
  
  ##totales
  geom_text(aes(y = perc[2]+25,label = Totales),
            position = position_dodge(width = 0.6),
            size = 6,
            color = colorNotes
  ) +

  
##Dimensiones titulo
geom_text(aes(y = -10, x = Dimension,label = Dimension),
          size = 6,
          color = colorNotes
          ) +
  
  ##Header totals
  geom_text(aes(y = 123, x = "Tests",label = "Totales"),
            nudge_x = .5,
            size = 6,
            color = colorNotes,
            fontface ="bold"
            
  ) +
  
  
  geom_vline(xintercept = 1.5,
             linetype = "dotted") +
  
  geom_vline(xintercept = 2.5,
             linetype = "dotted") +
  
  xlim(c("Muertes","Casos","Tests",""))+
  
  
  guides(fill = guide_legend(title.position = "top")) +
 
  scale_fill_manual(name="Población",
                    breaks=c("Noind","ind"),
                    labels = c("No indígena", "Indígena"),
                    values= c(colorNoind, colorind)) +
  
  labs(title = "Porcentaje de casos reportados de coronavirus que son indígenas",
       subtitle = "La población indígena es prácticamente inexistente en el total de casos testados y confirmados\npor gobierno mexicano",
       caption = "@AndresArau\n22 de abril, 2020\nDatos:https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico" )+
  tema
  



#old-----------------------------------------------------------------------------------------------

##Estilo de tabla
tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 14, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_text(size = 12, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 10, family="Didact Gothic Regular"),
        legend.title.align = 0.1,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_blank()
  )






##colores


# pal = carto_pal("SunsetDark",n=4)
# pal = carto_pal("OrYel",n=4)
# pal = carto_pal("Sunset",n=5)
# pal = carto_pal("BurgYl",n=5)
# pal = carto_pal("RedOr",n=5)
# pal = carto_pal("Mint",n=5)
# pal = carto_pal("TealGrn",n=5)
pal = c("#B0F2BC","#257D98") #green this is the one!
#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674
#pal = c("#a8dbd9","#3b738f") #blue


##porcentaje de pobreza
mapa = ggplot() +
  geom_sf(data = originarios_poly,
          color = alpha("gray",0.2), ##removes borders of polygons
          aes(fill=as.factor(grupo)),
          size =.1
  ) +
  coord_sf(crs= "+proj=longlat +datum=WGS84") +
tema +
  scale_fill_manual(values = pal,
                    labels = c("Menos 50%", "50% o más")) +
  labs(fill= "% de personas que hablan alguna lengua originaria en el municipio",
       title ="Relacion entre tests totales de covid19 y prevalencia de poblacion indigena"
       )


mapa
mapa +
  geom_sf(data=originarios_centroids %>%
            filter(Tests > 5),
          aes(size=Tests),
          color = alpha("red",0.4),
          fill=alpha("red",0.2),
          show.legend = NULL
          ) +
  tema

  
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
