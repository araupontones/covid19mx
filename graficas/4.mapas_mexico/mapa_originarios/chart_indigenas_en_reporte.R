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
              plot.subtitle =  element_text(size = 20, colour = colorNotes),
              legend.position = c(.28,.85),
              legend.direction = "horizontal",
              legend.title = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 12),
              legend.title.align = 0,
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()
              )


#chart1 : identificados por datos oficiales

chart_reporte = ggplot(data = grupos_covid2,
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
 
  scale_fill_manual(name="% de personas en reporte del 21 de abril",
                    breaks=c("Noind","ind"),
                    labels = c("No indígena", "Indígena"),
                    values= c(colorNoind, colorind)) +
  
  labs(title = "Porcentaje de personas indígenas testadas, confirmadas y fallecidas por coronavirus",
       subtitle = "La proporción de población indígena en los tres grupos es muy baja",
       caption = "@AndresArau\n21 de abril, 2020\nDatos:https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico" )+
  tema
  
chart_reporte


exfile = file.path(mapa_dir,"chart_reporte.png")

ggsave(exfile, chart_reporte, dpi = 200, width = 14, height = 9)
