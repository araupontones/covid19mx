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


##prepara data

data_chart = grupos_municipios %>%
  filter(Dimension %in% c("casos_mil", "tests_mil","muertes_mil")) %>%
  mutate(Dimension=str_remove(Dimension,"_mil"),
         Dimension = str_to_title(Dimension),
         Dimension = factor(Dimension,
                levels = c("Tests", "Casos", "Muertes")))

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

        


chart_por_mil = ggplot(data = data_chart,
       aes(x = reorder(Dimension, desc(Dimension)),
           y = valor,
           fill = grupo) 
) +
  
  ##bars
  geom_bar(aes(y = valor),
           stat="identity",
           position = position_dodge(width = 0.6),
           width = 0.5
           
           
  ) +
  coord_flip() +
  ##labels 
  geom_text(aes(y = valor+2.9,label = valor),
            position = position_dodge(width = 0.7),
            size = 6
  ) +
  ##Dimensiones titulo
  geom_text(aes(y = -10, x = Dimension,label = Dimension),
            size = 6,
            color = colorNotes,
            hjust = 0
  ) +
  ylim(-10,60) +
  geom_vline(xintercept = 1.5,
             linetype = "dotted") +
  
  geom_vline(xintercept = 2.5,
             linetype = "dotted") +
  
  xlim(c("Muertes","Casos","Tests","")) +
  guides(fill = guide_legend(title.position = "top")) +
  
  scale_fill_manual(name="Números por 100,000 habitantes en municipios",
                    breaks=c("Noind","ind"),
                    labels = c("No indígenas", "Indígenas"),
                    values= c(colorNoind, colorind)) +
  
  labs(title = "Número de tests, casos y muertes por cada 100,000 habitantes",
       subtitle = "Los números son desproporcionadamente menores en aquellos municipios\ndonde el 50% o más de las personas hablan alguna lengua indígena",
       caption = "@AndresArau\n21 de abril, 2020\nDatos:https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico\nIndicadores Socioeconómicos de los Pueblos Indígenas de México, 2015" )+
  tema


chart_por_mil

exfile = file.path(mapa_dir,"chart_por_mil.png")

ggsave(exfile, chart_por_mil, dpi = 200, width = 14, height = 9)
