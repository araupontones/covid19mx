

library(covidMex)
library(tidyverse)
library(lubridate)


#devtools::install_github("pablorm296/covidMex")
setwd("C:/Users/andre/Dropbox/Andres/03.Dashboards/11. Coronavirus_graphs")

?covidWWSituation


#leer data para el mundo
world <- covidWWSituation() 


#casos de espana y Mexico
spain_mx1 <- world %>%
  filter(pais_territorio %in% c("Spain", "Mexico"))

spain_mx2 <- spain_mx1 %>%
  
  #transformar fecha a formato correcto
  mutate(Fecha = ymd(fecha_corte)) %>%
  
  #contar casos cumulativos por pais
  group_by(pais_territorio) %>%
  arrange(pais_territorio, Fecha) %>%
  mutate(Casos = cumsum(casos_nuevos),
         Casos_log = log(Casos)) %>%
  #mantener solamente desde el dia que se llegaron a os 10 casos
  filter(Casos > 9) %>%
  select(pais_territorio, Fecha,dia, casos_nuevos, Casos, Casos_log) %>%
  filter(row_number()<=21) %>%
  mutate(Dia = row_number()) 

#%>%
  
  #crear dia desde el dia 10
#  mutate(Dia = (paste("Día", row_number()))) 
  



#spain_mx2$Dia <- factor(spain_mx2$Dia, levels= unique(spain_mx2$Dia))


  
#establecer estilo y elementos de la tabla

      caption = "Creado por @AndresArau\n Datos de CSSE obtenidos a través del paquete covidMX (Reyes,2020) \n y el repositorio covid19_mex(Guzmán, 2020)."


      #numero de ticks en eje x
      keep = c(1, 5, 10,15, 20)
   
      
      
      #mostrar solamente las labels para los dias 15, 18 y 21
      dtalabel = spain_mx2 %>%
        filter(Dia %in% c(15, 18, 21))
      
      
      
      
#crear grafico
      
      ggplot(spain_mx2,
             aes(Dia,
                 Casos,
                 colour = pais_territorio)
      ) +
        geom_point(
          size =1) +
        geom_point(size = 2,
                   fill="white") +
        geom_line() +
      
        geom_label(data = dtalabel, 
                   aes(label= Casos),
                   group = 1,
                   show.legend = F) +
        
        
        theme_minimal()+
        theme(text = element_text(family="Poppins",
                                  color = "#333333",
                                  size = 14),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_text(family = "Poppins",
                                       size = 10),
              legend.position="top",
              legend.title = element_blank(),
              plot.caption = element_text(size = 8,face = "italic"),
              plot.title = element_text(size=13,
                                        face = "bold"),
              plot.subtitle = element_text(size = 11),
              
              
        ) +
        scale_color_manual(values=c("#FF6F69","#006847"))+
        scale_shape_discrete(labels=c("Espana", "Mexico")) +
        
        labs(title = "Comparación de la evolución de casos confirmados entre Mexico y España",
             subtitle = "Desde el día que se confirmó el caso 10 en cada país",
             x = "Días",
             y =NULL,
             caption = caption
        )

       y el re
