# #dias de duplicacion 
 

#importar datos "DiasDRA

seq(from = 1, to =25,by=5)
 dataLabel = diasData %>%
   #crear labels
   filter(tasa_dup %in% c(4.9, 4.3, 2.4, 2.8)) %>%
   filter(Fecha != "2020-03-22") %>%
   filter(Fecha != "2020-03-12") %>%
   mutate(label = paste(tasa_dup, "Días"))  %>%
   select(label, Fecha, tasa_dup)
 
 
 #test to label every 5 observations
 # test = diasData %>%
 #   filter(row_number() %in% seq(3,n(), by=5))






ggplot( data =  diasData,
        aes(x=Fecha,
            y = tasa_dup)
) +
  geom_point(size = 3,
             colour = "#FBA9AC",
           fill ='#FBA9AC',
          ) +

  stat_smooth(se = T,
              color ="#FE5B5D" ,
              size = 1.3) +
  geom_label(data = dataLabel,
             aes(label=label),
             nudge_x = 1,
             color ='red',
             fill = "white") +
  theme_minimal() +
  theme(text = element_text(family="Poppins",
                            color = "#333333",
                            size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Poppins"),
        plot.subtitle = element_text(size = 12,
                                     hjust = .5),

        ) +

  labs(title = "Tiempo de duplicación de casos confirmados por\n COVID19 en México",
       subtitle = "Cuántos días tardan los casos confirmados en duplicarse",
       x = NULL,
       y ="Días",
       caption = "Creado por @AndresArau\n
       Datos de la @ssalud obtenidos a través del paquete covidMX (Reyes,2020) \n y el repositorio covid19_mex(Guzmán, 2020)."
       )
# 
# 
# 
# 
# 
# 
# 
#            
#   
#  
#  
# 
# 
# 