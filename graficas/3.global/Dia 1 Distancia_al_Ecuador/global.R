

library(covidMex)
library(tidyverse)
library(lubridate)
library(CoordinateCleaner)

#devtools::install_github("pablorm296/covidMex")
setwd("C:/Users/andre/Dropbox/Andres/03.Dashboards/10.Coronavirus/graficas")
Sys.setlocale("LC_TIME", "Spanish")


 
#leer datos de los paises (CENTRAL INTELLIGENCE AGENCY (2014) The World Factbook, Washington, DC.)
countryref <- CoordinateCleaner::countryref %>%
  filter(type == "country") %>%
  group_by(name) %>%
  filter(row_number()==1) %>%
  select(name, iso2, centroid.lon, centroid.lat) %>%
  mutate(iso2=if_else(name=="Greece", "EL", iso2),
         iso2=if_else(name=="United Kingdom", "UK", iso2))


#leer data para el mundo
world <- covidWWSituation() 


#contar casos totales en cada pais
casos_totales = world %>%
  group_by(pais_territorio, geo_id) %>%
  summarise(Casos = sum(casos_nuevos),
            logCasos = log(Casos)) %>%
  #renombrar iso para ser concistente con country ref
  rename(iso2 = geo_id)


#juntar casos con country ref para obtener los centroides
world2 <- plyr::join(casos_totales, countryref, by="iso2", type="left")



world2$name[world2$name=="Italy"] <- "Italia"
world2$name[world2$name=="United States"] <- "E.U"
world2$name[world2$name=="Spain"] <- "España"
world2$name[world2$name=="Germany"] <- "Alemania"
world2$name[world2$name=="Iran"] <- "Iran"
world2$name[world2$name=="United Kingdom"] <- "GB"
world2$name[world2$name=="France"] <- "Francia"




#remover paises que no existen en reference (son muy pocos, 4)
world3 <- world2 %>%
  filter(!is.na(centroid.lon)) %>%
  #calculate distance to the equator 
  mutate(lat_tocalculate = abs(centroid.lat) )%>%
  arrange(Casos)



                   

#crear datos para label (solamente los 8 paises con mas casos)
keep = seq(from=nrow(world3)-7,
                 to= nrow(world3),
                 by=1)
  





tema =  theme(
  legend.position = "none",
  text=element_text(family="Garamond"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(family = "Poppins",
                           size = 10),
  axis.title = element_text(family = "Poppins",
                            size = 12,
                            color = "#404040")
  
)

#grafico con todos los paieses

#label para los 5 paises con mas casos
dataLabel = world3 %>%
  ungroup() %>%
  filter(row_number() %in% keep) 
  
  

#################################################################################
#todos los paises
ggplot(data = world3,aes(
         x= lat_tocalculate,
         y = logCasos))+
    geom_point(aes(size = Casos,
                   color= Casos >20000))+
    geom_text(data=dataLabel,
              aes(label=name),
              nudge_x = 4,
              size =3,
              #fontface="normal",
              family = "Poppins",
              color="#686868") +
  geom_smooth(se=F,
              color="#A0A0A0",
              method = "lm") +
    xlab("Grados del ecuador") +
    ylab("Log.Casos confirmados") +
  theme_minimal()+
    tema +
  scale_color_manual(values=c("#3b6978", "#eb4559"))+
    labs(title = "Distancia con el ecuador y casos confirmados por COVID-19\nen el mundo",
         caption= "@AndresArau\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)")
         
    
 
########################################################
#South

dataLabelsouth = world3 %>%
  ungroup() %>%
  filter(centroid.lat<0) %>%
  filter(Casos > 1300) 

ggplot(data = world3 %>%
         filter(centroid.lat<0),aes(
  x= lat_tocalculate,
  y = logCasos))+
  geom_point(aes(size = Casos,
                 color= Casos >1300))+
geom_text(data=dataLabelsouth,
          aes(label=name),
          nudge_x = 3,
          size =3,
          #fontface="normal",
          family = "Poppins",
          color="#686868") +
  geom_smooth(se=F,
              color="#A0A0A0",
              method = "lm") +
  xlab("Grados del ecuador") +
  ylab("Log.Casos confirmados") +
  theme_minimal()+
  tema +
  scale_color_manual(values=c("#3b6978", "#eb4559"))+
  labs(title = "Distancia con el ecuador y casos confirmados por COVID-19\nen el mundo",
       subtitle= "Países del hemisferio sur",
       caption= "@AndresArau\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)")
  




###################################################
#Emisferio norte
ggplot(data = world3 %>%
         filter(centroid.lat>0),aes(
           x= lat_tocalculate,
           y = logCasos))+
  geom_point(aes(size = Casos,
                 color= Casos >20000))+
  geom_text(data=dataLabel,
            aes(label=name),
            nudge_x = 4,
            size =3,
            #fontface="normal",
            family = "Poppins",
            color="#686868") +
  geom_smooth(se=F,
              color="#A0A0A0",
              method = "lm") +
  xlab("Grados del ecuador") +
  ylab("Log.Casos confirmados") +
  theme_minimal()+
  tema +
  scale_color_manual(values=c("#3b6978", "#eb4559"))+
  labs(title = "Distancia con el ecuador y casos confirmados por COVID-19\nen el mundo",
       subtitle= "Países del hemisferio norte",
       caption= "Creado por: @AndresArau\n\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)")



