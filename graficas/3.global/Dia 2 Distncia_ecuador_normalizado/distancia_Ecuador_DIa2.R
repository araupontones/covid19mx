

#Grafica de relacion entre casos confirmados y distancia al ecuador. Normalizando dias desde caso 100
#https://cran.r-project.org/web/packages/wbstats/wbstats.pdf

library(covidMex)
library(tidyverse)
library(lubridate)
library(CoordinateCleaner)
library(wbstats)

#devtools::install_github("pablorm296/covidMex")
setwd("C:/Users/andre/Dropbox/Andres/03.Dashboards/10.Coronavirus/graficas")
Sys.setlocale("LC_TIME", "Spanish")



#leer datos de poblacion del banco mundial
population<-wb(indicator = c("SP.POP.TOTL"), mrv = 1)

population <- population %>%
  group_by(country) %>%
  arrange(date) %>%
  rename(Population = value,
         iso2 = iso2c) %>%
  select(iso2, country, Population)
  
population$iso2[population$country=="United Kingdom"] <- "UK"
population$iso2[population$country=="Greece"] <- "EL"


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
  rename(iso2 = geo_id,
         population = popData2018) %>%
  group_by(pais_territorio) %>%
  #casos por pais
  mutate(Casos = cumsum(casos_nuevos)) %>%
  #mantener despues del dia que se contaron 100
  filter(Casos>=100) %>%
  mutate(Dia = row_number(),
         logCasos= log(Casos)) %>%
  filter(Dia == 20)




#juntar casos con country ref para obtener los centroides
world2 <- plyr::join(casos_totales, countryref, by="iso2", type="left")
world2 <-  plyr::join(world2, population, by="iso2", type="left")



world2$name[world2$name=="Italy"] <- "Italia"
world2$name[world2$name=="United States"] <- "E.U"
world2$name[world2$name=="Spain"] <- "España"
world2$name[world2$name=="Germany"] <- "Alemania"
world2$name[world2$name=="Iran"] <- "Iran"
world2$name[world2$name=="United Kingdom"] <- "GB"
world2$name[world2$name=="France"] <- "Francia"
world2$name[world2$name=="Switzerland"] <- "Suiza"
world2$name[world2$name=="Belgium"] <- "Belgica"
world2$name[world2$name=="Norway"] <- "Noruega"
world2$name[world2$name=="Netherlands"] <- "Países Bajos"
world2$name[world2$name=="Ireland"] <- "Irlanda"

	




#remover paises que no existen en reference (son muy pocos, 4)
world3 <- world2 %>%
  filter(!is.na(centroid.lon)) %>%
  #calculate distance to the equator 
  mutate(lat_tocalculate = abs(centroid.lat),
         CasosPop = (100000 *Casos)/Population)%>%
  arrange(CasosPop) %>%
  ungroup() %>%
  select(name, iso2, Casos, logCasos, CasosPop, lat_tocalculate, Population, centroid.lat) %>%
  filter(CasosPop <300)



                   

#crear datos para label (solamente los 8 paises con mas casos)
keep = seq(from=nrow(world3)-12,
                 to= nrow(world3),
                 by=1)
  





tema =  theme(
  legend.position = "none",
  text=element_text(family="Garamond"),
  plot.title = element_text(size =16),
 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(family = "Poppins",
                           size = 10),
  axis.title = element_text(family = "Poppins",
                            size = 12,
                            color = "#404040"),
  
  
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
         y = CasosPop))+
    geom_point(aes(size = CasosPop,
                   color= CasosPop >100))+
     
  #linea vertical al grado 30
  geom_vline(xintercept=30) +
    geom_text(aes(x=28.5, 
                  y=200,
                  label="30°"),
              fontface="bold",
              size = 4,
              family = "Poppins",
              color="#686868"
                  ) +
  #linea vertical al grado 60
  geom_vline(xintercept=60) +
  
  geom_text(aes(x=61.4, 
                y=200,
                label="60°"),
            fontface="bold",
            size = 4,
            family = "Poppins",
            color="#686868"
  ) +
  
  #label de paises
    geom_text(data=dataLabel,
              aes(label=name),
              nudge_x =2,
              nudge_y = 4,
              size =3,
              check_overlap = TRUE,
              #fontface="normal",
              family = "Poppins",
              color="#686868") +
  geom_smooth(se=F,
              color="#A0A0A0",
              method = "lm") +
    xlab("Grados del ecuador") +
    ylab("Casos confirmados por 100,000 habs.") +
  theme_minimal()+
    tema +
  scale_color_manual(values=c("#3b6978", "#eb4559"))+
    labs(title = "Distancia con el ecuador y casos confirmados por COVID-19 en el mundo",
         subtitle = "Casos acumulados por 100,000 habitantes 20 días después de que cada país confirmara su caso número 100",
         caption= "@AndresArau\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)")
         
    
 
########################################################
#Hemisferio sur

dataLabelsouth = world3 %>%
  ungroup() %>%
  filter(centroid.lat<0) 

ggplot(data = world3 %>%
         filter(centroid.lat<0),aes(
  x= lat_tocalculate,
  y = CasosPop))+
  
  geom_point(aes(size = CasosPop),
             color='#3b6978')+
  
  #linea vertical al grado 30
  geom_vline(xintercept=30) +
  geom_text(aes(x=28.5, 
                y=200,
                label="30°"),
            fontface="bold",
            size = 4,
            family = "Poppins",
            color="#686868"
  ) +
  #linea vertical al grado 60
  geom_vline(xintercept=60) +
  
  geom_text(aes(x=61.4, 
                y=200,
                label="60°"),
            fontface="bold",
            size = 4,
            family = "Poppins",
            color="#686868"
  ) +
  
  #label de paises
  geom_text(data=dataLabelsouth,
            aes(label=name),
            nudge_x =3,
            nudge_y = 4,
            size =3,
            check_overlap = TRUE,
            #fontface="normal",
            family = "Poppins",
            color="#3b6978") +
  
  xlab("Grados del ecuador") +
  ylab("Casos confirmados por 100,000 habs.") +
  theme_minimal()+
  tema +
  scale_color_manual(values=c("#3b6978"))+
  labs(title = "Distancia con el ecuador y casos confirmados por COVID-19 en el hemisferio sur",
       subtitle = "Casos acumulados por 100,000 habitantes 20 días después de que cada país confirmara su caso número 100",
       caption= "@AndresArau\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)"
       )


###################################################
#Emisferio norte
ggplot(data = world3 %>%
         filter(centroid.lat>0),aes(
  x= lat_tocalculate,
  y = CasosPop))+
  geom_point(aes(size = CasosPop,
                 color= CasosPop >100))+
  
  #linea vertical al grado 30
  geom_vline(xintercept=30) +
  geom_text(aes(x=28.5, 
                y=200,
                label="30°"),
            fontface="bold",
            size = 4,
            family = "Poppins",
            color="#686868"
  ) +
  #linea vertical al grado 60
  geom_vline(xintercept=60) +
  
  geom_text(aes(x=61.4, 
                y=200,
                label="60°"),
            fontface="bold",
            size = 4,
            family = "Poppins",
            color="#686868"
  ) +
  
  #label de paises
  geom_text(data=dataLabel,
            aes(label=name),
            nudge_x =2,
            nudge_y = 4,
            size =3,
            check_overlap = TRUE,
            #fontface="normal",
            family = "Poppins",
            color="#686868") +
  geom_smooth(se=F,
              color="#A0A0A0",
              method = "lm") +
  xlab("Grados del ecuador") +
  ylab("Casos confirmados por 100,000 habs.") +
  theme_minimal()+
  tema +
  scale_color_manual(values=c("#3b6978", "#eb4559"))+
  labs(title = "Distancia con el ecuador y casos confirmados por COVID-19 en el hemisferio norte",
       subtitle = "Casos acumulados por 100,000 habitantes 20 días después de que cada país confirmara su caso número 100",
       caption= "@AndresArau\nDatos de CSSE via el paquete covidMX (Reyes,2020 y el repositario covid19_mex(Guzman,2020)")



