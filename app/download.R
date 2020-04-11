library(CoordinateCleaner)
library(leaflet)
library(tabulizer)
library(tidyverse)
library(stringr)
library(htmltools)
library(ggplot2)
library(Hmisc)
library(lubridate)
library(rbokeh)




#https://www.gob.mx/salud/es/archivo/documentos

#Parameters 
setwd("C:\\Users\\andre\\Dropbox\\Andres\\03.Dashboards\\10.Coronavirus")
file = file.path("data","10Abril.pdf") 
file_muertos = file.path("data", "Muertos.xlsx")
file_poblacion = file.path("data", "Poblacion.xlsx")


pages = get_n_pages(file)
penultimaPag = pages-1
namesColumns = c("Caso", "Estado", "Sexo", "Edad", "FechaSintomas", "Status")
centroids = read_rds("app/centroids.rds")


#locate_areas(file)
locate_areas(file = file, pages = 2)

#Funciton to remove Xs from strings
removeX = function(x){str_remove(x,"X")}

#read data ---------------------------------------------------------------------------------------------------------------
#pagina 1
table1 = extract_tables(file, 
                       method = "lattice", 
                       guess = F,
                       output = "data.frame",
                       pages = 1,
                       encoding = "UTF-8",
                       area =  list(
                         c(115, 38, 957, 584)
                         
                         )
                       )

table1df = table1[[1]]  #convert table 1 to dataframe
 
#fix for table 1 (DELETE IF NOT WORKING)#!!!!!!!!!!!!!!!!!!!!!


#delete empty rows
table1df = table1df[which(str_detect(table1df[,1],"[0-9]")), ]

##if the columns were missread
if(is.na(table1df[1,1])) {
  
  table1df$Status = str_extract_all(table1df[,5], "[a-zA-Z]{1,}") #split column 4
  table1df$FechaSintomas = str_remove_all(table1df[,5], "[a-zA-Z]{1,}")
  table1df <- table1df[,-5] #remove problematic column
  names(table1df) <- c("Caso", "Estado", "Sexo","Edad", "Procedencia", "FechaSintomas", "Status") #rename consistently
  
  table1df <- table1df %>%
    select(Caso, Estado, Sexo, Edad, FechaSintomas, Status, Procedencia)
  
  names(table1df) = namesColumns
  table1df = table1df %>%
    filter(!is.na(Caso))
  

##if the names were missread
      
} else if(names(table1df)[1] == "X1"){
  table1df = rbind(names(table1df), table1df)
  names(table1df) = namesColumns
  
} else {
  names(table1df) = namesColumns #name headers consisntently
  
}


##correct names
table1df = table1df %>%
  mutate_at(c("Caso", "Edad", "FechaSintomas"), removeX) %>%
  mutate(Estado= if_else(Estado =="CIUDAD.DE.M.U.00C9.XICO", "Ciudad de Mexico", Estado))



#resto de tablas hasta penultima ---------------------------------------------------------------------------
table2= extract_tables(file, 
                        method = "lattice", 
                        guess = F,
                        output = "data.frame",
                       encoding = "UTF-8",
                        pages = 2:penultimaPag,
                        area =  list(
                          c(0, 38, 939, 584)
                        )
                       )


#append table 2
organizeTable2 = function(x){
  
  print(x)
  t = table2[[x]] 
  t = t[which(str_detect(t[,1],"[0-9]")), ]
  t2 = rbind(names(t),t)  #rbind the headers to the column
  names(t2) = namesColumns #rename for consistency with p1
  
  
  
  t3 = t2 %>%
    mutate_all(str_trim) %>%
    filter(Status != "")
  
  
  
  if(x < penultimaPag) {
    
    t4 = t3
    
  }
  
  else {
    
    
    t4 = t3
    t4 = t3 %>%
      mutate(Sexo = Estado,
             Estado = str_remove(Caso,"[0-9]{1,}"),
             Caso =str_extract(Caso,"[0-9]{1,}" )
      )
  }
  
  
  t4 = t4 %>%
    mutate_at(c("Caso","Edad", "FechaSintomas"), removeX)
  
  
}


#append pages de la segunda tabla
until = penultimaPag-1
table2df = map(1:until, organizeTable2)
table2df = do.call(rbind, table2df)




#ultima pagina------------------------------------------------------------------------------
table3 = extract_tables(file, 
                        method = "lattice", 
                        guess = F,
                        output = "data.frame",
                        encoding = "UTF-8",
                        pages = pages,
                        area =  list(
                          c(0, 38, 939, 584)
                        ))



t=table3[[1]]
t2 = rbind(names(t), t) #titulo a observacion
names(t2)[1] <- "Caso" #Cambiar nombre de primera columna

t3 <- t2 %>% #limpiar Caso y Estado
  mutate(Estado = str_remove(Caso,"[0-9]{1,}"),
         Caso = stringi::stri_trans_general(Caso,"Latin-ASCII"),
         Caso = str_remove_all(Caso, '[A-Z]'),
         Caso = str_remove_all(Caso, '\\.'))%>%
         select(-X)

names(t3)<-c("Caso", "Sexo", "Edad", "FechaSintomas", "Status", "Estado") #nombrar con consistencia

table3df <- t3%>% filter(!is.na(Edad)) %>% #elimiar footing
  mutate(Estado= str_remove(Estado,"\\.")) %>%
  mutate_at(c("Caso","Edad", "FechaSintomas", "Estado"), removeX) %>%
  select(Caso, Estado, Sexo, Edad, FechaSintomas, Status) #ordenar consistentemente

rm(t,t2,t3)



#-------------------------------------------------------------------------------------------------------------------------

#Append and clean
#if table 3 no existe
  if(exists("table3df")==F){
    table =rbind(table1df, table2df)
  }  else { #si la tabla 3 existe
    table =rbind(table1df, table2df, table3df)
  }




#crete table
table =  table%>%
  mutate(Caso = as.numeric(Caso),
         Estado = str_remove(Estado, "X\\."),
         Estado = str_to_title(Estado),
         Estado = str_replace_all(Estado,"\\."," "),
         Estado = str_trim(Estado),
         Estado = str_to_title(Estado),
         Estado = stringi::stri_trans_general(Estado,"Latin-ASCII"), #
        
         )
  
  

#join with centroids
table$Estado[table$Estado=="Ciudad De Mexico"] <- "Ciudad de Mexico"
table$Estado[table$Estado=="M U 00c9 Xico"] <- "Mexico"
table$Estado[table$Estado=="Ciudad De M U 00c9 Xico"] <- "Ciudad de Mexico"
table$Estado[table$Estado=="Nuevo Le U 00d3 N"] <- "Nuevo Leon"
table$Estado[table$Estado=="Ciudad De Meico"] <- "Ciudad de Mexico"
table$Estado[table$Estado=="Tlacala"] <- "Tlaxcala"
table$Estado[table$Estado=="Michoac U 00c1 N"] <- "Michoacan"
table$Estado[table$Estado=="San Luis Potos U 00cd"] <- "San Luis Potosi"
table$Estado[table$Estado=="Meico"] <- "Mexico"
table$Estado[table$Estado=="Distrito Federal"] <- "Ciudad de Mexico"
table$Estado[table$Estado=="Oaaca"] <- "Oaxaca"




setdiff(table$Estado, centroids$Estado)

table = plyr::join(table, centroids, by="Estado", type ="left" ) #join centroids
 

table =table %>%
  mutate(Grupo = case_when(Edad %in% c(0:9) ~ "0-9", #crear grupos de edad
                           Edad %in% c(10:19) ~ "10-19",
                           Edad %in% c(20:29) ~ "20-29",
                           Edad %in% c(30:39) ~ "30-39",
                           Edad %in% c(40:49) ~ "40-49",
                           Edad %in% c(50:59) ~ "50-59",
                           Edad %in% c(60:69) ~ "60-69",
                           Edad %in% c(70:79) ~ "70-79",
                           Edad >79 ~ "80+"
                           ))


  
#join with centroids 
rm(table1, table1df, table2, table2df, centroids)

#MANUAL: dentificar dia del caso reportado
table = table %>%
  mutate(Dia =case_when(Caso %in% c(994:1094) ~ 30,
                       Caso %in% c(849:993) ~ 30,
                        Caso %in% c(728:848) ~ 29,
                        Caso %in% c(586:727)~ 28,
                        Caso %in% c(476:585) ~ 27,
                        Caso %in% c(406:475) ~ 26,
                        Caso %in% c(368:405) ~ 25,
                        Caso %in% c(317:367) ~ 24,
                        Caso %in% c(252:316) ~ 23,
                        Caso %in% c(204:251) ~ 22,
                        Caso %in% c(165:203) ~ 21,
                        Caso %in% c(119:164) ~ 20,
                        Caso %in% c(94:118) ~ 19,
                        Caso %in% c(83:93) ~ 18,
                        Caso %in% c(54:82) ~ 17,
                        Caso %in% c(42:53) ~ 16,
                        Caso %in% c(27:41) ~ 15,
                        Caso %in% c(9:26) ~ 14,
                        Caso %in% c(8:8) ~ 13,
                        Caso %in% c(7:7) ~ 12,
                        Caso %in% c(6:6) ~ 8,
                        Caso %in% c(8:8) ~ 8,
                        Caso %in% c(5:5) ~ 3,
                        Caso %in% c(4:4) ~ 2,
                        Caso %in% c(1:3) ~ 1,
                        
  )
  )

#Fix femenino and masculino (make it consistent with the system)


if("MASCULINO" %in% unique(table$Sexo)  == TRUE){
  
  table = table%>%
    mutate(Sexo = if_else(Sexo=="FEMENINO", "F", "M"))
}



table = write_rds(table,"app/table.rds")
#shapefile = read_rds("shapefile.rds")


#Count of casos by day (update manually daily) ----------------------------------------------------------------------------


diasList =list( data.frame("Fecha" ="10/04/2020","Casos"=3844, "Dia"=42),
  data.frame("Fecha" ="9/04/2020","Casos"=3441, "Dia"=41),
  data.frame("Fecha" ="8/04/2020","Casos"=3181, "Dia"=40),
  data.frame("Fecha" ="7/04/2020","Casos"=2785, "Dia"=39),
  data.frame("Fecha" ="6/04/2020","Casos"=2439, "Dia"=38),  
  data.frame("Fecha" ="5/04/2020","Casos"=2143, "Dia"=37),  
  data.frame("Fecha" ="4/04/2020","Casos"=1890, "Dia"=36),  
  data.frame("Fecha" ="3/04/2020","Casos"=1688, "Dia"=35),
  data.frame("Fecha" ="2/04/2020","Casos"=1509, "Dia"=34), 
  data.frame("Fecha" ="1/04/2020","Casos"=1378, "Dia"=33), 
  data.frame("Fecha" ="31/03/2020","Casos"=1206, "Dia"=32), 
  data.frame("Fecha" ="30/03/2020","Casos"=1094, "Dia"=31),
                           data.frame("Fecha" ="29/03/2020","Casos"=993, "Dia"=30),
                           data.frame("Fecha" ="28/03/2020","Casos"=848, "Dia"=29),
                            data.frame("Fecha" ="27/03/2020","Casos"=727, "Dia"=28),
                           data.frame("Fecha" ="26/03/2020","Casos"=585, "Dia"=27),
                            data.frame("Fecha" ="25/03/2020","Casos"=475, "Dia"=26),
                            data.frame("Fecha" ="24/03/2020","Casos"=405, "Dia"=25),
                            data.frame("Fecha" ="23/03/2020","Casos"=367, "Dia"=24),
                            data.frame("Fecha" ="22/03/2020","Casos"=316, "Dia"=23),
                            data.frame("Fecha" ="21/03/2020","Casos"=251, "Dia"=22),
                            data.frame("Fecha" ="20/03/2020","Casos"=203, "Dia"=21),
                            data.frame("Fecha" ="19/03/2020","Casos"=164, "Dia"=20),
                            data.frame("Fecha" ="18/03/2020","Casos"=118, "Dia"=19),
                            data.frame("Fecha" ="17/03/2020","Casos"=93, "Dia"=18),
                            data.frame("Fecha" ="16/03/2020","Casos"=82, "Dia"=17),
                            data.frame("Fecha" ="15/03/2020","Casos"=53, "Dia"=16),
                            data.frame("Fecha" ="14/03/2020","Casos"=41, "Dia"=15),
                            data.frame("Fecha" ="12/03/2020","Casos"=26, "Dia"=14),
                            data.frame("Fecha" ="11/03/2020","Casos"=8, "Dia"=13),
                            data.frame("Fecha" ="10/03/2020","Casos"=7, "Dia"=12),
                            data.frame("Fecha" ="9/03/2020","Casos"=7, "Dia"=11),
                            data.frame("Fecha" ="8/03/2020","Casos"=7, "Dia"=10),
                            data.frame("Fecha" ="7/03/2020","Casos"=7, "Dia"=9),
                            data.frame("Fecha" ="6/03/2020","Casos"=6, "Dia"=8),
                            data.frame("Fecha" ="5/03/2020","Casos"=5, "Dia"=7),
                            data.frame("Fecha" ="4/03/2020","Casos"=5, "Dia"=6),
                            data.frame("Fecha" ="3/03/2020","Casos"=5, "Dia"=5),
                            data.frame("Fecha" ="2/03/2020","Casos"=5, "Dia"=4),
                            data.frame("Fecha" ="1/03/2020","Casos"=5, "Dia"=3),
                            data.frame("Fecha" ="29/02/2020","Casos"=4, "Dia"=2),
                            data.frame("Fecha" ="28/02/2020","Casos"=3, "Dia"=1)
                            
)

  Sys.setlocale("LC_TIME", "Spanish")
  
#append days and create a single table
diasData = do.call(rbind, diasList) %>%
  mutate(Fecha = dmy(Fecha),
         Casoslog=log(Casos))%>%
  arrange(Dia) %>%
  mutate(Nuevos=Casos - lag(Casos),
         Nuevos = ifelse(is.na(Nuevos), Casos, Nuevos),
         fecha= paste(day(Fecha), month(Fecha, label = T)))

rm(diasList)

#poblacion

poblacion = readxl::read_xlsx(file_poblacion) %>%
  mutate(Estado = stringi::stri_trans_general(Estado,"Latin-ASCII"))
  

poblacion$Estado[poblacion$Estado=="Michoacan de Ocampo"] <- "Michoacan" #clean to make it consistent with table
poblacion$Estado[poblacion$Estado=="Coahuila de Zaragoza"] <- "Coahuila"
poblacion$Estado[poblacion$Estado=="Veracruz de Ignacio de la Llave"] <- "Veracruz"



#Muertos 

muertos = readxl::read_xlsx(file_muertos)%>%
  mutate(Fecha = ymd(Fecha),
         DefuncionesMasQAyer = Defunciones - lag(Defunciones))


write_rds(diasData, "app/diasData.rds")
#write_csv(table, "app/table.csv")
write_rds(muertos, "app/muertos.rds")
write_rds(poblacion, "app/poblacion.rds")

