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
library(rvest)
library(xml2)
library(httr)



##get pdf of casos confirmados -----------------------------------------------------------------------------------

##scrap html of of gob.mx 
url = "https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicado-tecnico-diario-238449"
site = read_html(url)

##get all elements with href
hrefs =  site %>% 
  html_node("body") %>%
  xml_find_all("//@href")

##get link where the pdf is located
link = paste0("https://www.gob.mx",html_text(hrefs[which(str_detect(hrefs,"casos_positivos"))]))

##create directory and file name to download file
file = "casos_confirmados.pdf"
tempdir = tempdir()
pdfFile=file.path(tempdir, file)


##save pdf file in temp directory
RawData <- GET(link) #get file


filecon <- file(pdfFile, "wb")  ##open connection to write data in download folder
writeBin(RawData$content, filecon)  ###write data contents to download file!!
close(filecon) ##close connection



##test using tabulizer 
#locate_areas(file = pdfFile, pages = 2) ##succes!!!


##read and clean data --------------------------------------------------------------------------------------
#Parameters 
setwd("C:\\Users\\andre\\Dropbox\\Andres\\03.Dashboards\\10.Coronavirus\\covid19mx")
#file = file.path("data","10Abril.pdf") 
file_muertos = file.path("data", "Muertos.xlsx")
file_poblacion = file.path("data", "Poblacion.xlsx")


pages = get_n_pages(pdfFile)
penultimaPag = pages-1
namesColumns = c("Caso", "Estado", "Sexo", "Edad", "FechaSintomas", "Status")
centroids = read_rds("app/centroids.rds")


#locate_areas(file)
locate_areas(file = pdfFile, pages = 2)

#Funciton to remove Xs from strings
removeX = function(x){str_remove(x,"X")}

#read data ---------------------------------------------------------------------------------------------------------------
#pagina 1
table1 = extract_tables(pdfFile, 
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
table2= extract_tables(pdfFile, 
                        method = "lattice", 
                        guess = F,
                        output = "data.frame",
                       encoding = "UTF-8",
                        pages = 2:penultimaPag,
                        area =  list(
                          c(0, 38, 953, 584)
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
table3 = extract_tables(pdfFile, 
                        method = "lattice", 
                        guess = F,
                        output = "data.frame",
                        encoding = "UTF-8",
                        pages = pages,
                        area =  list(
                          c(0, 38, 953, 584)
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
Sys.setlocale("LC_TIME", "Spanish")
diasData = covidMex::covidWWSituation() %>%
  filter(geo_id =="MX") %>%
  select(fecha_corte, casos_nuevos, decesos) %>%
  mutate(Fecha = ymd(fecha_corte)-1) %>%
  arrange(Fecha) %>%
  filter(Fecha > "2020-02-28") %>%
  mutate(Casos = cumsum(casos_nuevos),
         Casoslog=log(Casos),
         Dia = row_number()) %>%
  rename(Muertos = decesos,
         Nuevos = casos_nuevos) %>%
  select(-fecha_corte) %>%
  arrange(Dia) %>%
  mutate(fecha= paste(day(Fecha), month(Fecha, label = T))
         )


muertos = diasData %>%
  select(Muertos, Fecha) %>%
  rename(DefuncionesMasQAyer= Muertos) %>%
  mutate(Defunciones = cumsum(DefuncionesMasQAyer))






  
#save data

write_rds(diasData, "app/diasData.rds")
write_rds(muertos, "app/muertos.rds")
#write_rds(poblacion, "app/poblacion.rds")

