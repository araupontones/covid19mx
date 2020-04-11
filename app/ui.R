library(shiny)
library(shinycssloaders)
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
library(leaflet.extras)


#file = "https://www.gob.mx/cms/uploads/attachment/file/542354/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.19.pdf"  
link = "https://www.docdroid.net/YLMXs1N/22marzo.pdf"

Sys.setlocale("LC_TIME", "Spanish")

#file

fluidPage(
  
  tags$head(
    tags$title("Seguimiento COVID19 MX"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Poppins&display=swap", rel='stylesheet'),
    tags$meta(name="viewport",  content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
    ),

  fluidRow(id = "titulo",
    
    h1("Seguimiento de casos confirmados por COVID-19 en México")
  ),
  
  
  
  fluidRow(class="container_intro", 
           
           htmlOutput("intro")
           
  ),
           
  
  
  fluidRow(class = "container_cajas",
  
    
    
    column(4, class = "caja",
           withSpinner(htmlOutput("dia")
                          )),
    
    column(4,class = "caja",
           withSpinner(htmlOutput("casos")),
          
    ),
    
    column(4, class = "caja",
           withSpinner(htmlOutput("fallecidos"))
           )
    
    ),
  
  hr(class = "linea_division"),
  
  #second row (table, map, amd casos)
  fluidRow(class="container_map_table",
           
            column(3, align="center",
                   h4("Casos por entidad federativa"),
                      actionButton(class="boton_tabla boton_casos", "total", "Total de casos"),   
                   actionButton(class="boton_tabla boton_habitantes", "habitantes", "Casos por 100,000 personas"), 
           withSpinner(DT::dataTableOutput("table"))
                         ),
           column(9,
                  leafletOutput("map")
                           )
           ),
  hr(),
  
  fluidRow(column(7, class= "container_botones",
                  
                  actionButton(class= "boton", "linear", "Lineal"),
                  actionButton(class = "boton", "log", "Logarítmico"),
                  actionButton(class= "boton", "nuevos", "Casos nuevos"),
                  actionButton(class= "boton", "muertes", "Muertes"),
                  #titulo de las graficas de tiempo
                  htmlOutput("Titulo_tiempo"),
                  plotOutput("chartTiempo")),
           #casos por grupo de edad
             column(5, align="center",
                    h4("Casos confirmados por grupo de edad"),
                    plotOutput("chartEdad"))
           
  )
                                                
                        
                         
                       
                        
                         )
 
  
  
  #
  
  
  
  
  
  
  
  

