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
library(rgdal)



server <- function(input, output, session) {
  

  
  #file = "https://www.gob.mx/cms/uploads/attachment/file/542354/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.19.pdf"  
  
  
  #extract table from pdf

  
 source("global.R")
  
  Sys.setlocale("LC_TIME", "Spanish")
  
  react = reactiveValues(
    
    #table with casos totales y nuevos casos por dia
    diasData = diasData,
    timeAxis = "yes",
    chart = "acumulados",
    titulo_tiempo="Evolución cumulativa de casos confirmados (escala lineal)",
    tabla="totales"
    
  )
 
#eventos de botones de tabla de casos por entidad federativa -----------------------------------------------------
  
  observeEvent(input$total,{
    
    react$tabla = "totales"
    
  })
  
  
  observeEvent(input$habitantes,{
    
    react$tabla = "habitantes"
    
  })
  
  
  
#eventos de botones de casos sobre tiempo--------------------------------------------------------------------------
  observeEvent(input$nuevos, {

    
    react$chart = "nuevos"
    react$titulo_tiempo= "Número de casos nuevos por día"
    
    
  })
  
  
  observeEvent(input$linear, {
    
    react$diasData =  diasData
    
    react$timeAxis = "yes"
    
    react$chart = "acumulados"
    
    react$titulo_tiempo="Evolución cumulativa de casos confirmados (escala lineal)"
   
    
  })
  
  
  observeEvent(input$log, {
    
    react$diasData = diasData %>%
      mutate(Casos= Casoslog)
    
    react$timeAxis = "no"
    react$chart = "acumulados"
    react$titulo_tiempo="Evolución cumulativa de casos confirmados (escala logarítmica)"
    
    
  })
  
 observeEvent(input$muertes,{
   
   react$chart = "muertes"
   react$titulo_tiempo="Evolución cumulativa de muertes (escala lineal)"
   
 })
  
  
### Outputs ----------------------------------------------------------------------------------------------------------

  output$intro <- renderUI({
    
    HTML(paste('<p class="introtext"> <span class= intro_title> Fuente: </span> datos @ssalud', fuente, '| Los datos de muertes son capturados de los reportes técnicos de la Secretería del', 
               fecha , INEGI,' </p>',
               '<p class = introtext> <span class= intro_title> Creador:</span> <a href ="https://www.linkedin.com/in/andr%C3%A9s-arau-017b1127/" target ="_blank">Andrés Arau Pontones </a> (andres.arau@outlook.com)</p>',
               '<p class = "introtext introtextsub"> <span class= intro_title> Colaboradores:</span> Rodrigo Sirvent</p>'))
    
    # 
    # htmlOutput(class="text6","actualizado"),
    #  h6("Datos: Reporte oficial de la Secretaría de Salud"),
    #  h6("Creado por:",tags$a(href="https://www.linkedin.com/in/andr%C3%A9s-arau-017b1127/",
    #                          "Andrés Arau Pontones")),
    #  h6(id = "colaboradres", "Colaboradores: Alejando Morales, Rodrigo Sirvent")),
    
    
  })
  
  
  output$dia <- renderUI({
    
    HTML(paste("<p class = caja_titulo>Día</p>  <hr class= caja_linea> <p class= caja_info>", fecha, "</p>"))
    
    
  })
  
  output$casos <- renderUI({
    
    
    HTML(paste('<p class = "caja_titulo"> Casos</p> <hr class= caja_linea>',
               '<p class = "caja_info casos_info"> ',totalCases,'</p>',
               '<p class = caja_nota>*', casosNuevos,'más que el', ayer, '</p>' )
         
    )
    
  })
  
  
  output$fallecidos <- renderUI({
    
    HTML(paste('<p class = "caja_titulo"> Muertes </p> <hr class = caja_linea>', 
    '<p class = caja_info>',totalMuertes, '</p>',
    '<p class = caja_nota> *', muertosMasqAyer, 'más que el', ayer, '</p>'))
      
      
    
  })
  


  output$table <- DT::renderDT({
    
#if covidMX doesnt work   
    
    tableEstados = dfTables %>%
      group_by(Estado, centroid.lon, centroid.lat) %>%
      summarise(Casos = n()) %>%
      ungroup()%>%
      arrange(desc(Casos)) %>%
      select(Estado, Casos) %>%
      mutate(Casoslab = paste(Estado,'<b><font color="#fb4444">', Casos, "</font></b>"))
    
    #join with population data and estimate per 100,00
    tableEstados = plyr::join(tableEstados, poblacion, by="Estado", type="left") %>%
      mutate(Casospop = round((Casos*100000)/Poblacion, digits = 1),
             CasospopLab = paste(Estado,'<b><font color="#fb4444">', Casospop, "</font></b>")
             ) 
    
    
    
      
#if covidMX works
    
    # tableEstados <- dfTables %>%
    #   group_by(Estado, Fecha) %>%
    #   arrange(Estado, Fecha) %>%
    #   summarise(Nuevos = n()) %>%
    #   mutate(Casos = cumsum(Nuevos),
    #          Fechalabel = paste(day(lag(Fecha)), "de", month(lag(Fecha), label = T, abbr = F)),
    #          Casos2 = paste(Estado,'<b><font color="#fb4444">', Casos, '</font></b> <p><span class ="table_note">*'
    #                         , Nuevos, 'más que el', Fechalabel, '</span></p>'),
    #          Casos2 = translate_date(Casos2)) %>%
    #   filter(row_number()==n()) %>%
    #   arrange(desc(Casos)) %>%
    #   ungroup() %>%
    #   select(Casos2)
     
             
    if(react$tabla == "totales"){
      
      tableEstados <- tableEstados %>%
        arrange(desc(Casos)) %>%
        select(Casoslab) %>%
        rename(Casos = Casoslab)
      
      
    } else if(react$tabla == "habitantes"){
      
      tableEstados <- tableEstados %>%
        arrange(desc(Casospop)) %>%
        select(CasospopLab) %>%
        rename(Casps = CasospopLab)
      
      
    }
             
  
    
    
    
    DT::datatable(tableEstados,
    options = list(pageLength = 32,
                   LengthChange = F,
                   columnDefs = list(list(orderable=F, targets=c(0))),
                   dom = 't',
                   scrollY = '600px'
                   
                   ),
      selection = 'none',
      rownames = FALSE,
    escape = F,
    colnames = c("")
  
    )



  })

  output$map <- renderLeaflet({


    labs <- lapply(seq(nrow(casosEstados)), function(i) {
      paste0( '<p> <b>', casosEstados[i, "Estado"], '</b><p></p>', 
              'Casos confirmados : <b><font color="#fb4444">', casosEstados[i, "Casos"], '</b></font>')
      
    })
    
    
    map = leaflet(shapefile,
                  options = leafletOptions(maxZoom = 12,
                                           minZoom = 4
                  )) %>%
      addProviderTiles("CartoDB.Positron", group = "Basemap") %>%
      addPolygons(
        color = "#4d4d4d",
        weight = .5,
        opacity = 1,
        fillColor = "pink"
      ) %>%
      
      addCircleMarkers(data = casosEstados,
                       lng = ~centroid.lon,
                       lat = ~centroid.lat,
                       color = "#CE0A0A",
                       weight = 1.5,
                       stroke = T,
                       fillOpacity = .3,
                       radius =  ~1 *Casos^0.5,
                       label = lapply(labs,HTML),
                       group = "Casos totales"
                       
                       
      ) %>%
      addCircleMarkers( data = casosEstados,
                        lng = ~centroid.lon,
                        lat = ~centroid.lat,
                        label = ~Casos,
                        weight = 0,
                        stroke = F,
                        radius = 0,
                        group = "Número de casos totales",
                        labelOptions = labelOptions(noHide = TRUE,  direction = "top",
                                                    offset=c(0,10),textOnly = TRUE,
                                                    textsize = "11px", textcolor="red",
                                                    style = list("color"="#fb4444",
                                                                 "font-family"="Poppins")))  %>%
      
      #add circle mujeres
      addCircleMarkers( data = dfmujeres,
                        lng = ~centroid.lon,
                        lat = ~centroid.lat,
                        weight = 1.5,
                        stroke = T,
                        fillOpacity = .3,
                        radius =  ~1 *Casos^0.5,
                        color = colorM,
                        group = "Casos de mujeres"
                       ) %>%

                       
      
      #max bouds
      setMaxBounds(lng1= -116,lat1=33,
                    lng2= -85,lat2 =  13
                             ) %>%
      setView(zoom = 5,
              lat = 20,
              lng = -98) %>%
      
      #layers control
      addLayersControl(
        overlayGroups = c("Casos totales","Casos de mujeres","Número de casos totales","Basemap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      #hideGroup("Número de casos totales") %>%
      hideGroup("Basemap") %>%
      hideGroup("Casos de mujeres")
     
    
    


    map

  })

  #titulo de la grafica de tiempo
  output$Titulo_tiempo <-renderUI({
    
    h4(react$titulo_tiempo)
    
  })
  
  #grafica de tiempo
  output$chartTiempo <- renderPlot({

      #if boton is acumulados
      if(react$chart == "acumulados"){
      
      chart_Tiempo = ggplot( data =  react$diasData,
              aes(x=Fecha,
                  y = Casos)
              ) +
    geom_point(size = size_point,
               color = color_chart) +
        geom_line(size= size_line,
                  color = color_chart) +
      tema_linea +
        xlab("") +
        ylab("") +
        scale_x_date(date_breaks = "4 days",
                     date_labels =  " %d %b") 
      
      if(react$timeAxis == "yes"){
        
        test = react$diasData[seq(nrow(react$diasData),1,-3),]
        
        chart_Tiempo  +
          geom_label(data = test, 
                     aes(label = Casos))
        
      } else {
        
        chart_Tiempo  
        
        
      }
      
      }
      
      #chart casos nuevos
      else if(react$chart=="nuevos"){
        
        
        ggplot(data = diasData,
               aes(x=Fecha,
                   y = Nuevos)) +
          geom_col(color = "white",
                   fill = "steelblue") +
          geom_text(aes(Fecha, Nuevos+2.5, label = Nuevos),
                    check_overlap = TRUE,
                    size = 3,
                    color = "black") +
          tema_linea +
          xlab("") +
          ylab("") +
          scale_x_date(date_breaks = "1 week",
                       date_labels =  " %d %b"
          ) 
        
      } 
      
      #muertes
      else if(react$chart=="muertes"){
        
        ggplot(data = muertos,
               aes(x=Fecha,
                   y=Defunciones)) +
          
          geom_point(size = size_point,
                     colour = "black") +
          geom_line(size  = size_line,
                    colour = "black")+
          geom_label(aes(label = Defunciones))+
          tema_linea +
          xlab("") +
          ylab("") +
          scale_x_date(date_breaks = "3 days",
                       date_labels =  " %d %b")
        
        
      }
      
      
    
                
                
              
    })
  
  #grafica por grupo de edad
  output$chartEdad<-renderPlot({
    
    dfTables$Grupo <- factor(dfTables$Grupo, levels = sort(unique(dfTables$Grupo)))
    
    tableGrupo = dfTables %>%
      group_by(Grupo, Sexo) %>%
      summarise(Casos = n()) %>%
      ungroup() %>%
      group_by(Sexo) %>%
      mutate(Casos_perc= round(Casos*100/sum(Casos), digits = 0),
             Casos_label = if_else(Casos_perc>5,paste0(Casos_perc,"%"),""),
             Casos_perc = if_else(Sexo=="M", Casos_perc*-1, Casos_perc)) %>%
      ungroup() %>%
      mutate(Sexo = if_else(Sexo=="M", "Hombres", "Mujeres"))
    
    
    dataM <- tableGrupo %>%
      filter(Sexo=="Hombres")
    
    dataF <- tableGrupo %>%
      filter(Sexo=="Mujeres")
    
   
    #graph
    chartGrupo = ggplot(data =tableGrupo,
           aes(x=Grupo,
               y=Casos_perc,
               fill=Sexo),
           xlab="Grupo de Edad",
           lineend="round") +
      
      #barra de hombres
      geom_bar(data= dataM,
               stat="identity",
               size=1,
               alpha=.7
               
      )+
      
      geom_text(data= dataM,
                aes(label= Casos_label),
                nudge_y =2,
                size=3.5,
                color="white",
                show.legend = F) +
      
      #barra de mujeres
      geom_bar(data= dataF,
               stat = "identity",
               alpha=.7,
               show.legend = F) +
      
      geom_text(data= dataF,
                aes(label= Casos_label),
                nudge_y =-2,
                size=3.5,
                color="white",
                show.legend = F) +
      
      #breaks y lables de eje
      scale_y_continuous(breaks = seq(-100, 100, 10),
                         labels = paste0(as.character(c(seq(100, 0, -10), seq(10, 100, 10))), "%")) +
      #tema
      coord_flip() +
      theme_bw() +
      scale_fill_manual(values = adjustcolor(c(colorH, colorM), alpha.f =.6)) +
      xlab("Grupo de edad") +
      ylab("% de casos confirmados") +
      
      theme(legend.position=c(.55,.95),
            legend.direction = "horizontal",
            legend.title = element_blank()
            )
    
    chartGrupo
    
    
  })
  
  
#end server    
}
    
    
    
  
  
  
  