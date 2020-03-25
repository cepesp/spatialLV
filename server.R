library(plyr)
library(data.table)
library(sp)
library(sf)
library(spdep)
library(scales)
library(leaflet)
library(rgeos)
library(raster)
library(maptools)
library(ggplot2)
library(httr)
library(ape)
library(RCurl)
library(digest)
library(dplyr)
library(DT)
library(magrittr)
library(shinyalert)
library(tidyr)
library(shiny)

source("global.R")
#source("database.R")

spatial2Server <- function(input, output, session) {
  
  ### Turno 
  
  turno <- reactive({
    cargo <- as.numeric(input$cargo)
    if(cargo %in% c(1,3)){
      return(input$turno_value)
    } else {
      return(1)
    }
  })
  
  output$turno_UI <- renderUI({
    cargo <- as.numeric(input$cargo)
    if(cargo %in% c(11)){
      selectizeInput("turno_value", 
                     label = NULL,
                     selected = NULL,
                     choices = list("",
                                    "1º Turno" = 1),
                     options = list(
                       placeholder = "Selecione um turno"
                     ))
    }
  })
  
  mun_code <- reactive({
    muns_ref <- tibble(COD_MUN_IBGE=c(3550308,3304557,3513801,3501608,
                                      3530607,3548500,3548807),
           Nome_Municipio=c("São Paulo","Rio De Janeiro","Diadema","Americana",
                            "Mogi Das Cruzes","Santos","São Caetano Do Sul"))
    mun_code <- muns_ref %>% filter(Nome_Municipio==input$municipio) %>%
      pull(COD_MUN_IBGE)
    cat(mun_code)
    return(mun_code)
  })

  
 ### Shapes
  
  state_shp <- reactive({
    municipio <- input$municipio
    if(municipio == "")
      municipio <- "br"
    state_shp <- readr::read_rds(paste0("data/output/shape_municipios/", municipio,".rds"))
    
  })
  
 ### Partidos
  
  parties <- reactive({
    
    parties <- LVs_votes_turno_1_Largest %>% 
      filter(COD_MUN_IBGE==mun_code())  %>% 
      st_drop_geometry() %>%
      distinct(Largest_Party) %>%
      filter(!(Largest_Party %in% c(95, 96))) %>%
      filter(!is.na(Largest_Party)) %>%
      pull(Largest_Party)
    
    cat(parties)
    return(parties)
  })    
 ### Mapa
  
  # Base do mapa
  
    output$map <- renderLeaflet({
      geo <- as.numeric(st_bbox(state_shp()))
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addPolygons(data= state_shp(), 
                    fillOpacity = 0, 
                    weight=2, 
                    color="black") %>% 
        addProviderTiles(providers$CartoDB.Positron)%>% 
        flyToBounds(geo[3], geo[4], geo[1], geo[2])
    })
  

   # Distribuicao de votos nos locais de votacao
    
    
   observeEvent(input$button,{
     
     for(party in parties()){
     leafletProxy("map", data=LVs_votes_turno_1_Largest) %>%
         clearControls() %>% 
      addCircleMarkers(data = LVs_votes_turno_1_Largest %>% 
                       dplyr::filter(COD_MUN_IBGE==mun_code()
                                     & Largest_Party == party), 
                       stroke = F,
                       opacity=0.7,
                       fillOpacity = 0.7,
                       radius= ~Tot_Votes/1000,
                       popup=~paste0("<h4> Local de Votação ", NR_LOCVOT,"</h4> Partido ",
                                     NUM_VOTAVEL," recebeu ",QTDE_VOTOS," votos, ",
                                     round(Pct_Votos,1),"% do total de ",Tot_Votes," votos"),
                       fillColor = ~party_palettes[[as.character(party)]](Pct_Votos))} %>%   
     
       addLegend("topright", 
                pal = party_palette_discrete,
                values = ~factor(party_colours$Numero_Partido),
                title = "Partidos",
                opacity = 1) %>%
       addLegend("bottomleft",
                 pal = colorNumeric(c(tinter("#525252",direction="tints",steps=10)[3],"#525252"), 
                                    domain=c(20,80)),
                 values=seq(20,80,10),
                 title="Proporção do Voto",
                 opacity=1
       )
  })
  
 ### Controles de zoom   
   
   # Zoom out
   
   observeEvent(input$map_zoom_out ,{
     leafletProxy("map") %>% 
       setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
               lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
               zoom = input$map_zoom - 1)
   })
   # Zoom in
   observeEvent(input$map_zoom_in ,{
     leafletProxy("map") %>% 
       setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
               lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
               zoom = input$map_zoom + 1)
   })
 
   
 ### Sobre   
   
  output$Note <- renderUI({
    note <- paste0("<font size='3'> Os mapas eleitorais foram desenvolvidos no âmbito do 
                   projeto temático da FAPESP (processo # 2013/15658-1), sob coordenação do 
                   Prof. George Avelino. Eles utilizam os dados do TSE coletados e limpos pela 
                   equipe do <a href='http://cepesp.io/'> CEPESPData</a>. Desenvolvido por Jonathan Phillips 
                   com apoio dos pesquisadores do CEPESP. </font>")
    HTML(note)
  })
  
  
 
  
  
}