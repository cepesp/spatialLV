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
source("database.R")

spatial2Server <- function(input, output, session) {
  
  ### Turno ###
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
                                    "1º Turno" = 1,
                                    "2º Turno" = 2),
                     options = list(
                       placeholder = "Selecione um turno"
                     ))
    }
  })
  
  reactive({
  cargo <- as.numeric(input$cargo)
  ano <- as.numeric(input$ano)
  turno_use <- turno()
  municipio <- input$municipio
  })
  

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  state_shp <- reactive({
    municipio <- input$municipio
    if(municipio == "")
      municipio <- "br"
    state_shp <- readr::read_rds(paste0("data/output/shape_municipios/", municipio,".rds"))
  })

  observe({
    municipio <- input$municipio
    
    geo <- as.numeric(st_bbox(state_shp()))
    
    ### Base Map ###
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(data = state_shp(),
                  fillOpacity  = 0,
                  weight       = 3,
                  color        = "black",
                  fillColor    = NULL) %>% 
      flyToBounds(geo[3], geo[4], geo[1], geo[2])
  })
  
  
  output$Note <- renderUI({
    note <- paste0("<font size='3'> Os mapas eleitorais foram desenvolvidos no âmbito do 
                   projeto temático da FAPESP (processo # 2013/15658-1), sob coordenação do 
                   Prof. George Avelino. Eles utilizam os dados do TSE coletados e limpos pela 
                   equipe do <a href='http://cepesp.io/'> CEPESPData</a>. Desenvolvido por Jonathan Phillips 
                   e Rafael de Castro Coelho Silva com apoio dos pesquisadores do CEPESP. </font>")
    HTML(note)
  })
  
  
 
    
    # actual tooltip created as wellPanel
  
  
}