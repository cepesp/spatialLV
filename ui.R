library(shinythemes)
library(shinyBS)
library(mapview)
library(shinyalert)
library(leaflet)

sidebarPanelUi <- function () {
  return(
    tags$div(class="pad-20",
      useShinyalert(),
      h4("Opções:"),
      selectizeInput(
        "municipio",
        label = NULL,
        selected = NULL,
        choices = c("", "São Paulo"),
        options = list(
          placeholder = "Selecione um município"
        )
      ),
      selectizeInput(
        "cargo",
        label = NULL,
        selected = NULL,
        choices = c("",
          "Prefeito" = 11),
        options = list(
          placeholder = "Selecione um cargo"
        )
      ),
      selectizeInput(
        "ano",
        label = NULL,
        selected = NULL,
        choices = c("", 2004, 2008, 2012, 2016),
        options = list(
          placeholder = "Selecione um ano"
        )
      ),
      uiOutput("turno_UI"),
      actionButton("button", label = strong("Atualizar"), class = "btn-primary btn-block"),
        conditionalPanel(
        condition = 'input.button > 0',
        downloadButton('downloadMap', class="btn btn-success btn-block mt-10", label = "Download")
      )
    )
  )
  
}

mapTabPanelUi <- function () {
  return(tabPanel(
    "Mapa",
    div(
      class = "outer",
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }",
                 "#controlPanel {background-color: rgba(255,255,255,0.8);}",
                 ".leaflet-top.leaflet-right .leaflet-control {
      margin-right: 10px; margin-top: 250px;
    }"),
      leafletOutput("map", width = "100%", height = "100%")
    ),
    absolutePanel(
      draggable = FALSE,
      top = "auto",
      left = "auto",
      right = 20,
      bottom = 20,
      width = "auto",
      height = "auto",
      actionButton("map_zoom_in", "+"),
      actionButton("map_zoom_out", "-")
    )
   
  ))
}

aboutTabPanelUi <- function () {
  return (
    tabPanel("Sobre",
             column(width = 3, ""),
             column(
               width = 8, h4("Sobre CepespMapas"), htmlOutput("Note")
             ))
  )
}

# ROOT UI COMPONENT
spatial2Ui <- function () {
  return (
    
    tagList(
      
      tags$head(includeCSS("styles.css")),
      tags$div(class = "btn-header", checked = NA,
               tags$a(href = "http://cepespdata.io/", class="btn btn-primary", "Ir para CepespData")),
      
      
      navbarPage("Spatial Maps", theme = shinytheme("lumen"), collapsible = TRUE, fluid = TRUE,
                 
                 mapTabPanelUi(),
                 aboutTabPanelUi(),
                 
                 absolutePanel(id = "controls", class = "panel panel-primary", fixed = F,
                               draggable = F, top = 60, left = 10, right = "auto", bottom = "auto",
                               width = 260, height = "auto", sidebarPanelUi())
    ))
    
  )
}