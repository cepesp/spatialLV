library(shinythemes)
library(shinyBS)
library(mapview)
library(shinyalert)

### Controles do painel

# Municipio

sidebarPanelUi <- function () {
  return(
    tags$div(class="pad-20",
      useShinyalert(),
      h4("Opções:"),
      selectizeInput(
        "estado",
        label = NULL,
        selected = NULL,
<<<<<<< HEAD
        choices = c("", "São Paulo","Rio de Janeiro"),
=======
        choices = c("", "São Paulo","Rio De Janeiro","Diadema","Americana",
                    "Mogi Das Cruzes","Santos","São Caetano Do Sul"),
>>>>>>> ed2d01d2b5706630f346f897547e6da5f972ad61
        options = list(
          placeholder = "Selecione um Estado"
        )
      ),
      uiOutput("mun_UI"),
      selectizeInput(
        "ano",
        label = NULL,
<<<<<<< HEAD
        selected = 2016,
        choices = c("", seq(2006, 2016, 2)),
=======
        selected = NULL,
        choices = c("", 2016),
>>>>>>> ed2d01d2b5706630f346f897547e6da5f972ad61
        options = list(
          placeholder = "Selecione um ano"
        )
      ),
      uiOutput("cargo_UI"),
  # Turno
#      uiOutput("turno_UI"),
  
  # Botao 'atualizar'
  
      actionButton("button", 
                   label = strong("Atualizar"), 
                   class = "btn-primary btn-block"),
  
  # Botao 'download'
  
        conditionalPanel(
        condition = 'input.button > 0',
        downloadButton('downloadMap', 
                       class="btn btn-success btn-block mt-10", 
                       label = "Download")
      )
    )
  )
  
}

  # Mapa

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
      leafletOutput("map", 
                    width = "100%", 
                    height = "100%")
    ),
    
  # Controles do mapa
    
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

### Sobre

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