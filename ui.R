library(shinythemes)
library(shinyBS)
#library(mapview)
library(shinyalert)

### Controles do painel

# Municipio

sidebarPanelUi <- function () {
  return(
    tags$div(class="pad-20",
             useShinyalert(),
             h4("Opções:"),
             selectizeInput(
               "cargo",
               label = NULL,
               selected = NULL,
               choices = list("Presidente"=1, "Governador"=3, 
                              "Senador"=5, "Deputado Federal"=6, 
                              "Deptuado Estadual"=7, "Prefeito"=11, 
                              "Vereador"=13),
               options = list(
                 placeholder = "Selecione um Estado"
               )),
             selectizeInput(
               "estado",
               label = NULL,
               selected = NULL,
               choices = c("",
                           "Acre"="AC",
                           "Alagoas"="AL",
                           "Amapá"="AP",
                           "Amazonas"="AM",
                           "Bahia"="BA",
                           "Ceará"="CE",
                           "Distrito Federal"="DF",
                           "Espirito Santo"="ES",
                           "Goiás"="GO",
                           "Maranhão"="MA",
                           "Mato Grosso"="MT",
                           "Mato Grosso do Sul"="MS",
                           "Minas Gerais"="MG",
                           "Pará"="PA",
                           "Paraíba"="PB",
                           "Paraná"="PR",
                           "Pernambuco"="PB",
                           "Piauí"="PI",
                           "Rio de Janeiro"="RJ",
                           "Rio Grande do Norte"="RN",
                           "Rio Grande do Sul"="RS",
                           "Rondônia"="RO",
                           "Roraima"="RR",
                           "Santa Catarina"="SC",
                           "São Paulo"="SP",
                           "Sergipe"="SE",
                           "Tocantins"="TO"
                           ),
               options = list(
                 placeholder = "Selecione um Estado"
               )
             ),
             uiOutput("mun_UI"),
               uiOutput("turno_UI"),
               uiOutput("ano_UI"),
               uiOutput("partido_UI"),
             checkboxInput("eleito", "Somente Candidatos Eleitos?", value = 1),
              uiOutput("candidato_UI"),
             
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
      margin-right: 10px;
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
               width = 8, h4("Sobre CepespMapas"), 
               htmlOutput("Note")
             ))
  )
}

# ROOT UI COMPONENT
spatial2Ui <- function () {
  
 
  return (
    
    tagList(
      
      tags$head(includeCSS("styles.css"),
                
                tags$style(HTML("
                
                .leaflet-top {
                 top: 100px;
                 }
                
                .leaflet-top.leaflet-right .leaflet-control{
                 margin-top: 10px;
                }"
                ))),
        
      tags$div(class = "btn-header", checked = NA,
               tags$a(href = "http://cepespdata.io/", 
                      class="btn btn-primary", 
                      "Ir para CepespData")),
      
      
      navbarPage("Eleições por Local de Votação - VERSÃO BETA", 
                 theme = shinytheme("lumen"), 
                 collapsible = TRUE, 
                 fluid = TRUE,
                 
                 mapTabPanelUi(),
                 aboutTabPanelUi(),
                 
                 add_busy_spinner(spin = "fulfilling-bouncing-circle",
                                  position = "top-right",
                                  margins = c(300, 650)
                                  ),
                 
                 absolutePanel(id = "controls", class = "panel panel-primary", fixed = F,
                               draggable = F, top = 60, left = 10, right = "auto", bottom = "auto",
                               width = 260, height = "auto", sidebarPanelUi())
      ))
    
  )
}