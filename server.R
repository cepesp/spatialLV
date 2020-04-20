library(sf)
library(scales)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

source("global.R")


spatial2Server <- function(input, output, session) {
  
  
  muns_escolhas <- reactive({
    muns_escolhas <- IBGE_Muns %>% 
      filter(UF == input$estado) %>% 
      pull(NOME_MUNICIPIO)
    return(muns_escolhas)
  })
  
  output$mun_UI <- renderUI({
    if(is.null(muns_escolhas())){
      cat("Outputing muns_UI. NULL\n")
      return(NULL)
    }
    selectizeInput(
      inputId = "mun_UI",
      label = NULL,
      selected = NULL,
      choices = c("", muns_escolhas()),
      options = list(
        placeholder = "Selecione um Município"
      )
    )
    })
  
  
  output$turno_UI <- renderUI({
    if(!(input$cargo %in% c(1,3,11))){
      return(1)
    } else {
      selectizeInput(
        inputId = "turno_UI",
        label = NULL,
        selected = 1,
        choices = c(1, 2),
        options = list(
          placeholder = "Selecione um Turno"
        )  
      )
    }
  })
  
  output$ano_UI <- renderUI({
    if (input$cargo %in% c(11, 13)){
      anos <- c(2008, 2012, 2016)
    } else (
      anos <- c(2006, 2010, 2014, 2018)
    )
    sliderInput(
      inputId = "ano_UI",
      label = NULL,
      min=min(anos), 
      max=max(anos),
      value=max(anos),
      step=4,
      sep=""
    )
  })
  
  ### Partido
  
  
  partidos_escolhas <- reactive({
    cat("Parsing partidos_escolhas\n")
    
    if(shiny::req(input$mun_UI) > 0){    
    
    choices <- dados_ano_mun_cargo_turno()%>%
      pull(NUM_VOTAVEL)
    choices <- c("Partido Mais Votado no LV", unique(sort(choices)))
    cat("Parsing partidos_escolhas. CHECK!!!\n")
    return(choices)
    } else if(shiny::req(input$mun_UI) == ""){
      cat("Parsing partidos_escolhas. NULL\n")
      return(NULL)
    }
  }) 
  
  output$partido_UI <- renderUI({
    
    partidos <- partidos_escolhas()
    if(is.null(partidos)){
      cat("Outputing partido_UI. NULL\n")
      return(NULL)
    }
    
    UI <- selectizeInput("partido_UI",
                         label = NULL,
                         choices = partidos,
                         selected = NULL,
                         options  = list(placeholder = 'Escolha um partido:',
                                         allowEmptyOption=TRUE))
    cat("Outputing party_UI. CHECK!!!\n")
    return(UI)
  })
  
  
    mun_code <- reactive({
      if(input$mun_UI > 0){
        mun_code <- IBGE_Muns %>% 
          filter(NOME_MUNICIPIO == input$mun_UI) %>%
          pull(COD_MUN_IBGE) #Need to add state filter as well here
      }
      else if(input$mun_UI == ""){
        mun_code <- NULL
      }
      return(mun_code)
    })

 ### Shapes
  
  mun_shp <- reactive({
    if(is.null(mun_code())){
      municipio_fronteira <- "br"
    } else {
      municipio_fronteira <- mun_code()
      
    }
    mun_shp <- readr::read_rds(paste0("data/output/shape_municipios/", 
                                      municipio_fronteira,".rds"))
    
  })
  
  form_parties_text <- function(df){
    parties <- colnames(df)[-1:-4]
    text <- c()
    for (i in parties){
      text <- paste0(text, i, ": ",round(df[,i],1),"% <br>")
    }
    return(text)
  }
  
  ano_mun_cargo_turno <- reactive({
    ano <- input$ano_UI
    cargo <- input$cargo
    turno <- input$turno_UI
    ano_mun_cargo_turno <- paste(ano, mun_code(), cargo, turno, sep="_")
  })
  
  ### Abrir dados do ano_mun
  
  dados_ano_mun_cargo_turno <- reactive({
    
   
    dados_ano_mun_cargo_turno <- readr::read_rds(paste0("data/output/Prepared_Ano_Mun/", 
                                                        ano_mun_cargo_turno(),".rds"))
    cat("Data opened")
    
    if (input$cargo %in% c(5, 6, 7, 13)){
      dados_ano_mun_cargo_turno <- dados_ano_mun_cargo_turno %>% 
        mutate(NUM_VOTAVEL=str_sub(NUM_VOTAVEL, 1, 2)) %>%
        group_by(NUM_ZONA, NR_LOCVOT, lat, lon, NUM_VOTAVEL) %>%
        summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T),
                  Pct_Votos_LV=sum(Pct_Votos_LV,na.rm=T))
    }
    
   
    dados_ano_mun_cargo_turno_other_parties <- dados_ano_mun_cargo_turno %>% 
      dplyr::select(-QTDE_VOTOS) %>% 
      pivot_wider(names_from="NUM_VOTAVEL", 
                  values_from="Pct_Votos_LV", 
                  values_fill=list(Pct_Votos_LV=0)) %>%
      rowwise() %>%
      do(row = as_data_frame(.)) %>%
      mutate(Other_Parties=form_parties_text(row)) %>%
      unnest() %>%
      dplyr::select(NUM_ZONA, NR_LOCVOT, Other_Parties)
    
    dados_ano_mun_cargo_turno <- dados_ano_mun_cargo_turno %>% 
      left_join(dados_ano_mun_cargo_turno_other_parties, by=c("NUM_ZONA","NR_LOCVOT"))
    return(dados_ano_mun_cargo_turno)
  })
  
  
  dados_ano_mun_cargo_turno_largest_sf <- reactive({
    dados_ano_mun_cargo_turno_largest_sf <- dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NR_LOCVOT) %>%
      mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
      filter(Pct_Votos_LV==max(Pct_Votos_LV,na.rm=T)) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
  })
  
  dados_specific_party <- reactive({
    dados_specific_party <- dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NR_LOCVOT) %>%
      mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
      filter(NUM_VOTAVEL==input$partido_UI) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
  })
  
  dados_to_map <- reactive({
    if (is.null(input$partido_UI)) {
      return(NULL)
    } else if (input$partido_UI=="Partido Mais Votado no LV") {
      dados_to_map <- dados_ano_mun_cargo_turno_largest_sf()
    } else {
      dados_to_map <- dados_specific_party()
    }
    return(dados_to_map)
  })
  
  ### Partidos
  
  parties <- reactive({
    cat(input$partido_UI)
    if (is.null(input$partido_UI)) {
      return(NULL)
    } else if (input$partido_UI=="Partido Mais Votado no LV") {
    parties <- dados_ano_mun_cargo_turno_largest_sf() %>% 
      st_drop_geometry() %>%
      ungroup() %>% 
      distinct(NUM_VOTAVEL) %>%
      filter(!(NUM_VOTAVEL %in% c(95, 96))) %>%
      pull(NUM_VOTAVEL)
    
    cat(parties)
    } else {
      parties <- input$partido_UI
    }
    cat(parties)
    return(parties)
  })    
  
  parties_legend <- reactive({
    
    parties_legend <- party_colours %>% ungroup() %>% 
      filter(NUM_VOTAVEL %in% parties())
    
    return(parties_legend)
  })    
  
  
  ### Mapa
  
  # Base do mapa
  
  
  
  output$map <- renderLeaflet({
    geo <- as.numeric(st_bbox(mun_shp()))
    
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addPolygons(data= mun_shp(), 
                  fillOpacity = 0, 
                  weight=2, 
                  color="black") %>% 
      addProviderTiles(providers$CartoDB.Positron)%>% 
      flyToBounds(geo[3], geo[4], geo[1], geo[2], 
                  options=list(duration=1.5)) 
    
       })
  
  
  # Distribuicao de votos nos locais de votacao
  
  addLegendCustom <- function(map, colors, 
                              labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                             "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                             sizes, "px;margin-top: 4px;line-height: ", sizes, 
                             "px;'>", labels, "</div>")
    
    return(addLegend(map,
                     colors = colorAdditions, 
                     labels = labelAdditions, 
                     opacity = opacity))
  }
  
  
  observeEvent(input$button,{
    
    for(party in parties()){
      leafletProxy("map", data=dados_to_map()) %>%
        clearControls() %>% 
        addCircleMarkers(data = dados_to_map() %>% 
                           dplyr::filter(NUM_VOTAVEL == party), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~log(Tot_Votos_LV/2),
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT,"</h4> Partido ",
                                       NUM_VOTAVEL," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos <br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(party)]](Pct_Votos_LV))} %>%   
      
      addLegend(position = "topright", 
                pal = party_palette_discrete,
                values = ~parties_legend()$NUM_VOTAVEL,
                title = "Partidos",
                opacity = 1) %>%
      addLegend(position = "topright",
                pal = colorNumeric(c(tinter("#525252",direction="tints",steps=10)[3],"#525252"), 
                                   domain=c(0,100)),
                values=~Pct_Votos_LV,
                title="Proporção do Voto",
                opacity=1
      ) %>%
      addLegendCustom(
                      colors = c("grey", "grey", "grey"), 
                      labels = c("100", "1000", "10000"), 
                      sizes = c(log(100/2), log(1000/2), log(10000/2)))
    
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
                   equipe do <a href='http://cepesp.io/'> CEPESPData</a>. Desenvolvido pelos pesquisadores do CEPESP. </font>")
    HTML(note)
  })
  
    }
    

