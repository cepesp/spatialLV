library(sf)
library(scales)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

source("global.R")

input <- tibble(estado="SP",
                mun_UI="São Paulo",
                cargo=13,
                ano_UI=2016,
                turno_UI=1,
                partido_UI=10,
                candidato_UI="ALBERTO SABINO DE OLIVEIRA")

spatial2Server <- function(input, output, session) {
  
  
  muns_escolhas <- reactive({
    IBGE_Muns %>% 
      filter(SIGLA_UF == input$estado) %>% 
      pull(NOME_MUNICIPIO)
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
        choices = c("1° Turno"=1, "2° Turno"=2),
        options = list(
          placeholder = "Selecione um Turno"
        )  
      )
    }
  })
  
  output$ano_UI <- renderUI({
    if (input$cargo %in% c(11, 13)){
      anos <- c(2008, 2012, 2016)
    } else {
      anos <- c(2006, 2010, 2014, 2018)
    }
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
      
      choices <- dados_ano_mun_cargo_turno() %>%
        pull(NUM_VOTAVEL)
      choices <- tibble(NUM_VOTAVEL=choices %>% str_sub(1, 2)) %>%
        distinct()
      choices <- choices %>% 
        left_join(party_colours %>% select(NUM_VOTAVEL, SIGLA_PARTIDO) %>% distinct(), 
                  by="NUM_VOTAVEL") %>%
        add_row(NUM_VOTAVEL="Partido Mais Votado no LV", 
                SIGLA_PARTIDO="Partido Mais Votado no LV",
                .before=1) %>%
        select(SIGLA_PARTIDO, NUM_VOTAVEL) %>%
        deframe()
      
      cat("Parsing partidos_escolhas. CHECK!!!\n")
      return(choices)
    } else if(shiny::req(input$mun_UI) == ""){
      cat("Parsing partidos_escolhas. NULL\n")
      return(NULL)
    }
  }) 
  
  output$partido_UI <- renderUI({
    partidos <- partidos_escolhas()
    special_1 <- which(partidos %in% c("Partido Mais Votado no LV"))
    special_2 <- which(partidos %in% c(95, 96))
    partidos <- c(partidos[special_1],
                  partidos[-c(special_1, special_2)][sort(names(partidos[-c(special_1, special_2)]))],
                  partidos[special_2])
    
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
  
  ### Candidato
  
  candidatos_escolhas <- reactive({
    cat("Parsing candidatos_escolhas\n")
    
    if(shiny::req(input$partido_UI) != "Partido Mais Votado no LV"){    
      
      #if (input$eleito==1) {
      #dados_cands <- dados_ano_mun_cargo_turno() %>%
      #  filter(eleito==1)
      #} else {
      #  dados_cands <- dados_ano_mun_cargo_turno()
      #}
      
      dados_cands <- dados_ano_mun_cargo_turno()
      
      if (input$cargo %in% c(5, 6, 7, 13)) {
        
        choices <- dados_cands %>%
          filter(str_length(NUM_VOTAVEL)>2) %>%
          mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
          filter(NUM_PARTIDO==input$partido_UI) %>%
          pull(NOME_CANDIDATO)
      } else {
        
        choices <- dados_cands %>%
          mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
          filter(NUM_PARTIDO==input$partido_UI) %>%
          pull(NOME_CANDIDATO) }
      
      choices <- c("Total do Partido", unique(sort(choices)))
      cat("Parsing candidatos_escolhas. CHECK!!!\n")
      return(choices)
    } else if(shiny::req(input$mun_UI) == ""){
      cat("Parsing candidatos_escolhas. NULL\n")
      return(NULL)
    }
  }) 
  
  output$candidato_UI <- renderUI({
    
    candidatos <- candidatos_escolhas()
    if(is.null(candidatos)){
      cat("Outputing candidato_UI. NULL\n")
      return(NULL)
    }
    
    UI <- selectizeInput("candidato_UI",
                         label = NULL,
                         choices = candidatos,
                         selected = NULL,
                         options  = list(placeholder = 'Escolha um candidato:',
                                         allowEmptyOption=TRUE))
    cat("Outputing candidato_UI. CHECK!!!\n")
    return(UI)
  })
  
  
  
  mun_code <- reactive({
    
    if(input$estado > 0 & input$mun_UI == ""){
      code <- input$estado
    } else if(input$mun_UI > 0){
      code <- IBGE_Muns %>% 
        filter(NOME_MUNICIPIO == input$mun_UI & SIGLA_UF==input$estado) %>%
        pull(COD_MUN_IBGE)
    } else if(input$mun_UI == ""){
      code <- NULL
    }
    
    return(code)
  })
  
  ### Shapes
  
  mun_shp <- reactive({
    c <- mun_code()
    
    if(is.null(c)){
      municipio_fronteira <- "br"
    } else {
      municipio_fronteira <- c
    }
    
    readr::read_rds(paste0("data/output/shape_municipios/", 
                           municipio_fronteira,".rds"))
  })
  
  form_parties_text <- function(df){
    parties <- colnames(df)[-1:-5]
    text <- c()
    
    for (i in parties){
      text <- paste0(text, i, ": ",round(df[,i],1),"% <br>")
    }
    
    text
  }
  
  ano_mun_cargo_turno <- reactive({
    ano <- input$ano_UI
    cargo <- input$cargo
    turno <- input$turno_UI
    
    ano_mun_cargo_turno <- paste(ano, mun_code(), cargo, turno, sep="_")
  })
  
  siglas <- reactive({
    party_colours %>% 
      filter(ANO_ELEICAO==input$ano_UI) %>%
      select(NUM_VOTAVEL, SIGLA_PARTIDO)
  })
  
  ### Abrir dados do ano_mun
  
  dados_ano_mun_cargo_turno <- reactive({
    
    #Diagnosing missing parties siglas for specific years
    #dados_ano_mun_cargo_turno %>% 
    #  mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
    #  distinct(NUM_PARTIDO)  %>% slice(16, 18) pull(NUM_PARTIDO) %in% siglas$NUM_VOTAVEL
    
    data <- readr::read_rds(paste0("data/output/Prepared_Ano_Mun/", 
                                   ano_mun_cargo_turno(),".rds"))
    
    cat("Data opened")
    
    data %>% 
      mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>% 
      left_join(siglas(), 
                by=c("NUM_PARTIDO"="NUM_VOTAVEL"))%>%
      mutate(Other_Parties="")
  })
  
  ## Issue of points overlapping where equal number of votes so no largest, ex. LV_NR 1708 in Aracaju for partido 10
  
  dados_ano_mun_cargo_turno_largest_sf <- reactive({
    
    if (input$cargo %in% c(5, 6, 7, 13)) {
      
      data <- dados_ano_mun_cargo_turno() %>%
        mutate(NUM_VOTAVEL=str_sub(NUM_VOTAVEL, 1, 2)) %>%
        group_by(NUM_ZONA, NR_LOCVOT, NUM_VOTAVEL, SIGLA_PARTIDO, lon, lat, Other_Parties) %>%
        summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T)) %>%
        group_by(NUM_ZONA, NR_LOCVOT) %>%
        mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T),
               Pct_Votos_LV=100*(QTDE_VOTOS/sum(QTDE_VOTOS, na.tm=T))) %>%
        filter(QTDE_VOTOS==max(QTDE_VOTOS,na.rm=T)) %>% 
        st_as_sf(coords=c("lon", "lat"), crs=4326)
    } else {
      data <- dados_ano_mun_cargo_turno() %>%
        group_by(NUM_ZONA, NR_LOCVOT) %>%
        mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
        filter(Pct_Votos_LV==max(Pct_Votos_LV,na.rm=T)) %>% 
        st_as_sf(coords=c("lon", "lat"), crs=4326)
    }
    
    return(data)
  })
  
  dados_specific_party <- reactive({
    dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NR_LOCVOT) %>%
      mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
      filter(NUM_VOTAVEL==input$partido_UI) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
  })
  
  dados_specific_candidato <- reactive({
    dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NR_LOCVOT) %>%
      mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
      filter(NOME_CANDIDATO==input$candidato_UI) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
  })
  
  dados_to_map <- reactive({
    if (is.null(input$partido_UI)) {
      return(NULL)
    } else if (input$partido_UI=="Partido Mais Votado no LV") {
      dados_to_map <- dados_ano_mun_cargo_turno_largest_sf()
    } else if (input$candidato_UI=="Total do Partido") {
      dados_to_map <- dados_specific_party()
    } else {
      dados_to_map <- dados_specific_candidato()
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
        distinct(SIGLA_PARTIDO) %>%
        pull(SIGLA_PARTIDO)
      
      cat(parties)
    } else {
      parties <- unique(dados_to_map()$SIGLA_PARTIDO)
    } 
    cat(parties)
    return(parties)
  })    
  
  #parties_legend <- reactive({
  
  #  parties_legend <- siglas %>% ungroup() %>% 
  #    filter(SIGLA_PARTIDO %in% parties())
  
  #  return(parties_legend)
  #})    
  
  
  ### Mapa
  
  # Base do mapa
  
  
  
  output$map <- renderLeaflet({
    
    geo <- as.numeric(st_bbox(mun_shp()))
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data= mun_shp(), 
                  fillOpacity = 0, 
                  weight=2, 
                  color="black") %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  
  # Distribuicao de votos nos locais de votacao
  
  addLegendCustom <- function(map, colors, 
                              labels, sizes, opacity = 0.5, title){
    colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                             "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                             sizes, "px;margin-top: 4px;line-height: ", sizes, 
                             "px;'>", labels, "</div>")
    
    return(addLegend(map,
                     colors = colorAdditions, 
                     labels = labelAdditions, 
                     opacity = opacity,
                     title=title))
  }
  
  observeEvent(input$button,{
    
    
    
    geo <- as.numeric(st_bbox(mun_shp()))
    map_data <- dados_to_map()
    
    if (input$partido_UI=="Partido Mais Votado no LV") {
      
      for(party in parties()){
        leafletProxy("map", data=map_data) %>%
          clearControls() %>% 
          addCircleMarkers(data = map_data %>% 
                             dplyr::filter(SIGLA_PARTIDO == party), 
                           stroke = F,
                           opacity=0.7,
                           fillOpacity = 0.7,
                           radius= ~(QTDE_VOTOS/100),
                           popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                         SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                         round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                         Other_Parties),
                           fillColor = ~party_palettes[[as.character(party)]](Pct_Votos_LV))}  %>% 
        addCircleMarkers(data = map_data, 
                         stroke = T,
                         opacity=0.7,
                         radius= ~(Tot_Votos_LV/100),
                         fillOpacity = 0,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties)) %>%   
        addLegend(position = "topright", 
                  pal = party_palette_discrete,
                  values = ~SIGLA_PARTIDO,
                  title = "Partidos",
                  opacity = 1) %>%
        addLegend(position = "topright",
                  pal = colorNumeric(c(tinter("#525252",direction="tints",steps=10)[3],"#525252"), 
                                     domain=NULL),
                  values=~Pct_Votos_LV,
                  title="<p>Proporção do</p><p>Voto no LV</p>",
                  opacity=1
        ) %>%
        addLegendCustom(
          colors = c("black", "black", "black"), 
          labels = c("100", "1000", "10000"), 
          sizes = c(log(100/2), log(1000/2), log(10000/2)),
          title="Número de Votos") %>% 
        flyToBounds(geo[3], geo[4], geo[1], geo[2],
                    options=list(duration=0.1))
    } else if (input$candidato_UI=="Total do Partido") {
      
      leafletProxy("map", data=map_data) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = map_data, 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = map_data, 
                         stroke = T,
                         opacity=0.7,
                         radius= ~(Tot_Votos_LV/100),
                         fillOpacity = 0,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties)) %>%   
        addLegend(position = "topright", 
                  pal = party_palette_discrete,
                  values = ~SIGLA_PARTIDO,
                  title = "Partidos",
                  opacity = 1) %>%
        addLegend(position = "topright",
                  pal = colorNumeric(c(tinter("#525252",direction="tints",steps=10)[3],"#525252"), 
                                     domain=NULL),
                  values=~Pct_Votos_LV,
                  title="<p>Proporção do</p><p>Voto no LV</p>",
                  opacity=1
        ) %>%
        addLegendCustom(
          colors = c("black", "black", "black"), 
          labels = c("100", "1000", "10000"), 
          sizes = c(log(100/2), log(1000/2), log(10000/2)),
          title="Número de Votos") %>% 
        flyToBounds(geo[3], geo[4], geo[1], geo[2],
                    options=list(duration=0.1))
    } else {
      leafletProxy("map", data=map_data) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = map_data, 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_CANDIDATO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = map_data, 
                         stroke = T,
                         opacity=0.7,
                         radius= ~(Tot_Votos_LV/100),
                         fillOpacity = 0,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ", NR_LOCVOT," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_CANDIDATO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties)) %>%   
        addLegend(position = "topright", 
                  pal = party_palette_discrete,
                  values = ~SIGLA_PARTIDO,
                  title = "Partidos",
                  opacity = 1) %>%
        addLegend(position = "topright",
                  pal = colorNumeric(c(tinter("#525252",direction="tints",steps=10)[3],"#525252"), 
                                     domain=NULL),
                  values=~Pct_Votos_LV,
                  title="<p>Proporção do</p><p>Voto no LV</p>",
                  opacity=1
        ) %>%
        addLegendCustom(
          colors = c("black", "black", "black"), 
          labels = c("100", "1000", "10000"), 
          sizes = c(log(100/2), log(1000/2), log(10000/2)),
          title="Número de Votos") %>% 
        flyToBounds(geo[3], geo[4], geo[1], geo[2],
                    options=list(duration=0.1))
    }
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
