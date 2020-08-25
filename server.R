library(sf)
library(scales)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(mapview)

source("global.R")

input <- tibble(estado="AP",
                mun_UI="Macapá",
                cargo=13,
                ano_UI=2016,
                turno_UI=1,
                partido_UI=15,
                candidato_UI="ALINE ALFAIA")

spatial2Server <- function(input, output, session) {
  
  
  muns_escolhas <- reactive({
    muns_escolhas <- IBGE_Muns %>% 
      filter(SIGLA_UF == input$estado) %>% 
      pull(NOME_MUNICIPIO)
    print(Cstack_info())
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
      print(Cstack_info())
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
      print(Cstack_info())
    } else {
      anos <- c(2006, 2010, 2014, 2018)
      print(Cstack_info())
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
      print(Cstack_info())
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
      
      if (input$eleito==1) {
      dados_cands <- dados_ano_mun_cargo_turno() %>%
        filter(DESC_SIT_TOT_TURNO %in% c("ELEITO","ELEITO POR QP", "ELEITO POR MEDIA"))
      } else {
        dados_cands <- dados_ano_mun_cargo_turno()
      }
      
      
      if (input$cargo %in% c(5, 6, 7, 13)) {
        print(Cstack_info())
        choices <- dados_cands %>%
          filter(str_length(NUM_VOTAVEL)>2) %>%
          mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
          filter(NUM_PARTIDO==input$partido_UI) %>%
          pull(NOME_URNA_CANDIDATO)
      } else {
        print(Cstack_info())
        choices <- dados_cands %>%
          mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
          filter(NUM_PARTIDO==input$partido_UI) %>%
          pull(NOME_URNA_CANDIDATO) }
      
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
    
    if(input$estado > 0 &
       input$mun_UI == ""){
      mun_code <- input$estado
    } else if(input$mun_UI > 0){
      mun_code <- IBGE_Muns %>% 
        filter(NOME_MUNICIPIO == input$mun_UI & SIGLA_UF==input$estado) %>%
        pull(COD_MUN_IBGE)
    } else if(input$mun_UI == ""){
      mun_code <- NULL
    }
    print(Cstack_info())
    return(mun_code)
  })
  
  ### Shapes
  
  mun_shp <- reactive({
    if(is.null(mun_code())){
      municipio_fronteira <- "br"
    } else {
      municipio_fronteira <- mun_code()
      
    }
    print(Cstack_info())
    mun_shp <- readr::read_rds(paste0("data/output/shape_municipios/", 
                                      municipio_fronteira,".rds"))
    
  })
  
  form_parties_text <- function(df){
    parties <- colnames(df)[-1:-5]
    text <- c()
    print(Cstack_info())
    for (i in parties){
      text <- paste0(text, i, ": ",round(df[,i],1),"% <br>")
    }
    return(text)
    
  }
  
  ano_mun_cargo_turno <- reactive({
    ano <- input$ano_UI
    cargo <- input$cargo
    turno <- input$turno_UI
    print(Cstack_info())
    ano_mun_cargo_turno <- paste(ano, mun_code(), cargo, turno, sep="_")
    
  })
  
  siglas <- reactive({
    siglas <- party_colours %>% 
      filter(ANO_ELEICAO==input$ano_UI) %>%
      select(NUM_VOTAVEL, SIGLA_PARTIDO)
  })
  
  ### Abrir dados do ano_mun
  
  
  dados_ano_mun_cargo_turno <- reactive({
    
    #Diagnosing missing parties siglas for specific years
    #dados_ano_mun_cargo_turno %>% 
    #  mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>%
    #  distinct(NUM_PARTIDO)  %>% slice(16, 18) pull(NUM_PARTIDO) %in% siglas$NUM_VOTAVEL
    
    dados_ano_mun_cargo_turno <- readr::read_rds(paste0("data/output/Prepared_Ano_Mun/", 
                                                        ano_mun_cargo_turno(),".rds"))
    print(Cstack_info())
    
    cat("Data opened")
    
    dados_ano_mun_cargo_turno <- dados_ano_mun_cargo_turno %>% 
      mutate(NUM_PARTIDO=str_sub(NUM_VOTAVEL, 1, 2)) %>% 
      left_join(siglas(), 
                by=c("NUM_PARTIDO"="NUM_VOTAVEL"))%>%
      mutate(Other_Parties="")
    
    print(Cstack_info())
      
    return(dados_ano_mun_cargo_turno)
  })
  
  ## Issue of points overlapping where equal number of votes so no largest, ex. LV_NR 1708 in Aracaju for partido 10
  unique_LVs <- reactive({
    unique_LVs <- dados_ano_mun_cargo_turno() %>% 
      group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT, lat, lon) %>%
      summarize(Tot_Votos_LV=sum(QTDE_VOTOS, na.rm=T))
    return(unique_LVs)
  })
  
  dados_ano_mun_cargo_turno_largest_sf <- reactive({
    print(Cstack_info())
    if (input$cargo %in% c(5, 6, 7, 13)) {
    dados_ano_mun_cargo_turno_largest_sf <- dados_ano_mun_cargo_turno() %>%
      mutate(NUM_VOTAVEL=str_sub(NUM_VOTAVEL, 1, 2)) %>%
      group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT, NUM_VOTAVEL, SIGLA_PARTIDO, lon, lat, Other_Parties) %>%
      summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T)) %>%
      group_by(NUM_ZONA, NR_LOCVOT) %>%
      mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T),
             Pct_Votos_LV=100*(QTDE_VOTOS/sum(QTDE_VOTOS, na.rm=T))) %>%
      filter(QTDE_VOTOS==max(QTDE_VOTOS,na.rm=T)) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
    } else {
        dados_ano_mun_cargo_turno_largest_sf <- dados_ano_mun_cargo_turno() %>%
          group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT) %>%
          mutate(Tot_Votos_LV=sum(QTDE_VOTOS,na.rm=T)) %>%
          filter(Pct_Votos_LV==max(Pct_Votos_LV,na.rm=T)) %>% 
          st_as_sf(coords=c("lon", "lat"), crs=4326)
      }
  })
  
  dados_specific_party <- reactive({
    print(Cstack_info())
    dados_specific_party_temp <- dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT) %>%
      filter(NUM_PARTIDO==input$partido_UI) %>% 
      group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT, SIGLA_PARTIDO, NUM_PARTIDO, lon, lat, Other_Parties) %>%
      summarize(QTDE_VOTOS=sum(QTDE_VOTOS, na.rm=T))  %>%
      st_as_sf(coords=c("lon", "lat"), crs=4326)
    
    dados_specific_party <- unique_LVs() %>% 
      left_join(dados_specific_party_temp, by=c("NUM_ZONA","NM_LOCVOT",
                                                    "NR_LOCVOT")) %>%
      ungroup() %>%
      fill(SIGLA_PARTIDO, 
           .direction="downup") %>%
      replace_na(list(QTDE_VOTOS=0, Pct_Votos_LV=0)) %>%
      mutate(Pct_Votos_LV=100*(QTDE_VOTOS/Tot_Votos_LV))
  })
  
  dados_specific_candidato <- reactive({
    print(Cstack_info())
    dados_specific_candidato_temp <- dados_ano_mun_cargo_turno() %>%
      group_by(NUM_ZONA, NM_LOCVOT, NR_LOCVOT) %>%
      group_by(NUM_VOTAVEL) %>%
      mutate(Tot_Votos_Mun=sum(QTDE_VOTOS, na.rm=T),
             Pct_Votos_Mun=100*QTDE_VOTOS/sum(QTDE_VOTOS, na.rm=T)) %>%
      filter(NOME_URNA_CANDIDATO==input$candidato_UI) %>% 
      st_as_sf(coords=c("lon", "lat"), crs=4326)
    
    dados_specific_candidato <- unique_LVs() %>% 
      left_join(dados_specific_candidato_temp, by=c("NUM_ZONA","NM_LOCVOT",
                                               "NR_LOCVOT")) %>%
      ungroup() %>%
      fill(NUM_VOTAVEL, NOME_CANDIDATO, NOME_URNA_CANDIDATO,
           DESC_SIT_TOT_TURNO, SIGLA_PARTIDO, Tot_Votos_Mun, 
           .direction="downup") %>%
      replace_na(list(QTDE_VOTOS=0, Pct_Votos_LV=0, 
                      Pct_Votos_Mun=0))
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
    print(Cstack_info())
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
    
if (input$partido_UI=="Partido Mais Votado no LV") {
  
    for(party in parties()){
      leafletProxy("map", data=dados_to_map()) %>%
        addCircleMarkers(data = dados_to_map() %>% 
                           dplyr::filter(SIGLA_PARTIDO == party), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(party)]](Pct_Votos_LV))}  %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 1,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
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
          title="Número de Votos") 
    } else if (input$candidato_UI=="Total do Partido") {
      
      leafletProxy("map", data=dados_to_map()) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 1,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
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
        title="Número de Votos")
    } else {
      leafletProxy("map", data=dados_to_map()) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_URNA_CANDIDATO," (",DESC_SIT_TOT_TURNO,") recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação e ",
                                       round(Pct_Votos_Mun,1),"% do total do candidato neste município (",Tot_Votos_Mun," votos)."),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 1,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_URNA_CANDIDATO," (",DESC_SIT_TOT_TURNO,") recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação e ",
                                       round(Pct_Votos_Mun,1),"% do total do candidato neste município (",Tot_Votos_Mun," votos).")) %>%   
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
          title="Número de Votos") 
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
  
  map_download <- eventReactive(input$downloadMap, {
    
    if (input$partido_UI=="Partido Mais Votado no LV") {
      
        leaflet(data=dados_to_map(),
                options = leafletOptions(zoomControl = FALSE)) %>%
    clearShapes() %>% 
    clearControls() %>% 
    addPolygons(data= mun_shp(), 
                fillOpacity = 0, 
                weight=2, 
                color="black") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
          clearControls() %>% 
        for(party in parties()){
          addCircleMarkers(data = dados_to_map() %>% 
                             dplyr::filter(SIGLA_PARTIDO == party), 
                           stroke = F,
                           opacity=0.7,
                           fillOpacity = 0.7,
                           radius= ~(QTDE_VOTOS/100),
                           popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                         SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                         round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                         Other_Parties),
                           fillColor = ~party_palettes[[as.character(party)]](Pct_Votos_LV))}  %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 3,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
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
          title="Número de Votos") 
    } else if (input$candidato_UI=="Total do Partido") {
      
      leaflet(data=dados_to_map(),
              options = leafletOptions(zoomControl = FALSE)) %>%
        clearShapes() %>% 
        clearControls() %>% 
        addPolygons(data= mun_shp(), 
                    fillOpacity = 0, 
                    weight=2, 
                    color="black") %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       SIGLA_PARTIDO," recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação<br>",
                                       Other_Parties),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 3,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
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
          title="Número de Votos")
    } else {
      leaflet(data=dados_to_map(),
              options = leafletOptions(zoomControl = FALSE)) %>%
        clearShapes() %>% 
        clearControls() %>% 
        addPolygons(data= mun_shp(), 
                    fillOpacity = 0, 
                    weight=2, 
                    color="black") %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         fillOpacity = 0.7,
                         radius= ~(QTDE_VOTOS/100),
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_URNA_CANDIDATO," (",DESC_SIT_TOT_TURNO,") recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação e ",
                                       round(Pct_Votos_Mun,1),"% do total do candidato neste município (",Tot_Votos_Mun," votos)."),
                         fillColor = ~party_palettes[[as.character(parties())]](Pct_Votos_LV)) %>% 
        addCircleMarkers(data = dados_to_map(), 
                         stroke = F,
                         opacity=0.7,
                         radius= 3,
                         fillOpacity = 0.5,
                         weight=0.5,
                         col = 'black',
                         popup=~paste0("<h4> Local de Votação ",NM_LOCVOT," (", NR_LOCVOT,")"," em Zona ", NUM_ZONA, "</h4>",
                                       NOME_URNA_CANDIDATO," (",DESC_SIT_TOT_TURNO,") recebeu ",QTDE_VOTOS," votos, ",
                                       round(Pct_Votos_LV,1),"% do total de ",Tot_Votos_LV," votos no local de votação e ",
                                       round(Pct_Votos_Mun,1),"% do total do candidato neste município (",Tot_Votos_Mun," votos).")) %>%   
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
          title="Número de Votos") 
    }
  })

  output$downloadMap <- downloadHandler(
    filename = function () {
      return(paste0(paste("CepespData",
                          input$ano_UI, 
                          input$estado, 
                          input$cargo,
                          "Turno", 
                          input$turno_UI,
                          sep="_"),
                    ".png"))
    },
    content = function(file) {
      mapshot( x = map_download()
               , file = file
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    }
  )  
}

