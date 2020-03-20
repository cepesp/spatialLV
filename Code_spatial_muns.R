library(tidyverse)
library(sf)
library(leaflet)
library(geobr)
library(ggnewscale)
library(scales)
library(tinter)

LVs_votes_turno_1_Largest <- readRDS("data/input/LVs_votes_turno_1_Largest.rds")

Ano <- 2016
Municipio <- 3550308

muns <- readRDS("data/input/muns.rds")

party_colours <- tibble(Numero_Partido=c(10, 13, 15, 45),
                        High_Colour=c("#0066CC","#c4122d","#00aa4f","#0080FF")) %>%
  rowwise() %>%
  mutate(Low_Colour=tinter(High_Colour, direction="tints", steps=10)[3],
         palette=list(c(Low_Colour, High_Colour)))

party_palettes <- party_colours %>% 
  dplyr::select(palette) %>%
  pmap(colorNumeric, domain=c(20,80)) %>%
  setNames(party_colours$Numero_Partido)

party_colours_discrete <- party_colours %>% 
  dplyr::mutate(Numero_Partido=factor(Numero_Partido)) %>% 
  dplyr::select(Numero_Partido,High_Colour) %>%
  dplyr::rename("domain"=Numero_Partido,
         "palette"=High_Colour)

party_palette_discrete <- colorFactor(palette=party_colours$High_Colour, 
                                      domain=party_colours$Numero_Partido)

Leaflet_Party <- function(data){
  parties <- unique(data$Largest_Party)
  parties <- parties[!(parties %in% c(95, 96))]
  parties <- parties[!is.na(parties)]
  
  map <- data %>% 
    filter(ANO_ELEICAO==Ano & COD_MUN_IBGE==Municipio) %>% 
    leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=muns, fillOpacity = 0, weight=2, color="black") 
  
  for (party in parties){
    
    map <- addCircleMarkers(map=map, data=data %>% 
                              filter(ANO_ELEICAO==Ano & 
                                       COD_MUN_IBGE==Municipio & 
                                       Largest_Party==party),
                            stroke=F,
                            opacity=0.7,
                            fillOpacity = 0.7,
                            radius=~Tot_Votes/1000,
                            popup=~paste0("<h4> Local de Votação ", NR_LOCVOT,"</h4> Partido ",NUM_VOTAVEL," recebeu ",QTDE_VOTOS," votos, ",round(Pct_Votos,1),"% do total de ",Tot_Votes," votos"),
                            fillColor = ~party_palettes[[as.character(party)]](Pct_Votos) )
  }
  
  map %>%
    addLegend("bottomright", pal = party_palette_discrete,
              values = ~factor(party_colours$Numero_Partido),
              title = "Parties",
              opacity = 1)
}

LVs_votes_turno_1_Largest %>% Leaflet_Party()


