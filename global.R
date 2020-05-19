library(magrittr)
library(dplyr)
library(tinter)
library(purrr)
library(leaflet)
library(shinybusy)
library(readr)


### Carrega os bancos de dados
IBGE_Muns <- readr::read_rds("data/input/IBGE_Muns.rds") #%>%
  #filter(UF=="São Paulo" | COD_MUN_IBGE==3304557)

siglas_partidos <- read_delim("data/input/siglas_partidos.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)


### Cria as paletas de cores
#https://pt.wikipedia.org/wiki/Predefini%C3%A7%C3%A3o:Cor_de_partido_pol%C3%ADtico/BRA

#Arbitrary colours for PSD, PP, PCB, PTC, Patriota, UP, PRP
#54

party_colours <- tibble(NUM_VOTAVEL=factor(c(10, 11, 12, 13, 14, 15,
                                         16, 17, 18, 19,
                                         20, 21, 22, 23, 25,
                                         27, 28, 29, 30, 35,
                                         36, 40,
                                         43, 45, 50, 51,
                                         55, 65, 70, 77,
                                         80, 90,
                                         31, 33, 44, 54)),
                        High_Colour=c("#0066CC","#756bb1",
                                      "#FE8E6D","#c4122d",
                                      "#00bfff","#00aa4f",
                                      "#FF0000","#054577",
                                      "#00C2BB","#31a836",
                                      "#006f41","#8c510a",
                                      "#FF4500","#ec008c",
                                      "#8CC63E","#0009a8",
                                      "#2cb53f","#9F030A",
                                      "#FF4D00","#DD3333",
                                      "#999999","#e5ad02",
                                      "#006600","#0080FF",
                                      "#fbe106","#c51b7d",
                                      "#feb24c","#DA251C",
                                      "#d05f3b","#1b1845",
                                      "#bebada","#ec8c34",
                                      "#831D1C","#DD3333",
                                      "#FFFF00", "#9ACD32")) %>%
  rowwise() %>%
  mutate(Low_Colour=tinter(High_Colour, direction="tints", steps=10)[3],
         palette=list(c(Low_Colour, High_Colour)))

## Acrescenta as siglas dos partidos as paletas

siglas_partidos$NUM_VOTAVEL <- as.factor(siglas_partidos$NUM_VOTAVEL)

party_colours <- left_join(party_colours, siglas_partidos)

party_colours$NUM_VOTAVEL <- paste(party_colours$NUM_VOTAVEL,"-",
                                   party_colours$SIGLA_PARTIDO) 
party_colours <- party_colours %>% 
  dplyr::select(NUM_VOTAVEL, High_Colour, Low_Colour, palette)

party_colours <- unique(party_colours)

#party_palettes <- party_colours %>% 
#  dplyr::select(palette) %>%
#  pmap(colorNumeric, domain=c(0,100)) %>%
#  setNames(party_colours$NUM_VOTAVEL)

#With null domain for flexibility to values
party_palettes <- party_colours %>% 
  dplyr::select(palette) %>%
  pmap(colorNumeric, domain=NULL) %>%
  setNames(party_colours$NUM_VOTAVEL)


party_colours_discrete <- party_colours %>% 
  ungroup() %>% 
  dplyr::mutate(NUM_VOTAVEL=factor(NUM_VOTAVEL)) %>% 
  dplyr::select(NUM_VOTAVEL,High_Colour) %>%
  dplyr::rename("domain"=NUM_VOTAVEL,
                "palette"=High_Colour)

party_palette_discrete <- colorFactor(palette=party_colours$High_Colour, 
                                      levels=party_colours$NUM_VOTAVEL)


