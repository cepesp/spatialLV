library(magrittr)
library(plyr)
library(dplyr)
library(tinter)
library(purrr)
library(leaflet)

### Carrega os bancos de dados

LVs_votes_turno_1_Largest <- readRDS("data/input/LVs_votes_turno_1_Largest.rds")

muns <- readRDS("data/input/muns.rds")


### Cria as paletas de cores

party_colours <- tibble(Numero_Partido=c(10, 13, 15, 20, 43, 45, 50, 77),
                        High_Colour=c("#0066CC","#c4122d","#00aa4f","#006f41","#006600","#0080FF",	"#fbe106","#1b1845")) %>%
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
