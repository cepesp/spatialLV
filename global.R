library(magrittr)
library(dplyr)
library(tinter)
library(purrr)
library(leaflet)

### Carrega os bancos de dados
IBGE_Muns <- readr::read_rds("data/input/IBGE_muns.rds") #%>%
  #filter(UF=="São Paulo" | COD_MUN_IBGE==3304557)

### Cria as paletas de cores

<<<<<<< HEAD
#Arbitrary colours for PSD, PP, PCB, PTC, Patriota, UP

party_colours <- tibble(NUM_VOTAVEL=c(10, 11, 12, 13, 14, 15,
                                         16, 17, 18, 19,
                                         20, 21, 22, 23, 25,
                                         27, 28, 29, 30, 35,
                                         36, 40,
                                         43, 45, 50, 51,
                                         55, 65, 70, 77,
                                         80, 90),
                        Sigla_Partido=c("REP","PP","PDT","PT","PTB","(P)MDB",
                                        "PSTU","PSL","REDE","PODE",
                                        "PSC","PCB","PL","CDN","DEM",
                                        "DC","PRTB","PCO","NOVO","PMN",
                                        "PTC","PSB",
                                        "PV","PSDB","PSOL","PARTRI",
                                        "PSD","PCdoB","AVANTE","SD",
                                        "UP","PROS"),
                        High_Colour=c("#0066CC","#756bb1","#FE8E6D","#c4122d","#00bfff","#00aa4f",
                                      "#FF0000","#054577","#00C2BB","#31a836",
                                      "#006f41","#8c510a","#FF4500","#ec008c","#8CC63E",
                                      "#0009a8","#2cb53f","#9F030A","#FF4D00","#DD3333",
                                      "#999999","#e5ad02",
                                      "#006600","#0080FF","#fbe106","#c51b7d",
                                      "#feb24c","#DA251C","#d05f3b","#1b1845",
                                      "#bebada","#ec8c34")) %>%
=======
party_colours <- tibble(Numero_Partido=c(10, 13, 15, 20, 43, 45, 50, 77),
                        High_Colour=c("#0066CC","#c4122d","#00aa4f","#006f41","#006600","#0080FF",	"#fbe106","#1b1845")) %>%
>>>>>>> ed2d01d2b5706630f346f897547e6da5f972ad61
  rowwise() %>%
  mutate(Low_Colour=tinter(High_Colour, direction="tints", steps=10)[3],
         palette=list(c(Low_Colour, High_Colour)))

party_palettes <- party_colours %>% 
  dplyr::select(palette) %>%
  pmap(colorNumeric, domain=c(0,100)) %>%
  setNames(party_colours$NUM_VOTAVEL)

party_colours_discrete <- party_colours %>% 
<<<<<<< HEAD
  ungroup() %>% 
  dplyr::mutate(NUM_VOTAVEL=factor(NUM_VOTAVEL)) %>% 
  dplyr::select(NUM_VOTAVEL,High_Colour) %>%
  dplyr::rename("domain"=NUM_VOTAVEL,
                "palette"=High_Colour)

party_palette_discrete <- colorFactor(palette=party_colours$High_Colour, 
                                      domain=party_colours$NUM_VOTAVEL)

=======
  dplyr::mutate(Numero_Partido=factor(Numero_Partido)) %>% 
  dplyr::select(Numero_Partido,High_Colour) %>%
  dplyr::rename("domain"=Numero_Partido,
                "palette"=High_Colour)

party_palette_discrete <- colorFactor(palette=party_colours$High_Colour, 
                                      domain=party_colours$Numero_Partido)
>>>>>>> ed2d01d2b5706630f346f897547e6da5f972ad61
