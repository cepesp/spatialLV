library(magrittr)
library(plyr)
library(dplyr)
library(tinter)
library(radiant)
library(purrr)
library(leaflet)

url <- "http://cepesp.io/api/consulta/tse"

#d_uniq <- suppressWarnings(suppressMessages(readr::read_csv("data/d_uniq_all_new_aug.csv", locale = readr::locale(encoding = "ISO-8859-1"))))
d_stats <- suppressWarnings(suppressMessages(readRDS("data/output/spatial_data.rds"))) 

d_stats <- d_stats %>%
  mutate(Party_vote=ifelse(nchar(NUMERO_CANDIDATO)==2,1,0)) %>%
  filter(Party_vote!=1)

mun <- readRDS("data/output/mun_simple3.rds")


radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}


LVs_votes_turno_1_Largest <- readRDS("data/input/LVs_votes_turno_1_Largest.rds")

Ano <- 2016
Municipio <- 3550308

muns <- readRDS("data/input/muns.rds")

party_colours <- tibble(Numero_Partido=c(10, 13, 15, 45),
                        High_Colour=c("#0066CC","#c4122d","#00aa4f","#0080FF")) %>%
  rowwise() %>%
  mutate(Low_Colour = tinter(High_Colour, direction="tints", steps=10)[3],
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


