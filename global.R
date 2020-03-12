library(magrittr)
library(plyr)
library(dplyr)

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