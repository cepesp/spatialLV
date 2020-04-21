rm(list = ls())

library(tidyverse)
library(geobr)


# 1. Download Shape -------------------------------------------------------

estados <- c("AC", "AL", "AM", 
             "AP", "BA", "CE", "DF", "ES","GO",
             "MA", "MG","MS", "MT", "PA", "PB", 
             "PE", "PI","PR", "RJ", "RN", "RO", 
             "RR","RS", "SC", "SE", "SP", "TO")


for(i in estados){
  
  cat("Lendo", i, "\n")

  mun <- read_municipality(code_muni= i, 
                         year=2018,
                         simplified = TRUE)

  mun <- mun %>% 
    dplyr::select(code_muni,
                name_muni,
                abbrev_state,
                geom) %>% 
    dplyr::rename("COD_MUN_IBGE" = "code_muni",
                "NOME_MUNICIPIO" = "name_muni",
                "UF" = "abbrev_state",
                "geometry" = "geom")

  for(i in seq_along(mun$COD_MUN_IBGE)){
    mun_shape_use <- mun[mun$COD_MUN_IBGE == mun$COD_MUN_IBGE[[i]],]
    readr::write_rds(mun_shape_use, paste0("data/output/shape_municipios/", 
                                         mun$COD_MUN_IBGE[[i]],".rds"))
    }

  }



length(list.files("data/output/shape_municipios/"))

# 2. Shape Brasil ---------------------------------------------------------


br <- read_state(code_state = "all",
                 year = 2018)

br <- br %>% 
  dplyr::select(abbrev_state,
                geom) %>% 
  dplyr::rename("UF" = "abbrev_state",
                "geometry" = "geom")

readr::write_rds(br, paste0("data/output/shape_municipios/br.rds"))


length(list.files("data/output/shape_municipios/"))

## Arruma o shape do Espirito Santo e de Vitoria

ES <- readr::read_rds("data/output/shape_municipios/ES.rds")

ES <- ms_filter_islands(ES, min_area = 12391399903)

vitoria <- readr::read_rds("data/output/shape_municipios/3205309.rds")

vitoria <- ms_filter_islands(vitoria, min_area = 12391399)

## Salva o novo shape

readr::write_rds(ES, paste0("data/output/shape_municipios/ES.rds"))

readr::write_rds(vitoria, paste0("data/output/shape_municipios/3205309.rds"))
