rm(list = ls())

library(tidyverse)
library(sf)
library(RCurl)

# 1. Download Shape -------------------------------------------------------


temp_file <- tempfile()

temp_dir <- tempdir()

url_use <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2018/Brasil/BR/br_municipios.zip"

download.file(url_use, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

mun_shape <- sf::read_sf("br_municipios/BRMUE250GC_SIR.shp")

mun_shape <- mun_shape %>% 
  dplyr::rename("NOME_MUNICIPIO" = "NM_MUNICIP",
                "COD_MUN_IBGE" = "CD_GEOCMU")

mun_shape$NOME_MUNICIPIO <- stringr::str_to_title(mun_shape$NOME_MUNICIPIO)

getwd()

# 2. Save -----------------------------------------------------------------

for(i in seq_along(mun_shape$COD_MUN_IBGE)){
  mun_shape_use <- mun_shape[mun_shape$COD_MUN_IBGE == mun_shape$COD_MUN_IBGE[[i]],]
  readr::write_rds(mun_shape_use, paste0("data/output/shape_municipios/", mun_shape$COD_MUN_IBGE[[i]],".rds"))
}

length(list.files("data/output/shape_municipios/"))

# 3. Shape Brasil ---------------------------------------------------------

temp_file <- tempfile()

temp_dir <- tempdir()

url_use = "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/Brasil/BR/br_unidades_da_federacao.zip"

download.file(url_use, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

br_shape <- sf::read_sf(paste0(temp_dir,"/BRUFE250GC_SIR.shp"))

readr::write_rds(br_shape, paste0("data/output/shape_municipios/br.rds"))




