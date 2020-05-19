
rm(list = ls()) # Limpa a área de trabalho do R

# Titulo: Insere as siglas dos partidos nos arquivos rds
# Autor: Rebeca Carvalho
# Data: 19/05/2020

# Pacotes utilizados:

library(readr)
library(tidyverse)

# 1. Data -----------------------------------------------------------------



siglas_partidos <- read_delim("data/input/siglas_partidos.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)



# 2. For loop ------------------------------------------------------------------


files <- list.files(path = "C:/Users/beca_/OneDrive/Documentos/Prepared_Ano_Mun_1") 


data <- list()

for (i in seq_along(files)) {
  cat("lendo", files[i], "\n")
  data <- readr::read_rds(paste0("C:/Users/beca_/OneDrive/Documentos/Prepared_Ano_Mun_1/",files[i]))
  ano <- substr(files[i], start = 1, stop = 4)
  data$NUM_VOTAVEL <- as.numeric(str_sub(data$NUM_VOTAVEL, 1, 2))
  siglas_partidos2 <- siglas_partidos %>% 
    filter(ANO_ELEICAO == ano)
  data <- left_join(data, siglas_partidos2)
  data$NUM_VOTAVEL <-  paste(data$NUM_VOTAVEL,"-",
                                  data$SIGLA_PARTIDO)
  data <- data %>% 
    select(-ANO_ELEICAO,-SIGLA_PARTIDO,-NOME_PARTIDO)
  nome <- files[i]
  saveRDS(data, paste0("data/output/Prepared_Ano_Mun/",nome))
}


