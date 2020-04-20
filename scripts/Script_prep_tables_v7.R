library(tidyverse)
library(sf)
library(geobr)
library(cepespR)
select <- dplyr::select
rename <- dplyr::rename

#State of SP and RJ city
#THIS FILE NOT ON GITHUB
painel_lvs_secao <- read_delim("../LV_dados/final_painel_BR_secao.csv",
                             delim=";",
                             col_types=cols(NR_LOCVOT="c")) #671,767, one per seção eleitoral em cada ano, with LV and coords

ibge_key <- read_csv("data/input/aux_mun_code.csv") %>%
  rename("SIGLA_UE"="COD_MUN_TSE") %>%
  select(SIGLA_UE, COD_MUN_IBGE) %>%
  mutate(SIGLA_UE=as.numeric(SIGLA_UE))

read_csv("data/input/aux_mun_code.csv") %>%
  select(SIGLA_UF, COD_MUN_IBGE, NOME_MUNICIPIO) %>%
  filter(SIGLA_UF %in% c("SP","RJ")) %>%
  mutate(UF=case_when(SIGLA_UF=="SP"~"São Paulo",
                      SIGLA_UF=="RJ"~"Rio de Janeiro")) %>%
  select(-SIGLA_UF) %>%
  saveRDS("IBGE_Muns.rds")

painel_lvs_secao_all <- painel_lvs_secao %>% 
  select(ANO, CD_LOCALIDADE_TSE, NR_ZONA, NR_LOCVOT, NR_SECAO, lat, lon) %>%
  left_join(ibge_key, by=c("CD_LOCALIDADE_TSE"="SIGLA_UE")) %>%
  rename("NUM_ZONA"="NR_ZONA",
         "NUM_SECAO"="NR_SECAO",
         "CODIGO_MUNICIPIO"="CD_LOCALIDADE_TSE",
         "ANO_ELEICAO"="ANO")

### Dados eleitorais
#NOT ON GITHUB
files <- list.files("C:/Users/jonny/Google Drive/Academic/USP/Data/Electoral/Seções Eleitorais",
                    pattern="*.txt", full.names=T, recursive=T)

#files <- files[files %>% str_detect("SP.txt|RJ.txt")]

#2006 - 2016 same format; 2018 different
votes_secao <- files %>% map(read_delim, delim=";", 
                             col_names=c("DATA_GERACAO","HORA_GERACAO","ANO_ELEICAO",
                                         "NUM_TURNO","DESCRICAO_ELEICAO","SIGLA_UF",
                                         "SIGLA_UE","CODIGO_MUNICIPIO","NOME_MUNICIPIO",
                                         "NUM_ZONA","NUM_SECAO","CODIGO_CARGO",
                                         "DESCRICAO_CARGO","NUM_VOTAVEL","QTDE_VOTOS"),
                             col_types=c("cciiccciciiicii"),
                             locale=locale(encoding="latin1"),
                             na=c("#NULO","#NE",-1,-3)) %>% 
  bind_rows() %>%
  select(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, NUM_ZONA, NUM_SECAO, CODIGO_CARGO, NUM_VOTAVEL, QTDE_VOTOS) 

#2018
#NOT ON GITHUB
files_2018 <- list.files("C:/Users/jonny/Google Drive/Academic/USP/Data/Electoral/Seções Eleitorais",
                    pattern="*.csv", full.names=T, recursive=T)
#files_2018 <- files_2018[files_2018 %>% str_detect("SP.csv|RJ.csv")]

votes_secao_2018 <- files_2018 %>% map(read_delim, delim=";", 
                             col_names=c("DATA_GERACAO","HORA_GERACAO","ANO_ELEICAO",
                                         "CD_TIPO_ELEICAO","NM_TIPO_ELEICAO",
                                         "NUM_TURNO","CD_ELEICAO","DESCRICAO_ELEICAO","DATA_ELEICAO",
                                         "TP_ABRANGENCIA","SIGLA_UF",
                                         "SIGLA_UE","NOME_UE","CODIGO_MUNICIPIO","NOME_MUNICIPIO",
                                         "NUM_ZONA","NUM_SECAO","CODIGO_CARGO",
                                         "DESCRICAO_CARGO","NUM_VOTAVEL","NM_VOTAVEL","QTDE_VOTOS"),
                             col_types=c("cciiciicccccciciiicici"),
                             locale=locale(encoding="latin1"),
                             na=c("#NULO","#NE",-1,-3)) %>% 
  bind_rows() %>%
  select(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, NUM_ZONA, NUM_SECAO, CODIGO_CARGO, NUM_VOTAVEL, QTDE_VOTOS) %>%
  filter(!is.na(ANO_ELEICAO)) 

votes_secao <- votes_secao %>% bind_rows(votes_secao_2018)

painel_lvs_secao_votos_inner <- painel_lvs_secao_all  %>% 
  inner_join(votes_secao, by=c("ANO_ELEICAO","CODIGO_MUNICIPIO","NUM_ZONA","NUM_SECAO"))

#painel_lvs_secao_votos_inner %>% saveRDS("painel_lvs_secao_votos_inner.rds")
rm(votes_secao, votes_secao_2018, painel_lvs_secao, painel_lvs_secao_all)
#painel_lvs_secao_votos_inner <- readRDS("painel_lvs_secao_votos_inner.rds")

painel_lvs_secao_votos_inner_1 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2006, 2008))
painel_lvs_secao_votos_inner_2 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2010, 2012))
painel_lvs_secao_votos_inner_3 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2014, 2016))
painel_lvs_secao_votos_inner_4 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2018))

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner %>% 
  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner_lv %>%
  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO) %>%
  mutate(Largest_Party_LV=case_when(QTDE_VOTOS==max(QTDE_VOTOS,na.rm=T)~1,
                                    TRUE~0))

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner_lv %>%
  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO) %>%
  mutate(Pct_Votos_LV=100*(QTDE_VOTOS/sum(QTDE_VOTOS,na.rm=T)))
  
#painel_lvs_secao_votos_inner_lv %>% ungroup() %>% 
#  slice(1:100) %>% 
#  select(NR_LOCVOT,CODIGO_CARGO, NUM_TURNO, QTDE_VOTOS,Largest_Party_LV, Pct_Votos_LV)

painel_lvs_secao_votos_inner_mun_ano <- painel_lvs_secao_votos_inner_lv %>%
  nest(Zonas=c(NUM_ZONA, NR_LOCVOT,lat,lon,CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, QTDE_VOTOS))

painel_lvs_secao_votos_inner_mun_ano_cargo_turno <- painel_lvs_secao_votos_inner_lv %>%
  select(-Largest_Party_LV) %>%
  nest(Zonas_LVs=c(NUM_ZONA, NR_LOCVOT,lat,lon, NUM_VOTAVEL, QTDE_VOTOS, Pct_Votos_LV))

#painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% filter(ANO_ELEICAO==2016 &
#                                                              CODIGO_CARGO==11 &
#                                                              COD_MUN_IBGE==3500204)

painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unite("ANO_MUN_CARGO_TURNO",
        ANO_ELEICAO, COD_MUN_IBGE, CODIGO_CARGO, NUM_TURNO) %>%
  ungroup() %>%
  select(-CODIGO_MUNICIPIO) %>%
  mutate(ANO_MUN_CARGO_TURNO=paste0("data/output/Prepared_Ano_Mun/",ANO_MUN_CARGO_TURNO,".rds")) %>%
  rename(object=Zonas_LVs,file=ANO_MUN_CARGO_TURNO) %>%
  pmap(saveRDS)
