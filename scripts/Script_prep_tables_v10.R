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

painel_lvs_secao_all <- painel_lvs_secao %>% 
  select(ANO, CD_LOCALIDADE_TSE, NR_ZONA, NR_LOCVOT, NR_SECAO, lat, lon) %>%
  left_join(ibge_key, by=c("CD_LOCALIDADE_TSE"="SIGLA_UE")) %>%
  rename("NUM_ZONA"="NR_ZONA",
         "NUM_SECAO"="NR_SECAO",
         "CODIGO_MUNICIPIO"="CD_LOCALIDADE_TSE",
         "ANO_ELEICAO"="ANO")

muns_disponivel <- unique(painel_lvs_secao_all$COD_MUN_IBGE)

read_csv("data/input/aux_mun_code.csv") %>%
  select(SIGLA_UF, COD_MUN_IBGE, NOME_MUNICIPIO) %>%
  filter(COD_MUN_IBGE %in% muns_disponivel) %>%
  saveRDS("data/input/IBGE_Muns.rds")

### Dados eleitorais
#NOT ON GITHUB
files <- list.files("C:/Users/jonny/Google Drive/Academic/USP/Data/Electoral/Seções Eleitorais",
                    pattern="*.txt", full.names=T, recursive=T)

#files <- files[files %>% str_detect("RJ.txt")]
#files <- files[files %>% str_detect("AC.txt")]

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
#files_2018 <- files_2018[files_2018 %>% str_detect("AC.csv")]

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

#painel_lvs_secao_votos_inner %>% saveRDS("../LV_dados/painel_lvs_secao_votos_inner.rds")
rm(votes_secao, votes_secao_2018, painel_lvs_secao, painel_lvs_secao_all)
#painel_lvs_secao_votos_inner <- readRDS("../LV_dados/painel_lvs_secao_votos_inner.rds")

painel_lvs_secao_votos_inner_1 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2006, 2008))
painel_lvs_secao_votos_inner_2 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2010, 2012))
painel_lvs_secao_votos_inner_3 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2014, 2016))
painel_lvs_secao_votos_inner_4 <- painel_lvs_secao_votos_inner %>% filter(ANO_ELEICAO %in% c(2018))

rm(painel_lvs_secao_votos_inner)

painel_lvs_secao_votos_inner_1 <- painel_lvs_secao_votos_inner_1 %>% 
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))

painel_lvs_secao_votos_inner_2 <- painel_lvs_secao_votos_inner_2 %>% 
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))

painel_lvs_secao_votos_inner_3 <- painel_lvs_secao_votos_inner_3 %>% 
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))

painel_lvs_secao_votos_inner_4 <- painel_lvs_secao_votos_inner_4 %>% 
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner_1 %>% 
  bind_rows(painel_lvs_secao_votos_inner_2) %>%
  bind_rows(painel_lvs_secao_votos_inner_3) %>%
  bind_rows(painel_lvs_secao_votos_inner_4)

#painel_lvs_secao_votos_inner_lv %>% saveRDS("../LV_dados/painel_lvs_secao_votos_inner_lv.rds")
#painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner %>% 
#  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, lat, lon) %>%
#  summarize(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=T))
#painel_lvs_secao_votos_inner_lv <- readRDS("../LV_dados/painel_lvs_secao_votos_inner_lv.rds")
rm(painel_lvs_secao_votos_inner_1,painel_lvs_secao_votos_inner_2,painel_lvs_secao_votos_inner_3,painel_lvs_secao_votos_inner_4)

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner_lv %>%
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO) %>%
  mutate(Largest_Party_LV=case_when(QTDE_VOTOS==max(QTDE_VOTOS,na.rm=T)~1,
                                    TRUE~0))

painel_lvs_secao_votos_inner_lv <- painel_lvs_secao_votos_inner_lv %>%
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, COD_MUN_IBGE, NUM_ZONA, NR_LOCVOT, CODIGO_CARGO, NUM_TURNO) %>%
  mutate(Pct_Votos_LV=100*(QTDE_VOTOS/sum(QTDE_VOTOS,na.rm=T)))

#painel_lvs_secao_votos_inner_lv %>% ungroup() %>% 
#  slice(1:100) %>% 
#  select(NR_LOCVOT,CODIGO_CARGO, NUM_TURNO, QTDE_VOTOS,Largest_Party_LV, Pct_Votos_LV)

painel_lvs_secao_votos_inner_mun_ano <- painel_lvs_secao_votos_inner_lv %>%
  nest(Zonas=c(NUM_ZONA, NR_LOCVOT,lat,lon,CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, QTDE_VOTOS))

painel_lvs_secao_votos_inner_mun_ano_cargo_turno <- painel_lvs_secao_votos_inner_lv %>%
  select(-Largest_Party_LV) %>%
  nest(Zonas_LVs=c(NUM_ZONA, NR_LOCVOT,lat,lon, NUM_VOTAVEL, QTDE_VOTOS, Pct_Votos_LV))

#painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% saveRDS("../LV_dados/painel_lvs_secao_votos_inner_mun_ano_cargo_turno.rds")
#painel_lvs_secao_votos_inner_mun_ano_cargo_turno <- readRDS("../LV_dados/painel_lvs_secao_votos_inner_mun_ano_cargo_turno.rds")


#painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% filter(ANO_ELEICAO==2016 &
#                                                              CODIGO_CARGO==11 &
#                                                              COD_MUN_IBGE==3500204)

painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unite("ANO_MUN_CARGO_TURNO",
        ANO_ELEICAO, COD_MUN_IBGE, CODIGO_CARGO, NUM_TURNO) %>%
  ungroup() %>%
  select(-CODIGO_MUNICIPIO, -SIGLA_UF, -SIGLA_UE) %>%
  mutate(ANO_MUN_CARGO_TURNO=paste0("data/output/Prepared_Ano_Mun/",ANO_MUN_CARGO_TURNO,".rds")) %>%
  rename(object=Zonas_LVs,file=ANO_MUN_CARGO_TURNO) %>%
  pmap(saveRDS)

rm(painel_lvs_secao_votos_inner_mun_ano, painel_lvs_secao_votos_inner_lv, painel_lvs_secao, painel_lvs_secao_all)

#### Load Candidate details
cands_args <- expand_grid(year=seq(2006, 2018, 2),
            position=c(1,3,5,6,7,11,13))

cand_data <- cands_args %>% pmap(get_candidates,
                                 columns_list=list("ANO_ELEICAO","NUM_TURNO","CODIGO_CARGO",
                                                   "NUMERO_CANDIDATO", "NUMERO_PARTIDO",
                                                   "NOME_CANDIDATO", "NOME_URNA_CANDIDATO",
                                                   "SIGLA_UE", "SIGLA_UF"))


cand_data <- cand_data %>% bind_rows() %>%
  rename("NUM_VOTAVEL"="NUMERO_CANDIDATO")

cand_data %>% saveRDS("../../cand_data.rds")

library(data.table)

painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006 <- painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unnest() %>%
  filter(ANO_ELEICAO %in% c(2006)) %>%
  as.data.table() %>%
  mutate(SIGLA_UE_MATCH=case_when(CODIGO_CARGO==1~"BR",
  CODIGO_CARGO %in% c(3,5,6,7,8,11,13) ~SIGLA_UF)) %>%
  merge(cand_data %>% filter(ANO_ELEICAO %in% c(2006)) %>% as.data.table(),
            by=c("ANO_ELEICAO",
                 "CODIGO_CARGO",
                 "NUM_TURNO",
                 "SIGLA_UE_MATCH"="SIGLA_UF",
                 "NUM_VOTAVEL"))

painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006 %>% saveRDS("../../painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006.rds")

rm(painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006)

painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2008 <- painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unnest() %>%
  filter(ANO_ELEICAO %in% c(2008)) %>%
  as.data.table() %>%
  mutate(SIGLA_UE_MATCH=case_when(CODIGO_CARGO==1~"BR",
                                  CODIGO_CARGO %in% c(3,5,6,7,8,11,13) ~SIGLA_UF)) %>%
  merge(cand_data %>% filter(ANO_ELEICAO %in% c(2008)) %>% as.data.table(),
        by=c("ANO_ELEICAO",
             "CODIGO_CARGO",
             "NUM_TURNO",
             "SIGLA_UE_MATCH"="SIGLA_UF",
             "NUM_VOTAVEL"))

painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006 %>% saveRDS("../../painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand_2006.rds")


painel_lvs_secao_votos_inner_mun_ano_cargo_turno_cand <- painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unnest() %>%
  mutate(SIGLA_UE_MATCH=case_when(CODIGO_CARGO==1~"BR",
                                  CODIGO_CARGO %in% c(3,5,6,7,8,11,13) ~SIGLA_UF)) %>%
  left_join(cand_data %>%
            by=c("ANO_ELEICAO",
                 "CODIGO_CARGO",
                 "NUM_TURNO",
                 "SIGLA_UE_MATCH"="SIGLA_UF",
                 "NUM_VOTAVEL"))

#"SIGLA_UE_MATCH"="SIGLA_UE",
cand_data %>% filter(ANO_ELEICAO==2008 & CODIGO_CARGO==11 & NUM_VOTAVEL==13 & NUM_TURNO==1 & SIGLA_UF=="AC")

painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% unnest() %>%
  filter(ANO_ELEICAO==2008 & CODIGO_CARGO==11 & NUM_VOTAVEL==13 & NUM_TURNO==1 & SIGLA_UF=="AC") 

temp %>% ungroup() %>% 
  select(ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL, NOME_URNA_CANDIDATO) %>%
  sample_n(20)

temp %>% distinct(NOME_URNA_CANDIDATO)

cand_data %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==6) %>%
  select(ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, SIGLA_UF, SIGLA_UE, NUM_VOTAVEL, NOME_URNA_CANDIDATO)

painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% unnest() %>%
  filter(ANO_ELEICAO==2006 & CODIGO_CARGO==6)  %>%
  mutate(SIGLA_UE_MATCH=case_when(CODIGO_CARGO==1~"BR",
                                  CODIGO_CARGO %in% c(3,5,6,7,8) ~SIGLA_UF)) %>%
  ungroup() %>%
  select(ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, SIGLA_UF, SIGLA_UE_MATCH, NUM_VOTAVEL) %>%
  distinct(NUM_VOTAVEL) %>%
  print(n=100)

#FOR CARGO 1, need to match without COD_MUNICIPIO. SIGLA_UE doesn't work either. Need to add "BR" to painel. OK
#FOR CARGO 3, need to match with state, can use SIGLA_UE from cand_data and SIGLA_UF from painel
#FOR CARGOS 11, 13, need to check NUM_VOTAVEL/CANDIDATO can't be repeated in muns in same state.

#Where need to match on state and SIGLA_UE doesn't work, can use SIGLA_UF from painel to match SIGLA_UE in cand_data
#For Pres, need to make cand_data SIGLA_UE "BR"

temp$CODIGO_MUNICIPIO
cand_data$CODIGO_MUNICIPIO

#Tricky to match - sigla_UE is letters for generals, but codigo_mun for vereadores
#and i have cod_mun but want to match on state

cand_data %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==1) #NUM_VOTAVEL=party

temp %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==1) %>%
  select(ANO_ELEICAO, CODIGO_MUNICIPIO, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL) #NUM_VOTAVEL=party

cand_data %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==3) #NUM_VOTAVEL=party

temp %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==3) %>%
  select(ANO_ELEICAO, CODIGO_MUNICIPIO, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL) #NUM_VOTAVEL=party

cand_data %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==5) #NUM_VOTAVEL=Candidate

temp %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==5) %>%
  select(ANO_ELEICAO, CODIGO_MUNICIPIO, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL) #NUM_VOTAVEL=Candidate

cand_data %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==6) %>% distinct(NUM_VOTAVEL, CODIGO_MUNICIPIO) 
#COD_MUN does differeniate. NUM_VOTAVEL=Candidate

temp %>% filter(ANO_ELEICAO==2006 & CODIGO_CARGO==6) %>%
  select(ANO_ELEICAO, CODIGO_MUNICIPIO, CODIGO_CARGO, NUM_TURNO, NUM_VOTAVEL) %>% 
  distinct(NUM_VOTAVEL) #COD_MUN doesn't add anything. NUM_VOTAVEL = party


painel_lvs_secao_votos_inner_mun_ano_cargo_turno %>% 
  unite("ANO_MUN_CARGO_TURNO",
        ANO_ELEICAO, COD_MUN_IBGE, CODIGO_CARGO, NUM_TURNO) %>%
  ungroup() %>%
  select(-CODIGO_MUNICIPIO) %>%
  mutate(ANO_MUN_CARGO_TURNO=paste0("data/output/Prepared_Ano_Mun/",ANO_MUN_CARGO_TURNO,".rds")) %>%
  rename(object=Zonas_LVs,file=ANO_MUN_CARGO_TURNO) %>%
  pmap(saveRDS)
