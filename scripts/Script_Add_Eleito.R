library(tidyverse)
library(stringr)
library(cepespR)

files_orig <- tibble(file=list.files("data/output/Prepared_Ano_Mun")) %>%
  mutate(file_full=file.path("data/output/Prepared_Ano_Mun/", file)) 

add_branco_nulo <- function(df) {
  df %>% add_row(NUM_VOTAVEL=95, NOME_CANDIDATO="VOTO BRANCO") %>%
    add_row(NUM_VOTAVEL=96, NOME_CANDIDATO="VOTO NULO")
}

join_in_df <- function(data, cands) {
  data %>% left_join(cands, by=c("NUM_VOTAVEL")) 
}

parties <- tibble(NUM_VOTAVEL=(c(10, 11, 12, 13, 14, 15,
                                             16, 17, 18, 19,
                                             20, 21, 22, 23, 25,
                                             27, 28, 29, 30, 35,
                                             36, 40,
                                             43, 45, 50, 51,
                                             55, 65, 70, 77,
                                             80, 90,
                                             31, 33, 44, 54)),
                        NOME_CANDIDATO=c("REP","PP","PDT","PT","PTB","(P)MDB",
                                        "PSTU","PSL","REDE","PODE",
                                        "PSC","PCB","PL","CDN","DEM",
                                        "DC","PRTB","PCO","NOVO","PMN",
                                        "PTC","PSB",
                                        "PV","PSDB","PSOL","PARTRI",
                                        "PSD","PCdoB","AVANTE","SD",
                                        "UP","PROS",
                                        "PHS","PMN","PRP", "PPL"))

#files_orig %>% slice(5000) #5000 is prefeito
#i <- 3962

ibge_key <- read_csv("data/input/aux_mun_code.csv") %>%
  rename("SIGLA_UE"="COD_MUN_TSE") %>%
  select(SIGLA_UE, COD_MUN_IBGE)

add_cands <- function (i) {

print(i)

files <- files_orig[i,]

files <- files %>% mutate(data=map(file_full,readRDS))
  
files <- files %>% 
  separate(file, into=c("year", "COD_MUN_IBGE", "position", "Turno"), sep="_", remove=F) %>%
  mutate(Turno=str_split(Turno, "\\."),
         Turno=as.numeric(map_chr(Turno, 1)),
         COD_MUN_IBGE=as.numeric(COD_MUN_IBGE),
         position=as.numeric(position))

files <- files %>% left_join(ibge_key, by="COD_MUN_IBGE") %>%
  rename(SIGLA_UE_data=SIGLA_UE) %>%
  mutate(CODIGO_UF=str_sub(COD_MUN_IBGE, 1, 2),
         SIGLA_UF_data=case_when(CODIGO_UF==11~"RO",
                            CODIGO_UF==12~"AC",
                            CODIGO_UF==13~"AM",
                            CODIGO_UF==14~"RR",
                            CODIGO_UF==15~"PA",
                            CODIGO_UF==16~"AP",
                            CODIGO_UF==17~"TO",
                            CODIGO_UF==21~"MA",
                            CODIGO_UF==22~"PI",
                            CODIGO_UF==23~"CE",
                            CODIGO_UF==24~"RN",
                            CODIGO_UF==25~"PB",
                            CODIGO_UF==26~"PE",
                            CODIGO_UF==27~"AL",
                            CODIGO_UF==28~"SE",
                            CODIGO_UF==29~"BA",
                            CODIGO_UF==31~"MG",
                            CODIGO_UF==32~"ES",
                            CODIGO_UF==33~"RJ",
                            CODIGO_UF==35~"SP",
                            CODIGO_UF==41~"PR",
                            CODIGO_UF==42~"SC",
                            CODIGO_UF==43~"RS",
                            CODIGO_UF==50~"MS",
                            CODIGO_UF==51~"MT",
                            CODIGO_UF==52~"GO",
                            CODIGO_UF==53~"DF"),
         SIGLA_UF_data=case_when(position==1~"BR",
                            TRUE~SIGLA_UF_data))

files <- files %>%
  mutate(eleito=map2(year, position, state=SIGLA_UF_data, get_votes, 
                     columns_list=list("ANO_ELEICAO","NUM_TURNO","CODIGO_CARGO",
                                       "NUMERO_CANDIDATO", "NOME_URNA_CANDIDATO",
                                       "SIGLA_UE", "SIGLA_UF", 
                                       "COD_SIT_TOT_TURNO")))



files <- files %>% 
  mutate(cands=map(cands, filter, NUM_TURNO==Turno),
                 cands=map(cands, filter, CODIGO_CARGO==position),
                 cands=map(cands, filter, SIGLA_UF==SIGLA_UF_data),
                 cands=map(cands, filter, COD_SIT_TOT_TURNO!=-1),
                 cands=map(cands, rename, "NUM_VOTAVEL"="NUMERO_CANDIDATO"))

files <- files %>% 
  mutate(data=map(data, mutate, NUM_VOTAVEL=str_split(NUM_VOTAVEL, " - ")),
         data=map(data, mutate, NUM_VOTAVEL=map_chr(NUM_VOTAVEL, 1)),
         data=map(data, mutate, NUM_VOTAVEL=as.numeric(NUM_VOTAVEL)))

#sort(unique(files$data[[1]]$NUM_VOTAVEL))
#sort(unique(files$cands[[1]]$NUM_VOTAVEL))

if (files$position %in% c(11, 13)) {
  files <- files %>% 
    mutate(cands=map(cands, filter, SIGLA_UE==SIGLA_UE_data))
}

if (files$position %in% c(5, 6, 7)) {
files <- files %>% mutate(cands=map(cands, bind_rows, parties))
}

files <- files %>% mutate(cands=map(cands, select, NUM_VOTAVEL, NOME_CANDIDATO),
                          cands=map(cands, add_branco_nulo))

files <- files %>% mutate(data_cands=map2(data, cands,join_in_df))

#files$data_cands[[1]] %>% sample_n(10) #Fails for parties, as it should

out_file <- file.path("data/output/Prepared_Ano_Mun/", files$file)

files$data_cands[[1]] %>% saveRDS(out_file)

}

for (i in 19084:20863) {
  add_cands(i)  
}

#20864 not present; missing a file??

for (i in 20751:20864) {
  add_cands(i)  
}

files$cands[[1]]

temp <- get_candidates(year=2018, position=1)
temp$NOME_CANDIDATO

temp %>% select(NOME_CANDIDATO, COD_SITUACAO_CANDIDATURA)
##Problem: Lula appears in 2018 as Pres candidate for PT as I haven't filtered for COD_SITUACAO_CANDIDATURA...


#new_files_source <- files_orig %>% slice(c(1:200, 5000:5200)) %>% pull(file)
new_files_source <- list.files("data/output/Prepared_Ano_Mun/") %>% sample(100)

old_files <- file.path("../../spatial_municipios_8/spatial.municipios/data/output/Prepared_Ano_Mun",new_files_source)
new_files <- file.path("data/output/Prepared_Ano_Mun",new_files_source)

old_data <- old_files %>% map(readRDS)
new_data <- new_files %>% map(readRDS)

difs <- tibble(file=new_files_source,
       old_n=old_data %>% map_dbl(nrow),
       new_n=new_data %>% map_dbl(nrow)) %>%
  rownames_to_column() %>%
  filter(old_n!=new_n)

difs %>% print(n=100)

#11 always different
#13 always different
#6 slightly different
#Sometimes 1 slightly different, and 3

old_data_temp <- old_data %>% pluck(1)
new_data_temp <- new_data %>% pluck(1)

new_data_temp %>% group_by(NUM_ZONA, NR_LOCVOT, NUM_VOTAVEL, QTDE_VOTOS) %>% 
  filter(n()>1) %>% print(n=30)

get_elections(year=2018, position=3) %>%
  filter(COD_MUN_IBGE==3503406 & NUMERO_CANDIDATO==29) %>%
  select(NOME_URNA_CANDIDATO)

get_candidates(year=2018, position=3) %>%
  filter(NUMERO_CANDIDATO==29 & SIGLA_UE=="SP") %>%
  select(NOME_URNA_CANDIDATO, COD_SITUACAO_CANDIDATURA, COD_SIT_TOT_TURNO)

get_candidates(year=2018, position=6) %>%
  filter(NUMERO_CANDIDATO==5145) %>%
  select(NOME_URNA_CANDIDATO, COD_SITUACAO_CANDIDATURA, COD_SIT_TOT_TURNO, DESC_SIT_TOT_TURNO)

#line 21 - problem is COD_SIT_TOT_TURNO - remove -1
#line 55 - problem is COD_SIT_TOT_TURNO - need to remove -1
#line 43 - problem is COD_SIT_TOT_TURNO - need to remove -1

###***Also note old-date has party added to NUM_VOTAVEL which isn't true for new_dat. At least for cargo 3.

#temp <- readRDS("data/output/Prepared_Ano_Mun/2006_1100205_6_1.rds")
