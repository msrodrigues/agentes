library(pacman)
p_load(openxlsx)
p_load(scales)
p_load(readxl)
p_load(WriteXLS)
p_load(ggthemes)
p_load(RColorBrewer)
p_load(lubridate)
p_load(caret)
p_load(tidyverse)
p_load(here)
p_load(googlesheets4)
p_load(DescTools)
p_load(obAnalytics)
p_load(collapse)
p_load(tictoc)
p_load(directlabels)
p_load(zoo)
p_load(kableExtra)
p_load(vroom)
p_load(janitor)
p_load(msrpack)
p_load(ggmap)
p_load(patchwork)
p_load(esquisse)

Sys.setenv(TZ="America/Recife")
options(tz="America/Recife")
Sys.getenv("TZ")
options(scipen = 999999)
Sys.setlocale("LC_TIME", "pt_BR")


imesf <- read_xls(path = "data/Tabela  Trabalhadores.xls") %>% 
  clean_names()

unidades_imesf <- imesf %>% 
  distinct(centro_de_custo) %>% 
  rename(us = centro_de_custo) %>% 
  filter(grepl("^CRTB|^UBS|^US|^ESF", us)) %>% 
  mutate(
    nome = str_remove(us, "^UBS |^US |^ESF "),
    nome = str_remove(nome, " - GD(.)*$"),
    nome_limpo = toupper(rm_accent(nome))
  )


acs_raw <- range_read("11BpmBYPMFT-A1vLcdstl2moBq3HpPoWaV_8w2bMX09Q",sheet = "unidades") %>% 
  clean_names()


acs <- acs_raw %>% 
  select(gd, 
         cnes = estab_cnes, 
         nome = estab_nome_fantasia, 
         n_equipes = equipes, 
         n_acs_por_equipe = acs_equipe, 
         acs_51 = emenda_51, 
         acs_imesf = quantidades_acs_no_imesf) %>% 
  mutate(
    acs_51 = replace(acs_51, is.na(acs_51), 0),  
    acs_imesf = replace(acs_imesf, is.na(acs_imesf), 0),   
    cnes = factor(cnes),
    n_equipes = replace(n_equipes, is.na(n_equipes), 0),
    total_acs_necessarios = n_equipes * n_acs_por_equipe,
    total_acs_e51_imesf = acs_51 + acs_imesf,
    acs_necessarios_sem51 = total_acs_necessarios - acs_51,
    us_suprida_com51 = ifelse(acs_51 >= total_acs_necessarios, TRUE, FALSE)
  ) %>% 
  filter(nome != "CGVS") 


acs_nome <- acs %>% 
  select(nome_sms = nome) %>% 
  mutate(
    nome_limpo = str_remove(nome_sms, "^CENTRO DE EXTENSAO UNIVERSITARIA |^CLINICA DA FAMILIA |^UNIDADE DE SAUDE "),
    nome_limpo = rm_accent(nome_limpo)
  )

unidades <- left_join(unidades_imesf, acs_nome, by = "nome_limpo")

unidades_fechadas = c("PLANALTO",
                      "ALTO EMBRATEL",
                      "ORFANOTROFIO",
                      "MATO GROSSO",
                      "NOSSA SENHORA MEDIANEIRA",
                      "BELEM VELHO",
                      "VILA GAUCHA",
                      "QUINTA DO PORTAL", 
                      "TRONCO"
                      )

unidades_nomes_trocados <- c("MARIA DA CONCEICAO", "SAUDE INDIGENA", "VILA DOS COMERCIARIOS")

unidades_ghc <- c("VILA FLORESTA",
                  "VILA SESC")

unidades_para_remover <- c("CRTB")

sms_us <-  range_read(ss = "1sDw-hef5Vr6WggoYC_VZDejziGRnSvVSD5rOTOBOu9A", sheet = "GERAL - 130 US") %>% 
  clean_names() %>% 
  mutate(
    nome_limpo = str_remove(unidade_de_saude, "^US |^CF "),
    nome_limpo = toupper(rm_accent(nome_limpo)),
    us_aberta = nome_limpo %!in% unidades_fechadas
  ) %>% 
  filter(!grepl("Equipe Consultório na Rua Centro", unidade_de_saude)) %>% 
  select(unidade_de_saude, nome_limpo)

us <- left_join(unidades, sms_us, by = "nome_limpo")
