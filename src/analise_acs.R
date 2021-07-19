library(pacman)
p_load(openxlsx)
p_load(scales)
p_load(readxl)
p_load(WriteXLS)
p_load(ggthemes)
p_load(RColorBrewer)
p_load(lubridate)
p_load(caret)
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
p_load(tidyverse)

Sys.setenv(TZ="America/Recife")
options(tz="America/Recife")
Sys.getenv("TZ")
options(scipen = 999999)
Sys.setlocale("LC_TIME", "pt_BR")



# Carregamento do banco
acs_raw <- range_read("11BpmBYPMFT-A1vLcdstl2moBq3HpPoWaV_8w2bMX09Q",sheet = "unidades") %>% 
  clean_names()

names(acs_raw)



imesf <- read_xls(path = "data/Tabela  Trabalhadores.xls") %>% 
  clean_names() %>% 
  mutate(
    nome = toupper(str_remove(localizacao, "^GD(.)*->\\s")),
    nome = rm_accent(str_remove(nome, "\\s-\\sGD(.)*$"))
  )


imesf <- imesf %>% 
  filter(cargo == "AGENTE COMUNITARIO DE SAUDE DA ESF")



acs <- acs_raw %>% 
  select(gd, 
         cnes = estab_cnes, 
         nome = estab_nome_fantasia, 
         n_equipes = equipes, 
         n_acs_por_equipe = acs_equipe, 
         acs_51 = emenda_51, 
         acs_imesf = quantidades_acs_no_imesf) %>% 
  mutate(
    acs_51 = replace(acs_51, is.na(acs_51), 0),                               # Remove NAs
    cnes = factor(cnes),
    n_equipes = replace(n_equipes, is.na(n_equipes), 0),
    acs_necessarios = n_equipes * n_acs_por_equipe, 
    acs_necessarios_sem51 = acs_necessarios - acs_51,
    us_suprida_com51 = ifelse(acs_51 >= acs_necessarios, TRUE, FALSE)
    # acs_clt_necessarios = acs_necessarios - acs_51,
    # us_suprida_com51 = ifelse(acs_clt_necessarios <=0, TRUE, FALSE),
    # total_acs = acs_51 + acs_imesf,                                           # Total de ACS trabalhando (E.51 + ACS imesf)
    # balanco = total_acs - acs_necessarios,
    # acs_para_desligar = ifelse(acs_clt_necessarios <= 0, 0,0 ),
    # balanco_clt = acs_clt_necessarios - acs_necessarios
  ) %>% 
  filter(nome != "CGVS") 

acs_supridas_com51 <- acs %>% 
  filter(us_suprida_com51) 

acs_clt <- acs %>% 
  filter(!us_suprida_com51) %>% 
  select(1:3, acs_necessarios_sem51, acs_imesf) %>% 
  mutate(
    balanco = acs_imesf - acs_necessarios_sem51
  )

acs_clt_demissivel <- acs_clt %>% 
  filter(balanco > 0)
  
# Total passível de demissão mantendo as equipes completas
sum(acs_clt_demissivel$balanco)

# Total de agentes necessários
sum(acs$acs_necessarios, na.rm = TRUE)


# Quantidade de Unidades de Saúde
n_unidades <-  length(unique(acs$nome))

# Quantidades de equipes de ESA
sum(acs$n_equipes)

# Quantidade de acs necessários
sum(acs$acs_necessarios, na.rm = TRUE)

# total de unidades supridas
sum(acs$us_suprida_com51)

# total agentes CLT faltantes para completar equipes
acs_faltantes <- acs %>% 
  filter(!us_suprida_com51) %>% 
  pull(acs_clt_necessarios) %>% 
  sum()


