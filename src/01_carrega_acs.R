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


# Carregamento da planilha dos ACS do IMESF - Fonte APS, planilha da Carol
acs_raw <- range_read("1EjEgzJrzvhPpQ2QoURoBkCDovwsA2b7RVtEmHMSknoo", sheet = "Todos ACS", col_types = "c") %>% 
  clean_names() 

# Reviso os centros de Custos únicos
sort(unique(acs_raw$centro_de_custo)) %>% write.xlsx(file = "report/centros_de_custo.xlsx")

# Tabela dos Agentes ajustada
acs <- acs_raw %>% 
  mutate(
    admissao = mdy(admissao),
    flag_ativo = is.na(rescisao) | rescisao == "EM51",
    
    # Recodifico nomes de unidades que modificaram
    centro_de_custo = case_when(
      centro_de_custo == "US Vila Floresta - GDNHNI" ~ "US Floresta - GDNHNI",
      centro_de_custo == "US Vila SESC - GDLN" ~ "US SESC - GDLN",
      centro_de_custo == "US Maria da Conceição - GDPLP"  ~ "MARIA DA CONCEICAO MARCELO MARTINS MOREIRA",
      centro_de_custo == "US Saúde Indígena - GDPLP" ~ "US Indígena",
      TRUE ~ centro_de_custo
    ),
    nome_limpo_acs = rm_accent(toupper(str_remove(string = centro_de_custo, pattern = "^US |^ESF |^CF "))),
    nome_limpo_acs = str_remove(nome_limpo_acs, " - GD.*$")
    
  )
acs %>% 
  relocate(nome_limpo_acs, .before = nome) %>% 
  relocate(centro_de_custo, .before = nome) %>% 
  arrange(desc(nome_limpo_acs)) %>% write.xlsx("report/acs.xlsx")
  

# Somente ACS ativos
acs_ativos <- acs %>% 
  filter(is.na(rescisao) | rescisao == "EM51") %>% 
  filter(!is.na(admissao))


# Carregamento do banco das Unidades de Saúde da Carol
# us_raw <- range_read("11BpmBYPMFT-A1vLcdstl2moBq3HpPoWaV_8w2bMX09Q",sheet = "unidades") %>% 
#   clean_names()


us_raw <- range_read("191vkPIzTLcf3Jg0IW4I--S7J-VQW1sJaE61L02vxdIM",sheet = "Equipes por US") %>% 
  clean_names()

us <- us_raw %>% 
  select(
    gd, 
    cnes = estab_cnes, 
    us_nome = estab_nome_fantasia, 
    n_equipes = equipes, 
    n_acs_por_equipe = acs_equipe, 
    acs_51 = emenda_51, 
    acs_imesf = quantidades_acs_no_imesf
  ) %>% 
  mutate(
    acs_necessarios = if_else(n_equipes * n_acs_por_equipe> 5, 
                              (n_equipes * n_acs_por_equipe) - 1, 
                              n_equipes * n_acs_por_equipe),
    acs_51 = replace(acs_51, is.na(acs_51), 0),                             
    cnes = factor(cnes),
    n_equipes = replace(n_equipes, is.na(n_equipes), 0),
    acs_necessarios_sem51 = acs_necessarios - acs_51,
    us_suprida_com51 = ifelse(acs_51 >= acs_necessarios, TRUE, FALSE),
    nome_limpo_us =  rm_accent(str_remove(rm_accent(us_nome), "^CENTRO DE EXTENSAO UNIVERSITARIA |^CLINICA DA FAMILIA |^UNIDADE DE SAUDE "))
  ) %>% 
  filter(us_nome != "CGVS") %>% 
  filter(!is.na(acs_necessarios))

# Join dos bancos dos ACS com as US
acs_us <- left_join(acs, us, by = c("nome_limpo_acs" = "nome_limpo_us"), keep = TRUE)




# Demanda de ACS pof unidade
demanda <- us %>% 
  transmute(
    equipes = map2(us$nome_limpo_us, us$acs_necessarios, rep)
  ) %>% 
  unnest(cols = equipes) %>% 
  group_by(equipes) %>% 
  mutate(
    no_equipe = seq_along(equipes),
    equipe = glue::glue("{equipes} - Vaga {no_equipe}")
  ) %>% 
  ungroup() %>% 
  rename(
    "nome_limpo_us" = "equipes"
  )

alocados <- acs_ativos %>% 
  select(nome, rescisao, nome_limpo_acs) %>% 
  mutate(
    contrato = ifelse(is.na(rescisao), "CLT", "EM51") 
  ) %>% 
  group_by(nome_limpo_acs, contrato) %>% 
  tally() %>% 
  pivot_wider(names_from = contrato, values_from = n) %>% 
  mutate(
    CLT = ifelse(is.na(CLT), 0, CLT),
    EM51 = ifelse(is.na(EM51), 0, EM51)
  ) 

us_balanco <- us %>% 
  select(nome_limpo_us, acs_necessarios) %>% 
  left_join(alocados, by = c("nome_limpo_us" = "nome_limpo_acs")) %>% 
  mutate(
    CLT = ifelse(is.na(CLT), 0, CLT),
    EM51 = ifelse(is.na(EM51), 0, EM51),
    balanco = CLT + EM51 - acs_necessarios,
    CLT_necessarios = acs_necessarios - EM51 - CLT,
    flag_plena = balanco == 0
  )

acs_demanda <- acs_ativos %>% 
  count(nome_limpo_acs, name = "alocados") %>% 
  right_join(acs_ativos, by = "nome_limpo_acs") %>% 
  select(nome_limpo_acs, alocados, nome, contrato = rescisao, admissao) %>% 
  mutate(
    contrato = ifelse(is.na(contrato), "CLT", "EM51")
  ) %>% 
  left_join(us_balanco, by = c("nome_limpo_acs" = "nome_limpo_us")) %>% 
  
  group_by(nome_limpo_acs, contrato) %>% 
  mutate(
    contrato = factor(contrato, levels = c("EM51","CLT"), ordered = TRUE),
    posicao = min_rank(admissao)
  ) %>%
  ungroup() %>% 
  mutate(
    flag_desligar = case_when(
      contrato == "EM51" ~ FALSE,
      balanco < 0 ~ FALSE,
      contrato == "CLT" & balanco > 0 & posicao > CLT_necessarios ~ TRUE,
      TRUE ~ FALSE
    )
  )




# Final -------------------------------------------------------------------

existentes <- acs_ativos %>% 
  count(nome_limpo_acs, name = "alocados")

set.seed(45)

idades <- round(rnorm(n = 551, mean = 55, sd = 10))

filhos <- abs(round(rnorm(n = 551, mean = 1, sd = 1)) )


ativos <- acs_ativos %>% 
  left_join(us, by = c("nome_limpo_acs" = "nome_limpo_us")) %>% 
  left_join(existentes, by = "nome_limpo_acs") %>% 
  mutate(idade = idades,
         filhos = filhos, 
         contrato = ifelse(is.na(rescisao), "CLT", "EM51"),
         contrato = factor(contrato, levels = c("EM51","CLT"), ordered = TRUE)
  )


final <- ativos %>% 
  select(nome_limpo_acs, nome, acs_necessarios, alocados, contrato, admissao, idade, filhos, EM51 = acs_51) %>% 
  mutate(
    necessidade_ajustada = acs_necessarios - alocados
  ) %>% 
  arrange(nome_limpo_acs, contrato, admissao, desc(filhos), desc(idade)) %>% 
  group_by(nome_limpo_acs) %>% 
  mutate(
    ordem = row_number()
  ) %>%
  mutate(
    demitir = case_when(
      contrato == "EM51" ~  FALSE,
      necessidade_ajustada == 0 ~ FALSE,
      acs_necessarios >= ordem ~ FALSE,
      TRUE ~ TRUE
    )
  ) 

# Salva binários ----------------------------------------------------------


write_rds(acs, file = "bin/acs.rds")
write_rds(acs_ativos, file = "bin/acs_ativos.rds")
write_rds(us, file = "bin/us.rds")
write_rds(demanda, file = "bin/demanda.rds")
write_rds(acs_demanda, file = "bin/acs_demanda.rds")
write_rds(us_balanco, file = "bin/us_balanco.rds")
write_rds(final, file = "bin/final.rds")

us %>% 
  mutate(
    vagas = acs_necessarios
  )
