library(stringr)
library(lubridate)
library(janitor)
library(cnc)
library(tpur)
library(magrittr)
library(tidyverse)
library(forcats)
data(tidy_cnc, package = 'cnc')

# -----------------------------------------------------------------------------
# isso aqui vai sumir

tabela_assuntos <- download_table("assunto","estadual","primeiro grau") %>%
  build_table()

assuntos_penais <- tabela_assuntos %>%
  filter(str_detect(n1, regex("penal", ignore_case = T))) %>%
  select(dplyr::contains("n")) %>%
  gather(nivel, assunto) %>%
  distinct(assunto, .keep_all = T) %>%
  with(assunto)

assuntos <- tidy_cnc %>%
  select(dplyr::contains("assunto_nm")) %>%
  mutate(id = 1:n()) %>%
  gather(rank, assunto, -id) %>%
  filter(!is.na(assunto)) %>%
  mutate(penal_lgl = assunto %in% assuntos_penais)

assuntos_penal_any <- assuntos %>%
  group_by(id) %>%
  summarise(assunto_penal_any = any(penal_lgl))

assuntos_penal_all <- assuntos %>%
  group_by(id) %>%
  summarise(assunto_penal_all = all(penal_lgl))

tidy_cnc %<>%
  mutate(id = 1:n()) %>%
  left_join(assuntos_penal_all) %>%
  left_join(assuntos_penal_any)

# /isso aqui vai sumir
# -----------------------------------------------------------------------------

# Análises eixo 3
eixo03_filtros <- c('tipo_pena', 'esfera_processo', 'assunto_penal_any', 'instancia')
eixo03_results <- tidy_cnc %>%
  mutate_at(eixo03_filtros, as.character) %>%
  distinct_(.dots = eixo03_filtros) %>%
  group_by_(.dots = names(.)) %>%
  do(d = {
    expr <- sprintf('%s=="%s"', eixo03_filtros, as.character(.[1,]))
    filter_(tidy_cnc, .dots = expr)
  }) %>%
  ungroup() %>%
  # adicionar mutates das análises
  mutate(graf_tempo = map(d, graf_tempo))




