library(stringr)
library(lubridate)
library(janitor)
library(cnc)
library(tpur)
library(magrittr)
library(tidyverse)
library(forcats)
data(tidy_cnc, package = 'cnc')
tidy_cnc %<>% ungroup()

source('R/eixo01.R')
source('R/eixo02.R')
source('R/eixo03.R')

# Análises eixo 3
eixo03_filtros <- c('tipo_pena', 'esfera_processo', 'assunto_penal_any', 'instancia')
eixo03_results <- tidy_cnc %>%
  distinct_(.dots = eixo03_filtros) %>%
  group_by_(.dots = names(.)) %>%
  do(d = {
    expr <- sprintf('%s=="%s"', eixo03_filtros, as.character(.[1,]))
    filter_(tidy_cnc, .dots = expr)
  }) %>%
  ungroup() %>%
  # adicionar mutates das análises
  mutate(n = map_int(d, nrow),
         graf_tempo = map(d, graf_tempo))




