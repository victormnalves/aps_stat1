library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx") %>% clean_names()
