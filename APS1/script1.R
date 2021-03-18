library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx") %>% clean_names()

media_gols_casa <- as.data.frame(cbind(tapply(dados$golcasa, dados$casa, mean))) %>% 
                    rename('media_casa' = 'V1')
medias_gols_fora <- as.data.frame(cbind(tapply(dados$golvisitante, dados$visitante, mean))) %>% 
                    rename('media_fora' = 'V1')

media_gols <- bind_cols(media_gols_casa, medias_gols_fora) %>% rename('media_casa' = 'media...1','media_fora' = 'media...2')