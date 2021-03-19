library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx") %>% clean_names()


#### MEDIA GOLS ####
media_gols_casa <- cbind(tapply(dados$golcasa, dados$casa, mean))
media_gols_fora <- cbind(tapply(dados$golvisitante, dados$visitante, mean))
media_gols <- bind_cols(media_gols_casa, media_gols_fora) %>% rename('media_casa' = '...1','media_fora' = '...2')

### MEDIANA GOLS ###
mediana_gols_casa <- cbind(tapply(dados$golcasa, dados$casa, median))
mediana_gols_fora <- cbind(tapply(dados$golvisitante, dados$visitante, mean))
mediana_gols <- bind_cols(mediana_gols_casa, mediana_gols_fora) %>% rename('mediana_casa' = '...1','median_fora' = '...2')


dados %>% group_by(res)%>% summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golcasa) %>% filter(res == 'C') %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golcasa) %>% filter(res == 'V') %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golcasa) %>% filter(res == 'E') %>% summarise(n = n()) %>% mutate(freq = n / sum(n))



dados %>% ggplot(mapping = aes(golcasa,length(dados), fill = res)) + geom_bar(stat="identity") +
          ggtitle('quantidade de gols por resultado') + xlab('quantidade de gols') + ylab('quantidade de vitórias em casa/visitante ou empate') +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme_minimal()
dados %>% ggplot(mapping = aes(golvisitante,length(dados), fill = res)) + geom_bar(stat="identity") +
          ggtitle('quantidade de gols por resultado') + xlab('quantidade de gols') + ylab('quantidade de vitórias em casa/visitante ou empate') +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme_minimal()
