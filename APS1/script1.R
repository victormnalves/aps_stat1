library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx") %>% clean_names()

#### TRABALHANDO COM PROBABILIDADES E TEMPORADA POR RESULTADO ####

# desconsiderando a probabilidade e considerando temporada
dados %>% group_by(res) %>% filter(temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# considerando a probabilidade do time mandante vencer segregado por ano

dados %>% group_by(res) %>% filter(pc > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5) %>% summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# considerando a probabilidade do time visitante vencer segregado por ano
dados %>% group_by(res) %>% filter(pv > 0.5) %>% summarise(n = n()) %>% 
  mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pv > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pv > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


#### TRABALHANDO COM PROBABILIDADES, TEMPORADA E GOLS DADO QUE A CASA VENCEU ####

# analisando a quantidade de gols dado o resultado e probabilidades da casa e que ele venceu
dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > 0.5) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > 0.5) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# desconsiderando a probabilidade, considerando temporada e gols da casa e que ele venceu, segregado por ano
dados %>% group_by(golcasa) %>% filter(res == 'C' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'C' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

dados %>% group_by(golcasa) %>% filter(res == 'C' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res =='C' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# considerando a probabilidade do time da casa e que ele venceu, segregado por ano

dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


#### TRABALHANDO COM PROBABILIDADES, TEMPORADA E GOLS DADO QUE O VISITANTE VENCEU ####

# analisando a quantidade de gols dado o resultado e probabilidades do visitante ele venceu
dados %>% group_by(golcasa) %>% filter(res == 'V' & pv > 0.5) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'V' & pv > 0.5) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# desconsiderando a probabilidade, considerando temporada e gols do visitante e que ele venceu
dados %>% group_by(golcasa) %>% filter(res == 'V' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'V' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

dados %>% group_by(golcasa) %>% filter(res == 'V' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res =='V' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# considerando a probabilidade do time visitante vencer e que ele venceu

dados %>% group_by(golcasa) %>% filter(res == 'V' & pv > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'V' & pv > 0.5 & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


dados %>% group_by(golcasa) %>% filter(res == 'V' & pv > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(golvisitante) %>% filter(res == 'V' & pv > 0.5 & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
