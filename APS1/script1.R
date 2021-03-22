library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx", 
                            col_types = c("text", "text", "numeric", 
                                          "date", "text", "text", "text", "numeric", 
                                          "numeric", "numeric", "text", "numeric", 
                                          "numeric", "numeric")) %>% clean_names()

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


#### TRABALHANDO COM AS ESTAÇÕES DO ANO, RESULTADOS DE JOGO DADA PROBABILIDADE OU NÃO ####

# Analisando o resultado no outono e inverno de 2014
dados %>% group_by(res) %>% filter(data > '2014-03-20' & data < '2014-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2014-03-20' & data < '2014-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2014
dados %>% group_by(res) %>% filter(data > '2014-09-22' & data < '2015-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2014-09-22' & data < '2015-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado no outono e inverno de 2016
dados %>% group_by(res) %>% filter(data > '2016-03-20' & data < '2016-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2016-03-20' & data < '2016-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2016
dados %>% group_by(res) %>% filter(data > '2016-09-22' & data < '2017-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2016-09-22' & data < '2017-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado no outono e inverno de 2018
dados %>% group_by(res) %>% filter(data > '2018-03-20' & data < '2018-09-22' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2018-03-20' & data < '2018-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2018
dados %>% group_by(res) %>% filter(data > '2018-09-22' & data < '2019-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2018-09-22' & data < '2019-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# Analisando o resultado no outono e inverno de 2020
dados %>% group_by(res) %>% filter(data > '2020-03-20' & data < '2020-09-22' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2020-03-20' & data < '2020-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# Analisando o resultado na primavera e verão de 2020
dados %>% group_by(res) %>% filter(data > '2020-09-22' & data < '2021-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2020-09-22' & data < '2021-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))



#### TRABALHANDO COM AS ESTAÇÕES DO ANO, ESTADOS, RESULTADOS DE JOGO DADA PROBABILIDADE OU NÃO

#### TRABALHANDO COM AS ESTAÇÕES DO ANO, ESTADOS, RESULTADOS DE JOGO DADA PROBABILIDADE OU NÃO ####
# Analisando o resultado no outono e inverno de 2014
dados %>% group_by(res) %>% filter(data > '2014-03-20' & data < '2014-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2014-03-20' & data < '2014-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2014
dados %>% group_by(res) %>% filter(data > '2014-09-22' & data < '2015-03-20' & estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2014-09-22' & data < '2015-03-20' & estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado no outono e inverno de 2016
dados %>% group_by(res) %>% filter(data > '2016-03-20' & data < '2016-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2016-03-20' & data < '2016-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2016
dados %>% group_by(res) %>% filter(data > '2016-09-22' & data < '2017-03-20' & estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2016-09-22' & data < '2017-03-20' & estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado no outono e inverno de 2018
dados %>% group_by(res) %>% filter(data > '2018-03-20' & data < '2018-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro') ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2018-03-20' & data < '2018-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

# Analisando o resultado na primavera e verão de 2018
dados %>% group_by(res) %>% filter(data > '2018-09-22' & data < '2019-03-20' & estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2018-09-22' & data < '2019-03-20' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro') ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# Analisando o resultado no outono e inverno de 2020
dados %>% group_by(res) %>% filter(data > '2020-03-20' & data < '2020-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                              'Ceara', 'Pernambuco', 'Rio de Janeiro') ) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2020-03-20' & data < '2020-09-22' & !estado %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))


# Analisando o resultado na primavera e verão de 2020
dados %>% group_by(res) %>% filter(data > '2020-09-22' & data < '2021-03-20' & estado %in% c('Alagoas', 'Bahia', 'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
                            summarise(n = n()) %>% mutate(freq = n / sum(n))
dados %>% group_by(res) %>% filter(pc > 0.5 & data > '2020-09-22' & data < '2021-03-20' & estados %in% c('Alagoas', 'Bahia', 
                                                                                                         'Ceara', 'Pernambuco', 'Rio de Janeiro')) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))
