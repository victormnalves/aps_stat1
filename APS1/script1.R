library(tidyverse)
library(janitor)
library(gridExtra)

dados <- readxl::read_excel("C:/Users/alves/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx", 
                            col_types = c("text", "text", "numeric", 
                                          "date", "text", "text", "text", "numeric", 
                                          "numeric", "numeric", "text", "numeric", 
                                          "numeric", "numeric")) %>% clean_names() %>% 
                            mutate('somagols' = golcasa+golvisitante)

#### TRABALHANDO COM PROBABILIDADES E TEMPORADA POR RESULTADO ####

# desconsiderando a probabilidade e considerando temporada
resultado_n2020 <- dados %>% group_by(res) %>% filter(temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
resultado_2020 <- dados %>% group_by(res) %>% filter(temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# considerando a probabilidade do time mandante vencer segregado por ano


resultado_pc <- dados %>% group_by(res) %>% filter(pc > pv) %>% summarise(n = n()) %>% 
  mutate(freq = round((n / sum(n))*100,2))
resultado_n2020_pc <- dados %>% group_by(res) %>% filter(pc > pv & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
resultado_2020_pc <- dados %>% group_by(res) %>% filter(pc > pv & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# considerando a probabilidade do time visitante vencer segregado por ano
resultado_pv <- dados %>% group_by(res) %>% filter(pv > pc) %>% summarise(n = n()) %>% 
  mutate(freq = round((n / sum(n))*100,2))
resultado_n2020_pv <- dados %>% group_by(res) %>% filter(pv > pc & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
resultado_2020_pv <- dados %>% group_by(res) %>% filter(pv > pc & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# UNINDO OS APENAS RESULTADOS
resultados <- as.tibble(cbind(resultado_2020$res,resultado_2020$n,resultado_2020$freq,
                    resultado_n2020$n,resultado_n2020$freq, 
                    resultado_pc$n, resultado_pc$freq,
                    resultado_pv$n, resultado_pv$freq,
                    resultado_2020_pc$n, resultado_2020_pc$freq, 
                    resultado_n2020_pc$n, resultado_n2020_pc$freq, 
                    resultado_2020_pv$n, resultado_2020_pv$freq, 
                    resultado_n2020_pv$n, resultado_n2020_pv$freq))

names(resultados) <- c('r','n_2020','f_2020',
                       'n_n2020','f_n2020',
                       'n_pc','f_pc',
                       'n_pv','f_pv',
                       'n_2020_pc','f_2020_pc',
                       'n_n2020_pc','f_n2020_pc',
                       'n_2020_pv','f_2020_pv',
                       'n_n2020_pc','f_n2020_pv')

resultados2 <- as.tibble(cbind(resultado_2020$res,resultado_2020$freq,
                               resultado_n2020$freq, 
                               resultado_pc$freq,
                               resultado_pv$freq,
                               resultado_2020_pc$freq, 
                               resultado_n2020_pc$freq, 
                               resultado_2020_pv$freq, 
                               resultado_n2020_pv$freq))
names(resultados2) <- c('r','f_2020',
                       'f_n2020',
                       'f_pc',
                       'f_pv',
                       'f_2020_pc',
                       'f_n2020_pc',
                       'f_2020_pv',
                       'f_n2020_pv')
#### TRABALHANDO COM PROBABILIDADES, TEMPORADA E GOLS DADO QUE A CASA VENCEU ####

# analisando a quantidade de gols dados o resultado e probabilidades da casa e que ele venceu
golc_c_pc <- dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > pv) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_c_pc <- dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > pv) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


# desconsiderando a probabilidade, considerando temporada e gols da casa e que ele venceu, segregado por ano
golc_c_n2020 <- dados %>% group_by(golcasa) %>% filter(res == 'C' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_c_n2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'C' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

golc_c_2020 <- dados %>% group_by(golcasa) %>% filter(res == 'C' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_c_2020 <- dados %>% group_by(golvisitante) %>% filter(res =='C' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


# considerando a probabilidade do time da casa e que ele venceu, segregado por ano

golc_c_pc_n2020 <- dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > pv & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_c_pc_n2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > pv & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


golc_c_pc_2020 <- dados %>% group_by(golcasa) %>% filter(res == 'C' & pc > pv & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_c_pc_2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'C' & pc > pv & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))






#### TRABALHANDO COM PROBABILIDADES, TEMPORADA E GOLS DADO QUE O VISITANTE VENCEU ####

# analisando a quantidade de gols dados o resultado e probabilidades do visitante ele venceu
golc_v_pv <- dados %>% group_by(golcasa) %>% filter(res == 'V' & pv > pc) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_v_pv <- dados %>% group_by(golvisitante) %>% filter(res == 'V' & pv > pc) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


# desconsiderando a probabilidade, considerando temporada e gols do visitante e que ele venceu
golc_v_n2020 <- dados %>% group_by(golcasa) %>% filter(res == 'V' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_v_n2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'V' & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

golc_v_2020 <- dados %>% group_by(golcasa) %>% filter(res == 'V' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_v_2020 <- dados %>% group_by(golvisitante) %>% filter(res =='V' & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


# considerando a probabilidade do time visitante vencer e que ele venceu

golc_pv_v_n2020 <- dados %>% group_by(golcasa) %>% filter(res == 'V' & pv > pc & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_pv_v_n2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'V' & pv > pc & temporada != 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


golc_pv_v_2020 <- dados %>% group_by(golcasa) %>% filter(res == 'V', pv > pc & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
golv_pv_v_2020 <- dados %>% group_by(golvisitante) %>% filter(res == 'V', pv > pc & temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#### NOVOS DATAFRAMES ####

#criando o df para gols da casa

golcasa <- left_join(golc_c_2020, golc_c_n2020, by = 'golcasa') %>%
  right_join(., golc_v_2020,by = 'golcasa') %>%
  right_join(., golc_v_n2020, by = 'golcasa') %>%
  right_join(., golc_v_pv, by = 'golcasa') %>%
  right_join(., golc_pv_v_2020, by = 'golcasa') %>%
  right_join(., golc_pv_v_n2020, by = 'golcasa') %>%
  right_join(., golc_c_pc, by = 'golcasa') %>%
  right_join(., golc_c_pc_2020, by = 'golcasa') %>%
  right_join(., golc_c_pc_n2020, by = 'golcasa')

names(golcasa) <- c('gc', 'n_gc_c_2020','f_gc_c_2020',
                    'n_gc_c_n2020','f_gc_c_n2020',
                    'n_gc_v_2020','f_gc_v_2020',
                    'n_gc_v_n2020','f_gc_v_n2020',
                    'n_gc_pv_v', 'f_gc_pv_v',
                    'n_gc_pv_v_2020','f_gc_pv_v_2020',
                    'n_gc_pv_v_n2020','f_gc_pv_v_n2020',
                    'n_gc_c_pc','f_gc_c_pc',
                    'n_gc_c_pc_2020','f_gc_c_pc_2020',
                    'n_gc_c_pc_n2020','f_gc_c_pc_n2020')



#### TRABALHANDO COM AS ESTAÇÕES DO ANO, RESULTADOS DE JOGO DADA PROBABILIDADE OU NÃO ####

# Analisando o resultado no outono e inverno de 2014
frio_2014 <- dados %>% group_by(res) %>% filter(data > '2014-03-20' & data < '2014-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
calor_2014 <- dados %>% group_by(res) %>% filter(data > '2014-09-22' & data < '2015-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

clima_2014 <- left_join(calor_2014, frio_2014, by='res')
names(clima_2014) <- c('r','n_calor','f_calor','n_frio','f_frio')

# Analisando o resultado no frio e calor de 2016
frio_2016 <- dados %>% group_by(res) %>% filter(data > '2016-03-20' & data < '2016-09-22') %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
calor_2016 <- dados %>% group_by(res) %>% filter(data > '2016-09-22' & data < '2017-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

clima_2016 <- left_join(calor_2016, frio_2016, by='res')
names(clima_2016) <- c('r','n_calor','f_calor','n_frio','f_frio')

# Analisando o resultado no frio e calor de 2018
frio_2018 <- dados %>% group_by(res) %>% filter(data > '2018-03-20' & data < '2018-09-22' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
calor_2018 <- dados %>% group_by(res) %>% filter(data > '2018-09-22' & data < '2019-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

clima_2018 <- left_join(calor_2018, frio_2018, by='res')
names(clima_2018) <- c('r','n_calor','f_calor','n_frio','f_frio')

# Analisando o resultado no frio e calor de 2020
frio_2020 <- dados %>% group_by(res) %>% filter(data > '2020-03-20' & data < '2020-09-22' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
calor_2020 <- dados %>% group_by(res) %>% filter(data > '2020-09-22' & data < '2021-03-20' ) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

clima_2020 <- left_join(calor_2020, frio_2020, by='res')
names(clima_2020) <- c('r','n_calor','f_calor','n_frio','f_frio')

#unido os climas e anos
clima_geral <- as.tibble(left_join(clima_2014, clima_2016, by = 'r') %>%
                           left_join(., clima_2018, by = 'r') %>%
                           left_join(., clima_2020, by = 'r'))

names(clima_geral) <- c('r','n_calor_2014', 'f_calor_2014',
                        'n_frio_2014', 'f_frio_2014',
                        'n_calor_2016', 'f_calor_2016',
                        'n_frio_2016', 'f_frio_2016',
                        'n_calor_2018', 'f_calor_2018',
                        'n_frio_2018', 'f_frio_2018',
                        'n_calor_2020', 'f_calor_2020',
                        'n_frio_2020', 'f_frio_2020')


#### TRABALHANDO COM AS ESTAÇÕES DO ANO, ESTADOS, RESULTADOS DE JOGO DADA PROBABILIDADE OU NÃO ####
# Analisando o resultado no outono e inverno de 2014

estados_quentes <- c('Alagoas', 'Bahia', 
                     'Ceara', 'Pernambuco', 'Rio de Janeiro')

dados %>% group_by(res) %>% filter(data > '2014-03-20' & data < '2014-09-22' &
                                     !estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado na primavera e verão de 2014
dados %>% group_by(res) %>% filter(data > '2014-09-22' & data < '2015-03-20' &
                                     estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado no outono e inverno de 2016
dados %>% group_by(res) %>% filter(data > '2016-03-20' & data < '2016-09-22' &
                                     !estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado na primavera e verão de 2016
dados %>% group_by(res) %>% filter(data > '2016-09-22' & data < '2017-03-20' &
                                     estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado no outono e inverno de 2018
dados %>% group_by(res) %>% filter(data > '2018-03-20' & data < '2018-09-22' & 
                                     !estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado na primavera e verão de 2018
dados %>% group_by(res) %>% filter(data > '2018-09-22' & data < '2019-03-20' & 
                                     estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado no outono e inverno de 2020
dados %>% group_by(res) %>% filter(data > '2020-03-20' & data < '2020-09-22' & 
                                     !estado %in% estados_quentes) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# Analisando o resultado na primavera e verão de 2020
dados %>% group_by(res) %>% filter(data > '2020-09-22' & data < '2021-03-20' & 
                                     estado %in% estados_quentes) %>% 
                            summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))



#### TRABALHANDO COM A SOMA DE GOLS SEGREGADOS POR TEMPORADA ####

# 2012
gols_2012 <- dados %>% group_by(somagols) %>% filter(temporada == 2012) %>% 
              summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2013
gols_2013 <- dados %>% group_by(somagols) %>% filter(temporada == 2013) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2014
gols_2014 <- dados %>% group_by(somagols) %>% filter(temporada == 2014) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2015
gols_2015 <- dados %>% group_by(somagols) %>% filter(temporada == 2015) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2016
gols_2016 <- dados %>% group_by(somagols) %>% filter(temporada == 2016) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2017
gols_2017 <- dados %>% group_by(somagols)%>% filter(temporada == 2017) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2018
gols_2018 <- dados %>% group_by(somagols) %>% filter(temporada == 2018) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2019
gols_2019 <- dados %>% group_by(somagols) %>% filter(temporada == 2019) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# 2020
gols_2020 <- dados %>% group_by(somagols) %>% filter(temporada == 2020) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


#### TRABALHANDO COM A SOMA DE GOLS POR PERIODO ####
gols_estado_noite <- dados %>% group_by(somagols, estado) %>% 
           filter(periodo == 'Noite') %>%
           summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
gols_estado_tarde <- dados %>% group_by(somagols, estado) %>% 
           filter(periodo == 'Tarde') %>%
           summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


media_sg_estado_noite <- tapply(dados$somagols[dados$periodo == 'Noite'], 
                                dados$estado[dados$periodo == 'Noite'], mean)
media_sg_estado_tarde <- tapply(dados$somagols[dados$periodo == 'Tarde'], 
                                dados$estado[dados$periodo == 'Tarde'], mean)

#### TRABALHANDO COM A SOMA DE GOLS POR ESTADO POR ESTADO ####
estados <- c('Alagoas','Bahia','Ceara','Goias','Minas Gerais','Parana','Pernambuco','Rio de Janeiro','Rio Grande do Sul','Santa Catarina','Sao Paulo')


# Possibilidades por estado e por período

gols_estado_tarde <- dados %>% group_by(somagols, estado) %>% filter(periodo == 'Tarde') %>% 
  summarise(n = n())
gols_estado_noite <- dados %>% group_by(somagols, estado) %>% filter(periodo == 'Noite') %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#### PLOTS ####

dados %>% ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 10) + geom_density(alpha=.8) + 
  ggtitle('Distribuição da soma dos gols por período') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()


dados %>%
  ggplot(aes(periodo, estado, fill = somagols)) + 
  geom_tile() + 
  scale_fill_gradient(low='white', high='red') + 
  ggtitle('Distribuição da soma dos gols por período') +
  xlab('Periodo') + ylab('Estado') + labs(fill = 'Soma dos gols por temporada') + 
  theme_minimal() + ggeasy::easy_center_title() 