library(tidyverse)
library(janitor)
library(gridExtra)
library(viridis)
library(RColorBrewer)

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
#### RESULTADOS POR ANO ####

resultado_ano_casa <- dados %>% group_by(res, temporada, casa) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))
resultado_ano_visitante <- dados %>% group_by(res, temporada, visitante) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

# posição e dispersão vitorias casa

medidas_res_c <- as.tibble(resultado_ano_casa %>% filter(res == 'C') %>% 
                             summarise('media' = mean(n),
                                      'mediana' = median(n),
                                      'desvpad' = sd(n)))

medidas_res_v <- as.tibble(resultado_ano_casa %>% filter(res == 'V') %>% 
                             summarise('media' = mean(n),
                                       'mediana' = median(n),
                                       'desvpad' = sd(n)))

medidas_res_e <- as.tibble(resultado_ano_casa %>% filter(res == 'E') %>% 
                             summarise('media' = mean(n),
                                       'mediana' = median(n),
                                       'desvpad' = sd(n)))

medidas_res_c_casa <- as.tibble(resultado_ano_casa %>% group_by(casa) %>% filter(res == 'C') %>% 
                      summarise('media' = mean(n),
                                'mediana' = median(n),
                                'desvpad' = sd(n)))

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



#### TRABALHANDO COM AS ESTAÇÕES DO ANO, RESULTADOS DE JOGO ####

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


#### CLIMA NOS ESTADOS QUENTES ####
estados_quentes <- c('Alagoas','Bahia','Ceara','Pernambuco','Goias' ,'Rio de Janeiro')

climao_quente <- dados %>% group_by(res, data) %>% filter(estado %in% estados_quentes) %>%
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

medidas_frio_quentes_2014 <- as.tibble(climao_quente %>% 
                             filter(data > '2014-03-20' & data < '2014-09-22') %>% 
                             summarise('media' = mean(n),
                                       'mediana' = median(n),
                                       'desvpad' = sd(n)))
medidas_calor_quentes_2014 <- as.tibble(climao_quente %>% 
                                 filter(data > '2014-09-22' & data < '2015-03-20') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))

medidas_frio_quentes_2015 <- as.tibble(climao_quente %>% 
                                 filter(data > '2015-03-20' & data < '2015-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2015 <- as.tibble(climao_quente %>% 
                                  filter(data > '2015-09-22' & data < '2016-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))

medidas_frio_quentes_2016 <- as.tibble(climao_quente %>% 
                                 filter(data > '2016-03-20' & data < '2016-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2016 <- as.tibble(climao_quente %>% 
                                  filter(data > '2016-09-22' & data < '2017-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))
medidas_frio_quentes_2017 <- as.tibble(climao_quente %>% 
                                 filter(data > '2017-03-20' & data < '2017-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2017 <- as.tibble(climao_quente %>% 
                                  filter(data > '2017-09-22' & data < '2018-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))
medidas_frio_quentes_2018 <- as.tibble(climao_quente %>% 
                                 filter(data > '2018-03-20' & data < '2018-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2018 <- as.tibble(climao_quente %>% 
                                  filter(data > '2018-09-22' & data < '2019-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))
medidas_frio_quentes_2019 <- as.tibble(climao_quente %>% 
                                 filter(data > '2019-03-20' & data < '2019-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2019 <- as.tibble(climao_quente %>% 
                                  filter(data > '2019-09-22' & data < '2020-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))
medidas_frio_quentes_2020 <- as.tibble(climao_quente %>% 
                                 filter(data > '2020-03-20' & data < '2020-09-22') %>% 
                                 summarise('media' = mean(n),
                                           'mediana' = median(n),
                                           'desvpad' = sd(n)))
medidas_calor_quentes_2020 <- as.tibble(climao_quente %>% 
                                  filter(data > '2020-09-22' & data < '2021-03-20') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))



#### CLIMA NOS ESTADOS FRIOS ####
climao_gelado <- dados %>% group_by(res, data) %>% filter(!estado %in% estados_quentes) %>%
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

medidas_frio_gelado_2014 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2014-03-20' & data < '2014-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2014 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2014-09-22' & data < '2015-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))

medidas_frio_gelado_2015 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2015-03-20' & data < '2015-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2015 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2015-09-22' & data < '2016-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))

medidas_frio_gelado_2016 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2016-03-20' & data < '2016-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2016 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2016-09-22' & data < '2017-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))
medidas_frio_gelado_2017 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2017-03-20' & data < '2017-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2017 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2017-09-22' & data < '2018-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))
medidas_frio_gelado_2018 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2018-03-20' & data < '2018-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2018 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2018-09-22' & data < '2019-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))
medidas_frio_gelado_2019 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2019-03-20' & data < '2019-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2019 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2019-09-22' & data < '2020-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))
medidas_frio_gelado_2020 <- as.tibble(climao_gelado %>% 
                                         filter(data > '2020-03-20' & data < '2020-09-22') %>% 
                                         summarise('media' = mean(n),
                                                   'mediana' = median(n),
                                                   'desvpad' = sd(n)))
medidas_calor_gelado_2020 <- as.tibble(climao_gelado %>% 
                                          filter(data > '2020-09-22' & data < '2021-03-20') %>% 
                                          summarise('media' = mean(n),
                                                    'mediana' = median(n),
                                                    'desvpad' = sd(n)))


#### TRABALHANDO COM A SOMA DE GOLS POR PERIODO ####

somagols_estado_periodo_ano <- dados %>% group_by(somagols, estado, temporada, periodo) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))


medidas_gols_tarde <- as.tibble(somagols_estado_periodo_ano %>% group_by(estado, temporada) %>%
                                  filter(periodo == 'Tarde') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))

medidas_gols_noite <- as.tibble(somagols_estado_periodo_ano %>% group_by(estado, temporada) %>%
                                  filter(periodo == 'Noite') %>% 
                                  summarise('media' = mean(n),
                                            'mediana' = median(n),
                                            'desvpad' = sd(n)))



#### TRABALHANDO COM A SOMA DE GOLS POR ESTADO POR ESTADO ####
estados <- c('Alagoas','Bahia','Ceara','Goias','Minas Gerais','Parana','Pernambuco','Rio de Janeiro','Rio Grande do Sul','Santa Catarina','Sao Paulo')


# Possibilidades por estado e por período

gols_estado_tarde <- dados %>% group_by(somagols, estado) %>% filter(periodo == 'Tarde') %>% 
  summarise(n = n())
gols_estado_noite <- dados %>% group_by(somagols, estado) %>% filter(periodo == 'Noite') %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#### PLOTS ####

resultado_ano_casa %>% filter(res == 'C') %>% ggplot(aes(n, y = stat(density))) + 
  geom_histogram(bins = 8, fill = 'springgreen3') + 
  geom_density(alpha=.5, fill = 'steelblue2') +
  ggtitle('Distribuição do número de vitórias do time da casa') +
  xlab('Número de vitórias') + ylab('Frequência') + 
  theme_minimal() + ggeasy::easy_center_title()

######################################

media_gol_tarde <- medidas_gols_tarde %>%  mutate(estado = fct_reorder(estado, media)) %>% 
  ggplot(aes(media, estado, colour=temporada)) + geom_point() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Media de gols a tarde por estado') + 
  xlab('Media de gols') + ylab('Estado') +
  theme_minimal() + ggeasy::easy_center_title()

media_gol_noite <- medidas_gols_noite %>% mutate(estado = fct_reorder(estado, media)) %>%
  ggplot(aes(media, estado, colour=temporada)) + geom_point() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Media de gols a noite por estado') + 
  xlab('Media de gols') + ylab('Estado') +
  theme_minimal() + ggeasy::easy_center_title()

###############################################################

dist_sg_al <- somagols_estado_periodo_ano %>% filter(estado == 'Alagoas') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período em Alagoas') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_ba <- somagols_estado_periodo_ano %>% filter(estado == 'Bahia') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) +
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período na Bahia') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_ce <- somagols_estado_periodo_ano %>% filter(estado == 'Ceara') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período no Ceara') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_go <- somagols_estado_periodo_ano %>% filter(estado == 'Goias') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período em Goias') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_mg <- somagols_estado_periodo_ano %>% filter(estado == 'Minas Gerais') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período em Minas Gerais') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_pr <- somagols_estado_periodo_ano %>% filter(estado == 'Parana') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período no Parana') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_pb <- somagols_estado_periodo_ano %>% filter(estado == 'Pernambuco') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período no Pernambuco') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_rs <- somagols_estado_periodo_ano %>% filter(estado == 'Rio Grande do Sul') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período no Rio Grande do Sul') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_rj <- somagols_estado_periodo_ano %>% filter(estado == 'Rio de Janeiro') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período no Rio de Janeiro') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_sc <- somagols_estado_periodo_ano %>% filter(estado == 'Santa Catarina') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período em Santa Catarina') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title()

dist_sg_sp <- somagols_estado_periodo_ano %>% filter(estado == 'Sao Paulo') %>%
  ggplot(aes(somagols, y = stat(density), fill = periodo)) + 
  geom_histogram(color = 'black', bins = 8) + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição da soma dos gols por período em São Paulo') +
  xlab('Soma dos gols por jogos') + ylab('Densidade') +
  theme_minimal() + ggeasy::easy_center_title() 

############################################################

dist_frio_gelados2014 <- climao_gelado %>% 
  filter(data > '2014-03-20' & data < '2014-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2014') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2014 <- climao_gelado %>% 
  filter(data > '2014-09-22' & data < '2015-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2014') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_gelados2015 <- climao_gelado %>% 
  filter(data > '2015-03-20' & data < '2015-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2015') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2015 <- climao_gelado %>% 
  filter(data > '2015-09-22' & data < '2016-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2015') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_gelados2016 <- climao_gelado %>% 
  filter(data > '2016-03-20' & data < '2016-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2016') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2016 <- climao_gelado %>% 
  filter(data > '2016-09-22' & data < '2016-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2016') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_gelados2017 <- climao_gelado %>% 
  filter(data > '2017-03-20' & data < '2017-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2017') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2017 <- climao_gelado %>% 
  filter(data > '2017-09-22' & data < '2018-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2017') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_gelados2018 <- climao_gelado %>% 
  filter(data > '2018-03-20' & data < '2018-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2018') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2018 <- climao_gelado %>% 
  filter(data > '2018-09-22' & data < '2019-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2018') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title() 


dist_frio_gelados2019 <- climao_gelado %>% 
  filter(data > '2019-03-20' & data < '2019-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2019') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2019 <- climao_gelado %>% 
  filter(data > '2019-09-22' & data < '2020-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2019') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_gelados2020 <- climao_gelado %>% 
  filter(data > '2020-03-20' & data < '2020-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no inverno de 2020') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_gelados2020 <- climao_gelado %>% 
  filter(data > '2020-09-22' & data < '2021-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados frios no verão de 2020') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

######
dist_frio_quentes2014 <- climao_quente %>% 
  filter(data > '2014-03-20' & data < '2014-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2014') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2014 <- climao_quente %>% 
  filter(data > '2014-09-22' & data < '2015-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2014') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2015 <- climao_quente %>% 
  filter(data > '2015-03-20' & data < '2015-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2015') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2015 <- climao_quente %>% 
  filter(data > '2015-09-22' & data < '2016-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2015') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2016 <- climao_quente %>% 
  filter(data > '2016-03-20' & data < '2016-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2016') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2016 <- climao_quente %>% 
  filter(data > '2016-09-22' & data < '2016-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2016') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2017 <- climao_quente %>% 
  filter(data > '2017-03-20' & data < '2017-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2017') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2017 <- climao_quente %>% 
  filter(data > '2017-09-22' & data < '2018-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2017') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2018 <- climao_quente %>% 
  filter(data > '2018-03-20' & data < '2018-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2018') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2018 <- climao_quente %>% 
  filter(data > '2018-09-22' & data < '2019-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2018') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2019 <- climao_quente %>% 
  filter(data > '2019-03-20' & data < '2019-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2019') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()

dist_calor_quentes2019 <- climao_quente %>% 
  filter(data > '2019-09-22' & data < '2020-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2019') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title()


dist_frio_quentes2020 <- climao_quente %>% 
  filter(data > '2020-03-20' & data < '2020-09-22') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no inverno de 2020') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title() 

dist_calor_quentes2020 <- climao_quente %>% 
  filter(data > '2020-09-22' & data < '2021-03-20') %>%
  ggplot(aes(n, fill = res)) + 
  geom_bar(alpha = .6,color = 'black') + 
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Distribuição de resultado nos estados quentes no verão de 2020') +
  xlab('Quantidade de resultados') + ylab('Ocorrências') +
  theme_minimal() + ggeasy::easy_center_title() 


somagols_estado_periodo_ano %>%
  ggplot(aes(periodo, estado, fill = somagols)) + 
  geom_tile() + 
  scale_fill_gradient(low='white', high='red') + 
  ggtitle('Distribuição da soma dos gols por período') +
  xlab('Periodo') + ylab('Estado') + labs(fill = 'Soma dos gols por temporada') + 
  theme_minimal() + ggeasy::easy_center_title() 



dist_res2014 <- grid.arrange(arrangeGrob(dist_calor_gelados2014, dist_calor_quentes2014,
                         dist_frio_gelados2014, dist_frio_quentes2014), 
                         top = 'Distribuição dos resultados em 2014')
dist_res2015 <- grid.arrange(arrangeGrob(dist_calor_gelados2015, dist_calor_quentes2015,
                         dist_frio_gelados2015, dist_frio_quentes2015), 
                         top = 'Distribuição dos resultados em 2015')
dist_res2016 <- grid.arrange(arrangeGrob(dist_calor_gelados2016, dist_calor_quentes2016,
                         dist_frio_gelados2016, dist_frio_quentes2016), 
                         top = 'Distribuição dos resultados em 2016')
dist_res2017 <- grid.arrange(arrangeGrob(dist_calor_gelados2017, dist_calor_quentes2017,
                         dist_frio_gelados2017, dist_frio_quentes2017), 
                         top = 'Distribuição dos resultados em 2017')
dist_res2018 <- grid.arrange(arrangeGrob(dist_calor_gelados2018, dist_calor_quentes2018,
                         dist_frio_gelados2018, dist_frio_quentes2018), 
                         top = 'Distribuição dos resultados em 2018')
dist_res2019 <- grid.arrange(arrangeGrob(dist_calor_gelados2019, dist_frio_quentes2019,
                         dist_frio_gelados2019, dist_frio_quentes2019), 
                         top = 'Distribuição dos resultados em 2019')
dist_res2020 <- grid.arrange(arrangeGrob(dist_calor_gelados2020, dist_calor_quentes2020,
                         dist_frio_gelados2020, dist_frio_quentes2020), 
                         top = 'Distribuição dos resultados em 2020')


dist_sg1 <- grid.arrange(arrangeGrob(dist_sg_al, dist_sg_ba, dist_sg_ce, 
                         dist_sg_go,dist_sg_mg, dist_sg_pb), 
                         top = 'Distribuição da quantidade total de gols por estado e período (1)')

dist_sg2 <- grid.arrange(arrangeGrob(dist_sg_pr, dist_sg_rj, dist_sg_rs,
                         dist_sg_sc, dist_sg_sp, 
                         top = 'Distribuição da quantidade total de gols por estado e período (2)'))

dist_mgolsper <- grid.arrange(arrangeGrob(media_gol_noite, media_gol_tarde), 
                              top = 'Distribuição da quantidade de gols por estado e período')