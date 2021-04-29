#' ---
#' title: "APS Estatística I - Analisando os times do Brasil"
#' author: Victor Alves
#' date: 03/09/2021
#' output: pdf_document
#' ---
#' 
#' # Importando os dados
## --------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(gridExtra)
library(viridis)
library(RColorBrewer)
theme_set(theme_minimal())

dados <- readxl::read_excel("C:/Users/Victor/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/Estat/aps_stat1/APS1/BRA2.xlsx", 
                            col_types = c("text", "text", "numeric", 
                                          "date", "text", "text", "text", "numeric", 
                                          "numeric", "numeric", "text", "numeric", 
                                          "numeric", "numeric")) %>% clean_names()


#' 
#' 
#' # Manipulando dados
#' 
#' ## Considerando a contagem de resultados por time e temporadas.
#' 
#' Contagem de cada resultado por temporada
## --------------------------------------------------------------------------
resultado_ano <- dados %>% group_by(temporada, res) %>%  
  summarise(n = n())

#' 
#' Computando a quantidade de resultados do time mandante
## --------------------------------------------------------------------------
resultado_ano_casa <- dados %>% group_by(res, temporada, casa) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Computando a quantidade de resultados do time visitante
## --------------------------------------------------------------------------
resultado_ano_visitante <- dados %>% group_by(res, temporada, visitante) %>% 
  summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Calculando algumas medidas de posição e dispersão da contagem dos resultados por time e por ano
## --------------------------------------------------------------------------
medidas_res <- as_tibble(resultado_ano_casa %>% group_by(res, temporada) %>%
                             summarise('media' = round(mean(n),2),
                                       'mediana' = round(median(n),2),
                                       'desvpad' = round(sd(n),2)))


medidas_res_casa <- as_tibble(resultado_ano_casa %>% group_by(casa, res, temporada) %>% 
                      summarise('media' = round(mean(n),2)))


#' 
#' ## Trabalhando com os climas e estações do ano
#' 
#' Criando vetores com as estações de cada ano
## --------------------------------------------------------------------------
inverno_2012 <- c(dados %>% select(data) %>% filter(data > '2012-03-20' & data < '2012-09-22'))
verao_2012 <- dados %>% select(data) %>%  filter(data > '2012-09-22' & data < '2013-03-20')

inverno_2013 <- dados %>% select(data) %>% filter(data > '2013-03-20' & data < '2013-09-22')
verao_2013 <- dados %>% select(data) %>%  filter(data > '2013-09-22' & data < '2014-03-20')

inverno_2014 <- dados %>% select(data) %>% filter(data > '2014-03-20' & data < '2014-09-22')
verao_2014 <- dados %>% select(data) %>%  filter(data > '2014-09-22' & data < '2015-03-20')

inverno_2015 <- dados %>% select(data) %>% filter(data > '2015-03-20' & data < '2015-09-22')
verao_2015 <- dados %>% select(data) %>%  filter(data > '2015-09-22' & data < '2016-03-20')

inverno_2016 <- dados %>% select(data) %>% filter(data > '2016-03-20' & data < '2016-09-22')
verao_2016 <- dados %>% select(data) %>%  filter(data > '2016-09-22' & data < '2017-03-20')

inverno_2017 <- dados %>% select(data) %>% filter(data > '2017-03-20' & data < '2017-09-22')
verao_2017 <- dados %>% select(data) %>%  filter(data > '2017-09-22' & data < '2018-03-20')

inverno_2018 <- dados %>% select(data) %>% filter(data > '2018-03-20' & data < '2018-09-22')
verao_2018 <- dados %>% select(data) %>%  filter(data > '2018-09-22' & data < '2019-03-20')

inverno_2019 <- dados %>% select(data) %>% filter(data > '2019-03-20' & data < '2019-09-22')
verao_2019 <- dados %>% select(data) %>%  filter(data > '2019-09-22' & data < '2020-03-20')

inverno_2020 <- dados %>% select(data) %>% filter(data > '2020-03-20' & data < '2020-09-22')
verao_2020 <- dados %>% select(data) %>%  filter(data > '2020-09-22' & data < '2021-03-20')

#' 
#' Dataset contendo a contagem de resultados por data a cada período do dia nos estados mais quentes
## --------------------------------------------------------------------------
climao_quente <- dados %>% group_by(res, data, periodo) %>%
  summarise(n = n())

#' 
#' 
#' Quantidade de resultados a cada estação por ano de cada estado
## --------------------------------------------------------------------------
contagem_frio_quentes_2014 <- as_tibble(dados %>% filter(data %in% inverno_2014$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2014 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2014$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2015 <- as_tibble(dados %>% 
                                       filter(data %in% inverno_2015$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2015 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2015$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2016 <- as_tibble(dados %>% 
                                       filter(data %in% inverno_2016$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2016 <- as_tibble(dados %>% 
                                  filter(data %in% verao_2016$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2017 <- as_tibble(dados %>% 
                                 filter(data %in% inverno_2017$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2017 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2017$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2018 <- as_tibble(dados %>% 
                                       filter(data %in% inverno_2018$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2018 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2018$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2019 <- as_tibble(dados %>% 
                                       filter(data %in% inverno_2019$data)%>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2019 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2019$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_frio_quentes_2020 <- as_tibble(dados %>% 
                                       filter(data %in% inverno_2020$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))

contagem_calor_quentes_2020 <- as_tibble(dados %>% 
                                       filter(data %in% verao_2020$data) %>%  
                                       group_by(res, periodo) %>% 
                                       summarise(n = n(), freq_rel = n/sum(n)))


#' 
#' 
#' Medidas da soma de gols em cada estado (desconsiderando qual mais quente) a cada ano por estação do ano
## --------------------------------------------------------------------------
medidas_sg_inverno_2012 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2012$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2012 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2012$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))


medidas_sg_inverno_2013 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2013$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2013 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2013$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))



medidas_sg_inverno_2014 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2014$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2014 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2014$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2015 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2015$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2015 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2015$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2016 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2016$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2016 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2016$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2017 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2017$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2017 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2017$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2018 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2018$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2018 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2018$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2019 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2019$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2019 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2019$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_inverno_2020 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                         filter(data %in% inverno_2020$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))

medidas_sg_verao_2020 <- as_tibble(dados %>% group_by(estado, periodo) %>%
                                          filter(data %in% verao_2020$data) %>% 
                                         summarise('media' = round(mean(total),2),
                                                   'mediana' = round(median(total),2),
                                                   'desvpad' = round(sd(total),2),
                                                    'cv' = round(sd(total)/mean(total),2)*100,
                                                    'max' = max(total),
                                                    'min' = min(total),
                                                    'amp' = max - min,
                                                    'IQR' = IQR(total)))


#' 
#' 
#' ## Trabalhando com a soma dos gols por período do dia
#' 
#' 
#' Dataset contendo a soma dos gols por estado e por período
## --------------------------------------------------------------------------
total_estado_periodo <- dados %>% group_by(total, estado, periodo)

#' 
#' Medidas das soma de gols por período e por estado
## --------------------------------------------------------------------------
medidas_gols_estado_periodo <- as_tibble(total_estado_periodo %>% group_by(estado, periodo) %>% 
                                  summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

#' Medidas da soma de gols por estado
## --------------------------------------------------------------------------
medidas_gols_estado <- as_tibble(total_estado_periodo %>% group_by(estado) %>% 
                                  summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

#' 
#' 
#' Medidas da soma de gols apenas por estacao
## --------------------------------------------------------------------------
medidas_sg_frio <- as_tibble(dados %>% 
                                    group_by(temporada) %>%
                                    filter(data %in% inverno_2012$data | 
                                             data %in% inverno_2013$data |
                                             data %in% inverno_2014$data | 
                                             data %in% inverno_2015$data |
                                             data %in% inverno_2016$data |
                                             data %in% inverno_2017$data |
                                             data %in% inverno_2018$data |
                                             data %in% inverno_2019$data |
                                             data %in% inverno_2020$data) %>% 
                                    summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

medidas_sg_calor <- as_tibble(dados %>% 
                                    group_by(temporada) %>%
                                    filter(data %in% verao_2012$data | 
                                             data %in% verao_2013$data |
                                             data %in% verao_2014$data |
                                             data %in% verao_2015$data |
                                             data %in% verao_2016$data | 
                                             data %in% verao_2017$data |
                                             data %in% verao_2018$data |
                                             data %in% verao_2019$data |
                                             data %in% verao_2020$data) %>% 
                                    summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

#' 
#' 
#' Medidas da soma de gols apenas por estacao e por periodo
#' 
## --------------------------------------------------------------------------
medidas_sg_frio_per <- as_tibble(dados %>% 
                                    group_by(temporada, periodo) %>%
                                    filter(data %in% inverno_2012$data | 
                                             data %in% inverno_2013$data |
                                             data %in% inverno_2014$data | 
                                             data %in% inverno_2015$data |
                                             data %in% inverno_2016$data |
                                             data %in% inverno_2017$data |
                                             data %in% inverno_2018$data |
                                             data %in% inverno_2019$data |
                                             data %in% inverno_2020$data) %>% 
                                    summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

medidas_sg_calor_per <- as_tibble(dados %>% 
                                    group_by(temporada, periodo) %>%
                                    filter(data %in% verao_2012$data | 
                                             data %in% verao_2013$data |
                                             data %in% verao_2014$data |
                                             data %in% verao_2015$data |
                                             data %in% verao_2016$data | 
                                             data %in% verao_2017$data |
                                             data %in% verao_2018$data |
                                             data %in% verao_2019$data |
                                             data %in% verao_2020$data) %>% 
                                    summarise('media' = round(mean(total),2),
                                            'mediana' = round(median(total),2),
                                            'desvpad' = round(sd(total),2),
                                            'cv' = round(sd(total)/mean(total),2)*100,
                                            'max' = max(total),
                                            'min' = min(total),
                                            'amp' = max - min,
                                            'IQR' = IQR(total)))

#' 
#' 
#' 
#' ## Trabalhando com os gols da casa quando ele ganha
#' 
#' Calculando medidas dos gols da casa e visitante, por ano
## --------------------------------------------------------------------------
medidas_gols_ano <- as_tibble(dados %>% group_by(temporada)%>% 
                                  summarise('media casa' = round(mean(golcasa),2),
                                            'mediana casa' = median(golcasa),
                                            'desvpad casa' = sd(golcasa),
                                            'cv casa' = round(sd(golcasa)/mean(golcasa),2)*100,
                                            'media visitante' = round(mean(golvisitante),2),
                                            'mediana visitante' = median(golvisitante),
                                            'desvpad visitante' = sd(golvisitante),
                                            'cv visitante' = round(sd(golvisitante)/mean(golvisitante),2)*100))



#' 
#' ## Soma de gols por estado
## --------------------------------------------------------------------------

soma_estado_inverno2012 <- dados %>% filter(data %in% inverno_2012$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)
soma_estado_verao2012 <- dados %>% filter(data %in% verao_2012$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)

soma_estado_inverno2014 <- dados %>% filter(data %in% inverno_2014$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)
soma_estado_verao2014 <- dados %>% filter(data %in% verao_2014$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)

soma_estado_inverno2016 <- dados %>% filter(data %in% inverno_2016$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)
soma_estado_verao2016 <- dados %>% filter(data %in% verao_2016$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)

soma_estado_inverno2018 <- dados %>% filter(data %in% inverno_2018$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)
soma_estado_verao2018 <- dados %>% filter(data %in% verao_2018$data) %>% group_by(estado) %>%
                            mutate(somaestado = sum(total)) %>% select(estado,temporada, somaestado)


#' 
#' 
#' 
#' ## Gerando frequências
#' 
#' Obtendo a frequência de jogos por período do dia
## --------------------------------------------------------------------------
dados %>% group_by(periodo) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Obtendo a frequência de jogos por estado
## --------------------------------------------------------------------------
dados %>% group_by(estado) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Obtendo a frequência de jogos por temporada por período
## --------------------------------------------------------------------------
dados %>% group_by(periodo, temporada) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' Obtendo a frequência de jogos por temporada por estado
## --------------------------------------------------------------------------
dados %>% group_by(periodo, estado) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Obtendo a frequência de gols totais por estado
## --------------------------------------------------------------------------
dados %>% group_by(total) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' Obtendo a frequência de gols totais por estado
## --------------------------------------------------------------------------
dados %>% group_by(total, estado) %>% summarise(n = n()) %>% mutate(freq = round((n / sum(n))*100,2))

#' 
#' 
#' # Plots
#' ## Plot dos resultados ao longo dos anos
#' 
#' Distribuição da quantidade dos resultados a cada temporada
## --------------------------------------------------------------------------
dist_res_2020 <- resultado_ano %>% 
    group_by(temporada) %>%
    ggplot(aes(temporada, n, colour = res)) + geom_point() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle('Quantidade de resultados por temporada') + 
    xlab('Temporada') + ylab('Quantidade') + labs(col = 'Resultado') + 
    ggeasy::easy_center_title()

#' 
#' Distribuição da quantidade média de resultados a cada temporada
## --------------------------------------------------------------------------
dist_mu_res <- medidas_res %>% ggplot(aes(temporada, media, colour = res)) + 
  geom_point() + 
  scale_colour_viridis(discrete = TRUE) + ylim(3,11) +
  ggtitle('Quantidade média de resultados por temporada') + 
  xlab('Temporada') + ylab('Média') + labs(col = 'Resultado') +
  ggeasy::easy_center_title()

#' 
#' Distribuição da quantidade mediana de resultados a cada temporada
## --------------------------------------------------------------------------
dist_me_res <- medidas_res %>% ggplot(aes(temporada, mediana, colour = res)) + 
  geom_point() +
  scale_colour_viridis(discrete = TRUE) + ylim(3,11) +
  ggtitle('Quantidade mediana de resultados por temporada') + 
  xlab('Temporada') + ylab('Mediana') + labs(col = 'Resultado') +
  ggeasy::easy_center_title()

#' 
#' Unindo as distribuições da quantidade de resultado
## --------------------------------------------------------------------------
grid.arrange(arrangeGrob(dist_res_2020, dist_mu_res, dist_me_res), 
                         top = 'Distribuição da quantidade de resultados por temporada')

#' 
#' 
#' Distribuição da quantidade de resultados por período
## --------------------------------------------------------------------------
dist_resp <- dados %>% 
    ggplot(aes(res, fill = periodo)) + geom_bar(alpha = 0.8, color = 'black', position = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle('Resultado por período do dia') + 
    xlab('Resultado') + ylab('Frequência') + labs(fill = 'Período') + coord_flip() + 
    ggeasy::easy_center_title() 

#' 
#' Distribuição da quantidade de vitórias da casa por temporada
## --------------------------------------------------------------------------
dist_c_casa_ano <- resultado_ano_casa %>% filter(res == 'C') %>%
  mutate(casa = fct_reorder(casa, n)) %>%
  group_by(casa) %>%
  ggplot(aes(temporada, casa, colour = n)) + geom_point() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Quantidade de vitórias da casa por ano') + 
  xlab('Temporada') + ylab('Time') + labs(col = 'Quantidade') + 
   ggeasy::easy_center_title()

#' 
#' Distribuição da quantidade de derrotas da casa por temporada
## --------------------------------------------------------------------------
dist_v_casa_ano <- resultado_ano_casa %>% filter(res == 'V') %>%
  mutate(casa = fct_reorder(casa, n)) %>%
  group_by(casa) %>%
  ggplot(aes(temporada, casa, colour = n)) + geom_point() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Quantidade de derrotas da casa por ano') + 
  xlab('Temporada') + ylab('Time') + labs(col = 'Quantidade') + 
   ggeasy::easy_center_title()

#' 
#' Unindo a distribuição das vitórias e derrotas do time mandante
## --------------------------------------------------------------------------
grid.arrange(arrangeGrob(dist_c_casa_ano, dist_v_casa_ano), 
                         top = 'Distribuição da quantidade de resultados do time mandante')

#' 
#' Distribuição de jogos por estado
## --------------------------------------------------------------------------
dados %>% group_by(estado) %>% summarise(n = n()) %>%
  mutate(freq_rel = n/sum(n)) %>%
  ggplot(aes(estado, freq_rel)) + 
  geom_col(colour = 'black', fill = 'chartreuse2') +  
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Distribuição das partidas por estado') + 
  xlab('Estado') + ylab('Frequência relativa') + coord_flip() +
   ggeasy::easy_center_title() 

#' 
#' 
#' ## Plot de gols
#' 
#' Distribuição dos gols da casa e dos gols do mandante
## --------------------------------------------------------------------------
dist_gc_gv <- dados %>% 
  ggplot(aes(temporada, golcasa)) + geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle('Média de gols a tarde por estado') + 
  xlab('Média de gols') + ylab('Estado') +
   ggeasy::easy_center_title()

#' 
#' Distribuição média de gols por período
#' 
## --------------------------------------------------------------------------
dados %>% ggplot(aes(total,stat(density) ,fill = periodo)) + 
  geom_histogram(alpha = 0.8, colour = 'black', 
                 position = 'identity', breaks = seq(0,8)) +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Distribuição da soma de gols por período do dia') + 
  xlab('Total de gols por partida') + ylab('Frequência relativa') + 
  labs(fill = 'Período') + ggeasy::easy_center_title()

#' 
## --------------------------------------------------------------------------
dados %>% ggplot(aes(total,stat(density) ,fill = periodo)) + 
  geom_histogram(colour = 'black', 
                 position = 'dodge', breaks = seq(0,8)) +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Distribuição da soma de gols por período do dia') + 
  xlab('Total de gols por partida') + ylab('Frequência relativa') + 
  labs(fill = 'Período') + ggeasy::easy_center_title()

#' 
#' 
#' Distribuição média de gols (como fator) por período
#' 
## --------------------------------------------------------------------------
dados %>% group_by(periodo, total) %>% summarise(n = n()) %>%
  mutate(freq_rel = n/sum(n)) %>% 
  ggplot(aes(as.factor(total), freq_rel, fill = periodo)) + 
  geom_col(colour = 'black', position = 'dodge') +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Distribuição da soma de gols por período do dia') + 
  xlab('Total de gols por partida') + ylab('Frequência relativa') + 
  labs(fill = 'Período') +
  ggeasy::easy_center_title()

#' 
#' 
#' Distribuição da média de gols por estado e período
## --------------------------------------------------------------------------
dist_med_gols_periodo <- medidas_gols_estado_periodo %>% 
  mutate(estado = fct_reorder(estado, media)) %>%
  group_by(estado) %>%
  ggplot(aes(media, estado, colour = periodo)) + geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle('Média de gols a tarde por estado') + 
  xlab('Média de gols') + ylab('Estado') +
   ggeasy::easy_center_title()

#' 
#' Distribuição da média de gols por estado
## --------------------------------------------------------------------------
dist_med_gols_estado <- medidas_gols_estado %>% 
  mutate(estado = fct_reorder(estado, media)) %>%
  ggplot(aes(media, estado, colour = media)) + geom_point() +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Media de gols da casa por estado') + 
  xlab('Ano') + ylab('Media de gols') + 
  theme(legend.position="none")
   ggeasy::easy_center_title()

#' 
#' 
#' ## Plots medias gol por estado e média gol por temporada
#' 
#' Distribuição dos gols do mandante por ano
## --------------------------------------------------------------------------
medias_gols_casa_ano <- medidas_gols_ano %>%
  ggplot(aes(temporada, `media casa`, colour = `media casa`)) + geom_point() + 
  ylim(0.79,1.5) +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Media de gols da casa por ano') + 
  xlab('Ano') + ylab('Media de gols') +
  theme(legend.position="none")
   ggeasy::easy_center_title()


#' 
#' Distribuição dos gols do visitante por ano
## --------------------------------------------------------------------------
medias_gols_visitante_ano <- medidas_gols_ano %>%
  ggplot(aes(temporada, `media visitante`, colour = `media visitante`)) + geom_point() +
  ylim(0.79,1.5) +
  scale_color_viridis(discrete = FALSE) +
  ggtitle('Media de gols do visitante por ano') + 
  xlab('Ano') + ylab('Media de gols') +
  theme(legend.position="none")
   ggeasy::easy_center_title()

#' 
#' Unindo os plots da média de gols da casa e do visitante
## --------------------------------------------------------------------------
grid.arrange(arrangeGrob(medias_gols_casa_ano, medias_gols_visitante_ano), 
                         top = 'Distribuição da média de gols da casa e do visitante por temporada')

#' 
#' Distribuição da soma de gols por estado
## --------------------------------------------------------------------------
dist_sg_al <- dados %>% filter(estado == 'Alagoas') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
     xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período em Alagoas') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_ba <- dados %>% filter(estado == 'Bahia') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período na Bahia') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_ce <- dados %>% filter(estado == 'Ceara') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período no Ceara') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_go <- dados %>% filter(estado == 'Goias') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período em Goias') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_mg <- dados %>% filter(estado == 'Minas Gerais') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período em Minas Gerais') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_pr <- dados %>% filter(estado == 'Parana') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período no Parana') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_pb <- dados %>% filter(estado == 'Pernambuco') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período no Pernambuco') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_rs <- dados %>% filter(estado == 'Rio Grande do Sul') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período no Rio Grande do Sul') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_rj <- dados %>% filter(estado == 'Rio de Janeiro') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período no Rio de Janeiro') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_sc <- dados %>% filter(estado == 'Santa Catarina') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período em Santa Catarina') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title()

dist_sg_sp <- dados %>% filter(estado == 'Sao Paulo') %>%
  ggplot(aes(total, y = stat(density), fill = periodo)) + 
  geom_histogram(alpha = .8 ,color = 'black', 
                 breaks = seq(0,9, 1), position = 'identity') + 
  xlim(0,9) + ylim(0,0.45) + scale_fill_viridis(discrete = TRUE) +
  ggtitle('Distribuição da soma dos gols por período em São Paulo') +
  xlab('Soma dos gols por jogos') + ylab('Frequência relativa') + labs(fill = 'Período') +
  ggeasy::easy_center_title() 


#' 
#' Unindo os plots da soma de gols por estado
## --------------------------------------------------------------------------
dist_sg1 <- grid.arrange(arrangeGrob(dist_sg_al, dist_sg_ba, dist_sg_ce, 
                         dist_sg_go,dist_sg_mg, dist_sg_pb), 
                         top = 'Distribuição da quantidade total de gols por estado e período (1)')

dist_sg2 <- grid.arrange(arrangeGrob(dist_sg_pr, dist_sg_rj, dist_sg_rs,
                         dist_sg_sc, dist_sg_sp, 
                         top = 'Distribuição da quantidade total de gols por estado e período (2)'))

#' 
#' 
#' ## Plots dos resultados por estação do ano
#' 
#' Distribuição da quantidade de resultados nos estados por estação
## --------------------------------------------------------------------------
dist_frio_quentes2012 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% inverno_2012$data) %>%
  ggplot(aes(res, fill = res)) +
  ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2012') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2012 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% verao_2012$data) %>%
  ggplot(aes(res, fill = res)) + 
  ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2012') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2013 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% inverno_2013$data) %>%
  ggplot(aes(res, fill = res)) + 
  ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2013') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2013 <- dados %>% group_by(res, data, periodo) %>%  
  filter(data %in% verao_2013$data) %>%
  ggplot(aes(res, fill = res)) + 
  ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2013') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()



dist_frio_quentes2014 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% inverno_2014$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2014') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2014 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% verao_2014$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2014') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2015 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% inverno_2015$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') + 
  ggtitle('Distribuição de resultado nos dias mais frios de 2015') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2015 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% verao_2015$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2015') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2016 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% inverno_2016$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2016') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2016 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% verao_2016$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2016') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2017 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% inverno_2017$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2017') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2017 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% verao_2017$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2017') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2018 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% inverno_2018$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2018') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2018 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% verao_2018$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2018') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2019 <- dados %>% group_by(res, data, periodo) %>%
  filter(data %in% inverno_2019$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2019') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()

dist_calor_quentes2019 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% verao_2019$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2019') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title()


dist_frio_quentes2020 <- dados %>% group_by(res, data, periodo) %>% 
  filter(data %in% inverno_2020$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Blues') +
  ggtitle('Distribuição de resultado nos dias mais frios de 2020') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title() 

dist_calor_quentes2020 <- dados %>% group_by(res, data, periodo) %>%  
  filter(data %in% verao_2020$data) %>%
  ggplot(aes(res, fill = res)) +    ylim(0,150) +
  geom_bar(color = 'black') + 
  scale_fill_brewer(palette='Reds') +
  ggtitle('Distribuição de resultado nos dias mais quentes  de 2020') +
  xlab('Resultado') + ylab('Frequência') + labs(fill ='Resultado') +
  ggeasy::easy_center_title() 


#' 
#' Unindo os plots para comparação de resultados ano-ano clima-clima
## --------------------------------------------------------------------------
dist_res2012 <- grid.arrange(arrangeGrob(dist_calor_quentes2012,dist_frio_quentes2012), 
                         top = 'Distribuição dos resultados em 2012')

dist_res2013 <- grid.arrange(arrangeGrob(dist_calor_quentes2013,dist_frio_quentes2013), 
                         top = 'Distribuição dos resultados em 2013')

dist_res2014 <- grid.arrange(arrangeGrob(dist_calor_quentes2014, dist_frio_quentes2014), 
                         top = 'Distribuição dos resultados em 2014')

dist_res2015 <- grid.arrange(arrangeGrob(dist_calor_quentes2015, dist_frio_quentes2015), 
                         top = 'Distribuição dos resultados em 2015')

dist_res2016 <- grid.arrange(arrangeGrob(dist_calor_quentes2016, dist_frio_quentes2016), 
                         top = 'Distribuição dos resultados em 2016')

dist_res2017 <- grid.arrange(arrangeGrob(dist_calor_quentes2017, dist_frio_quentes2017), 
                         top = 'Distribuição dos resultados em 2017')

dist_res2018 <- grid.arrange(arrangeGrob(dist_calor_quentes2018,dist_frio_quentes2018), 
                         top = 'Distribuição dos resultados em 2018')

dist_res2019 <- grid.arrange(arrangeGrob(dist_frio_quentes2019, dist_frio_quentes2019), 
                         top = 'Distribuição dos resultados em 2019')

dist_res2020 <- grid.arrange(arrangeGrob(dist_calor_quentes2020, dist_frio_quentes2020), 
                         top = 'Distribuição dos resultados em 2020')


#' 
#' 
#' Unindo as uniões
## --------------------------------------------------------------------------
dist_res20142014 <- grid.arrange(arrangeGrob(dist_calor_quentes2012, dist_frio_quentes2012,
                                             dist_calor_quentes2014, dist_frio_quentes2014), 
                         top = 'Distribuição dos resultados em 2012 e 2014')


dist_res20162018 <- grid.arrange(arrangeGrob(dist_calor_quentes2016, dist_frio_quentes2016,
                                             dist_calor_quentes2018, dist_frio_quentes2018), 
                         top = 'Distribuição dos resultados em 2016 e 2018')


#' 
#' 
#' Plot da soma de gols por estação
## --------------------------------------------------------------------------
total_inverno2012 <- medidas_sg_inverno_2012 %>% 
  mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + 
  geom_col(alpha = .8) +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2012') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2012 <- medidas_sg_verao_2012 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2012') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 


total_inverno2013 <- medidas_sg_inverno_2013 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2013') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2013 <- medidas_sg_verao_2013 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2013') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 



total_inverno2014 <- medidas_sg_inverno_2014 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2014') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2014 <- medidas_sg_verao_2014 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2014') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_inverno2015 <- medidas_sg_inverno_2015 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2015') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2015 <- medidas_sg_verao_2015 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2015') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_inverno2016 <- medidas_sg_inverno_2016 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2016') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2016 <- medidas_sg_verao_2016 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2016') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 


total_inverno2017 <- medidas_sg_inverno_2017 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2017') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2017 <- medidas_sg_verao_2017 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2017') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 


total_inverno2018 <- medidas_sg_inverno_2018 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2018') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2018 <- medidas_sg_verao_2018 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2018') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 


total_inverno2019 <- medidas_sg_inverno_2019 %>% mutate(estado = fct_reorder(estado, media)) %>%
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2014') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2019 <- medidas_sg_verao_2019 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2019') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 


total_inverno2020 <- medidas_sg_inverno_2020 %>% mutate(estado = fct_reorder(estado, media)) %>%
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2020') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

total_verao2020 <- medidas_sg_verao_2020 %>% mutate(estado = fct_reorder(estado, media)) %>% 
  group_by(estado) %>%
  ggplot(aes(media, estado, fill = periodo), position = 'identity') + geom_col() +
  scale_fill_viridis(discrete = TRUE) + 
  ggtitle('Média de gols no frio por período por estado em 2020') + 
  xlab('Média de gols') + ylab('Estado') + labs(fill = 'Período') + 
  ggeasy::easy_center_title() 

#' 
#' Unindo gols por ano estação
#' 
## --------------------------------------------------------------------------
gols0 <- grid.arrange(arrangeGrob(total_verao2014, total_inverno2014,
                         total_verao2015, total_inverno2015), 
                         top = 'Distribuição da soma dos gols por estação (1)')
gols1 <- grid.arrange(arrangeGrob(total_verao2016, total_inverno2016,
                         total_verao2017, total_inverno2017), 
                         top = 'Distribuição da soma dos gols por estação (2)')
gols2 <- grid.arrange(arrangeGrob(total_verao2018, total_inverno2018,
                         total_verao2019, total_inverno2019), 
                         top = 'Distribuição da soma dos gols por estação (3)')
gols3 <- grid.arrange(arrangeGrob(total_verao2020, total_inverno2020), 
                         top = 'Distribuição da soma dos gols por estação (4)')

#' 
#' Unindo uniões
## --------------------------------------------------------------------------
gols20122014 <- grid.arrange(arrangeGrob(total_verao2012, total_inverno2012,
                         total_verao2014, total_inverno2014), 
                         top = 'Distribuição da média da soma dos gols por clima em 2012 e 2014')

gols20162018 <- grid.arrange(arrangeGrob(total_verao2016, total_inverno2016,
                         total_verao2018, total_inverno2018), 
                         top = 'Distribuição da soma dos gols por clima em 2016 e 2018')



#' 
#' 
#' ## Plots da soma de gols por estado por estação do ano 
#' 
## --------------------------------------------------------------------------
sg_estado_inverno2012 <- soma_estado_inverno2012 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'skyblue1') +
  ggtitle('Soma de gols por estado no frio em 2012') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

sg_estado_verao2012 <- soma_estado_verao2012 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'tomato1') +
  ggtitle('Soma de gols por estado no calor em 2012') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 


sg_estado_inverno2014 <- soma_estado_inverno2014 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'skyblue1') +
  ggtitle('Soma de gols por estado no frio em 2014') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

sg_estado_verao2014 <- soma_estado_verao2014 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'tomato1') +
  ggtitle('Soma de gols por estado no calor em 2014') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 


sg_estado_inverno2016 <- soma_estado_inverno2016 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'skyblue1') +
  ggtitle('Soma de gols por estado no frio em 2016') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

sg_estado_verao2016 <- soma_estado_verao2016 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'tomato1') +
  ggtitle('Soma de gols por estado no calor em 2016') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

sg_estado_inverno2018 <- soma_estado_inverno2018 %>% 
  mutate(estado = fct_reorder(as.factor(estado), somaestado)) %>%
  ggplot(aes(somaestado, as.factor(estado))) + geom_point(colour = 'skyblue1') +
  ggtitle('Soma de gols por estado no frio em 2018') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

sg_estado_verao2018 <- soma_estado_verao2018 %>% 
  ggplot(aes(somaestado, estado)) + geom_point(colour = 'tomato1') +
  ggtitle('Soma de gols por estado no calor em 2018') + 
  xlim(10, 180) +
  xlab('Soma de gols') + ylab('Estado') +  
  ggeasy::easy_center_title() 

#' 
#' Unindo
## --------------------------------------------------------------------------
golsestado20122014 <- grid.arrange(arrangeGrob(sg_estado_inverno2012, sg_estado_verao2012,
                         sg_estado_inverno2014, sg_estado_verao2014), 
                         top = 'Soma dos gols por estado e clima em 2012 e 2014')

golsestado20162018 <- grid.arrange(arrangeGrob(sg_estado_inverno2016, sg_estado_verao2016,
                         sg_estado_inverno2018, sg_estado_verao2018), 
                         top = 'Soma dos gols por estado e clima em 2016 e 2018')

#' 
#' 
#' ## Plots da média do total de gols por clima
#' 
#' Distribuição da media da soma de gols por estação
## --------------------------------------------------------------------------
dist_gols_frio <- medidas_sg_frio %>% 
  ggplot(aes(temporada, media)) +
  geom_col(fill = 'skyblue1') +
  ggtitle('Média da soma de gols por temporada no frio') + 
  xlab('Temporada') + ylab('Média da soma de gols') +  
  ggeasy::easy_center_title() 


dist_gols_calor <- medidas_sg_calor %>% 
  ggplot(aes(temporada, media)) +
  geom_col(fill = 'tomato1') +
  ggtitle('Média da soma de gols por temporada no calor') + 
  xlab('Temporada') + ylab('Média da soma de gols') +  
  ggeasy::easy_center_title() 


#' 
#' Unindo a distribuição da média da soma de gols
## --------------------------------------------------------------------------
grid.arrange(arrangeGrob(dist_gols_frio, dist_gols_calor), top = 'Distribuição da média do total de gols por temporada e clima')

#' Distribuição da média de total de gols por periodo
#' 
## --------------------------------------------------------------------------
dist_gols_frio_per <- medidas_sg_frio_per %>% 
  ggplot(aes(temporada, media, fill = periodo)) +
  geom_col(position = 'identity') +
  scale_fill_brewer(palette = 'Blues') +
  ggtitle('Média da soma de gols por temporada no frio por período') + 
  xlab('Temporada') + ylab('Média da soma de gols') + labs(fill = 'Período') +
  ggeasy::easy_center_title() 


dist_gols_calor_per <- medidas_sg_calor_per %>% 
  ggplot(aes(temporada, media, fill = periodo)) +
  geom_col(position = 'identity') +
  scale_fill_brewer(palette = 'Reds') +
  ggtitle('Média da soma de gols por temporada no calor por período') + 
  xlab('Temporada') + ylab('Média da soma de gols') + labs(fill = 'Período') +
  ggeasy::easy_center_title() 

#' 
#' Unindo a distribuição da média do total de gols por período
#' 
## --------------------------------------------------------------------------
grid.arrange(arrangeGrob(dist_gols_frio_per, dist_gols_calor_per), top = 'Distribuição da média do total de gols por clima e por período do dia em cada temporada')

