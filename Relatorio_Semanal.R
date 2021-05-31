library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

Censo_2018 <- library(readxl)
Censo_2018 <- read_excel("C:/Users/carol/Downloads/Censo 2018.xlsx")
Escolas_2018 <- read_excel("Escolas_2018.xlsx")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

analise_escola <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                   `Nome da escola`,
                                                   `Escola pública ou privada?`,
                                                   `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`,
                                                   `Idade da Pessoa* fórmula`,
                                                   `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                   `Data dos primeiros sintomas:`,
                                                   `Data de afastamento da turma`,
                                                   `Quem fez o agendamento para caso suspeito?`,
                                                   `Tipo de Exame para os casos positivos: OBRIGATÓRIO!!!! Tipo de exame solicitado para os suspeitos`,
                                                   `Município de residência`,
                                                   `É surto?`)

names(analise_escola) <- c('ALUNO_PROF', 'ESCOLA','INSTITUICAO', 'TURMA', 'IDADE','DIAGNOSTICO','DT_PRIM_SINTOM','AFASTAMENTO', 'AGENDAMENTO', 'EXAME', 'MUNICIPIO', 'SURTO')
analise_escola$DT_PRIM_SINTOM <- as.Date(analise_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
analise_escola <- analise_escola[!is.na(analise_escola$ESCOLA),]
analise_escola <- analise_escola[!is.na(analise_escola$DIAGNOSTICO),]
analise_escola <- analise_escola[!is.na(analise_escola$ALUNO_PROF),]


analise_escola_notif <- subset(analise_escola, analise_escola$DIAGNOSTICO %in% 
                                 c("Confirmado visto laudo",
                                   "Suspeito", "Suspeito e recusou exame", 
                                   "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))

analise_escola_notif$CASOS <-1                                  
escolas_notificadas <- analise_escola_notif %>%
  group_by(ESCOLA, INSTITUICAO)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)


###### ANALISE DE NOTIFICAÇÕES POR TIPO DE INSTITUICAO ######## 

#################################### ESCOLAS FILANTROPICAS ######################################
# ESCOLAS FILANTROPICAS COM NOTIFICAÇÕES
filantropicas_notificadas <- subset(analise_escola_notif, analise_escola_notif$INSTITUICAO == "Filantropica")
filantropicas_notificadas$CASOS <-1

#LISTA DE ESCOLAS COM NOTIFICAÇÕES
filantropicas_notificadas_tot <- filantropicas_notificadas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES
sum(filantropicas_notificadas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS AFETADAS 
filantropicas_notificadas_tot$CASO <-1
sum (filantropicas_notificadas_tot$CASO)


############ ALUNOS FILANTROPICA ##############

# ALUNOS AFETADOS EM ESCOLA FILANTROPICA
alunos_notificados_filantropicas <- subset(filantropicas_notificadas, filantropicas_notificadas$ALUNO_PROF %in% 
                                             c("Aluno", "Aluno do ensino infantil",
                                               "Aluno do ensino fundamental",
                                               "Aluno do ensino médio",
                                               "Aluno de idiomas",
                                               "Outros alunos (universidade; escola para adultos; por exemplo)"))

#Lista de escolas com ALUNOS notificados
alunos_notificados_filantropicas_tot <- alunos_notificados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de ALUNOS
sum(alunos_notificados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM ALUNOS AFETADOS 
alunos_notificados_filantropicas_tot$CASO <-1
sum (alunos_notificados_filantropicas_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM ALUNOS DE ESCOLA FILATROPICA
alunos_confirmados_filantropicas <- subset(alunos_notificados_filantropicas, 
                                           alunos_notificados_filantropicas$DIAGNOSTICO  
                                          == "Confirmado visto laudo")


#Lista de escolas FILANTROPICAS com ALUNOS CONFIRMADOS
alunos_confirmados_filantropicas_tot <- alunos_confirmados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE ALUNOS CONFIRMADOS
sum(alunos_confirmados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM ALUNOS CONFIRMADOS
alunos_confirmados_filantropicas_tot$CASO <-1
sum (alunos_confirmados_filantropicas_tot$CASO)



######## PROFESSORES FILANTROPICAS ##########

prof_notificados_filantropicas <- subset(filantropicas_notificadas, filantropicas_notificadas$ALUNO_PROF %in% 
                                           c("Professor ou auxiliar de sala", 
                                             "Professor ou auxiliar de sala do ensino infantil",
                                             "Professor do ensino fundamental",
                                             "Professor do ensino médio",
                                             "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                             "Professor do ensino fundamental e médio",
                                             "Outros professores (como de universidade; escola para adultos)"))


#Lista de escolas com PROFESSORES notificados
prof_notificados_filantropicas_tot <- prof_notificados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de PROFESSORES
sum(prof_notificados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM PROFESSORES AFETADOS 
prof_notificados_filantropicas_tot$CASO <-1
sum (prof_notificados_filantropicas_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM PROFESSORES DE ESCOLA FILANTROPICA
prof_confirmados_filantropicas <- subset(prof_notificados_filantropicas, 
                                         prof_notificados_filantropicas$DIAGNOSTICO 
                                        == "Confirmado visto laudo")

#Lista de escolas FILANTROPICAS com PROFESSORES CONFIRMADOS
prof_confirmados_filantropicas_tot <- prof_confirmados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE PROFESSORES CONFIRMADOS
sum(prof_confirmados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM PROFESSORES CONFIRMADOS
prof_confirmados_filantropicas_tot$CASO <-1
sum (prof_confirmados_filantropicas_tot$CASO)


####### OUTROS COLABORADORES FILANTROPICAS #######


outros_notificados_filantropicas <- subset(filantropicas_notificadas, 
                                           filantropicas_notificadas$ALUNO_PROF  
                                            =="Outros colaboradores")


#Lista de escolas com OUTROS COLABORADORES notificados
outros_notificados_filantropicas_tot <- outros_notificados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de OUTROS COLABORADORES
sum(outros_notificados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM OUTROS COLABORADORES AFETADOS 
outros_notificados_filantropicas_tot$CASO <-1
sum (outros_notificados_filantropicas_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM OUTROS COLABORADORES DE ESCOLA FILANTROPICA
outros_confirmados_filantropicas <- subset(outros_notificados_filantropicas, 
                                           outros_notificados_filantropicas$DIAGNOSTICO  
                                            == "Confirmado visto laudo")

#Lista de escolas FILANTROPICAS com OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_filantropicas_tot <- outros_confirmados_filantropicas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE OUTROS COLABORADORES CONFIRMADOS
sum(outros_confirmados_filantropicas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FILANTROPICAS COM OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_filantropicas_tot$CASO <-1
sum (outros_confirmados_filantropicas_tot$CASO)


#####################################   ESCOLAS PRIVADAS   #########################################


# ESCOLAS PRIVADAS COM NOTIFICAÇÕES
privadas_notificadas <- subset(analise_escola_notif, analise_escola_notif$INSTITUICAO == "Privada")
privadas_notificadas$CASOS <-1

#LISTA DE ESCOLAS COM NOTIFICAÇÕES
privadas_notificadas_tot <- privadas_notificadas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES
sum(privadas_notificadas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS AFETADAS 
privadas_notificadas_tot$CASO <-1
sum (privadas_notificadas_tot$CASO)


############ ALUNOS PRIVADA ##############

# ALUNOS AFETADOS EM ESCOLA PRIVADA
alunos_notificados_priv <- subset(privadas_notificadas, privadas_notificadas$ALUNO_PROF %in% 
                                    c("Aluno", "Aluno do ensino infantil",
                                      "Aluno do ensino fundamental",
                                      "Aluno do ensino médio",
                                      "Aluno de idiomas",
                                      "Outros alunos (universidade; escola para adultos; por exemplo)"))

#Lista de escolas com ALUNOS notificados
alunos_notificados_priv_tot <- alunos_notificados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de ALUNOS
sum(alunos_notificados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM ALUNOS AFETADOS 
alunos_notificados_priv_tot$CASO <-1
sum (alunos_notificados_priv_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM ALUNOS DE ESCOLA PRIVADA
alunos_confirmados_priv <- subset(alunos_notificados_priv, 
                                  alunos_notificados_priv$DIAGNOSTICO  
                                  =="Confirmado visto laudo")


#Lista de escolas PRIVADAS com ALUNOS CONFIRMADOS
alunos_confirmados_priv_tot <- alunos_confirmados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE ALUNOS CONFIRMADOS
sum(alunos_confirmados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM ALUNOS CONFIRMADOS
alunos_confirmados_priv_tot$CASO <-1
sum (alunos_confirmados_priv_tot$CASO)



######## PROFESSORES PRIVADA ##########

prof_notificados_priv <- subset(privadas_notificadas, privadas_notificadas$ALUNO_PROF %in% 
                                  c("Professor ou auxiliar de sala", 
                                    "Professor ou auxiliar de sala do ensino infantil",
                                    "Professor do ensino fundamental",
                                    "Professor do ensino médio",
                                    "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                    "Professor do ensino fundamental e médio",
                                    "Outros professores (como de universidade; escola para adultos)"))


#Lista de escolas com PROFESSORES notificados
prof_notificados_priv_tot <- prof_notificados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de PROFESSORES
sum(prof_notificados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM PROFESSORES AFETADOS 
prof_notificados_priv_tot$CASO <-1
sum (prof_notificados_priv_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM PROFESSORES DE ESCOLA PRIVADA
prof_confirmados_priv <- subset(prof_notificados_priv, prof_notificados_priv$DIAGNOSTICO  
                                == "Confirmado visto laudo")

#Lista de escolas PRIVADAS com PROFESSORES CONFIRMADOS
prof_confirmados_priv_tot <- prof_confirmados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE PROFESSORES CONFIRMADOS
sum(prof_confirmados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM PROFESSORES CONFIRMADOS
prof_confirmados_priv_tot$CASO <-1
sum (prof_confirmados_priv_tot$CASO)



####### OUTROS COLABORADORES PRIVADA #######


outros_notificados_priv <- subset(privadas_notificadas, privadas_notificadas$ALUNO_PROF  
                                  =="Outros colaboradores")


#Lista de escolas com OUTROS COLABORADORES notificados
outros_notificados_priv_tot <- outros_notificados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de OUTROS COLABORADORES
sum(outros_notificados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM OUTROS COLABORADORES AFETADOS 
outros_notificados_priv_tot$CASO <-1
sum (outros_notificados_priv_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM OUTROS COLABORADORES DE ESCOLA PRIVADA
outros_confirmados_priv <- subset(outros_notificados_priv, outros_notificados_priv$DIAGNOSTICO 
                                  =="Confirmado visto laudo")

#Lista de escolas PRIVADAS com OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_priv_tot <- outros_confirmados_priv %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE OUTROS COLABORADORES CONFIRMADOS
sum(outros_confirmados_priv_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS PRIVADAS COM OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_priv_tot$CASO <-1
sum (outros_confirmados_priv_tot$CASO)





##################################   ESCOLAS MUNICIPAIS   ############################################


# ESCOLAS COM NOTIFICAÇÕES
municipais_notificadas <- subset(analise_escola_notif, 
                                 analise_escola_notif$INSTITUICAO %in% 
                                   c("Pública Municipal", "Conveniada com PMF"))
municipais_notificadas$CASOS <-1

#Lista de escolas com notificados
municipais_notificadas_tot <- municipais_notificadas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES
sum(municipais_notificadas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS AFETADAS 
municipais_notificadas_tot$CASO <-1
sum (municipais_notificadas_tot$CASO)



############ ALUNOS MUNICIPAIS #################

# ALUNOS AFETADOS EM ESCOLA MUNICIPAL
alunos_notificados_municipais <- subset(municipais_notificadas, municipais_notificadas$ALUNO_PROF %in% 
                                          c("Aluno", "Aluno do ensino infantil",
                                            "Aluno do ensino fundamental",
                                            "Aluno do ensino médio",
                                            "Aluno de idiomas",
                                            "Outros alunos (universidade; escola para adultos; por exemplo)"))

#Lista de escolas com ALUNOS notificados
alunos_notificados_munic_tot <- alunos_notificados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de ALUNOS
sum(alunos_notificados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM ALUNOS AFETADOS 
alunos_notificados_munic_tot$CASO <-1
sum (alunos_notificados_munic_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM ALUNOS DE ESCOLA MUNICIPAIS
alunos_confirmados_municipais <- subset(alunos_notificados_municipais, 
                                        alunos_notificados_municipais$DIAGNOSTICO  
                                        =="Confirmado visto laudo")


#Lista de escolas MUNICIPAIS com ALUNOS CONFIRMADOS
alunos_confirmados_munic_tot <- alunos_confirmados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE ALUNOS CONFIRMADOS
sum(alunos_confirmados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM ALUNOS CONFIRMADOS
alunos_confirmados_munic_tot$CASO <-1
sum (alunos_confirmados_munic_tot$CASO)



######## PROFESSORES MUNICIPAIS ##########


prof_notificados_municipais<- subset(municipais_notificadas, municipais_notificadas$ALUNO_PROF %in% 
                                       c("Professor ou auxiliar de sala", 
                                         "Professor ou auxiliar de sala do ensino infantil",
                                         "Professor do ensino fundamental",
                                         "Professor do ensino médio",
                                         "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                         "Professor do ensino fundamental e médio",
                                         "Outros professores (como de universidade; escola para adultos)"))


#Lista de escolas com PROFESSORES notificados
prof_notificados_munic_tot <- prof_notificados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de PROFESSORES
sum(prof_notificados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM PROFESSORES AFETADOS 
prof_notificados_munic_tot$CASO <-1
sum (prof_notificados_munic_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM PROFESSORES DE ESCOLA MUNICIPAIS
prof_confirmados_municipais <- subset(prof_notificados_municipais, prof_notificados_municipais$DIAGNOSTICO  
                                    =="Confirmado visto laudo")

#Lista de escolas MUNICIPAIS com PROFESSORES CONFIRMADOS
prof_confirmados_munic_tot <- prof_confirmados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE PROFESSORES CONFIRMADOS
sum(prof_confirmados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM PROFESSORES CONFIRMADOS
prof_confirmados_munic_tot$CASO <-1
sum (prof_confirmados_munic_tot$CASO)



####### OUTROS COLABORADORES MUNICIPAIS #######


outros_notificados_municipais <- subset(municipais_notificadas, municipais_notificadas$ALUNO_PROF 
                                        =="Outros colaboradores")


#Lista de escolas com OUTROS COLABORADORES notificados
outros_notificados_munic_tot <- outros_notificados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de OUTROS COLABORADORES
sum(outros_notificados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM OUTROS COLABORADORES AFETADOS 
outros_notificados_munic_tot$CASO <-1
sum (outros_notificados_munic_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM OUTROS COLABORADORES DE ESCOLA MUNICIPAIS
outros_confirmados_municipais <- subset(outros_notificados_municipais, outros_notificados_municipais$DIAGNOSTICO  
                                        == "Confirmado visto laudo")

#Lista de escolas MUNICIPAIS com OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_munic_tot <- outros_confirmados_municipais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE OUTROS COLABORADORES CONFIRMADOS
sum(outros_confirmados_munic_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS MUNICIPAIS COM OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_munic_tot$CASO <-1
sum (outros_confirmados_munic_tot$CASO)





##################################   ESCOLAS ESTADUAIS   ############################################


# ESCOLAS COM NOTIFICAÇÕES
estaduais_notificadas <- subset(analise_escola_notif, 
                                analise_escola_notif$INSTITUICAO == 
                                  "Pública Estadual")
estaduais_notificadas$CASOS <-1

#Lista de escolas com notificados
estaduais_notificadas_tot <- estaduais_notificadas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES
sum(estaduais_notificadas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS AFETADAS 
estaduais_notificadas_tot$CASO <-1
sum (estaduais_notificadas_tot$CASO)



############ ALUNOS ESTADUAIS #################

# ALUNOS AFETADOS EM ESCOLA ESTADUAL
alunos_notificados_estaduais <- subset(estaduais_notificadas, estaduais_notificadas$ALUNO_PROF %in% 
                                         c("Aluno", "Aluno do ensino infantil",
                                           "Aluno do ensino fundamental",
                                           "Aluno do ensino médio",
                                           "Aluno de idiomas",
                                           "Outros alunos (universidade; escola para adultos; por exemplo)"))


#Lista de escolas com ALUNOS notificados
alunos_notificados_est_tot <- alunos_notificados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de ALUNOS
sum(alunos_notificados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM ALUNOS AFETADOS 
alunos_notificados_est_tot$CASO <-1
sum (alunos_notificados_est_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM ALUNOS DE ESCOLA ESTADUAIS
alunos_confirmados_estaduais <- subset(alunos_notificados_estaduais, 
                                       alunos_notificados_estaduais$DIAGNOSTICO 
                                      =="Confirmado visto laudo")


#Lista de escolas ESTADUAIS com ALUNOS CONFIRMADOS
alunos_confirmados_est_tot <- alunos_confirmados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE ALUNOS CONFIRMADOS
sum(alunos_confirmados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM ALUNOS CONFIRMADOS
alunos_confirmados_est_tot$CASO <-1
sum (alunos_confirmados_est_tot$CASO)



######## PROFESSORES ESTADUAIS ##########


prof_notificados_estaduais <- subset(estaduais_notificadas, estaduais_notificadas$ALUNO_PROF %in% 
                                       c("Professor ou auxiliar de sala", 
                                         "Professor ou auxiliar de sala do ensino infantil",
                                         "Professor do ensino fundamental",
                                         "Professor do ensino médio",
                                         "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                         "Professor do ensino fundamental e médio",
                                         "Outros professores (como de universidade; escola para adultos)"))


#Lista de escolas com PROFESSORES notificados
prof_notificados_est_tot <- prof_notificados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de PROFESSORES
sum(prof_notificados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM PROFESSORES AFETADOS 
prof_notificados_est_tot$CASO <-1
sum (prof_notificados_est_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM PROFESSORES DE ESCOLA ESTADUAIS
prof_confirmados_estaduais <- subset(prof_notificados_estaduais, prof_notificados_estaduais$DIAGNOSTICO  
                                    =="Confirmado visto laudo")

#Lista de escolas ESTADUAIS com PROFESSORES CONFIRMADOS
prof_confirmados_est_tot <- prof_confirmados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE PROFESSORES CONFIRMADOS
sum(prof_confirmados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM PROFESSORES CONFIRMADOS
prof_confirmados_est_tot$CASO <-1
sum (prof_confirmados_est_tot$CASO)



####### OUTROS COLABORADORES ESTADUAIS #######


outros_notificados_estaduais <- subset(estaduais_notificadas, estaduais_notificadas$ALUNO_PROF  
                                      =="Outros colaboradores")


#Lista de escolas com OUTROS COLABORADORES notificados
outros_notificados_est_tot <- outros_notificados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de OUTROS COLABORADORES
sum(outros_notificados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM OUTROS COLABORADORES AFETADOS 
outros_notificados_est_tot$CASO <-1
sum (outros_notificados_est_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM OUTROS COLABORADORES DE ESCOLA ESTADUAIS
outros_confirmados_estaduais <- subset(outros_notificados_estaduais, outros_notificados_estaduais$DIAGNOSTICO  
                                      =="Confirmado visto laudo")

#Lista de escolas ESTADUAIS com OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_est_tot <- outros_confirmados_estaduais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE OUTROS COLABORADORES CONFIRMADOS
sum(outros_confirmados_est_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS ESTADUAIS COM OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_est_tot$CASO <-1
sum (outros_confirmados_est_tot$CASO)



####################### ESCOLAS FEDERAIS #####################################

# ESCOLAS COM NOTIFICAÇÕES
federais_notificadas <- subset(analise_escola_notif, 
                               analise_escola_notif$INSTITUICAO == 
                                 "Pública Federal")
federais_notificadas$CASOS <-1

#Lista de escolas com notificados
federais_notificadas_tot <- federais_notificadas %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES
sum(federais_notificadas_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS AFETADAS 
federais_notificadas_tot$CASO <-1
sum (federais_notificadas_tot$CASO)



############ ALUNOS FEDERAIS #################

# ALUNOS AFETADOS EM ESCOLA FEDERAl
alunos_notificados_federais <- subset(federais_notificadas, federais_notificadas$ALUNO_PROF %in% 
                                        c("Aluno", "Aluno do ensino infantil",
                                          "Aluno do ensino fundamental",
                                          "Aluno do ensino médio",
                                          "Aluno de idiomas",
                                          "Outros alunos (universidade; escola para adultos; por exemplo)"))


#Lista de escolas com ALUNOS notificados
alunos_notificados_federais_tot <- alunos_notificados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de ALUNOS
sum(alunos_notificados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM ALUNOS AFETADOS 
alunos_notificados_federais_tot$CASO <-1
sum (alunos_notificados_federais_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM ALUNOS DE ESCOLA FEDERAIS
alunos_confirmados_federais <- subset(alunos_notificados_federais, 
                                      alunos_notificados_federais$DIAGNOSTICO  
                                      =="Confirmado visto laudo")


#Lista de escolas FEDERAIS com ALUNOS CONFIRMADOS
alunos_confirmados_federais_tot <- alunos_confirmados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE ALUNOS CONFIRMADOS
sum(alunos_confirmados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM ALUNOS CONFIRMADOS
alunos_confirmados_federais_tot$CASO <-1
sum (alunos_confirmados_federais_tot$CASO)



######## PROFESSORES FEDERAIS ##########


prof_notificados_federais <- subset(federais_notificadas, federais_notificadas$ALUNO_PROF %in% 
                                      c("Professor ou auxiliar de sala", 
                                        "Professor ou auxiliar de sala do ensino infantil",
                                        "Professor do ensino fundamental",
                                        "Professor do ensino médio",
                                        "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                        "Professor do ensino fundamental e médio",
                                        "Outros professores (como de universidade; escola para adultos)"))


#Lista de escolas com PROFESSORES notificados
prof_notificados_federais_tot <- prof_notificados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de PROFESSORES
sum(prof_notificados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM PROFESSORES AFETADOS 
prof_notificados_federais_tot$CASO <-1
sum (prof_notificados_federais_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM PROFESSORES DE ESCOLA FEDERAIS
prof_confirmados_federais <- subset(prof_notificados_federais, prof_notificados_federais$DIAGNOSTICO  
                                   =="Confirmado visto laudo")

#Lista de escolas FEDERAIS com PROFESSORES CONFIRMADOS
prof_confirmados_federais_tot <- prof_confirmados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE PROFESSORES CONFIRMADOS
sum(prof_confirmados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM PROFESSORES CONFIRMADOS
prof_confirmados_federais_tot$CASO <-1
sum (prof_confirmados_federais_tot$CASO)



####### OUTROS COLABORADORES FEDERAIS #######


outros_notificados_federais <- subset(federais_notificadas, federais_notificadas$ALUNO_PROF 
                                      =="Outros colaboradores")


#Lista de escolas com OUTROS COLABORADORES notificados
outros_notificados_federais_tot <- outros_notificados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE NOTIFICAÇÕES de OUTROS COLABORADORES
sum(outros_notificados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM OUTROS COLABORADORES AFETADOS 
outros_notificados_federais_tot$CASO <-1
sum (outros_notificados_federais_tot$CASO)

# TOTAL DE CASOS CONFIRMADOS EM OUTROS COLABORADORES DE ESCOLA FEDERAIS
outros_confirmados_federais <- subset(outros_notificados_federais, outros_notificados_federais$DIAGNOSTICO 
                                      =="Confirmado visto laudo")

#Lista de escolas FEDERAIS com OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_federais_tot <- outros_confirmados_federais %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASOS, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

#TOTAL DE OUTROS COLABORADORES CONFIRMADOS
sum(outros_confirmados_federais_tot$CASOS_ESCOLA) 

# TOTAL DE ESCOLAS FEDERAIS COM OUTROS COLABORADORES CONFIRMADOS
outros_confirmados_federais_tot$CASO <-1
sum (outros_confirmados_federais_tot$CASO)


########################    TOTAIS   #########################################
## ESCOLAS AFETADAS ## 
privada <- sum(privadas_notificadas_tot$CASO)
municipal <- sum (municipais_notificadas_tot$CASO)
estadual <- sum (estaduais_notificadas_tot$CASO)
federal <- sum (federais_notificadas_tot$CASO)
filantropica <- sum (filantropicas_notificadas_tot$CASO)
escolas_tipo_escola <- data.frame("Privada" = privada,
                                  "Publica Municipal" = municipal,
                                  "Publica Estadual" = estadual,
                                  "Publica Federal" = federal,
                                  "Filantropica" = filantropica)
escolas_tipo_escola <- melt(escolas_tipo_escola)
names(escolas_tipo_escola) <- c("Tipo", "Escolas")

sum(escolas_tipo_escola$Escolas)

## TOTAL NOTIFICAÇÕES 
privada <- sum(privadas_notificadas_tot$CASOS_ESCOLA)
municipal <- sum(municipais_notificadas_tot$CASOS_ESCOLA)
estadual <-  sum(estaduais_notificadas_tot$CASOS_ESCOLA)
federal <- sum(federais_notificadas_tot$CASOS_ESCOLA)
filantropica <- sum(filantropicas_notificadas_tot$CASOS_ESCOLA)
notificacao_tipo_escola <- data.frame("Privada" = privada,
                                      "Publica Municipal" = municipal,
                                      "Publica Estadual" = estadual,
                                      "Publica Federal" = federal,
                                      "Filantropica" = filantropica)

notificacao_tipo_escola <- melt(notificacao_tipo_escola)
names(notificacao_tipo_escola) <- c("Tipo", "Notificacoes")

sum(notificacao_tipo_escola$Notificacoes)

### ALUNOS NOTIFICADOS ## 
privada <- sum(alunos_notificados_priv_tot$CASOS_ESCOLA)
municipal<- sum(alunos_notificados_munic_tot$CASOS_ESCOLA)
estadual <- sum(alunos_notificados_est_tot$CASOS_ESCOLA)
federal <- sum(alunos_notificados_federais_tot$CASOS_ESCOLA)  
filantropica <- sum(alunos_notificados_filantropicas_tot$CASOS_ESCOLA)

aluno_notificacao_tipo_escola <- data.frame("Privada" = privada,
                                            "Publica Municipal" = municipal,
                                            "Publica Estadual" = estadual,
                                            "Publica Federal" = federal,
                                            "Filantropica" = filantropica)

aluno_notificacao_tipo_escola <- melt(aluno_notificacao_tipo_escola)
names(aluno_notificacao_tipo_escola) <- c("Tipo", "Notificacoes")

sum(aluno_notificacao_tipo_escola$Notificacoes)

## ALUNOS CONFIRMADOS ##
privada <- sum(alunos_confirmados_priv_tot$CASOS_ESCOLA)
municipal <- sum(alunos_confirmados_munic_tot$CASOS_ESCOLA)
estadual <- sum(alunos_confirmados_est_tot$CASOS_ESCOLA)
federal <- sum(alunos_confirmados_federais_tot$CASOS_ESCOLA)  
filantropica <- sum(alunos_confirmados_filantropicas_tot$CASOS_ESCOLA)

aluno_conf_tipo_escola <- data.frame("Privada" = privada,
                                     "Publica Municipal" = municipal,
                                     "Publica Estadual" = estadual,
                                     "Publica Federal" = federal,
                                     "Filantropica" = filantropica)

aluno_conf_tipo_escola <- melt(aluno_conf_tipo_escola)
names(aluno_conf_tipo_escola) <- c("Tipo", "Casos")

sum(aluno_conf_tipo_escola$Casos)

## Professores Notificados ## 
privada <- sum(prof_notificados_priv_tot$CASOS_ESCOLA)
municipal <- sum(prof_notificados_munic_tot$CASOS_ESCOLA)
estadual <- sum(prof_notificados_est_tot$CASOS_ESCOLA)
federal < - sum(prof_notificados_federais_tot$CASOS_ESCOLA) 
filantropica <-sum(prof_notificados_filantropicas_tot$CASOS_ESCOLA)

prof_notificacao_tipo_escola <- data.frame("Privada" = privada,
                                           "Publica Municipal" = municipal,
                                           "Publica Estadual" = estadual,
                                           "Publica Federal" = federal,
                                           "Filantropica" = filantropica)

prof_notificacao_tipo_escola <- melt(prof_notificacao_tipo_escola)
names(prof_notificacao_tipo_escola) <- c("Tipo", "Notificacoes")

sum(prof_notificacao_tipo_escola$Notificacoes)


### PROFESSORES CONFIRMADOS### 
privada <- sum(prof_confirmados_priv_tot$CASOS_ESCOLA)
municipal <- sum(prof_confirmados_munic_tot$CASOS_ESCOLA)
estadual <- sum(prof_confirmados_est_tot$CASOS_ESCOLA) 
federal <- sum(prof_confirmados_federais_tot$CASOS_ESCOLA)  
filantropica <- sum(prof_confirmados_filantropicas_tot$CASOS_ESCOLA)

prof_conf_tipo_escola <- data.frame("Privada" = privada,
                                    "Publica Municipal" = municipal,
                                    "Publica Estadual" = estadual,
                                    "Publica Federal" = federal,
                                    "Filantropica" = filantropica)

prof_conf_tipo_escola <- melt(prof_conf_tipo_escola)
names(prof_conf_tipo_escola) <- c("Tipo", "Casos")

sum(prof_conf_tipo_escola$Casos)


####OUTROS COLABORADORES NOTIF ### 
privada <-sum(outros_notificados_priv_tot$CASOS_ESCOLA)
municipal <- sum(outros_notificados_munic_tot$CASOS_ESCOLA)
estadual <- sum(outros_notificados_est_tot$CASOS_ESCOLA)
federal <- sum(outros_notificados_federais_tot$CASOS_ESCOLA)  
filantropica <-  sum(outros_notificados_filantropicas_tot$CASOS_ESCOLA)

outros_notificacao_tipo_escola <- data.frame("Privada" = privada,
                                             "Publica Municipal" = municipal,
                                             "Publica Estadual" = estadual,
                                             "Publica Federal" = federal,
                                             "Filantropica" = filantropica)

outros_notificacao_tipo_escola <- melt(outros_notificacao_tipo_escola)
names(outros_notificacao_tipo_escola) <- c("Tipo", "Notificacoes")

sum(outros_notificacao_tipo_escola$Notificacoes)                                                                                            

#### OUTROS COLABORADORES CONF ### 
privada <- sum(outros_confirmados_priv_tot$CASOS_ESCOLA)
municipal <- sum(outros_confirmados_munic_tot$CASOS_ESCOLA)
estadual <-  sum(outros_confirmados_est_tot$CASOS_ESCOLA)
federal <- sum(outros_confirmados_federais_tot$CASOS_ESCOLA)  
filantropica <-sum(outros_confirmados_filantropicas_tot$CASOS_ESCOLA)

outros_conf_tipo_escola <- data.frame("Privada" = privada,
                                      "Publica Municipal" = municipal,
                                      "Publica Estadual" = estadual,
                                      "Publica Federal" = federal,
                                      "Filantropica" = filantropica)

outros_conf_tipo_escola <- melt(outros_conf_tipo_escola)
names(outros_conf_tipo_escola) <- c("Tipo", "Casos")

sum(outros_conf_tipo_escola$Casos)


###### DATA FRAME ########

placar_geral_1 <- data.frame(Escolas = c('PRIVADAS', 'MUNICIPAIS', 'ESTADUAIS', 'FEDERAIS', 'FILANTROPICAS', 'TOTAL'),
                           
                           "ESCOLAS AFETADAS" = c (sum(privadas_notificadas_tot$CASO), sum (municipais_notificadas_tot$CASO),
                                                   sum (estaduais_notificadas_tot$CASO), sum (federais_notificadas_tot$CASO), 
                                                   sum (filantropicas_notificadas_tot$CASO), sum(escolas_tipo_escola$Escolas)), 
                           
                           "TOTAL NOTIFICAÇÕES" = c(sum(privadas_notificadas_tot$CASOS_ESCOLA), sum(municipais_notificadas_tot$CASOS_ESCOLA),
                                                    sum(estaduais_notificadas_tot$CASOS_ESCOLA), sum(federais_notificadas_tot$CASOS_ESCOLA),
                                                    sum(filantropicas_notificadas_tot$CASOS_ESCOLA), sum(notificacao_tipo_escola$Notificacoes)),
                           
                           "ALUNOS NOTIFICADOS" = c (sum(alunos_notificados_priv_tot$CASOS_ESCOLA),sum(alunos_notificados_munic_tot$CASOS_ESCOLA), 
                                                     sum(alunos_notificados_est_tot$CASOS_ESCOLA), sum(alunos_notificados_federais_tot$CASOS_ESCOLA), 
                                                     sum(alunos_notificados_filantropicas_tot$CASOS_ESCOLA), sum(aluno_notificacao_tipo_escola$Notificacoes)),
                           
                           "ALUNOS CONFIRMADOS" = c(sum(alunos_confirmados_priv_tot$CASOS_ESCOLA), sum(alunos_confirmados_munic_tot$CASOS_ESCOLA),
                                                    sum(alunos_confirmados_est_tot$CASOS_ESCOLA), sum(alunos_confirmados_federais_tot$CASOS_ESCOLA), 
                                                    sum(alunos_confirmados_filantropicas_tot$CASOS_ESCOLA), sum(aluno_conf_tipo_escola$Casos)), 
                           
                           "PROFESSORES NOTIFICADOS" = c (sum(prof_notificados_priv_tot$CASOS_ESCOLA), sum(prof_notificados_munic_tot$CASOS_ESCOLA), 
                                                          sum(prof_notificados_est_tot$CASOS_ESCOLA), sum(prof_notificados_federais_tot$CASOS_ESCOLA), 
                                                          sum(prof_notificados_filantropicas_tot$CASOS_ESCOLA), sum(prof_notificacao_tipo_escola$Notificacoes)), 
                           
                           "PROFESSORES CONFIRMADOS" = c(sum(prof_confirmados_priv_tot$CASOS_ESCOLA), sum(prof_confirmados_munic_tot$CASOS_ESCOLA), 
                                                         sum(prof_confirmados_est_tot$CASOS_ESCOLA) , sum(prof_confirmados_federais_tot$CASOS_ESCOLA),
                                                         sum(prof_confirmados_filantropicas_tot$CASOS_ESCOLA), sum(prof_conf_tipo_escola$Casos)),
                           
                           "OUTROS COLABORADORES NOTIF" = c(sum(outros_notificados_priv_tot$CASOS_ESCOLA), sum(outros_notificados_munic_tot$CASOS_ESCOLA),
                                                            sum(outros_notificados_est_tot$CASOS_ESCOLA), sum(outros_notificados_federais_tot$CASOS_ESCOLA), 
                                                            sum(outros_notificados_filantropicas_tot$CASOS_ESCOLA), sum(outros_notificacao_tipo_escola$Notificacoes)), 
                           
                           "OUTROS COLABORADORES CONF" = c(sum(outros_confirmados_priv_tot$CASOS_ESCOLA), sum(outros_confirmados_munic_tot$CASOS_ESCOLA), 
                                                           sum(outros_confirmados_est_tot$CASOS_ESCOLA), sum(outros_confirmados_federais_tot$CASOS_ESCOLA), 
                                                           sum(outros_confirmados_filantropicas_tot$CASOS_ESCOLA), sum(outros_conf_tipo_escola$Casos)))

#Dados em CSV
write.csv(placar_geral_1, "placar_geral_1.csv", row.names = F)


#### Tabelas com as proporções ### 
alunos <- (placar_geral_1$ALUNOS.CONFIRMADOS / placar_geral_1$ALUNOS.NOTIFICADOS *100)
prof  <-  (placar_geral_1$PROFESSORES.CONFIRMADOS/placar_geral_1$PROFESSORES.NOTIFICADOS *100)
outros <- (placar_geral_1$OUTROS.COLABORADORES.CONF/placar_geral_1$OUTROS.COLABORADORES.NOTIF *100)

proporcao_ <- data.frame("TAXA_ALUNOS" = alunos,
                         "TAXA_PROFESSORES" = prof,
                          "TAXA_OUTROS" = outros)


proporcao_ <- data.frame (ESCOLA = c("PRIVADA", "MUNICIPAL", "ESTADUAL", "FEDERAL","FILANTROPICA", "TOTAL"), 
"Proporção_Alunos" = (placar_geral_1$ALUNOS.CONFIRMADOS / placar_geral_1$ALUNOS.NOTIFICADOS *100),
"Proporçãoc_Professores" = (placar_geral_1$PROFESSORES.CONFIRMADOS/placar_geral_1$PROFESSORES.NOTIFICADOS *100),
"Proporção_Outros" = (placar_geral_1$OUTROS.COLABORADORES.CONF/placar_geral_1$OUTROS.COLABORADORES.NOTIF *100))



                        
### PROPORÇÃO TOTAL ## 
privada <- sum((alunos_confirmados_priv_tot$CASOS_ESCOLA), (prof_confirmados_priv_tot$CASOS_ESCOLA), (outros_confirmados_priv_tot$CASOS_ESCOLA)) /sum((alunos_notificados_priv_tot$CASOS_ESCOLA), (prof_notificados_priv_tot$CASOS_ESCOLA), (outros_notificados_priv_tot$CASOS_ESCOLA)) *100
municipal<-sum ((alunos_confirmados_munic_tot$CASOS_ESCOLA),(prof_confirmados_munic_tot$CASOS_ESCOLA),(outros_confirmados_munic_tot$CASOS_ESCOLA)) / sum ((alunos_notificados_munic_tot$CASOS_ESCOLA), (prof_notificados_munic_tot$CASOS_ESCOLA), (outros_notificados_munic_tot$CASOS_ESCOLA))*100
estadual <-sum ((alunos_confirmados_est_tot$CASOS_ESCOLA), (prof_confirmados_est_tot$CASOS_ESCOLA), (outros_confirmados_est_tot$CASOS_ESCOLA)) / sum((alunos_notificados_est_tot$CASOS_ESCOLA), (prof_notificados_est_tot$CASOS_ESCOLA), (outros_notificados_est_tot$CASOS_ESCOLA))*100
federal <- sum ((alunos_confirmados_federais_tot$CASOS_ESCOLA), (prof_confirmados_federais_tot$CASOS_ESCOLA), (outros_confirmados_federais_tot$CASOS_ESCOLA)) / sum((alunos_notificados_federais_tot$CASOS_ESCOLA), (prof_notificados_federais_tot$CASOS_ESCOLA), (outros_notificados_federais_tot$CASOS_ESCOLA))*100 
filantropica <-sum((alunos_confirmados_filantropicas_tot$CASOS_ESCOLA), (prof_confirmados_filantropicas_tot$CASOS_ESCOLA), (outros_confirmados_filantropicas_tot$CASOS_ESCOLA)) / sum((alunos_notificados_filantropicas_tot$CASOS_ESCOLA), (prof_notificados_filantropicas_tot$CASOS_ESCOLA), (outros_notificados_filantropicas_tot$CASOS_ESCOLA))*100

proporcao_tot <- data.frame("PRIVADA" = privada,
                            "MUNICIPAL" = municipal,
                            "ESTADUAL" = estadual,
                            "FEDERAL" = federal,
                            "FILANTROPICA" = filantropica)

proporcao_tot <- melt(proporcao_tot)
names(proporcao_tot) <- c("ESCOLA", "TAXA_POSITIVOS")

placar_taxas <- merge(proporcao_, proporcao_tot, by = 'ESCOLA')

#Dados em CSV
write.csv(placar_taxas, "placar_taxas.csv", row.names = F)


                             
################## Proporação Publica x Privada (Grafico)############################


publica_privada <- notificaescola %>% dplyr::select (`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                     `Nome da escola`,
                                                     `Data dos primeiros sintomas:`,
                                                     `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                     `É surto?`,
                                                     `Escola pública ou privada?`,
                                                     `Idade da Pessoa* fórmula`) 

names(publica_privada) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'INSTITUICAO', 'IDADE')
publica_privada$DT_PRIM_SINTOM <- as.Date(publica_privada$DT_PRIM_SINTOM, format = '%d/%m/%Y')
publica_privada <- publica_privada[!is.na(publica_privada$ESCOLA),]
publica_privada <- publica_privada[!is.na(publica_privada$DIAGNOSTICO),]
publica_privada <- publica_privada[!is.na(publica_privada$ALUNO_PROF),]

publica_privada <- subset(publica_privada, publica_privada$DIAGNOSTICO %in% 
                            c("Confirmado visto laudo",
                              "Suspeito", "Suspeito e recusou exame", 
                              "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))


################ ESCOLA PRIVADA ###########################

# Calculo de notificados por escola privada
privada_privada <- subset(publica_privada, publica_privada$INSTITUICAO == 
                            "Privada")
privada_privada$CASO <- 1
casos_privada <- privada_privada %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_privada, "casos_privada.csv", row.names = F)

#CALCULO DE POSITIVOS - PRIVADA
privada_privada_conf <- subset(privada_privada, privada_privada$DIAGNOSTICO == 
                              "Confirmado visto laudo")
sum(privada_privada_conf$CASO)


################### ESCOLA MUNICIPAL ####################
# Calculo de notificados por escola publica municipal 
publica_municipal <- subset(publica_privada, publica_privada$INSTITUICAO %in% 
                              c("Pública Municipal", "Conveniada com PMF"))

publica_municipal$CASO <- 1
casos_municipal <- publica_municipal %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_municipal, "casos_municipal.csv", row.names = F)

#CALCULO DE POSITIVOS - MUNICIPAL
publica_municipal_conf <- subset(publica_municipal, publica_municipal$DIAGNOSTICO == 
                                "Confirmado visto laudo")
sum(publica_municipal_conf$CASO)


######################### ESCOLA ESTADUAL ##############################

# Calculo de notificados por escola estadual
publica_estadual <- subset(publica_privada, publica_privada$INSTITUICAO == 
                             "Pública Estadual")
publica_estadual$CASO <- 1
casos_estadual <- publica_estadual %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_estadual, "casos_estadual.csv", row.names = F)

#CALCULO DE POSITIVOS - ESTADUAL
publica_estadual_conf <- subset(publica_estadual, publica_estadual$DIAGNOSTICO == 
                                "Confirmado visto laudo")
sum(publica_estadual_conf$CASO)



######################### ESCOLA FEDERAL ##############################

# Calculo de notificados por escola federal
publica_federal <- subset(publica_privada, publica_privada$INSTITUICAO == 
                            "Pública Federal")
publica_federal$CASO <- 1
casos_federal <- publica_federal %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_federal, "casos_federal.csv", row.names = F)

#CALCULO DE POSITIVOS - FEDERAL
publica_federal_conf <- subset(publica_federal, publica_federal$DIAGNOSTICO == 
                              "Confirmado visto laudo")
sum(publica_federal_conf$CASO)

######################### ESCOLA FILANTROPICA ##############################

# Calculo de notificados por escola FILANTROPICA
publica_filantropica <- subset(publica_privada, publica_privada$INSTITUICAO == 
                                 "Filantropica")
publica_filantropica$CASO <- 1
casos_filantropica <- publica_filantropica %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_filantropica, "casos_filantropica.csv", row.names = F)

#CALCULO DE POSITIVOS - filantropica
publica_filantropica_conf <- subset(publica_filantropica, publica_filantropica$DIAGNOSTICO == 
                                   "Confirmado visto laudo")
sum(publica_filantropica_conf$CASO)



#Comparativo casos NOTIFICADOS
tot_casos_privada <- sum(casos_privada$CASOS_ESCOLA)
tot_casos_municipal <- sum(casos_municipal$CASOS_ESCOLA)
tot_casos_estadual <- sum(casos_estadual$CASOS_ESCOLA)
tot_casos_federal <- sum(casos_federal$CASOS_ESCOLA) 
tot_casos_filantropica <- sum(casos_filantropica$CASOS_ESCOLA)

total_casos_tipo_escola <- data.frame("Privada" = tot_casos_privada,
                                      "Municipal" = tot_casos_municipal,
                                      "Estadual" = tot_casos_estadual,
                                      "Federal" = tot_casos_federal,
                                      "Filantropica" = tot_casos_filantropica)

total_casos_tipo_escola <- melt(total_casos_tipo_escola)
names(total_casos_tipo_escola) <- c("Tipo", "Quantidade")

ggplot(total_casos_tipo_escola, aes(x= Tipo, y= Quantidade, fill= Tipo ))+
  geom_col()+
  theme_bw()+
  xlab("Escolas")+
  ylab(" Notificações") 

write.csv(total_casos_tipo_escola, "total_casos_tipo_escola.csv", row.names = F)

######PROPORÇÃO POR TIPO DE ESCOLA
prop_casos_privada <- (sum(casos_privada$CASOS_ESCOLA) /200) 
prop_casos_municipal <- (sum(casos_municipal$CASOS_ESCOLA)/134)
prop_casos_estadual <- (sum(casos_estadual$CASOS_ESCOLA)/57)
prop_casos_federal <- (sum(casos_federal$CASOS_ESCOLA)/4) 


prop_casos_tipo_escola <- data.frame("Privada" = prop_casos_privada,
                                    "Municipal" = prop_casos_municipal,
                                    "Estadual" = prop_casos_estadual,
                                    "Federal" = prop_casos_federal)
                                      

prop_casos_tipo_escola <- melt(prop_casos_tipo_escola)
names(prop_casos_tipo_escola) <- c("Tipo", "Quantidade")

ggplot(prop_casos_tipo_escola, aes(x= Tipo, y= Quantidade, fill= Tipo ))+
  geom_col()+
  theme_bw()+
  xlab("Escolas")+
  ylab(" Proporção") 



################################# NÍVEL DE ESCOLARIDADE ##############################


notif_geral <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                `Nome da escola`,
                                                `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                `Idade da Pessoa* fórmula`) 

names(notif_geral) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA','DIAGNOSTICO','IDADE')
notif_geral$TURMA <- ifelse(notif_geral$TURMA == 'NULL', NA, notif_geral$TURMA)

notif_geral <-notif_geral[!is.na(notif_geral$ESCOLA),]
notif_geral <- notif_geral[!is.na(notif_geral$DIAGNOSTICO),]
notif_geral <- notif_geral[!is.na(notif_geral$ALUNO_PROF),]


notif_geral <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% 
                        c("Confirmado visto laudo",
                          "Suspeito", "Suspeito e recusou exame", 
                          "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))

############## ANALISE CASOS NOTIFICADOS POR ESCOLARIDADE ###################### 



######## ANALISE CASOS CONFIRMADOS POR ESCOLARIDADE (mantido casos finalizados) ##########

notif_geral_conf <- subset(notif_geral, notif_geral$DIAGNOSTICO == 
                          "Confirmado visto laudo")


############ CASOS EM ALUNOS POR ESCOLARIDADE ##################### 
notif_geral_alunos_conf <- subset(notif_geral_conf, notif_geral_conf$ALUNO_PROF %in%
                                    c("Aluno", "Aluno do ensino infantil",
                                      "Aluno do ensino fundamental",
                                      "Aluno do ensino médio",
                                      "Aluno de idiomas",
                                      "Outros alunos (universidade; escola para adultos; por exemplo)"))



notif_geral_alunos_conf$ESCOLARIDADE <- ifelse(notif_geral_alunos_conf$ALUNO_PROF == "Aluno do ensino infantil", "Ensino Infantil",
                                        ifelse(notif_geral_alunos_conf$ALUNO_PROF == "Aluno do ensino fundamental","Ensino Fundamental", 
                                        ifelse(notif_geral_alunos_conf$ALUNO_PROF == "Aluno do ensino médio" ,"Ensino Médio",
                                        ifelse(notif_geral_alunos_conf$ALUNO_PROF == "Outros alunos (universidade; escola para adultos; por exemplo)", "Outros alunos",
                                        ifelse(notif_geral_alunos_conf$ALUNO_PROF == "Aluno de idiomas", "Aluno de idiomas", NA)))))


notif_geral_alunos_conf$CASOS <- 1

alunos_conf_nivel_escolaridade <- notif_geral_alunos_conf %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(alunos_conf_nivel_escolaridade, "alunos_conf_nivel_escolaridade.csv", row.names = F)

ggplot(alunos_conf_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 25))

###### Nº DE CASOS POR ESCOLARIDADE EM ALUNOS #######    **** Dado importante!!! *****
casos_idade_pura_alunos <- notif_geral_alunos_conf %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

casos_infantil <- subset(alunos_conf_nivel_escolaridade, alunos_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Infantil")
sum(casos_infantil$CASOS)

casos_fudamental <- subset(alunos_conf_nivel_escolaridade, alunos_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Fundamental")
sum(casos_fudamental$CASOS)

casos_medio <- subset(alunos_conf_nivel_escolaridade, alunos_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Médio")
sum(casos_medio$CASOS)

#Dados em CSV
write.csv(casos_idade_pura_alunos, "casos_idade_pura_alunos.csv", row.names = F)

ggplot(casos_idade_pura_alunos, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 




############ CASOS EM PROFESSORES POR ESCOLARIDADE ##################### 
notif_geral_prof_conf <- subset(notif_geral_conf, notif_geral_conf$ALUNO_PROF %in%
                                  c("Professor ou auxiliar de sala", 
                                    "Professor ou auxiliar de sala do ensino infantil",
                                    "Professor do ensino fundamental",
                                    "Professor do ensino médio",
                                    "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                    "Professor do ensino fundamental e médio",
                                    "Outros professores (como de universidade; escola para adultos)"))



notif_geral_prof_conf$ESCOLARIDADE <- ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor ou auxiliar de sala do ensino infantil", "Ensino Infantil",
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor do ensino fundamental","Ensino Fundamental", 
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor do ensino médio" ,"Ensino Médio",
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor ou auxiliar de sala", "Ignorado",
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor ou auxiliar de sala de ensino infantil ou fundamental", "Ignorado",
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Professor do ensino fundamental e médio", "Ignorado",
                                      ifelse(notif_geral_prof_conf$ALUNO_PROF == "Outros professores (como de universidade; escola para adultos)", "Outros Professores", NA))))))) 

notif_geral_prof_conf$CASOS <- 1

prof_conf_nivel_escolaridade <- notif_geral_prof_conf %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(prof_conf_nivel_escolaridade, "prof_conf_nivel_escolaridade.csv", row.names = F)

ggplot(prof_conf_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 25))

###### Nº DE CASOS POR ESCOLARIDADE EM PROFESSORES #######   **** Dado importante!!! *****
casos_idade_pura_prof <- notif_geral_prof_conf %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))


casos_infantil_prof <- subset(prof_conf_nivel_escolaridade, prof_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Infantil")
sum(casos_infantil_prof$CASOS)

casos_fudamental_prof <- subset(prof_conf_nivel_escolaridade, prof_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Fundamental")
sum(casos_fudamental_prof$CASOS)

casos_medio_prof <- subset(prof_conf_nivel_escolaridade, prof_conf_nivel_escolaridade$ESCOLARIDADE == "Ensino Médio")
sum(casos_medio_prof$CASOS)

casos_outros_prof <- subset(prof_conf_nivel_escolaridade, prof_conf_nivel_escolaridade$ESCOLARIDADE == "Outros Professores")
sum(casos_outros_prof$CASOS)

casos_ignorado_prof <- subset(prof_conf_nivel_escolaridade, prof_conf_nivel_escolaridade$ESCOLARIDADE == "Ignorado")
sum(casos_ignorado_prof$CASOS)


#Dados em CSV
write.csv(casos_idade_pura_prof, "casos_idade_pura_prof.csv", row.names = F)

ggplot(casos_idade_pura_prof, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 



################### DESCARTADOS POR NÍVEL DE ENSINO ######################## 
notif_geral_descartado <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% 
                                   c ("Descartado", "Descartado (suspeito que fez exame e foi negativo)"))


############ CASOS DESCARTADOS EM ALUNOS POR ESCOLARIDADE ##################### 
notif_geral_alunos_desc <- subset(notif_geral_descartado, notif_geral_descartado$ALUNO_PROF %in%
                                    c("Aluno", "Aluno do ensino infantil",
                                      "Aluno do ensino fundamental",
                                      "Aluno do ensino médio",
                                      "Aluno de idiomas",
                                      "Outros alunos (universidade; escola para adultos; por exemplo)"))



notif_geral_alunos_desc$ESCOLARIDADE <- ifelse(notif_geral_alunos_desc$ALUNO_PROF == "Aluno do ensino infantil", "Ensino Infantil",
                                        ifelse(notif_geral_alunos_desc$ALUNO_PROF == "Aluno do ensino fundamental","Ensino Fundamental", 
                                        ifelse(notif_geral_alunos_desc$ALUNO_PROF == "Aluno do ensino médio" ,"Ensino Médio", 
                                        ifelse(notif_geral_alunos_desc$ALUNO_PROF == "Outros alunos (universidade; escola para adultos; por exemplo)", "Outros alunos",
                                        ifelse(notif_geral_alunos_desc$ALUNO_PROF == "Aluno de idiomas", "Aluno de idiomas", NA)))))

notif_geral_alunos_desc$CASOS <- 1

alunos_desc_nivel_escolaridade <- notif_geral_alunos_desc %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(alunos_desc_nivel_escolaridade, "alunos_desc_nivel_escolaridade.csv", row.names = F)

ggplot(alunos_desc_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 100))

###### Nº DE DESCARTADOS POR ESCOLARIDADE EM ALUNOS #######    **** Dado importante!!! *****
descartados_idade_pura_alunos <- notif_geral_alunos_desc %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))


descartados_infantil <- subset(alunos_desc_nivel_escolaridade, alunos_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Infantil")
sum(descartados_infantil$CASOS)

descartados_fudamental <- subset(alunos_desc_nivel_escolaridade, alunos_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Fundamental")
sum(descartados_fudamental$CASOS)

descartados_medio <- subset(alunos_desc_nivel_escolaridade, alunos_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Médio")
sum(descartados_medio$CASOS)


#Dados em CSV
write.csv(descartados_idade_pura_alunos, "descartados_idade_pura_alunos.csv", row.names = F)

ggplot(descartados_idade_pura_alunos, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados") +
  labs(fill='Nível de Ensino') 




############ CASOS DESCARTADOS EM PROFESSORES POR ESCOLARIDADE ##################### 
notif_geral_prof_desc <- subset(notif_geral_descartado, notif_geral_descartado$ALUNO_PROF %in%
                                  c("Professor ou auxiliar de sala", 
                                    "Professor ou auxiliar de sala do ensino infantil",
                                    "Professor do ensino fundamental",
                                    "Professor do ensino médio",
                                    "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                    "Professor do ensino fundamental e médio",
                                    "Outros professores (como de universidade; escola para adultos)"))



notif_geral_prof_desc$ESCOLARIDADE <- ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor ou auxiliar de sala do ensino infantil", "Ensino Infantil",
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor do ensino fundamental","Ensino Fundamental", 
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor do ensino médio" ,"Ensino Médio",
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor ou auxiliar de sala", "Ignorado",
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor ou auxiliar de sala de ensino infantil ou fundamental", "Ignorado",
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Professor do ensino fundamental e médio", "Ignorado",
                                      ifelse(notif_geral_prof_desc$ALUNO_PROF == "Outros professores (como de universidade; escola para adultos)", "Outros Professores", NA))))))) 

notif_geral_prof_desc$CASOS <- 1

prof_desc_nivel_escolaridade <- notif_geral_prof_desc %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(prof_desc_nivel_escolaridade, "prof_desc_nivel_escolaridade.csv", row.names = F)

ggplot(prof_desc_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 25))

###### Nº DE CASOS DESCARTADOS POR ESCOLARIDADE EM PROFESSORES #######   **** Dado importante!!! *****
descartados_idade_pura_prof <- notif_geral_prof_desc %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))


descartados_infantil_prof <- subset(prof_desc_nivel_escolaridade, prof_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Infantil")
sum(descartados_infantil_prof$CASOS)

descartados_fudamental_prof <- subset(prof_desc_nivel_escolaridade, prof_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Fundamental")
sum(descartados_fudamental_prof$CASOS)

descartados_medio_prof <- subset(prof_desc_nivel_escolaridade, prof_desc_nivel_escolaridade$ESCOLARIDADE == "Ensino Médio")
sum(descartados_medio_prof$CASOS)

descartados_outros_prof <- subset(prof_desc_nivel_escolaridade, prof_desc_nivel_escolaridade$ESCOLARIDADE == "Outros Professores")
sum(descartados_outros_prof$CASOS)

descartados_ignorado_prof <- subset(prof_desc_nivel_escolaridade, prof_desc_nivel_escolaridade$ESCOLARIDADE == "Ignorado")
sum(descartados_ignorado_prof$CASOS)

#Dados em CSV
write.csv(descartados_idade_pura_prof, "descartados_idade_pura_prof.csv", row.names = F)

ggplot(descartados_idade_pura_prof, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados") +
  labs(fill='Nível de Ensino') 



################### SUSPEITOS POR NÍVEL DE ENSINO ######################## 
notif_geral_suspeito <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% 
                                 c ("Suspeito", "Suspeito e recusou exame"))


############ CASOS SUSPEITOS EM ALUNOS POR ESCOLARIDADE ##################### 
notif_geral_alunos_susp <- subset(notif_geral_suspeito, notif_geral_suspeito$ALUNO_PROF %in%
                                    c("Aluno", "Aluno do ensino infantil",
                                      "Aluno do ensino fundamental",
                                      "Aluno do ensino médio",
                                      "Aluno de idiomas",
                                      "Outros alunos (universidade; escola para adultos; por exemplo)"))



notif_geral_alunos_susp$ESCOLARIDADE <- ifelse(notif_geral_alunos_susp$ALUNO_PROF == "Aluno do ensino infantil", "Ensino Infantil",
                                        ifelse(notif_geral_alunos_susp$ALUNO_PROF == "Aluno do ensino fundamental","Ensino Fundamental", 
                                        ifelse(notif_geral_alunos_susp$ALUNO_PROF == "Aluno do ensino médio" ,"Ensino Médio", NA)))

notif_geral_alunos_susp$CASOS <- 1

alunos_susp_nivel_escolaridade <- notif_geral_alunos_susp %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(alunos_susp_nivel_escolaridade, "alunos_susp_nivel_escolaridade.csv", row.names = F)

ggplot(alunos_susp_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 100))

###### Nº DE SUSPEITOS POR ESCOLARIDADE EM ALUNOS #######    **** Dado importante!!! *****
suspeito_idade_pura_alunos <- notif_geral_alunos_susp %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(suspeito_idade_pura_alunos, "suspeito_idade_pura_alunos.csv", row.names = F)

ggplot(suspeito_idade_pura_alunos, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos") +
  labs(fill='Nível de Ensino') 




############ CASOS SUSPEITOS EM PROFESSORES POR ESCOLARIDADE ##################### 
notif_geral_prof_susp <- subset(notif_geral_suspeito, notif_geral_suspeito$ALUNO_PROF %in%
                                  c("Professor ou auxiliar de sala", 
                                    "Professor ou auxiliar de sala do ensino infantil",
                                    "Professor do ensino fundamental",
                                    "Professor do ensino médio",
                                    "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                    "Professor do ensino fundamental e médio",
                                    "Outros professores (como de universidade; escola para adultos)"))



notif_geral_prof_susp$ESCOLARIDADE <- ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor ou auxiliar de sala do ensino infantil", "Ensino Infantil",
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor do ensino fundamental","Ensino Fundamental", 
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor do ensino médio" ,"Ensino Médio",
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor ou auxiliar de sala", "Ignorado",
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor ou auxiliar de sala de ensino infantil ou fundamental", "Ignorado",
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Professor do ensino fundamental e médio", "Ignorado",
                                     ifelse(notif_geral_prof_susp$ALUNO_PROF == "Outros professores (como de universidade; escola para adultos)", "Outros Professores", NA))))))) 

notif_geral_prof_susp$CASOS <- 1

prof_susp_nivel_escolaridade <- notif_geral_prof_susp %>%
  group_by(ESCOLA,ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#Dados em CSV
write.csv(prof_susp_nivel_escolaridade, "prof_susp_nivel_escolaridade.csv", row.names = F)

ggplot(prof_susp_nivel_escolaridade, aes(x= ESCOLA, y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 25))

###### Nº DE CASOS SUSPEITOS POR ESCOLARIDADE EM PROFESSORES #######   **** Dado importante!!! *****
suspeito_idade_pura_prof <- notif_geral_prof_susp %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(suspeito_idade_pura_prof, "suspeito_idade_pura_prof.csv", row.names = F)

ggplot(suspeito_idade_pura_prof, aes(x= reorder(ESCOLARIDADE,CASOS), y= CASOS, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos") +
  labs(fill='Nível de Ensino') 



###### DATA FRAME ########

placar_nivel_escolaridade <- data.frame("Nível de Ensino" = c('INFANTIL', 'FUNDAMENTAL', 'MEDIO', 'OUTROS', 'IGNORADOS'),
                                        
                                        "ALUNOS CONFIRMADOS" = c (sum(casos_infantil$CASOS), sum(casos_fudamental$CASOS),
                                                                  sum(casos_medio$CASOS), 0 , 0),
                                        
                                        "ALUNOS DESCARTADOS" = c (sum(descartados_infantil$CASOS), sum(descartados_fudamental$CASOS),
                                                                  sum(descartados_medio$CASOS) , 0 , 0), 
                                        
                                        "PROF CONFIRMADOS" = c (sum(casos_infantil_prof$CASOS),sum(casos_fudamental_prof$CASOS), 
                                                                sum(casos_medio_prof$CASOS),  sum(casos_outros_prof$CASOS) , sum(casos_ignorado_prof$CASOS)), 
                                        
                                        "PROF DESCARTADOS" = c(sum(descartados_infantil_prof$CASOS), sum(descartados_fudamental_prof$CASOS),
                                                               sum(descartados_medio_prof$CASOS), sum(descartados_outros_prof$CASOS) ,  sum(descartados_ignorado_prof$CASOS))) 


#Dados em CSV
write.csv(placar_nivel_escolaridade, "placar_nivel_escolaridade.csv", row.names = F)




###########################  SURTOS ######################################


notif_escola <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `É surto?`,
                                                 `Escola pública ou privada?`,
                                                 `Idade da Pessoa* fórmula`) 


names(notif_escola) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'INSTITUICAO','IDADE')
notif_escola$DT_PRIM_SINTOM <- as.Date(notif_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
notif_escola <- notif_escola[!is.na(notif_escola$ESCOLA),]
notif_escola <- notif_escola [!is.na(notif_escola $DIAGNOSTICO),]
notif_escola  <- notif_escola [!is.na(notif_escola $ALUNO_PROF),]


notif_escola  <- subset(notif_escola , notif_escola $DIAGNOSTICO %in% 
                          c("Confirmado visto laudo",
                            "Suspeito", "Suspeito e recusou exame", 
                            "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))


com_dados_sobre_data <- sum(!is.na(notif_escola$DT_PRIM_SINTOM))
com_dados_sobre_data
sem_dados_sobre_data <- sum(is.na(notif_escola$DT_PRIM_SINTOM))
sem_dados_sobre_data
sem_dados_sobre_data/(com_dados_sobre_data+sem_dados_sobre_data)

# Calculo de confirmados por escola (LISTA DE ESCOLAS COM RESPECTIVO Nº DE CASOS CONFIRMARDOS)
notif_escola_conf <- subset(notif_escola, notif_escola$DIAGNOSTICO == 
                            "Confirmado visto laudo")
notif_escola_conf$CASO <- 1
casos_escola <- notif_escola_conf %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

# DADOS EM CSV 
write.csv(casos_escola, "casos_escola.csv", row.names = F)
write.csv(notif_escola, "notif_escola.csv", row.names = F)

# calculo de total por escola (LISTA DE ESCOLAS COM RESPECTIVAS NOTIFICAÇÕES)
notif_escola_tot <- notif_escola
notif_escola_tot$CASO <- 1
casos_escola_tot <- notif_escola_tot %>%
  group_by(ESCOLA)%>%
  summarise(TOT_ESCOLA = sum(CASO, na.rm = T)) %>%
  arrange(TOT_ESCOLA)


# Merge dos confirmados com o total (TAXA DE POSITIVIDADE POR ESCOLA)
notif_escola_final <- merge(casos_escola, casos_escola_tot,by = 'ESCOLA', all = T )
notif_escola_final$TAXA_POSITIVOS <- notif_escola_final$CASOS_ESCOLA/notif_escola_final$TOT_ESCOLA * 100

# Taxa total de confirmados por escola
sum(notif_escola_final$CASOS_ESCOLA, na.rm = T) / sum(notif_escola_final$TOT_ESCOLA, na.rm = T) * 100

ggplot(notif_escola_final, aes(x = reorder(ESCOLA, TOT_ESCOLA), y = TOT_ESCOLA, fill=ESCOLA)) + 
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de Suspeitos")

# DADOS EM CSV
write.csv(notif_escola_final, "notif_escola_final.csv", row.names = F)

casos_escola_plot <- notif_escola_final[!is.na(notif_escola_final$CASOS_ESCOLA),]

# DADOS EM CSV
write.csv(casos_escola_plot, "casos_escola_plot.csv", row.names = F)

ggplot(casos_escola_plot, aes(x = reorder(ESCOLA, CASOS_ESCOLA), y = CASOS_ESCOLA, fill=ESCOLA)) + 
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de casos")


#Série histórica de suspeitos - Todas as Escolas 
serie_hist_susp <- notif_escola
serie_hist_susp$QUANTIDADE <- 1
serie_hist_susp <- serie_hist_susp %>%
  group_by(DT_PRIM_SINTOM) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))

"Média móvel de 14 dias" <- "red"
serie_hist_susp$MM_14 <- rollmean(serie_hist_susp$QUANTIDADE,14, align = "right",fill = NA)
ggplot(serie_hist_susp, aes(x = DT_PRIM_SINTOM))+
  geom_line(aes(y = QUANTIDADE))+
  geom_line(aes(y = MM_14, color = "Média móvel de 14 dias"))+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de suspeitos")+
  scale_color_discrete(name = " ")


#Série histórica de suspeitos - Por Escola
serie_hist_escolas_susp <- notif_escola
serie_hist_escolas_susp$QUANTIDADE <- 1
serie_hist_escolas_susp <- serie_hist_escolas_susp %>%
  group_by(DT_PRIM_SINTOM, ESCOLA) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))
ggplot(serie_hist_escolas_susp, aes(DT_PRIM_SINTOM, QUANTIDADE, group = ESCOLA))+
  geom_line()+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de Suspeitos")


#Série histórica de casos - Todas as Escolas
serie_hist_casos <- subset(notif_escola, notif_escola$DIAGNOSTICO == "Confirmado visto laudo")
serie_hist_casos$QUANTIDADE <- 1
serie_hist_casos <- serie_hist_casos %>%
  group_by(DT_PRIM_SINTOM) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))


"Média móvel de 14 dias" <- "red"
serie_hist_casos$MM_14 <- rollmean(serie_hist_casos$QUANTIDADE,14, align = "right",fill = NA)
ggplot(serie_hist_casos, aes(x = DT_PRIM_SINTOM))+
  geom_line(aes(y = QUANTIDADE))+
  geom_line(aes(y = MM_14, color = "Média móvel de 14 dias"))+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de casos")+
  scale_color_discrete(name = " ")


#Série histórica de casos - Por Escola
serie_hist_escolas_casos <- subset(notif_escola, notif_escola$DIAGNOSTICO == "Confirmado visto laudo")
serie_hist_escolas_casos$QUANTIDADE <- 1 #Inclui o 1 para poder somar o número de casos
serie_hist_escolas_casos <- serie_hist_escolas_casos %>%
  group_by(DT_PRIM_SINTOM, ESCOLA) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))
ggplot(serie_hist_escolas_casos, aes(DT_PRIM_SINTOM, QUANTIDADE, group = ESCOLA))+
  geom_line()+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de Casos")


######################  ANALISE SURTOS  ##################################


###### TOTAL DE SURTOS ######## 
surtos_tot <- subset(notif_escola_conf, notif_escola_conf$SURTO == "Sim")
surtos_sum <- surtos_tot %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
sum (surtos_sum$CASOS_ESCOLA)
surtos_sum$CASO <-1
sum (surtos_sum$CASO)


################ ANALISE SURTOS POR TIPO DE INSTITUICAO (ALUNO/PROF/OUTROS) ############

# ###########  SURTOS EM ESCOLAS PRIVADAS  #############
surtos_privada <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Privada")
tot_surtos_privada <- surtos_privada %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_privada$CASO <- 1
sum (tot_surtos_privada$CASOS_ESCOLA)
sum (tot_surtos_privada$CASO)


##############  SURTOS EM ESCOLAS PUBLICAS MUNICIPAIS ############
surtos_publica_munic <- subset(surtos_tot, surtos_tot$INSTITUICAO %in% c("Pública Municipal", "Conveniada com PMF"))
tot_surtos_munic <- surtos_publica_munic %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_munic$CASO <- 1
sum(tot_surtos_munic$CASOS_ESCOLA)
sum (tot_surtos_munic$CASO)


##################  SURTOS EM ESCOLAS ESTADUAIS ########## 
surtos_estadual <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Pública Estadual")
tot_surtos_estadual <- surtos_estadual %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_estadual$CASO <- 1
sum(tot_surtos_estadual$CASOS_ESCOLA)
sum (tot_surtos_estadual$CASO)

############ SURTOS EM ESCOLAS FEDERAIS ################

surtos_federal <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Pública Federal")
tot_surtos_federal <- surtos_federal %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_federal$CASO <- 1
sum (tot_surtos_federal$CASOS_ESCOLA)
sum (tot_surtos_federal$CASO)


############ SURTOS EM ESCOLAS FILANTROPICAS ################

surtos_filantropica <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Filantropica")
tot_surtos_filantropica <- surtos_filantropica %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_filantropica$CASO <- 1
sum (tot_surtos_filantropica$CASOS_ESCOLA)
sum (tot_surtos_filantropica$CASO)


############      ANALISE SURTOS POR INSTITUICAO ######################

privada <- sum(tot_surtos_privada$CASO)
municipal <- sum(tot_surtos_munic$CASO)
estadual <- sum(tot_surtos_estadual$CASO)
federal <- sum (tot_surtos_federal$CASO)
filantropica <- sum(tot_surtos_filantropica$CASO) 

total_surtos_tipo_escola <- data.frame("Privada" = privada,
                                       "Publica Municipal" = municipal,
                                       "Publica Estadual" = estadual,
                                       "Publica Federal" = federal,
                                       "Filantropica" = filantropica)

total_surtos_tipo_escola <- melt(total_surtos_tipo_escola)
names(total_surtos_tipo_escola) <- c("Tipo", "Nº Escolas")


################   ANALISE SURTOS POR CASOS ###############

privada <- sum(tot_surtos_privada$CASOS_ESCOLA)
municipal <- sum(tot_surtos_munic$CASOS_ESCOLA)
estadual <- sum(tot_surtos_estadual$CASOS_ESCOLA)
federal <- sum(tot_surtos_federal$CASOS_ESCOLA)
filantropica <-sum(tot_surtos_filantropica$CASOS_ESCOLA)

casos_surtos_tipo_escola <- data.frame("Privada" = privada,
                                       "Publica Municipal" = municipal,
                                       "Publica Estadual" = estadual,
                                       "Publica Federal" = federal,
                                       "Filantropica" = filantropica)

casos_surtos_tipo_escola <- melt(casos_surtos_tipo_escola)
names(casos_surtos_tipo_escola) <- c("Tipo", "Casos")


########### ANALISE COMPLETA CASOS POR TIPO DE INSTITUICAO ###########
base_unificada <- merge(casos_surtos_tipo_escola, total_surtos_tipo_escola, by = "Tipo", all = T)
sum(base_unificada$Casos)

######### SURTOS POR NÍVEL DE ENSINO (SOMENTE ALUNOS) ########### 
surtos_tot$CATEGORIA <- ifelse(surtos_tot$ALUNO_PROF %in% 
                                 c("Professor ou auxiliar de sala", 
                                   "Professor ou auxiliar de sala do ensino infantil",
                                   "Professor do ensino fundamental",
                                   "Professor do ensino médio",
                                   "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                   "Professor do ensino fundamental e médio",
                                   "Outros professores (como de universidade; escola para adultos)"),
                                   "Professor ou auxiliar de sala",
                               
                               ifelse(surtos_tot$ALUNO_PROF %in% 
                                        c("Aluno", "Aluno do ensino infantil",
                                          "Aluno do ensino fundamental",
                                          "Aluno do ensino médio",
                                          "Aluno de idiomas",
                                          "Outros alunos (universidade; escola para adultos; por exemplo)"),
                                          "Aluno",
                                      
                                      ifelse(surtos_tot$ALUNO_PROF %in% 
                                      c("Outros colaboradores"), "Outros colaboradores", NA)))




surtos_nivel_ensino <- subset(surtos_tot, surtos_tot$CATEGORIA == "Aluno") 



surtos_nivel_ensino$ESCOLARIDADE <- ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino infantil", "Ensino Infantil",
                                    ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino fundamental","Ensino Fundamental", 
                                    ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino médio" ,"Ensino Médio", NA)))


surtos_idade_escola <- surtos_nivel_ensino %>%
  group_by(ESCOLA,ESCOLARIDADE, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

#Grafico surto escola por nível de ensino ALUNOS
ggplot(surtos_idade_escola, aes(x= ESCOLA, y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino') +
  scale_y_continuous(limits=c(0, 40))

surtos_idade_pura <- surtos_idade_escola %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))
sum(surtos_idade_escola$CASO)

#Grafico casos por nível de ensino
ggplot(surtos_idade_pura, aes(x= reorder(ESCOLARIDADE,CASO), y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 

######## Valores por tipo de instituição ############ 
surtos_alunos_privada <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Privada")
sum(surtos_alunos_privada$CASO)

surtos_alunos_municipal <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Pública Municipal")
sum(surtos_alunos_municipal$CASO)

surtos_alunos_estadual <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Pública Estadual")
sum(surtos_alunos_estadual$CASO)

surtos_alunos_federal <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Pública Federal")
sum(surtos_alunos_federal$CASO)

surtos_alunos_filantropica <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Filantropica")
sum(surtos_alunos_filantropica$CASO)


########## Tabela nivel de ensino ############
surtos_idade_escola$CASOS <-1
surtos_infantil <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Infantil")
sum (surtos_infantil$CASO)
surtos_fundamental <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Fundamental")
sum (surtos_fundamental$CASO)
surtos_medio <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Médio")
sum (surtos_medio$CASO)

############# SURTOS NÍVEL DE ENSINO - PROFESSORES #####################
surtos_nivel_prof <- subset(surtos_tot, surtos_tot$ALUNO_PROF 
                            %in% c("Professor ou auxiliar de sala", 
                                   "Professor ou auxiliar de sala do ensino infantil",
                                   "Professor do ensino fundamental",
                                   "Professor do ensino médio",
                                   "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                   "Professor do ensino fundamental e médio",
                                   "Outros professores (como de universidade; escola para adultos)"))



surtos_nivel_prof$ESCOLARIDADE <- ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala do ensino infantil", "Ensino Infantil",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino fundamental","Ensino Fundamental", 
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino médio" ,"Ensino Médio",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala de ensino infantil ou fundamental", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino fundamental e médio", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Outros professores (como de universidade; escola para adultos)", "Outros Professores", NA))))))) 


surtos_idade_escola_prof <- surtos_nivel_prof %>%
  group_by(ESCOLA,ESCOLARIDADE, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

#Grafico surto escola por nível de ensino
ggplot(surtos_idade_escola_prof, aes(x= ESCOLA, y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino') +
  scale_y_continuous(limits=c(0, 60))

surtos_idade_pura_prof <- surtos_idade_escola_prof %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))
sum(surtos_idade_escola_prof$CASO)

#Grafico casos por nível de ensino professor
ggplot(surtos_idade_pura_prof, aes(x= reorder(ESCOLARIDADE,CASO), y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 


###### Tabela nivel ensino prof ########

surtos_infantil_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Infantil")
sum (surtos_infantil_prof$CASO)
surtos_fundamental_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Fundamental")
sum (surtos_fundamental_prof$CASO)
surtos_medio_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Médio")
sum (surtos_medio_prof$CASO)
surtos_outros_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Outros Professores")
sum (surtos_outros_prof$CASO)
surtos_ignorado_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ignorado")
sum (surtos_ignorado_prof$CASO)

#SURTOS EM PROFESSORES
surtos_tot$CATEGORIA <- ifelse(surtos_tot$ALUNO_PROF %in% c("Professor ou auxiliar de sala", 
                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                            "Professor do ensino fundamental",
                                                            "Professor do ensino médio",
                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                            "Professor do ensino fundamental e médio",
                                                            "Outros professores (como de universidade; escola para adultos)"),
                                                           "Professor ou auxiliar de sala",
                         ifelse(surtos_tot$ALUNO_PROF %in% c("Aluno", "Aluno do ensino infantil",
                                                            "Aluno do ensino fundamental",
                                                            "Aluno do ensino médio",
                                                            "Aluno de idiomas",
                                                            "Outros alunos (universidade; escola para adultos; por exemplo)"), 
                                                            "Aluno",
                         ifelse(surtos_tot$ALUNO_PROF %in% c("Outros colaboradores"), 
                                                            "Outros colaboradores", NA)))



surtos_prof <- subset(surtos_tot, surtos_tot$CATEGORIA == "Professor ou auxiliar de sala")

surtos_prof_instituicao <- surtos_prof %>%
  group_by(ESCOLA,CATEGORIA, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

sum(surtos_prof_instituicao$CASO)

surtos_prof_privada <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Privada")
sum(surtos_prof_privada$CASO)

surtos_prof_municipal <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Pública Municipal")
sum(surtos_prof_municipal$CASO)

surtos_prof_estadual <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Pública Estadual")
sum(surtos_prof_estadual$CASO)

surtos_prof_federal <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Pública Federal")
sum(surtos_prof_federal$CASO)

surtos_prof_filantropica <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Filantropica")
sum(surtos_prof_filantropica$CASO)

############## #SURTOS OUTROS COLABORADORES ################
surtos_outros_colab <- subset(surtos_tot, surtos_tot$CATEGORIA == "Outros colaboradores")
surtos_outros_instituicao <- surtos_outros_colab  %>%
  group_by(ESCOLA,CATEGORIA, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

sum(surtos_outros_colab$CASO)

surtos_outros_privada <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Privada")
sum(surtos_outros_privada$CASO)

surtos_outros_municipal <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Pública Municipal")
sum(surtos_outros_municipal$CASO)

surtos_outros_estadual <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Pública Estadual")
sum(surtos_outros_estadual$CASO)

surtos_outros_federal<- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Pública Federal")
sum(surtos_outros_federal$CASO)

surtos_outros_filantropica <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Filantropica")
sum(surtos_outros_filantropica$CASO)


#########################   ANALISE DE SURTOS ATIVOS ##############################################


monitora_escola_ativo <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                          
                                                          `Nome da escola`,
                                                          `Data dos primeiros sintomas:`,
                                                          `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                          `É surto?`,
                                                          `SURTO ENCERRADO? *fórmula`,
                                                          `CASO FINALIZADO? *fórmula`,
                                                          `Idade da Pessoa* fórmula`,
                                                          `Escola pública ou privada?`) 

names(monitora_escola_ativo) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'IDADE', 'INSTITUICAO')

monitora_escola_ativo$DT_PRIM_SINTOM <- as.Date(monitora_escola_ativo$DT_PRIM_SINTOM, format = '%d/%m/%Y')
monitora_escola_ativo <- subset(monitora_escola_ativo, monitora_escola_ativo$DIAGNOSTICO == "Confirmado visto laudo")

#Mantido os casos finalizados para analise do surto
notif_escola_surto <- subset(monitora_escola_ativo, is.na(monitora_escola_ativo$SURTO_FINALZ))

#NUMERO DE SURTOS ATIVOS 
notif_escola_suativo <- subset(notif_escola_surto, notif_escola_surto$SURTO == "Sim")

notif_escola_suativo$CASO <- 1
surtos_escola <- notif_escola_suativo %>%
  group_by(ESCOLA, INSTITUICAO)%>%
  summarise(SURTOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(SURTOS_ESCOLA))
surtos_escola$CASO <- 1
sum(surtos_escola$CASO)


# DADOS EM CSV 
write.csv(surtos_escola, "surtos_escola.csv", row.names = F)

#Surtos ATIVOS por Instituição
suativo_priv <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Privada")
sum(suativo_priv$CASO)

suativo_munic <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Municipal")
sum (suativo_munic$CASO)

suativo_estadual <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Estadual")
sum(suativo_estadual$CASO)

suativo_federal <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Federal")
sum(suativo_federal$CASO)

suativo_filantropica <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Filantropica")
sum(suativo_filantropica$CASO)

#Grafico Escola com Surtos ATIVOS x Nº de Casos Confirmados
ggplot(surtos_escola, aes(x= reorder(ESCOLA, SURTOS_ESCOLA), y= SURTOS_ESCOLA, fill=ESCOLA))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de casos")



########################  MEU DATA FRAME DE SURTOS ###############################################

placar_surtos <- data.frame(Escolas = c('PRIVADAS', 'MUNICIPAIS', 'ESTADUAIS', 'FEDERAIS', 'FILANTROPICAS', 'TOTAL'),
                            
                            "Total Surtos/Escola" = 
                              c(sum (tot_surtos_privada$CASO),sum (tot_surtos_munic$CASO),
                                sum (tot_surtos_estadual$CASO), sum (tot_surtos_federal$CASO), 
                                filantropica <- sum(tot_surtos_filantropica$CASO), sum (surtos_sum$CASO)),  
                            
                            
                            "Total de Casos" = 
                              c(sum (tot_surtos_privada$CASOS_ESCOLA), sum(tot_surtos_munic$CASOS_ESCOLA),
                                sum(tot_surtos_estadual$CASOS_ESCOLA), sum(tot_surtos_federal$CASOS_ESCOLA),
                                sum(tot_surtos_filantropica$CASOS_ESCOLA), sum(base_unificada$Casos)),
                            
                            "Alunos" = 
                              c(sum(surtos_alunos_privada$CASO), sum(surtos_alunos_municipal$CASO),
                                sum(surtos_alunos_estadual$CASO), sum(surtos_alunos_federal$CASO),
                                sum(surtos_alunos_filantropica$CASO), sum(surtos_idade_escola$CASO)), 
                            
                            "Professores" = 
                              c(sum(surtos_prof_privada$CASO), sum(surtos_prof_municipal$CASO),
                                sum(surtos_prof_estadual$CASO), sum(surtos_prof_federal$CASO), 
                                sum(surtos_prof_filantropica$CASO), sum(surtos_prof_instituicao$CASO)) ,
                            
                            
                            "Outros Colaboradores" = 
                              c(sum(surtos_outros_privada$CASO), sum(surtos_outros_municipal$CASO),
                                sum(surtos_outros_estadual$CASO), sum(surtos_outros_federal$CASO), 
                                sum(surtos_outros_filantropica$CASO),sum(surtos_outros_colab$CASO)),
                            
                            "Surtos Ativos/Escola" = 
                              c (sum(suativo_priv$CASO), sum (suativo_munic$CASO),
                                 sum(suativo_estadual$CASO), sum (suativo_federal$CASO), 
                                 sum (suativo_filantropica$CASO), sum(surtos_escola$CASO)))

# DADOS EM CSV 
write.csv(placar_surtos, "placar_surtos.csv", row.names = F)


############ DATA FRAME POR NIVEL DE ENSINO ####################### 
placar_surtos_escolaridade <- data.frame("Nível de Ensino" = c('INFANTIL', 'FUNDAMENTAL', 'MEDIO', 'OUTROS', 'IGNORADOS'),
                                         "Alunos" = c(sum (surtos_infantil$CASO),sum (surtos_fundamental$CASO),
                                                      sum (surtos_medio$CASO), 0, 0),
                                         "Professores"= c(sum (surtos_infantil_prof$CASO), sum (surtos_fundamental_prof$CASO), 
                                                          sum (surtos_medio_prof$CASO), sum (surtos_outros_prof$CASO),
                                                          sum (surtos_ignorado_prof$CASO)))


# DADOS EM CSV 
write.csv(placar_surtos_escolaridade, "placar_surtos_escolaridade.csv", row.names = F)



            ################## MONITORAMENTO ########################


monitora_escola <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                    `Nome da escola`,
                                                    `Data dos primeiros sintomas:`,
                                                    `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                    `É surto?`,
                                                    `SURTO ENCERRADO? *fórmula`,
                                                    `CASO FINALIZADO? *fórmula`,
                                                    `Escola pública ou privada?`)
names(monitora_escola) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'INSTITUICAO')
monitora_escola$DT_PRIM_SINTOM <- as.Date(monitora_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
monitora_escola <- monitora_escola[!is.na(monitora_escola$ESCOLA),]
monitora_escola <- monitora_escola[!is.na(monitora_escola$DIAGNOSTICO),]
monitora_escola <- monitora_escola[!is.na(monitora_escola$ALUNO_PROF),]


monitora_escola <- subset(monitora_escola, monitora_escola$DIAGNOSTICO %in% 
                            c("Confirmado visto laudo",
                              "Suspeito", "Suspeito e recusou exame", 
                              "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))


# ESCOLAS AFETADAS ATUALMENTE (confirmados e suspeitos)
monitora_escola_atual <- subset(monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Confirmado visto laudo", "Suspeito", "Suspeito e recusou fazer teste"))
monitora_escola_atual <- subset(monitora_escola, is.na(monitora_escola$CASO_FINALZ))
monitora_escola_atual$CASOS <- 1 
sum(monitora_escola_atual$CASOS)

monitora_escola_afetadas <- monitora_escola_atual %>%
  group_by(ESCOLA,INSTITUICAO, CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

# DADOS EM CSV 
write.csv(monitora_escola_afetadas, "monitora_escola_afetadas.csv", row.names = F)


#CASOS CONFIRMADOS ATIVOS
notif_escola_hoje <- subset(monitora_escola, monitora_escola$DIAGNOSTICO == "Confirmado visto laudo")
notif_escola_hoje$CASOS <- 1 
notif_escola_hoje <- subset(notif_escola_hoje, is.na(notif_escola_hoje$CASO_FINALZ))
sum(notif_escola_hoje$CASOS)


notif_escola_conf_hoje <-notif_escola_hoje %>%
  group_by(ESCOLA,INSTITUICAO, CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

# DADOS EM CSV 
write.csv(notif_escola_conf_hoje, "notif_escola_conf_hoje.csv", row.names = F)


#CASOS CONFIRMADOS EM ALUNOS
notif_escola_hoje$CATEGORIA <- ifelse(notif_escola_hoje$ALUNO_PROF %in% c("Professor ou auxiliar de sala", 
                                                                          "Professor ou auxiliar de sala do ensino infantil",
                                                                          "Professor do ensino fundamental",
                                                                          "Professor do ensino médio",
                                                                          "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                          "Professor do ensino fundamental e médio",
                                                                          "Outros professores (como de universidade; escola para adultos)"),
                                      "Professor ou auxiliar de sala",
                                      ifelse(notif_escola_hoje$ALUNO_PROF %in% c("Aluno", "Aluno do ensino infantil",
                                                                                 "Aluno do ensino fundamental",
                                                                                 "Aluno do ensino médio",
                                                                                 "Aluno de idiomas",
                                                                                 "Outros alunos (universidade; escola para adultos; por exemplo)"),
                                             "Aluno",
                                             ifelse(notif_escola_hoje$ALUNO_PROF %in% c("Outros colaboradores"), 
                                                    "Outros colaboradores", NA)))





#ALUNOS
notif_alunos_hj <- subset(notif_escola_hoje, notif_escola_hoje$CATEGORIA == "Aluno")
sum(notif_alunos_hj$CASOS)

#CASOS EM PROFESSORES
notif_prof_hj <- subset(notif_escola_hoje, notif_escola_hoje$CATEGORIA == "Professor ou auxiliar de sala")
sum(notif_prof_hj$CASOS)

#CASOS EM OUTROS COLABORADORES
notif_outros_hj <- subset(notif_escola_hoje, notif_escola_hoje$CATEGORIA == "Outros colaboradores")
sum(notif_outros_hj$CASOS)


# CASOS SUSPEITOS NOTIFICADOS
notif_escola_susp_hoje <- subset (monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Suspeito", "Suspeito e recusou exame"))
notif_escola_susp_hoje$CASOS <- 1 
notif_escola_susp_hoje <- subset(notif_escola_susp_hoje, is.na(notif_escola_susp_hoje$CASO_FINALZ))
sum(notif_escola_susp_hoje$CASOS)

notif_escolacomsusp_hoje <-notif_escola_susp_hoje %>%
  group_by(ESCOLA,INSTITUICAO, CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

# DADOS EM CSV 
write.csv(notif_escolacomsusp_hoje, "notif_escolacomsusp_hoje.csv", row.names = F)


notif_escola_susp_hoje$CATEGORIA <- ifelse(notif_escola_susp_hoje$ALUNO_PROF %in% c("Professor ou auxiliar de sala", 
                                                                                    "Professor ou auxiliar de sala do ensino infantil",
                                                                                    "Professor do ensino fundamental",
                                                                                    "Professor do ensino médio",
                                                                                    "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                                    "Professor do ensino fundamental e médio",
                                                                                    "Outros professores (como de universidade; escola para adultos)"), 
                                           "Professor ou auxiliar de sala",
                                           ifelse(notif_escola_susp_hoje$ALUNO_PROF %in% c("Aluno", "Aluno do ensino infantil",
                                                                                           "Aluno do ensino fundamental",
                                                                                           "Aluno do ensino médio",
                                                                                           "Aluno de idiomas",
                                                                                           "Outros alunos (universidade; escola para adultos; por exemplo)"),
                                                  "Aluno",
                                                  ifelse(notif_escola_susp_hoje$ALUNO_PROF %in% c("Outros colaboradores"), 
                                                         "Outros colaboradores", NA)))




#SUSPEITOS EM ALUNOS
notifsusp_alunos_hj <- subset(notif_escola_susp_hoje, notif_escola_susp_hoje$CATEGORIA == "Aluno")
sum(notifsusp_alunos_hj$CASOS)

#SUSPEITOS EM PROFESSORES
notifsusp_prof_hj <- subset(notif_escola_susp_hoje, notif_escola_susp_hoje$CATEGORIA == "Professor ou auxiliar de sala")
sum(notifsusp_prof_hj$CASOS)

#SUSPEITOS EM OUTROS COLABORADORES
notifsusp_outros_hj <- subset(notif_escola_susp_hoje, notif_escola_susp_hoje$CATEGORIA == "Outros colaboradores")
sum(notifsusp_outros_hj$CASOS)


#MEU DATA FRAME
monitoramento_final <- data.frame(NOTIFICACOES = c('CONFIRMADO', 'SUSPEITO'),
                                  "Alunos" = c(sum(notif_alunos_hj$CASOS),sum(notifsusp_alunos_hj$CASOS)),
                                  
                                  
                                  "Professores" = c(sum(notif_prof_hj$CASOS), sum(notifsusp_prof_hj$CASOS)),
                                  
                                  
                                  "Outros Colaboradores" = c(sum(notif_outros_hj$CASOS), sum(notifsusp_outros_hj$CASOS)))



# DADOS EM CSV 
write.csv(monitoramento_final, "monitoramento_final.csv", row.names = F)




#################   ANALISE DE SURTOS ATIVOS #########################

monitora_escola_ativo <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                          
                                                          `Nome da escola`,
                                                          `Data dos primeiros sintomas:`,
                                                          `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                          `É surto?`,
                                                          `SURTO ENCERRADO? *fórmula`,
                                                          `CASO FINALIZADO? *fórmula`,
                                                          `Idade da Pessoa* fórmula`,
                                                          `Escola pública ou privada?`) 

names(monitora_escola_ativo) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'IDADE', 'INSTITUICAO')

monitora_escola_ativo$DT_PRIM_SINTOM <- as.Date(monitora_escola_ativo$DT_PRIM_SINTOM, format = '%d/%m/%Y')
monitora_escola_ativo <- subset(monitora_escola_ativo, monitora_escola_ativo$DIAGNOSTICO == "Confirmado visto laudo")

#Mantido os casos finalizados para analise do surto
notif_escola_surto <- subset(monitora_escola_ativo, is.na(monitora_escola_ativo$SURTO_FINALZ))

#NUMERO DE SURTOS ATIVOS 
notif_escola_suativo <- subset(notif_escola_surto, notif_escola_surto$SURTO == "Sim")

notif_escola_suativo$CASO <- 1
surtos_escola <- notif_escola_suativo %>%
  group_by(ESCOLA, INSTITUICAO)%>%
  summarise(SURTOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(SURTOS_ESCOLA))
surtos_escola$CASO <- 1
sum(surtos_escola$CASO)


# DADOS EM CSV 
write.csv(surtos_escola, "surtos_escola.csv", row.names = F)

#Surtos ATIVOS por Instituição
suativo_priv <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Privada")
sum(suativo_priv$CASO)

suativo_munic <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Municipal")
sum (suativo_munic$CASO)

suativo_estadual <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Estadual")
sum(suativo_estadual$CASO)

suativo_federal <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Pública Federal")
sum(suativo_federal$CASO)

suativo_filantropica <- subset(surtos_escola, surtos_escola$INSTITUICAO == "Filantropica")
sum(suativo_filantropica$CASO)



#Grafico Escola com Surtos x Nº de Casos Confirmados
ggplot(surtos_escola, aes(x= reorder(ESCOLA, SURTOS_ESCOLA), y= SURTOS_ESCOLA, fill=ESCOLA))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de casos")



