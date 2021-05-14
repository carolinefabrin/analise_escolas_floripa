library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

Censo_2019 <- library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")

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
                                 c("Confirmado", "Confirmado visto laudo",
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
                                    alunos_notificados_filantropicas$DIAGNOSTICO %in% 
                                    c("Confirmado visto laudo", "Confirmado"))


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
                                         prof_notificados_filantropicas$DIAGNOSTICO %in% 
                                        c("Confirmado visto laudo", "Confirmado"))

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
                                           filantropicas_notificadas$ALUNO_PROF %in% 
                                           c("Outros colaboradores"))


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
                                           outros_notificados_filantropicas$DIAGNOSTICO %in% 
                                            c("Confirmado visto laudo", "Confirmado"))

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
                                  alunos_notificados_priv$DIAGNOSTICO %in% 
                               c("Confirmado visto laudo", "Confirmado"))


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
prof_confirmados_priv <- subset(prof_notificados_priv, prof_notificados_priv$DIAGNOSTICO %in% 
                                    c("Confirmado visto laudo", "Confirmado"))

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


outros_notificados_priv <- subset(privadas_notificadas, privadas_notificadas$ALUNO_PROF %in% 
                                  c("Outros colaboradores"))


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
outros_confirmados_priv <- subset(outros_notificados_priv, outros_notificados_priv$DIAGNOSTICO %in% 
                                  c("Confirmado visto laudo", "Confirmado"))

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
                                  alunos_notificados_municipais$DIAGNOSTICO %in% 
                                    c("Confirmado visto laudo", "Confirmado"))


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
prof_confirmados_municipais <- subset(prof_notificados_municipais, prof_notificados_municipais$DIAGNOSTICO %in% 
                                  c("Confirmado visto laudo", "Confirmado"))

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


outros_notificados_municipais <- subset(municipais_notificadas, municipais_notificadas$ALUNO_PROF %in% 
                                    c("Outros colaboradores"))


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
outros_confirmados_municipais <- subset(outros_notificados_municipais, outros_notificados_municipais$DIAGNOSTICO %in% 
                                    c("Confirmado visto laudo", "Confirmado"))

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
                                        alunos_notificados_estaduais$DIAGNOSTICO %in% 
                                          c("Confirmado visto laudo", "Confirmado"))


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
prof_confirmados_estaduais <- subset(prof_notificados_estaduais, prof_notificados_estaduais$DIAGNOSTICO %in% 
                                        c("Confirmado visto laudo", "Confirmado"))

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


outros_notificados_estaduais <- subset(estaduais_notificadas, estaduais_notificadas$ALUNO_PROF %in% 
                                          c("Outros colaboradores"))


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
outros_confirmados_estaduais <- subset(outros_notificados_estaduais, outros_notificados_estaduais$DIAGNOSTICO %in% 
                                          c("Confirmado visto laudo", "Confirmado"))

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
                                       alunos_notificados_federais$DIAGNOSTICO %in% 
                                       c("Confirmado visto laudo", "Confirmado"))


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
prof_confirmados_federais <- subset(prof_notificados_federais, prof_notificados_federais$DIAGNOSTICO %in% 
                                       c("Confirmado visto laudo", "Confirmado"))

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


outros_notificados_federais <- subset(federais_notificadas, federais_notificadas$ALUNO_PROF %in% 
                                         c("Outros colaboradores"))


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
outros_confirmados_federais <- subset(outros_notificados_federais, outros_notificados_federais$DIAGNOSTICO %in% 
                                         c("Confirmado visto laudo", "Confirmado"))

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

placar_geral <- data.frame(Escolas = c('PRIVADAS', 'MUNICIPAIS', 'ESTADUAIS', 'FEDERAIS', 'FILANTROPICAS', 'TOTAL'),
                           
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
write.csv(placar_geral, "placar_geral.csv", row.names = F)

                    



