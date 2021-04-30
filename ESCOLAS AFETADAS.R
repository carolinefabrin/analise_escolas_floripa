library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

Censo_2019 <- library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

analise_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
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

analise_escola_notif <- subset(analise_escola, analise_escola$DIAGNOSTICO %in% 
                                 c("Confirmado", "Confirmado visto laudo",
                                   "Suspeito", "Suspeito e recusou exame", 
                                   "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))
                                  
                            


              ###### ANALISE DE NOTIFICAÇÕES POR TIPO DE INSTITUICAO ######## 

                            
#####################################   ESCOLAS PRIVADAS   #########################################


# ESCOLAS COM NOTIFICAÇÕES
privadas_notificadas <- subset(analise_escola_notif, analise_escola_notif$INSTITUICAO == "Privada")
privadas_notificadas$CASOS <-1

#Lista de escolas com notificados
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
                               c("Aluno do ensino infantil",
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
                                    c("Professor ou auxiliar de sala do ensino infantil",
                                      "Professor do ensino fundamental",
                                      "Professor do ensino médio",
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
                                    c("Aluno do ensino infantil",
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
                                  c("Professor ou auxiliar de sala do ensino infantil",
                                    "Professor do ensino fundamental",
                                    "Professor do ensino médio",
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







###### DATA FRAME ########

placar_geral <- data.frame(Escolas = c('PRIVADAS', 'MUNICIPAIS', 'ESTADUAIS', 'FEDERAIS', 'TOTAL'),
                           
                            "ESCOLAS AFETADAS" = c (sum(privadas_notificadas_tot$CASO), sum (municipais_notificadas_tot$CASO),
                            
                            "TOTAL NOTIFICAÇÕES" = c(sum(privadas_notificadas_tot$CASOS_ESCOLA), sum(municipais_notificadas_tot$CASOS_ESCOLA),
                            
                            "ALUNOS NOTIFICADOS" = c (sum(alunos_notificados_priv_tot$CASOS_ESCOLA),sum(alunos_notificados_munic_tot$CASOS_ESCOLA)                     
                           
                            "ALUNOS CONFIRMADOS" = c(sum(alunos_confirmados_priv_tot$CASOS_ESCOLA), sum(alunos_confirmados_munic_tot$CASOS_ESCOLA),
                                                     
                            "PROFESSORES NOTIFICADOS" = c (sum(prof_notificados_priv_tot$CASOS_ESCOLA),                       
                            
                            "PROFESSORES CONFIRMADOS" = c(sum(prof_confirmados_priv_tot$CASOS_ESCOLA),  
                            
                           "OUTROS COLABORADORES NOTIF" = c(sum(outros_notificados_priv_tot$CASOS_ESCOLA),
                           
                           "OUTROS COLABORADORES CONF" - c(sum(outros_confirmados_priv_tot$CASOS_ESCOLA), 

                           
            