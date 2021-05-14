#23/03/2021 - ANALISE POR ESCOLARIDADE DE SUSPEITOS/CASOS CONFIRMADOS 
library(tidyverse)
library(dplyr)
source("CUIDADO_link_com_a_base.R")
library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")
View(Censo_2019)

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

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
                                 c("Confirmado", "Confirmado visto laudo",
                                   "Suspeito", "Suspeito e recusou exame", 
                                   "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))

############## ANALISE CASOS NOTIFICADOS POR ESCOLARIDADE ###################### 

              

     ######## ANALISE CASOS CONFIRMADOS POR ESCOLARIDADE (mantido casos finalizados) ##########

notif_geral_conf <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% 
                             c("Confirmado visto laudo","Confirmado"))


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
                                                 
                          
