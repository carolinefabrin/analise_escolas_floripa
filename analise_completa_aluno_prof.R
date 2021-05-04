#26/03/2021 - Analise Completa (ativos e inativos) / ALuno, Prof, Outros colaboradores
library(tidyverse)
library(dplyr)
source("CUIDADO_link_com_a_base.R")
library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")
View(Censo_2019)


notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

analise_geral <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                 `Nome da escola`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `Idade da Pessoa* fórmula`)

names(analise_geral) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DIAGNOSTICO', 'IDADE')
analise_geral$TURMA <- ifelse(analise_geral$TURMA == 'NULL', NA, analise_geral$TURMA)
analise_geral <- analise_geral[!is.na(analise_geral$DIAGNOSTICO),]
analise_geral <- analise_geral[!is.na(analise_geral$ESCOLA),]
analise_geral <- analise_geral[!is.na(analise_geral$ALUNO_PROF),]

#Analise quantidade de casos aluno x prof x outros colaboradores
analise_geral_conf <- subset(analise_geral, analise_geral$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
analise_geral_conf$CASOS <- 1


analise_geral_conf$CATEGORIA <- ifelse(analise_geral_conf$ALUNO_PROF %in% c("Professor ou auxiliar de sala", 
                                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                                            "Professor do ensino fundamental",
                                                                            "Professor do ensino médio",
                                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                            "Professor do ensino fundamental e médio",
                                                                            "Outros professores (como de universidade; escola para adultos)"),
                                                                            "Professor ou auxiliar de sala",
                                ifelse(analise_geral_conf$ALUNO_PROF %in% c("Aluno", "Aluno do ensino infantil",
                                                                            "Aluno do ensino fundamental",
                                                                            "Aluno do ensino médio",
                                                                            "Aluno de idiomas",
                                                                            "Outros alunos (universidade; escola para adultos; por exemplo)"),
                                                                            "Aluno",
                               ifelse(analise_geral_conf$ALUNO_PROF %in% c("Outros colaboradores"), "Outros colaboradores", NA)))
                                                     
                                              
                                          

analise_geral_conf <- analise_geral_conf[!is.na(analise_geral_conf$CATEGORIA),]                                           
confirmados_geral <- analise_geral_conf %>%
  group_by(CATEGORIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(confirmados_geral, "confirmados_geral.csv", row.names = F)

ggplot(confirmados_geral, aes(x= reorder(CATEGORIA, CASOS), y= CASOS, fill= CATEGORIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  scale_y_continuous(limits=c(0, 260))  

#Analise quantidade de suspeitos aluno x prof x outros colaboradores
analise_geral_susp <- subset(analise_geral, analise_geral$DIAGNOSTICO %in% c("Suspeito","Suspeito e recusou exame"))
analise_geral_susp$CASOS <- 1


analise_geral_susp$CATEGORIA <- ifelse(analise_geral_susp$ALUNO_PROF %in% c("Professor ou auxiliar de sala",
                                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                                            "Professor do ensino fundamental e médio",
                                                                            "Professor do ensino médio",
                                                                            "Outros professores (como de universidade; escola para adultos",
                                                                            "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                       ifelse(analise_geral_susp$ALUNO_PROF %in% c("Aluno", "Outros alunos (universidade; escola para adultos; por exemplo)",
                                                                                   "Aluno do ensino infantil",
                                                                                   "Aluno do ensino fundamental",
                                                                                   "Aluno do ensino médio"), "Aluno",
                                              ifelse(analise_geral_susp$ALUNO_PROF %in% c("Outros colaboradores"), "Outros colaboradores", NA)))
                                                     
                                              
                                      

analise_geral_susp <- analise_geral_susp[!is.na(analise_geral_susp$CATEGORIA),]                                          
suspeitos_geral <- analise_geral_susp %>%
  group_by(CATEGORIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(suspeitos_geral, "suspeitos_geral.csv", row.names = F)

ggplot(suspeitos_geral, aes(x= reorder(CATEGORIA, CASOS), y= CASOS, fill= CATEGORIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos")+
  scale_y_continuous(limits=c(0, 80))  


#Analise quantidade de DESCARTADOS aluno x prof x outros colaboradores
analise_geral_descartados <- subset(analise_geral, analise_geral$DIAGNOSTICO %in% c("Descartado", "Descartado (suspeito que fez exame e foi negativo)"))
analise_geral_descartados$CASOS <- 1


analise_geral_descartados$CATEGORIA <- ifelse(analise_geral_descartados$ALUNO_PROF %in% c("Professor ou auxiliar de sala",
                                                                                          "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                                          "Professor ou auxiliar de sala do ensino infantil",
                                                                                          "Professor do ensino fundamental e médio",
                                                                                          "Professor do ensino médio",
                                                                                          "Outros professores (como de universidade; escola para adultos",
                                                                                          "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                        ifelse(analise_geral_descartados$ALUNO_PROF %in% c("Aluno", "Outros alunos (universidade; escola para adultos; por exemplo)",
                                                                                            "Aluno do ensino infantil",
                                                                                           "Aluno do ensino fundamental",
                                                                                            "Aluno do ensino médio"), "Aluno",
                                         ifelse(analise_geral_descartados$ALUNO_PROF %in% c("Outros colaboradores"),
                                                                                           "Outros colaboradores", NA)))
                                                        
                                                
                                          

analise_geral_descartados <- analise_geral_descartados[!is.na(analise_geral_descartados$CATEGORIA),]                                          
descartados_geral <- analise_geral_descartados %>%
  group_by(CATEGORIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(descartados_geral, "descartados_geral.csv", row.names = F)

ggplot(descartados_geral, aes(x= reorder(CATEGORIA, CASOS), y= CASOS, fill= CATEGORIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados")+
  scale_y_continuous(limits=c(0,120))  



#Calculo das Taxas de Positividade

#Taxa de Positividade em Alunos
alunos_positivos <- subset(analise_geral_conf, analise_geral_conf$CATEGORIA == "Aluno")
alunos_positivos$CASO <- 1
total_positivo_alunos <- sum(alunos_positivos$CASOS)

#Calculo do total de alunos suspeitos
alunos_suspeitos <-subset(analise_geral_susp, analise_geral_susp$CATEGORIA == "Aluno")
alunos_suspeitos$CASO <- 1
total_suspeitos_alunos <- sum(alunos_suspeitos$CASOS)

#Calculo do total de contato de caso
contatos_caso <-subset(analise_geral_contato, analise_geral_contato$CATEGORIA == "Aluno")
contatos_caso$CASO <- 1
total_contato_alunos <- sum(contatos_caso$CASOS)

# calculo de total de alunos descartados
alunos_descartados <- subset(analise_geral_descartados, analise_geral_descartados$CATEGORIA == "Aluno")
alunos_descartados$CASO <- 1
total_descartados_alunos <- sum(alunos_descartados$CASO)

# Taxa total de alunos confirmados
alunos_tot <- sum(total_positivo_alunos + total_suspeitos_alunos + total_contato_alunos
                  + total_descartados_alunos)
taxa_positividade_alunosnotif <- (total_positivo_alunos  / alunos_tot) * 100

#Taxa alunos afetados x matriculados CENSO 2019
Taxa_afetados <- (alunos_tot /93368) * 100

###########################################
#Taxa de Positividade em Professores
professores_positivos <- subset(analise_geral_conf, analise_geral_conf$CATEGORIA == "Professor ou auxiliar de sala")
professores_positivos$CASO <- 1
total_positivos_prof <- sum(professores_positivos$CASOS)

#Calculo do total de professores suspeitos
professores_suspeitos <-subset(analise_geral_susp, analise_geral_susp$CATEGORIA == "Professor ou auxiliar de sala")
professores_suspeitos$CASO <- 1
total_suspeitos_prof <- sum(professores_suspeitos$CASOS)

#Calculo do total de contato de caso
contatos_caso_prof <-subset(analise_geral_contato, analise_geral_contato$CATEGORIA == "Professor ou auxiliar de sala")
contatos_caso_prof$CASO <- 1
total_contato_prof <- sum(contatos_caso_prof$CASOS)

# calculo de total de professores descartados
professores_descartados <- subset(analise_geral_descartados, analise_geral_descartados$CATEGORIA == "Professor ou auxiliar de sala")
professores_descartados$CASO <- 1
total_descartados_prof <- sum(professores_descartados$CASOS)

# Taxa total de professores confirmados
prof_tot <- sum(total_positivos_prof + total_suspeitos_prof + total_contato_prof
                  + total_descartados_prof)
taxa_positividade_profnotif <- (total_positivos_prof  / prof_tot) * 100

#Taxa professores afetados x matriculados CENSO 2019
Taxa_afetados <- (prof_tot /

###############################################
#Taxa de Positividade em Outros colaboradores
outros_positivos <- subset(analise_geral_conf, analise_geral_conf$CATEGORIA == "Outros colaboradores")
sum(outros_positivos$CASOS)

# calculo de total de Outros colaboradores
outros_tot <- subset(analise_geral, analise_geral$ALUNO_PROF == "Outros colaboradores")
outros_tot$CASO <- 1
sum(outros_tot$CASO, na.rm = T)

# Taxa total de Outros colaboradores confirmados
sum(outros_positivos$CASOS, na.rm = T) / sum(outros_tot$CASO, na.rm = T) * 100

#Tabelas
