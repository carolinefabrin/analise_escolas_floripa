#26/03/2021 - Analise Completa (ativos e inativos) / ALuno, Prof, Outros colaboradores
library(tidyverse)
library(dplyr)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

analise_geral <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                 `Nome da escola`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `Idade da Pessoa* fórmula`)

names(analise_geral) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DIAGNOSTICO', 'IDADE')
analise_geral$TURMA <- ifelse(analise_geral$TURMA == 'NULL', NA, analise_geral$TURMA)
analise_geral <- analise_geral[!is.na(analise_geral$ESCOLA),]

#Analise quantidade de casos aluno x prof x outros colaboradores
analise_geral_conf <- subset(analise_geral, analise_geral$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
analise_geral_conf$CASOS <- 1


analise_geral_conf$CATEGORIA <- ifelse(analise_geral_conf$ALUNO_PROF %in% c("Professor ou auxiliar de sala",
                                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                                            "Professor do ensino fundamental e médio",
                                                                            "Professor do ensino médio",
                                                                            "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                       ifelse(analise_geral_conf$ALUNO_PROF %in% c("Aluno",
                                                                                   "Aluno do ensino infantil",
                                                                                   "Aluno do ensino fundamental",
                                                                                   "Aluno do ensino médio"), "Aluno",
                                              ifelse(analise_geral_conf$ALUNO_PROF %in% c("Outros colaboradores"), "Outros colaboradores", NA
                                                     
                                              )
                                          )
                                       )

analise_geral_conf <- analise_geral_conf[!is.na(analise_geral_conf$CATEGORIA),]                                           
confirmados_geral <- analise_geral_conf %>%
  group_by(CATEGORIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(confirmados_geral, "confirmados_geral.csv", row.names = F)

ggplot(confirmados_geral, aes(x= reorder(CATEGORIA, -CASOS), y= CASOS, fill= CATEGORIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  scale_y_continuous(limits=c(0, 200))  

#Analise quantidade de suspeitos aluno x prof x outros colaboradores
analise_geral_susp <- subset(analise_geral, analise_geral$DIAGNOSTICO == "Suspeito")
analise_geral_susp$CASOS <- 1


analise_geral_susp$CATEGORIA <- ifelse(analise_geral_susp$ALUNO_PROF %in% c("Professor ou auxiliar de sala",
                                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                                            "Professor do ensino fundamental e médio",
                                                                            "Professor do ensino médio",
                                                                            "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                       ifelse(analise_geral_susp$ALUNO_PROF %in% c("Aluno",
                                                                                   "Aluno do ensino infantil",
                                                                                   "Aluno do ensino fundamental",
                                                                                   "Aluno do ensino médio"), "Aluno",
                                              ifelse(analise_geral_susp$ALUNO_PROF %in% c("Outros colaboradores"), "Outros colaboradores", NA
                                                     
                                              )
                                       )
)

analise_geral_susp <- analise_geral_susp[!is.na(analise_geral_susp$CATEGORIA),]                                          
suspeitos_geral <- analise_geral_susp %>%
  group_by(CATEGORIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(suspeitos_geral, "suspeitos_geral.csv", row.names = F)

ggplot(suspeitos_geral, aes(x= reorder(CATEGORIA, -CASOS), y= CASOS, fill= CATEGORIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos")+
  scale_y_continuous(limits=c(0, 60))  
