#MONITORAMENTO ESCOLAS ATIVAS
library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

monitora_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
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

# ESCOLAS AFETADAS ATUALMENTE (confirmados e suspeitos)
monitora_escola_atual <- subset(monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado", "Suspeito", "Suspeito e recusou fazer teste"))
monitora_escola_atual <- subset(monitora_escola, is.na(monitora_escola$CASO_FINALZ))
monitora_escola_atual$CASOS <- 1 
sum(monitora_escola_atual$CASOS)
 
monitora_escola_afetadas <- monitora_escola_atual %>%
  group_by(ESCOLA,INSTITUICAO, CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))


#CASOS CONFIRMADOS ATIVOS
notif_escola_hoje <- subset(monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
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
                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                            "Professor do ensino fundamental e médio",
                                                            "Professor do ensino médio",
                                                            "Outros professores (como de universidade; escola para adultos)",
                                                            "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                      ifelse(notif_escola_hoje$ALUNO_PROF %in% c("Aluno", 
                                                                   "Outros alunos (universidade; escola para adultos; por exemplo)",
                                                                   "Aluno do ensino infantil",
                                                                   "Aluno do ensino fundamental",
                                                                   "Aluno do ensino médio"), "Aluno",
                                      ifelse(notif_escola_hoje$ALUNO_PROF %in% c("Outros colaboradores"), 
                                                                                 "Outros colaboradores", NA
                                             
                                      )
                               )
)



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
notif_escola_susp_hoje <- subset (monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Suspeito", "Suspeito e recusou fazer teste"))
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
                                                                          "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                                          "Professor ou auxiliar de sala do ensino infantil",
                                                                          "Professor do ensino fundamental e médio",
                                                                          "Professor do ensino médio",
                                                                          "Outros professores (como de universidade; escola para adultos)",
                                                                          "Professor do ensino fundamental"), "Professor ou auxiliar de sala",
                                      ifelse(notif_escola_susp_hoje$ALUNO_PROF %in% c("Aluno", "Outros alunos (universidade; escola para adultos; por exemplo)",
                                                                                 "Aluno do ensino infantil",
                                                                                 "Aluno do ensino fundamental",
                                                                                 "Aluno do ensino médio"), "Aluno",
                                             ifelse(notif_escola_susp_hoje$ALUNO_PROF %in% c("Outros colaboradores"), "Outros colaboradores", NA
                                                    
                                             )
                                      )
)



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




#19/03/2021 - ANALISE DE SURTOS ATIVOS
library(tidyverse)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

monitora_escola_ativo <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
  
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
monitora_escola_ativo <- subset(monitora_escola_ativo, monitora_escola_ativo$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))

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



#Grafico Escola com Surtos x Nº de Casos Confirmados
ggplot(surtos_escola, aes(x= reorder(ESCOLA, SURTOS_ESCOLA), y= SURTOS_ESCOLA, fill=ESCOLA))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de casos")










