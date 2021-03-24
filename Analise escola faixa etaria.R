#23/03/2021 - ANALISE POR ESCOLARIDADE DE SUSPEITOS/CASOS CONFIRMADOS E SURTOS ATIVOS
library(tidyverse)
library(dplyr)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_geral <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma?`, 
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!`,
                                                 `É surto?`,
                                                 `SURTO ENCERRADO? *fórmula`,
                                                 `CASO FINALIZADO? *fórmula`,
                                                 `Idade da Pessoa* fórmula`) 

names(notif_geral) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'IDADE')
notif_geral$TURMA <- ifelse(notif_geral$TURMA == 'NULL', NA, notif_geral$TURMA)
notif_geral$DT_PRIM_SINTOM <- as.Date(notif_geral$DT_PRIM_SINTOM, format = '%d/%m/%Y')


#Analise Casos confirmados x faixa etária (mantido casos finalizados)
notif_geral_conf <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado") 
                           & notif_geral$ALUNO_PROF %in% c("Aluno", "aluno")) %>%
  arrange(IDADE)





#Analise Casos suspeitos X faixa etária


#sOMENTE OS CASOS/SURTOS ATIVOS DE CASOS CONFIRMADOS
notif_escola_ativo <- notif_escola
notif_escola_ativo <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ))
notif_escola_ativo <- subset(notif_escola_ativo, is.na(notif_escola_ativo$CASO_FINALZ))
notif_geral <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))

#Mantido os casos finalizados para analise do surto
notif_escola_surto <- notif_escola
notif_escola_surto <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ))

#NUMERO DE SURTOS ATIVOS Com casos confirmados
notif_escola_suativo <- subset(notif_escola_surto, notif_escola_surto$SURTO == "Sim") 
arrange(desc(SURTOS_ESCOLA))

#Grafico Escola com Surtos x Nº de Casos Confirmados
ggplot(surtos_escola, aes(x= reorder(ESCOLA, SURTOS_ESCOLA), y= SURTOS_ESCOLA, fill=ESCOLA))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de casos")

#sURTOS AGRUPADOS POR FAIXA ETÁRIA
surtos_idade <- select(notif_escola_suativo, ESCOLA, ALUNO_PROF, SURTO, IDADE) 
surtos_idade <- subset(surtos_idade, surtos_idade$ALUNO_PROF %in% c("Aluno","aluno" ))
Group_idade <- select(surtos_idade, ESCOLA, IDADE)


ggplot(surtos_idade, aes(x= ESCOLA, y= IDADE, fill=ESCOLA, ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Faixa Etária")


