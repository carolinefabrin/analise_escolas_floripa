library(tidyverse)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `SURTO ENCERRADO? *fórmula`,
                                                 `CASO FINALIZADO? *fórmula`)

names(notif_escola) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO_FINALZ', 'CASO_FINALZ')
notif_escola$TURMA <- ifelse(notif_escola$TURMA == 'NULL', NA, notif_escola$TURMA)
notif_escola$DT_PRIM_SINTOM <- as.Date(notif_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
notif_escola$SURTO_FINALZ <- NULL
notif_escola$CASO_FINALZ <- NULL
notif_escola <- notif_escola[!is.na(notif_escola$ESCOLA),]
com_dados_sobre_data <- sum(!is.na(notif_escola$DT_PRIM_SINTOM))
com_dados_sobre_data
sem_dados_sobre_data <- sum(is.na(notif_escola$DT_PRIM_SINTOM))
sem_dados_sobre_data

sem_dados_sobre_data/(com_dados_sobre_data+sem_dados_sobre_data)

notif_escola <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ) & is.na(notif_escola$CASO_FINALZ))
notif_escola$SURTO_FINALZ <- NULL
notif_escola$CASO_FINALZ <- NULL
notif_escola <- na.omit(notif_escola)

# Calculo de confirmados por escola
notif_escola_conf <- subset(notif_escola, notif_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
notif_escola_conf$CASO <- 1
casos_escola <- notif_escola_conf %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))

# DADOS EM CSV 
write.csv(casos_escola, "casos_escola.csv", row.names = F)

# calculo de total por escola
notif_escola_tot <- notif_escola
notif_escola_tot$CASO <- 1
casos_escola_tot <- notif_escola_tot %>%
  group_by(ESCOLA)%>%
  summarise(TOT_ESCOLA = sum(CASO, na.rm = T))
# DADOS EM CSV 
write.csv(casos_escola, "casos_escola.csv", row.names = F)

# Merge dos confirmados com o total
notif_escola_final <- merge(casos_escola, casos_escola_tot,by = 'ESCOLA', all = T )
notif_escola_final$TAXA_POSITIVOS <- notif_escola_final$CASOS_ESCOLA/notif_escola_final$TOT_ESCOLA * 100

# Taxa total de confirmados
sum(notif_escola_final$CASOS_ESCOLA, na.rm = T) / sum(notif_escola_final$TOT_ESCOLA, na.rm = T) * 100

ggplot(notif_escola_final, aes(x = reorder(ESCOLA, TOT_ESCOLA), y = TOT_ESCOLA, fill=ESCOLA)) + 
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de Suspeitos")



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
ggplot(serie_hist_susp, aes(DT_PRIM_SINTOM, QUANTIDADE, group = 1))+
  geom_line()+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de suspeitos")

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
serie_hist_casos <- subset(notif_escola, notif_escola$DIAGNOSTICO %in% c("Confirmado visto laudo", "Confirmado"))
serie_hist_casos$QUANTIDADE <- 1
serie_hist_casos <- serie_hist_casos %>%
  group_by(DT_PRIM_SINTOM) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))
ggplot(serie_hist_casos, aes(DT_PRIM_SINTOM, QUANTIDADE, group = 1))+
  geom_line()+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de casos")

#Série histórica de casos - Por Escola
serie_hist_escolas_casos <- subset(notif_escola, notif_escola$DIAGNOSTICO %in% c("Confirmado visto laudo", "Confirmado"))
serie_hist_escolas_casos$QUANTIDADE <- 1 #Inclui o 1 para poder somar o número de casos
serie_hist_escolas_casos <- serie_hist_escolas_casos %>%
  group_by(DT_PRIM_SINTOM, ESCOLA) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE, na.rm = T))
ggplot(serie_hist_escolas_casos, aes(DT_PRIM_SINTOM, QUANTIDADE, group = ESCOLA))+
  geom_line()+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de Casos")

#19/03/2021 - ANALISE SOMENTE COM CASOS CONFIRMADOS E SURTOS ATIVOS
library(tidyverse)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `É surto?`,
                                                 `SURTO ENCERRADO? *fórmula`,
                                                 `CASO FINALIZADO? *fórmula`,
                                                 `Idade da Pessoa* fórmula`) 

names(notif_escola) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'IDADE')
notif_escola$TURMA <- ifelse(notif_escola$TURMA == 'NULL', NA, notif_escola$TURMA)
notif_escola$DT_PRIM_SINTOM <- as.Date(notif_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
notif_escola <- subset(notif_escola, notif_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))

#sOMENTE OS CASOS/SURTOS ATIVOS DE CASOS CONFIRMADOS
notif_escola_ativo <- notif_escola
notif_escola_ativo <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ))
notif_escola_ativo <- subset(notif_escola_ativo, is.na(notif_escola_ativo$CASO_FINALZ))

#Mantido os casos finalizados para analise do surto
notif_escola_surto <- notif_escola
notif_escola_surto <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ))

#NUMERO DE SURTOS ATIVOS Com casos confirmados
notif_escola_suativo <- subset(notif_escola_surto, notif_escola_surto$SURTO == "Sim")
notif_escola_suativo$SURTO <- 1
surtos_escola <- notif_escola_suativo %>%
  group_by(ESCOLA)%>%
  summarise(SURTOS_ESCOLA = sum(SURTO, na.rm = T))%>%
  arrange(desc(SURTOS_ESCOLA))

# DADOS EM CSV 
write.csv(surtos_escola, "surtos_escola.csv", row.names = F)

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
surtos_idade <- subset(surtos_idade, surtos_idade$ALUNO_PROF %in% c("Aluno","aluno" )) %>%
  arrange(IDADE)
Group_idade <- select(surtos_idade, ESCOLA, IDADE)

surtos_idade$FAIXA_ETARIA <- ifelse(surtos_idade$IDADE < 6,"Ensino Infantil",
                                    ifelse(surtos_idade$IDADE >= 6 & surtos_idade$IDADE < 15,"Ensino Fundamental", 
                                           ifelse(surtos_idade$IDADE >= 15 & surtos_idade$IDADE < 18,"Ensino Médio", NA)))

surtos_idade$CASOS <- 1

surtos_idade_escola <- surtos_idade %>%
  group_by(ESCOLA,FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

# DADOS EM CSV 
write.csv(surtos_idade, "surtos_idade.csv", row.names = F)

ggplot(surtos_idade_escola, aes(x= ESCOLA, y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino') +
  scale_y_continuous(limits=c(0, 15))

surtos_idade_pura <- surtos_idade %>%
  group_by(ESCOLA,FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

# DADOS EM CSV 
write.csv(surtos_idade_pura, "surtos_idade_pura.csv", row.names = F)

ggplot(surtos_idade_pura, aes(x= reorder(FAIXA_ETARIA,CASOS), y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 





