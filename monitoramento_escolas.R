#MONITORAMENTO ESCOLAS ATIVAS
library(tidyverse)
library(zoo)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

monitora_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `É surto?`,
                                                 `SURTO ENCERRADO? *fórmula`,
                                                 `CASO FINALIZADO? *fórmula`)
names(monitora_escola) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ')
monitora_escola$DT_PRIM_SINTOM <- as.Date(monitora_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
monitora_escola <- monitora_escola[!is.na(monitora_escola$ESCOLA),]

#CASOS CONFIRMADOS ATIVOS
notif_escola_hoje <- subset(monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
notif_escola_hoje$CASOS <- 1 
notif_escola_hoje <- subset(notif_escola_hoje, is.na(notif_escola_hoje$CASO_FINALZ))
sum(notif_escola_hoje$CASOS)


# CASOS SUSPEITOS NOTIFICADOS
notif_escola_susp_hoje <- subset (monitora_escola, monitora_escola$DIAGNOSTICO %in% c("Suspeito", "Suspeito e recusou fazer teste"))
notif_escola_susp_hoje$CASOS <- 1 
notif_escola_susp_hoje <- subset(notif_escola_susp_hoje, is.na(notif_escola_susp_hoje$CASO_FINALZ))
sum(notif_escola_susp_hoje$CASOS)

notif_escolacomsusp_hoje <-notif_escola_susp_hoje %>%
  group_by(ESCOLA,CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

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
                                                 `Idade da Pessoa* fórmula`) 

names(monitora_escola_ativo) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'SURTO_FINALZ', 'CASO_FINALZ', 'IDADE')

monitora_escola_ativo$DT_PRIM_SINTOM <- as.Date(monitora_escola_ativo$DT_PRIM_SINTOM, format = '%d/%m/%Y')
monitora_escola_ativo <- subset(monitora_escola_ativo, monitora_escola_ativo$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))

#Mantido os casos finalizados para analise do surto
notif_escola_surto <- subset(monitora_escola_ativo, is.na(monitora_escola_ativo$SURTO_FINALZ))

#NUMERO DE SURTOS ATIVOS 
notif_escola_suativo <- subset(notif_escola_surto, notif_escola_surto$SURTO == "Sim")
notif_escola_suativo$CASO <- 1
surtos_escola <- notif_escola_suativo %>%
  group_by(ESCOLA)%>%
  summarise(SURTOS_ESCOLA = sum(CASO, na.rm = T))%>%
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










