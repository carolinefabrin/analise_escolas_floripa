library(tidyverse)

source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_escola <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma?`, 
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:OBRIGATÓRIO!!!!`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!`,
                                                 `SURTO ENCERRADO? *fórmula`,
                                                 `CASO FINALIZADO? *fórmula`)

names(notif_escola) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO_FINALZ', 'CASO_FINALZ')
notif_escola$TURMA <- ifelse(notif_escola$TURMA == 'NULL', NA, notif_escola$TURMA)
notif_escola <- subset(notif_escola, is.na(notif_escola$SURTO_FINALZ) & is.na(notif_escola$CASO_FINALZ))
notif_escola$SURTO_FINALZ <- NULL
notif_escola$CASO_FINALZ <- NULL
notif_escola <- na.omit(notif_escola)
notif_escola$DT_PRIM_SINTOM <- as.Date(notif_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')

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

library (ggplot2)
ggplot(subset(notif_escola_final, !is.na(notif_escola_final)), aes(x = reorder(ESCOLA, CASOS_ESCOLA), y = CASOS_ESCOLA, fill=ESCOLA)) + 
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")

# Relação professor/aluno 
notif_escola_alunoprof <- notif_escola
notif_escola_alunoprof <- subset(notif_escola, notif_escola$ALUNO_PROF %in% c("Aluno","Professor ou auxiliar de sala"))
notif_escola_alunoprof <- subset (notif_escola_alunoprof, notif_escola_alunoprof$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))




