#MONITORAMENTO ESCOLAS ATIVAS

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

#CASOS CONFIRMADOS ATIVOS
notif_escola_hoje <- select(notif_escola_ativo, ESCOLA)
notif_escola_hoje$CASOS <- 1 

notif_escola_escola <- notif_escola_hoje %>%
  group_by(ESCOLA,CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

# CASOS SUSPEITOS NOTIFICADOS
notif_escola_susp_hoje <- subset (notif_escola, notif_escola$DIAGNOSTICO == "Suspeito")
notif_escola_susp_hoje$CASOS <- 1 
notif_escola_susp_hoje <- subset(notif_escola_susp_hoje, is.na(notif_escola_susp_hoje$CASO_FINALZ))

notif_escolacomsusp_hoje <-notif_escola_susp_hoje %>%
  group_by(ESCOLA,CASOS) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))
