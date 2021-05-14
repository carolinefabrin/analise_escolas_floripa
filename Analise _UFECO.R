
library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

solicitacoes_escola <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                 `Nome da escola`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`,
                                                 `Data de afastamento da turma`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `Quem fez o agendamento para caso suspeito?`,
                                                 `Tipo de Exame para os casos positivos: OBRIGATÓRIO!!!! Tipo de exame solicitado para os suspeitos`,
                                                 `Escola pública ou privada?`,
                                                 `Idade da Pessoa* fórmula`) 

names(solicitacoes_escola) <- c('ALUNO_PROF', 'ESCOLA', 'TURMA', 'AFASTAMENTO', 'DIAGNOSTICO', 'EXAME', 'AGENDAMENTO',' INSTITUICAO','IDADE')

solicitacoes_escola <- solicitacoes_escola[!is.na(solicitacoes_escola$ESCOLA),]
solicitacoes_escola <- solicitacoes_escola [!is.na(solicitacoes_escola $DIAGNOSTICO),]
solicitacoes_escola  <- solicitacoes_escola [!is.na(solicitacoes_escola $ALUNO_PROF),]


## NUMERO CÇAS TESTADAS ##
## FAIXA ETARIA 0-9 anos####
solicitacoes_escola_infantil <- subset(solicitacoes_escola, solicitacoes_escola$IDADE
                                       %in% c(0, 1 , 2 , 3, 4 ,5 ,6 ,7 ,8 ,9))
solicitacoes_escola_infantil$CASOS <- 1
solicitacoes_escola_infantil <- solicitacoes_escola_infantil[!is.na(solicitacoes_escola_infantil$EXAME),]  

#Total de testadas
sum(solicitacoes_escola_infantil$CASOS)

#Numero de positivas
solicitacoes_escola_infantil_positivos <- subset(solicitacoes_escola_infantil, solicitacoes_escola_infantil$DIAGNOSTICO
                                 %in% c("Confirmado visto laudo", "Confirmado"))
#Total positivas
sum (solicitacoes_escola_infantil_positivos$CASOS)

## FAIXA ETARIA 10-19 anos####
solicitacoes_escola_adolesc <- subset(solicitacoes_escola, solicitacoes_escola$IDADE
                                       %in% c(10, 11 , 12 , 13, 14 ,15 ,16 ,17 ,18 ,19))
solicitacoes_escola_adolesc$CASOS <- 1
solicitacoes_escola_adolesc <- solicitacoes_escola_adolesc[!is.na(solicitacoes_escola_adolesc$EXAME),]  

#Total de testadas
sum(solicitacoes_escola_adolesc$CASOS)

#Numero de positivas
solicitacoes_escola_adolesc_positivos <- subset(solicitacoes_escola_adolesc, solicitacoes_escola_adolesc$DIAGNOSTICO
                                                 %in% c("Confirmado visto laudo", "Confirmado"))
#Total positivas
sum (solicitacoes_escola_adolesc_positivos$CASOS)


#PROFESSORES TESTADOS
professores_testados <- subset (solicitacoes_escola, solicitacoes_escola$ALUNO_PROF 
                                %in% c ("Professor ou auxiliar de sala", 
                                        "Professor ou auxiliar de sala do ensino infantil",
                                        "Professor do ensino fundamental",
                                        "Professor do ensino médio",
                                        "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                        "Professor do ensino fundamental e médio",
                                        "Outros professores (como de universidade; escola para adultos)")) 
                                       

professores_testados <- professores_testados[!is.na(professores_testados$EXAME),]  
professores_testados$CASOS <-1
sum(professores_testados$CASOS)

professores_testados_positvos <- subset(professores_testados, professores_testados$DIAGNOSTICO
                                 %in% c("Confirmado visto laudo", "Confirmado"))


sum(professores_testados_positvos$CASOS)



##Outros colaboradores testados
outros_colab_testados <- subset (solicitacoes_escola, solicitacoes_escola$ALUNO_PROF 
                                == "Outros colaboradores")


outros_colab_testados  <- outros_colab_testados [!is.na(outros_colab_testados$EXAME),]  
outros_colab_testados $CASOS <-1
sum(outros_colab_testados$CASOS)

outros_colab_testados_positvos <- subset(outros_colab_testados, outros_colab_testados$DIAGNOSTICO
                                        %in% c("Confirmado visto laudo", "Confirmado"))


sum(outros_colab_testados_positvos$CASOS)




#NUMERO DE FUNCIONARIOS TESTADOS
funcionarios_testados <- subset (solicitacoes_escola, solicitacoes_escola$ALUNO_PROF 
                                 %in% c ("Professor ou auxiliar de sala", 
                                         "Professor ou auxiliar de sala do ensino infantil",
                                         "Professor do ensino fundamental",
                                         "Professor do ensino médio",
                                         "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                         "Professor do ensino fundamental e médio",
                                         "Outros professores (como de universidade; escola para adultos)", 
                                         "Outros colaboradores"))

funcionarios_testados <- funcionarios_testados[!is.na(funcionarios_testados$EXAME),]  
funcionarios_testados$CASOS <-1
sum(funcionarios_testados$CASOS)

funcionarios_positivos <- subset(funcionarios_testados, funcionarios_testados$DIAGNOSTICO
                                %in% c("Confirmado visto laudo", "Confirmado"))

funcionarios_positivos$CASOS <-1
sum(funcionarios_positivos$CASOS)

funcionarios_positivos_privada <- subset (funcionarios_positivos, funcionarios_positivos$` INSTITUICAO` == "Privada")
sum(funcionarios_positivos_privada$CASOS)

funcionarios_positivos_municipal <- subset(funcionarios_positivos, funcionarios_positivos$` INSTITUICAO`%in% c ("Pública Municipal", "Conveniada com PMF"))
sum(funcionarios_positivos_municipal$CASOS)

funcionarios_positivos_estadual <- subset(funcionarios_positivos, funcionarios_positivos$` INSTITUICAO` == "Pública Estadual")
sum(funcionarios_positivos_estadual$CASOS)

#Dados em CSV
write.csv(funcionarios_positivos, "funcionarios_positivos.csv", row.names = F)

# DATA FRAME
trabalhadores_testados <- data.frame (Trabalhadores = c("Testes Realizados", "Teste Positivo"),
"Total" = c(sum(funcionarios_testados$CASOS), sum(funcionarios_positivos$CASOS)))

#Dados em CSV
write.csv(trabalhadores_testados, "trabalhadores_testados.csv", row.names = F)


# ALUNOS 
solicitacoes_alunos <- subset(solicitacoes_escola, solicitacoes_escola$ALUNO_PROF
                              %in% c ("Aluno", "Aluno do ensino infantil",
                                      "Aluno do ensino fundamental",
                                      "Aluno do ensino médio",
                                      "Aluno de idiomas",
                                      "Outros alunos (universidade; escola para adultos; por exemplo)"))
#Crianças notificadas
solicitacoes_alunos$CASOS <- 1
sum(solicitacoes_alunos$CASOS)

solicitacoes_alunos_positivos <- subset(solicitacoes_alunos, solicitacoes_alunos$DIAGNOSTICO
                                        %in% c("Confirmado visto laudo", "Confirmado"))

solicitacoes_alunos_positivos$CASOS <- 1
sum(solicitacoes_alunos_positivos$CASOS)

alunos_positivos_privada <- subset (solicitacoes_alunos_positivos, solicitacoes_alunos_positivos$` INSTITUICAO` == "Privada")
sum(alunos_positivos_privada$CASOS)


alunos_positivos_municipal <- subset(solicitacoes_alunos_positivos, solicitacoes_alunos_positivos$` INSTITUICAO`%in% c ("Pública Municipal", "Conveniada com PMF"))
sum(alunos_positivos_municipal$CASOS)

alunos_positivos_estadual <- subset(solicitacoes_alunos_positivos, solicitacoes_alunos_positivos$` INSTITUICAO` == "Pública Estadual")
sum(alunos_positivos_estadual$CASOS)

# DATA FRAME
alunos_notificados <- data.frame (Alunos = c("Notificados", "Teste Positivo"),
                                      "Total" = c (sum(solicitacoes_alunos$CASOS), sum(solicitacoes_alunos_positivos$CASOS)))

#Dados em CSV
write.csv(alunos_notificados, "alunos_notificados.csv", row.names = F)

#Escolas com serviço suspenso
escolas_suspensas <- subset(solicitacoes_escola, 
                            solicitacoes_escola$AFASTAMENTO != "NA")

escolas_suspensas$CASOS <-1
escolas_suspensas_tot <- escolas_suspensas %>%
  group_by(ESCOLA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))




