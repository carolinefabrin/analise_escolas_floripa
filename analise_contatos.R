library(tidyverse)
library (googlesheets4)

id_notificacontatos <- "https://docs.google.com/spreadsheets/d/15IuDUWEIL11k0PrSCDvEx7yCCq7OxIhxrVofsL4R0ig/edit#gid=49342861&fvid=1857359446"
notificacontatos <- read_sheet(id_notificacontatos, "Contatos dos casos positivos", col_names = T)


notif_contatos <- notificacontatos %>% dplyr::select(`O caso indice é aluno ou funcionário?`,
                                                 `Nome da escola`,
                                                 `Nome completo do caso índice`,
                                                 `Vínculo com o caso`,
                                                 `Resultado do exame:`)
                                                


names(notif_contatos) <- c('ALUNO_PROF', 'ESCOLA', 'CASO_INDICE','VINCULO', 'DIAGNOSTICO')
notif_contatos_limpa <- notif_contatos[!is.na(notif_contatos$ESCOLA),]
notif_contatos_limpa <- notif_contatos_limpa [!is.na(notif_contatos_limpa$DIAGNOSTICO),]
notif_contatos_limpa <- notif_contatos_limpa [!is.na(notif_contatos_limpa$VINCULO),]

notif_contatos_positivo <- subset(notif_contatos_limpa, notif_contatos_limpa$DIAGNOSTICO == "Confirmado")

notif_contatos_positivo$CASO <- 1
contato_caso <- notif_contatos_positivo %>%
  group_by(ALUNO_PROF, CASO_INDICE, VINCULO)%>%
  summarise(CASOS_CONTATO = sum(CASO, na.rm = T))%>%
  arrange(CASOS_CONTATO)

sum (contato_caso$CASOS_CONTATO)

# DADOS EM CSV
write.csv(contato_caso, "contato_caso.csv", row.names = F)


              ############### CASO INDICE ALUNO ################## 
### Ele pode passar para: Estudante, Professor ou auxiliar de sala, Familiar, Amizade (se da escola: selecionar outra categoria)

secundario_aluno_indice <- subset (contato_caso, contato_caso$ALUNO_PROF %in%
                                   c("Aluno", "Aluno do ensino infantil",
                                    "Aluno do ensino fundamental",
                                    "Aluno do ensino médio",
                                    "Aluno de idiomas",
                                    "Outros alunos (universidade; escola para adultos; por exemplo)"))

## Caso secundario Aluno ## 
aluno_estudante <- subset (secundario_aluno_indice, secundario_aluno_indice$VINCULO
                           == "Estudante")
  
sum (aluno_estudante$CASOS_CONTATO) ## nº de alunos contaminados por outro aluno 



## Caso secundario Professor ou auxiliar de sala ##
aluno_professor <- subset (secundario_aluno_indice, secundario_aluno_indice$VINCULO
                           == "Professor ou auxiliar de sala")


sum (aluno_professor$CASOS_CONTATO) ## nº de prof contaminados por outro aluno


## Caso secundario Familiar ##
aluno_familia <- subset (secundario_aluno_indice, secundario_aluno_indice$VINCULO
                         == "Familiar")


sum (aluno_familia$CASOS_CONTATO) ## nº de familiares contaminados por outro aluno





               ############### CASO INDICE PROFESSOR ################## 
### Ele pode passar para: Estudante, Professor ou auxiliar de sala, Familiar, Laboral, Amizade (se da escola: selecionar outra categoria)

secundario_prof_indice <- subset(contato_caso, contato_caso$ALUNO_PROF %in%
                                c("Professor ou auxiliar de sala", 
                                  "Professor ou auxiliar de sala do ensino infantil",
                                  "Professor do ensino fundamental",
                                  "Professor do ensino médio",
                                  "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                  "Professor do ensino fundamental e médio",
                                  "Outros professores (como de universidade; escola para adultos)"))


## Caso secundario Aluno ## 
prof_estudante <- subset (secundario_prof_indice, secundario_prof_indice$VINCULO
                           == "Estudante")

sum (prof_estudante$CASOS_CONTATO) ## nº de alunos contaminados pelo prof


## Caso secundario Professor ou auxiliar de sala ##
prof_professor <- subset (secundario_prof_indice, secundario_prof_indice$VINCULO
                           == "Professor ou auxiliar de sala")


sum (prof_professor$CASOS_CONTATO) ## nº de prof contaminados por outro prof

## Caso secundario Familiar ##
prof_familia <- subset (secundario_prof_indice, secundario_prof_indice$VINCULO
                         == "Familiar")


sum (prof_familia$CASOS_CONTATO) ## nº de familiares contaminados por prof


### Caso secundario laboral ##
prof_laboral <- subset (secundario_prof_indice, secundario_prof_indice$VINCULO
                        == "Laboral")

sum (prof_laboral$CASOS_CONTATO) ## nº de trabalhadores contaminados por prof




            
       ############### CASO INDICE OUTROS COLABORADORES ################## 
### Ele pode passar para: Estudante, Professor ou auxiliar de sala, Familiar, Laboral, Amizade (se da escola: selecionar outra categoria)

secundario_outros_indice <- subset(contato_caso, contato_caso$ALUNO_PROF 
                                   == "Outros colaboradores")


## Caso secundario Aluno ## 
outros_estudante <- subset (secundario_outros_indice, secundario_outros_indice$VINCULO
                          == "Estudante")

sum (outros_estudante$CASOS_CONTATO) ## nº de alunos contaminados por outros colab


## Caso secundario Professor ou auxiliar de sala ##
outros_professor <- subset (secundario_outros_indice, secundario_outros_indice$VINCULO
                          == "Professor ou auxiliar de sala")


sum (outros_professor$CASOS_CONTATO) ## nº de prof contaminados por outros colab

## Caso secundario Familiar ##
outros_familia <- subset (secundario_outros_indice, secundario_outros_indice$VINCULO
                        == "Familiar")


sum (outros_familia$CASOS_CONTATO) ## nº de familiares contaminados por outros


### Caso secundario laboral ##
outros_laboral <- subset (secundario_outros_indice, secundario_outros_indice$VINCULO
                        == "Laboral")

sum (outros_laboral$CASOS_CONTATO) ## nº de trabalhadores contaminados por outros colab


### Caso secundario Amizade ##
outros_amizade <- subset (secundario_outros_indice, secundario_outros_indice$VINCULO
                          == "Amizade (se da escola: selecionar outra categoria)")

sum (outros_amizade$CASOS_CONTATO) ## nº de amigos contaminados por outros colab





##### DATA FRAME #######
casos_secundarios <- data.frame(NOTIFICACOES = c('CONFIRMADO', 'SUSPEITO'),
                                  "Alunos" = c(sum(notif_alunos_hj$CASOS),sum(notifsusp_alunos_hj$CASOS)),
                                  
                                  
                                  "Professores" = c(sum(notif_prof_hj$CASOS), sum(notifsusp_prof_hj$CASOS)),
                                  
                                  
                                  "Outros Colaboradores" = c(sum(notif_outros_hj$CASOS), sum(notifsusp_outros_hj$CASOS)))



# DADOS EM CSV 
write.csv(casos_secundarios, "casos_secundarios.csv", row.names = F)

