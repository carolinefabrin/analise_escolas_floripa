library(tidyverse)
library(zoo)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

Censo_2019 <- library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_escola <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                 `Nome da escola`,
                                                 `Data dos primeiros sintomas:`,
                                                 `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                 `É surto?`,
                                                 `Escola pública ou privada?`,
                                                 `Idade da Pessoa* fórmula`) 
                                             

names(notif_escola) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'INSTITUICAO','IDADE')
notif_escola$DT_PRIM_SINTOM <- as.Date(notif_escola$DT_PRIM_SINTOM, format = '%d/%m/%Y')
notif_escola <- notif_escola[!is.na(notif_escola$ESCOLA),]
com_dados_sobre_data <- sum(!is.na(notif_escola$DT_PRIM_SINTOM))
com_dados_sobre_data
sem_dados_sobre_data <- sum(is.na(notif_escola$DT_PRIM_SINTOM))
sem_dados_sobre_data
sem_dados_sobre_data/(com_dados_sobre_data+sem_dados_sobre_data)

# Calculo de confirmados por escola
notif_escola_conf <- subset(notif_escola, notif_escola$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado" ))
notif_escola_conf$CASO <- 1
casos_escola <- notif_escola_conf %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)

# DADOS EM CSV 
write.csv(casos_escola, "casos_escola.csv", row.names = F)
write.csv(notif_escola, "notif_escola.csv", row.names = F)

# calculo de total por escola
notif_escola_tot <- notif_escola
notif_escola_tot$CASO <- 1
casos_escola_tot <- notif_escola_tot %>%
  group_by(ESCOLA)%>%
  summarise(TOT_ESCOLA = sum(CASO, na.rm = T)) %>%
  arrange(TOT_ESCOLA)


# Merge dos confirmados com o total
notif_escola_final <- merge(casos_escola, casos_escola_tot,by = 'ESCOLA', all = T )
notif_escola_final$TAXA_POSITIVOS <- notif_escola_final$CASOS_ESCOLA/notif_escola_final$TOT_ESCOLA * 100

# Taxa total de confirmados por escola
sum(notif_escola_final$CASOS_ESCOLA, na.rm = T) / sum(notif_escola_final$TOT_ESCOLA, na.rm = T) * 100

ggplot(notif_escola_final, aes(x = reorder(ESCOLA, TOT_ESCOLA), y = TOT_ESCOLA, fill=ESCOLA)) + 
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab(" ")+
  ylab("Número de Suspeitos")

# DADOS EM CSV
write.csv(notif_escola_final, "notif_escola_final.csv", row.names = F)

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

"Média móvel de 14 dias" <- "red"
serie_hist_susp$MM_14 <- rollmean(serie_hist_susp$QUANTIDADE,14, align = "right",fill = NA)
ggplot(serie_hist_susp, aes(x = DT_PRIM_SINTOM))+
  geom_line(aes(y = QUANTIDADE))+
  geom_line(aes(y = MM_14, color = "Média móvel de 14 dias"))+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de suspeitos")+
  scale_color_discrete(name = " ")


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


"Média móvel de 14 dias" <- "red"
serie_hist_casos$MM_14 <- rollmean(serie_hist_casos$QUANTIDADE,14, align = "right",fill = NA)
ggplot(serie_hist_casos, aes(x = DT_PRIM_SINTOM))+
  geom_line(aes(y = QUANTIDADE))+
  geom_line(aes(y = MM_14, color = "Média móvel de 14 dias"))+
  theme_bw()+
  xlab("Data dos Primeiros Sintomas")+
  ylab("Número de casos")+
  scale_color_discrete(name = " ")
  

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


######################  ANALISE SURTOS  ##################################


###### TOTAL DE SURTOS ######## 
surtos_tot <- subset(notif_escola_conf, notif_escola_conf$SURTO == "Sim")
surtos_sum <- surtos_tot %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
sum (surtos_sum$CASOS_ESCOLA)
surtos_sum$CASO <-1
sum (surtos_sum$CASO)


################ ANALISE SURTOS POR TIPO DE INSTITUICAO (ALUNO/PROF/OUTROS) ############

# ###########  SURTOS EM ESCOLAS PRIVADAS  #############
surtos_privada <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Privada")
tot_surtos_privada <- surtos_privada %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_privada$CASO <- 1
sum (tot_surtos_privada$CASOS_ESCOLA)
sum (tot_surtos_privada$CASO)


##############  SURTOS EM ESCOLAS PUBLICAS MUNICIPAIS ############
surtos_publica_munic <- subset(surtos_tot, surtos_tot$INSTITUICAO %in% c("Pública Municipal", "Conveniada com PMF"))
tot_surtos_munic <- surtos_publica_munic %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_munic$CASO <- 1
sum(tot_surtos_munic$CASOS_ESCOLA)
sum (tot_surtos_munic$CASO)


##################  SURTOS EM ESCOLAS ESTADUAIS ########## 
surtos_estadual <- subset(surtos_tot, surtos_tot$INSTITUICAO == "Pública Estadual")
tot_surtos_estadual <- surtos_estadual %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(CASOS_ESCOLA)
tot_surtos_estadual$CASO <- 1
sum(tot_surtos_estadual$CASOS_ESCOLA)
sum (tot_surtos_estadual$CASO)

############ SURTOS EM ESCOLAS FEDERAIS ################


############      ANALISE SURTOS POR INSTITUICAO ######################

privada <- sum(tot_surtos_privada$CASO)
municipal <- sum(tot_surtos_munic$CASO)
estadual <- sum(tot_surtos_estadual$CASO)
total_surtos_tipo_escola <- data.frame("Privada" = privada,
                                      "Publica Municipal" = municipal,
                                      "Publica Estadual" = estadual )
total_surtos_tipo_escola <- melt(total_surtos_tipo_escola)
names(total_surtos_tipo_escola) <- c("Tipo", "Nº Escolas")


################   ANALISE SURTOS POR CASOS ###############

privada <- sum(tot_surtos_privada$CASOS_ESCOLA)
municipal <- sum(tot_surtos_munic$CASOS_ESCOLA)
estadual <- sum(tot_surtos_estadual$CASOS_ESCOLA)
casos_surtos_tipo_escola <- data.frame("Privada" = privada,
                                       "Publica Municipal" = municipal,
                                       "Publica Estadual" = estadual )
casos_surtos_tipo_escola <- melt(casos_surtos_tipo_escola)
names(casos_surtos_tipo_escola) <- c("Tipo", "Casos")


########### ANALISE COMPLETA CASOS POR TIPO DE INSTITUICAO ###########
base_unificada <- merge(casos_surtos_tipo_escola, total_surtos_tipo_escola, by = "Tipo", all = T)
sum(base_unificada$Casos)

######### SURTOS POR NÍVEL DE ENSINO (SOMENTE ALUNOS) ########### 
surtos_tot$CATEGORIA <- ifelse(surtos_tot$ALUNO_PROF %in% 
                                 c("Professor ou auxiliar de sala", 
                                   "Professor ou auxiliar de sala do ensino infantil",
                                   "Professor do ensino fundamental",
                                   "Professor do ensino médio",
                                   "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                   "Professor do ensino fundamental e médio",
                                   "Outros professores (como de universidade; escola para adultos)"),
                                   "Professor ou auxiliar de sala",
                              
                              ifelse(surtos_tot$ALUNO_PROF %in% 
                                       c("Aluno", "Aluno do ensino infantil",
                                         "Aluno do ensino fundamental",
                                         "Aluno do ensino médio",
                                         "Aluno de idiomas",
                                         "Outros alunos (universidade; escola para adultos; por exemplo)"),
                                         "Aluno",
                                                     
                              ifelse(surtos_tot$ALUNO_PROF %in% 
                              c("Outros colaboradores"), "Outros colaboradores", NA)))
                                                            
                                                     
                                            

surtos_nivel_ensino <- subset(surtos_tot, surtos_tot$CATEGORIA == "Aluno") 



surtos_nivel_ensino$ESCOLARIDADE <- ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino infantil", "Ensino Infantil",
                                    ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino fundamental","Ensino Fundamental", 
                                    ifelse(surtos_nivel_ensino$ALUNO_PROF == "Aluno do ensino médio" ,"Ensino Médio", NA)))


surtos_idade_escola <- surtos_nivel_ensino %>%
  group_by(ESCOLA,ESCOLARIDADE, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

#Grafico surto escola por nível de ensino ALUNOS
ggplot(surtos_idade_escola, aes(x= ESCOLA, y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino') +
  scale_y_continuous(limits=c(0, 40))

surtos_idade_pura <- surtos_idade_escola %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))
sum(surtos_idade_escola$CASO)

#Grafico casos por nível de ensino
ggplot(surtos_idade_pura, aes(x= reorder(ESCOLARIDADE,CASO), y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 

######## Valores por tipo de instituição ############ 
surtos_alunos_privada <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Privada")
sum(surtos_alunos_privada$CASO)

surtos_alunos_municipal <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Pública Municipal")
sum(surtos_alunos_municipal$CASO)

surtos_alunos_estadual <- subset(surtos_idade_escola, surtos_idade_escola$INSTITUICAO == "Pública Estadual")
sum(surtos_alunos_estadual$CASO)


########## Tabela nivel de ensino ############
surtos_idade_escola$CASOS <-1
surtos_infantil <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Infantil")
sum (surtos_infantil$CASO)
surtos_fundamental <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Fundamental")
sum (surtos_fundamental$CASO)
surtos_medio <- subset(surtos_idade_escola, surtos_idade_escola$ESCOLARIDADE == "Ensino Médio")
sum (surtos_medio$CASO)

############# SURTOS NÍVEL DE ENSINO - PROFESSORES #####################
surtos_nivel_prof <- subset(surtos_tot, surtos_tot$ALUNO_PROF 
                       %in% c("Professor ou auxiliar de sala", 
                              "Professor ou auxiliar de sala do ensino infantil",
                              "Professor do ensino fundamental",
                              "Professor do ensino médio",
                              "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                              "Professor do ensino fundamental e médio",
                              "Outros professores (como de universidade; escola para adultos)"))



surtos_nivel_prof$ESCOLARIDADE <- ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala do ensino infantil", "Ensino Infantil",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino fundamental","Ensino Fundamental", 
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino médio" ,"Ensino Médio",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor ou auxiliar de sala de ensino infantil ou fundamental", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Professor do ensino fundamental e médio", "Ignorado",
                                  ifelse(surtos_nivel_prof$ALUNO_PROF == "Outros professores (como de universidade; escola para adultos)", "Outros Professores", NA))))))) 


surtos_idade_escola_prof <- surtos_nivel_prof %>%
  group_by(ESCOLA,ESCOLARIDADE, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

#Grafico surto escola por nível de ensino
ggplot(surtos_idade_escola_prof, aes(x= ESCOLA, y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino') +
  scale_y_continuous(limits=c(0, 60))

surtos_idade_pura_prof <- surtos_idade_escola_prof %>%
  group_by(ESCOLARIDADE) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))
sum(surtos_idade_escola_prof$CASO)

#Grafico casos por nível de ensino professor
ggplot(surtos_idade_pura_prof, aes(x= reorder(ESCOLARIDADE,CASO), y= CASO, fill=ESCOLARIDADE ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 


###### Tabela nivel ensino prof ########

surtos_infantil_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Infantil")
sum (surtos_infantil_prof$CASO)
surtos_fundamental_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Fundamental")
sum (surtos_fundamental_prof$CASO)
surtos_medio_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ensino Médio")
sum (surtos_medio_prof$CASO)
surtos_outros_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Outros Professores")
sum (surtos_outros_prof$CASO)
surtos_ignorado_prof <- subset(surtos_idade_escola_prof, surtos_idade_escola_prof$ESCOLARIDADE == "Ignorado")
sum (surtos_ignorado_prof$CASO)

#SURTOS EM PROFESSORES
surtos_tot$CATEGORIA <- ifelse(surtos_tot$ALUNO_PROF %in% c("Professor ou auxiliar de sala", 
                                                            "Professor ou auxiliar de sala do ensino infantil",
                                                            "Professor do ensino fundamental",
                                                            "Professor do ensino médio",
                                                            "Professor ou auxiliar de sala de ensino infantil ou fundamental",
                                                            "Professor do ensino fundamental e médio",
                                                            "Outros professores (como de universidade; escola para adultos)"),
                                                            "Professor ou auxiliar de sala",
                        ifelse(surtos_tot$ALUNO_PROF %in% c("Aluno", "Aluno do ensino infantil",
                                                            "Aluno do ensino fundamental",
                                                            "Aluno do ensino médio",
                                                            "Aluno de idiomas",
                                                            "Outros alunos (universidade; escola para adultos; por exemplo)"), 
                                                            "Aluno",
                        ifelse(surtos_tot$ALUNO_PROF %in% c("Outros colaboradores"), 
                                                            "Outros colaboradores", NA)))
                                             
                              
  
surtos_prof <- subset(surtos_tot, surtos_tot$CATEGORIA == "Professor ou auxiliar de sala")

surtos_prof_instituicao <- surtos_prof %>%
  group_by(ESCOLA,CATEGORIA, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

sum(surtos_prof_instituicao$CASO)

surtos_prof_privada <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Privada")
sum(surtos_prof_privada$CASO)

surtos_prof_municipal <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Pública Municipal")
sum(surtos_prof_municipal$CASO)

surtos_prof_estadual <- subset(surtos_prof_instituicao, surtos_prof_instituicao$INSTITUICAO == "Pública Estadual")
sum(surtos_prof_estadual$CASO)

############## #SURTOS OUTROS COLABORADORES ################
surtos_outros_colab <- subset(surtos_tot, surtos_tot$CATEGORIA == "Outros colaboradores")
surtos_outros_instituicao <- surtos_outros_colab  %>%
  group_by(ESCOLA,CATEGORIA, INSTITUICAO) %>%
  summarise(CASO = sum(CASO, na.rm = T))%>%
  arrange(desc(CASO))

sum(surtos_outros_colab$CASO)

surtos_outros_privada <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Privada")
sum(surtos_outros_privada$CASO)

surtos_outros_municipal <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Pública Municipal")
sum(surtos_outros_municipal$CASO)

surtos_outros_estadual <- subset(surtos_outros_instituicao, surtos_outros_instituicao$INSTITUICAO == "Pública Estadual")
sum(surtos_outros_estadual$CASO)


#########################   ANALISE DE SURTOS ATIVOS ##############################################
library(tidyverse)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

monitora_escola_ativo <- notificaescola %>% dplyr::select(`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                          
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




########################  MEU DATA FRAME DE SURTOS ###############################################

placar_surtos <- data.frame(Escolas = c('PRIVADAS', 'MUNICIPAIS', 'ESTADUAIS', 'FEDERAIS', 'TOTAL'),
                         "Total Surtos/Escola" = c(sum (tot_surtos_privada$CASO),sum (tot_surtos_munic$CASO),
                          sum (tot_surtos_estadual$CASO), 0, sum (surtos_sum$CASO)), 

                        "Total de Casos" = c(sum (tot_surtos_privada$CASOS_ESCOLA), sum(tot_surtos_munic$CASOS_ESCOLA),
                         sum(tot_surtos_estadual$CASOS_ESCOLA), 0, sum(base_unificada$Casos)), 

                         "Alunos" = c(sum(surtos_alunos_privada$CASO), sum(surtos_alunos_municipal$CASO),
                        sum(surtos_alunos_estadual$CASO), 0, sum(surtos_idade_escola$CASO)), 

                      "Professores" = c(sum(surtos_prof_privada$CASO), sum(surtos_prof_municipal$CASO),
                       sum(surtos_prof_estadual$CASO), 0, sum(surtos_prof_instituicao$CASO)) ,


                      "Outros Colaboradores" = c(sum(surtos_outros_privada$CASO), sum(surtos_outros_municipal$CASO),
                      sum(surtos_outros_estadual$CASO), 0, sum(surtos_outros_colab$CASO)),
  
                      "Surtos Ativos/Escola" = c (sum(suativo_priv$CASO), sum (suativo_munic$CASO),
                      sum(suativo_estadual$CASO), 0 , sum(surtos_escola$CASO)))

# DADOS EM CSV 
write.csv(placar_surtos, "placar_surtos.csv", row.names = F)


############ DATA FRAME POR NIVEL DE ENSINO ####################### 
placar_surtos_escolaridade <- data.frame("Nível de Ensino" = c('INFANTIL', 'FUNDAMENTAL', 'MEDIO', 'OUTROS', 'IGNORADOS'),
                                         "Alunos" = c(sum (surtos_infantil$CASO),sum (surtos_fundamental$CASO),
                                                      sum (surtos_medio$CASO), 0, 0),
                                         "Professores"= c(sum (surtos_infantil_prof$CASO), sum (surtos_fundamental_prof$CASO), 
                                                          sum (surtos_medio_prof$CASO), sum (surtos_outros_prof$CASO),
                                                          sum (surtos_ignorado_prof$CASO)))
                                         

# DADOS EM CSV 
write.csv(placar_surtos_escolaridade, "placar_surtos_escolaridade.csv", row.names = F)
                                      