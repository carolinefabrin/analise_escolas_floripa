library(tidyverse)
library(reshape2)
source("CUIDADO_link_com_a_base.R")

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)
publica_privada <- notificaescola %>% dplyr::select (`A pessoa que você quer notificar é aluno ou funcionário?`,
                                                    `Nome da escola`,
                                                    `Data dos primeiros sintomas:`,
                                                    `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                    `É surto?`,
                                                    `Escola pública ou privada?`,
                                                    `Idade da Pessoa* fórmula`) 

names(publica_privada) <- c('ALUNO_PROF', 'ESCOLA', 'DT_PRIM_SINTOM', 'DIAGNOSTICO', 'SURTO', 'INSTITUICAO', 'IDADE')
publica_privada$DT_PRIM_SINTOM <- as.Date(publica_privada$DT_PRIM_SINTOM, format = '%d/%m/%Y')
publica_privada <- publica_privada[!is.na(publica_privada$ESCOLA),]
publica_privada <- publica_privada[!is.na(publica_privada$DIAGNOSTICO),]
publica_privada <- publica_privada[!is.na(publica_privada$ALUNO_PROF),]

publica_privada <- subset(publica_privada, publica_privada$DIAGNOSTICO %in% 
                                 c("Confirmado", "Confirmado visto laudo",
                                   "Suspeito", "Suspeito e recusou exame", 
                                   "Descartado", "Descartado (suspeito que fez exame e foi negativo)"))


################ ESCOLA PRIVADA ###########################

# Calculo de notificados por escola privada
privada_privada <- subset(publica_privada, publica_privada$INSTITUICAO == 
                            "Privada")
privada_privada$CASO <- 1
casos_privada <- privada_privada %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_privada, "casos_privada.csv", row.names = F)

#CALCULO DE POSITIVOS - PRIVADA
privada_privada_conf <- subset(privada_privada, privada_privada$DIAGNOSTICO %in% 
                                 c("Confirmado visto laudo","Confirmado"))
sum(privada_privada_conf$CASO)


################### ESCOLA MUNICIPAL ####################
# Calculo de notificados por escola publica municipal 
publica_municipal <- subset(publica_privada, publica_privada$INSTITUICAO %in% 
                              c("Pública Municipal", "Conveniada com PMF"))

publica_municipal$CASO <- 1
casos_municipal <- publica_municipal %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_municipal, "casos_municipal.csv", row.names = F)

#CALCULO DE POSITIVOS - MUNICIPAL
publica_municipal_conf <- subset(publica_municipal, publica_municipal$DIAGNOSTICO %in% 
                                   c("Confirmado visto laudo","Confirmado"))
sum(publica_municipal_conf$CASO)


######################### ESCOLA ESTADUAL ##############################

# Calculo de notificados por escola estadual
publica_estadual <- subset(publica_privada, publica_privada$INSTITUICAO == 
                             "Pública Estadual")
publica_estadual$CASO <- 1
casos_estadual <- publica_estadual %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_estadual, "casos_estadual.csv", row.names = F)

#CALCULO DE POSITIVOS - ESTADUAL
publica_estadual_conf <- subset(publica_estadual, publica_estadual$DIAGNOSTICO %in% 
                                  c("Confirmado visto laudo","Confirmado"))
sum(publica_estadual_conf$CASO)



######################### ESCOLA FEDERAL ##############################

# Calculo de notificados por escola federal
publica_federal <- subset(publica_privada, publica_privada$INSTITUICAO == 
                             "Pública Federal")
publica_federal$CASO <- 1
casos_federal <- publica_federal %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_federal, "casos_federal.csv", row.names = F)

#CALCULO DE POSITIVOS - FEDERAL
publica_federal_conf <- subset(publica_federal, publica_federal$DIAGNOSTICO %in% 
                                  c("Confirmado visto laudo","Confirmado"))
sum(publica_federal_conf$CASO)

######################### ESCOLA FILANTROPICA ##############################

# Calculo de notificados por escola FILANTROPICA
publica_filantropica <- subset(publica_privada, publica_privada$INSTITUICAO == 
                            "Filantropica")
publica_filantropica$CASO <- 1
casos_filantropica <- publica_filantropica %>%
  group_by(ESCOLA)%>%
  summarise(CASOS_ESCOLA = sum(CASO, na.rm = T))%>%
  arrange(desc(CASOS_ESCOLA))

# DADOS EM CSV 
write.csv(casos_filantropica, "casos_filantropica.csv", row.names = F)

#CALCULO DE POSITIVOS - filantropica
publica_filantropica_conf <- subset(publica_filantropica, publica_filantropica$DIAGNOSTICO %in% 
                                 c("Confirmado visto laudo","Confirmado"))
sum(publica_filantropica_conf$CASO)



#Comparativo casos NOTIFICADOS
tot_casos_privada <- sum(casos_privada$CASOS_ESCOLA)
tot_casos_municipal <- sum(casos_municipal$CASOS_ESCOLA)
tot_casos_estadual <- sum(casos_estadual$CASOS_ESCOLA)
tot_casos_federal <- sum(casos_federal$CASOS_ESCOLA)
tot_casos_filantropica <- sum(casos_filantropica$CASOS_ESCOLA)

total_casos_tipo_escola <- data.frame("Privada" = tot_casos_privada,
                                      "Municipal" = tot_casos_municipal,
                                      "Estadual" = tot_casos_estadual,
                                      "Federal" = tot_casos_federal,
                                      "Filantropica" = tot_casos_filantropica)

total_casos_tipo_escola <- melt(total_casos_tipo_escola)
names(total_casos_tipo_escola) <- c("Tipo", "Quantidade")

ggplot(total_casos_tipo_escola, aes(x= Tipo, y= Quantidade, fill= Tipo ))+
  geom_col()+
  theme_bw()+
  xlab("Escolas")+
  ylab(" Notificações") 

write.csv(total_casos_tipo_escola, "total_casos_tipo_escola.csv", row.names = F)

