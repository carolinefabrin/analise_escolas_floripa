#23/03/2021 - ANALISE POR ESCOLARIDADE DE SUSPEITOS/CASOS CONFIRMADOS 
library(tidyverse)
library(dplyr)
source("CUIDADO_link_com_a_base.R")
library(readxl)
Censo_2019 <- read_excel("C:/Users/carol/Downloads/Censo 2019.xlsx")
View(Censo_2019)

notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)

notif_geral <- notificaescola %>% dplyr::select(`O caso suspeito ou confirmado é aluno ou funcionário?`,
                                                 `Se aluno ou professor, qual a turma? Se outros colaboradores, em qual setor trabalha?`, 
                                                 `Nome da escola`,
                                                `Após análise do caso, o caso é: OBRIGATÒRIO!!!*Se suspeito, tem que ver resultado do exame e mudar!`,
                                                `Idade da Pessoa* fórmula`) 

names(notif_geral) <- c('ALUNO_PROF', 'TURMA', 'ESCOLA','DIAGNOSTICO','IDADE')
notif_geral$TURMA <- ifelse(notif_geral$TURMA == 'NULL', NA, notif_geral$TURMA)

#Analise Casos confirmados x faixa etária (mantido casos finalizados)
notif_geral_conf <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% c("Confirmado visto laudo","Confirmado") 
                           & notif_geral$ALUNO_PROF %in% c("aluno", "Aluno", "Outros alunos (universidade; escola para adultos; por exemplo)",
                                                           "Aluno do ensino infantil",
                                                           "Aluno do ensino fundamental",
                                                           "Aluno do ensino médio")) %>%
  arrange(IDADE)
notif_geral_conf$TURMA <- NULL
notif_geral_conf <- notif_geral_conf[!is.na(notif_geral_conf$IDADE),]

notif_geral_conf$FAIXA_ETARIA <- ifelse(notif_geral_conf$IDADE < 6,"Ensino Infantil",
                                    ifelse(notif_geral_conf$IDADE >= 6 & notif_geral_conf$IDADE < 15,"Ensino Fundamental", 
                                           ifelse(notif_geral_conf$IDADE >= 15 & notif_geral_conf$IDADE < 18,"Ensino Médio", NA)))

notif_geral_conf$CASOS <- 1

confirmados_idade_escola <- notif_geral_conf %>%
  group_by(ESCOLA,FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))
sum(confirmados_idade_escola$CASOS)

#Dados em CSV
write.csv(confirmados_idade_escola, "confirmados_idade_escola.csv", row.names = F)

ggplot(confirmados_idade_escola, aes(x= ESCOLA, y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0, 25))

casos_idade_pura <- notif_geral_conf %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(casos_idade_pura, "casos_idade_pura.csv", row.names = F)

ggplot(casos_idade_pura, aes(x= reorder(FAIXA_ETARIA,CASOS), y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Casos") +
  labs(fill='Nível de Ensino') 

#Analise Casos suspeitos X faixa etária 
notif_geral_suspeitos <- subset(notif_geral, notif_geral$DIAGNOSTICO %in% c("Suspeito","Suspeito e recusou fazer teste"))
notif_geral_suspeitos <- subset(notif_geral_suspeitos, notif_geral_suspeitos$ALUNO_PROF %in% c("Aluno", "aluno"))%>%
                          arrange(IDADE)
notif_geral_suspeitos$TURMA <- NULL
notif_geral_suspeitos <- notif_geral_suspeitos[!is.na(notif_geral_suspeitos$IDADE),]

notif_geral_suspeitos$FAIXA_ETARIA <- ifelse(notif_geral_suspeitos$IDADE < 6,"Ensino Infantil",
                                        ifelse(notif_geral_suspeitos$IDADE >= 6 & notif_geral_suspeitos$IDADE < 15,"Ensino Fundamental", 
                                               ifelse(notif_geral_suspeitos$IDADE >= 15 & notif_geral_suspeitos$IDADE < 18,"Ensino Médio", NA)))
notif_geral_suspeitos <- notif_geral_suspeitos[!is.na(notif_geral_suspeitos$FAIXA_ETARIA),]

notif_geral_suspeitos$CASOS <- 1
suspeitos_idade_escola <- notif_geral_suspeitos %>%
  group_by(ESCOLA,FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

#Dados em CSV
write.csv(suspeitos_idade_escola, "suspeitos_idade_escola.csv", row.names = F)

ggplot(suspeitos_idade_escola, aes(x= ESCOLA, y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0,6))

suspeitos_idade_pura <- notif_geral_suspeitos %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(suspeitos_idade_pura, "suspeitos_idade_pura.csv", row.names = F)

ggplot(suspeitos_idade_pura, aes(x= reorder(FAIXA_ETARIA,CASOS), y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Suspeitos") +
  labs(fill='Nível de Ensino')+
  scale_y_continuous(limits=c(0, 20))

#Analise Casos descartados X faixa etária 
notif_geral_descartados <- subset(notif_geral, notif_geral$DIAGNOSTICO == "Descartado")
notif_geral_descartados <- subset(notif_geral_descartados, notif_geral_descartados$ALUNO_PROF %in% c("Aluno", "aluno"))%>%
  arrange(IDADE)
notif_geral_descartados$TURMA <- NULL
notif_geral_descartados <- notif_geral_descartados[!is.na(notif_geral_descartados$IDADE),]

notif_geral_descartados$FAIXA_ETARIA <- ifelse(notif_geral_descartados$IDADE < 6,"Ensino Infantil",
                                             ifelse(notif_geral_descartados$IDADE >= 6 & notif_geral_descartados$IDADE < 15,"Ensino Fundamental", 
                                                    ifelse(notif_geral_descartados$IDADE >= 15 & notif_geral_descartados$IDADE < 18,"Ensino Médio", NA)))
notif_geral_descartados<- notif_geral_descartados[!is.na(notif_geral_descartados$FAIXA_ETARIA),]

notif_geral_descartados$CASOS <- 1
descartados_idade_escola <- notif_geral_descartados %>%
  group_by(ESCOLA,FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))%>%
  arrange(desc(CASOS))

#Dados em CSV
write.csv(descartados_idade_escola, "descartados_idade_escola.csv", row.names = F)

ggplot(descartados_idade_escola, aes(x= ESCOLA, y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados")+
  labs(fill='Nível de Ensino')+ 
  scale_y_continuous(limits=c(0,15))

descartados_idade_pura <- notif_geral_descartados %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(CASOS = sum(CASOS, na.rm = T))

#Dados em CSV
write.csv(descartados_idade_pura, "descartados_idade_pura.csv", row.names = F)

ggplot(descartados_idade_pura, aes(x= reorder(FAIXA_ETARIA,CASOS), y= CASOS, fill=FAIXA_ETARIA ))+
  geom_col()+
  theme_bw()+
  coord_flip()+ 
  xlab(" ")+
  ylab("Descartados") +
  labs(fill='Nível de Ensino')+
  scale_y_continuous(limits=c(0, 30))

#População afetada Censo 2019

