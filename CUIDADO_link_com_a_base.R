library(tidyverse)
library (googlesheets4)
id_notificaescola <- "https://docs.google.com/spreadsheets/d/15IuDUWEIL11k0PrSCDvEx7yCCq7OxIhxrVofsL4R0ig/edit#gid=1541566188"
notificaescola <- read_sheet(id_notificaescola, "Casos positivos e suspeitos em escolares", col_names = T, skip = 2)
write.csv(notificaescola, "notificaescola.csv", row.names = F)
