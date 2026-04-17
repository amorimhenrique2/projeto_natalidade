#abrindo o arquivo

#tarefa 1
library(readr)
library(readxl)

dados = read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados)

dados_sinasc <-  read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados_sinasc)
str(dados_sinasc)

#tarefa 2
dados_sinasc_1 <- dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)] 
dados_sinasc_1$CODMUNRES <- as.character(dados_sinasc_1$CODMUNRES)

#tarefa 3
dados_sinasc_2 <- dados_sinasc_1[substr(dados_sinasc_1$CODMUNRES, 1, 2) == "23", ]

#tarefa 4

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$APGAR5)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)


#TAREFA 5
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$RACACOR[dados_sinasc_2$RACACOR == 9] <- NA

#tarefa 6 
# SEXO
dados_sinasc_2$SEXO <- factor(
  dados_sinasc_2$SEXO,
  levels = c(1,2),
  labels = c("Masculino", "Feminino")
)

# PARTO
dados_sinasc_2$PARTO <- factor(
  dados_sinasc_2$PARTO,
  levels = c(1,2),
  labels = c("Vaginal", "Cesáreo")
)

# GRAVIDEZ
dados_sinasc_2$GRAVIDEZ <- factor(
  dados_sinasc_2$GRAVIDEZ,
  levels = c(1,2,3),
  labels = c("Única", "Dupla", "Tripla ou mais")
)

# ESTADO CIVIL
dados_sinasc_2$ESTCIVMAE <- factor(
  dados_sinasc_2$ESTCIVMAE,
  levels = c(1,2,3,4,5),
  labels = c("Solteira", "Casada", "Viúva", "Separada", "União estável")
)

# KOTELCHUCK
dados_sinasc_2$KOTELCHUCK <- factor(
  dados_sinasc_2$KOTELCHUCK,
  levels = c(1,2,3,4,5),
  labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado")
)
#tarefa 7

dados_sinasc_2$IDADEMAE <- as.numeric(dados_sinasc_2$IDADEMAE)
dados_sinasc_2$PESO <- as.numeric(dados_sinasc_2$PESO)
dados_sinasc_2$APGAR5 <- as.numeric(dados_sinasc_2$APGAR5)

# PESO
dados_sinasc_2$F_PESO <- NA
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO < 2500] <- "Baixo peso"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000] <- "Peso normal"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 4000] <- "Macrossomia"
dados_sinasc_2$F_PESO <- factor(dados_sinasc_2$F_PESO)

# IDADE
dados_sinasc_2$F_IDADE <- NA
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE < 15] <- "<15"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19] <- "15-19"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24] <- "20-24"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29] <- "25-29"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34] <- "30-34"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39] <- "35-39"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44] <- "40-44"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49] <- "45-49"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 50] <- "50+"
dados_sinasc_2$F_IDADE <- factor(dados_sinasc_2$F_IDADE)

# APGAR5
dados_sinasc_2$F_APGAR5 <- NA
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 < 7] <- "Baixo"
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 >= 7] <- "Normal"
dados_sinasc_2$F_APGAR5 <- factor(dados_sinasc_2$F_APGAR5)

# PEREGRINAÇÃO
dados_sinasc_2$PERIG <- NA
dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES] <- "Não"
dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC != dados_sinasc_2$CODMUNRES] <- "Sim"
dados_sinasc_2$PERIG <- factor(dados_sinasc_2$PERIG)

# ESTADO CIVIL (agrupado)
dados_sinasc_2$ESTCIV <- NA
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c(1,3,4)] <- "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c(2,5)] <- "Com companheiro"
dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV)

