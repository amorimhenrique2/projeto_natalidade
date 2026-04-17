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




