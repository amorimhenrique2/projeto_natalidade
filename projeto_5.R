#abrindo o arquivo

library(readr)
library(readxl)

dados = read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados)

dados_sinacs <-  read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados_sinacs)
str(dados_sinacs)

dados_sinacs_1 <- dados_sinacs[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)] 
