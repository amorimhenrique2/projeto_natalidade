#abrindo o arquivo

library(readr)
library(readxl)

dados = read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados)

dados_sinacs <-  read.csv("SINASC_2015.csv",header = TRUE,sep = ";")
dim(dados_sinacs)
