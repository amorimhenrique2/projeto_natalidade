#abrindo o arquivo

#tarefa 1
library(readr)
library(readxl)


dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)
# -------------------------------
# Tarefa 2: Seleção de colunas
# -------------------------------
dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)


summary(dados_sinasc_1$CODMUNRES)
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)


# -------------------------------
# Tarefa 3: Filtrar CEARÁ (23)
# -------------------------------

dados_sinasc_2 = dados_sinasc_1[UF == "23", ]

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

rm(dados_sinasc, dados_sinasc_1)
gc()


# -------------------------------
# Tarefa 4: Frequências
# -------------------------------

freq_LOCNASC <- table(dados_sinasc_2$LOCNASC)
freq_ESTCIVMAE <- table(dados_sinasc_2$ESTCIVMAE)
freq_GESTACAO <- table(dados_sinasc_2$GESTACAO)
freq_GRAVIDEZ <- table(dados_sinasc_2$GRAVIDEZ)
freq_PARTO <- table(dados_sinasc_2$PARTO)
freq_SEXO <- table(dados_sinasc_2$SEXO)
freq_APGAR5 <- table(dados_sinasc_2$APGAR5)
freq_RACACOR <- table(dados_sinasc_2$RACACOR)
freq_IDANOMAL <- table(dados_sinasc_2$IDANOMAL)
freq_ESCMAE2010 <- table(dados_sinasc_2$ESCMAE2010)
freq_RACACORMAE <- table(dados_sinasc_2$RACACORMAE)
freq_TPAPRESENT <- table(dados_sinasc_2$TPAPRESENT)
freq_TPROBSON <- table(dados_sinasc_2$TPROBSON)
freq_PARIDADE <- table(dados_sinasc_2$PARIDADE)
freq_KOTELCHUCK <- table(dados_sinasc_2$KOTELCHUCK)


table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)



# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)


# -------------------------------
# Tarefa 5: Tratar NA
# -------------------------------
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)


n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)


# -------------------------------
# Tarefa 6: Labels (exemplo)
# -------------------------------


dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# -------------------------------
# Tarefa 7: Novas variáveis
# -------------------------------

dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))


#tarefa 8

tabela_pig = read.csv( "Tabela_PIG_Brasil.csv", header = TRUE, sep=";")



tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))



dados_sinasc_2$SEMAGESTAC <- as.numeric(dados_sinasc_2$SEMAGESTAC)
tabela_pig$SEMAGESTAC <- as.numeric(tabela_pig$SEMAGESTAC)



names(tabela_pig)[names(tabela_pig) == "SEXO"] <- "SEXO_PIG"

 
dados_sinasc_2 <- merge(
  dados_sinasc_2,
  tabela_pig,
  by = "SEMAGESTAC",
  all.x = TRUE
)

# Criar F_PIG
dados_sinasc_2$F_PIG <- ifelse(
  dados_sinasc_2$GRAVIDEZ != 1,
  NA,
  ifelse(
    is.na(dados_sinasc_2$PESO) |
      is.na(dados_sinasc_2$PESO_P10) |
      is.na(dados_sinasc_2$PESO_P90),
    NA,
    ifelse(
      dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
      ifelse(
        dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG",
        "GIG"
      )
    )
  )
)


dados_sinasc_2$F_PIG <- factor(
  dados_sinasc_2$F_PIG,
  levels = c("PIG","AIG","GIG")
)

#TAREFA 9


# Frequências principais
freq_LOCNASC <- table(dados_sinasc_2$LOCNASC)
freq_ESTCIVMAE <- table(dados_sinasc_2$ESTCIVMAE)
freq_GESTACAO <- table(dados_sinasc_2$GESTACAO)
freq_GRAVIDEZ <- table(dados_sinasc_2$GRAVIDEZ)
freq_PARTO <- table(dados_sinasc_2$PARTO)
freq_SEXO <- table(dados_sinasc_2$SEXO)
freq_APGAR5 <- table(dados_sinasc_2$F_APGAR5)
freq_RACACOR <- table(dados_sinasc_2$RACACOR)
freq_ESCMAE2010 <- table(dados_sinasc_2$ESCMAE2010)
freq_RACACORMAE <- table(dados_sinasc_2$RACACORMAE)
freq_TPAPRESENT <- table(dados_sinasc_2$TPAPRESENT)
freq_TPROBSON <- table(dados_sinasc_2$TPROBSON)
freq_PARIDADE <- table(dados_sinasc_2$PARIDADE)
freq_KOTELCHUCK <- table(dados_sinasc_2$KOTELCHUCK)

# Variáveis criadas
freq_F_PESO <- table(dados_sinasc_2$F_PESO)
freq_F_IDADE <- table(dados_sinasc_2$F_IDADE)
freq_PERIG <- table(dados_sinasc_2$PERIG)
freq_ESTCIV <- table(dados_sinasc_2$ESTCIV)
freq_F_PIG <- table(dados_sinasc_2$F_PIG)


#. MEDIDAS — IDADE DA MÃE
IM_P25 <- quantile(dados_sinasc_2$IDADEMAE, 0.25, na.rm = TRUE)
IM_P50 <- quantile(dados_sinasc_2$IDADEMAE, 0.50, na.rm = TRUE)
IM_P75 <- quantile(dados_sinasc_2$IDADEMAE, 0.75, na.rm = TRUE)
IM_MD  <- mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE)
IM_DP  <- sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE)



#. MEDIDAS — PESO

PESO_P25 <- quantile(dados_sinasc_2$PESO, 0.25, na.rm = TRUE)
PESO_P50 <- quantile(dados_sinasc_2$PESO, 0.50, na.rm = TRUE)
PESO_P75 <- quantile(dados_sinasc_2$PESO, 0.75, na.rm = TRUE)
PESO_MD  <- mean(dados_sinasc_2$PESO, na.rm = TRUE)
PESO_DP  <- sd(dados_sinasc_2$PESO, na.rm = TRUE)


#. MEDIDAS — DURAÇÃO DA GESTAÇÃO
DG_P25 <- quantile(dados_sinasc_2$SEMAGESTAC, 0.25, na.rm = TRUE)
DG_P50 <- quantile(dados_sinasc_2$SEMAGESTAC, 0.50, na.rm = TRUE)
DG_P75 <- quantile(dados_sinasc_2$SEMAGESTAC, 0.75, na.rm = TRUE)
DG_MD  <- mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE)
DG_DP  <- sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE)



#. MEDIDAS — APGAR
APG5_MD <- mean(dados_sinasc_2$APGAR5, na.rm = TRUE)
APG5_DP <- sd(dados_sinasc_2$APGAR5, na.rm = TRUE)


#tarefa 10
criar_linha <- function(df, uf_nome = "CE") {
  
  data.frame(
    
    ANO = 2015,
    UFR = uf_nome,
    
    TN = nrow(df),
    TNRC = sum(complete.cases(df)),
    
    TGI_15 = sum(df$IDADEMAE < 15, na.rm = TRUE),
    TGI_15_19 = sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 19, na.rm = TRUE),
    TGI_20_24 = sum(df$IDADEMAE >= 20 & df$IDADEMAE <= 24, na.rm = TRUE),
    TGI_25_29 = sum(df$IDADEMAE >= 25 & df$IDADEMAE <= 29, na.rm = TRUE),
    TGI_30_34 = sum(df$IDADEMAE >= 30 & df$IDADEMAE <= 34, na.rm = TRUE),
    TGI_35_39 = sum(df$IDADEMAE >= 35 & df$IDADEMAE <= 39, na.rm = TRUE),
    TGI_40_44 = sum(df$IDADEMAE >= 40 & df$IDADEMAE <= 44, na.rm = TRUE),
    TGI_45_49 = sum(df$IDADEMAE >= 45 & df$IDADEMAE <= 49, na.rm = TRUE),
    TGI_50 = sum(df$IDADEMAE >= 50, na.rm = TRUE),
    
    TGIF = sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 49, na.rm = TRUE),
    
    IM_P25 = quantile(df$IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(df$IDADEMAE, 0.5, na.rm = TRUE),
    IM_P75 = quantile(df$IDADEMAE, 0.75, na.rm = TRUE),
    IM_MD = mean(df$IDADEMAE, na.rm = TRUE),
    IM_DP = sd(df$IDADEMAE, na.rm = TRUE),
    
    EM_S = sum(df$ESCMAE2010 == 0, na.rm = TRUE),
    EM_FI = sum(df$ESCMAE2010 == 1, na.rm = TRUE),
    EM_FII = sum(df$ESCMAE2010 == 2, na.rm = TRUE),
    EM_M = sum(df$ESCMAE2010 == 3, na.rm = TRUE),
    EM_SI = sum(df$ESCMAE2010 == 4, na.rm = TRUE),
    EM_SC = sum(df$ESCMAE2010 == 5, na.rm = TRUE),
    
    TGRC_B = sum(df$RACACORMAE == 1, na.rm = TRUE),
    TGRC_PT = sum(df$RACACORMAE == 2, na.rm = TRUE),
    TGRC_A = sum(df$RACACORMAE == 3, na.rm = TRUE),
    TGRC_PD = sum(df$RACACORMAE == 4, na.rm = TRUE),
    TGRC_I = sum(df$RACACORMAE == 5, na.rm = TRUE),
    
    TGSC = sum(df$ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(df$ESTCIV == "Com companheiro", na.rm = TRUE),
    
    TGPRI = sum(df$PARIDADE == 1, na.rm = TRUE),
    TGNPRI = sum(df$PARIDADE != 1, na.rm = TRUE),
    
    TGU = sum(df$GRAVIDEZ == 1, na.rm = TRUE),
    TGG = sum(df$GRAVIDEZ == 2, na.rm = TRUE),
    
    TGD_22 = sum(df$GESTACAO == 1, na.rm = TRUE),
    TGD_22_27 = sum(df$GESTACAO == 2, na.rm = TRUE),
    TGD_28_31 = sum(df$GESTACAO == 3, na.rm = TRUE),
    TGD_32_36 = sum(df$GESTACAO == 4, na.rm = TRUE),
    TGD_37_41 = sum(df$GESTACAO == 5, na.rm = TRUE),
    TGD_42 = sum(df$GESTACAO == 6, na.rm = TRUE),
    
    TGD_PRT = sum(df$SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT = sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(df$SEMAGESTAC >= 42, na.rm = TRUE),
    
    DG_P25 = quantile(df$SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(df$SEMAGESTAC, 0.5, na.rm = TRUE),
    DG_P75 = quantile(df$SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD = mean(df$SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(df$SEMAGESTAC, na.rm = TRUE),
    
    TKC_NR = sum(df$KOTELCHUCK == 1, na.rm = TRUE),
    TKC_ID = sum(df$KOTELCHUCK == 2, na.rm = TRUE),
    TKC_IT = sum(df$KOTELCHUCK == 3, na.rm = TRUE),
    TKC_AD = sum(df$KOTELCHUCK == 4, na.rm = TRUE),
    TKC_MAD = sum(df$KOTELCHUCK == 5, na.rm = TRUE),
    
    TGPRG_S = sum(df$PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(df$PERIG == "Nao", na.rm = TRUE),
    
    TPV = sum(df$PARTO == 1, na.rm = TRUE),
    TPC = sum(df$PARTO == 2, na.rm = TRUE),
    
    TRAP_C = sum(df$TPAPRESENT == 1, na.rm = TRUE),
    TRAP_P = sum(df$TPAPRESENT == 2, na.rm = TRUE),
    TRAP_T = sum(df$TPAPRESENT == 3, na.rm = TRUE),
    
    TGROB_1 = sum(df$TPROBSON == 1, na.rm = TRUE),
    TGROB_2 = sum(df$TPROBSON == 2, na.rm = TRUE),
    TGROB_3 = sum(df$TPROBSON == 3, na.rm = TRUE),
    TGROB_4 = sum(df$TPROBSON == 4, na.rm = TRUE),
    TGROB_5 = sum(df$TPROBSON == 5, na.rm = TRUE),
    TGROB_6 = sum(df$TPROBSON == 6, na.rm = TRUE),
    TGROB_7 = sum(df$TPROBSON == 7, na.rm = TRUE),
    TGROB_8 = sum(df$TPROBSON == 8, na.rm = TRUE),
    TGROB_9 = sum(df$TPROBSON == 9, na.rm = TRUE),
    TGROB_10 = sum(df$TPROBSON == 10, na.rm = TRUE),
    
    TNLOC_H = sum(df$LOCNASC == 1, na.rm = TRUE),
    TNLOC_ES = sum(df$LOCNASC == 2, na.rm = TRUE),
    TNLOC_D = sum(df$LOCNASC == 3, na.rm = TRUE),
    TNLOC_O = sum(df$LOCNASC == 4, na.rm = TRUE),
    TNLOC_AI = sum(df$LOCNASC == 5, na.rm = TRUE),
    
    TRRC_B = sum(df$RACACOR == 1, na.rm = TRUE),
    TRRC_PT = sum(df$RACACOR == 2, na.rm = TRUE),
    TRRC_A = sum(df$RACACOR == 3, na.rm = TRUE),
    TRRC_PD = sum(df$RACACOR == 4, na.rm = TRUE),
    TRRC_I = sum(df$RACACOR == 5, na.rm = TRUE),
    
    TRP_BP = sum(df$F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(df$F_PESO == "Normal", na.rm = TRUE),
    TRP_M = sum(df$F_PESO == "Macrossomia", na.rm = TRUE),
    
    PESO_P25 = quantile(df$PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(df$PESO, 0.5, na.rm = TRUE),
    PESO_P75 = quantile(df$PESO, 0.75, na.rm = TRUE),
    PESO_MD = mean(df$PESO, na.rm = TRUE),
    PESO_DP = sd(df$PESO, na.rm = TRUE),
    
    TRPIG_P = sum(df$F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(df$F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(df$F_PIG == "GIG", na.rm = TRUE),
    
    TRAPG5_B = sum(df$F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(df$F_APGAR5 == "Normal", na.rm = TRUE),
    
    APG5_MD = mean(df$APGAR5, na.rm = TRUE),
    APG5_DP = sd(df$APGAR5, na.rm = TRUE),
    
    TRAC = sum(df$IDANOMAL == 1, na.rm = TRUE),
    TRSAC = sum(df$IDANOMAL == 2, na.rm = TRUE),
    
    EXTRA1 = NA,
    EXTRA2 = NA,
    EXTRA3 = NA,
    EXTRA4 = NA
    
  )
}

linha_total <- criar_linha(dados_sinasc_2, "CE")

lista_municipios <- split(dados_sinasc_2, dados_sinasc_2$CODMUNRES)

linhas_municipios <- do.call(rbind,
                             lapply(lista_municipios, function(x) criar_linha(x, "CE"))
)

SINASC_CE <- rbind(linha_total, linhas_municipios)


#tarefa 11
write.csv(SINASC_CE, "SINASC_CE.csv", row.names = FALSE)






#ETAPA 2

#TAREFA 1

dados_sim = read.csv("Mortalidade_Geral_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)

#TAREFA 2
dados_sim_1 = dados_sim[, c(1,3,4,8,9,10,11,14,17,35,36,37,47,77,84)]

colnames(dados_sim_1) = c(
  "CONTADOR",
  "TIPOBITO",
  "DTOBITO",
  "DTNASC",
  "IDADE",
  "SEXO",
  "RACACOR",
  "ESC2010",
  "CODMUNRES",
  "TPMORTEOCO",
  "OBITOGRAV",
  "OBITOPUERP",
  "CAUSABAS",
  "TPOBITOCOR",
  "MORTEPARTO"
)

#TAREFA 3
dados_sim_2 = subset(dados_sim_1, substr(CODMUNRES,1,2) == "23")

table(substr(dados_sim_2$CODMUNRES,1,2))

write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)


#TAREFA 4
table(dados_sim_2$TIPOBITO)

table(dados_sim_2$SEXO)

table(dados_sim_2$RACACOR)

table(dados_sim_2$TPMORTEOCO)

table(dados_sim_2$OBITOGRAV)

table(dados_sim_2$OBITOPUERP)

table(dados_sim_2$CAUSABAS)

table(dados_sim_2$TPOBITOCOR)

table(dados_sim_2$MORTEPARTO)

freq_TIPOBITO <- table(dados_sim_2$TIPOBITO)

freq_SEXO <- table(dados_sim_2$SEXO)

freq_RACACOR <- table(dados_sim_2$RACACOR)

freq_TPMORTEOCO <- table(dados_sim_2$TPMORTEOCO)

freq_OBITOGRAV <- table(dados_sim_2$OBITOGRAV)

freq_OBITOPUERP <- table(dados_sim_2$OBITOPUERP)

freq_CAUSABAS <- table(dados_sim_2$CAUSABAS)

freq_TPOBITOCOR <- table(dados_sim_2$TPOBITOCOR)

freq_MORTEPARTO <- table(dados_sim_2$MORTEPARTO)


#valores das variáveis quantitativas

unique(dados_sim_2$IDADE)

summary(dados_sim_2$IDADE)


#TAREFA 5
dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] <- NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 9] <- NA

dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA

dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA

dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] <- NA

dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] <- NA

dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] <- NA

dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA

dados_sim_2$IDADE[dados_sim_2$IDADE == 999] <- NA


#tAREFA 6
dados_sim_2$TIPOBITO <- factor(
  dados_sim_2$TIPOBITO,
  levels = c(1,2),
  labels = c("Fetal","Nao fetal")
)

dados_sim_2$SEXO <- factor(
  dados_sim_2$SEXO,
  levels = c("M","F","I"),
  labels = c("Masculino","Feminino","Ignorado")
)

dados_sim_2$RACACOR <- factor(
  dados_sim_2$RACACOR,
  levels = c(1,2,3,4,5,9),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indigena",
    "Ignorado"
  )
)

dados_sim_2$ESC2010 <- factor(
  dados_sim_2$ESC2010,
  levels = c(0,1,2,3,4,5,9),
  labels = c(
    "Sem escolaridade",
    "Fundamental I",
    "Fundamental II",
    "Medio",
    "Superior incompleto",
    "Superior completo",
    "Ignorado"
  )
)

dados_sim_2$TPMORTEOCO <- factor(
  dados_sim_2$TPMORTEOCO,
  levels = c(1,2,3,4,5,8,9),
  labels = c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Ate 42 dias apos o parto",
    "De 43 dias a 1 ano apos o parto",
    "Nao ocorreu nestes periodos",
    "Ignorado"
  )
)

dados_sim_2$OBITOGRAV <- factor(
  dados_sim_2$OBITOGRAV,
  levels = c(1,2,9),
  labels = c(
    "Sim",
    "Nao",
    "Ignorado"
  )
)

dados_sim_2$OBITOPUERP <- factor(
  dados_sim_2$OBITOPUERP,
  levels = c(1,2,3,9),
  labels = c(
    "Sim ate 42 dias",
    "Sim de 43 dias a 1 ano",
    "Nao",
    "Ignorado"
  )
)

dados_sim_2$CAUSABAS <- factor(
  dados_sim_2$CAUSABAS
)

dados_sim_2$TPOBITOCOR <- factor(
  dados_sim_2$TPOBITOCOR,
  levels = c(1,2,3,4,5,9),
  labels = c(
    "Via publica",
    "Endereco residencia",
    "Outro domicilio",
    "Estabelecimento de saude",
    "Outros",
    "Ignorado"
  )
)

dados_sim_2$MORTEPARTO <- factor(
  dados_sim_2$MORTEPARTO,
  levels = c(1,2,3,9),
  labels = c(
    "Antes",
    "Durante",
    "Apos",
    "Ignorado"
  )
)
dados_sim_2$CAUSABAS <- as.character(dados_sim_2$CAUSABAS)

dados_sim_2$CAUSABAS[grep("^C61", dados_sim_2$CAUSABAS)] <- "Neoplasia maligna da prostata"

dados_sim_2$CAUSABAS[grep("^C67", dados_sim_2$CAUSABAS)] <- "Neoplasia maligna da bexiga"

dados_sim_2$CAUSABAS[grep("^C83", dados_sim_2$CAUSABAS)] <- "Linfoma nao Hodgkin"

dados_sim_2$CAUSABAS[grep("^C71", dados_sim_2$CAUSABAS)] <- "Neoplasia maligna do encefalo"

dados_sim_2$CAUSABAS[grep("^C24", dados_sim_2$CAUSABAS)] <- "Neoplasia das vias biliares"

dados_sim_2$CAUSABAS[grep("^C34", dados_sim_2$CAUSABAS)] <- "Neoplasia maligna dos bronquios e pulmao"

dados_sim_2$CAUSABAS[grep("^C22", dados_sim_2$CAUSABAS)] <- "Neoplasia maligna do figado"

dados_sim_2$CAUSABAS[grep("^D38", dados_sim_2$CAUSABAS)] <- "Neoplasia de comportamento incerto"

dados_sim_2$CAUSABAS[grep("^E88", dados_sim_2$CAUSABAS)] <- "Outros disturbios metabolicos"

dados_sim_2$CAUSABAS[grep("^E87", dados_sim_2$CAUSABAS)] <- "Outros transtornos hidroeletroliticos"

dados_sim_2$CAUSABAS[grep("^E10", dados_sim_2$CAUSABAS)] <- "Diabetes mellitus"

dados_sim_2$CAUSABAS[grep("^G30", dados_sim_2$CAUSABAS)] <- "Doenca de Alzheimer"

dados_sim_2$CAUSABAS[grep("^I21", dados_sim_2$CAUSABAS)] <- "Infarto agudo do miocardio"

dados_sim_2$CAUSABAS[grep("^I50", dados_sim_2$CAUSABAS)] <- "Insuficiencia cardiaca"

dados_sim_2$CAUSABAS[grep("^I64", dados_sim_2$CAUSABAS)] <- "Acidente vascular cerebral"

dados_sim_2$CAUSABAS[grep("^J12", dados_sim_2$CAUSABAS)] <- "Pneumonia viral"

dados_sim_2$CAUSABAS[grep("^J18", dados_sim_2$CAUSABAS)] <- "Pneumonia"

dados_sim_2$CAUSABAS[grep("^J44", dados_sim_2$CAUSABAS)] <- "Doenca pulmonar obstrutiva cronica"

dados_sim_2$CAUSABAS[grep("^J43", dados_sim_2$CAUSABAS)] <- "Enfisema"

dados_sim_2$CAUSABAS[grep("^Q07", dados_sim_2$CAUSABAS)] <- "Malformacoes congenitas do sistema nervoso"

dados_sim_2$CAUSABAS[grep("^R54", dados_sim_2$CAUSABAS)] <- "Senilidade"

dados_sim_2$CAUSABAS <- factor(dados_sim_2$CAUSABAS)


#TAREFA 7
criar_linha_sim <- function(df, nivel, codigo){
  
  data.frame(
    
    ANO = 2015,
    
    NIVEL = nivel,
    
    CODMUNRES = codigo,
    
    TO = nrow(df),
    
    TORC = sum(complete.cases(dados_sim), na.rm = TRUE),
    
    TORCR = sum(complete.cases(df), na.rm = TRUE),
    
    TO_NN = sum(grepl("^[VWXY]", df$CAUSABAS), na.rm = TRUE),
    
    TO_N = sum(!grepl("^[VWXY]", df$CAUSABAS), na.rm = TRUE),
    
    TO_CB_I = sum(grepl("^[AB]", df$CAUSABAS), na.rm = TRUE),
    
    TO_CB_N = sum(grepl("^[CD]", df$CAUSABAS), na.rm = TRUE),
    
    TO_CB_C = sum(grepl("^I", df$CAUSABAS), na.rm = TRUE),
    
    TO_CB_R = sum(grepl("^J", df$CAUSABAS), na.rm = TRUE),
    
    TO_CB_O = sum(
      !grepl("^[ABCDIJVWXY]", df$CAUSABAS),
      na.rm = TRUE
    ),
    
    TO_M = sum(df$SEXO == "Masculino", na.rm = TRUE),
    
    TO_F = sum(df$SEXO == "Feminino", na.rm = TRUE),
    
    TO_F_IF = sum(
      df$SEXO == "Feminino" &
        df$IDADE >= 15 &
        df$IDADE <= 49,
      na.rm = TRUE
    ),
    
    TO_FT = sum(df$TIPOBITO == "Fetal", na.rm = TRUE),
    
    TO_NT = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27,
      na.rm = TRUE
    ),
    
    TO_NT_P = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 6,
      na.rm = TRUE
    ),
    
    TO_NT_T = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 7 &
        df$IDADE <= 27,
      na.rm = TRUE
    ),
    
    TO_PNT = sum(
      df$IDADE >= 28 &
        df$IDADE <= 364,
      na.rm = TRUE
    ),
    
    TO_MT_G = sum(
      df$TPMORTEOCO == "Na gravidez",
      na.rm = TRUE
    ),
    
    TONT_B = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27 &
        df$RACACOR == "Branca",
      na.rm = TRUE
    ),
    
    TONT_PT = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27 &
        df$RACACOR == "Preta",
      na.rm = TRUE
    ),
    
    TONT_A = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27 &
        df$RACACOR == "Amarela",
      na.rm = TRUE
    ),
    
    TONT_PD = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27 &
        df$RACACOR == "Parda",
      na.rm = TRUE
    ),
    
    TONT_I = sum(
      df$TIPOBITO == "Nao fetal" &
        df$IDADE >= 0 &
        df$IDADE <= 27 &
        df$RACACOR == "Indigena",
      na.rm = TRUE
    ),
    
    TO_MT = sum(
      !is.na(df$TPMORTEOCO),
      na.rm = TRUE
    ),
    
    TO_MT_DG = sum(
      df$TPMORTEOCO == "Na gravidez",
      na.rm = TRUE
    ),
    
    TO_MT_PT = sum(
      df$TPMORTEOCO == "No parto",
      na.rm = TRUE
    ),
    
    TO_MT_AB = sum(
      df$TPMORTEOCO == "No abortamento",
      na.rm = TRUE
    ),
    
    TO_MT_42 = sum(
      df$TPMORTEOCO == "Ate 42 dias apos o parto",
      na.rm = TRUE
    ),
    
    TO_MT_43 = sum(
      df$TPMORTEOCO == "De 43 dias a 1 ano apos o parto",
      na.rm = TRUE
    ),
    
    TO_MT_P = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ),
      na.rm = TRUE
    ),
    
    TO_MT_P_I = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$IDADE >= 15 &
        df$IDADE <= 49,
      na.rm = TRUE
    ),
    
    TO_MT_P_ES = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Sem escolaridade",
      na.rm = TRUE
    ),
    
    TO_MT_P_EFI = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Fundamental I",
      na.rm = TRUE
    ),
    
    TO_MT_P_EFII = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Fundamental II",
      na.rm = TRUE
    ),
    
    TO_MT_P_EM = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Medio",
      na.rm = TRUE
    ),
    
    TO_MT_P_ESI = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Superior incompleto",
      na.rm = TRUE
    ),
    
    TO_MT_P_ESC = sum(
      df$TPMORTEOCO %in% c(
        "Na gravidez",
        "No parto",
        "No abortamento",
        "Ate 42 dias apos o parto"
      ) &
        df$ESC2010 == "Superior completo",
      na.rm = TRUE
    )
    
  )
  
}

linha_uf <- criar_linha_sim(
  dados_sim_2,
  "UF",
  23
)

lista_municipios <- split(
  dados_sim_2,
  dados_sim_2$CODMUNRES
)

linhas_municipios <- do.call(
  rbind,
  lapply(
    names(lista_municipios),
    function(mun){
      
      criar_linha_sim(
        lista_municipios[[mun]],
        "MUNICIPIO",
        mun
      )
      
    }
  )
)

SIM_CE <- rbind(
  linha_uf,
  linhas_municipios
)




#TAREFA 8
write.csv(SIM_CE, "SIM_CE.csv", row.names = FALSE)

#
