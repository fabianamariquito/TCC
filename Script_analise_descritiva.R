######################################################################################################
###################################### TCC: RAIS - São Paulo ########################################
#####################################################################################################

#Diretório
setwd("D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA")
#setwd("C:/Users/e-fabiana.sousa/OneDrive - CNI - Confederação Nacional da Indústria/RAIS/BASE DE DADOS/RAIS - CORRIGIDA")

#Bibliotecas

pacotes <- c("ggplot2", "tidyverse", "EnvStats", 
             "knitr", "GGally", "plotly", "moments",
             "readxl", "readr","dplyr", "stringr", 
             "openxlsx","kableExtra","car","multcompView",
             "reshape2","ggrepel","ggmosaic")
lapply(pacotes, library, character.only = T)

# Importando os dados
RAIS_SP<-readRDS("RAIS_SP_TCC.rds")


# Mudando tipo da variável
### Caracter
RAIS_SP$CD_MOTIVO_DESLIGAMENTO<-as.character(RAIS_SP$CD_MOTIVO_DESLIGAMENTO)
RAIS_SP$CD_SEXO<-as.character(RAIS_SP$CD_SEXO)
RAIS_SP$FL_VINCULO_ATIVO_3112_2016<-as.character(RAIS_SP$FL_VINCULO_ATIVO_3112_2016)
RAIS_SP$FL_VINCULO_ATIVO_3112_2017<-as.character(RAIS_SP$FL_VINCULO_ATIVO_3112_2017)
RAIS_SP$FL_VINCULO_ATIVO_3112_2018<-as.character(RAIS_SP$FL_VINCULO_ATIVO_3112_2018)
RAIS_SP$TEMPO_ADM<-as.character(RAIS_SP$TEMPO_ADM)
### Numérica
RAIS_SP$VL_REMUN_MEDIA_NOM<-as.numeric(RAIS_SP$VL_REMUN_MEDIA_NOM_2015)
RAIS_SP$NR_MES_TEMPO_EMPREGO<-as.numeric(RAIS_SP$NR_MES_TEMPO_EMPREGO)
RAIS_SP$VL_IDADE<-as.numeric(RAIS_SP$VL_IDADE)
RAIS_SP$CD_GRAU_INSTRUCAO<-as.numeric(RAIS_SP$CD_GRAU_INSTRUCAO)
RAIS_SP$CD_CNAE20_DIVISAO<-as.numeric(RAIS_SP$CD_CNAE20_DIVISAO)
RAIS_SP$CD_TAMANHO_ESTABELECIMENTO<-as.numeric(RAIS_SP$CD_TAMANHO_ESTABELECIMENTO)

###### Filtrando dados 

### Tipo Vínculo
RAIS_SP <- RAIS_SP %>%
  filter((CD_TIPO_VINCULO %in% c("10","15")))
### Raça cor
RAIS_SP <- RAIS_SP %>%
  filter((CD_RACA_COR %in% c("02","04","06","08")))
### Motivo desligamento
RAIS_SP <- RAIS_SP %>%
  filter(CD_MOTIVO_DESLIGAMENTO %in% c("10","11","12"))
### Faixa de Idade
RAIS_SP <- RAIS_SP %>%
  filter(VL_IDADE > 17 & VL_IDADE < 61)
### Natureza Jurídica
RAIS_SP <- RAIS_SP %>%
  filter(VL_REMUN_MEDIA_SM_2015 >= 1)
### Grau de Instrução
RAIS_SP <- RAIS_SP %>%
  filter(CD_GRAU_INSTRUCAO > 1)
### Setores
RAIS_SP <- RAIS_SP %>%
  filter(CD_CNAE20_DIVISAO > 4)
### Tempo de emprego
RAIS_SP <- RAIS_SP %>%
  filter(NR_MES_TEMPO_EMPREGO >= 1)


##### Categorizando variáveis

## Escolaridade após 2005
RAIS_SP <- RAIS_SP %>%
  mutate(GR_INSTR = ifelse(CD_GRAU_INSTRUCAO > 1 & CD_GRAU_INSTRUCAO <= 5, "N. Fundamental",
                                  ifelse(CD_GRAU_INSTRUCAO > 5 & CD_GRAU_INSTRUCAO <= 7, "N. Médio",
                                         ifelse(CD_GRAU_INSTRUCAO > 7 & CD_GRAU_INSTRUCAO <= 11, "N. Superior",NA))))
RAIS_SP$CD_GRAU_INSTRUCAO<- NULL

## Setores CNAE

RAIS_SP <- RAIS_SP %>%
  mutate(SETOR = ifelse(CD_CNAE20_DIVISAO >= 5 & CD_CNAE20_DIVISAO <= 39, "Indústria",
                               ifelse(CD_CNAE20_DIVISAO >= 40 & CD_CNAE20_DIVISAO <= 43, "Construção",
                                      ifelse(CD_CNAE20_DIVISAO >= 45 & CD_CNAE20_DIVISAO <= 47, "Comércio",
                                             ifelse(CD_CNAE20_DIVISAO >= 49, "Serviços", NA)))))
RAIS_SP$CD_CNAE20_DIVISAO<- NULL


## Tamanho estabelecimento

RAIS_SP <- RAIS_SP %>%
  mutate(PORTE_ESTABELECIMENTO = ifelse(CD_TAMANHO_ESTABELECIMENTO >= 1 & CD_TAMANHO_ESTABELECIMENTO <= 3, "Micro",
                                        ifelse(CD_TAMANHO_ESTABELECIMENTO >= 4 & CD_TAMANHO_ESTABELECIMENTO <= 5, "Pequena",
                                               ifelse(CD_TAMANHO_ESTABELECIMENTO == 6, "Média",
                                                      ifelse(CD_TAMANHO_ESTABELECIMENTO >= 7, "Grande",NA)))))
RAIS_SP$CD_TAMANHO_ESTABELECIMENTO<- NULL

## Raça-cor

RAIS_SP <- RAIS_SP %>%
  mutate(RACA_COR = ifelse(CD_RACA_COR %in% c("02","06"), "Branca e Amarela", "Preta e Parda"))

RAIS_SP$CD_RACA_COR<- NULL

## Tipo Salário

RAIS_SP <- RAIS_SP %>%
  mutate(TIPO_SALARIO = ifelse(CD_TIPO_SALARIO == "01", "Mensal", "Outros"))
RAIS_SP$CD_TIPO_SALARIO<- NULL

## Ano de Retorno

RAIS_SP <- RAIS_SP %>%
  mutate(TEMPO_ADM = ifelse(TEMPO_ADM == "0", "Não Retornou",
         ifelse(TEMPO_ADM == "2016", "2016",
                ifelse(TEMPO_ADM == "2017", "2017",
                       ifelse(TEMPO_ADM == "2018", "2018", NA)))))

## Sexo
RAIS_SP <- RAIS_SP %>%
  mutate(CD_SEXO = ifelse(CD_SEXO == "1","Masculino", "Feminino"))

## motivo de desligamento
RAIS_SP <- RAIS_SP %>%
  mutate(CD_MOTIVO_DESLIGAMENTO = ifelse(CD_MOTIVO_DESLIGAMENTO == "10","Com justa causa",
                                         ifelse(CD_MOTIVO_DESLIGAMENTO == "11", "Sem justa causa",
                                                ifelse( CD_MOTIVO_DESLIGAMENTO == "12", "Término do contrato", NA))))


## Variáveis de UF de admissão

# 2016
RAIS_SP <- RAIS_SP %>%
  mutate(CD_UF_2016 = ifelse(CD_UF_2016  == "35", "Permaneceu","Não Permaneceu"))

# 2017
RAIS_SP <- RAIS_SP %>%
  mutate(CD_UF_2017 = ifelse(CD_UF_2017  == "35", "Permaneceu","Não Permaneceu"))

# 2018
RAIS_SP <- RAIS_SP %>%
  mutate(CD_UF_2018 = ifelse(CD_UF_2018  == "35", "Permaneceu","Não Permaneceu"))

## Variáveis de município de admissão

#2016
RAIS_SP <- RAIS_SP %>%
  mutate(CD_MUNICIPIO_2016 = ifelse(CD_UF_2016 == "Permaneceu" & CD_MUNICIPIO_2016 == "355030", "Permaneceu",
                                        ifelse(CD_UF_2016 == "Permaneceu" & CD_MUNICIPIO_2016 != "355030", "Não Permaneceu",NA)))
#2017
RAIS_SP <- RAIS_SP %>%
  mutate(CD_MUNICIPIO_2017 = ifelse(CD_UF_2017 == "Permaneceu" & CD_MUNICIPIO_2017 == "355030", "Permaneceu",
                                    ifelse(CD_UF_2017 == "Permaneceu" & CD_MUNICIPIO_2017 != "355030", "Não Permaneceu",NA)))
#2018
RAIS_SP <- RAIS_SP %>%
  mutate(CD_MUNICIPIO_2018 = ifelse(CD_UF_2018 == "Permaneceu" & CD_MUNICIPIO_2018 == "355030", "Permaneceu",
                                    ifelse(CD_UF_2018 == "Permaneceu" & CD_MUNICIPIO_2018 != "355030", "Não Permaneceu",NA)))



############################## Análise Descritiva #############################

#### Limpando das base variáveis que não vão ser utilizadas na análise

va.excl2<- c('CD_TIPO_VINCULO', 'CD_NATUREZA_JURIDICA', 'VL_REMUN_MEDIA_NOM_2015', 
             'VL_REMUN_MEDIA_NOM_2016', 'VL_REMUN_MEDIA_NOM_2017', 'VL_REMUN_MEDIA_NOM_2018')
RAIS_SP[va.excl2]<- NULL

################### Análises Univariadas 

# Univariada da variável resposta: TEMPO_ADM

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
TEMPO_ADM <- RAIS_SP %>% select(TEMPO_ADM) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

TEMPO_ADM$cumsum <- cumsum(TEMPO_ADM$Freq)
TEMPO_ADM$cumsum_freq <- cumsum(TEMPO_ADM$freq.rel)
TEMPO_ADM <- TEMPO_ADM%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)
write.csv2(TEMPO_ADM, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/TEMPO_ADM.csv")


#Gráfico de Barras
TEMPO_ADM2 <- RAIS_SP %>% select(TEMPO_ADM) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)
ggplot(TEMPO_ADM2,aes(x = var, y = freq.rel))+
  geom_col(fill=c("#A7C8E6", "#376987","#9999CC", "#339999"),
           width = 0.5, position = position_dodge(width = 0.5),
           color = "black")+
  geom_label(aes(label = freq.rel),
             nudge_y = 3, size= 5)+
  #geom_text(aes(label = freq.rel), vjust = 0, colour = "black", size=6)+
  theme_bw(base_size = 16)+ 
  labs(x="Ano de Retorno", y="Proporção (%)", size=16)


########### Separando bases para análises univaridas

RAIS_SP_2016 <- RAIS_SP %>%
  filter((TEMPO_ADM == 2016))

RAIS_SP_2017 <- RAIS_SP %>%
  filter((TEMPO_ADM == 2017))

RAIS_SP_2018 <- RAIS_SP %>%
  filter((TEMPO_ADM == 2018))

# Variáveis de vínculo ativo de 2016, 2017 a 2018 

#FL_VINCULO_ATIVO_3112_2016

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
FL_VINCULO_ATIVO_3112_2016 <- RAIS_SP_2016 %>% select(FL_VINCULO_ATIVO_3112_2016) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

FL_VINCULO_ATIVO_3112_2016$cumsum <- cumsum(FL_VINCULO_ATIVO_3112_2016$Freq)
FL_VINCULO_ATIVO_3112_2016$cumsum_freq <- cumsum(FL_VINCULO_ATIVO_3112_2016$freq.rel)
FL_VINCULO_ATIVO_3112_2016 <- FL_VINCULO_ATIVO_3112_2016%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(FL_VINCULO_ATIVO_3112_2016, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/FL_VINCULO_ATIVO_3112_2016.csv")


#FL_VINCULO_ATIVO_3112_2017

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
FL_VINCULO_ATIVO_3112_2017 <- RAIS_SP_2017 %>% select(FL_VINCULO_ATIVO_3112_2017) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

FL_VINCULO_ATIVO_3112_2017$cumsum <- cumsum(FL_VINCULO_ATIVO_3112_2017$Freq)
FL_VINCULO_ATIVO_3112_2017$cumsum_freq <- cumsum(FL_VINCULO_ATIVO_3112_2017$freq.rel)
FL_VINCULO_ATIVO_3112_2017 <- FL_VINCULO_ATIVO_3112_2017%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(FL_VINCULO_ATIVO_3112_2017, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/FL_VINCULO_ATIVO_3112_2017.csv")


#FL_VINCULO_ATIVO_3112_2018

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
FL_VINCULO_ATIVO_3112_2018 <- RAIS_SP_2018 %>% select(FL_VINCULO_ATIVO_3112_2018) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

FL_VINCULO_ATIVO_3112_2018$cumsum <- cumsum(FL_VINCULO_ATIVO_3112_2018$Freq)
FL_VINCULO_ATIVO_3112_2018$cumsum_freq <- cumsum(FL_VINCULO_ATIVO_3112_2018$freq.rel)
FL_VINCULO_ATIVO_3112_2018 <- FL_VINCULO_ATIVO_3112_2018%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(FL_VINCULO_ATIVO_3112_2018, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/FL_VINCULO_ATIVO_3112_2018.csv")


# Univariada das variáveis de UF e Município de admissão de 2016, 2017 a 2018 
#CD_UF_2016

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_UF_2016 <- RAIS_SP_2016 %>% select(CD_UF_2016) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_UF_2016$cumsum <- cumsum(CD_UF_2016$Freq)
CD_UF_2016$cumsum_freq <- cumsum(CD_UF_2016$freq.rel)
CD_UF_2016 <- CD_UF_2016%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)
write.csv2(CD_UF_2016, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_UF_2016.csv")


#CD_MUNICIPIO_2016

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_MUNICIPIO_2016 <- RAIS_SP_2016 %>% select(CD_MUNICIPIO_2016) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_MUNICIPIO_2016$cumsum <- cumsum(CD_MUNICIPIO_2016$Freq)
CD_MUNICIPIO_2016$cumsum_freq <- cumsum(CD_MUNICIPIO_2016$freq.rel)
CD_MUNICIPIO_2016 <- CD_MUNICIPIO_2016%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(CD_MUNICIPIO_2016, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_MUNICIPIO_2016.csv")


#CD_UF_2017

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_UF_2017 <- RAIS_SP_2017 %>% select(CD_UF_2017) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_UF_2017$cumsum <- cumsum(CD_UF_2017$Freq)
CD_UF_2017$cumsum_freq <- cumsum(CD_UF_2017$freq.rel)
CD_UF_2017 <- CD_UF_2017%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(CD_UF_2017, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_UF_2017.csv")


#CD_MUNICIPIO_2017

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_MUNICIPIO_2017 <- RAIS_SP_2017 %>% select(CD_MUNICIPIO_2017) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_MUNICIPIO_2017$cumsum <- cumsum(CD_MUNICIPIO_2017$Freq)
CD_MUNICIPIO_2017$cumsum_freq <- cumsum(CD_MUNICIPIO_2017$freq.rel)
CD_MUNICIPIO_2017 <- CD_MUNICIPIO_2017%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(CD_MUNICIPIO_2017, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_MUNICIPIO_2017.csv")


#CD_UF_2018

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_UF_2018 <- RAIS_SP_2018 %>% select(CD_UF_2018) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_UF_2018$cumsum <- cumsum(CD_UF_2018$Freq)
CD_UF_2018$cumsum_freq <- cumsum(CD_UF_2018$freq.rel)
CD_UF_2018 <- CD_UF_2018%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)
write.csv2(CD_UF_2018, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_UF_2018.csv")


#CD_MUNICIPIO_2018

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
CD_MUNICIPIO_2018 <- RAIS_SP_2018 %>% select(CD_MUNICIPIO_2018) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

CD_MUNICIPIO_2018$cumsum <- cumsum(CD_MUNICIPIO_2018$Freq)
CD_MUNICIPIO_2018$cumsum_freq <- cumsum(CD_MUNICIPIO_2018$freq.rel)
CD_MUNICIPIO_2018 <- CD_MUNICIPIO_2018%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(CD_MUNICIPIO_2018, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/CD_MUNICIPIO_2018.csv")


# Bivariada das variáveis de remuneração

#Criando variável para identificar se os trabalhadores admitidos recebem menos, 
#mais ou o mesmo valor dos vínculos que foram desligados

# 2016

RAIS_SP_2016 <- RAIS_SP_2016 %>%
  mutate(REMUNERACAO = ifelse(VL_REMUN_MEDIA_SM_2015 > VL_REMUN_MEDIA_SM_2016, "Menos",
                           ifelse(VL_REMUN_MEDIA_SM_2015 < VL_REMUN_MEDIA_SM_2016, "Mais",
                                  ifelse(VL_REMUN_MEDIA_SM_2015 == VL_REMUN_MEDIA_SM_2016, "Igual",NA))))

# 2017

RAIS_SP_2017 <- RAIS_SP_2017 %>%
  mutate(REMUNERACAO = ifelse(VL_REMUN_MEDIA_SM_2015 > VL_REMUN_MEDIA_SM_2017, "Menos",
                              ifelse(VL_REMUN_MEDIA_SM_2015 < VL_REMUN_MEDIA_SM_2017, "Mais",
                                     ifelse(VL_REMUN_MEDIA_SM_2015 == VL_REMUN_MEDIA_SM_2017, "Igual",NA))))
# 2018

RAIS_SP_2018 <- RAIS_SP_2018 %>%
  mutate(REMUNERACAO = ifelse(VL_REMUN_MEDIA_SM_2015 > VL_REMUN_MEDIA_SM_2018, "Menos",
                              ifelse(VL_REMUN_MEDIA_SM_2015 < VL_REMUN_MEDIA_SM_2018, "Mais",
                                     ifelse(VL_REMUN_MEDIA_SM_2015 == VL_REMUN_MEDIA_SM_2018, "Igual",NA))))

### Analisando 

#REMUNERACAO 2016

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
REMUNERACAO_2016 <- RAIS_SP_2016 %>% select(REMUNERACAO) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

REMUNERACAO_2016$cumsum <- cumsum(REMUNERACAO_2016$Freq)
REMUNERACAO_2016$cumsum_freq <- cumsum(REMUNERACAO_2016$freq.rel)
REMUNERACAO_2016 <- REMUNERACAO_2016%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(REMUNERACAO_2016, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/REMUNERACAO_2016.csv")



#REMUNERACAO 2017

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
REMUNERACAO_2017 <- RAIS_SP_2017 %>% select(REMUNERACAO) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

REMUNERACAO_2017$cumsum <- cumsum(REMUNERACAO_2017$Freq)
REMUNERACAO_2017$cumsum_freq <- cumsum(REMUNERACAO_2017$freq.rel)
REMUNERACAO_2017 <- REMUNERACAO_2017%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(REMUNERACAO_2017, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/REMUNERACAO_2017.csv")


#REMUNERACAO 2018

#Tabela de Frequencia (Frequência absoluta, relativa (%), acumulada e relativa acumulada)
REMUNERACAO_2018 <- RAIS_SP_2018 %>% select(REMUNERACAO) %>%
  table() %>% data.frame() %>%
  mutate(freq.rel = round((Freq/sum(Freq))*100,
                          digits = 2)) %>%
  rename(var = 1)

REMUNERACAO_2018$cumsum <- cumsum(REMUNERACAO_2018$Freq)
REMUNERACAO_2018$cumsum_freq <- cumsum(REMUNERACAO_2018$freq.rel)
REMUNERACAO_2018 <- REMUNERACAO_2018%>%
  rename(`Frequência absoluta` = 2 ,
         `Frequência relativa (%)` = 3,
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5,
         ` ` = 1)

write.csv2(REMUNERACAO_2018, "D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA/REMUNERACAO_2018.csv")


################### Análises Bivariadas (Variável Resposta X variáveis Explicativas)

### Qualitativa Ordinal	x Qualitativa Ordinal


#ANO_ADM x CD_TIPO_SALARIO
#### Tabela de Contingência
addmargins(table(RAIS_SP$TEMPO_ADM,RAIS_SP$TIPO_SALARIO))
m<-table(RAIS_SP$TEMPO_ADM,RAIS_SP$TIPO_SALARIO)
plotdata <- RAIS_SP %>%
  select(TEMPO_ADM, TIPO_SALARIO) %>%
  
ggplot(plotdata, 
       mapping=aes(x = TEMPO_ADM, fill=factor(TIPO_SALARIO))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values=c("#A7C8E6", "#376987"),
                    name="Tipo Salário",
                    labels=c("Mensal", "Outros"))+
  #geom_label(nudge_y = 3, size= 5)+
  #geom_text(aes(label = plotdata$lbl),
   #         vjust = 0, colour = "black", size=6)+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(plotdata, mapping=aes(x = TEMPO_ADM, y = pct, fill=TIPO_SALARIO))+
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987"),
                    name="Tipo Salário",
                    labels=c("Mensal", "Outros"))+
  geom_label(aes(label = lbl),
             nudge_y = 3, size= 5)+
  #geom_text(aes(label = freq.rel), vjust = 0, colour = "black", size=6)+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = factor(TEMPO_ADM), fill=TIPO_SALARIO)) + 
  geom_bar(aes(fill = TIPO_SALARIO), position = "stack", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987"),
                    name="Tipo Salário",
                    labels=c("Mensal", "Outros"))+
  #scale_y_continuous(labels=paste0(y=(((..count..)/sum(..count..))*100)))
  #geom_label(nudge_y = 3, size= 5)+
 # geom_text(aes(label = plotdata$pct),
  #          vjust = 0, colour = "black", size=6)+
  ylim(0,100)+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")


ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(TIPO_SALARIO,TEMPO_ADM), fill=TIPO_SALARIO))+
  scale_fill_manual(values=c("#A7C8E6", "#376987"))+
  labs(x="Ano de Retorno", y="Tipo Salário")+
  theme_bw(base_size = 16)
  

#ANO_ADM x PORTE_ESTABELECIMENTO

#### Tabela de Contingência
addmargins(table(RAIS_SP$PORTE_ESTABELECIMENTO,RAIS_SP$TEMPO_ADM))

m<-matrix(table(RAIS_SP$PORTE_ESTABELECIMENTO,RAIS_SP$TEMPO_ADM),4,4)

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=factor(PORTE_ESTABELECIMENTO, levels=c("Grande", "Média", "Pequena", "Micro")))) + 
  geom_bar(aes(fill = factor(PORTE_ESTABELECIMENTO, levels=c("Grande", "Média", "Pequena", "Micro"))), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987","#9999CC", "#339999"),
                    name="Porte Estabelecimento",
                    labels=c("Grande", "Média", "Pequena", "Micro"))+
  theme_bw(base_size = 16)+
  xlab("Ano de Retorno") + ylab("Proporção (%)")
ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(PORTE_ESTABELECIMENTO,TEMPO_ADM), fill=PORTE_ESTABELECIMENTO))+
  scale_fill_manual(values=c("#A7C8E6", "#376987","#9999CC", "#339999"))+
  labs(x="Ano de Retorno", y="Porte do Estabelecimento")+
  theme_bw(base_size = 16)


library(ggpubr)
df <- as.data.frame(m)

ggballoonplot(m, fill ="values",)+
  scale_fill_viridis_c(option = "C")

  
#ANO_ADM x GR_INSTR
  
#### Tabela de Contingência
addmargins(table(RAIS_SP$GR_INSTR,RAIS_SP$TEMPO_ADM))
  
#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=GR_INSTR)) + 
  geom_bar(aes(fill = GR_INSTR), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987","#339999"),
                    name="Grau de Instrução",
                    labels=c("Nível Fundamental","Nível Médio", "Nível Superior"))+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(GR_INSTR,TEMPO_ADM), fill=GR_INSTR))+
  scale_fill_manual(values=c("#A7C8E6", "#376987","#9999CC"))+
  labs(x="Ano de Retorno", y="Grau de Instrução")+
  theme_bw(base_size = 16)


### Qualitativa Ordinal x	Qualitativa Nominal


#ANO_ADM x CD_RACA_COR
#### Tabela de Contingência
addmargins(table(RAIS_SP$RACA_COR,RAIS_SP$TEMPO_ADM))

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=RACA_COR)) + 
  geom_bar(aes(fill = RACA_COR), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#376987","#339999"),
                    name="Raça - Cor",
                    labels=c("Não preta", "Preta"))+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(RACA_COR,TEMPO_ADM), fill=RACA_COR))+
  scale_fill_manual(values=c("#A7C8E6", "#376987"))+
  labs(x="Ano de Retorno", y="Raça-cor")+
  theme_bw(base_size = 16)

#ANO_ADM x CD_MOTIVO_DESLIGAMENTO
RAIS_SP <- RAIS_SP %>%
 mutate(CD_MOTIVO_DESLIGAMENTO = ifelse(CD_MOTIVO_DESLIGAMENTO == "10","Com justa causa - empregador",
                      ifelse(CD_MOTIVO_DESLIGAMENTO == "11", "Sem justa causa - empregador",
                             ifelse( CD_MOTIVO_DESLIGAMENTO == "12", "Término do contrato de trabalho", NA))))

#### Tabela de Contingência
addmargins(table(RAIS_SP$CD_MOTIVO_DESLIGAMENTO,RAIS_SP$TEMPO_ADM))

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=CD_MOTIVO_DESLIGAMENTO)) + 
  geom_bar(aes(fill = CD_MOTIVO_DESLIGAMENTO), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987","#339999"),
                    name="Motivo de Desligamento",
                    labels=c("Com justa causa", "Sem justa causa", "Término do contrato"))+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(CD_MOTIVO_DESLIGAMENTO,TEMPO_ADM), fill=CD_MOTIVO_DESLIGAMENTO))+
  scale_fill_manual(values=c("#A7C8E6", "#376987","#339999"))+
  labs(x="Ano de Retorno", y="Motivo de Desligamento")+
  theme_bw(base_size = 16)

#ANO_ADM x CD_SEXO
RAIS_SP <- RAIS_SP %>%
  mutate(CD_SEXO = ifelse(CD_SEXO == "1","Masculino", "Feminino"))
#### Tabela de Contingência
addmargins(table(RAIS_SP$CD_SEXO,RAIS_SP$TEMPO_ADM))

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=CD_SEXO)) + 
  geom_bar(aes(fill = CD_SEXO), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987"),
                    name="Sexo",
                    labels=c("Feminino", "Masculino"))+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(CD_SEXO,TEMPO_ADM), fill=CD_SEXO))+
  scale_fill_manual(values=c("#A7C8E6", "#376987"))+
  labs(x="Ano de Retorno", y="Sexo")+
  theme_bw(base_size = 16)


#ANO_ADM x SETOR
#### Tabela de Contingência
addmargins(table(RAIS_SP$SETOR,RAIS_SP$TEMPO_ADM))

#### Gráfico de Barras
ggplot(RAIS_SP, 
       aes(x = TEMPO_ADM, y=(((..count..)/sum(..count..))*100), fill=SETOR)) + 
  geom_bar(aes(fill = SETOR), position = "dodge", color = "black") +
  scale_fill_manual(values=c("#A7C8E6", "#376987","#9999CC", "#339999"),
                    name="Setor",
                    labels=c("Comércio", "Construção", "Indústria", "Serviços"))+
  theme_bw(base_size = 16)+ 
  xlab("Ano de Retorno") + ylab("Proporção (%)")

ggplot(data=RAIS_SP)+
  geom_mosaic(aes(x=product(SETOR,TEMPO_ADM), fill=SETOR))+
  scale_fill_manual(values=c("#A7C8E6", "#376987","#9999CC", "#339999"))+
  labs(x="Ano de Retorno", y="Setor")+
  theme_bw(base_size = 16)

### Qualitativa Ordinal x	Quantitativa Contínua

n=1018333
descritiva=function(x){
  y=c(summary(x),
      cv=cv(x))
  data.frame(t(y))
}


#ANO_ADM x VL_REMUN_MEDIA_SM
quantile(RAIS_SP$VL_REMUN_MEDIA_SM_2015)

## Classificação de acordo com os quartis
REMUN_MEDIA <- cut(RAIS_SP$VL_REMUN_MEDIA_SM_2015, breaks =  quantile(RAIS_SP$VL_REMUN_MEDIA_SM_2015),
                   include.lowest = TRUE)
## Tabela de frequências absolutas
adm_remun<-table(REMUN_MEDIA,RAIS_SP$TEMPO_ADM)
adm_remun2<-prop.table(adm_remun)
addmargins(adm_remun)


## Medidas Descritivas
tapply(RAIS_SP$VL_REMUN_MEDIA_SM_2015,RAIS_SP$TEMPO_ADM, descritiva)

## Análise de Variância
summary(aov(RAIS_SP$VL_REMUN_MEDIA_SM_2015~RAIS_SP$TEMPO_ADM))


#ANO_ADM x VL_IDADE
quantile(RAIS_SP$VL_IDADE)

## Classificação de acordo com os quartis
VL_IDADE <- cut(RAIS_SP$VL_IDADE, breaks =  quantile(RAIS_SP$VL_IDADE),
                   include.lowest = TRUE)
## Tabela de frequências absolutas
adm_idade<-table(VL_IDADE,RAIS_SP$TEMPO_ADM)
adm_idade2<-prop.table(adm_idade)
addmargins(adm_idade)

## Medidas Descritivas
tapply(RAIS_SP$VL_IDADE,RAIS_SP$TEMPO_ADM, descritiva)

## Análise de Variância
summary(aov(RAIS_SP$VL_IDADE~RAIS_SP$TEMPO_ADM))



#ANO_ADM x NR_MES_TEMPO_EMPREGO
quantile(RAIS_SP$NR_MES_TEMPO_EMPREGO)

## Classificação de acordo com os quartis
NR_MES_TEMPO_EMPREGO <- cut(RAIS_SP$NR_MES_TEMPO_EMPREGO, breaks =  quantile(RAIS_SP$NR_MES_TEMPO_EMPREGO),
                include.lowest = TRUE)
## Tabela de frequências absolutas
adm_tempempr<-table(NR_MES_TEMPO_EMPREGO,RAIS_SP$TEMPO_ADM)
adm_tempempr2<-prop.table(adm_tempempr)
addmargins(adm_tempempr)


## Medidas Descritivas
tapply(RAIS_SP$NR_MES_TEMPO_EMPREGO,RAIS_SP$TEMPO_ADM, descritiva)


###################### Salvando base para modelagem ############################
va.excl3<- c('FL_VINCULO_ATIVO_3112_2016', 'FL_VINCULO_ATIVO_3112_2017', 'FL_VINCULO_ATIVO_3112_2018', 
             'VL_REMUN_MEDIA_SM_2016', 'VL_REMUN_MEDIA_SM_2017', 'VL_REMUN_MEDIA_SM_2018',
             'CD_MUNICIPIO_2016','CD_MUNICIPIO_2017','CD_MUNICIPIO_2018',
             'CD_UF_2016','CD_UF_2017','CD_UF_2018',
             'NR_MES_TEMPO_EMPREGO_2016','NR_MES_TEMPO_EMPREGO_2017','NR_MES_TEMPO_EMPREGO_2018',
             'VL_REMUN_MEDIA_NOM', 'ID_TRABALHADOR', 'ID_ESTABELECIMENTO')
RAIS_SP[va.excl3]<- NULL

saveRDS(RAIS_SP, "D:/TCC 2/RAIS/MODELAGEM/RAIS_SP.rds") 


###################### Série Histórica de Taxa de desemprego ###################

setwd("D:/TCC 2/RAIS/BASE DE DADOS/RAIS - CORRIGIDA")
library(ggplot2)
library(readxl)
HISTORICO_TX_DESOCUP <- read_excel("HISTORICO_TX_DESOCUP.xlsx")

HISTORICO_TX_DESOCUP$Taxas<- round(HISTORICO_TX_DESOCUP$Taxas, 1)
ls(HISTORICO_TX_DESOCUP)

ggplot(HISTORICO_TX_DESOCUP, aes(x=Ano, y=Taxas, group=`Taxas de Desocupação`)) +
  geom_line(aes(linetype=`Taxas de Desocupação`, color=`Taxas de Desocupação`), size=1.8)+
  ylim(0,14.5)+
  scale_color_manual(values=c("#008B8B","DodgerBlue"))+
  labs(y = "Taxa Média de Desocupação (%)")+
  geom_label(vjust = 0.5,aes(label = Taxas), size= 5)+
  theme_bw(base_size = 15)

         