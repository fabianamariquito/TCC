######################################################################################################
###################################### TCC: RAIS - São Paulo ########################################
#####################################################################################################

#Diretório
setwd("D:/TCC 2/RAIS/MODELAGEM")

#Bibliotecas

pacotes <- c("ggplot2", "tidyverse", "EnvStats", 
             "knitr", "GGally", "plotly", "moments",
             "readxl", "readr","dplyr", "stringr", 
             "openxlsx","kableExtra","car","multcompView",
             "reshape2","ggrepel", "MASS", "lmtest", "pROC",
             "VGAM", "ordinal", "ggeffects", "aod")
lapply(pacotes, library, character.only = T)

# Importando os dados
RAIS_SP<-readRDS("RAIS_SP.rds")

##### Codificando variáveis

## Ano de Retorno

RAIS_SP <- RAIS_SP %>%
  mutate(TEMPO_ADM = ifelse(TEMPO_ADM == "2016", 1,
                                   ifelse(TEMPO_ADM == "2017", 2,
                                          ifelse(TEMPO_ADM == "2018", 3, 4))))
## Porte Estabelecimento
RAIS_SP <- RAIS_SP %>%
  mutate(PORTE_ESTABELECIMENTO = ifelse(PORTE_ESTABELECIMENTO == "Micro" | PORTE_ESTABELECIMENTO == "Pequena", "Pequeno","Grande"))


################################# Modelo - Geral ###############################

#### Amostragem
set.seed(22)
RAIS_SP_AMOS <- sample_n(RAIS_SP, 10183, replace = FALSE)

#ordered(RAIS_SP_AMOS$TEMPO_ADM)

#### Referenciando categorias
RAIS_SP_AMOS$TEMPO_ADM = relevel(factor(RAIS_SP_AMOS$TEMPO_ADM), ref='4')
RAIS_SP_AMOS$GR_INSTR = relevel(factor(RAIS_SP_AMOS$GR_INSTR), ref='N. Médio')
RAIS_SP_AMOS$CD_SEXO = relevel(factor(RAIS_SP_AMOS$CD_SEXO), ref='Masculino')
RAIS_SP_AMOS$CD_MOTIVO_DESLIGAMENTO = relevel(factor(RAIS_SP_AMOS$CD_MOTIVO_DESLIGAMENTO), ref='Sem justa causa - empregador')
RAIS_SP_AMOS$SETOR = relevel(factor(RAIS_SP_AMOS$SETOR), ref='Serviços')


#### Teste modelo com todas as variáveis 


### Modelo completo
modelo_AMOS <- clm(as.factor(TEMPO_ADM) ~ TIPO_SALARIO  + RACA_COR + SETOR + PORTE_ESTABELECIMENTO + 
                       GR_INSTR + CD_SEXO + CD_MOTIVO_DESLIGAMENTO + VL_IDADE + VL_REMUN_MEDIA_SM_2015 + 
                       NR_MES_TEMPO_EMPREGO, data=RAIS_SP_AMOS, link = "logit")
### Não significativo para tipo salário, setor, grau de instrução e motivo desligamento
summary(modelo_AMOS)


## Stepwise
stepAIC(modelo_AMOS)


## Modelo selecionado AIC=24591.43
modelo_AMOS_STEP <- clm(as.factor(TEMPO_ADM) ~ RACA_COR + PORTE_ESTABELECIMENTO + CD_SEXO + 
                          VL_IDADE + VL_REMUN_MEDIA_SM_2015 + NR_MES_TEMPO_EMPREGO,
                        data=RAIS_SP_AMOS, link = "logit")

summary(modelo_AMOS_STEP)

#234 dado 1
prob4<- (exp(1.57767+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*1))/
  (1+exp(1.57767+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*6))
prob4
#1 dado 234
prob1<- (exp(0.27006+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*1))/
  (1+exp(0.27006+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*6))
prob1
#12 dado 34 
prob2<- (exp(1.48594+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*1))/
  (1+exp(1.48594+0.0906721*1-0.1314849*1-0.1173307*1-0.0232350*1-0.0133281*1-0.0031532*6))
prob2
#modelo_AMOS_STEP_polr <- vglm(as.factor(TEMPO_ADM) ~ RACA_COR + PORTE_ESTABELECIMENTO + CD_SEXO + 
                          #VL_IDADE + VL_REMUN_MEDIA_SM_2015 + NR_MES_TEMPO_EMPREGO, family=cumulative(parallel = TRUE),
                          #data=RAIS_SP_AMOS)
table(RAIS_SP_AMOS$VL_IDADE)
#summary(modelo_AMOS_STEP_polr)


#### Testes de ajuste global do modelo

## Teste de Razão de Verossimilhanças:
lmtest::lrtest(modelo_AMOS_STEP)


## Teste de Wald:
lmtest::waldtest(modelo_AMOS_STEP)


#### Testes de significância das variáveis do modelo.

## Teste de Razão de Verossimilhanças:
Anova(modelo_AMOS_STEP)

##Teste de Wald
#summary(modelo_AMOS_STEP)
anova(modelo_AMOS_STEP, type="3")
wald.test(b = coef(modelo_AMOS_STEP), Sigma = vcov(modelo_AMOS_STEP), Terms = 3)


#### Intervalo de confiança para as estimativas
## Int. Conf. OR:
exp(coef(modelo_AMOS_STEP))
exp(confint.default(modelo_AMOS_STEP)) 

summary(RAIS_SP_AMOS$VL_REMUN_MEDIA_SM_2015)


