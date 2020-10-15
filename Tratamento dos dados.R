# Tratamento dos dados para o Artigo (Grupos raciais e renda: tendências diferenciadas)

# Aluno: Ulisses Matheus

library(tidyverse)
library(readr)
library(sjPlot)
library(haven)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)
library(ggthemes)
library(InformationValue)
library(margins)
library(dotwhisker)


#Os códigos abaixo foram utilizados para a obtenção dos resultados apresentados no artigo final.

#Primeiro foi realizada a importação dos bancos de dados, como os bancos que eu utilizei precisavam
#da inscrição na página da CESOP exportei os mesmos para o meu perfil no github, estes são os links 
#utilizados nas importações abaixo.

#Após a importação foi realizada a filtragem dos dados. Foram desconsideradas respostas negativas as
#perguntas sobre a variável renda familiar, educação, raça, opinião sobre o PT, candidato do PT e candidato em
#oposição ao PT. Também foi nessa fase que limitei os valores da variável referente a renda familiar.

#Alterei as variáveis de raça, gênero e região (que eram entendidas como numéricas pelo R), para 
#variáveis categóricas (as_factor), que que facilitou no tratamento dos dados e na elaboração
#dos gráficos.

#Reconfigurei as variáveis de Raça (troquei amarelos e indígenas pela categoria "outros"), e a variável
#religião (troquei a varíável de ateus e os sem religião para uma categoria única dos "sem religião"
#e troquei as religiões com menos adeptos para a categoria "outras", em alguns anos também reuni as
#vertentes evangélicas em uma única categoria).

#Apliquei os gráficos observatórios nas variáveis que seriam utilizadas nas regressões. Isso permitiu
#observar certas tendências (categorias raciais de pardos e brancos respondem por cerca de 80% da
#população, há uma grande desigualde de renda no Brasil o que promove a falta de distribuição normal
#nos dados referentes a renda e houve uma tendência crescente de rejeição ao PT e seus candidatos com
#o passar do tempo).

#Realizei as regressões, que foram acompanhadas pelos testes de linearidade, heterocedasticidade,
#outliers e multicolinearidade. Esses testes verificam alguns dos pressupostos básicos de uma regressão
#linear. Alguns desses pressupostos são matemáticos, outros são sobre o componente estocástico do
#modelo populacional, eles aumentam a segurança na validade dos resultados de uma regressão.

#Importante detalhar os resultados dos testes e alguns desses pressupostos.
#As regressões apresentaram certa linearidade (as que utilizaram os dados de 2018 tiveram certas 
#distorções). As regressões que utilizaram a interação entre a renda e a raça dos eleitores apresentaram
#heterocedasticidade de forma recorrente, ou seja, as variâncias dos erros não foram constantes nas 
#regressões. Apesar disso foi aplicado o coeftest e observou-se que tal problema não interferiu de
#forma significativa nos resultados finais. Essas regressões também apresentaram multicolinearidade
#entre a interação de renda e raça e a variável raça (o que já era esperado). Os outliers encontrados
#foram retirados, o que melhorou a significância das regressões.

#Por fim foram feitos os gráficos, que apresentam seus resultados em odds ratio.

#Nota-se que realizei uma filtragem de dados específica para o voto no segundo turno, tendo em vista
#que pessoas entrevistadas que não emitiram opinião sobre os partidos e candidatos podem ter declarado
#seu voto.

#Nas regressões logísticas transformei os coeficientes em probabilidade, o que possibilitou encontrar
#dados probabilísticos sobre as tendências raciais de voto no segundo turno das eleições.

#Esse procedimento foi repetido, seguindo essa mesma sequência, em todos os bancos de dados analisados.


# Tratamento dos dados do ESEB de 2002

ESEB2002 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB%202002.sav")


# Filtragem de entrevistados que não responderam as variáveis utilizadas na pesquisa

Eseb2002 <- ESEB2002 %>% filter(p189 < 6, p42a < 11,
                                p42c < 11, p43a <11, p43b < 11,
                                p176 < 7150, p182 < 70) %>%
  mutate(p189 = as_factor(p189),
         p182 = as_factor(p182),
         estrato = as_factor(estrato))

#Variável religião

Outras <- levels(Eseb2002$p182)[-c(2,3,8,12,13)]

Eseb2002 <- Eseb2002 %>%
  mutate(religiao = case_when(p182 %in% Outras ~ "Outras",
                              p182 == "Católica" ~ "Católica",
                              p182 == "Evangélica pentecostal" ~ "Evangélica",
                              p182 == "Evangélica não-pentecostal" ~ "Evangélica",
                              p182 == "Não tem religião" ~ "Não tem religião",
                              p182 == "É ateu/Não acredita em Deus" ~ "Não tem religião"))

#Variável Raça

Outros <- levels(Eseb2002$p189)[c(4,5)]

Eseb2002 <- Eseb2002 %>%
  mutate(Raca = case_when(p189 %in% Outros ~ "Outros",
                          p189 == "Preto" ~ "Preto",
                          p189 == "Pardo" ~ "Pardo",
                          p189 == "Branco" ~ "Branco"))



# Filtragem de dados para as regressões com o segundo turno

Eseb20022turno <- ESEB2002 %>% filter(p189 < 6, p08 < 5,
                                      p176 < 7150, p182 < 70) %>%
  mutate(p189 = as_factor(p189),
         p182 = as_factor(p182),
         estrato = as_factor(estrato),
         p08 = case_when(p08 == 1 ~ 1,  
                         p08 == 2 ~ 0),
         p08 = as_factor(p08)) 


Outras <- levels(Eseb20022turno$p182)[-c(2,3,8,12,13)]

Eseb20022turno<- Eseb20022turno %>%
  mutate(religiao = case_when(p182 %in% Outras ~ "Outras",
                              p182 == "Católica" ~ "Católica",
                              p182 == "Evangélica pentecostal" ~ "Evangélica",
                              p182 == "Evangélica não-pentecostal" ~ "Evangélica",
                              p182 == "Não tem religião" ~ "Não tem religião",
                              p182 == "É ateu/Não acredita em Deus" ~ "Não tem religião"))



Outros <- levels(Eseb20022turno$p189)[c(4,5)]

Eseb20022turno <- Eseb20022turno %>%
  mutate(Raca = case_when(p189 %in% Outros ~ "Outros",
                          p189 == "Preto" ~ "Preto",
                          p189 == "Pardo" ~ "Pardo",
                          p189 == "Branco" ~ "Branco"))


# Gráficos das variáveis analisadas

# Raça 

ggplot(Eseb2002, aes(p189)) + geom_bar()


# Renda familiar

ggplot(Eseb2002, aes(p176)) + geom_density()
ggplot(Eseb2002, aes(p176)) + geom_boxplot()


# Opinião sobre o PT

ggplot(Eseb2002, aes(p42a)) + geom_bar()

# Opinião sobre Lula

ggplot(Eseb2002, aes(p43a)) + geom_bar()

# Opinião sobre Serra

ggplot(Eseb2002, aes(p43b)) + geom_bar()


# Regressões e gráficos sobre a covariação entre a renda, raça e a opinião política em 2002

# opinião sobre pt (2002)

regressaotd2002 <- lm(p42a ~ p176 + p157 + p158  + estrato + p159+ religiao + Raca, data = Eseb2002)

summary(regressaotd2002)

plot(regressaotd2002,1)
plot(regressaotd2002,3)
plot(regressaotd2002,4)
plot(regressaotd2002,5)
outlierTest(regressaotd2002)
vif(regressaotd2002)
coeftest(regressaotd2002, vcov. = vcovHC(regressaotd2002))


plot_model(regressaotd2002, type = "pred",
           terms = c("p176"), ci.lvl = 0.9) + labs(title = "Opinião sobre o PT a partir da Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre o PT") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# opinião sobre lula (2002)

regressaotd20021 <- lm(p43a ~ p176 + p157 + p158  + estrato + p159+ religiao + Raca, data = Eseb2002)

summary(regressaotd20021)

plot(regressaotd20021,1)
plot(regressaotd20021,3)
plot(regressaotd20021,4)
plot(regressaotd20021,5)
outlierTest(regressaotd20021)
vif(regressaotd20021)
coeftest(regressaotd20021, vcov. = vcovHC(regressaotd20021))


plot_model(regressaotd20021, type = "pred",
           terms = c("p176"), ci.lvl = 0.9) + labs(title = "Opinião sobre Lula a partir da Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre Lula") + theme(axis.text.x = element_text(angle = 45, hjust = 1))



# opinião sobre serra (2002)

regressaotd20022 <- lm(p43b ~ p176 + p157 + p158  + estrato + p159+ religiao + Raca, data = Eseb2002)

summary(regressaotd20022)

plot(regressaotd20022,1)
plot(regressaotd20022,3)
plot(regressaotd20022,4)
plot(regressaotd20022,5)
outlierTest(regressaotd20022)
vif(regressaotd20022)
coeftest(regressaotd20022, vcov. = vcovHC(regressaotd20022))

plot_model(regressaotd20022, type = "pred",
           terms = c("p176"), ci.lvl = 0.9) + labs(title = "Opinião sobre Serra a partir da Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre Serra") + theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Regressões que utilizam a interação entre raça e renda como variável independente


#Op PT

regressao <- lm(p42a ~ p176*Raca + p157 + p158  + estrato + p159+ religiao, data = Eseb2002)
summary(regressao)

plot(regressao,1)
plot(regressao,3)
plot(regressao,4)
plot(regressao,5)
outlierTest(regressao)
vif(regressao)
coeftest(regressao, vcov. = vcovHC(regressao))

plot_model(regressao, type = "pred",
           terms = c("p176","Raca"), ci.lvl = 0.9) + labs(title = "Opinião sobre o PT a partir da interação entre Raça e Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre o PT") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Op Lula

regressao1 <- lm(p43a ~ p176*Raca + p157 + p158  + estrato + p159+ religiao, data = Eseb2002)
summary(regressao1)

plot(regressao1,1)
plot(regressao1,3)
plot(regressao1,4)
plot(regressao1,5)
outlierTest(regressao1)
vif(regressao1)
coeftest(regressao1, vcov. = vcovHC(regressao1))

plot_model(regressao1, type = "pred",
           terms = c("p176","Raca"), ci.lvl = 0.9) + labs(title = "Opinião sobre Lula a partir da interação entre Raça e Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre Lula")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Op Serra

regressao2 <- lm(p43b ~ p176*Raca + p157 + p158  + estrato + p159+ religiao, data = Eseb2002)
summary(regressao2)

plot(regressao2,1)
plot(regressao2,3)
plot(regressao2,4)
plot(regressao2,5)
outlierTest(regressao2)
vif(regressao2)
coeftest(regressao2, vcov. = vcovHC(regressao2))

plot_model(regressao2, type = "pred",
           terms = c("p176","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ labs(title = "Opinião sobre Serra a partir da interação entre Raça e Renda (2002)", x = "Renda do Domicílio", y = "Opinião sobre Serra")



# Regressão logística do voto no 2º turno

logtd <- glm(p08 ~ p176 + p157 + p158  + estrato + p159+ religiao + Raca, data = Eseb20022turno, family = "binomial")

margins(logtd)
summary(margins(logtd))

plot(logtd,3)
plot(logtd,4)
plot(logtd,5)
outlierTest(logtd)
vif(logtd)
coeftest(logtd, vcov. = vcovHC(logtd))

exp(cbind(coef(logtd), confint(logtd)))

margins(logtd)

summary(margins(logtd))

plot_model(logtd, type = "pred",
           terms = c("p176"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))  + labs(title = "Voto no 2º turno a partir da Renda", x = "Renda do Domicílio", y = "Voto no 2º turno")


# Regressão logística do voto no 2º turno a partir da interação renda e raça

log <- glm(p08 ~ p176*Raca + p157 + p158  + estrato + p159+ religiao + Raca, data = Eseb20022turno, family = "binomial")

plot(log,3)
plot(log,4)
plot(log,5)
outlierTest(log)
vif(log)
coeftest(log, vcov. = vcovHC(log))

plot_model(log, type = "pred",
           terms = c("p176","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))  + labs(title = "Voto no 2º turno a partir da interação de Raça e Renda", x = "Renda do Domicílio", y = "Voto no 2º turno")



########################################################################################################

#Tratamento de dados do ESEB 2010

ESEB2010 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB2010.sav")


# Filtragem dos dados de 2010

Eseb2010 <- ESEB2010 %>% filter(COR < 6, v53 < 11,
                                v63 < 11, v64 <11,
                                RENDAF < 11680, RELIGI < 85) %>%
  mutate(SEXO = as_factor(SEXO),
         COR = as_factor(COR),
         RELIGI = as_factor(RELIGI),
         ESTADO = as_factor(ESTADO),
         renda = RENDAF/PESSOASDOMIC)



#Variável religião  

Outras2010 <- levels(Eseb2010$RELIGI)[c(1,4,5,6,7,9,10,11, na = F)]

Eseb2010 <- Eseb2010 %>%
  mutate(religiao = case_when(RELIGI %in% Outras2010 ~ "Outras",
    RELIGI == "Católica" ~ "Católica",
    RELIGI == "Evangélica pentecostal" ~ "Evangélica",
    RELIGI == "Evangélica não-pentecostal" ~ "Evangélica",
    RELIGI == "Não tem religião" ~ "Não tem religião",
    RELIGI == "É ateu/Não acredita em Deus" ~ "Não tem religião"))

#Variável Raça

Outros2010 <- levels(Eseb2010$COR)[c(4,5)]

Eseb2010 <- Eseb2010 %>%
  mutate(Raca = case_when(COR %in% Outros2010 ~ "Outros",
                          COR == "Preto" ~ "Preto",
                          COR == "Pardo/Moreno" ~ "Pardo",
                          COR == "Branco" ~ "Branco"))


#variavel região 

Norte <- levels(Eseb2010$ESTADO)[c(1,3,4,16,22,23,27)]
Nordeste <- levels(Eseb2010$ESTADO)[c(2,5,6,10,14,17,18,20,25)]
Sul <- levels(Eseb2010$ESTADO)[c(15,21,24)]
Sudeste <- levels(Eseb2010$ESTADO)[c(8,13,19,26)]
Centro_Oeste <- levels(Eseb2010$ESTADO)[c(7,9,11,12)]

Eseb2010 <- Eseb2010 %>%
  mutate(regiao = case_when(ESTADO %in% Sul ~ "Sul",
                            ESTADO %in% Norte ~ "Norte",
                            ESTADO  %in% Nordeste ~ "Nordeste",
                            ESTADO %in% Sudeste ~ "Sudeste",
                            ESTADO %in% Centro_Oeste ~ "Centro-Oeste"))





# Filtragem para os dados do 2º turno de 2010

Eseb20102turno <- ESEB2010 %>% filter(COR < 6, v96<5,
                                      RENDAF < 11680, RELIGI < 85) %>%
  mutate(SEXO = as_factor(SEXO),
         COR = as_factor(COR),
         RELIGI = as_factor(RELIGI),
         ESTADO = as_factor(ESTADO),
         renda = RENDAF/PESSOASDOMIC,
         v96 = case_when(v96 == 1 ~ 1,  
                         v96 == 2 ~ 0))



#Variável religião  


Outras2010 <- levels(Eseb20102turno$RELIGI)[c(1,4,5,6,7,9,10,11, na = F)]

Eseb20102turno <- Eseb20102turno %>%
  mutate(religiao = case_when(RELIGI %in% Outras2010 ~ "Outras",
                              RELIGI == "Católica" ~ "Católica",
                              RELIGI == "Evangélica pentecostal" ~ "Evangélica",
                              RELIGI == "Evangélica não-pentecostal" ~ "Evangélica",
                              RELIGI == "Não tem religião" ~ "Não tem religião",
                              RELIGI == "É ateu/Não acredita em Deus" ~ "Não tem religião"))


#Variável Raça

Outros2010 <- levels(Eseb20102turno$COR)[c(4,5)]

Eseb20102turno <- Eseb20102turno %>%
  mutate(Raca = case_when(COR %in% Outros2010 ~ "Outros",
                          COR == "Preto" ~ "Preto",
                          COR == "Pardo/Moreno" ~ "Pardo",
                          COR == "Branco" ~ "Branco"))


#Variável região  COR %in% Outros ~ "Outros",

Norte <- levels(Eseb20102turno$ESTADO)[c(1,3,4,16,22,23,27)]
Nordeste <- levels(Eseb20102turno$ESTADO)[c(2,5,6,10,14,17,18,20,25)]
Sul <- levels(Eseb20102turno$ESTADO)[c(15,21,24)]
Sudeste <- levels(Eseb20102turno$ESTADO)[c(8,13,19,26)]
Centro_Oeste <- levels(Eseb20102turno$ESTADO)[c(7,9,11,12)]

Eseb20102turno <- Eseb20102turno %>%
  mutate(regiao = case_when(ESTADO %in% Sul ~ "Sul",
                            ESTADO %in% Norte ~ "Norte",
                            ESTADO  %in% Nordeste ~ "Nordeste",
                            ESTADO %in% Sudeste ~ "Sudeste",
                            ESTADO %in% Centro_Oeste ~ "Centro-Oeste"))


# Gráficos das variáveis analisadas

# Raça 

ggplot(Eseb2010, aes(COR)) + geom_bar()


# Renda familiar

ggplot(Eseb2010, aes(RENDAF)) + geom_density()
ggplot(Eseb2010, aes(RENDAF)) + geom_boxplot()


# Opinião sobre o PT

ggplot(Eseb2010, aes(v53)) + geom_bar()

# Opinião sobre Dilma

ggplot(Eseb2010, aes(v63)) + geom_bar()

# Opinião sobre Serra

ggplot(Eseb2010, aes(v64)) + geom_bar()



# Regressões e gráficos com as variáveis independentes raça e renda

regressaotd2010 <- lm(v53 ~ RENDAF + SEXO + IDADE + regiao + ESC+ religiao  + Raca, data = Eseb2010)

summary(regressaotd2010)

plot(regressaotd2010,1)
plot(regressaotd2010,3)
plot(regressaotd2010,4)
plot(regressaotd2010,5)
outlierTest(regressaotd2010)
vif(regressaotd2010)
coeftest(regressaotd2010, vcov. = vcovHC(regressaotd2010))

plot_model(regressaotd2010, type = "pred",
           terms = c("RENDAF"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre o PT a partir da Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre o PT")


#opinião dilma (2010)

regressaotd20101 <- lm(v63 ~ RENDAF + SEXO + IDADE + regiao + ESC+ religiao + Raca, data = Eseb2010)

summary(regressaotd20101)

plot(regressaotd20101,1)
plot(regressaotd20101,3)
plot(regressaotd20101,4)
plot(regressaotd20101,5)
outlierTest(regressaotd20101)
vif(regressaotd20101)
coeftest(regressaotd20101, vcov. = vcovHC(regressaotd20101))

plot_model(regressaotd20101, type = "pred",
           terms = c("RENDAF"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre Dilma a partir da Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre Dilma")


#opinião Serra (2010)

regressaotd20102 <- lm(v64 ~ RENDAF + SEXO + IDADE + regiao + ESC+ religiao + Raca, data = Eseb2010)

summary(regressaotd20102)

plot(regressaotd20101,1)
plot(regressaotd20102,3)
plot(regressaotd20102,4)
plot(regressaotd20102,5)
outlierTest(regressaotd20102)
vif(regressaotd20102)
coeftest(regressaotd20102, vcov. = vcovHC(regressaotd20102))

plot_model(regressaotd20102, type = "pred",
           terms = c("RENDAF"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre Serra a partir da Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre Dilma")



#Regressões e gráficos feitos a partir da influência da interação de raça e renda na opinião política

# Op PT
regressao2010 <- lm(v53 ~ RENDAF*Raca + SEXO + IDADE + regiao + ESC+ religiao, data = Eseb2010)

summary(regressao2010)

plot(regressao2010,1)
plot(regressao2010,3)
plot(regressao2010,4)
plot(regressao2010,5)
outlierTest(regressao2010)
vif(regressao2010)
coeftest(regressao2010, vcov. = vcovHC(regressao2010))

plot_model(regressao2010, type = "pred",
           terms = c("RENDAF","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre o PT a partir da interação Raça e Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre o PT")

#Outlier retirado
Eseb2010 <- Eseb2010 %>% slice(-c(221))

#Op Dilma
regressao20101 <- lm(v63 ~ RENDAF*Raca + SEXO + IDADE + regiao + ESC+ religiao, data = Eseb2010)

summary(regressao20101)

plot(regressao20101,1)
plot(regressao20101,3)
plot(regressao20101,4)
plot(regressao20101,5)
outlierTest(regressao20101)
vif(regressao20101)
coeftest(regressao20101, vcov. = vcovHC(regressao20101))

plot_model(regressao20101,  type = "pred",
           terms = c("RENDAF","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre Dilma a partir da interação Raça e Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre Dilma")


#Op Serra
regressao20102 <- lm(v64 ~ RENDAF*Raca + SEXO + IDADE + regiao + ESC+ religiao, data = Eseb2010)

summary(regressao20102)

plot(regressao20102,1)
plot(regressao20102,3)
plot(regressao20102,4)
plot(regressao20102,5)
outlierTest(regressao20102)
vif(regressao20102)
coeftest(regressao20102, vcov. = vcovHC(regressao20102))

plot_model(regressao20102,  type = "pred",
           terms = c("RENDAF","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Opinião sobre Serra a partir da interação Raça e Renda (2010)", x = "Renda do Domicílio", y = "Opinião sobre Serra")



# Regressão logística do segundo turno (2010)

logtd2010 <- glm(v96 ~ RENDAF + SEXO + IDADE + regiao + ESC+ religiao + Raca, data = Eseb20102turno, family = "binomial")

margins(logtd2010)
summary(margins(logtd2010))

plot(logtd2010,3)
plot(logtd2010,4)
plot(logtd2010,5)
outlierTest(logtd2010)
vif(logtd2010)
coeftest(logtd2010, vcov. = vcovHC(logtd2010))

plot_model(logtd2010, type = "pred",
           terms = c("RENDAF"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7))  + labs(title = "Voto no 2º turno a partir da Renda (2010)", x = "Renda do Domicílio", y = "Voto no 2º turno")



# Regressão logística do segundo turno a partir da interação entre raça e renda

log2010 <- glm(v96 ~ RENDAF*Raca + SEXO + IDADE + regiao + ESC+ religiao, data = Eseb20102turno, family = "binomial")

plot(log2010,3)
plot(log2010,4)
plot(log2010,5)
outlierTest(log2010)
vif(log2010)
coeftest(log2010, vcov. = vcovHC(log2010))

plot_model(log2010,  type = "pred",
           terms = c("RENDAF","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title = "Voto 2º turno - Raça e Renda (2010)", x = "Renda do Domicílio", y = "Voto 2º turno")



######################################################################################################

#Tratamento dos dados do Eseb 2014

ESEB2014 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB2014.sav")

# Filtragem dos dados Eseb 2014 para a análise de opinião

Eseb2014 <- ESEB2014 %>% filter(D27A_COR_IBGE < 6, Q9A < 11,
                                Q10A < 11, Q10C < 11,
                                D20_REND_DOMICILIO < 14920,
                                D24_RELIGIÃO < 98) %>%
  mutate(D21A_PESSOAS_CASA = as.numeric(D21A_PESSOAS_CASA),
         D2_SEXO = as_factor(D2_SEXO),
         D27A_COR_IBGE = as_factor(D27A_COR_IBGE),
         D24_RELIGIÃO = as_factor(D24_RELIGIÃO),
         REGIÃO = as_factor(REGIÃO),
         renda = D20_REND_DOMICILIO/D21A_PESSOAS_CASA)

Eseb2014 <- Eseb2014 %>% slice(-c(549))

#Variável religião  

Outras2014 <- levels(Eseb2014$D24_RELIGIÃO)[c(1,2,4,6,7,8,9,10,95)]

Eseb2014 <- Eseb2014 %>%
  mutate(religiao = case_when(D24_RELIGIÃO %in% Outras2014 ~ "Outras",
                              D24_RELIGIÃO == "Católica" ~ "Católica",
                              D24_RELIGIÃO == "Evangélica" ~ "Evangélica",
                              D24_RELIGIÃO == "Não tem religião" ~ "Não tem religião",
                              D24_RELIGIÃO == "É ateu/agnóstico/ Não acredita em Deus" ~"Não tem religião"))

#Variavel Raça

Outros2014 <- levels(Eseb2014$D27A_COR_IBGE)[c(4,5)]

Eseb2014 <- Eseb2014 %>%
  mutate(Raca = case_when(D27A_COR_IBGE %in% Outros2014 ~ "Outros",
                          D27A_COR_IBGE == "Preto" ~ "Preto",
                          D27A_COR_IBGE == "Pardo" ~ "Pardo",
                          D27A_COR_IBGE == "Branco" ~ "Branco"))



# Filtragem dos dados da Eseb 2014 para o voto declarado no 2º turno

Eseb20142turno <- ESEB2014 %>% filter(D27A_COR_IBGE < 6, 
                                      D20_REND_DOMICILIO < 14920,
                                      D24_RELIGIÃO < 98, Q5P2B < 5) %>%
  mutate(D21A_PESSOAS_CASA = as.numeric(D21A_PESSOAS_CASA) ,
         D2_SEXO = as_factor(D2_SEXO),
         D27A_COR_IBGE = as_factor(D27A_COR_IBGE),
         D24_RELIGIÃO = as_factor(D24_RELIGIÃO),
         REGIÃO = as_factor(REGIÃO),
         Q5P2B = as_factor(Q5P2B)) 



#Variável religião  D24_RELIGIÃO %in% Outras ~ "Outras",

Outrasturno2014 <- levels(Eseb20142turno$D24_RELIGIÃO)[c(1,2,4,6,7,8,9,10,95)]

Eseb20142turno <- Eseb20142turno %>%
  mutate(religiao = case_when(D24_RELIGIÃO %in% Outrasturno2014 ~ "Outras",
                              D24_RELIGIÃO == "Católica" ~ "Católica",
                              D24_RELIGIÃO == "Evangélica" ~ "Evangélica",
                              D24_RELIGIÃO == "Não tem religião" ~ "Não tem religião",
                              D24_RELIGIÃO == "É ateu/agnóstico/ Não acredita em Deus" ~"Não tem religião"))


#Variável Raça

Outrosturno2014 <- levels(Eseb20142turno$D27A_COR_IBGE)[c(4,5)]


Eseb20142turno <- Eseb20142turno %>%
  mutate(Raca = case_when(D27A_COR_IBGE %in% Outrosturno2014 ~ "Outros", 
                          D27A_COR_IBGE == "Preto" ~ "Preto",
                          D27A_COR_IBGE == "Pardo" ~ "Pardo",
                          D27A_COR_IBGE == "Branco" ~ "Branco"))



# Gráficos das variáveis analisadas

# Raça 

ggplot(Eseb2014, aes(D27A_COR_IBGE)) + geom_bar()


# Renda familiar

ggplot(Eseb2014, aes(D20_REND_DOMICILIO)) + geom_density()
ggplot(Eseb2014, aes(D20_REND_DOMICILIO)) + geom_boxplot()


# Opinião sobre o PT

ggplot(Eseb2014, aes(Q9A)) + geom_bar()

# Opinião sobre Dilma

ggplot(Eseb2014, aes(Q10C)) + geom_bar()

# Opinião sobre Aécio

ggplot(Eseb2014, aes(Q10A)) + geom_bar()


#Regressões e gráficos (MQO) realizados com as variáveis renda e raça sobre a opinião política

#Opinião sobre o PT (2014)

regressaotd2014 <- lm(Q9A ~ D20_REND_DOMICILIO + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA+ religiao + Raca, data = Eseb2014)

summary(regressaotd2014)

plot(regressaotd2014,1)
plot(regressaotd2014,3)
plot(regressaotd2014,4)
plot(regressaotd2014,5)
outlierTest(regressaotd2014)
vif(regressaotd2014)
coeftest(regressaotd2014, vcov. = vcovHC(regressaotd2014))

plot_model(regressaotd2014,  type = "pred",
           terms = c("D20_REND_DOMICILIO"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7)) +labs(title ="Opinião sobre o PT - Renda (2014)", x = "Renda do Domicílio", y = "Opinião sobre o PT") 


#opinião sobre dilma (2014)

regressaotd20141 <- lm(Q10C ~ D20_REND_DOMICILIO + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA+ religiao + Raca, data = Eseb2014)

summary(regressaotd20141)

plot(regressaotd20141,1)
plot(regressaotd20141,3)
plot(regressaotd20141,4)
plot(regressaotd20141,5)
outlierTest(regressaotd20141)
vif(regressaotd20141)
coeftest(regressaotd20141, vcov. = vcovHC(regressaotd20141))

plot_model(regressaotd20141,  type = "pred",
           terms = c("D20_REND_DOMICILIO"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7)) +labs(title ="Opinião sobre Dilma - Renda (2014)", x = "Renda do Domicílio", y = "Opinião sobre Dilma") 


#op aécio (2014)

regressaotd20142 <- lm(Q10A ~ D20_REND_DOMICILIO + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA+ religiao + Raca, data = Eseb2014)

summary(regressaotd20142)

plot(regressaotd20142,1)
plot(regressaotd20142,3)
plot(regressaotd20142,4)
plot(regressaotd20142,5)
outlierTest(regressaotd20142)
vif(regressaotd20142)
coeftest(regressaotd20142, vcov. = vcovHC(regressaotd20142))

plot_model(regressaotd20142,  type = "pred",
           terms = c("D20_REND_DOMICILIO"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7)) +labs(title ="Opinião sobre Aécio - Renda (2014)", x = "Renda do Domicílio", y = "Opinião sobre o Aécio") 




# Regressões realizadas a partir da interação entre raça e renda sobre a opinião pública.

# Opinião sobre o PT

regressao2014 <- lm(Q9A ~ D20_REND_DOMICILIO*Raca + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA + religiao, data = Eseb2014)

summary(regressao2014)

plot(regressao2014,1)
plot(regressao2014,3)
plot(regressao2014,4)
plot(regressao2014,5)
outlierTest(regressao2014)
vif(regressao2014)
coeftest(regressao2014, vcov. = vcovHC(regressao2014))

plot_model(regressao2014,  type = "pred",
           terms = c("D20_REND_DOMICILIO","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre o PT - Renda e Raça (2014)", x = "Renda do Domicílio", y = "Opinião sobre o PT") 

#Outlier removido
Eseb2014 <- Eseb2014 %>% slice(-c(549))


# Opinião sobre Aécio

regressao20141 <- lm(Q10A ~ D20_REND_DOMICILIO*Raca + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA + religiao, data = Eseb2014)

summary(regressao20141)

plot(regressao20141,1)
plot(regressao20141,3)
plot(regressao20141,4)
plot(regressao20141,5)
outlierTest(regressao20141)
vif(regressao20141)
coeftest(regressao20141, vcov. = vcovHC(regressao20141))

plot_model(regressao20141,  type = "pred",
           terms = c("D20_REND_DOMICILIO","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre Aécio - Renda e Raça (2014)", x = "Renda do Domicílio", y = "Opinião sobre Aécio")


# Opinião sobre Dilma

regressao20142 <- lm(Q10C ~ D20_REND_DOMICILIO*Raca + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA + religiao, data = Eseb2014)

summary(regressao20142)

plot(regressao20142,1)
plot(regressao20142,3)
plot(regressao20142,4)
plot(regressao20142,5)
outlierTest(regressao20142)
vif(regressao20142)
coeftest(regressao20142, vcov. = vcovHC(regressao20142))

plot_model(regressao20142, type = "pred",
           terms = c("D20_REND_DOMICILIO","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre Dilma - Renda e Raça (2014)", x = "Renda do Domicílio", y = "Opinião sobre Dilma")


# Regressão logística do segundo turno (2014)

logtd2014 <- glm(Q5P2B ~ D20_REND_DOMICILIO + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA+ religiao+ Raca, data = Eseb20142turno, family = "binomial")

margins(logtd2014)
summary(margins(logtd2014))

summary(logtd2014)

plot(logtd2014,3)
plot(logtd2014,4)
plot(logtd2014,5)
outlierTest(logtd2014)
vif(logtd2014)
coeftest(logtd2014, vcov. = vcovHC(logtd2014))

plot_model(logtd2014, type = "pred",
           terms = c("D20_REND_DOMICILIO"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))  + labs(title = "Voto no 2º turno a partir da Renda (2014)", x = "Renda do Domicílio", y = "Voto no 2º turno")



# Regressão logística do segundo turno a partir da interação de renda e raça(2014)

log2014 <- glm(Q5P2B ~ D20_REND_DOMICILIO*Raca + D2_SEXO + D1A_IDADE + REGIÃO + D3_ESCOLA+ religiao, data = Eseb20142turno, family = "binomial")

summary(logtd2014)

plot(log2014,3)
plot(log2014,4)
plot(log2014,5)
outlierTest(log2014)
vif(log2014)
coeftest(log2014, vcov. = vcovHC(log2014))

plot_model(log2014,  type = "pred",
           terms = c("D20_REND_DOMICILIO","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Voto 2º turno - Raça e Renda (2014)", x = "Renda do Domicílio", y = "Voto no 2º turno")

# Outlier retirado

Eseb20142turno <- Eseb20142turno %>% slice(-c(570))




########################################################################################################


# Tratamento dos dados do Eseb 2018

ESEB2018 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB2018.sav")

# Filtragem dos dados para as regressões sobre opinião política


Eseb2018 <- ESEB2018 %>% filter(D12A < 6, Q1501 < 11,
                                Q1605 < 11, Q1607 < 11, Q1610 < 11,
                                D9 < 19080, D20>0) %>%
  mutate(D2_SEXO = as_factor(D2_SEXO),
         D12A = as_factor(D12A),
         D10 = as_factor(D10),
         REG = as_factor(REG)) 


#Variável religião

Outras2018 <- levels(Eseb2018$D10)[c(1,2,4,6,7,8,9,10,95)]

Eseb2018 <- Eseb2018 %>%
  mutate(religiao = case_when(D10 %in% Outras2018 ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião",
                              D10 == "É ateu/agnóstico/ Não acredita em Deus" ~"Não tem religião"))


Outros2018 <- levels(Eseb2018$D12A)[c(4,5)]

Eseb2018 <- Eseb2018 %>%
  mutate(Raca = case_when(D12A %in% Outros2018 ~ "Outros",
                          D12A == "Preto" ~ "Preto",
                          D12A == "Pardo" ~ "Pardo",
                          D12A == "Branco" ~ "Branco"))


# Filtragem dos dados para as regressões sobre o voto no 2º turno

Eseb20182turno <- ESEB2018 %>% filter(D12A < 6,
                                      D9 < 19080, Q12P2_B < 80) %>%
  mutate(D2_SEXO = as_factor(D2_SEXO),
         D12A = as_factor(D12A),
         D10 = as_factor(D10),
         Q12P2_B = case_when(Q12P2_B == 1 ~ 1,  
                             Q12P2_B == 2 ~ 0))


#Variável religião

Outrasturno2018 <- levels(Eseb20182turno$D10)[c(1,2,4,6,7,8,9,10,95)]

Eseb20182turno <- Eseb20182turno %>%
  mutate(religiao = case_when(D10 %in% Outrasturno2018 ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião",
                              D10 == "É ateu/agnóstico/ Não acredita em Deus" ~"Não tem religião"))

#Variável Raça

Outrosturno2018 <- levels(Eseb20182turno$D12A)[c(4,5)]

Eseb20182turno <- Eseb20182turno %>%
  mutate(Raca = case_when(D12A %in% Outrosturno2018 ~ "Outros", D12A == "Preto" ~ "Preto",
                          D12A == "Pardo" ~ "Pardo",
                          D12A == "Branco" ~ "Branco"))



# Gráficos das variáveis analisadas

# Raça 

ggplot(Eseb2018, aes(D12A)) + geom_bar()


# Renda familiar

ggplot(Eseb2018, aes(D9)) + geom_density()
ggplot(Eseb2018, aes(D9)) + geom_boxplot()


# Opinião sobre o PT

ggplot(Eseb2018, aes(Q1501)) + geom_bar()

# Opinião sobre Haddad

ggplot(Eseb2018, aes(Q1605)) + geom_bar()

# Opinião sobre Bolsonaro

ggplot(Eseb2018, aes(Q1607)) + geom_bar()

# Opinião sobre Lula

ggplot(Eseb2018, aes(Q1610)) + geom_bar()


#Regressões e gráficos sobre a influência da renda e raça na opinião política

#Op PT

regressaotd2018 <- lm(Q1501 ~ D9 + D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao+ Raca, data = Eseb2018)

summary(regressaotd2018)

plot(regressaotd2018,1)
plot(regressaotd2018,3)
plot(regressaotd2018,4)
plot(regressaotd2018,5)
outlierTest(regressaotd2018)
vif(regressaotd2018)
coeftest(regressaotd2018, vcov. = vcovHC(regressaotd2018))

plot_model(regressaotd2018, type = "pred",
           terms = c("D9"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre o PT - Renda(2018)", x = "Renda do Domicílio", y = "Opinião sobre o PT")


#op Haddad

regressaotd20181 <- lm(Q1605 ~ D9 + D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao+ Raca, data = Eseb2018)

summary(regressaotd20181)

plot(regressaotd20181,1)
plot(regressaotd20181,3)
plot(regressaotd20181,4)
plot(regressaotd20181,5)
outlierTest(regressaotd20181)
vif(regressaotd20181)
coeftest(regressaotd20181, vcov. = vcovHC(regressaotd20181))

plot_model(regressaotd20181, type = "pred",
           terms = c("D9"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre Haddad - Renda(2018)", x = "Renda do Domicílio", y = "Opinião sobre o Haddad")


#op Bolsonaro

regressaotd20182 <- lm(Q1607 ~ D9 + D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao+ Raca, data = Eseb2018)

summary(regressaotd20182)

plot(regressaotd20182,1)
plot(regressaotd20182,3)
plot(regressaotd20182,4)
plot(regressaotd20182,5)
outlierTest(regressaotd20182)
vif(regressaotd20182)
coeftest(regressaotd20182, vcov. = vcovHC(regressaotd20182))

plot_model(regressaotd20182, type = "pred",
           terms = c("D9"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre Bolsonaro - Renda(2018)", x = "Renda do Domicílio", y = "Opinião sobre Bolsonaro")


#op Lula

regressaotd20183 <- lm(Q1610 ~ D9 + D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao+ Raca, data = Eseb2018)

summary(regressaotd20183)

plot(regressaotd20183,1)
plot(regressaotd20183,3)
plot(regressaotd20183,4)
plot(regressaotd20183,5)
outlierTest(regressaotd20183)
vif(regressaotd20183)
coeftest(regressaotd20183, vcov. = vcovHC(regressaotd20183))

plot_model(regressaotd20183, type = "pred",
           terms = c("D9"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre o Lula - Renda(2018)", x = "Renda do Domicílio", y = "Opinião sobre Lula")



# Regressões e gráficos sobre a influência da raça na variável renda

#Op PT

regressao2018 <- lm(Q1501 ~ D9*Raca+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao, data = Eseb2018)

summary(regressao2018)

plot(regressao2018,1)
plot(regressao2018,3)
plot(regressao2018,4)
plot(regressao2018,5)
outlierTest(regressao2018)
vif(regressao2018)
coeftest(regressao2018, vcov. = vcovHC(regressao2018))

plot_model(regressao2018, type = "pred",
           terms = c("D9","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre o PT - Renda e Raça (2018)", x = "Renda do Domicílio", y = "Opinião sobre o PT")

#Op Haddad

regressao20181 <- lm(Q1605 ~ D9*Raca+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao, data = Eseb2018)

summary(regressao20181)

plot(regressao20181,1)
plot(regressao20181,3)
plot(regressao20181,4)
plot(regressao20181,5)
outlierTest(regressao20181)
vif(regressao20181)
coeftest(regressao20181, vcov. = vcovHC(regressao20181))

plot_model(regressao20181, type = "pred",
           terms = c("D9","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title ="Opinião sobre Haddad - Renda e Raça (2018)", x = "Renda do Domicílio", y = "Opinião sobre Haddad")


# Op Bolsonaro

regressao20182 <- lm(Q1607 ~ D9*Raca+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao, data = Eseb2018)

summary(regressao20182)

plot(regressao20182,1)
plot(regressao20182,3)
plot(regressao20182,4)
plot(regressao20182,5)
outlierTest(regressao20182)
vif(regressao20182)
coeftest(regressao20182, vcov. = vcovHC(regressao20182))

plot_model(regressao20182, type = "pred",
           terms = c("D9","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title ="Opinião sobre Bolsonaro - Renda e Raça (2018)", x = "Renda do Domicílio", y = "Opinião sobre Bolsonaro")



#Op Lula

renda <- log(Eseb2018$D9)

regressao20183 <- lm(Q1610 ~ D9*Raca+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao, data = Eseb2018)

summary(regressao20183)

plot(regressao20183,1)
plot(regressao20183,3)
plot(regressao20183,4)
plot(regressao20183,5)
outlierTest(regressao20183)
vif(regressao20183)
coeftest(regressao20183, vcov. = vcovHC(regressao20183))

plot_model(regressao20183, type = "pred",
           terms = c("D9","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Opinião sobre Lula - Renda e Raça (2018)", x = "Renda do Domicílio", y = "Opinião sobre Lula")


# Regressão logística do segundo turno (2018)

logtd2018 <- glm(Q12P2_B ~ D9+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao + Raca, data = Eseb20182turno, family = "binomial")

margins(logtd2018)
summary(margins(logtd2018))

summary(logtd2018)


plot(logtd2018,3)
plot(logtd2018,4)
plot(logtd2018,5)
outlierTest(logtd2018)
vif(logtd2018)
coeftest(logtd2018, vcov. = vcovHC(logtd2018))

plot_model(logtd2018, type = "pred",
           terms = c("D9"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7))  + labs(title = "Voto no 2º turno a partir da Renda (2018)", x = "Renda do Domicílio", y = "Voto no 2º turno")


# Regressão logística do segundo turno a partir da interação de renda e raça(2018)

log2018 <- glm(Q12P2_B ~ D9*Raca+ D1A_ID + D2_SEXO + REG + D3_ESCOLA+ religiao, data = Eseb20182turno, family = "binomial")

summary(log2018)
plot(log2018,3)
plot(log2018,4)
plot(log2018,5)
outlierTest(log2018)
vif(log2018)
coeftest(log2018, vcov. = vcovHC(log2018))

plot_model(log2018,  type = "pred",
           terms = c("D9","Raca"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title ="Voto 2º turno - Raça e Renda (2018)", x = "Renda do Domicílio", y = "Voto 2º turno")



















































































