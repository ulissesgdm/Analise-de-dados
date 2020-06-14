# Analise-de-dados - Exercício 1

# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/Eleição_presidencial_no_Brasil_em_2002
# Vá até o tópico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidência

candidatos <- c("Luiz Inácio Lula da Silva","José Serra","Anthony Garotinho",
                "Ciro Gomes","José Maria de Almeida","Rui Costa Pimenta") 

# Crie um vetor com a sigla do partido de cada candidato

partido <- c("PT","PSDB","PSB","PPS","PSTU","PCO")

# Crie um vetor com o total de votos de cada candidato

votos_candidatos <- c(39455233, 19705445, 15180097, 10170882, 402236, 38619)

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno

total_votos <- sum(votos_candidatos)

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operação aritmética entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- (votos_candidatos/total_votos)*100

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- matrix(c(votos_candidatos,porcentagem_votos), nrow = 6)

# Nomeie as linhas da matriz com o nome dos candidatos
row.names(matriz_votos) <- candidatos

# Nomeie também as colunas
colnames(matriz_votos) <- c("Votos","Porcentagem")

# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual
data.frame(candidatos, partido, votos_candidatos, porcentagem_votos)

# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno

segundo_turno <- c(T,T,F,F,F,F)


# Adicione esta coluna no dataframe

data.frame(candidatos, partido, votos_candidatos, 
           porcentagem_votos,segundo_turno)

eleições_2002 <- data.frame(candidatos, partido, votos_candidatos, 
                            porcentagem_votos,segundo_turno)

# Calcule a soma da porcentagem dos dois candidatos que obtiveram m

soma_mais_votados <- sum(matriz_votos[1:2,2])


# Exiba as informações do dataframe dos dois candidatos com mais votos

candidatos_mais_votados <- eleições_2002[1:2,1:5]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)

q[c(2,3,5)]
  
###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[3:4]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(x = c("d","e"),y = c(1,4))

str(df)

###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27


matriz_a <- matrix(19:27, nrow = 3)


###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

O símbolo situado na primeira linha e na quarta coluna da matriz.

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")

names(y) <- q

(y[1:4])

  ###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind(c(4, 3, 1),c(6, 7, 8))


###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:26

matrix(c(x,y),
       nrow = 2,
       ncol = 4,
       byrow = FALSE)

###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar

z <- c("sep","jul","jun","feb","mar")
y <- c("A","B","C","D","E")
x <- c("17","37","12","48","19")

df <- data.frame(x,y,z)

# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C

df[1:3,1:2]

###############################################################################

# Responder o exercício teórico abaixo

#Descobrir a distribuição racial dos integrantes de órgãos 
#que compõem e interagem com o poder judiciário brasileiro 
#e a possível influência da Lei de Cotas (12.711 de 2012) 
#na democratização racial da justiça nacional.

#Hipótese: A composição racial de advogados, promotores e 
#juízes vem se tornando mais racialmente democrática após a 
#implementação da Lei de Cotas há quase 8 anos.

#Utilizar o censo do Poder Judiciário, Ministério Público 
#e OAB para testar a eficiência de longo prazo da aplicação 
#de cotas no ensino superior e no provimento de cargos públicos.
