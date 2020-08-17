Exercicio 9
================

### Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco da mesma forma que nos exercicios anteriores

``` r
library(tidyverse)
library(haven)
library(scales)
library(sjPlot)

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q1607 < 11, 
         Q18 < 11,
         D9 < 9999998,
         Q1501 < 11)
```

### Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))


ggplot(banco, aes(religiao, ..count../sum(..count..) )) +
  geom_bar() +
  scale_y_continuous(labels = percent)
```

![](exercicio_9_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Faça uma regressão linear avaliando em que medida as variáveis independentes utilizadas nos exercícios 7 e 8, idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) explicam a avaliação de Bolsonaro (Q1607), mas com uma interação entre as variáveis religião e sexo. Exiba o resultado da regressão e interprete os valores dos coeficientes \(\beta\)s estimados.

``` r
regressao <- lm(Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q1501 + Q18 + D2_SEXO*religiao, data = banco)
summary(regressao)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q1501 + 
    ##     Q18 + D2_SEXO * religiao, data = banco)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -8.942 -2.561  0.361  2.303  9.052 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       6.114e+00  5.915e-01  10.338   <2e-16 ***
    ## D1A_ID                            1.065e-02  6.255e-03   1.703   0.0888 .  
    ## D3_ESCOLA                        -1.134e-01  4.491e-02  -2.524   0.0117 *  
    ## D9                               -3.632e-05  2.768e-05  -1.312   0.1897    
    ## Q1501                            -3.956e-01  2.370e-02 -16.696   <2e-16 ***
    ## Q18                               3.150e-01  2.607e-02  12.083   <2e-16 ***
    ## D2_SEXO                          -6.115e-01  2.438e-01  -2.508   0.0122 *  
    ## religiaoEvangélica                1.181e+00  6.146e-01   1.921   0.0549 .  
    ## religiaoNão tem religião          1.986e-01  1.059e+00   0.188   0.8512    
    ## religiaoOutras                   -1.583e+00  9.503e-01  -1.666   0.0960 .  
    ## D2_SEXO:religiaoEvangélica       -3.412e-01  3.895e-01  -0.876   0.3812    
    ## D2_SEXO:religiaoNão tem religião -1.889e-01  6.979e-01  -0.271   0.7867    
    ## D2_SEXO:religiaoOutras            5.041e-01  6.067e-01   0.831   0.4062    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.297 on 1449 degrees of freedom
    ## Multiple R-squared:  0.3028, Adjusted R-squared:  0.297 
    ## F-statistic: 52.44 on 12 and 1449 DF,  p-value: < 2.2e-16

O intercepto da correlação é de 6.114, o que significa que se todos as
variáveis independentes forem iguais a 0 o valor da variável dependente
predita será de 6.114.

As vaiáveis de idade (0.01), auto atribuição (0.31) ideológica tem uma
correlação positiva com a avaliação do candidato Bolsonaro, ou seja,
quanto mais velho e mais identificado com a direita maiores as chances
do entrevistado responder positivamente a avaliação de Bolsonaro.

Já as variáveis que medem escolaridade (-0.10), renda (-0.000036) e
avaliação do PT (-0.39) possuem uma correlação negativa com a avaliação
de Bolsonaro. O que significa que quanto maior a renda, a escolaridade e
a simpatia para com o PT maiores as chances de uma avaliação negativa ao
candidato Bolsonaro.

Já quanto as variáveis categóricas observamos a repetição da tendência
do exercício anterior. Mulheres avaliam Bolsonaro de fora mais negativa
que os homens (-0.61) e evangélicos avaliam o candidato mais
positivamente que católicos (1.18). A diferença dessa regressão com a do
exercício anterior é a correlação positiva entre pessoas que não
professam nenhuma religião e o apoio a Bolsonaro em comparação com os
católicos. Isso indica que as variáveis inseridas nessa nova regressão
diminuiram efeitos negativos va avaliação gerados mais por essas novas
variáveis do que pelo fator da não religiosidade.

Por fim é possível observar a regressão da interação entre as varáveis
religião e sexo apontam para uma avaliação negativa das mulheres
evangélicas (-0.34) e sem religião (-0.18) avaliam bolsonaro mais
negativamente que homens católicos, no entanto, mulheres de outras
religiões apresentaram valores positivos na avaliação a Bolsonaro se
comparados com homens católicos (0.5).

### Interprete a significancia estatística dos coeficientes estimados

Na regressão apenas o intercepto, e as variáveis referentes a
escolaridade, avaliação do PT, auto atribuição ideológica e o sexo
apresentaram signifcância estatísca, ou seja, um p-valor abaixo de 0.05,
também são as correleções que apresentam o erro padrão bem menor que os
coeficientes correspondentes.

Já as correlações referentes as todas as outras variáveis apresentaram
um p-valor alto, apenas as variáveis da religião evangélica e outras
religiões apresentaram um p-valor menor que 0.1 e talvez possam ser
levadas em conta (como veremos no gráfico feito na qustão seguinte o
intervalo de confiança dessas variáveis não se sobrepõem tanto). É
importante ressaltar, contudo, que o p-valor alto nas variáveis
categóricas indica apenas que não há uma diferença estatística
significante entre essas variáveis e a categoria de referência.

### Faça um gráfico que mostre a interação entre as duas variáveis. Interprete o resultado apresentado

``` r
plot_model(regressao, type = "pred", terms = c("religiao","D2_SEXO"),
           ci.lvl = 0.9)
```

![](exercicio_9_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

O gráfico representa bem as variações de crença e sexo para o apoio ao
candidato Bolsonaro. É possível observar que há uma dinstição
significante entre homens e mulheres envangélicos e católicos, sendo o
sexo femino significamente (intervalos de cofiança não se sobrepõem)
menos entusiasmado com Bolsonaro que os homens que professam essas
religiões.

Já entre outras religiões os valores são bastante similares, e entre os
que não tem religião há um apoio maior entre os homens mas que não tem
significância, já que os intervalos de confiança se sobrepõem bastante.
