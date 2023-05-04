---
title: "Análise de dados (EDA)"
author: "Cristian Villegas"
date: "2023-05-03"
output: 
  html_document: 
    toc: yes
    highlight: zenburn
    fig_caption: yes
    number_sections: yes
    keep_md: yes
---



# Leitura de dados


```r
library(tidyverse)
library(hnp)

dados <- read.csv("../dados/Arthritis.txt", sep="", 
                  stringsAsFactors=TRUE)

dados$id<- 1:nrow(dados)

dados<- tibble(dados)
dados
```

```
## # A tibble: 51 × 9
##    Sex     Age Group Week0 Week1 Week5 Week9 Week13    id
##    <fct> <int> <fct> <int> <int> <int> <int>  <int> <int>
##  1 M        48 A         1     1     1     1      1     1
##  2 M        29 A         1     1     1     1      1     2
##  3 M        59 P         1     1     1     1      1     3
##  4 F        56 P         1     1     1     1      1     4
##  5 M        33 P         1     1     1     1      1     5
##  6 M        61 P         1     1     0     1      1     6
##  7 M        63 A         0     0     1    NA     NA     7
##  8 M        57 P         1     0     1     1      1     8
##  9 M        47 P         1     1     1     0      1     9
## 10 F        42 A         0     0     1    NA      0    10
## # … with 41 more rows
```

# Alguns resumos dos dados

```r
dados %>% 
  group_by(Sex) %>% 
  summarise( n = n())
```

```
## # A tibble: 2 × 2
##   Sex       n
##   <fct> <int>
## 1 F        13
## 2 M        38
```

```r
dados %>% 
  group_by(Sex) %>% 
  summarise(media_Sex = mean(Age))
```

```
## # A tibble: 2 × 2
##   Sex   media_Sex
##   <fct>     <dbl>
## 1 F          51.8
## 2 M          50.2
```

```r
dados %>% 
  group_by(Group) %>% 
  summarise( n = n())
```

```
## # A tibble: 2 × 2
##   Group     n
##   <fct> <int>
## 1 A        27
## 2 P        24
```

```r
dados %>% 
  group_by(Group) %>% 
  summarise( media_Age = mean(Age))
```

```
## # A tibble: 2 × 2
##   Group media_Age
##   <fct>     <dbl>
## 1 A          51.0
## 2 P          50.2
```

```r
dados %>% 
  group_by(Sex, Group) %>% 
  summarise( n = n())
```

```
## # A tibble: 4 × 3
## # Groups:   Sex [2]
##   Sex   Group     n
##   <fct> <fct> <int>
## 1 F     A         7
## 2 F     P         6
## 3 M     A        20
## 4 M     P        18
```

# Gráficos de interesse

```r
ggplot(dados, aes(x = Sex)) +
  geom_bar()
```

![](EDA__files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ggplot(dados, aes(x = Group)) +
  geom_bar()
```

![](EDA__files/figure-html/unnamed-chunk-3-2.png)<!-- -->

# Transformando os dados


```r
dados_longos<- dados %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "week",
    names_prefix = "Week",
    values_to = "Y",
    values_drop_na = TRUE
  )

#stringr::str_order()
dados_longos$week<- factor(dados_longos$week, 
                           levels = c(0, 1, 5, 9, 13) )

dados_longos %>% 
  group_by(week) %>% 
  summarise( n = n())
```

```
## # A tibble: 5 × 2
##   week      n
##   <fct> <int>
## 1 0        51
## 2 1        51
## 3 5        48
## 4 9        45
## 5 13       42
```

# Gráficos de perfis

## Sexo F

```r
dados_longos %>% filter(Sex == "F") %>%
  ggplot(aes(week, Y, group = id)) +
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1))+
  facet_wrap(~id)
```

![](EDA__files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Sexo M


```r
dados_longos %>% filter(Sex == "M") %>%
  ggplot(aes(week, Y, group = id)) +
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1))+
  facet_wrap(~id)
```

![](EDA__files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(dados_longos, aes(Sex, Age)) + 
  geom_boxplot()+
  theme_minimal()
```

![](EDA__files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
ggplot(dados_longos, aes(Group, Age)) + 
  geom_boxplot()+
  theme_minimal()
```

![](EDA__files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
ggplot(dados_longos, aes(x = Sex, fill = Group)) + 
  geom_bar(position=position_dodge())+
  theme_minimal()
```

![](EDA__files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
ggplot(dados_longos, aes(Age)) + 
  geom_histogram(fill = "yellow", 
                 aes(y = after_stat(density)), bins=6)+
  geom_density(col = "red", linewidth = 2)+
  facet_wrap(~Sex)+
  theme_minimal()
```

![](EDA__files/figure-html/unnamed-chunk-6-5.png)<!-- -->

# Ajuste de modelos

## cloglog

```r
modelo_cloglog<- glm(Y ~ as.numeric(Sex) + 
               Age + 
               as.numeric(Group) + 
               as.numeric(week),
             family = binomial(link = "cloglog"),
             data= dados_longos)
modelo_cloglog$family
```

```
## 
## Family: binomial 
## Link function: cloglog
```

```r
summary(modelo_cloglog)
```

```
## 
## Call:
## glm(formula = Y ~ as.numeric(Sex) + Age + as.numeric(Group) + 
##     as.numeric(week), family = binomial(link = "cloglog"), data = dados_longos)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1146  -1.2417   0.6273   0.8684   1.2639  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        0.859674   0.628392   1.368 0.171295    
## as.numeric(Sex)    0.295307   0.197994   1.491 0.135832    
## Age               -0.008573   0.007965  -1.076 0.281788    
## as.numeric(Group) -0.588070   0.169064  -3.478 0.000504 ***
## as.numeric(week)   0.063655   0.059930   1.062 0.288164    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 282.26  on 236  degrees of freedom
## Residual deviance: 267.06  on 232  degrees of freedom
## AIC: 277.06
## 
## Number of Fisher Scoring iterations: 6
```

```r
hnp(modelo_cloglog, print.on = TRUE)
```

```
## Binomial model
```

![](EDA__files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
plot(predict.glm(modelo_cloglog, type="response")~predict.glm(modelo_cloglog, type="link"),
     ylab = "Probabilidades",
     xlab =  "modelo cloglog",
     ylim=c(0,1))
```

![](EDA__files/figure-html/unnamed-chunk-7-2.png)<!-- -->

## logit

```r
modelo_logit<- glm(Y ~ as.numeric(Sex) + 
                       Age + 
                       as.numeric(Group) + 
                       as.numeric(week),
                     family = binomial(link = "logit"),
                     data= dados_longos)
modelo_logit$family
```

```
## 
## Family: binomial 
## Link function: logit
```

```r
summary(modelo_logit)
```

```
## 
## Call:
## glm(formula = Y ~ as.numeric(Sex) + Age + as.numeric(Group) + 
##     as.numeric(week), family = binomial(link = "logit"), data = dados_longos)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0130  -1.2367   0.6319   0.8697   1.2642  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)   
## (Intercept)        2.06399    1.14583   1.801  0.07165 . 
## as.numeric(Sex)    0.46639    0.33413   1.396  0.16277   
## Age               -0.01268    0.01461  -0.868  0.38548   
## as.numeric(Group) -0.99584    0.30409  -3.275  0.00106 **
## as.numeric(week)   0.08382    0.10655   0.787  0.43147   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 282.26  on 236  degrees of freedom
## Residual deviance: 268.04  on 232  degrees of freedom
## AIC: 278.04
## 
## Number of Fisher Scoring iterations: 4
```

```r
hnp(modelo_logit, print.on = TRUE)
```

```
## Binomial model
```

![](EDA__files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
plot(predict.glm(modelo_logit, type="response")~predict.glm(modelo_logit, type="link"),
     ylab = "Probabilidades",
     xlab =  "modelo logit",
     ylim=c(0,1))
```

![](EDA__files/figure-html/unnamed-chunk-8-2.png)<!-- -->

## probit


```r
modelo_probit<- glm(Y ~ as.numeric(Sex) + 
                     Age + 
                     as.numeric(Group) + 
                     as.numeric(week),
                   family = binomial(link = "probit"),
                   data= dados_longos)
modelo_probit$family
```

```
## 
## Family: binomial 
## Link function: probit
```

```r
summary(modelo_probit)
```

```
## 
## Call:
## glm(formula = Y ~ as.numeric(Sex) + Age + as.numeric(Group) + 
##     as.numeric(week), family = binomial(link = "probit"), data = dados_longos)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0435  -1.2385   0.6287   0.8676   1.2667  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        1.256832   0.676520   1.858 0.063199 .  
## as.numeric(Sex)    0.285428   0.201061   1.420 0.155722    
## Age               -0.008069   0.008614  -0.937 0.348903    
## as.numeric(Group) -0.600992   0.178846  -3.360 0.000778 ***
## as.numeric(week)   0.054257   0.063396   0.856 0.392089    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 282.26  on 236  degrees of freedom
## Residual deviance: 267.77  on 232  degrees of freedom
## AIC: 277.77
## 
## Number of Fisher Scoring iterations: 4
```

```r
hnp(modelo_probit, print.on = TRUE)
```

```
## Binomial model
```

![](EDA__files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
plot(predict.glm(modelo_probit, type="response")~predict.glm(modelo_probit, type="link"),
     ylab = "Probabilidades",
     xlab = "modelo probit",
     ylim=c(0,1))
```

![](EDA__files/figure-html/unnamed-chunk-9-2.png)<!-- -->

## cauchit


```r
modelo_cauchit<- glm(Y ~ as.numeric(Sex) + 
                      Age + 
                      as.numeric(Group) + 
                      as.numeric(week),
                    family = binomial(link = "cauchit"),
                    data= dados_longos)
modelo_cauchit$family
```

```
## 
## Family: binomial 
## Link function: cauchit
```

```r
summary(modelo_cauchit)
```

```
## 
## Call:
## glm(formula = Y ~ as.numeric(Sex) + Age + as.numeric(Group) + 
##     as.numeric(week), family = binomial(link = "cauchit"), data = dados_longos)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8884  -1.2164   0.6416   0.8881   1.2159  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)   
## (Intercept)        2.008709   1.265027   1.588  0.11231   
## as.numeric(Sex)    0.428442   0.327513   1.308  0.19082   
## Age               -0.007066   0.015284  -0.462  0.64386   
## as.numeric(Group) -1.039856   0.378648  -2.746  0.00603 **
## as.numeric(week)   0.031014   0.107922   0.287  0.77383   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 282.26  on 236  degrees of freedom
## Residual deviance: 269.38  on 232  degrees of freedom
## AIC: 279.38
## 
## Number of Fisher Scoring iterations: 6
```

```r
hnp(modelo_cauchit, print.on = TRUE)
```

```
## Binomial model
```

![](EDA__files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
plot(predict.glm(modelo_cauchit, type="response")~predict.glm(modelo_cauchit, type="link"),
     ylab = "Probabilidades",
     xlab =  "modelo cauchit",
     ylim=c(0,1))
```

![](EDA__files/figure-html/unnamed-chunk-10-2.png)<!-- -->

