library(readxl)
library(TSA)
library(forecast)
library(fpp3)
library(readr)
library(dplyr)

dados <- read.csv("ESTATÍSTICA/8º Período/ANÁLISE DE SÉRIES TEMPORAIS/Lista 1/Questão 3/desmatamento_prodes.csv", sep = ",", header = TRUE)
View(dados)



## Transformando a variavel Sales no formato TS
y = ts( dados$maranhao, start = 1988,end = 2022,  frequency = 4  )

plot.ts(y)

library(tidyverse)


dados %>%
  pivot_longer(-referencia) %>%
  ggplot(aes(x = referencia, y = value, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(~name, scales = "free_y") +
  labs(
    title = "Desmatamento anual por estado (PRODES)",
    x = "Ano",
    y = "Área desmatada (km²)"
  )


dados %>%
  pivot_longer(-referencia) %>%
  ggplot(aes(x = referencia, y = value, colour = name)) +
  geom_line(size = 1) +
  labs(
    title = "Desmatamento anual por estado (PRODES)",
    x = "Ano",
    y = "Área desmatada (km²)"
  )
