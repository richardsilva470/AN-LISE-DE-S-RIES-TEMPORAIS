library(readxl)
library(TSA)
library(forecast)
library(fpp3)
library(readr)

dados <- read.csv("ESTATÍSTICA/8º Período/ANÁLISE DE SÉRIES TEMPORAIS/Nova pasta/DailyDelhiClimateTrain.csv", sep = ",", header = TRUE)
View(dados)



# meantemp -------------------------------------------------------------------------


## Transformando a variavel Sales no formato TS
y = ts( dados$meantemp, start = 2013,end = 2017,  frequency = 4  )

plot.ts( y  )



# Diario ------------------------------------------------------------------


# converter para data
dados1 <- dados %>%
  mutate(date = as.Date(date))


### Plot das series
dados1 %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Diaria Temperatura e Pressão")




# Mensal por média -------------------------------------------------------------------

# Criar uma coluna mensal
dados_mensal <- dados1 %>%
  mutate(mes = floor_date(date, "month")) %>%      # arredonda a data para o 1º dia do mês
  group_by(mes) %>%
  summarise(across(meantemp:meanpressure, mean, na.rm = TRUE))

# Visualizar as médias mensais em gráfico
dados_mensal %>%
  pivot_longer(-mes) %>%
  ggplot(aes(x = mes, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    title = "Médias Mensal de Temperatura e Pressão")


# Trimestral pela Média ---------------------------------------------------


# Criar uma coluna trimestral
dados_trimestral <- dados1 %>%
  mutate(trimestre = floor_date(date, "quarter")) %>%  # arredonda a data para o início do trimestre
  group_by(trimestre) %>%
  summarise(across(meantemp:meanpressure, mean, na.rm = TRUE))

# Visualizar as médias trimestrais em gráfico
dados_trimestral %>%
  pivot_longer(-trimestre) %>%
  ggplot(aes(x = trimestre, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    title = "Médias Trimestrais de Temperatura e Pressão")

