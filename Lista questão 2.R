library(readxl)
library(TSA)
library(forecast)
library(fpp3)
library(readr)
library(dplyr)

dados <- read.csv("ESTATÍSTICA/8º Período/ANÁLISE DE SÉRIES TEMPORAIS/Lista 1/Questão 2/vaccinations.csv", sep = ",", header = TRUE)
View(dados)



br = dados %>% filter(tolower(location) == "brazil")



# converter para data
dados1 <- br %>%
  mutate(date = as.Date(date))



dados1 %>%
  pivot_longer(
    cols = where(is.numeric),  # só pega colunas numéricas
    names_to = "name",
    values_to = "value"
  ) %>%
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    title = "Diaria Temperatura e Pressão")
