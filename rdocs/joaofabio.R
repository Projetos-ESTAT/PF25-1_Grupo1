source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

banco <- read.csv('inflacao.csv', sep = ',', dec = '.')

# Análise 1

dados1 <- subset(banco, ano >= 2002 & ano <= 2022)
#View(dados1)

## Modelo
modelo <- lm(juros_reais ~ selic_meta, dados1)
summary(modelo)

## Coeficiente de correlação de Pearson
r <- cor(dados1$selic_meta, dados1$juros_reais)

## R²
r2 <- summary(modelo)$r.squared

## Gráficos

grafic1 <- ggplot(dados1, aes(x = selic_meta, y = juros_reais)) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.3) +
  labs(
    x = "Taxa Selic",
    y = "Juros Reais",
  ) +
  theme_estat()

grafic2 <- ggplot(dados1, aes(x = selic_meta, y = juros_reais)) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#003366", size = 1.2) +
  labs(
    x = "Taxa Selic",
    y = "Juros Reais",
  ) +
  theme_estat()

## Quadro
#print_quadro_resumo(dados1, selic_meta)
#print_quadro_resumo(dados1, juros_reais)

# Análise 2

dados2 <- banco %>%
  mutate(referencia = as.Date(paste0(referencia, "-01")))

dados_filtrados <- dados2 %>%
  filter(referencia >= as.Date("2002-01-01") & referencia <= as.Date("2022-12-01"))

#view(dados2)

dados_long <- dados_filtrados %>%
  select(referencia, ipca_variacao, ipca15_variacao) %>%
  pivot_longer(cols = c(ipca_variacao, ipca15_variacao),
               names_to = "índice",
               values_to = "variacao")


dados_long$índice <- recode(dados_long$índice,
                            "ipca_variacao" = "IPCA",
                            "ipca15_variacao" = "IPCA-15")

dados_dezembro <- dados2 %>%
  filter(month(referencia) == 12 & year(referencia) >= 2002 & year(referencia) <= 2022) %>%
  mutate(ano = year(referencia)) %>%
  select(ano, ipc_fipe_acumulado_ano)

#view(dados_dezembro)


## Modelo
modelo2 <- lm(ipca_variacao ~ ipca15_variacao, dados1)
summary(modelo2)

## Coeficiente de correlação de Pearson
r_2 <- cor(dados1$ipca15_variacao, dados1$ipca_variacao)

## R²
r2_2 <- summary(modelo2)$r.squared

## Gráficos

grafic3 <- ggplot(dados_long) +
  aes(x = referencia, y = variacao, group = índice, colour = índice) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Índice", values = c("IPCA" = "#A11D21", "IPCA-15" = "#003366")) +
  labs(x = "Ano",
       y = "Variação (%)") +
  theme_estat()

grafic4 <- ggplot(dados_filtrados) +
  aes(x = referencia, y = ipc_fipe_acumulado_ano, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "Ano",
    y = "Variação Acumulada (%)"
  ) +
  theme_estat()

grafic5 <- ggplot(dados1, aes(x = ipca15_variacao, y = ipca_variacao)) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.3) +
  labs(
    x = "IPCA15",
    y = "IPCA",
  ) +
  theme_estat()

grafic6 <- ggplot(dados1, aes(x = ipca15_variacao, y = ipca_variacao)) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#003366", size = 1.2) +
  labs(
    x = "IPCA15",
    y = "IPCA",
  ) +
  theme_estat()



## Quadro
#print_quadro_resumo(dados1, ipca_variacao)
#print_quadro_resumo(dados1, ipca15_variacao)
#print_quadro_resumo(dados_dezembro, ipc_fipe_acumulado_ano)
