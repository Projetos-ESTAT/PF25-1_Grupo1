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
dados1 <- subset(banco, ano >= 2002 & ano <= 2022)

# Análise 1

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

dados_long <- dados_filtrados %>%
  select(referencia, ipca_variacao, ipca15_variacao) %>%
  pivot_longer(cols = c(ipca_variacao, ipca15_variacao),
               names_to = "índice",
               values_to = "variacao")


#view(dados2)

dados_long_ano <- dados_long %>%
  mutate(ano = year(referencia)) %>%
  group_by(ano, índice) %>%
  summarise(media_anual = mean(variacao, na.rm = TRUE), .groups = "drop")

#view(dados_long_ano)


dados_dezembro <- dados2 %>%
  filter(month(referencia) == 12 & year(referencia) >= 2002 & year(referencia) <= 2022) %>%
  mutate(ano = year(referencia)) %>%
  select(ano, ipc_fipe_acumulado_ano)

#view(dados_dezembro)

## Gráficos

grafic3 <- ggplot(dados_long_ano) +
  aes(x = ano, y = media_anual, group = índice, colour = índice) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Índice", values = c("IPCA" = "#A11D21", "IPCA-15" = "#003366")) +
  labs(
    x = "Ano",
    y = "Variação das Médias (%)"
  ) +
  theme_estat()


grafic4 <- ggplot(dados_dezembro) +
  aes(x = ano, y = ipc_fipe_acumulado_ano, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "Ano",
    y = "Variação Acumulada (%)"
  ) +
  theme_estat()

# Análise 3

dados1$mes <- factor(dados1$mes, 
                                levels = 1:12, 
                                labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
                                           "Jul", "Ago", "Set", "Out", "Nov", "Dez"))

media_anual <- dados1 %>%
  group_by(ano) %>%
  summarise(media_ipca = mean(ipca_variacao, na.rm = TRUE))

grafic5 <- ggplot(dados1) +
  aes(
    x = mes,
    y = ipca_variacao
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Mês", y = "Variação Mensal do IPCA (%)") +
  theme_estat()

grafic6 <- ggplot(media_anual) +
  aes(x = ano, y = media_ipca, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "Média Mensal do IPCA (%)") +
  theme_estat()



## Quadro
#print_quadro_resumo(dados1, ipca_variacao)
#print_quadro_resumo(dados1, ipca15_variacao)
#print_quadro_resumo(dados_dezembro, ipc_fipe_acumulado_ano)