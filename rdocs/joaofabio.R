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
dados <- subset(banco, ano >= 2002 & ano <= 2022)
View(dados)

# Modelo
modelo <- lm(juros_reais ~ selic_meta, dados)
summary(modelo)

# Coeficiente de correlação de Pearson
r <- cor(dados$selic_meta, dados$juros_reais)

# R²
r2 <- summary(modelo)$r.squared

## Gráficos
grafic1 <- ggplot(dados, aes(x = selic_meta, y = juros_reais)) +
  geom_jitter(colour = "#A11D21", size = 3) +
  labs(
    x = "Taxa Selic",
    y = "Juros Reais",
    title = "Relação entre SELIC e Juros Reais (2002–2022)"
  ) +
  theme_estat()

grafic2 <- ggplot(dados, aes(x = selic_meta, y = juros_reais)) +
  geom_jitter(colour = "#A11D21", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#003366", size = 1.2) +
  labs(
    x = "Taxa Selic",
    y = "Juros Reais",
    title = "Relação entre SELIC e Juros Reais (2002–2022)"
  ) +
  annotate("text", x = 8, y = 14, label = paste0("r = ", round(r, 2)), size = 5) +
  annotate("text", x = 8, y = 13, label = paste0("R² = ", round(r2, 2)), size = 5) +
  theme_estat()


# Quadro
print_quadro_resumo(dados, selic_meta)
print_quadro_resumo(dados, juros_reais)