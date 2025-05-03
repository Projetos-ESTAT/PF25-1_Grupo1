source("rdocs/source/packages.R")
install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(readr)
library(psych)
inflacao <- read_csv("inflacao.csv")
inflacao_analises <- inflacao[inflacao$ano > 2001,]
inflacao_analises <- inflacao_analises[inflacao_analises$ano < 2023,]

################
## Analise 1 ##
##############

analise_1 <- data.frame(inflacao_analises[,2], inflacao_analises[,7], inflacao_analises[,22], inflacao_analises[,24])
analise_1[,3] <- format(analise_1[,3], nsmall = 2)
analise_1[,3] <- as.numeric(analise_1[,3])
unlist(analise_1)
names(analise_1)[1] <- "ano" 
names(analise_1)[2] <- "ipca_acumulado_doze_meses" 
names(analise_1)[3] <- "selic_meta" 
names(analise_1)[4] <- "juros_reais"
analise_temporal <- analise_1 %>%
  group_by(ano) %>%
  summarise(
    Inflacao12 = mean(ipca_acumulado_doze_meses),
    Selic = mean(selic_meta),
    Juros = mean(juros_reais)
  )


#Grafico
analise1_graf <- ggplot(analise_temporal) +
  geom_line(aes(x = ano, y = Inflacao12, color = "Inflação (Doze meses)"),linewidth = 1) +
  geom_point(aes(x = ano, y = Inflacao12), size = 1) +
  geom_line(aes(x = ano, y = Selic, color = "Taxa SELIC"),linewidth = 1) +
  geom_point(aes(x = ano, y = Selic), size = 1)+
  geom_line(aes(x = ano, y = Juros, color = "Juros"), linewidth = 1) +
  geom_point(aes(x = ano, y = Juros), size = 1,)+
  labs(x = "Ano", y = "Valor", colour = "Legenda") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")


################
## Análise 2 ##
##############

analise_2 <- data.frame(inflacao_analises[,7], inflacao_analises[,22])
describe(analise_2$ipca_acumulado_doze_meses)
describe(analise_2$selic_meta)
correlacao <- cor(analise_2$ipca_acumulado_doze_meses, analise_2$selic_meta, method = "pearson")


#Grafico

analise2_graf <- ggplot(analise_2) +
  aes(x = selic_meta, y = ipca_acumulado_doze_meses) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Taxa Selic",
    y = "Inflação em 12 meses"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
################
## Análise 3 ##
##############

#Praparando a 
analise_3 <- data.frame(inflacao_analises[,2], inflacao_analises[,25])
analise_3[1:12,1] <- "FHC" 
analise_3[13:60,1] <- "Lula1"
analise_3[61:108,1] <- "Lula2"
analise_3[109:156,1] <- "Dilma1"
analise_3[157:204,1] <- "Dilma2/Temer"
analise_3[205:252,1] <- "Bolsonaro"

#Separando os mandatos para a análise para a tabela
FHC_Medidas <- filter(analise_3, ano == "FHC")
Lula1_Medidas <- filter(analise_3, ano == "Lula1")
Lula2_Medidas <- filter(analise_3, ano == "Lula2")
Dilma_Medidas <- filter(analise_3, ano == "Dilma1")
Temer_Medidas <- filter(analise_3, ano == "Dilma2/Temer")
Bolso_Medidas <- filter(analise_3, ano == "Bolsonaro")

#Medidas

describe(FHC_Medidas$salario_minimo)
sd(FHC_Medidas$salario_minimo)
quantile(FHC_Medidas$salario_minimo, 0.25)
quantile(FHC_Medidas$salario_minimo, 0.75)

describe(Lula1_Medidas$salario_minimo)
sd(Lula1_Medidas$salario_minimo)
quantile(Lula1_Medidas$salario_minimo, 0.25)
quantile(Lula1_Medidas$salario_minimo, 0.75)

describe(Lula2_Medidas$salario_minimo)
sd(Lula2_Medidas$salario_minimo)
quantile(Lula2_Medidas$salario_minimo, 0.25)
quantile(Lula2_Medidas$salario_minimo, 0.75)

describe(Dilma_Medidas$salario_minimo)
mean(Dilma_Medidas$salario_minimo)
sd(Dilma_Medidas$salario_minimo)
quantile(Dilma_Medidas$salario_minimo, 0.25)
quantile(Dilma_Medidas$salario_minimo, 0.75)

describe(Temer_Medidas$salario_minimo)
sd(Temer_Medidas$salario_minimo)
quantile(Temer_Medidas$salario_minimo, 0.25)
quantile(Temer_Medidas$salario_minimo, 0.75)

describe(Bolso_Medidas$salario_minimo)
mean(Bolso_Medidas$salario_minimo)
sd(Bolso_Medidas$salario_minimo)
quantile(Bolso_Medidas$salario_minimo, 0.25)
quantile(Bolso_Medidas$salario_minimo, 0.75)
#Gráfico

analise3_box <- ggplot(analise_3) +
  aes(x = reorder(ano, salario_minimo, FUN = median), y = salario_minimo) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Presidente regente", y = "Salario Mínimo") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")