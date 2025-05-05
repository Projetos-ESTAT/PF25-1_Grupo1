source("rdocs/source/packages.R")

# install.packages(c("xlsx", "tseries", "nortest", "car", "lmtest", "MASS", "xtable", "knitr", "tidyverse"))
# tinytex::tlmgr_install(c("multirow", "anyfontsize", "setspace", "multibib", "newfloat"))
# tinytex::tlmgr_option("install.packages", "on-request")

require(tidyverse)
require(car)
require(lmtest)
require(MASS)
require(tseries)
require(nortest)
require(knitr)
require(xtable)

inf <- read.csv("C:/Users/USER/Desktop/Estatística/ESTAT/inflacao.csv")
summary(inf)
attach(inf)

envelope_LR <- function(fit, main.title = "Envelope", faixa.fixed = NULL, number.id = NULL, colour = "#A11D21") { 
  B <- 100; #number of replicates
  X <- model.matrix(fit)
  p <- ncol(X);  n <- nrow(X)
  
  #***parameters for parametric bootstrap***#
  
  Menvelope_r <- matrix(numeric(0),nrow=n,ncol=B)
  
  #------------> residuals for the observed sample<--------------#
  ts <- rstudent(fit) #Studentized residuals
  betahat <- as.numeric(fit$coefficients)
  sigma2hatc <- sum(resid(fit)^2)/(n-p) #sigma2 estimate (constant)
  sigma2hat <- rep(sigma2hatc,n)
  
  for(j in 1:B){		        
    ygen <- rnorm(n, X%*%betahat, sqrt(sigma2hat)) 
    fitb <- lm(ygen~X[,2:p])
    Res <- rstudent(fitb)
    Menvelope_r[,j] = Res
  }
  Menvelope <- apply(Menvelope_r,2,sort);          
  res <-    ts;    
  res_min  <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.05)));         
  res_mean <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.5)));                              
  res_max  <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.95)));           
  faixa <- range(res,res_min,res_max)
  if(is.vector(faixa.fixed)) faixa <- faixa.fixed
  par(mar=c(5.0,5.0,4.0,2.0))
  v <- qqnorm(res, main=main.title, xlab="Percentil esperado via N(0,1)", ylab="Resíduos Studentizados", ylim=faixa, pch=16, cex=1, cex.lab=2.0, cex.axis=1.5, cex.main=2.0)
  if(is.numeric(number.id)){
    identify(v$x,v$y,cex =1.3, n=number.id) #identify points in the plot
  }
  par(new=T)
  #
  qqnorm(res_min,axes=F,main = "",xlab="",ylab="",type="l",ylim=faixa,lty=1,lwd=2.0)
  par(new=T)
  qqnorm(res_max,axes=F,main = "",xlab="",ylab="", type="l",ylim=faixa,lty=1,lwd=2.0)
  par(new=T)
  qqnorm(res_mean,axes=F,xlab="",main = "", ylab="", type="l",ylim=faixa,lty=2,lwd=2.0)
}#ends function

#############################################################

filtro <- ano >= 2002 & ano <= 2022

ipca_filtrado <- ipca_acumulado_ano[filtro]
ipca_filtrade <- ipca_acumulado_doze_meses[filtro]
anno <- ano[filtro]
salario.minimo <- salario_minimo[filtro]
incc.variacao <- incc_variacao[filtro]
selic.meta <- selic_meta[filtro]

#print(data.frame(ano = anno, ipca = ipca_filtrado, salario = salario.minimo))


inf <- data.frame(
  ipca = ipca_filtrado,
  ipcad = ipca_filtrade,
  salario = salario.minimo,
  incc = incc.variacao,
  selic = selic.meta
)

###################           PROJETO 1

graf_disp1 <- ggplot(inf) +
  aes(x = (ipca_filtrado), y = salario.minimo) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "IPCA acumulado",
    y = "Salário mínimo"
  ) +
  theme_estat() 


graf_lin1 <- ggplot(inf) +
  aes(x = anno) +
  
  # Linha e pontos para sal?rio m?nimo
  geom_line(aes(y = log(salario.minimo), colour = "Logaritimo do salário mínimo")) +
  geom_point(aes(y = log(salario.minimo), colour = "Logaritimo do salário mínimo"), size = 2) +
  
  # Linha e pontos para imposto
  geom_line(aes(y = (ipca_filtrado), colour = "IPCA"), size = 1) +
  geom_point(aes(y = (ipca_filtrado), colour = "IPCA"), size = 2) +
  
  # T?tulos e r?tulos
  labs(x = "Ano", y = "Variação das variáveis", colour = "Legenda") +
  
  #   # Cores personalizadas
  scale_colour_manual(
    values = c(
      "Logaritimo do salário mínimo" = "#003366",    # Vermelho escuro
      "IPCA" = "#A11D21"      # Azul Dodger
    )
  )+
  theme_estat()


cor.test(salario.minimo, ipca_filtrado, method = "pearson")

cor.test(salario.minimo, ipca_filtrado, method = "spearman")

# ###################           PROJETO 2
graf_disp2 <- ggplot(inf) +
  aes(x = (ipca_filtrade), y = (selic.meta)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "SELIC",
    y = "IPCA acumulado"
  )+
  theme_estat()


fit <- lm((ipca_filtrade)~selic.meta); summary(fit)

#r1 <- qqPlot(fit, colour = "#A11D21")

#pdf("qqplot_grafico.pdf")   # Abre o dispositivo para salvar em PDF
#qqPlot(fit, colour = "#A11D21")  # Cria o gráfico
#dev.off()                  # Fecha e salva o arquivo



envelope_ggplot <- function(fit, colour = "#A11D21") {
  B <- 100
  X <- model.matrix(fit)
  p <- ncol(X); n <- nrow(X)
  
  ts <- rstudent(fit)  # resíduos studentizados
  betahat <- as.numeric(fit$coefficients)
  sigma2hatc <- sum(resid(fit)^2)/(n-p)
  sigma2hat <- rep(sigma2hatc, n)
  
  Menvelope_r <- matrix(NA, nrow = n, ncol = B)
  for(j in 1:B){
    ygen <- rnorm(n, X %*% betahat, sqrt(sigma2hat))
    fitb <- lm(ygen ~ X[,2:p])
    Menvelope_r[, j] <- sort(rstudent(fitb))
  }
  
  ts_sorted <- sort(ts)
  theor_quantiles <- qnorm(ppoints(n))
  res_min  <- apply(Menvelope_r, 1, quantile, probs = 0.05)
  res_med  <- apply(Menvelope_r, 1, quantile, probs = 0.50)
  res_max  <- apply(Menvelope_r, 1, quantile, probs = 0.95)
  
  df <- data.frame(
    Quantis = theor_quantiles,
    Residuos = ts_sorted,
    Min = res_min,
    Med = res_med,
    Max = res_max
  )
  
  ggplot(df, aes(x = Quantis, y = Residuos)) +
    geom_point(color = colour, size = 2) +
    geom_line(aes(y = Min), linetype = "dashed", color = "gray50") +
    geom_line(aes(y = Max), linetype = "dashed", color = "gray50") +
    geom_line(aes(y = Med), linetype = "dotted", color = "gray40") +
    labs(x = "Quantis teóricos (N(0,1))", y = "Resíduos studentizados") +
    theme_estat()
}

grafenv <- envelope_ggplot(fit)

#r2 <- envelope_LR(fit,  main.title = "Envelope")

#pdf("envelope_grafico.pdf")    # Abre para salvar em PDF
#r2 <- envelope_LR(fit, main.title = "Envelope")  # Gera o gráfico
#dev.off()                      # Fecha e salva



# Cria data frame com valores ajustados e resíduos studentizados
df_residuos <- data.frame(
  Ajustado = fitted(fit),
  Residuo = rstudent(fit)
)

# Cria o gráfico
grafico_residuos <- ggplot(df_residuos, aes(x = Ajustado, y = Residuo)) +
  geom_point(color = "#A11D21", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    x = "Valor Ajustado",
    y = "Resíduo Studentizado"
  ) +
  theme_estat()

# normalidade
jarque.bera.test(rstudent(fit))
dwtest(fit)
gqtest(fit)

cor.test(selic.meta, ipca_filtrade, method = "pearson")

cor.test(selic.meta, ipca_filtrade, method = "spearman")

# ###################           PROJETO 3

graf_lin3 <- ggplot(inf, aes(x = factor(anno), y = incc.variacao)) +
  geom_col(fill = "#A11D21") +
  scale_x_discrete(breaks = unique(anno)) +
  labs(
    x = "Ano",
    y = "INCC"
  ) +
  theme_estat()


boxplot <- ggplot(inf, aes(x = factor(anno), y = incc.variacao)) +
  geom_boxplot(fill = "#A11D21") +
  scale_x_discrete(breaks = unique(anno)) +
  labs(
    x = "Ano",
    y = "INCC"
  ) +
  theme_estat()

mean(incc.variacao)
median(incc.variacao)
var(incc.variacao)
sd(incc.variacao)
min(incc.variacao)
max(incc.variacao)
