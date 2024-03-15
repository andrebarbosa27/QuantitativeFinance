### Aula sobre Taxa de Câmbio
rm(list = ls())

library(ipeadatar)
Busca <- ipeadatar::search_series()

### Obtenção das Séries Temporais
TCN <- ipeadata("PAN12_ERV12")
TCN_ts <- ts(TCN$value, start = c(1930,1), frequency = 12)

TCR_BR_US <- ipeadata("GAC12_TCEREUA12")
TCR_BR_US_ts <- ts(TCR_BR_US$value, start = c(1980,1), frequency = 12)

TCR_BR_ALE <- ipeadata("GAC12_TCERDEU12")
TCR_BR_ALE_ts <- ts(TCR_BR_ALE$value, start = c(1980,1), frequency = 12)

TCR_BR_CHI <- ipeadata("GAC12_TCERCHN12")
TCR_BR_CHI_ts <- ts(TCR_BR_CHI$value, start = c(1980,1), frequency = 12)

TCRE_imp <- ipeadata("GAC12_TCERMT12")
TCRE_imp_ts <- ts(TCRE_imp$value, start = c(1982,1), frequency = 12)


plot(TCN_ts, main = "Taxa de Câmbio Nominal")
plot(log(TCN_ts), main = "LOG da Taxa de Câmbio Nominal")
plot(TCR_BR_US_ts, main = "Taxa de Câmbio Real Bilateral BR-EUA")
plot(TCR_BR_ALE_ts, main = "Taxa de Câmbio Real Bilateral BR-ALE")
plot(TCR_BR_CHI_ts, main = "Taxa de Câmbio Real Bilateral BR-CHI")
plot(TCRE_imp_ts, main = "Taxa de Câmbio Real Efetiva das Importações")

### Parte 1 - Exploração dos Dados

Taxas <- cbind(TCN_ts,TCR_BR_ALE_ts,TCR_BR_US_ts,TCR_BR_CHI_ts,TCRE_imp_ts)
Taxas_2000 <- window(Taxas, start = c(2000,1))

plot(Taxas_2000)
plot(log(Taxas_2000))


#save(Taxas, file = "./Taxas_Cambio.Rdata")
#load("./Taxas_Cambio.Rdata")

library(highcharter)
library(htmltools)

hc0 = highchart(type = "stock") %>%
  hc_add_series(Taxas[,1], name = "Taxa de Câmbio Nominal - Real x Dólar") %>%
  hc_title(text = "Taxas de Câmbio Nominal - Real x Dólar", 
           margin = 10, style = list(fontSize= "14px")) %>%
  hc_subtitle(text = "Dados Mensais: 2000 a 2023")

hc1 = highchart(type = "stock") %>%
  hc_add_series(Taxas[,2], name = "Taxa de Câmbio Real Bilateral - Brasil x Alemanha") %>%
  hc_add_series(Taxas[,3], name = "Taxa de Câmbio Real Bilateral - Brasil x EUA") %>%
  hc_add_series(Taxas[,4], name = "Taxa de Câmbio Real Bilateral - Brasil x China") %>%
  hc_add_series(Taxas[,5], name = "Taxa de Câmbio Real Efetiva das Importações - Brasil") %>%
  hc_title(text = "Taxas de Câmbio Real Bilateral e Efetiva", 
           margin = 10, style = list(fontSize= "14px")) %>%
  hc_subtitle(text = "Dados Mensais: 2000 a 2023")

lst = list(hc0,hc1)

hw_grid(lst, ncol = 1, rowheight = 500)  %>% browsable()

### Parte 2 - Transformando os níveis de taxa de câmbio em taxa de variação mensal.

Ret_Taxas <- Taxas/stats::lag(Taxas,-1) -1
plot(Ret_Taxas)

Ret_Taxas_2000 <- window(Ret_Taxas, start = c(2000,1))
plot(Ret_Taxas_2000)


#install.packages(c("rugarch","dynlm"))   
library(rugarch)
library(dynlm)    
library(broom) 
library(FinTS) 
library(tseries)

Ret_Taxas_pos_2000 <- window(Ret_Taxas, start = c(2000,1))
rTS <- Ret_Taxas_pos_2000[,1]

# Teste para Efeitos ARCH
# Primeiro, estima-se a função dos retornos que é dada por um intercepto e o
# termo de erro.
camb.mean <- dynlm(rTS~1)
summary(camb.mean)

# Segundo, utilizando-se o res?duo da primeira equa??o, eleva-se ao quadrado criando
# uma nova s?rie e na sequ?ncia estima-se a depend?ncia dessa s?rie com rela??o as
# suas defasagens.
ehatsq <- resid(camb.mean)^2
camb.ARCH <- dynlm(ehatsq~L(ehatsq))
summary(camb.ARCH)

# Criando o teste LM na m?o...
T <- nobs(camb.mean)            # Extrai o número de observações de um modelo.
q <- length(coef(camb.ARCH))-1  # Graus de Liberdade.
Rsq <- glance(camb.ARCH)[[1]]   # Extrai o R^2 da Regress?o do ARCH.
LM <- (T-q)*Rsq                 # Cálculo da estat?stica LM.
LM
alpha <- 0.05                   # Utilizado para o percentual de confiança do teste.
Chicr <- qchisq(1-alpha, q)     # Valor Chi^2 cr?tico calculado.    
Chicr                           # Apresenta o valor que deve ser comparado.

# O resultado Chicr ? a estat?stica LM, igual a 14,94, a qual deve ser comparada com
# o valor chi^2 cr?tico com  ??=0.05 (95% de confian?a)  and  q=1 (graus de liberdade). 
# Esse valor ? ??2(0.95,1)=3.84; Isso indica que a hip?tese nula deve ser rejeitada, 
# concluindo que a s?rie sofre de efeitos do tipo ARCH.

# Teste autom?tico do efeito ARCH.
cambArchTest <- ArchTest(rTS, lags=1, demean=TRUE)
cambArchTest



# A função garch() do pacote tseries, pode ser utilizada para estimar um modelo ARCH,
# ao definir que a ordem é igual a c(0,1). A função então pode ser usada para estimar
# e plotar a variância h_t.
camb.arch <- garch(na.omit(rTS),c(0,1))
scambarch <- summary(camb.arch)
scambarch

# O plot
hhat <- ts(2*camb.arch$fitted.values[-1,1]^2, 
           start = c(2000,1), 
           frequency = 12) # Retira-se a primeira observa??o e usa-se
# a primeira coluna.
plot.ts(hhat)
lines(rTS^2, col = "blue")


highchart(type = "stock") %>%
  hc_add_series(hhat, name = "2x h_hat") %>%
  hc_add_series(rTS^2, name = "Quadrado da Taxa de C?mbio Nominal") %>%
  hc_title(text = "Volatilidade Cambial", 
           margin = 10, style = list(fontSize= "14px")) %>%
  hc_subtitle(text = "Dados Mensais: 2000 a 2023")


pac.camb  <- acf(rTS)
pacf.camb <- pacf(rTS)
dens.camb <- density(rTS)

plot(dens.camb, 
     main = "Densidade de Kernel para a Taxa de C?mbio Real BR-EUA Pós 2000",
     ylab = "Densidade")
