### Carregando bibliotecas e dados---------------------------------------------------------
{library(ggplot2)
library(dynlm)
library(sjPlot)
library(forecast)
}
load("C:/Users/nunom/OneDrive/Área de Trabalho/Ciências Econômicas/5º Período/Econometria II/Listas/Lista_computacional_ARMA.RData")

### Questão 1 ---------------------------------------------------------------------
### a) Análise gráfica das séries ------------------------------------------------
# Inicialmente vamos plotar as séries
autoplot(S1) +
  xlab("Time") +
  geom_line(color="#69b3a2")

autoplot(S2) +
  xlab("Time") +
  geom_line(color="#69b3a2")

# ambos os gráficos parecem indicar a não-estacionariedade das séries; a média
# e a variância não permanecem constantes ao longo do tempo. Assim, vamos plotar
# a 1a diferença das séries e observar se elas se tornam estacionárias.

difS1 <- diff(S1, 1, 1)
difS2 <- diff(S2, 1, 1)

autoplot(difS1) +
  xlab("Time") +
  geom_line(color="#69b3a2")

autoplot(difS2) +
  xlab("Time") +
  geom_line(color="#69b3a2")

# Em ambos os casos, as séries parecem se estacionarizar. Embora a variância nem
# sempre parecer constante, as séries parecem flutuar em torno de uma média ao 
# longo do tempo.
# Agora, analisaremos a FAC e a FACP das séries estacionarizadas.

acf(difS1, main = "FAC 1a diferença S1")
pacf(difS1, main = "FACP 1a diferença S1")
acf(difS2, main = "FAC 1a diferença S2")
pacf(difS2, main = "FACP 1a diferença S2")

# Em relação a S1, a FAC apresenta um padrão declinante,o que indica presença
# de variável autoregressiva. A FACP, por outro lado, não apresenta um padrão 
# tão claro.Embora ela decaia rapidamente entre os lags 1 e 2, o que pode
# parecer como um padrão truncado em 1, indicando MA(q), q=0 e AR(p), p=1, há 
# spikes nos lags 9 e 12. Além disso, a autocorrelação parcial nunca vai para 0.
# Isso sinaliza um padrão declinante e presença de variável de média móvel.

# Em relação a S1, sua FAC também é declinante, sinalizando elemento 
# autoregressivo. Sua FACP segue um padrão semelhante à da S1, porém, com
# mais spikes em lags maiores. A FACP parece seguir um padão declinante
#oscilatório. Isso indica presença de variável de média móvel.

### b) Identificando modelos candidatos -------------------------------------------------
# Devido ao padrão aparentemente declinante das séries, é difícil inferir
# os parâmetros p,q de um ARMA(p,q). Assim, iremos testar modelos e comparar
# quais possuem o melhor ajuste.
# Modelos candidatos S2: p = (1,2), q= (1,2)
# Modelos candidatos S2: p = (2,3), q= (1,2)

### c) Estimando os modelos e verificando se possuem resíduos ruído branco --------------------------
m1s1 <- arima(S1, order = c(1,1,1))
m2s1 <- arima(S1, order = c(2,1,1))
m3s1 <- arima(S1, order = c(1,1,2))
m4s1 <- arima(S1, order = c(2,1,2))

par(mfrow=c(2,2))
{plot(residuals(m1s1))
  plot(residuals(m2s1))
  plot(residuals(m3s1))
  plot(residuals(m4s1))}

{acf(residuals(m1s1))
  acf(residuals(m2s1))
  acf(residuals(m3s1))
  acf(residuals(m4s1))}

# Analisando os resíduos dos modelos estimados, todos aparentam
# ser white noise. Ainda assim, as FACs possuem alguns spikes significantes,
# mas no máximo dois. Isso pode ser consequência da amostra.
# Não iremos descartar nenhum modelo por enquanto.

m1s2 <- arima(S2, order = c(2,1,1))
m2s2 <- arima(S2, order = c(2,1,2))
m3s2 <- arima(S2, order = c(3,1,1))
m4s2 <- arima(S2, order = c(3,1,2))

{plot(residuals(m1s2))
  plot(residuals(m2s2))
  plot(residuals(m3s2))
  plot(residuals(m4s2))}

{acf(residuals(m1s2))
  acf(residuals(m2s2))
  acf(residuals(m3s2))
  acf(residuals(m4s2))}

# Igualmente, todos os resíduos parecem ser white noise analisando os plots
# e as FACs. Embora haja alguns spikes significativos, são poucos.
# Não será descartado nenhum modelo.

### d) Escolhendo o modelo final ----------------------------------------------------
modelos1 <- c("M1S1", "M2S1", "M3S1", "M4S1")
AICs1 <- c(AIC(m1s1), AIC(m2s1), AIC(m3s1), AIC(m4s1))
BICs1 <- c(BIC(m1s1), BIC(m2s1), BIC(m3s1), BIC(m4s1))
df_s1 <- data.frame(modelos1, AICs1, BICs1)
# O modelo com menor AIC e menor BIC, indicando melhor ajuste, é
# o modelo 2: ARIMA(2,1,1). Ele será o modelo escolhido para modelar S1.

modelos2 <- c("M1S2", "M2S2", "M3S2", "M4S2")
AICs2 <- c(AIC(m1s2), AIC(m2s2), AIC(m3s2), AIC(m4s2))
BICs2 <- c(BIC(m1s2), BIC(m2s2), BIC(m3s2), BIC(m4s2))
df_s2 <- data.frame(modelos2, AICs2, BICs2)
# O modelo escolhido será o modelo 1, ARIMA(2,1,1), pois ele
# possui os menores AIC e BIC.

# Assim, para modelar ambas as séries, escolhemos o modelo ARIMA(2,1,1).

### e) Sugestão da função auto.arima ------------------------------------------------
auto.arima(S1) # ARIMA(2,1,1)
auto.arima(S2) # ARIMA(3,1,2)

# Para S1, a função sugere o mesmo modelo sugerido na análise feita aqui.
# Na S2, por outro lado, ela sugere o modelo ARIMA(3,1,2), que estava
# entre os modelos candidatos como modelo 4. Porém, devido ao AIC e BIC,
# escolhemos o modelo ARIMA(2,1,1).
# Em ambos os casos, o auto.arima identificou a presença de não-estacionariedade
# de primeira ordem.
