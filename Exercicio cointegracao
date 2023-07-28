library(readxl)
library(urca)
library(aTSA)
library(zoo)
library(ggplot2)

dados <- read_excel("C:\\Users\\nunom\\OneDrive\\Área de Trabalho\\Ciências Econômicas\\5º Período\\Econometria II\\Listas\\ITUB.xlsx")

# Convertendo a coluna de data para o tipo correto
dados$Data <- as.Date(dados$Data, "%Y-%m-%d")

### Análise gráfica de cointegração --------------------------------------------------
ggplot(dados, aes(x=Data)) + 
  geom_line(aes(y = itub3_sa_close), color = "darkred") + 
  geom_line(aes(y = itub4_sa_close), color="steelblue") +
  theme_light() +
  labs(x = "Data",
       y ="Preço") 

# Observando a série temporal do preço de fechamento das duas
# ações, elas parecem ser cointegradas.
### Testando cointegração -------------------------------------------------
# Teste ADF de estacionariedade dos resíduos
modelo <- lm(dados$itub4_sa_close ~ dados$itub3_sa_close)
adf.test(modelo$residuals)
# Os p-valores dos tipos de não-estacionariedade deram significativos: 
# rejeitamos a hipótese nula de que os resíduos são não-estacionário.
# Vamos assumir que os resíduos são estacionários, o que sugere cointegração.

# Teste de Engle-Granger
coint.test(dados$itub3_sa_close, dados$itub4_sa_close, d=0)

# Temos que o p-valor do tipo 1 foi <0.05. Assim, rejeitamos a hipótese
# nula e aceitamos a alternativa: as séries são cointegradas com lag 0.

### Implementando uma estratégia de trade com esses papéis ------------------------------
# A ideia será observar o spread entre ITUB4 e ITUB3 (ITUB4-ITUB3).
# Se o spread se afastar da média móvel, isso será um sinal para entrar
# comprado ou vendido em uma posição.
# Se o spread estiver 1 desvio padrão acima da média móvel de 30 dias,
# é um sinal para comprar ITUB3 e vender ITUB4; se estiver 1 DP abaixo, comprar 
# ITUB4 e vender ITUB3.
# A ideia é que a média móvel gira em torno de um valor ao longo do tempo e 
# essas flutuações temporárias voltam a essa média.

# Criando dados de média móvel e um dataframe com tamanho adequado para plotar
dados$spread <- dados$itub4_sa_close - dados$itub3_sa_close
dados$media_movel <- rollmean(dados$spread,k=30, fill = "na")
dados_MM <- dados[-(1:14),]
dados_MM <- dados_MM[-(4054:4040),]

# Plotando a série do spread e a média móvel
ggplot(dados_MM, aes(x=Data, group=1)) + 
  geom_line(aes(y = spread), color = "darkred") + 
  geom_line(aes(y = media_movel), color="steelblue") +
  labs(x = "Data",
       y ="Preço")

# Criando o data frame com os sinais de buy/sell
# OBS: 1 = long ITUB4, short ITUB3; -1 = short ITUB4, long ITUB3; 0 = hold
signals_1 <- ifelse(dados$spread[15:4053] > dados$media_movel[15:4053] + 0.25*sd(dados$media_movel[15:4053]),-1,0)
signals_2 <- ifelse(dados$spread[15:4053] < dados$media_movel[15:4053] - 0.25*sd(dados$media_movel[15:4053]),1,0)
signals <- signals_1 + signals_2

dados$signal <- signals
dados_MM$signal <- signals

# Gráfico para ver pontos de long ITUB4/short ITUB3 (verde) e long ITUB3/short ITUB4 (vermelho)
graf <- ggplot(dados_MM, aes(x=Data)) + 
  geom_line(aes(y = spread), color="steelblue") +
  geom_line(aes(y = media_movel), color="purple") +
  geom_point(aes(x = Data, y = spread, color = factor(signal))) +
  scale_color_manual(values = c("-1" = "red", "1" = "green", "0" = "NA")) +
  theme_light() +
  labs(x = "Data",
       y ="Spread (ITUB4-ITUB3)") 
graf

# Dando zoom no eixo x para visualizar melhor
grafico <- graf +
  scale_x_date(limits = c(as.Date("2018-03-01"), as.Date("2020-06-30")),
               date_labels = "%Y-%m-%d")
grafico

# Podemos notar nos gráficos que, com essa estratégia para gerar sinais 
# de buy/sell, é possível obter retorno, pois o long ou short de ITUB3 ou ITUB4
# está quase sempre sendo feito no momento adequado (i.e., antes do spread 
# voltar a cair ou a subir). Portanto, é sim possível fazer um estratégia de
# trade com a conclusão de que as séries são cointegradas.

