{ library(stargazer)
  library(car)
  library(rgl)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(lmtest)
  library(sandwich)
  library(ggplot2)
  library(hrbrthemes)
  }

# Importando as bases de dados e utilizando merge() para agregar
{
  setwd('C:\\Users\\nunom\\OneDrive\\Área de Trabalho\\Ciências Econômicas\\4º Período\\Econometria I\\Trabalho')
  df_es <- read.csv('Tabela completa ES.csv', sep=';', header=TRUE, dec=',')
  df_es_inst <- read.csv('InstBraJEH ES.txt', sep=',', header=TRUE, dec='.')
  df_completo <- merge(df_es, df_es_inst, by="municipio", all.x = TRUE, all.y = FALSE)
}

# Criando novas colunas
{
  df_completo$lPIB_per_capita <- log(df_completo$PIB_per_capita) # Coluna com o log do PIB per capita dos municípios
  df_completo$escolaridade <- (df_completo$Superior_Incompleto +
                                 df_completo$Superior_Completo)/df_completo$Total # Variável para medir o nível educacional do município (trabalhadores com pelo menos médio completo) 
  df_completo$saneamento <- (df_completo$Rede.geral.de.esgoto.ou.pluvial...não.discriminado + df_completo$Fossa.séptica...não.discriminada)/df_completo$Total.sanit
  df_completo$dist_km <- df_completo$HubDist*111.320 # Distância em km da cidade litorânea mais próxima
  # Convertendo para numérico as colunas de temperatura
  df_completo$t1 <- as.numeric(df_completo$t1)
  df_completo$t2 <- as.numeric(df_completo$t2)
  df_completo$t3 <- as.numeric(df_completo$t3)
  df_completo$t4 <- as.numeric(df_completo$t4)
  df_completo$t5 <- as.numeric(df_completo$t5)
  df_completo$t6 <- as.numeric(df_completo$t6)
  df_completo$t7 <- as.numeric(df_completo$t7)
  df_completo$t8 <- as.numeric(df_completo$t8)
  df_completo$t9 <- as.numeric(df_completo$t9)
  df_completo$t10 <- as.numeric(df_completo$t10)
  df_completo$t11 <- as.numeric(df_completo$t11)
  df_completo$t12 <- as.numeric(df_completo$t12)
  
  df_completo <- df_completo[-c(27,13),] # removendo os municípios de presidente kennedy e boa esperanca (falta de dados gerando problemas nos testes)
  }

################################
# Rodando modelos
lm.1 <- lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
           + saneamento + sarg + scambi + slat + t1 + t2 + t3 + t4 + t5 + t6 
           + t7 + t8 + t9 + t10 + t11 + t12, data = df_completo)
summary(lm.1)
# Testando existência de heteroscedasticidade
usq_lm.1 <- (summary(lm.1)$residual)^2
lm_usq_lm.1 <- lm(usq_lm.1 <- lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
                  + saneamento + sarg + scambi + slat
                  + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12, data = df_completo)
summary(lm_usq_lm.1) # Estatística F de 21.75; rejeitamos a hipótese nula de homoscedasticidade
# Correção de heteroscedasticidade
coeftest(lm.1, vcov=vcovHC(lm.1, type = "HC0"))
# Criando tabela, com desvios padrões robustos
tab_model(lm.1, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.1, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))

# Teste F robusto para os controles de tipo de solo
lm.1.r <- lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + 
               dist_km + saneamento, data = df_completo)
waldtest(lm.1, lm.1.r, vcov = vcovHC(lm.1, type = "HC0"))
# p-valor de 0.000149, portanto, significante; vamos utilizar o modelo irrestrito 
tab_model(lm.1.r, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.1.r, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))

# Testando o modelo sem outliers de royalties
boxplot(df_completo$Royalties_pc)$out # Identificando os outliers
df_sem_ol <- df_completo[df_completo$Royalties_pc<40000,] # Criando o data frame sem os outliers
# Rodando o modelo
lm.2 <- lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
           + saneamento + sarg + scambi + slat
           + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12, data = df_sem_ol)
summary(lm.2)
coeftest(lm.2, vcov=vcovHC(lm.2, type = "HC0"))
tab_model(lm.2, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.2, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))
# Teste F robusto para os controles de tipo de solo
lm.2.r <- lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
             + saneamento, data = df_sem_ol)
waldtest(lm.2, lm.2.r, vcov = vcovHC(lm.2, type = "HC0"))
tab_model(lm.2.r, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.2.r, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))
# p-valor de 7.543e-05; os controles geográfico são significativos; vamos usar o modelo irrestrito


# Testando o modelo sem municípios litorâneos
df_interior <- df_completo[df_completo$dist_km != 0,] # Criando o data frame sem municípios com praia
lm.3 <- lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
           + saneamento + sarg + scambi + slat
           + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12,
           data = df_interior)
summary(lm.3)
coeftest(lm.3, vcov=vcovHC(lm.3, type = "HC0"))
tab_model(lm.3, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.3, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))
# Teste F robusto para os controles geográficos
lm.3.r <-lm(lPIB_per_capita ~ escolaridade + partic_ind + Royalties_pc + dist_km
            + saneamento, data = df_interior)
waldtest(lm.3, lm.3.r, vcov = vcovHC(lm.3, type = "HC0"))
tab_model(lm.3.r, p.style = 'stars', show.ci = FALSE, show.se = TRUE, digits = 4, 
          digits.p = 4, vcov.fun = vcovHC(lm.3.r, type = "HC0"),
          p.threshold = c(0.1, 0.05, 0.01))
# p-valor de 4.166e-06; controles de solos são significativos; vamos usar o modelo irrestrito


#############
# Construindo visuais
# construindo tabela para apresentar o modelo principal e os dois testes de robustez
stargazer(lm.1, lm.2, lm.3, type = 'html', 
          intercept.top = TRUE, intercept.bottom = FALSE, digits = 4,
          keep = c('escolaridade', 'partic_ind', 'Royalties_pc', 'dist_km', 'saneamento'),
          add.lines = list(c('controles geográficos', 'Sim', 'Sim', 'Sim')),
          out = "C:\\Users\\nunom\\Downloads\\stargaze")

# Criando gráficos de regressões parciais
avPlots(lm.1, pch=16, lwd=3, layout= c(1, 1), id = FALSE)

# Criando gráfico de correlação entre escolaridade e PIB pc para cidades do interior
ggplot(data = df_interior, aes(escolaridade, PIB_per_capita)) +
  geom_point() +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

ggplot(data = df_interior, aes(saneamento, PIB_per_capita)) +
  geom_point() +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

ggplot(data = df_sem_ol, aes(saneamento, PIB_per_capita)) +
  geom_point() +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

# Scatter plot para royalties e PIB
ggplot(data = df_completo, aes(Royalties_pc, PIB_per_capita)) +
  geom_point() +
  theme_ipsum()

# Criando gráficos de densidade para o PIB per capita com e sem outliers de royalties
ggplot(df_completo, aes(x = PIB_per_capita)) +
  xlab("PIB per capita") +
  geom_density(fill="#4adede", color="#4adede", alpha=0.8) +
  theme_ipsum()

ggplot(df_sem_ol, aes(x = PIB_per_capita)) +
  xlab("PIB per capita") +
  geom_density(fill="#4adede", color="#4adede", alpha=0.8) +
  theme_ipsum()

ggplot(df_interior, aes(x = PIB_per_capita)) +
  xlab("PIB per capita") +
  geom_density(fill="#4adede", color="#4adede", alpha=0.8) +
  theme_ipsum()

