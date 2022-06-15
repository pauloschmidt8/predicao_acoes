# Bibliotecas 
library(quantmod)
library(prophet)
library(dplyr)

#Buscanndo Ação

# Informar Papel(Ação) a ser consultado
acao <- "MGLU3.SA"

# Período em que será analisado 
cotacao_historica <- '2021-12-01'

# Dias para previsão
dias_previsao <- 3

# Recebe a Acao(Papel) que será consultado no site do Yahoo Finance a data histórica até hoje
papel <- getSymbols(acao, src = 'yahoo', from = cotacao_historica, to = Sys.Date(), auto.assign = FALSE)

# criando o X1 e X2
df <- data.frame(matrix(ncol = 2, nrow = nrow(papel)))

# Nomeando as colunas do DF
names_colunas <-c('ds','y')
colnames(df) <- names_colunas

# importando a data histórica para o ds
df$ds <- as.Date(index(papel), "%y/%m/%d %H:%M")

# nomeando as colunas
colnames(papel) <- c("Abertura", "Máxima", "Mínima", "Fechamento", "Volume", "Ajustado")

df$y <- papel$Fechamento

# Criando o perído da predição
modelo <- prophet::prophet(df, daily.seasonality = TRUE)

futuro <- prophet::make_future_dataframe(modelo, periods = dias_previsao)

forecast <- stats::predict(modelo, futuro)

previsao <- tail(forecast[c('ds', 'yhat_lower', 'yhat_upper')], dias_previsao)

# concatenando todos os dados e deployando dentro do gráfico
prophet::dyplot.prophet(modelo, forecast)

