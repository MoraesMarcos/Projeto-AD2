
### Trabalho 2 A.D.
## 1. Construam um gráfico temporal da série cronológica e descrevam as suas
# principais características. 

install.packages("dplyr")
library(dplyr)

install.packages("forecast")
install.packages("lmtest")
library(forecast)
library(lmtest)

data <- gas
show(data)

# Verificando estrutura da série temporal
str(data)

# Plotando gráfico temporal
plot(data, main="Produção mensal de gás na Austrália", ylab="Gás (milhões de metros cúbicos)", xlab="Ano")
monthplot(gas, main="Variação sazonal mensal do produção de gás", ylab="Gás (milhões de metros cúbicos)", xlab="Mês")


## Tendência: A série apresenta uma tendência ascendente ao longo do tempo, indicando um aumento na produção de gás na Austrália durante o período analisado.
## Sazonalidade: Há uma forte componente sazonal, com padrões que se repetem anualmente, aumentando a cada ano, indicando a presença de uma sazonalidade multiplicativa.
## Variabilidade: A amplitude das variações sazonais parece aumentar ao longo do tempo, sugerindo que a variabilidade na  produção de gás se torna mais pronunciada nos anos mais recentes.

################################################################################

## 2. Decomponham a série nas diferentes componentes, ilustrando graficamente a
#decomposição efetuada. Comentem os resultados. 

# Decompor a série temporal usando decompose
data_decomp <- decompose(data, type="multiplicative")

# Plotar a decomposição com decompose
autoplot(data_decomp)

# A tendência crescente mostra o aumento geral na produção de gás ao longo dos anos.
# A componente sazonal mostra um padrão que se repete anualmente.
# A componente residual captura as variações que não são explicadas pela tendência ou sazonalidade, incluindo ruídos. Podemos ver inicialmente um decrescimento do resíduos até pouco antes dos anos 80, quando ele volta a crescer e variar cada vez mais a cada ano.

################################################################################

## 3. Aplicação dos métodos de previsão, cálculo dos erros, e gráfico otimizado

#Aplicando Holt-Winters para sazonalidade mutiplicativa <<<< Método escolhido para esse projeto
holt_wintersM<-hw(gas,seasonal="multiplicative", h=24)
summary(holt_wintersM)
autoplot(holt_wintersM)
autoplot(forecast(holt_wintersM))

plot(holt_wintersM, main="Previsão Holt-Winters Otimizada para Produção de Gás")
lines(fitted(holt_wintersM), col = "red")

#Aplicando Holt-Winters para sazonalidade aditiva
holt_wintersA<-hw(gas,seasonal="additive", h=24)
summary(holt_wintersA)
autoplot(holt_wintersA)

#Aplicando método de Holt
holt_gas<-holt(gas,h=24)
summary(holt_gas)
autoplot(holt_gas)

#Aplicando Método de Amortecimento Exponencial Simples
AESimples_gas <- ses(gas, h=24)
summary(AESimples_gas)
autoplot(AESimples_gas)

#Comparação dos erros entre os métodos

accuracy(holt_wintersM)
accuracy(holt_wintersA)
accuracy(holt_gas)
accuracy(AESimples_gas)

################################################################################

## 4. Análise dos resíduos
residuals <- residuals(holt_wintersM)
acf(residuals, main = "Função de Autocorrelação dos Resíduos")
qqnorm(residuals)
qqline(residuals, col="red")
shapiro.test(residuals)

checkresiduals(holt_wintersM)

################################################################################

#Regressão Linear

# Carregar o pacote forecast
library(forecast)

# Carregar a série temporal gas
data("gas")

# Criar um dataframe com a série gas e uma variável de tempo
gas_data <- data.frame(
  time = time(gas),
  gas = as.numeric(gas)
)

# Ajustar um modelo de regressão linear simples
model <- lm(gas ~ time, data = gas_data)

# Resumo do modelo
summary(model)

# Plotar a série temporal e a linha de regressão
plot(gas, main="Produção de Gás Natural na Austrália", ylab="Produção (milhões de metros cúbicos)", xlab="Tempo")
abline(model, col="red")


# Previsões para os próximos 24 meses
start_time <- max(gas_data$time)
time_intervals <- start_time + seq(0.083, 0.083 * 24, by = 0.083)

# Inicializar um dataframe para armazenar as previsões
forecast_results <- data.frame(
  time = time_intervals,
  fit = numeric(length(time_intervals)),
  lwr = numeric(length(time_intervals)),
  upr = numeric(length(time_intervals))
)

# Loop para gerar previsões com intervalos de confiança
for (i in 1:length(time_intervals)) {
  previsao <- predict(model,
                      newdata = data.frame(time = time_intervals[i]),
                      interval = "prediction",
                      level = 0.95)
  
  forecast_results$fit[i] <- previsao[1, "fit"]
  forecast_results$lwr[i] <- previsao[1, "lwr"]
  forecast_results$upr[i] <- previsao[1, "upr"]
}

# Mostrar resultados das previsões
print(forecast_results)

# Plotar a série temporal original e as previsões

lines(forecast_results$time, forecast_results$fit, col="blue", lwd=3)
lines(forecast_results$time, forecast_results$lwr, col="red", lty=2, lwd=2)
lines(forecast_results$time, forecast_results$upr, col="red", lty=2, lwd=2)
abline(model, col="green", lwd=2)

# Verificação dos pressupostos do Modelo
# Independencia
plot(residuals(model), col="red")
abline(h = 0, lty = 2, lwd = 1.5)

#Normalidade
shapiro_reg <- shapiro.test(residuals(model))
shapiro_reg

#Homocedasticidade
bptest(model)
