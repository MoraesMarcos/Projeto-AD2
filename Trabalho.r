## 1. Construam um gráfico temporal da série cronológica e descrevam as suas
# principais características. 

install.packages("forecast")
library(forecast)

data <- gas
show(data)

# Verificando estrutura da série temporal
str(data)

# Plotando gráfico temporal
plot(data, main="Consumo mensal de gás na Austrália", ylab="Gás (milhões de metros cúbicos)", xlab="Ano")
monthplot(gas, main="Variação sazonal mensal do consumo de gás", ylab="Gás (milhões de metros cúbicos)", xlab="Mês")


## Tendência: A série apresenta uma tendência ascendente ao longo do tempo, indicando um aumento no consumo de gás na Austrália durante o período analisado.
## Sazonalidade: Há uma forte componente sazonal, com padrões que se repetem anualmente, aumentando a cada ano, indicando a presença de uma sazonalidade multiplicativa.
## Variabilidade: A amplitude das variações sazonais parece aumentar ao longo do tempo, sugerindo que a variabilidade no consumo de gás se torna mais pronunciada nos anos mais recentes.

################################################################################

## 2. Decomponham a série nas diferentes componentes, ilustrando graficamente a
#decomposição efetuada. Comentem os resultados. 

# Decompor a série temporal usando decompose
data_decomp <- decompose(data, type="multiplicative")

# Plotar a decomposição com decompose
plot(data_decomp,"Decomposição da Série Temporal - Método decompose")

# A tendência crescente mostra o aumento geral no consumo de gás ao longo dos anos.
# A componente sazonal mostra um padrão que se repete anualmente.
# A componente residual captura as variações que não são explicadas pela tendência ou sazonalidade, incluindo ruídos. Podemos ver inicialmente um decrescimento do resíduos até pouco antes dos anos 80, quando ele volta a crescer e variar cada vez mais a cada ano.

#Aplicando Holt-Winters para sazonalidade mutiplicativa <<<< Método escolhido para esse projeto
holt_wintersM<-hw(gas,seasonal="multiplicative", h=24)
summary(holt_wintersM)
autoplot(holt_wintersM)
autoplot(forecast(holt_wintersM))

#Aplicando Holt-Winters para sazonalidade aditiva
holt_wintersA<-hw(gas,seasonal="additive", h=24)
summary(holt_wintersA)
autoplot(holt_wintersA)

#Aplicando método de Holt
holt_gas<-holt(gas,h=24)
summary(holt_gas)
autoplot(holt_gas)

#Aplicando MAE
AESimples_gas <- ses(gas, h=24)
summary(AESimples_gas)
autoplot(AESimples_gas)
