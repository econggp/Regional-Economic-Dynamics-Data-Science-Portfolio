library(quantmod)
library(TSstudio)
library(urca)
library(lmtest)
library(vars)
library(tseries) 
library(dynlm) 
library(ggplot2)
library(knitr) 
library(forecast) 
library(fBasics)
library(zoo)
library(PerformanceAnalytics)
library(aTSA)
library(changepoint)
library(patchwork)

getSymbols("CPALTT01MXM659N", src = "FRED")
chartSeries(CPALTT01MXM659N)
pe = ts(CPALTT01MXM659N, start = c(1969, 1), frequency = 12)
p1 <- autoplot(pe) + xlab("Año") +
  ggtitle("Precios al consumidor 1969-2024")
tcpe <- diff(log(pe))
p2 <- autoplot(tcpe) + xlab("Año") +
  ggtitle("Inflación 1969-2024")

plot(pe, type="l", main = "Precios al consumidor e Inflación 1969-2024", 
     col = "red", lwd = 2,lty = 1, ylab="Índice de precios")

par(new=TRUE)

plot(tcpe,type="l",axes=FALSE, ylab="",col="blue",lwd=2,lty=2)

axis(4)

ggtsdisplay(tcpe)
p3 <- ggseasonplot(tcpe,polar = TRUE,main='Inflación 1969-2024')
p4 <- ggseasonplot(tcpe,year.labels = TRUE,year.labels.left = TRUE)+ylab("Porcentaje")+ggtitle("Inflación 1969-2024")+xlab("Meses")

(p2+p4)/p1

decompose_tcp = decompose(tcp) 
autoplot(decompose_tcp) + xlab("Año") +
  ggtitle("Descomposición del comportamiento de la Inflación 1968-2024")


getSymbols("RGDPNAMXA666NRUG", src = "FRED")
chartSeries(RGDPNAMXA666NRUG)
autoplot(RGDPNAMXA666NRUG) + xlab("Año") +
  ggtitle("PIB a precios constantes 1950-2019")
pib = ts(RGDPNAMXA666NRUG, start = c(1950, 1), frequency = 1)
plot(pib)
tcp <- diff(log(pib))
autoplot(tcp) + xlab("Año") +
  ggtitle("Tasa de Crecimiento del PIB a precios constantes 1950-2019")
fit <- decompose(tcp, type='additive')
autoplot(fit)+
  labs(title = "Descomposición de la tasa de crecimiento del PIB",                   
       x = "Años",
       y = "tcPIB",
       colour = "Gears")+
  theme_bw()

autoplot(tcp, series="Tasa de crecimiento") + 
  autolayer(trendcycle(fit), series="Tendencia") +
  labs(title = "Tasa de crecimiento del PIB",      
       x = "Años",
       y = "tcPIB"
  ) + 
  theme_bw()

plot(pib, type="l", main = "PIB a precios constantes 1950-2019", 
     col = "red", lwd = 2,lty = 1, ylab="$")

par(new=TRUE)

plot(tcp,type="l",axes=FALSE, ylab="",col="blue",lwd=2,lty=2)

axis(4)



getSymbols("INTGSTMXM193N", src = "FRED")
chartSeries(INTGSTMXM193N)
t1 <- autoplot(INTGSTMXM193N) + xlab("Año") +
  ggtitle("Tasa de interés 1986-2024")
ti = ts(INTGSTMXM193N, start = c(1986, 1), frequency = 12)
plot(ti)
tcti <- diff(log(ti))
t2 <- autoplot(tcti) + xlab("Año") +
  ggtitle("Tasa de Crecimiento de la tasa de interés 1986-2024")
t1
t1/t2
getSymbols("CCUSMA02MXM618N", src = "FRED")
chartSeries(CCUSMA02MXM618N)
autoplot(CCUSMA02MXM618N) + xlab("Año") +
  ggtitle("Tipo de Cambio 1957-2024")
tic = ts(CCUSMA02MXM618N, start = c(1957, 1), frequency = 12)
plot(tic)
tctic <- diff(log(tic))
autoplot(tctic) + xlab("Año") +
  ggtitle("Tasa de Crecimiento del tipo de cambio 1957-2024")

getSymbols("LCEAMN01MXM659S", src = "FRED")
chartSeries(LCEAMN01MXM659S)
autoplot(LCEAMN01MXM659S) + xlab("Año") +
  ggtitle("Compensaciones Laborales 1981-2024")
cl = ts(LCEAMN01MXM659S, start = c(1981, 1), frequency = 12)
plot(cl)
tccl <- diff(log(cl))
autoplot(tccl) + xlab("Año") +
  ggtitle("Tasa de Crecimiento de las compensaciones laborales 1981-2024")
une_ma01 = rollmean(cl, k = 13, fill = NA)
plot(cl, main='Compensación Laboral', xlab='Mes/Año', ylab='Tasa')
lines(une_ma01, col="red", lwd=1)
grid()

summary(cl)
