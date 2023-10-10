# Carregar bibliotecas ----
library(MASS)
library(ISLR2)
# LINEAR REGRESSION ----
## Help para dataset ----
?Boston
### head Boston
head(Boston)
## regressão linear simples ----
lm.fit <- lm(medv ~ lstat, data = Boston)
## summary da regressão ----
summary(lm.fit)
## names da regressão ----
names(lm.fit)
## coeficientes lm.fit ----
coef(lm.fit)
## Intervalo de confiança ----
confint(lm.fit)
## Intervalos de previsão ----
predict(lm.fit,data.frame(lstat = (c(5,10,15))), interval = 'confidence')
## Plot de medv e lstat junto com a linha de tendência ----
plot(Boston$lstat, Boston$medv, col = 'navyblue')
abline(lm.fit, col = 'tomato')
## gráficos de diagnóstico ----
### pode ser feito sem a função par, nesse caso será feito 1 por vez a cada 'enter'
par(mfrow = c(2,2))
plot(lm.fit)
## residuos X ajuste alternativo ----
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
## levarege statistics ----
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# MULTIPLE LINEAR REGRESSION ----
## rlm: 2 preditores ----
m.lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(m.lm.fit)
## rlm: todos preditores ----
m.lm.fit <- lm(medv ~ ., data = Boston)
summary(m.lm.fit)
## help summary.lm  ----
?summary.lm
### R-squared para m.lm.fit
summary(m.lm.fit)$r.sq
### sigma para m.lm.fit
summary(m.lm.fit)$sigma
## VIF: variance inflation afctors ----
###para utilizar a função vif(), o pacote car deve ser instalado
install.packages('car')
### carregar a biblioteca car
library(car)
### função vif()
vif(m.lm.fit)
## rlm: todos preditores - 1 ----
m.lm.fit <- lm(medv ~ . -age, data = Boston)
summary(m.lm.fit)
