# Ex 9 ----
## importar dataset
auto <- read.table("C:/Users/fabio/Desktop/dsbd/statlearning_livro/auto.data", 
                   header = T, na.strings = "?",stringsAsFactors = T)
## omitir NA's
na.omit(auto)

## 9.a ----
# str = visão compacta da estrutura do dataset
# todas as variaveis exceto o nome são quantitativas
str(auto)

## 9.b ----
# range para uma variável quantitativa
range(auto$mpg)
# range para todas variaveis quantitativas
round(sapply(auto[, 1:8], range, na.rm = TRUE), 2)

## 9.c ----
# média para cada coluna
round(sapply(auto[, 1:8], mean, na.rm = TRUE), 2)
# Desvio padrão para cada coluna
round(sapply(auto[, 1:8], sd, na.rm = TRUE), 2)

## 9.d ----
## Subset sem os valores de 10 até 85
subset_auto <- auto[-c(10:85), c(1:8)]
# range para subset_auto cada coluna
round(sapply(subset_auto[, 1:8], range, na.rm = TRUE), 2)
# média para subset_auto cada coluna
round(sapply(subset_auto[, 1:8], mean, na.rm = TRUE), 2)
# Desvio padrão para subset_auto cada coluna
round(sapply(subset_auto[, 1:8], sd, na.rm = TRUE), 2)

## 9.e ----
# Investigar o dataset original
pairs(auto[,-c(3,7,9)])
# Plot dos itens analisados
par(mfrow = c(2, 2), mar = c(5,5,2,2))
# Plot HP X MPG
plot(auto$mpg, auto$horsepower, xlab = 'MPG', ylab = 'Horsepower', col = 'orchid')
## lm para HP X MPG
m_hp_mpg <- lm(auto$horsepower ~ auto$mpg)
abline(m_hp_mpg, col = 'blue', lwd = 2)
# Plot para Acceleration X HP
plot(auto$acceleration, auto$horsepower, xlab = 'Acceleration', ylab = 'Horsepower', col = 'skyblue')
## lm para Acceleration X HP
m_acc_hp <- lm(auto$horsepower ~ auto$acceleration)
abline(m_acc_hp, col = 'blue', lwd = 2)
# Plot para Weight X MPG
plot(auto$weight, auto$mpg, xlab = 'MPG', ylab = 'Weight', col = 'tomato')
## lm para Acceleration X HP
m_wght_mpg <- lm(auto$mpg ~ auto$weight)
abline(m_wght_mpg, col = 'blue', lwd = 2)
# Plot para Origin X HP
plot(auto$origin, auto$horsepower, xlab = 'Origin', ylab = 'Horsepower', col = 'limegreen')
## lm para Origin X HP
m_ogn_hp <- lm(auto$horsepower ~ auto$origin)
abline(m_ogn_hp, col = 'blue', lwd = 2)

## 9.f ----
# Bem evidente o aumento de consumo relacionado ao aumento do HP e do peso.
par(mfrow=c(1,2))
plot(auto$mpg, auto$horsepower, xlab = 'MPG', ylab = 'Horsepower', col = 'orchid')
plot(auto$mpg, auto$weight, xlab = 'MPG', ylab = 'Weight', col = 'tomato')

# Ex 10 ----
## 10.a ----
# Instalar/Ler a biblioteca ISLR2
install.packages("ISLR2")
library(ISLR2)
# Infos sobre o pacote Boston
?Boston
# Qtd de linhas, colunas
str(Boston)
## 10.b ----
# Plot dos itens analisados
par(mfrow = c(2, 2), mar = c(5,5,2,2))
# Rm x Medv
plot(Boston$rm, Boston$age, xlab = 'Avarege rooms per dwelling', ylab = ' owner-occupied units', col = 'blueviolet')
# Tax X Medv
plot(Boston$tax, Boston$rad, xlab = 'Property-tax rate per $10.000', ylab = 'accessibility to radial highways', col = 'gray33')
# Zn X Crim
plot(Boston$zn, Boston$crim, xlab = 'Lots over 25.000 sq.ft', ylab = 'Per capita crime',  col = 'cadetblue')
# Zn X Crim
plot(Boston$dis, Boston$ptratio, xlab = 'distances to five Boston employment centres.', ylab = 'pupil-teacher ratio', col = 'burlywood3')

## 10.c ----
## Preditores associados à taxa de crime
par(mfrow=c(1,2))
## lstats x crim
plot(Boston$crim, Boston$lstat, xlab = 'Per capita crime', ylab = 'lower status of the population', col = 'burlywood3')
## medv x crim
plot(Boston$medv, Boston$crim, xlab = 'Median Value', ylab = 'Per capita crime', col = 'chocolate4')

## 10.d ----
par(mfrow = c(1,1))
# Crime rates e frequencia das maiores taxas
hist(Boston$crim, breaks = 20)
nrow(Boston[Boston$crim > 20, ])
# Tax rates
hist(Boston$tax, breaks = 20)
##encontrar maior frequência para tax rates
sort(table(Boston$tax), decreasing = TRUE)
#Ptratio
hist(Boston$ptratio, breaks = 20)
##encontrar maior frequência para ptratio
sort(table(Boston$ptratio), decreasing = TRUE)

## 10.e ----
# Verificar coluna chas: 1 para trechos ligados ao rio, 0 para o contrário
Boston$chas
## verificar qtd de trechos ligados ao rio
nrow(Boston[Boston$chas == 1, ])
## 10.f ----
# Media da coluna ptratio
median(Boston$ptratio)

## 10.g ----
# Suburbios com menores medv
which(Boston$medv == min(Boston$medv))
# Demais preditores para o suburbio 399
Boston[399,]
# média geral das colunas crim, ptratio, lstat comparadas com o suburbio 399
round(mean(Boston$crim),2) > Boston[399, 1]
round(mean(Boston$ptratio),2) > Boston[399, 11]
round(mean(Boston$lstat),2) > Boston[399, 12]

## 10.h ----
# qtd de imóveis com mais de 7 quartos
sum(Boston$rm > 7)
# > 8
sum(Boston$rm > 8)
# Comentários sobre imóveis com mais de 8 quartos
summary(subset(Boston, rm > 8))
subset_8q <- subset(Boston, rm > 8)

round(mean(Boston$crim),2) > round(mean(subset_8q$crim),2)
round(mean(Boston$crim),2) > round(mean(subset_8q$ptratio),2)
round(mean(Boston$crim),2) > round(mean(subset_8q$lstat),2)
