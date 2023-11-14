# CONCEPTUAL ----
library(ISLR2)
## 1 ----
### Os p-values referente à TV e rádio - table 3.4 - são relevantes <0.0001
### Portanto, a hipotse nula, para ambos, pode ser rejeitada. Visto que, a tabela
### mostra forte verossimilhança entre comerciais de TV/radio e o aumento nas
### vendas.
### Por outro lado, o p-value de jornal é insignificante 0.8599. Dessa forma, deve-se
### adotar a hipótese nula para jornal. Pois, propaganda através desse veículo de 
### informação não impacta as vendas positivamente na presença dos demais.
## 2 ----
### As duas abordagens KNN-classifier e KNN - regression funcionam a partir da 
### mesma ideia básica: encontrar os vizinhos mais próximos na quantidade definida por K.
### A partir disso, o classifier irá atribuir o novo valor à uma classe que tenha
### a maior probabilidade de pertencer. E, o regression irá prever o valor a partir
### dos vizinhos mais próximos.
## 3 ----
### a ----
#### Y = b0 + b1.GPA + b2.IQ + b3.Level + b4.GPA:IQ + b5.GPA:Level
#### Opção para college (1): Y = 50 + 20.GPA + 0.07.IQ + 35.1 + 0.01.GPA:IQ + -10.GPA:1
####                         y = 85 + 20GPA + 0,07IQ + 0.01GPA:IQ + (-10GPA:1)
#### Opção   highschool (0): y = 50 + 20.GPA + 0.07IQ + 35.0 + 0.01.GPA:IQ + (-10.GPA:0)
####                         y = 50 + 20GPA + 0,07IQ + 0.01GPA:IQ
#### Diferença entre College salary e Hifgschool salary
####       85 + 20GPA + 0.07IQ + 0.01GPA.IQ - 10GPA
#### (-1) -50 - 20GPA - 0.07IQ - 0.01GPA.IQ
####      =35 - 10GPA
#### Se diferença >= 0
#### 35 - 10GPA >= 0 --> GPA <= 3,5
#### Se diferença <= 0
#### 35 - 10GPA <= 0 --> GPA >= 3,5
#### Depende do valor do GPA para obter a resposta
#### Portanto, o item iii é a resposta correta.
### b ----
#### 85 + 20*4 + 0,07*110 + 0.01*4*110 + (-10*4*1)
sallary <- 85 + 20*4 + 0.07*110 + 0.01*4*110 + (-10*4*1)
cat("Para uma pessoa que possua college, gpa=4 e iq=110 o salário será:",sallary,"mil dólares")
### c ----
#### Falso: apenas o coeficiente não pode ser usado como evidência de significância estatística
## 4 ----
### a ----
#### Aparentemente não há informação suficiente para responder essa questão. 
#### Porém, como a questão cita que a relação entre X e Y é linear, é possível
#### que a relação linear obtenha RSS menores do que a cúbica.
### b ----
#### Aparentemente não há informação suficiente para responder essa questão. 
#### Porém, é possível supor que a regressão polinomial implicará em maior RSS
#### devido ao fato de haver possibilidade de overfitting no treino e o erro
#### ser maior que o da regressão linear.
### c ----
#### Provavelmente o RSS para o modelo polinomial seja menor, devido sua maior
#### flexibilidade quando comparado com o modelo linear.
### d ----
#### Não é possível saber quão longe o modelo está da linearidade. Portanto, fica
#### a dúvida referente ao menor RSS. Visto que, se o modelo estiver mais próximo
#### da linearidade o menor RSS tende a ser o da regressão linear. Assim como, se
#### for o contrário é possível esperar melhor resultado do RSS para o modelo
#### polinomial.
## 5,6,7 ----
#### Não resolvi essas questões. Há um link com as soluções: https://rpubs.com/ppaquay/65559
## 8 ----
lm_mpg_horsepower <- lm(mpg ~ horsepower, data = Auto)
summary(lm_mpg_horsepower)
### a.i ----
#### O teste F estar distante do número 1, é evidência de que a h0 pode ser 
#### descartada em favor do modelo. Bem como o fato do p-value ser pratica-
#### mente 0, fortalece o abandono da h0 em favor do modelo. 
#### Portanto, com o valor do teste F distante de 1 e o p-value baixo, é pos-
#### sível concluir que há grande significância estatística entre MPG e HP.
### a.ii ----
#### A porcentagem de erro se dá pelo seguinte cálculo
mean(Auto$mpg)
rse_mpg_horsepower <- 4.906
pctgm_erro <- (rse_mpg_horsepower/mean(Auto$mpg))*100
#### Portanto, a porcentagem de erro do modelo em questão é de 20.92%. Ainda,
#### o R-squared vale 0.6059, isso mostra que 60.59% da variabilidade é expli-
#### cada pelo modelo.
### a.iii ----
#### Correlação negativa. Visto que a estimativa para horsepower é -0.157845
### a.iv ----
predict(lm_mpg_horsepower, data.frame(horsepower=c(98)), interval = 'confidence')
predict(lm_mpg_horsepower, data.frame(horsepower=c(98)), interval = 'prediction')
#### A previsão é um consumo de 24.46 mpg para horsepower = 98.
#### Intervalo de confiança (95%) de 23.97 até 24.96
#### Intervalo de previsão 14.80 até 34.12
### b ----

plot(Auto$horsepower, Auto$mpg)
abline(lm_mpg_horsepower, col = 'orange4')
### c ----
par(mfrow=c(2,2), mar = c(4, 4, 2, 2))
plot(lm_mpg_horsepower)
#### Baseado no formato de U do gráfico residuals vs fitted, é possível identi-
#### ficar algum tipo de não linearidade.
## 9 ----
### a ----
pairs(Auto)
### b ----
#### Investigar o uso de cor()
?cor
#### Limpar o ambiente de trabalho
rm(list = ls())
#### Carregar biblioteca
library(ISLR2)
#### Encontrar a posição da variável qualitativa 
colnames(Auto)
#### subset sem a variável name
subset_Auto <- Auto[,-9]
#### Função cor() sem a variável name(-9) 
cor(subset_Auto)
### c ----
lm_mpg_todos <- lm(mpg ~ . , data = subset_Auto)
summary(lm_mpg_todos)
###c.i ----
#### Visto que: F-statistic: 252.4 e p-value: < 2.2e-16. Então, é possível 
#### concluir que há significância estatística entre os preditores e a resposta
### c.ii ----
#### Os preditores com p-values mais baixos são: Weight, year, origin e displacement
### c.iii ----
#### Sugere correlação positiva com a resposta (MPG). A cada unidade de aumento em 
#### year o mpg também aumenta 0.75. Isso mostra que com o passar dos anos os 
#### carros passaram a percorrer mais distâncias com 1 gallon.
### d ----
par(mfrow=c(2,2))
plot(lm_mpg_todos)
#### No q-q gráfico é possível observar tendência de linearidade até o ponto 2.
#### Já no residuals vs fitted values é possível observar leve tendência de 
#### não linearidade.
par(mfrow=c(1,1))
plot(predict(lm_mpg_todos), rstudent(lm_mpg_todos))
#### Utilizando o gráfico studentized vs predict é possível verificar que alguns
#### pontos se afastam mais que os demais do valor 0, mostrando que é possível
#### que haja high leverage entre os preditores.
### e ----
#### Primeiro ajuste
lm_mpg_interaction_1 <- lm(mpg ~ year + horsepower*weight, data = subset_Auto)
summary(lm_mpg_interaction_1)
#### Segundo ajuste (melhor)
lm_mpg_interaction_2 <- lm(mpg ~ .
                           - displacement 
                           - acceleration 
                           + horsepower:weight 
                           + year:cylinders, 
                           data = subset_Auto)
summary(lm_mpg_interaction_2)
#### year:cylinders e horsepower:weight aparentam ser estatisticamente relevantes
### f ----
lm_mpg_interaction_3 <- lm(mpg ~ .
                           - displacement 
                           - acceleration 
                           + horsepower:(weight^2),
                           data = subset_Auto)
summary(lm_mpg_interaction_3)

lm_mpg_interaction_4 <- lm(mpg ~ log(weight) + horsepower*displacement, data = subset_Auto)
summary(lm_mpg_interaction_1)
#### Algumas interações foram realizadas. Porém, não foi encontrado um ajuste
#### melhor do que a interação 2
## 10 ----
#### limpar ambiente
rm(list = ls())
#### Carregar biblioteca
library(ISLR2)
?Carseats
colnames(Carseats)
#### a ----
lm_carseats <- lm(Sales ~ Price + Urban + US, Carseats)
summary(lm_carseats)
### b ----
#### Price: Correlação negativa com Sales, para $1000 a mais espera-se que sales
#### diminua em 54 unidades, se as demais se mantiverem iguais.
#### Urban: Essa variável não afeta as vendas. p-value 0.936
#### US: Espera-se que uma unidade no US venda 1.200 unidades a mais que uma fora
#### do US.
### c ----
#### Sales = 13.043469 -0.054459*Price + 1.200573*US
### d ----
#### Para esse modelo a variável Urban deve ser retirada. A hipótese nula deve
#### ser descartada em favor dos preditores Price e US.
### e ----
lm_carseats_2 <- lm(Sales ~ Price + US, Carseats)
summary(lm_carseats_2)
### f ----
summary(lm_carseats)
summary(lm_carseats_2)
#### Baseado no R-squared de cada modelo é possível perceber que ambos são irrelevantes
#### pois podem explicar apenas 23.93% da variação em vendas.
### g ----
confint(lm_carseats_2, level=0.95)
### h ----
par(mfrow=c(2,2), mar = c(4, 4, 2, 2))
plot(lm_carseats_2)
## 11 ----
set.seed(1)
x <- rnorm(100)
y <- 2*x+rnorm(100)
### a ----
lm_fit_11_a <- lm(y ~ x + 0)
#### coeficiente, std error, t-statistic and p-value:
summary(lm_fit_11_a)
#### O P-value sugere forte relação entre y e x através do modelo em questão
### b ----
lm_fit_11_b <- lm(x ~ y + 0)
#### coeficiente, std error, t-statistic and p-value:
summary(lm_fit_11_b)
#### O P-value sugere forte relação entre y e x através do modelo em questão
### c ----
#### É possível observar que os dois modelos possuem mesmo R-squared e F-statistic
#### Porém, o segundo modelo apresente erro padrão menor, isso sugere que possa
#### ser melhor se comparado com o outro. Ainda, o p-value para as duas opções
#### é próxima de 0, isso demonstra que em ambas situações existe forte relação
#### entre resposta e preditor.
### d ----
#### Link para uma solução: https://rpubs.com/lmorgan95/ISLR_CH3_Solutions
### e ----
#### Link para uma solução: https://rpubs.com/lmorgan95/ISLR_CH3_Solutions
### f ----
lm_fit_11_f_yx <- lm(y ~ x)
lm_fit_11_f_xy <- lm(x ~ y)
summary(lm_fit_11_f_yx)
summary(lm_fit_11_f_xy)
#### É possível obersar que o t-value é o mesmo nas duas ocasiões. Ainda,
#### há outras compatibilidades: p-value r-squared f-statistc
## 12 ----
### a ----
#### Link para uma solução: https://rpubs.com/ppaquay/65559
### b ----
set.seed(7)
x <- 1:100
y <- 2*rnorm(100)
lm_12_a_yx <- lm(y~x + 0)
lm_12_a_xy <- lm(x~y + 0)
summary(lm_12_a_yx)
summary(lm_12_a_xy)
### c ----
set.seed(77)
x2 <- 1:100
y2 <- 100:1
lm_12_b_y2x2 <- lm(y2~x2 + 0)
lm_12_b_x2y2 <- lm(x2~y2 + 0)
summary(lm_12_b_y2x2)
summary(lm_12_b_x2y2)
## 13 ----
### a ----
set.seed(1)
x <- rnorm(100)
### b ----
eps <- rnorm(100, sd = 0.5)
### c ----
y <- -1+0.5*x+eps
length(y)
#### b0 = -1; b1 = 0.5
### d ----
par(mfrow = c(1, 1))
plot(y~x)
#### Possível verificar correlação linear, mas com os pontos muito dispersos
### e ----
lm.fit <- lm(y~x)
summary(lm.fit)
#### Baseado no r-squared é possível afirmar que são estatisticamente relevantes, já que o modelo pode explicar
#### 77,84% da variação entre os dados. Assim como o p-value sugere forte relação.
#### f ----
abline(lm.fit, col = 'lightgreen')
legend("bottomright", c("regression line"),lwd = 1, col = "lightgreen")
