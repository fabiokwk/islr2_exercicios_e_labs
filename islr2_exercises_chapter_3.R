# CONCEPTUAL ----
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
par(mfrow=c(2,2))
plot(lm_mpg_horsepower)
#### Baseado no formato de U do gráfico residuals vs fitted, é possível identi-
#### ficar algum tipo de não linearidade.

