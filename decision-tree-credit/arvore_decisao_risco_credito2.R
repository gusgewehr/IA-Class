base = read.csv('credit_data.csv')

base$clientid = NULL

media = mean(base$age[base$age>0], na.rm = TRUE)

base$age = ifelse(base$age<0, media, base$age)


base[,1:3]=scale(base[,1:3])


library(caTools)

set.seed(1)
divisao = sample.split(base$default,SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base,divisao == FALSE )


#install.packages('rpart')
library(rpart)

?rpart

base$default = factor(base$default, levels = c(0,1))

classificador = rpart(formula= default ~. , data = base_treinamento)

print(classificador)
plot(classificador)
text(classificador)

#install.packages('rpart.plot')

library(rpart.plot)


rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')


matriz_confusao = table(base_teste[,4], previsoes)
library(caret)

confusion_matrix(matriz_confusao)
