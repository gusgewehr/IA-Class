base = read.csv('credit_data.csv')

base$clientid = NULL

media = mean(base$age[base$age>0], na.rm = TRUE)

base$age = ifelse(base$age<0, media, base$age)


base[,1:3]=scale(base[,1:3])

base$default = factor(base$default, levels = c(0,1))

#remover os NAs
base[is.na(base$age),]

#base[is.na(base$income),]

#base[is.na(base$loan),]

base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)


library(caTools)

set.seed(1)
divisao = sample.split(base$default,SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base,divisao == FALSE )


#install.packages('e1071')

library(e1071)

?svm

classificador = svm(formula = default ~., 
                    data = base_treinamento, 
                    type = 'C-classification', 
                    kernel = 'radial', 
                    cost = 5)


previsoes = predict(classificador, newdata = base_teste[-4])



matriz_confusao = table(base_teste[,4], previsoes)
print(matriz_confusao)


library('caret')

confusionMatrix(matriz_confusao)


