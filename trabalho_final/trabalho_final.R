library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(caTools)
library(caret)

d1 = read.csv('student-mat.csv', sep=';', header = TRUE)
d2 = read.csv('student-por.csv', sep=';', header = TRUE)
#queremos prever o G3

d1 = data.frame(d1)
d2= data.frame(d2)

base_dados = full_join(d1,d2)

base_factor = base_dados

#base_dados %>%
#  group_by(G3) %>%
#  count()


base_factor <- base_factor %>%
  mutate(G3 = factor(G3))

base_factor[is.na(base_factor$G3)]

#como ela é numérica precisamos transformar em classes
levels(base_factor$G3)[levels(base_factor$G3)=="0"] <- "D"
levels(base_factor$G3)[levels(base_factor$G3)=="1"] <- "D"
levels(base_factor$G3)[levels(base_factor$G3)=="2"] <- "D"
levels(base_factor$G3)[levels(base_factor$G3)=="3"] <- "D"
levels(base_factor$G3)[levels(base_factor$G3)=="4"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="5"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="6"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="7"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="8"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="9"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="10"] <- "C"
levels(base_factor$G3)[levels(base_factor$G3)=="11"] <- "B"
levels(base_factor$G3)[levels(base_factor$G3)=="12"] <- "B"
levels(base_factor$G3)[levels(base_factor$G3)=="13"] <- "B"
levels(base_factor$G3)[levels(base_factor$G3)=="14"] <- "B"
levels(base_factor$G3)[levels(base_factor$G3)=="15"] <- "B"
levels(base_factor$G3)[levels(base_factor$G3)=="16"] <- "A"
levels(base_factor$G3)[levels(base_factor$G3)=="17"] <- "A"
levels(base_factor$G3)[levels(base_factor$G3)=="18"] <- "A"
levels(base_factor$G3)[levels(base_factor$G3)=="19"] <- "A"
levels(base_factor$G3)[levels(base_factor$G3)=="20"] <- "A"

#visualiza os novos valores
#view(summary(base_dados$G3))
#levels(base_dados$G3)

#write_csv(base_dados, "base_dados.csv")


base_factor$school = factor(base_factor$school, levels = c("GP", "MS"), labels = c(0, 1))
base_factor$sex = factor(base_factor$sex, levels = c("F", "M"), labels = c(0, 1))
base_factor$address = factor(base_factor$address, levels = c("U", "R"), labels = c(0, 1))
base_factor$famsize = factor(base_factor$famsize, levels = c("GT3", "LE3"), labels = c(0, 1))
base_factor$Pstatus = factor(base_factor$Pstatus, levels = c("A", "T"), labels = c(0, 1))
base_factor$Mjob = factor(base_factor$Mjob, levels = c("at_home", "health",  "other", "services", "teacher" ), labels = c(0,1,2,3,4))
base_factor$Fjob = factor(base_factor$Fjob, levels = c("teacher", "other", "services", "health", "at_home"), labels = c(0,1,2,3,4))
base_factor$reason = factor(base_factor$reason, levels = c("course","other","home", "reputation"), labels = c(0,1,2,3))
base_factor$guardian = factor(base_factor$guardian, levels = c("mother", "father", "other"), labels = c(0,1,2))
base_factor$schoolsup = factor(base_factor$schoolsup, levels = c("no", "yes"), labels = c(0,1))
base_factor$famsup = factor(base_factor$famsup, levels = c("no", "yes"), labels = c(0,1))
base_factor$paid = factor(base_factor$paid, levels = c("no", "yes"), labels = c(0,1))
base_factor$activities = factor(base_factor$activities, levels = c("no", "yes"), labels = c(0,1))
base_factor$nursery = factor(base_factor$nursery, levels = c("no", "yes"), labels = c(0,1))
base_factor$higher = factor(base_factor$higher, levels = c("no", "yes"), labels = c(0,1))
base_factor$internet = factor(base_factor$internet, levels = c("no", "yes"), labels = c(0,1))
base_factor$romantic = factor(base_factor$romantic, levels = c("no", "yes"), labels = c(0,1))
base_factor$G3 = factor(base_factor$G3, levels = c("D","C","B","A"), labels = c(0,1,2,3))


base_scale = base_factor

base_scale[,3]=as.numeric(scale(base_scale[,3]))
base_scale[,7:8]=as.numeric(scale(base_scale[,7:8]))
base_scale[,13:15]=as.numeric(scale(base_scale[,13:15]))
base_scale[,24:32]=as.numeric(scale(base_scale[,24:32]))


set.seed(1)

divisao = sample.split(base_dados$G3,SplitRatio = 0.75)
base_treinamento = subset(base_dados, divisao == TRUE)
base_teste = subset(base_dados,divisao == FALSE )

divisao_factor = sample.split(base_factor$G3,SplitRatio = 0.75)
base_factor_treinamento = subset(base_factor, divisao_factor == TRUE)
base_factor_teste = subset(base_factor,divisao_factor == FALSE )

divisao_scale = sample.split(base_scale$G3,SplitRatio = 0.75)
base_scale_treinamento = subset(base_scale, divisao_scale == TRUE)
base_scale_teste = subset(base_scale,divisao_scale == FALSE )




##---------------------------- NAIVE BAYES ----------------------------##
library(e1071)
classificador = naiveBayes(x=base_scale_treinamento[-33], y=base_scale_treinamento$G3)
previsoes = predict(classificador, newdata = base_scale_teste[-33])

matriz_confusao = table(base_scale_teste[ , 33], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)

##---------------------------- DECISION TREE ----------------------------##
library(rpart.plot)
classificador = rpart(formula= G3 ~. , data = base_scale_treinamento)
previsoes = predict(classificador, newdata = base_scale_teste[-33], type = 'class')

matriz_confusao = table(base_scale_teste[ , 33], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)

##---------------------------- RANDOM FOREST ----------------------------##
library(randomForest)
classificador = randomForest(x = base_scale_treinamento[-33],
                             y=base_scale_treinamento$G3, 
                             ntree = 40)
previsoes = predict(classificador, newdata = base_scale_teste[-33])

matriz_confusao = table(base_scale_teste[,33], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)
##---------------------------- KNN ----------------------------##
library('class')

previsoes = knn(train = base_scale_treinamento[, -33],test = base_scale_teste[, -33],cl = base_scale_treinamento[, 33],k = 5)

print(previsoes)

matriz_confusao = table(base_scale_teste[, 33], previsoes)

print(matriz_confusao)

confusionMatrix(matriz_confusao)

##---------------------------- REGRESSÃO LOGÍSTICA ----------------------------##

base_regress = base_scale

levels(base_regress$G3)[levels(base_regress$G3) < 1] <- 0
levels(base_regress$G3)[levels(base_regress$G3) >= 2] <- 1

divisao_regress = sample.split(base_regress$G3,SplitRatio = 0.75)
base_regress_treinamento = subset(base_regress, divisao_regress == TRUE)
base_regress_teste = subset(base_regress,divisao_regress == FALSE )


classificador = glm(formula = G3 ~., 
                    family = binomial,
                    data = base_regress_treinamento)

probabilidades = predict(classificador,
                         type = 'response',
                         newdata = base_regress_teste[ , -33])

previsoes = ifelse(probabilidades>0.5, 1, 0)
print(previsoes)

matriz_confusao = table(base_regress_teste[ , 33],
                        previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)

##---------------------------- SVM ----------------------------##
library(e1071)
classificador = svm(formula = G3 ~., 
                    data = base_scale_treinamento, 
                    type='C-classification',
                    kernel = 'radial',
                    cost = 5
)
previsoes = predict(classificador, newdata = base_scale_teste[-33])

matriz_confusao = table(base_scale_teste[,33], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)

##---------------------------- RNAM ----------------------------##
library(h2o)
h2o.init(nthreads = -1)
classificador = h2o.deeplearning(y = 'G3',
                                 training_frame = as.h2o(base_scale_treinamento),
                                 hidden = c(500, 500),
                                 epochs = 1000)
previsoes = h2o.predict(classificador, newdata = as.h2o(base_scale_teste[-33]))
print(previsoes)

previsoes = previsoes$predict
print(previsoes)
previsoes = as.vector(previsoes)
print(previsoes)

matriz_confusao = table(base_scale_teste[,33], previsoes)

print(matriz_confusao)

confusionMatrix(matriz_confusao)

