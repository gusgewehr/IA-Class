library(stringr)
#install.packages('tidyverse')
library(tidyverse)
library(lubridate)
library(dplyr)

d1 = read.csv('student-mat.csv', sep=';', header = TRUE)
d2 = read.csv('student-por.csv', sep=';', header = TRUE)
#queremos prever o G3

d1 = data.frame(d1)
d2= data.frame(d2)

base_dados = full_join(d1,d2)

base_dados %>%
  group_by(G3) %>%
  count()


base_dados <- base_dados %>%
  mutate(G3 = factor(G3))

#como ela é numérica precisamos transformar em classes
levels(base_dados$G3)[levels(base_dados$G3)=="0"] <- "D"
levels(base_dados$G3)[levels(base_dados$G3)=="1"] <- "D"
levels(base_dados$G3)[levels(base_dados$G3)=="2"] <- "D"
levels(base_dados$G3)[levels(base_dados$G3)=="3"] <- "D"
levels(base_dados$G3)[levels(base_dados$G3)=="4"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="5"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="6"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="7"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="8"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="9"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="10"] <- "C"
levels(base_dados$G3)[levels(base_dados$G3)=="11"] <- "B"
levels(base_dados$G3)[levels(base_dados$G3)=="12"] <- "B"
levels(base_dados$G3)[levels(base_dados$G3)=="13"] <- "B"
levels(base_dados$G3)[levels(base_dados$G3)=="14"] <- "B"
levels(base_dados$G3)[levels(base_dados$G3)=="15"] <- "B"
levels(base_dados$G3)[levels(base_dados$G3)=="16"] <- "A"
levels(base_dados$G3)[levels(base_dados$G3)=="17"] <- "A"
levels(base_dados$G3)[levels(base_dados$G3)=="18"] <- "A"
levels(base_dados$G3)[levels(base_dados$G3)=="19"] <- "A"
levels(base_dados$G3)[levels(base_dados$G3)=="20"] <- "A"

#visualiza os novos valores
view(summary(base_dados$G3))
levels(base_dados$G3)

write_csv(base_dados, "base_dados.csv")


##---------------------------- NAIVE BAYES ----------------------------##


##----------------------------...


