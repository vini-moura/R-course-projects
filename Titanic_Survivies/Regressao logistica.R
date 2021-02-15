titanic <- read.csv('Documents/Curso de R/8 Machine Learning with R/titanic_train.csv', sep = ',')
head(titanic)
str(titanic)
library(dplyr)
titanic$Ticket <- as.factor(titanic$Ticket)
str(titanic)
library("Amelia")
missmap(titanic, main = 'Missing Map', col = c('yellow','black'), legend = F)
library(ggplot2)
ggplot(titanic,aes(Survived)) + geom_bar()
ggplot(titanic,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
ggplot(titanic,aes(Sex)) + geom_bar(aes(fill=factor(Sex)))
ggplot(titanic,aes(Age)) + geom_histogram(bin = 20, alpha = 0.5 ,fill='blue')
ggplot(titanic,aes(SibSp)) + geom_bar()
ggplot(titanic,aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha=0.5)

#descobrir a media de idade por classe
ggplot(titanic,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass, fill = factor(Pclass),alpha=0.4)) + scale_y_continuous(breaks = seq(min(0),max(80),by =2)) + theme_bw()
                                               
#função para colocar a media de idade por classe na tabela
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(titanic$Age,titanic$Pclass)
titanic$Age <- fixed.ages
missmap

#tirar variaveis que nao usaremos
titanic <- select(titanic,-PassengerId, -Name, -Ticket, -Cabin)
str(titanic)
head(titanic)

#passar para fator o que é fator
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Parch <- factor(titanic$Parch)
titanic$SibSp <- factor(titanic$SibSp)
str(titanic)

modelo <- glm(Survived ~ ., family = binomial(link = logit), data = titanic)
summary(modelo)

#previsões
library(caTools)
set.seed(101)
amostra <- sample.split(titanic$Survived,SplitRatio = 0.7)
treino.final <- subset(titanic, amostra ==T)
teste.final <- subset(titanic, amostra == F)
modelo.final <- glm(Survived ~., family = binomial(link = logit), data = treino.final)
summary(modelo.final)

probabilidade <- predict(modelo.final, teste.final, type = 'response')
probabilidade <- ifelse(probabilidade>0.5,1,0)
misClassError <- mean(probabilidade != teste.final$Survived)
#acurácia
print(1- misClassError)

#matriz de confusão
table(teste.final$Survived,probabilidade>0.5)
