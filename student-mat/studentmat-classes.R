rm(list=ls(all=TRUE))
#limpar dados
setwd('~/Documentos/Projetos/R course/student-mat/')
studentmat <- read.csv('student-mat.csv',sep = ';',header = T)
str(studentmat)
summary(studentmat)
#verificar se tem algum que deveia ser fator, numerico ou caracter e nao ??
library(dplyr)
library('ggplot2')
#transformar caracters como fator
studentmat <- studentmat %>% mutate_if(is.character, factor)

#checar dados nulos
any(is.na(studentmat))

#verificar a correla????o entre os numericos:
#primeiro separar os dados numericos
col.num <- sapply(studentmat,is.numeric)
#depois filtrar
cor.dados <- cor(studentmat[,col.num])
print(cor.dados)

ggplot(cor.dados,aes(x=Salario.anual.R..)) + geom_histogram(color='black',fill='blue',binwidth = 5)
ggplot(cor.dados,aes(x=Pontuacao.de.Gasto..1.100.)) + geom_histogram(color='black',fill='blue',binwidth = 10)

#outro jeito
library(corrplot)
library(corrgram)



#Regress??o propriamente dita
#Separar os dados em Treino e Teste
library(caTools)
    #set a seed
    set.seed(101)
    #separar a amostra que quero predizer
    amostra <- sample.split(studentmat$G3,SplitRatio = 0.7)
    #salvando em 'treino' os 70% de amostra
    treino <- subset(studentmat,amostra == TRUE)
    #salvando  em 'teste os 30% restante
    teste <- subset(studentmat, amostra == FALSE)

#podemos ent??o treinar e criar o modelo com o grupo de 'treino'
modelo <- lm(G3 ~ .,data = treino)
    #ver detalhes do modelo:
    summary(modelo)
    plot(modelo)
  
#Predi????es
g3.previsoes <- predict(modelo,teste)
    
resultados <- cbind(g3.previsoes,teste$G3)
colnames(resultados) <- c('previsto',"atual")
resultados <- as.data.frame(resultados)
print(head(resultados))

#resolver os valores negativos  
zerar <- function(x){
  if (x<0) {
    return(0)
  }else{
    return(x)
  }
}
#aplicar fun????o zerar
resultados$previsto <- sapply(resultados$previsto,zerar)

#Vari??ncia
vari <- mean((resultados$atual - resultados$previsto)^2)
print('Vari??ncia')
print(vari)
#Desvio Padr??o
print('Desvio Padr??o')
print(vari^0.5)

# calcular R quadrado
#soma das variancias
sse <- sum( (resultados$previsto - resultados$atual)^2 )
#soma do quadrado total
sst <- sum( (mean(studentmat$G3) - resultados$atual)^2 )

r2 <- 1 - sse/sst
print(r2)


