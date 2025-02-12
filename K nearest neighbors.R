#install.packages('ISLR',repos = 'http://cran.us.r-project.org')
library(ISLR)
library(class)
library(ggplot2)
set.seed(101)

str(Caravan)
summary(Caravan$Purchase)
any(is.na(Caravan))

# KNN function needs the Y var to be in a separate argument
purchase <- Caravan[,86]


# standarizing ----
'Because the KNN classifier predicts the class of a given test observation by identifying 
the observations that are nearest to it, the scale of the variables matters. Any variables 
that are on a large scale will have a much larger effect on the distance between the observations, 
and hence on the KNN classifier, than variables that are on a small scale.'

var(Caravan[,1])

var(Caravan[,2])

standardized.Caravan <- scale(Caravan[,-86])

var(standardized.Caravan[,1])

var(standardized.Caravan[,2])



# train and test separation ----
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]


# model ----
predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
head(predicted.purchase)

mean(test.purchase != predicted.purchase)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=3)
mean(test.purchase != predicted.purchase)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=5)
mean(test.purchase != predicted.purchase)

predicted.purchase = NULL
error.rate = NULL

# two methods to find K number: for loop or eldow methods
for(i in 1:20){
  set.seed(101)
  predicted.purchase = knn(train.data,test.data,train.purchase,k=i)
  error.rate[i] = mean(test.purchase != predicted.purchase)
}

min(error.rate)
k.values <- 1:20

error.df <- data.frame(error.rate,k.values)

error.df

ggplot(error.df,aes(x=k.values,y=error.rate)) + 
  geom_point()+ 
  geom_line(lty="dotted",color='red')

# the best k number is 9
predicted.purchase <- knn(train.data,test.data,train.purchase,k=9)
head(predicted.purchase)

mean(test.purchase != predicted.purchase)