# exercise assignment: logistic regression
#In this project we will be working with the UCI adult dataset. We will be attempting to predict if people in the data set belong in a certain class by salary, either making <=50k or >50k per year.
#Typically most of your time is spent cleaning data, not running the few lines of code that build your model, this project will try to reflect that by showing different issues that may arise when cleaning data.

library(dplyr); library(Amelia)
library(ggplot2); library(dplyr)
library(caTools)
set.seed(101) 


adult <- read.csv('adult_sal.csv')
adult <- select(adult,-X)

head(adult)
str(adult)
summary(adult)



# cleaning & aggregating data ---- 
# type emp
table(adult$type_employer)
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

# job
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)

# marital
table(adult$marital)
group_marital <- function(mar){
  mar <- as.character(mar)
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
  }else if(mar=='Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)

# country
table(adult$country)
levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')


group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)


str(adult)

adult <- mutate_if(adult, is.character, as.factor)
adult[adult == '?'] <- NA
table(adult$type_employer)


missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
# May take awhile
adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))




# Dataviz ----
ggplot(adult,aes(age)) +
  geom_histogram(aes(fill=income),color='black',binwidth=1) + 
  theme_bw()


ggplot(adult,aes(hr_per_week)) + 
  geom_histogram() +
  theme_bw()


names(adult)[names(adult)=="country"] <- "region"
ggplot(adult,aes(region)) + 
  geom_bar(aes(fill=income),color='black')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(adult[adult$region != 'North.America',],aes(region)) + 
  geom_bar(aes(fill=income),color='black')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Model ----
sample <- sample.split(adult$income, SplitRatio = 0.70)  # SplitRatio = percent of sample==TRUE    
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)


model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)


new.step.model <- step(model)
summary(new.step.model)


test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)

(6372+1423)/(6372+1423+548+872)

#recall
6732/(6372+548)


#precision
6732/(6372+872)

