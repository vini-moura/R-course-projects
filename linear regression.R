# the original assignment on Kaggle was intended to build a random tree model. 
# The course assignment ask for a linear model, which I am doing here.

library('dplyr');library('ggplot2');library('corrplot')

data <- read.csv('bikeshare.csv')
str(data)
head(data)
summary(data)


# Graphics ----

ggplot(data,aes(x=temp,y=count))+
        geom_point(alpha=0.2, aes(colour=temp))

data$datetime <- as.POSIXct(data$datetime)
ggplot(data,aes(x=datetime,y=count))+
  geom_point(alpha=0.2, aes(colour=temp))+ 
  scale_color_continuous(low='#55D8CE',high='#FF6E2E')

cor(data[,c('count','temp')])

ggplot(data, aes(x=factor(season),y=count))+
  geom_boxplot(alpha=0.5,aes(colour=factor(season)))

data$hour <- sapply(data$datetime,function(x){format(x,"%H")})

ggplot(data[data$workingday==1,], aes(x=hour,y=count))+
  geom_point(alpha=0.2, aes(color=temp))+  #,position=position_jitter(w=1, h=0)
  scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))

ggplot(data[data$workingday==0,], aes(x=hour,y=count))+
  geom_point(alpha=0.2, aes(color=temp))+  #,position=position_jitter(w=1, h=0)
  scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))


n_data <- select_if(data,is.numeric)
cor_matrix <- cor(n_data)
corrplot(cor_matrix)

# considerations ----
"
the assignment asked only count and temp. However, on econometrics, is 
considered best practice to make a general model and then cut off those variables
that have a high p-value for significance. Thus, only for simplification, 
I'm assuming as someone has done a theorical investigation and has captured all
necessery data into this dataset. However, we must pay attention to the fact that
the graphical analysis showed us there is no clear linear correlation between
variables
"

# models ----
amostra <- sample(c( TRUE , FALSE ), nrow(data), replace= TRUE , prob=c( 0.7 , 0.3 ))
treinar <- data[amostra, ]
teste <- data[!amostra, ]

data$hour <- sapply(data$hour, as.numeric)
md1 <- lm(count ~ . -datetime, treino)
summary(md1)

md2 <- lm(count ~ .-datetime -atemp -humidity -casual, treinar) 
summary(md2)

md3 <- lm(count ~ . -casual -registered -datetime -atemp,treinar)
summary(md3)


# avaliation ----
AIC(md2);AIC(md3)
BIC(md2);BIC(md3)

adjusted <- fitted(md2)
comparation <- data.frame(Real = treinar$count, Adjusted = adjusted)
head(comparation)
ggplot(comparation, aes(x = Real, y = Adjusted)) +
  geom_point(color = "blue", size = 2)+
  labs(title="Predicted x Real")


#prediction ----
previsoes <- predict(md2, teste)

resultados <- data.frame(
  Observado = teste$count,
  Previsto = previsoes
)

ggplot(resultados, aes(x = Observado, y = Previsto)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Previsões vs Valores Observados",x = "Valores Observados",
       y = "Valores Previstos")

table(teste$season)
