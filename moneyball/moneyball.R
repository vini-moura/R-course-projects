rm(list = ls(all=T))

library('ggplot2')
library('dplyr')

battings <- read.csv('~/Documentos/Projetos/moneyball/Batting.csv')
salaries <- read.csv('~/Documentos/Projetos/moneyball/Salaries.csv')
head(battings)
head(salaries)

attach(battings)

str(battings)
str(salaries)

battings$BA <- H/AB
battings$OBP <- (H + BB + HBP) / (AB + BB + HBP + SF)
battings$X1B <- H - X2B - X3B - HR
battings$SLG <- (battings$X1B + 2*X2B + 3*X3B + 4*HR) / AB
head(battings$SLG)
summary(battings)

bat_01 <- subset(battings, yearID == 2001)
sal_01 <- subset(salaries, yearID == 2001)

complete <- merge(bat_01, sal_01)
summary(complete)

lost_players <- subset(complete, playerID %in% c('giambja01','damonjo01','saenzol01')) %>%
                select('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB','yearID')
summary(lost_players)
sum(lost_players$AB)

# new_players$salaries <= 15000000
# sum(new_players$AB) >= 1469
# mean(new_players$OBP) >= 0.3639

new_players <- complete %>%
                select(playerID, salary, AB, OBP) %>%
                filter(sum(salary) <= 15000000 & AB >= 500 & mean(OBP) >= 0.36) %>%


attach(new_players)
new_players
sum(salary)
is.logical(salary %in% sum() <= 15000000)
summary(new_players)
