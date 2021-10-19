library(readxl)
library(corrplot)
library(psych)

#mydata <- read_excel("~/NU Q4/Datasets/MoneyBall.xlsx")
allmydata <- read_excel("~/NU Q4/Datasets/MoneyBall.xlsx")
mydata <- subset(allmydata, select = -c(INDEX, TEAM_BATTING_HBP, TEAM_BASERUN_CS))
mydata <- mydata[complete.cases(mydata),]
head(mydata)
str(mydata)
cor(mydata)
colSums(is.na(mydata))
corrplot(cor(mydata[complete.cases(mydata),]))
summary(allmydata)
summary(mydata)

summary(mydata$TARGET_WINS)
hist(mydata$TARGET_WINS)
boxplot(mydata$TARGET_WINS)

summary(lm(TARGET_WINS~TEAM_BATTING_H, data=mydata))
summary(mydata$TEAM_BATTING_H)
hist(mydata$TEAM_BATTING_H)
boxplot(mydata$TEAM_BATTING_H)

summary(lm(TARGET_WINS~TEAM_BATTING_2B, data=mydata))
summary(mydata$TEAM_BATTING_2B)
hist(mydata$TEAM_BATTING_2B)
boxplot(mydata$TEAM_BATTING_2B)

summary(lm(TARGET_WINS~TEAM_BATTING_3B, data=mydata))
summary(mydata$TEAM_BATTING_3B)
hist(mydata$TEAM_BATTING_3B)
boxplot(mydata$TEAM_BATTING_3B)

summary(lm(TARGET_WINS~TEAM_BATTING_HR, data=mydata))
summary(mydata$TEAM_BATTING_HR)
hist(mydata$TEAM_BATTING_HR)
boxplot(mydata$TEAM_BATTING_HR)