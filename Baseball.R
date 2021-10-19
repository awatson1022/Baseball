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

summary(lm(TARGET_WINS~TEAM_BATTING_BB, data=mydata))
summary(mydata$TEAM_BATTING_BB)
hist(mydata$TEAM_BATTING_BB)
boxplot(mydata$TEAM_BATTING_BB)

#summary(lm(TARGET_WINS~TEAM_BATTING_HBP, data=mydata))
#summary(mydata$TEAM_BATTING_HBP)
#hist(mydata$TEAM_BATTING_HBP)
#boxplot(mydata$TEAM_BATTING_HBP)

summary(lm(TARGET_WINS~TEAM_BATTING_SO, data=mydata))
summary(mydata$TEAM_BATTING_SO)
hist(mydata$TEAM_BATTING_SO)
boxplot(mydata$TEAM_BATTING_SO)

summary(lm(TARGET_WINS~TEAM_BASERUN_SB, data=mydata))
summary(mydata$TEAM_BASERUN_SB)
hist(mydata$TEAM_BASERUN_SB)
boxplot(mydata$TEAM_BASERUN_SB)

#summary(lm(TARGET_WINS~TEAM_BASERUN_CS, data=mydata))
#summary(mydata$TEAM_BASERUN_CS)
#hist(mydata$TEAM_BASERUN_CS)
#boxplot(mydata$TEAM_BASERUN_CS)

summary(lm(TARGET_WINS~TEAM_FIELDING_E, data=mydata))
summary(mydata$TEAM_FIELDING_E)
hist(mydata$TEAM_FIELDING_E)
boxplot(mydata$TEAM_FIELDING_E)

summary(lm(TARGET_WINS~TEAM_FIELDING_DP, data=mydata))
summary(mydata$TEAM_FIELDING_DP)
hist(mydata$TEAM_FIELDING_DP)
boxplot(mydata$TEAM_FIELDING_DP)

summary(lm(TARGET_WINS~TEAM_PITCHING_BB, data=mydata))
summary(mydata$TEAM_PITCHING_BB)
hist(mydata$TEAM_PITCHING_BB)
boxplot(mydata$TEAM_PITCHING_BB)

summary(lm(TARGET_WINS~TEAM_PITCHING_H, data=mydata))
summary(mydata$TEAM_PITCHING_H)
hist(mydata$TEAM_PITCHING_H)
boxplot(mydata$TEAM_PITCHING_H)

summary(lm(TARGET_WINS~TEAM_PITCHING_HR, data=mydata))
summary(mydata$TEAM_PITCHING_HR)
hist(mydata$TEAM_PITCHING_HR)
boxplot(mydata$TEAM_PITCHING_HR)

summary(lm(TARGET_WINS~TEAM_PITCHING_SO, data=mydata))
summary(mydata$TEAM_PITCHING_SO)
hist(mydata$TEAM_PITCHING_SO)
boxplot(mydata$TEAM_PITCHING_SO)

#summary(lm(TARGET_WINS~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_HBP+TEAM_BATTING_SO, data=mydata))
#summary(lm(TARGET_WINS~TEAM_BASERUN_SB+TEAM_BASERUN_CS, data=mydata))
summary(lm(TARGET_WINS~TEAM_FIELDING_E+TEAM_FIELDING_DP, data=mydata))
summary(lm(TARGET_WINS~TEAM_PITCHING_BB+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_SO, data=mydata))
#summary(lm(TARGET_WINS~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_HBP+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_PITCHING_BB+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_SO, data=mydata))

###PCA#####
xdata <- subset(mydata, select = -c(TARGET_WINS))
xdata

returns.pca <- princomp(x=xdata,cor=TRUE)
# See the output components returned by princomp();
names(returns.pca)
returns.pca
pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)

plot(-10,10,type='p',xlim=c(-0.27,-0.12),ylim=c(-0.27,0.6),xlab='PC 1',ylab='PC 2')
#text(-pc.1,-pc.2,labels=names(pc.1),cex=0.75, col = ind_df$Color)

plot(returns.pca)
scree.values <- (returns.pca$sdev^2)/sum(returns.pca$sdev^2);
plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')

# Make Proportion of Variance Explained
variance.values <- cumsum(returns.pca$sdev^2)/sum(returns.pca$sdev^2);

plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(variance.values,lwd=2,cex=1.5)
abline(h=0.8,lwd=1.5,col='red')
abline(v=8,lwd=1.5,col='red')
text(13,0.5,'Keep 8 Principal Components',col='red')
title('Total Variance Explained Plot')

#####FA###
mycor <- cor(xdata)
corrplot(mycor)

###1###
Z<-eigen(mycor)
Z$val
Z$vec
zperc <- Z$val/13
zperc
cumsum(zperc)
cumsum(Z$val)

scree(mycor)
fa.parallel(mycor, n.obs=2236, fa="both", n.iter=100, show.legend=TRUE,
            main="Scree plot with parallel analysis")

#fa(mycor, nfactors=13, rotate="none", fm="pa")


#elbow at 8
#7 values above one
#Will not arrive at 90%

###2###
factors_data <- fa(r = mycor, nfactors = 4, rotate='varimax', fm='ml')
factors_data
factanal(x= xdata,factors=4,rotation='varimax')

fa_scores <- factor.scores(xdata, factors_data)$scores
fadata <- as.data.frame(fa_scores)
fadata

summary(lm(mydata$TARGET_WINS~fadata$ML1+fadata$ML2+fadata$ML3+fadata$ML4))
summary(lm(TARGET_WINS~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_PITCHING_BB+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_SO, data=mydata))

####clustering#####
set.seed(2)
x <- as.matrix(xdata)
x
xsc <- scale(mycor)
y <- dist(xsc)
hc.complete<-hclust(y,method="complete")
hc.average<-hclust(y,method="average")
hc.single<-hclust(y,method="single")


plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ", sub=" ",cex=0.9)





km1 <- kmeans(x,6,nstart=50)
km1$cluster
plot(fadata$ML1,fadata$ML2, col=(km1$cluster))
plot(xdata$TEAM_BATTING_H  , xdata$TEAM_PITCHING_H, col=(km1$cluster))
plot(xdata$TEAM_BATTING_HR  , xdata$TEAM_PITCHING_HR, col=(km1$cluster))
plot(xdata$TEAM_BATTING_SO  , xdata$TEAM_PITCHING_SO, col=(km1$cluster))
plot(xdata$TEAM_BATTING_BB  , xdata$TEAM_PITCHING_BB, col=(km1$cluster))
plot(fadata$ML1,mydata$TARGET_WINS, col=(km1$cluster))

