## Assignment 3 Titanic Log Regression
titanic<-read.table("http://math.ucdenver.edu/RTutorial/titanic.txt",sep="\t",header=TRUE)
titanic<-within(titanic,{
  + Age[Age>17]<-'Adult'
  + Age[Age>=0&Age<=17]<-'Child'
  + })
  + Survived[Survived==0]<-'no'
  + Survived[Survived==1]<-'yes'
  + })
summary(titanic)
str(titanic)
head(titanic)
#                                            Name PClass   Age    Sex Survived
# 1                  Allen, Miss Elisabeth Walton    1st Adult female      yes
# 2                   Allison, Miss Helen Loraine    1st Child female       no
# 3           Allison, Mr Hudson Joshua Creighton    1st Adult   male       no
# 4 Allison, Mrs Hudson JC (Bessie Waldo Daniels)    1st Adult female       no
# 5                 Allison, Master Hudson Trevor    1st Child   male      yes
# 6                            Anderson, Mr Harry    1st Adult   male      yes
xtabs(~ Survived + Age, data = titanic)
# Age
# Survived Adult Child
#      no    405    38
#      yes   255    58
xtabs(~ Survived + Sex, data = titanic)
# Sex
# Survived female male
# no     154  709
# yes    308  142
t <- xtabs(~ Survived + Age, data = titanic)
df <- as.data.frame(t)
df
# Survived   Age Freq
# 1       no Adult  405
# 2      yes Adult  255
# 3       no Child   38
# 4      yes Child   58
(308/142)/(142/709)
# [1] 10.8298
(308/(308+142))/(142/(142+709))
# [1] 4.101847

library(graphics)
library(ggplot2)
mosaicplot(Survived~ PClass+Age, data = titanic,shade=T, highlighting_direction = "right")
g <- ggplot(titanic,aes(x=PClass,fill=Survived))
g + geom_bar(position='stack')+ coord_flip() + facet_wrap(~Age)
mosaicplot(Survived~ PClass+Sex, data = titanic,shade=T, highlighting_direction = "right")
p <- ggplot(titanic,aes(x=PClass,fill=Survived))
p + geom_bar(position='stack')+ coord_flip() + facet_wrap(~Sex) 

library(MASS)
titanic<-read.table("http://math.ucdenver.edu/RTutorial/titanic.txt",sep="\t",header=TRUE)
my.model <- glm(Survived ~ PClass + Sex + Age + PClass:Sex, data = titanic, family = binomial(link = 'logit'))
summary(my.model)
predict <- predict(my.model,type='response')
head(predict)
o<-order(runif(dim(titanic)[1]))
summary(o)
titanic.train <- titanic[o[1:655],]
titanic.pred <- titanic[o[656:1313],]
titanic.survival.mod <- glm(Survived ~ PClass + Sex + PClass:Sex + Age, family = binomial(logit), data = titanic.train)
titanic.oos.predict<-predict(titanic.survival.mod, type="response", newdata=titanic.pred)
install.packages("fANCOVA")
library(fANCOVA)
loess.data<-as.data.frame(cbind(titanic.oos.predict, Survived=titanic.pred$Survived))
loess.data<-na.omit(loess.data)
titanic.lo.mod<-loess.as(loess.data$titanic.oos.predict, loess.data$Survived, degree=1)
titanic.lo.pred<-predict(titanic.lo.mod, newdata=titanic.oos.predict)
oo<-order(titanic.oos.predict)
plot(titanic.lo.pred[oo]~titanic.oos.predict[oo], type="l", ylim=c(0,1), xlab="logit predicted Pr(Survive)", ylab="empirical probability Pr(Survive)")
abline(0, 1, lty=2)
rug(titanic.oos.predict)
dev.copy2eps(file="logit-curve.eps")
# RStudioGD 
# 2 
dev.copy(png,'logit-curve.png')
# quartz_off_screen 
# 4 
dev.off()
# RStudioGD 
# 2 
live.logit<-which(loess.data$titanic.oos.predict>0.5)
sum(loess.data$survived[live.logit])/length(live.logit)
# [1] 0

library(rpart)
formula <- Survived~ PClass + Sex + Age
fit <- rpart(formula,titanic)
install.packages("maptree")
library(maptree)
draw.tree(fit)



