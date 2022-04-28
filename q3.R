install.packages("FactoMineR")
library(FactoMineR)
data(decathlon)

colnames(decathlon)[c(1,5,6,10)]<-c("x100m", "x400m", "x110m.hurdle", "x1500m")

cor.test(decathlon$x1500m,decathlon$x100m)
scatterplot(x1500m~x100m, smooth=FALSE, data=decathlon)
RegModel1 <-lm(x1500m ~ x100m, data=decathlon)
summary(RegModel1)

cor.test(decathlon$x1500m,decathlon$x400m)
scatterplot(x1500m~x400m, smooth=FALSE, data=decathlon)
RegModel2 <-lm(x1500m ~ x400m, data=decathlon)
summary(RegModel2)

cor.test(decathlon$x1500m,decathlon$x110m.hurdle)
scatterplot(x1500m~x110m.hurdle, smooth=FALSE, data=decathlon)
RegModel3 <-lm(x1500m ~ x110m.hurdle, data=decathlon)
summary(RegModel3)

beta0<-RegModel2$coefficients[1]
beta0
beta1<-RegModel2$coefficients[2]
beta1

library(lmtest)
shapiro.test(residuals(RegModel2))
bptest(RegModel2)
dwtest(RegModel2, alternative="two.sided")

random_athlete<-data.frame(decathlon[runif(1, 1, 41),]) #random athlete of 41
random_athlete
random_athletex400m<-data.frame(x400m=random_athlete$x400m) #x400m of the random athlete
random_athletex400m

predict.lm(RegModel2, newdata=random_athletex400m, interval="prediction") #prediction with the regression model
