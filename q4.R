library(FactoMineR)
data(decathlon)
competition <- which(colnames(decathlon) == "Competition")
plot(decathlon[,-c(competition)])
pca<-PCA(decathlon[,-c(competition)])

competition <- which(colnames(decathlon) == "Competition")
pca$eig
plot(pca$eig[,1], type="o", main="Scree Plot")

summary(pca)
decathlon$PC1<-pca$ind$coord[,1]
decathlon$PC2<-pca$ind$coord[,2]
decathlon$PC3<-pca$ind$coord[,3]
decathlon$PC4<-pca$ind$coord[,4]

pc<-lm(Points~PC1 + PC2 + PC3 + PC4, data=decathlon)
summary(pc)

shapiro.test(residuals(pc))
bptest(pc)
dwtest(pc, alternative = "two.sided")

plot(residuals(pc))

n <- nrow(decathlon)
train.sample <- sample(1:n, round(0.67*n))
train.set <- decathlon[train.sample, ]
test.set <- decathlon[-train.sample, ] 
train.model <- lm(Points ~ PC1+PC2+PC3+PC4 , data = train.set)
summary(train.model)

y<-predict(train.model, test.set, interval="prediction")
y
yt<-test.set$score
error<-cbind(y[,1,drop=FALSE],yt,(y[,1]-y)^2)
sqr_err<-error[,1]
sse<-sum(sqr_err)
RMSE<-sqrt(sse/(nrow(test.set))) #raiz(SSE/N)
RMSE
