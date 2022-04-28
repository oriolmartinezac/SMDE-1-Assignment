#Creation of the three population
n1<-rnorm(100, mean=10, sd=5)
n2<-rnorm(100, mean=15, sd=5)
n3<-rnorm(100, mean=10, sd=5)

plot(density(n2),main="Three Populations")
lines(density(n1),col=2)
lines(density(n3),col=3)

#We prepare the data for the ANOVA
v1n=data.frame(x1=n1, x2="n1")
v2n=data.frame(x1=n2, x2="n2")
v3n=data.frame(x1=n3, x2="n3")

library(RcmdrMisc)
#New dataframe
data=mergeRows(v1n, v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), v3n, common.only=FALSE)

AnovaModel <- aov(x1 ~ x2, data=data)
summary(AnovaModel)

Boxplot(x1~x2,data=data,id=FALSE)

dwtest(AnovaModel)
shapiro.test(residuals(AnovaModel))
bptest(AnovaModel)

#Q2
#Importing the diabetes database
diabetes <- read.csv("diabetes.csv")
head(diabetes)
#Categorize the Ages into groups (Young, Middle Age, Old)
diabetes$Age_Groups<-cut(diabetes$Age, c(0, 30, 50, 100))
levels(diabetes$Age_Groups)<-c("Young","Middle Age", "Old")

library(lmtest) #Library for the tests
for (i in 1:7){ 
  print((colnames(diabetes)[i])) #Pregnancies, Glucose, BloodPressure, Skin Thickness, Insulin, BMI, DiabetesPedigreeFunction
  AnovaModel.i<-aov(diabetes[,i]~Age, data=diabetes)
  print(dwtest(AnovaModel.i))  #Independency
  print(shapiro.test(residuals(AnovaModel.i))) #Normality
  print(bptest(AnovaModel.i)) #Homogeneity of variances
  print(summary(AnovaModel.i))
}

Boxplot(Pregnancies~Age,data=diabetes,id=FALSE)
Boxplot(Glucose~Age,data=diabetes,id=FALSE)
Boxplot(BloodPressure~Age,data=diabetes,id=FALSE)
Boxplot(SkinThickness~Age,data=diabetes,id=FALSE)

#2
for (i in 1:8){
  print((colnames(diabetes)[i]))
  AnovaModel.i<-aov(diabetes[,i]~Outcome, data=diabetes)
  print(summary(AnovaModel.i)[[1]][1,5]) #Extract the Pr(>F), p-value 
}



#3)
#Two-way ANOVA
model<-aov(diabetes$BloodPressure~diabetes$Age_Groups+diabetes$Outcome)
summary(model)
shapiro.test(diabetes$BloodPressure) #Normality
dwtest(model) #Independency
bptest(model) #Homogeneity of variances
