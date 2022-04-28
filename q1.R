#Question 1
#a)
decathlon <- read.csv("decathlon.csv") #Import the decathlon data 
boxplot(X100m ~ Competition, data=decathlon, main="Decathlon Data")

#b)
decathlon$new_category<-cut(decathlon$X100m, c(0,11,20)) #Cutting on 11
levels(decathlon$new_category)<-c("Lower than 11s","Higher than 11s") #Categorizing

tab<-table(decathlon$new_category, decathlon$Competition) #New table with the new category
tab
margin.table(tab)
chisq.test(tab)

#c)
plot(density(decathlon$X100m),main="Density function of X100m")
plot(density(decathlon$Long.jump),main="Density function of Long Jump")
plot(density(decathlon$Shot.put),main="Density function of Shot Put")
plot(density(decathlon$High.jump),main="Density function of High Jump")
plot(density(decathlon$X400m),main="Density function of X400m")
plot(density(decathlon$X110m.hurdle),main="Density function of X110m")
plot(density(decathlon$Discus),main="Density function of Discus")
plot(density(decathlon$Pole.vault),main="Density function of Pole")
plot(density(decathlon$Javeline),main="Density function of Javeline")
plot(density(decathlon$X1500m),main="Density function of X1500m")

#d)
n1<-rnorm(50, mean=5, sd=7)
n2<-rnorm(50, mean=5, sd=3)
n3<-rnorm(50, mean=8, sd=7)

t.test(n1, n2, var.equal = TRUE)
t.test(n2, n3, var.equal = TRUE)
t.test(n1, n3, var.equal = TRUE)

#e)
t.test(decathlon$X100m ~ decathlon$Competition, var.equal=TRUE)
t.test(decathlon$X400m ~ decathlon$Competition, var.equal=TRUE)



