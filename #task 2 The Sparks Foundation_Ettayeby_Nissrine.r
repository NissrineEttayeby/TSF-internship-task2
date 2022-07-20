#task 2 The Sparks Foundation data science & business Analytics intern
#ETTAYEBY NISSRINE 

library(cluster)
#overview of iris
head(iris)
dim(iris)
str(iris)

#summary of iris
summary(iris)

#verification if we need data cleaning 
colSums(is.na(iris))

#Clustering Iris dataset without species column
data <- iris[,-5]
class <- iris[,5]

#K_means algorithm
w <- 0
for (i in 1:15) w[i] <- kmeans(data,centers=i)$tot.withinss
plot(1:15, w, type="b",
     xlab="Number of Clusters",
     ylab="Sum of squares",col="red",pch=16,lwd=3)

# elbow of the graph at 3
res <- kmeans(data,centers=3)
class(res)

#clustering prediction 
table(class)

res$size
res$cluster

table(class,res$cluster)

#Visualisation Compairing clustering prediction with actual species in Petal.length and Petal.width

par(mfrow=c(1,2))
plot(data$Petal.Length,data$Petal.Width,col=res$cluster,pch=19,
     xlab="Petal Length",ylab="Petal Width",main="By cluster")
plot(data$Petal.Length,data$Petal.Width,col=class,pch=19,
     xlab="Petal Length",ylab="Petal Width",main="By species")

#Visualisation Compairing clustering prediction with actual species in Sepal.Length and Sepal.Width

par(mfrow=c(1,2))
plot(data$Sepal.Length, data$Sepal.Width,col=res$cluster,pch=19,
     xlab="Sepal Length",ylab="Sepal Width",main="By cluster")
plot(data$Sepal.Length, data$Sepal.Width,col=class,pch=19,
     xlab="Sepal Length",ylab="Sepal Width",main="By Species")

#Visualisation Compairing clustering prediction with actual species By Principal Component Analysis

p <- princomp(data)
par(mfrow=c(1,2))
plot(p$scores[,1],p$scores[,2],col=res$cluster,
     pch=16,
     xlab="Principal Component 1",
     ylab="Principal Component 2",
     main="By cluster")
plot(p$scores[,1],p$scores[,2],col=class,
     pch=16,
     xlab="Principal Component 1",
     ylab="Principal Component 2",
     main="By species")