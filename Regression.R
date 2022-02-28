# GEOG331 Regression Lecture Script
# Andrew Xie
# 02/28/22

# subset for Iris virginica
flower <- iris[iris$Species == "virginica",]

# linear model relating petal length to sepal length
fit <- lm(flower$Petal.Length~flower$Sepal.Length)
  
# view results
summary(fit)

# create a scatter plot
plot(flower$Sepal.Length, flower$Petal.Length,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple", pch = 16)