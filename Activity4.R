# GEOG331 Activity 4 Script
# Andrew Xie
# 03/11/22

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

# Create new dataframe with only versicolor irises
flower <- iris[iris$Species == "versicolor",]

# set up vectors of indices representing the columns that will be used in the regression calculations
x <- c(1, 3, 1)
y <- c(2, 4, 3)

# Create list to store regression tables
v <- list()

# For loop to generate regression tables and store them in the list v
for (i in 1:3) {
  v[[i]] <- summary(lm(flower[, x[i]]~flower[, y[i]]))
  print("Regression model: ")
  print(v[[i]])
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

# Full join iris and height by species
new_iris <- full_join(iris, height, by = 'Species')

# Change name of Height.cm column in the height data frame to "Max.Height"
colnames(new_iris)[ncol(new_iris)] <- "Max.Height"

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
new_plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
new_plot

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
no_grid_plot <- new_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
no_grid_plot

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
final_plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, size=Petal.Length)) + geom_point() + 
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
              ggtitle("Sepal Length vs. Width") + theme(plot.title = element_text(hjust = 0.5)) +
              xlab("Sepal length (cm)") + ylab("Sepal Width (cm)")
final_plot

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################
