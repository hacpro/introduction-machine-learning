

# install.packages("ggplot2", dependencies = T)
# install.packages("GGally", dependencies = T)
# install.packages("plot3D", dependencies = T)
# install.packages("e1071")

library(ggplot2)
library(GGally)
library(plot3D)
library(e1071)


# Visualisierung von 'Agenda' oder ML-Uebersicht 
# mit http://www.bioconductor.org/packages/2.8/bioc/html/Rgraphviz.html oder
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html

# Set the working directory
setwd("c:/source/introduction-machine-learning")

# Some setup for our investigation
buildings <- read.csv2("buildings.csv", sep = ",")

# We take a look at all the features we have
summary(buildings)

# And some basis statics for price
summary(buildings$price)
hist(buildings$price, breaks = 200, prob = T)
abline(v = mean(buildings$price),
     col = "blue",
     lwd = 1)
abline(v = median(buildings$price),
     col = "red",
     lwd = 1)
lines(density(buildings$price), 
 lwd = 2, # thickness of line
 col = "gray")

# Some more advanced stuff - nobody actually cares
skewness(buildings$price)
kurtosis(buildings$price)

# we can do this a little nicer - but not now

# Quesion 1: How many
# How can we guess the price of a building? Relevant features?
# -----------------------------------------------------------------------------------------------

# Regression with price
# if there are any suggestions we look at the regression (e.g. price and sqft)

# look at the model
ggplot(buildings, aes(sqft, price, col = buildings$in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")

# footnote other types of regression

# print our model
summary(lm(price ~ sqft, buildings))
# not that good? we need more variables

# Problem with visualization (price_per_sqft is a joke)
summary(lm(price ~ sqft + price_per_sqft, buildings))
# pretty good, but we need other types of visualization

# Visual Studio cannot display interactive graphs
# example(persp3D)
# scatter3D(buildings$sqft, buildings$price_per_sqft, buildings$price, buildings,
#    col = "blue", size = 1, type = "s", main = "3D Quadratic Model Fit with Log of Income")


# Question 2: Is this A or B
# -----------------------------------------------------------------------------------------------

# How can we determine if a house is from sf or ny?

# Lets look at the correlation matrix
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf))

# Any suggestions what may be a good indicator ?

# If suggestions: We learned regression, so this must be different for both
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(sqft, price, colour = in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")



# Question 3: Is this weird?
# Different dimensional Visualizations (why not the Kernel Trick, 3.3. Romeo)

# Quesion 4: How is it organized?
# Clustering

# Quesion 5: What should I do next?
# Recommenders
