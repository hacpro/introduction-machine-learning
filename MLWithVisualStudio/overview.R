

# install.packages("ggplot2", dependencies = T)
# install.packages("GGally", dependencies = T)

# install.packages("plot3D", dependencies = T)

library(ggplot2)
library(GGally)
library(plot3D)

buildings <- read.csv2("C:\\Users\\ch0125\\Dropbox\\Machine Learning\\part_1_data.csv", sep = ",")

# We take a look at all the features we have
summary(buildings)


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

# Any suggestions what may be a good indicator?

# look at the model
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(sqft, price, colour = in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")


# Lets look at the correlation matrix
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf))



# Question 3: Is this weird?
# Different dimensional Visualizations (why not the Kernel Trick, 3.3. Romeo)

# Quesion 4: How is it organized?
# Clustering

# Quesion 5: What should I do next?
# Recommenders
