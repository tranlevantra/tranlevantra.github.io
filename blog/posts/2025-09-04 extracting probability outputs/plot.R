library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# Prepare data
data <- read.csv("~/Documents/Github/tranlevantra.github.io/blog/posts/2024-12-11 decision threshold/code/data.csv")

# Prepare Plot Data
data |> 
  arrange(Observed) |> 
  mutate(x = row_number()) |> 
  pivot_longer(
    cols = c(Observed, Probability),
    names_to = "Type",
    values_to = "y") |> 
  select(x, y, Type) -> plot_data

plot_data


# Create the plot
plot_data |> 
  ggplot(aes(x = x, y = y, shape = Type, group = x)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1),
                     labels = c("True Class Labels", "Predicted Probabilities")) +
  geom_line(linetype = "dashed") +
  labs(title = "Model Performance on Test Data",
       subtitle = "with Probability Outputs",
       x = "Observations",
       y = "Probability",
       color = "",
       shape = "") +
  theme(
    legend.key = element_rect(colour = "black"),
    legend.position =  "bottom" 
  )
library(randomForest)
data(iris)
set.seed(111)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,])
iris.pred <- predict(iris.rf, iris[ind == 2,])
table(observed = iris[ind==2, "Species"], predicted = iris.pred)
## Get prediction for all trees.
predict(iris.rf, iris[ind == 2,], typ = "prob")
## Proximities.
predict(iris.rf, iris[ind == 2,], proximity=TRUE)
## Nodes matrix.



  
