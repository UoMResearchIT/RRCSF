# Plots kmeans clustering of iris data

library(dplyr)
library(cluster)
library(ggplot2)

setwd("/home/martin/R/RonCSF/bad_example/R")
source("../data-raw/prepare_data.R")

load("../data/iris.RData")

show(
  iris |>
    ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point(size = 4) + theme_minimal()
)

set.seed(42)
KM <- iris |> select(-Species) |> kmeans(centers = 3, nstart = 20)
clusplot(iris, KM$cluster, color = TRUE, shade = TRUE)