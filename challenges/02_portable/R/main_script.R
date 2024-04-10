# Plots kmeans clustering of iris data

library(dplyr)
library(cluster)
library(ggplot2)

library(here)
i_am("R/main_script.R")
setwd(here())

source("data-raw/prepare_data.R")
load("data/iris.RData")

if (!dir.exists("results")) dir.create("results")

iris |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 4) + theme_minimal()
ggsave("results/iris_plot.png", width = 600, height = 480, units = "px", dpi = 92)

set.seed(42)
KM <- iris |> select(-Species) |> kmeans(centers = 3, nstart = 20)

png(file = "results/KM_plot.png", width = 600, height = 480)
clusplot(iris, KM$cluster, color = TRUE, shade = TRUE)
dev.off()

save(file = "results/KM.RData", KM)