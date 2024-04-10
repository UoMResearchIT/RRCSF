# Writes the example `iris` data set to data/iris.RData

if (!dir.exists("data")) dir.create("data")

save(iris, file = "data/iris.RData")