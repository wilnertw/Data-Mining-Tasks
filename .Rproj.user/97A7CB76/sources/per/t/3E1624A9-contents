library(ggplot2)

abalone <- read.csv("abalone.csv")

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

abalone <- as.data.frame(lapply(abalone, function(x) if(is.numeric(x)) normalize(x) else x))

summary(abalone)

set.seed(123)

data <- abalone[, c("Length","Diameter", "Whole.weight","Height")]

str(data)

k_3 <- kmeans(data, 3)

clus <- cbind(data, clus2 = k_3$cluster)

# Assuming clus is your data and k_3 is the result of k-means clustering
plot(clus$Diameter, abalone$Rings, col = k_3$cluster, pch = k_3$cluster,
     main = "Diameter vs Rings", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Rings", ylab = "Diameter(mm)")

