# Load the necessary library
library(kknn)
library(dplyr)
# Assuming 'abalone' is your dataset and it already includes a 'Class' column with 'young', 'adult', 'old' classifications
# If not, you would first need to create this classification based on your criteria.

# Reading the dataset
abalone <- read.csv("abalone.csv")

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

data <- as.data.frame(lapply(abalone, function(x) {
  if(is.numeric(x) && !(colnames(abalone) %in% c("Rings", "Age"))) {
    normalize(x)
  } else {
    x
  }
}))

#convert Sex attributes to factors 
data$Sex <- as.factor(data$Sex)

# Convert Whole.weight to numeric
# First, check if it's not already numeric to avoid unnecessary conversion
if(!is.numeric(data$Whole.weight)) {
  data$Whole.weight <- as.numeric(as.character(data$Whole.weight))
}

# Now, proceed with creating the 'Class' attribute using 'cut'
data$Class <- cut(data$Whole.weight,
                     breaks = quantile(data$Whole.weight, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     labels = c("Young", "Adult", "Old"),
                     include.lowest = TRUE)

# set Class as factor
data$Class <- as.factor(data$Class)

# Splitting the dataset into training and testing sets
set.seed(123)

indexes <- sample(1:nrow(data), size = 0.75 * nrow(data))
train_data <- data[indexes, ]
test_data <- data[-indexes, ]

# Using kknn from the kknn package
knn_3 <- kknn(Class ~ ., train_data, test_data, k = 3, distance = 1, kernel = "optimal")

# Predictions
predictions <- knn_3$fitted.values

# Convert predictions to factors for classification accuracy evaluation
predicted_classes <- factor(predictions, levels = levels(test_data$Class))

# Evaluating model performance
confusionMatrix <- table(Predicted = predicted_classes, Actual = test_data$Class)
print(confusionMatrix)

# Accuracy
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))

# Load required libraries for plotting
library(ggplot2)

# Add the predicted classes to the test_data for plotting
test_data$PredictedClass <- predicted_classes

# Scatter plot using ggplot2
ggplot(test_data, aes(x = Length, y = Diameter, color = PredictedClass)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Young" = "blue", "Adult" = "green", "Old" = "red")) +
  labs(title = "Abalone Age Class Predictions",
       x = "Length (normalized)",
       y = "Diameter (normalized)",
       color = "Class") +
  theme_minimal()

abalone$ActualClass <- cut(abalone$Rings,
                           breaks = c(-Inf, 5, 10, Inf),
                           labels = c("Young", "Adult", "Old"),
                           right = TRUE)

ggplot(abalone, aes(x = Length, y = Diameter, color = ActualClass)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Young" = "blue", "Adult" = "green", "Old" = "red")) +
  labs(title = " Actual Abalone Age Class",
       x = "Length",
       y = "Diameter",
       color = "Class") +
  theme_minimal()


