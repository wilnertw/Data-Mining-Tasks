# Load the dataset
abalone <- read.csv("abalone.csv")

# Load the necessary library
library(e1071)
?naiveBayes

# summarize data
summary(abalone)

# Discretize the "Rings" column into age groups (e.g., young, adult, old)
abalone$AgeGroup <- cut(abalone$Rings, breaks = c(0, 8, 11, Inf), labels = c("Young", "Adult", "Old"))

# Convert the "Sex" column to a factor
abalone$Sex <- as.factor(abalone$Sex)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
train_index <- sample(nrow(abalone), nrow(abalone) * 0.7)
train_data <- abalone[train_index, ]
test_data <- abalone[-train_index, ]

# Create the model
model <- naiveBayes(AgeGroup ~ ., data = train_data)

# Predict the age group for the testing data
predictions <- predict(model, test_data)

# visualize the data
table(predictions, test_data$AgeGroup)

# Evaluate the model
library(caret)
confusionMatrix(predictions, test_data$AgeGroup)
