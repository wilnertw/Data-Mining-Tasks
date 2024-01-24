# //// load the packages ///////////

#install.packages('C50')
library('C50')

#install.packages('gmodels')
library('gmodels')

#install.packages('readr')
library('readr')

# /// end of load for packages ////


# Read abalone.csv as abalone
abalone <- read_csv("abalone.csv")

#Using Normalize to normalize continuous variable data
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
abalone <- as.data.frame(lapply(abalone, function(x) if(is.numeric(x)) normalize(x) else x))

#set seed 123 to allow data not generate at random
set.seed(123)

#Split the data to two sections: train and test data
data_split <- sample(nrow(abalone)*0.8)

#split into train data
train_data <- abalone[data_split,]

#split into test data
test_data  <- abalone[-data_split,]

#check unique values in the outcome variable
unique(train_data[, 10])

#Extract the outcome variable as a vector
outcome_variable <- train_data$Class

#If it's not a factor, convert it into a factor
outcome_variable <- as.factor(outcome_variable)

#generate irTree using the function C5.0
irTree     <- C5.0(train_data[,-10], outcome_variable)

#summary() function is used to view model output

# view the model components
print(summary(irTree))

#view model graphically
#plot(irTree, main = 'abalone decision tree')
plot(irTree, main = 'abalone decision tree')

#prediction
abalone_pred <- predict(irTree, test_data)
library('gmodels')
result <- CrossTable(test_data$Class,abalone_pred,prop.chisq = FALSE,
                     prop.c = FALSE, prop.r = FALSE, dnn = c('actual class','predicted class'))

