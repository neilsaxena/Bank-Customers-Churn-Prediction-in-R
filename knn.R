# Installing required libraries
# install.packages('caTools')

# Importing libraries
library(caTools)
library(class)

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the target feature as factor
dataset$Exited = factor(dataset$Exited, levels = c(0, 1))

# Encoding categorical variables as factor
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
split = sample.split(dataset$Exited, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[1:10] = scale(training_set[1:10])
test_set[1:10] = scale(test_set[1:10])


# Fitting K-NN to the Training set and Predicting the Test set results
y_pred = knn(train = training_set[, -11],
             test = test_set[, -11],
             cl = training_set[, 11],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
print("Confusion Matrix : ")
cm

# Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class
accuracy = sum(diag) / n 
print(paste("Accuracy : ",accuracy))