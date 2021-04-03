# Installing required libraries
install.packages('caTools')
install.packages("neuralnet")

# Importing libraries
library(neuralnet)
library(caTools)

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the target feature as factor
dataset$Exited = factor(dataset$Exited, levels = c(0, 1))

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[1:10] = scale(training_set[1:10])
test_set[1:10] = scale(test_set[1:10])

# fit neural network
nn=neuralnet(Exited~CreditScore+Geography+Gender+Age+Tenure+Balance+NumOfProducts
             +HasCrCard+IsActiveMember+EstimatedSalary,
             data=training_set, 
             stepmax = 1000000,
             hidden=10,act.fct = "logistic",
             algorithm = "rprop+",
             linear.output = FALSE)

# plot neural network
plot(nn)

## Prediction using neural network
Predict=compute(nn,test_set[1:10])
Predict$net.result

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

pred[,2]

# Making the Confusion Matrix
cm = table(test_set$Exited, pred[,2])
print("Confusion Matrix : ")
cm

# Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class
accuracy = sum(diag) / n 
print(paste("Accuracy : ",accuracy))
