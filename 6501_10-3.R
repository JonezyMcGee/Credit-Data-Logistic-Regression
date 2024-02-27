#Clearing Environment
rm(list=ls())

#Setting Seed and Loading Data / Libraries
set.seed(8625)

credit_data <- read.delim('german_credit_data.txt', header=FALSE, sep='')


#Turning response variable into 0s and 1s
#Turns 1s to 0s and 2s to 1s

credit_data$V25

ones = which(credit_data$V25==1, arr.ind=TRUE)
credit_data$V25[ones] = 0

twos = which(credit_data$V25==2, arr.ind=TRUE)
credit_data$V25[twos] = 1


#Separating Train Data(80%) and Test Data(20%)
proportion <- 0.8
num_to_train <- as.integer(proportion * nrow(credit_data))
train_sample <- sample(seq(1:nrow(credit_data)),size=num_to_train, 
                       replace=FALSE)


#shuffling data to make sure I'm choosing a 'random' sample.
credit_data <- credit_data[sample(seq(1:nrow(credit_data)), 
                                  size=nrow(credit_data), 
                                  replace=FALSE),]


#Training Data
training_data <- credit_data[train_sample,]


#Testing Data
testing_data <- credit_data[-train_sample,]


#Creating Validation Set
training_data <- training_data[sample(seq(1:nrow(training_data)),
                                      size=nrow(training_data),
                                      replace=FALSE),]
proportion2 <- 0.25
val_sample <- sample(seq(1:nrow(training_data)),
                    size = as.integer(proportion2 * nrow(training_data)),
                     replace=FALSE)
validation_data <- training_data[val_sample,]
training_data <- training_data[-val_sample,]



#Calculating the cost at different thresholds with train and test data
cost <- function(training_set, validation_set, threshold, expression){
  
  
  #Logistic Regression With Inputs
  glm <- glm(expression, family = binomial(link = 'logit'), training_set)

  #Predictions
  glm_pred <- predict(glm, validation_set[,-25])

  #Converting results into probabilities
  glm_pred_prob <- exp(glm_pred) / (1 + exp(glm_pred))

  #Converting Results into 0s and 1s based on threshold.
  greater <- which(glm_pred_prob > threshold, arr.ind=TRUE)
  glm_results <- as.vector(glm_pred_prob)
  
  #Any amount greater than threshold is a 1 and any amount less than is a 0.
  glm_results[greater] <- 1
  glm_results[-greater] <- 0

  #Actual response values of test data set
  response <- validation_set[,25]

  #Setting up confusion matrix
  conf_matrix <- matrix(data=0, nrow=2, ncol=2)
  row.names(conf_matrix) = c('Actually True', 'Actually False')
  colnames(conf_matrix) = c('True', 'False')

  for (i in 1:length(response)){
  
    #True Negatives
    if (response[i] == 1 & glm_results[i] == 1){
      conf_matrix[2,2] = conf_matrix[2,2] + 1
    }
    #False Positives
    else if (response[i] == 1 & glm_results[i] == 0){
      conf_matrix[2,1] = conf_matrix[2,1] + 1
    }
    #False Negatives
    else if (response[i] == 0 & glm_results[i] == 1){
      conf_matrix[1,2] = conf_matrix[1,2] + 1
    }
    #True Positives
    else if (response[i] == 0 & glm_results[i] == 0){
      conf_matrix[1,1] = conf_matrix[1,1] + 1
    }
  }

  #Cost of bad answers
  #False Positives are five times as bad as False Negatives
  cost <- 5 * conf_matrix[2,1] + conf_matrix[1,2]
  
  return (cost)
}


#Thresholds from 0 to 1 segmented by 0.01.
thresholds = seq(0,1, 0.01)

#Variable to hold the costs with varying thresholds.
costs <- c()

for (i in 1:length(thresholds)){
  
  #Inputting into function with varying thresholds
  costs[i] <- cost(training_data,
                   validation_data,
                   thresholds[i],
                   V25~.)
}

costs

#Finding minimum costs and choosing threshold
min_costs <- which(costs==102, arr.ind=TRUE)
min_costs
min_thresholds <- thresholds[min_costs]
min_thresholds


#Chosen Threshold (0.32)
thresh = min_thresholds[1]

#Logistic Regression with Threshold of 0.32
glm1 <- glm(V25~., family = binomial, training_data)
summary(glm1)

#Predictions
glm_pred <- predict(glm1, testing_data[,-25])

#Converting predictions into probabilities
glm_pred_prob <- exp(glm_pred) / (1 + exp(glm_pred))

#Converting Results into 0s and 1s based on threshold.
greater <- which(glm_pred_prob > thresh, arr.ind=TRUE)
glm_results <- as.vector(glm_pred_prob)

#Classifying by Threshold of 0.32. 
glm_results[greater] <- 1
glm_results[-greater] <- 0
glm_results

#Confusion Matrix
response <- testing_data[,25]

conf_matrix <- matrix(data=0, nrow=2, ncol=2)
row.names(conf_matrix) = c('Actually True', 'Actually False')
colnames(conf_matrix) = c('True', 'False')

for (i in 1:length(response)){
  
  if (response[i] == 1 & glm_results[i] == 1){
    conf_matrix[2,2] = conf_matrix[2,2] + 1
  }
  else if (response[i] == 1 & glm_results[i] == 0){
    conf_matrix[2,1] = conf_matrix[2,1] + 1
  }
  else if (response[i] == 0 & glm_results[i] == 1){
    conf_matrix[1,2] = conf_matrix[1,2] + 1
  }
  else if (response[i] == 0 & glm_results[i] == 0){
    conf_matrix[1,1] = conf_matrix[1,1] + 1
  }
}
conf_matrix

#Cost of bad answers
#False Positives are five times as bad as False Negatives
cost <- 5 * conf_matrix[2,1] + conf_matrix[1,2]
cost

#Accuracy
accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / length(response)
accuracy

#Let's see if there are any adjustments that can be made to the predictors.
#Then we will try to minimize the costs and compute another confusion matrix.
#And accuracies
#There are a lot of binary predictors so I don't believe PCA analysis
#Would be a good idea.





#Costs 2 with important coefficients
costs2 = c()
thresholds2 = seq(0,1, 0.01)
for (i in 1:length(thresholds2)){
  costs2[i] <- cost(training_data, 
                    validation_data, 
                    thresholds2[i],
                    V25 ~ V1 + V2 + V3 + V5 + V17 + V18)
}

costs2
#We can get the costs down to 111 with these predictors.

min_costs2 <- which(costs2==114, arr.ind=TRUE)
min_thresholds2 <- thresholds2[min_costs2]
min_thresholds2
#Choose between 0.26 and 0.28. We will choose 0.26 to be safe.

#Logistic Regression Model 2
glm2 <- glm(V25~ V1 + V2 + V3 + V5 + V17 + V18, 
            family = binomial, training_data)
summary(glm2)

#Predictions
glm2_pred <- predict(glm2, testing_data[,-25])

#Converting results into probabilities
glm2_pred_prob <- exp(glm2_pred) / (1 + exp(glm2_pred))

#Converting Results into 0s and 1s based on threshold.
greater2 <- which(glm2_pred_prob > min_thresholds2[1], arr.ind=TRUE)
glm2_results <- as.vector(glm2_pred_prob)

#Classifying 1s and 0s by threshold.
glm2_results[greater2] <- 1
glm2_results[-greater2] <- 0
glm2_results

#Confusion Matrix
response2 <- testing_data[,25]

conf_matrix2 <- matrix(data=0, nrow=2, ncol=2)
row.names(conf_matrix2) = c('Actually True', 'Actually False')
colnames(conf_matrix2) = c('True', 'False')

for (i in 1:length(response2)){
  
  if (response2[i] == 1 & glm2_results[i] == 1){
    conf_matrix2[2,2] = conf_matrix2[2,2] + 1
  }
  else if (response2[i] == 1 & glm2_results[i] == 0){
    conf_matrix2[2,1] = conf_matrix2[2,1] + 1
  }
  else if (response2[i] == 0 & glm2_results[i] == 1){
    conf_matrix2[1,2] = conf_matrix2[1,2] + 1
  }
  else if (response2[i] == 0 & glm2_results[i] == 0){
    conf_matrix2[1,1] = conf_matrix2[1,1] + 1
  }
}
conf_matrix2

#Cost of bad answers
#False Positives are five times as bad as False Negatives
cost2 <- 5 * conf_matrix2[2,1] + conf_matrix2[1,2]
cost2

#Accuracy
accuracy2 <- (conf_matrix2[1,1] + conf_matrix2[2,2]) / length(response2)
accuracy2

