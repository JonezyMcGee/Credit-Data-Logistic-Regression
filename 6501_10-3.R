#Clearing Environment
rm(list=ls())

#Setting Seed and Loading Data / Libraries
set.seed(8625)

credit_data <- read.delim('german_credit_data.txt', header=FALSE, sep='')

#Turning response variable into 0s and 1s
#Turns 1s to 0s and 2s to 1s
ones = which(credit_data$V25==1, arr.ind=TRUE)
credit_data$V25[ones] = 0

twos = which(credit_data$V25==2, arr.ind=TRUE)
credit_data$V25[twos] = 1

#Separating Train Data(80%) and Test Data(20%)

proportion <- 0.8
num_to_train <- as.integer(proportion * nrow(credit_data))
train_sample <- sample(seq(1:nrow(credit_data)),size=num_to_train, replace=FALSE)

#shuffling data to make sure I'm choosing a 'random' sample.
credit_data <- credit_data[sample(seq(1:nrow(credit_data)), size=nrow(credit_data), replace=FALSE),]

#Training Data
training_data <- credit_data[train_sample,]

#Testing Data
testing_data <- credit_data[-train_sample,]

#Performing cross validation on training data
#Function returns the prediction accuracy on each validation set

cross_validation <- function(data, k, threshold=0.5){
  
  #Creating k slices
  slices <- cut(seq(nrow(data)), labels=FALSE, k)
  
  #shuffling data
  data <- data[sample(seq(nrow(data)), nrow(data), replace=FALSE),]
  
  accuracy = c()
  
  for (i in 1:k){
    #indices for validation set
    inds <- which(slices==i, arr.ind=TRUE)
    
    #Separating Data
    val_set <- data[inds,]
    train_set <- data[-inds,]
    
    #Resetting Index
    row.names(val_set) <- NULL
    row.names(train_set) <- NULL
    
    #Logistic Regression Model
    l_regression <- glm(V25~., family = binomial(link = 'logit'), train_set)
    
    predictions <- predict(l_regression, val_set[,-25])
    
    transf_pred <- exp(predictions) / (1 + exp(predictions))
    
    greater <- which(transf_pred > threshold, arr.ind = TRUE)
    lesser <- which(transf_pred < threshold, arr.ind = TRUE)
    
    transf_pred[greater] = 1
    transf_pred[-greater] = 0
    
    accuracy[i] <- sum(transf_pred == val_set[,25]) / nrow(val_set)
    
  }
  return (accuracy)
}

accuracy1 <- cross_validation(training_data, 10, threshold=0.8)
accuracy1
mean(accuracy1)


#Calculating the cost at different thresholds with train and test data
cost <- function(train, test, threshold, expression){

  #Logistic Regression with Threshold of 0.8
  glm <- glm(expression, family = binomial, train)

  #Predictions
  glm_pred <- predict(glm, test[,-25])

  #Converting results into probabilities
  glm_pred_prob <- exp(glm_pred) / (1 + exp(glm_pred))

  #Converting Results into 0s and 1s based on threshold.
  greater <- which(glm_pred_prob > threshold, arr.ind=TRUE)
  glm_results <- as.vector(glm_pred_prob)
  glm_results[greater] <- 1
  glm_results[-greater] <- 0
  glm_results

  #Confusion Matrix
  response <- test[,25]

  conf_matrix <- matrix(data=0, nrow=2, ncol=2)
  row.names(conf_matrix) = c('Actually True', 'Actually False')
  colnames(conf_matrix) = c('True', 'False')

  for (i in 1:length(response)){
  
    if (response[i] == 1 & glm_results[i] == 1){
      conf_matrix[1,1] = conf_matrix[1,1] + 1
    }
    else if (response[i] == 1 & glm_results[i] == 0){
      conf_matrix[1,2] = conf_matrix[1,2] + 1
    }
    else if (response[i] == 0 & glm_results[i] == 1){
      conf_matrix[2,1] = conf_matrix[2,1] + 1
    }
    else if (response[i] == 0 & glm_results[i] == 0){
      conf_matrix[2,2] = conf_matrix[2,2] + 1
    }
  }
  conf_matrix

  #Cost of bad answers
  #False Positives are five times as bad as False Negatives
  cost <- 5 * conf_matrix[2,1] + conf_matrix[1,2]
  
  return (cost)
}



thresholds = seq(0,1, 0.01)
costs <- c()
for (i in 1:length(thresholds)){
  costs[i] <- cost(training_data, 
                   testing_data, 
                   thresholds[i],
                   V25~.)
}

costs
min_costs <- which(costs==42, arr.ind=TRUE)
min_thresholds <- thresholds[min_costs]

#Can choose between 0.17, 0.24, and 0.25. 0.25 seems quite square
#I will use that.



#Logistic Regression with Threshold of 0.25
glm1 <- glm(V25~., family = binomial, training_data)
summary(glm1)

#Predictions
glm_pred <- predict(glm1, testing_data[,-25])

#Converting results into probabilities
glm_pred_prob <- exp(glm_pred) / (1 + exp(glm_pred))

#Converting Results into 0s and 1s based on threshold.
greater <- which(glm_pred_prob > 0.81, arr.ind=TRUE)
glm_results <- as.vector(glm_pred_prob)
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
    conf_matrix[1,1] = conf_matrix[1,1] + 1
  }
  else if (response[i] == 1 & glm_results[i] == 0){
    conf_matrix[1,2] = conf_matrix[1,2] + 1
  }
  else if (response[i] == 0 & glm_results[i] == 1){
    conf_matrix[2,1] = conf_matrix[2,1] + 1
  }
  else if (response[i] == 0 & glm_results[i] == 0){
    conf_matrix[2,2] = conf_matrix[2,2] + 1
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






#Let's use the five highest signifcant variables from the first model.
#This would be V1, V2, V3, V5, V17
glm2 <- glm(V25~ V1 + V2 + V3 + V5 + V17, family = binomial, training_data)
summary(glm2)

costs2 = c()
thresholds2 = seq(0,1, 0.01)
for (i in 1:length(thresholds2)){
  costs2[i] <- cost(training_data, 
                    testing_data, 
                    thresholds2[i],
                    V25 ~ V1 + V2 + V3 + V5 + V17)
}

costs2
#We can get the costs down to 111 with these predictors.

min_costs2 <- which(costs2==47, arr.ind=TRUE)
min_thresholds2 <- thresholds2[min_costs2]
#Choose between 0.26 and 0.28. We will choose 0.26 to be safe.

#Predictions
glm2_pred <- predict(glm2, testing_data[,-25])

#Converting results into probabilities
glm2_pred_prob <- exp(glm2_pred) / (1 + exp(glm2_pred))

#Converting Results into 0s and 1s based on threshold.
greater2 <- which(glm2_pred_prob > 0.87, arr.ind=TRUE)
glm2_results <- as.vector(glm2_pred_prob)
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
    conf_matrix2[1,1] = conf_matrix2[1,1] + 1
  }
  else if (response2[i] == 1 & glm2_results[i] == 0){
    conf_matrix2[1,2] = conf_matrix2[1,2] + 1
  }
  else if (response2[i] == 0 & glm2_results[i] == 1){
    conf_matrix2[2,1] = conf_matrix2[2,1] + 1
  }
  else if (response2[i] == 0 & glm2_results[i] == 0){
    conf_matrix2[2,2] = conf_matrix2[2,2] + 1
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

#The first model with all predictors is demonstrably better when it comes to 
#both cost and accuracy,
