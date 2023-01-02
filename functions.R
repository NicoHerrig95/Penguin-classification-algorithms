# sourcing script for functions


# Normalization function
# INPUT: numerical vector
# OUTPUT: Normalized numerical vector
normalizer <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# Function for testing accuracy
# INPUT: Categorical classification vectors, one for predictions and one for
# true values of train set
# OUTPUT: Accuracy in percent
accuracy_testing <- function(vector_predictions, vector_trueclasses) {
  
  # testing whether input vectors do have equal length
  if (length(vector_predictions) != length(vector_trueclasses)) {
    stop("vector do not have equal length")
  }
  
  accuracy = sum(vector_predictions == vector_trueclasses) / length(vector_trueclasses)
  
  return(accuracy)
}



# generating testset and trainset
# INPUT: Data frame and test/train split (default is 80/20)
# OUTPUT: Trainset and Testset
set_generator <- function(data, p_trainset = 0.8, p_testset = 0.2) {
  
  # sampling for train set observations (80% of data)
  obs_train <- sample(x = seq(1, nrow(data)),
                      size = nrow(data)*0.8)
  
  # sampling for test set observations (20% of data)
  obs_test <- seq(1, nrow(data))[- obs_train]
  
  
  trainset <- data[obs_train,]
  testset <- data[- obs_train,]
  
  
  output <- list(trainset = trainset,
                 testset = testset)
  
  
  return(output)
  
}


