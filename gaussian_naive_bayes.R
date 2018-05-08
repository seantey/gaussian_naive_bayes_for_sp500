# Gaussian Naive Bayes:
# Tweaked for continuous input and binary class output.

# Use bayes rule to predict probability of a class Y given data X
# Solve for P(Y|X) = P(X|Y)P(Y) / P(X)
# Use law of total probability, assume Y = k , where j = 1...m classes 
# Expand to P(Y = k|X) = P(X|Y = k)P(Y = k) / ( P(X|Y=1)P(Y=1) + P(X|Y=2)P(Y=2) + ... P(X|Y=m)P(Y=m) )

# For our purpose, we will use the classifier to predict for a given stock metric today, whether S&P500 will increase tomorrow.

# Solve for P(Y=k|Xij) = P(Xij|Y)P(Y=k) / P(Xij), where:
# i = 1,...,n market days
# j = 1,...,p stock metric/features
# k = 0,1 where 1 = S&P500 increased since yesterday, 0 otherwise

# Assume naive assumption: conditional independence of Xij's
# Xij = Xi1,...Xip for a given day i, we have stock metric 1...p
# P(Xi1,Xi2,..Xip|Y) = P(Xi1|Y) x P(Xi2|Y) x ... x P(Xip|Y) x P(Y)

# Therefor to determin if S&P500 increased, we will solve for P(Y=1|Xij) = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) / P(Xij)
# Re-written as:
# P(Y=1|Xij) = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) / P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) + P(Xi1|Y=0)...P(Xip|Y=1)P(Y=0)

# Function Name: gnb_check_input()
# Parameters: X -> Data Matrix, y -> Response vector (labels/classes)
# Purpose: Function for checking input to be used in classifier
gnb_check_input <- function(X,y){
  # Guard statements, ensures input data validity.
  if(!is.data.frame(X)) {
    stop("Parameter X must be a data frame, please provide the data frame whose columns are numeric variables and rows are observations")
  }
  
  # Check if length of vector y matches number of rows in matrix X
  if(!(nrow(X)==length(y))) stop("Length of response vector y must be equal to number of rows in X")
  
  # Helper function to verify input.
  binary_vector_check <- function(input_vector){
    
    if(!is.integer(input_vector)) stop("Please provide an integer vector")
    
    is_binary <- all(input_vector <= 1) && all(input_vector >= 0)
    
    if(!is_binary) stop("Please provide an integer vector of 1's and 0's")
    
    return(TRUE)
    
  }
  # Check if y is made of 1's and 0's, where 1 = increase in price since yesterday, 0 = decrease or no increase
  binary_vector_check(y)
}

# Function Name: gnb_check_input()
# Parameters: X -> Data Matrix, 
#             y -> Response vector (labels/classes)
#             bayes_estimator -> logical indicator to toggle bayes estimator for mean calculation vs using just sample mean
# Purpose:  For a given set of numeric data matrix X of size nxp, with labels represented by response vector of size nx1,
#           The function will attempt to use bayes rule to predict the class of a given data row.
gaussian_nb <- function(X,y,bayes_estimator=FALSE){
  
  gnb_check_input(X,y)
  
  # First make empirical estimates of prior probability P(Y) and P(X|Y)
  
  # P(Y) is easy to calculate, just find the proportion of labels, in this case
  # occurence of label=1 over total observation n
  
  # sum(y) gives total days which we observe increase in S&P500
  # length(y) gives total observations (i.e. days)
  prior_Y_one <- sum(y) / length(y)
  prior_Y_zero <- 1 - prior_Y_one 
  
  # For empirical P(X|Y), if X was discrete we we do a similar proportion calculation as above
  # Since our X's are stock related metrics, we will assume P(X|Y) follows some distribution, e.g. gaussian
  
  # First we need to find mean and sd for rows where Y = 0,1 in order to calculate gaussian pdf
  # we need the mean and sd for Y = 1 and Y = 0, for each column j = 1,..,p
  # Therefore we will have p x k number of different mew and sigma in total
  
  X_given_one_subset <- X[as.logical(y),] # subset only rows where Y = 1
  X_given_zero_subset <- X[as.logical(1-y),] # subset only rows where Y = 0
  
  mew_one = 0 # initialize mew
  sigma_one = 1 # initialize sigma
  
  mew_zero = 0 # initialize mew
  sigma_zero = 1 # initialize sigma
  
  if (bayes_estimator == FALSE){
    
    mew_one = sapply(X_given_one_subset,mean)
    sigma_one = sapply(X_given_one_subset,sd)
    
    mew_zero = sapply(X_given_zero_subset,mean)
    sigma_zero = sapply(X_given_zero_subset,sd)
    
  } else {
    # Some bayes estimator function output:
    mew_one = sapply(X_given_one_subset,mean) # replace mean function with something else!
    sigma_one = sapply(X_given_one_subset,sd) # replace sd function with something else!
    
    mew_zero = sapply(X_given_zero_subset,mean) # replace mean function with something else!
    sigma_zero = sapply(X_given_zero_subset,sd) # replace sd function with something else!
    
  }  
  
  # Initialize a data frame of size n x p to hold P(Xij|Y=k) for each entry Xij of data matrix X
  X_given_Y_one <- data.frame(matrix(nrow = nrow(X),ncol = ncol(X))) # Y = 1
  X_given_Y_zero <- data.frame(matrix(nrow = nrow(X),ncol = ncol(X))) # Y = 0 
  
  # For every individual row, calculate the prob(Xij|Y = 1), recall that we are fitting a gaussian distribution for this
  # The notation prob(Xij|Y = 1) is mathematically inaccurate, this would be a probability density not a probability!
  # This still works as an application of bayes rule but will be explained below.
  
  # Use appropriate mew & sigma corresponding to column p and class Y = k
  for (i in 1:nrow(X)){
    row_data = X[i,] 
    
    for (p in 1:ncol(X)){
      prob_XY <- dnorm(row_data[[p]],mew_one[p],sigma_one[p])
      X_given_Y_one[i,p] <- prob_XY
    }    
  }

  # For every individual row, calculate the prob(Xij|Y = 0)  
  for (i in 1:nrow(X)){
    row_data <- X[i,] 
    
    for (p in 1:ncol(X)){
      prob_XY <- dnorm(row_data[[p]],mew_zero[p],sigma_zero[p])
      X_given_Y_zero[i,p] <- prob_XY
      
    }    
  }
  
  # Recall P(Y=1|Xij) = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) / P(Y=1|Xij) = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) + P(Xi1|Y=0)...P(Xip|Y=1)P(Y=0)
    
  # Numerator = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1)
  row_product_ones <- apply(X_given_Y_one,1,prod) # take product of entire row from rows 1...n
  numerator_ones <- row_product_ones*prior_Y_one
  
  # Denominator = P(Xi1|Y=1)...P(Xip|Y=1)P(Y=1) + P(Xi1|Y=0)...P(Xip|Y=1)P(Y=0)
  row_product_zeros <- apply(X_given_Y_zero,1,prod)
  numerator_zeros <- row_product_zeros*prior_Y_zero

  # Even though P(Xij|Y=k) is a density not a probability, but because the numerator is a component of the denominator,
  # P(Y=1|Xij) will at most be equal to one, this normalizes the entire equation and therefore its value is between 0 to 1
  
  posteriors_ones <- numerator_ones / (numerator_ones + numerator_zeros)
  posteriors_zeros <- numerator_zeros / (numerator_ones + numerator_zeros)
  
  # Predictions
  # If P(Y=1|Xij) > P(Y=0|Xij), i.e. likelihood of class 1 is greater, 
  # we will label the observation row i as class Y = 1
  predictions <- ifelse(posteriors_ones>posteriors_zeros,1,0)
  
  
  
  # Generate list of outputs: (1) Prior Probabilities of Y, (2) Class conditional probabilities P(Xij|Y=k),
  # (3) Posterior probabilities, (4) Predicted Labels for each data row, (5) accuracy
  
  output_priors <- c(prior_Y_zero,prior_Y_one)

  class_conditional_DF_list <- list(X_given_Y_zero,X_given_Y_one)
  
  posteriors_DF <- data.frame(posteriors_zeros,posteriors_ones)
  
  accuracy <- sum(ifelse(predictions==y,1,0))/nrow(X)
  
  # Generate a named list for easy access to elements. 
  # E.g. output_list$priors will access first element etc
  output_list <- list(priors = output_priors, class_conditional_DF_list = class_conditional_DF_list,
                      posteriors_DF = posteriors_DF, predictions = predictions,
                      accuracy = accuracy)
  
  return(output_list)
  
}
