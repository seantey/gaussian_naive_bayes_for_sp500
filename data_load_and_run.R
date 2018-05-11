
stock_data_raw <- read.table("test_stock_data.txt")


# For testing purposes, running all data on classifier
X_stocks <- stock_data_raw[,1:ncol(stock_data_raw)-1]
y_stocks <- stock_data_raw[,ncol(stock_data_raw)]
y_stocks <- as.integer(y_stocks)

print("MLE")
nb_output <- gaussian_nb(X_stocks,y_stocks,FALSE)
print("means for class 0")
print(nb_output$means_zero)
print("means for class 1")
print(nb_output$means_one)
print("sigmas for class 0")
print(nb_output$sigma_zero)
print("sigmas for class 1")
print(nb_output$sigma_one)
print("MLE estimate accuracy")
print(nb_output$accuracy)

print("BAYESIAN")
nb_output <- gaussian_nb(X_stocks,y_stocks,TRUE)
print("means for class 0")
print(nb_output$means_zero)
print("means for class 1")
print(nb_output$means_one)
print("sigmas for class 0")
print(nb_output$sigma_zero)
print("sigmas for class 1")
print(nb_output$sigma_one)
print("Bayesian estimate accuracy")
print(nb_output$accuracy)

# Work in progress!! Need to implement predict function.
# 25% test data 75% training data
train_end_index <- nrow(stock_data_raw)- round(nrow(stock_data_raw)/4)
test_start_index <-(nrow(stock_data_raw)- round(nrow(stock_data_raw)/4)+1)

X_stocks_train <- stock_data_raw[1:train_end_index,1:ncol(stock_data_raw)-1]
y_stocks_train <- stock_data_raw[1:train_end_index,ncol(stock_data_raw)]
y_stocks_train <- as.integer(y_stocks_train)

X_stocks_test <- stock_data_raw[test_start_index:nrow(stock_data_raw),1:ncol(stock_data_raw)-1]
y_stocks_test <- stock_data_raw[test_start_index:nrow(stock_data_raw),ncol(stock_data_raw)]
y_stocks_test <- as.integer(y_stocks_test)


format(1810032000, scientific = FALSE)

print("MLE")
print("Means for class 0")

format(nb_output$means_zero, scientific = FALSE)

print("Means for class 1")
print(nb_output$means_one)

means_df <- data.frame(nb_output_mle$means_zero,nb_output_mle$means_one,nb_output_bayes$means_zero,nb_output_bayes$means_one)

write.table(means_df,file = "means_file", sep = " & ")
