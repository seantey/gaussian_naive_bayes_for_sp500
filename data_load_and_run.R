# First make sure current working directory is the same path as test_stock_data.txt.

# Run this to automatically set CWD to script location or manually specifying dir for setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stock_data_raw <- read.table("test_stock_data.txt")

# For testing purposes, running all data on classifier
X_stocks <- stock_data_raw[,1:ncol(stock_data_raw)-1]
y_stocks <- stock_data_raw[,ncol(stock_data_raw)]
y_stocks <- as.integer(y_stocks)

nb_output_MLE <- gaussian_nb(X_stocks,y_stocks,FALSE)
nb_output_Bayes <- gaussian_nb(X_stocks,y_stocks,TRUE)

print(nb_output_MLE$accuracy)
print(nb_output_Bayes$accuracy)

# Work in progress!! Need to implement predict function.
# Do 4 fold cross validation with 25% test data 75% training data
train_end_index <- nrow(stock_data_raw)- round(nrow(stock_data_raw)/4)
test_start_index <-(nrow(stock_data_raw)- round(nrow(stock_data_raw)/4)+1)

X_stocks_train <- stock_data_raw[1:train_end_index,1:ncol(stock_data_raw)-1]
y_stocks_train <- stock_data_raw[1:train_end_index,ncol(stock_data_raw)]
y_stocks_train <- as.integer(y_stocks_train)

X_stocks_test <- stock_data_raw[test_start_index:nrow(stock_data_raw),1:ncol(stock_data_raw)-1]
y_stocks_test <- stock_data_raw[test_start_index:nrow(stock_data_raw),ncol(stock_data_raw)]
y_stocks_test <- as.integer(y_stocks_test)
