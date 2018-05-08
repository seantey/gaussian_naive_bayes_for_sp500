setwd("~/Documents/USF-Classes/2018Spring/Math371/final-project/")


stock_data_raw <- read.table("mydata.txt")


# For testing purposes, running all data on classifier
X_stocks <- stock_data_raw[,1:ncol(stock_data_raw)-1]
y_stocks <- stock_data_raw[,ncol(stock_data_raw)]
y_stocks <- as.integer(y_stocks)

nb_output <- gaussian_nb(X_stocks,y_stocks)
nb_output$posteriors_DF
nb_output$predictions
nb_output$accuracy

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


gaussian_nb(X_stocks_train,y_stocks_train)


