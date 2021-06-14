library("forecast") # Read the dataset

data <- read.csv("~/R/01_District_wise_crimes_committed_IPC_2001_2012.csv")    # Read the (2001 - 2012) dataset
ndata <- data[ which(data[2]=='TOTAL'), ]   # Extract the columnsrows with the totals of every state over the years

st <- ndata[1]  # Extract the state column from the dataset to a list
un <- unique(st)  # Get all the state names

for (i in un[[1]])
{
  newdata <- data[ which(data[2]=='TOTAL' & data[1]==i), ]  # Extract the totals of every state of each year
  a <- ts(newdata[33], start=c(2001, 1), end=c(2012, 1))    # The variable is converted into time series class
  fit <- arima(a, order=c(1, 1, 0)) # The ARIMA (1, 1, 0) model is fitted
  pred <- forecast(fit, 2)  # The ARIMA model is used to predict the values for the next 2 years
  
  print(paste(2013, "===", pred$mean[1]))
  print(paste(2014, "===", pred$mean[2]))
  print("------------------------------")

  jpeg(paste(i, '.jpg'))
  plot(forecast(fit, 2), main=i, xlab="Year", ylab="Crime Numbers")   # The plot of the training and prediction value is plotted.
  dev.off()
}
