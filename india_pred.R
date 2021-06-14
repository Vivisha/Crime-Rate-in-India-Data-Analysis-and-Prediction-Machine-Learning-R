library("forecast") # Importing the package forecast

data <- read.csv("~/R/01_District_wise_crimes_committed_IPC_2001_2012.csv") # Read the (2001 - 2012) dataset
data2 <- read.csv("~/R/2013.csv")   # Read the 2013 dataset

tot2013 <- data2[ which(data2[2]=='ZZ TOTAL'), ]  # The dataset is narrowed down to state Totals
tot2013 <- tot2013[c(1, 2, 3, 33)]  # The dataset is narrawed down to State name, District name, Year, Crime
total_2013 <- sum(tot2013[4])   # The total number of crimes in the country is calculated using sum()

tot <- c()  # The list containing the total crimes in India since 2001 to 2012

for (i in 2012:2001)
{
  newdata <- data[ which(data[2]=='TOTAL' & data[3]==i), ]    # The dataset is narrowed down to state Totals
  newdata <- newdata[c(1, 2, 3, 33)]    # The dataset is narrawed down to State name, District name, Year, Crime
  t <- sum(newdata[4])     # The total number of crimes in the country is calculated using sum()
  tot <- append(t, tot)    # It is appended to the list containing the totals of every year
}
a <- ts(tot, start=c(2001, 1), end=c(2012, 1))  # The variable is converted into time series class
fit <- arima(a, order=c(1, 1, 0)) # The ARIMA (1, 1, 0) model is fitted
pred <- forecast(fit, 2)  # The ARIMA model is used to predict the values for the next 2 years
accuracy( pred$mean[1], total_2013)   # The accuracy is tested over the validation data (2013)
  
jpeg(paste("India", '.jpg'))
plot(forecast(fit, 2), main="India", xlab="Year", ylab="Crime Numbers")  # The plot of the training and prediction value is plotted.
dev.off()