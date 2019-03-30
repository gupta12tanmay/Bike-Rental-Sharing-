#Capital Bike-sharing rental with weather and seasonal information for past years 2017-2018

library(data.table)
library(ggplot2)
library(forecast)
library(tseries)

bikedaily_data = read.csv('Dailydata2017-2018.csv',header = TRUE, stringsAsFactors = FALSE)
View(bikedaily_data)

bikehourly_data = read.csv('Hourlydata2017-2018.csv',header = TRUE, stringsAsFactors = FALSE)
View(bikehourly_data)

#summary(bikedaily_data)
#summary(bikehourly_data)

#Examining Data

bikedaily_data$Date = as.Date(bikedaily_data$dteday)

ggplot(bikedaily_data, aes(Date,cnt)) + geom_line() + scale_x_date('month') + ylab("Daily Bike Checkouts") + xlab("")

#In some cases, the number of bicycles checked out dropped below 100 on day and rose to over 4,000 
#the next day. #These are suspected outliers that could bias the model by skewing statistical summaries. 

#Removing outliers

count_ts = ts(bikedaily_data[, c('cnt')])

bikedaily_data$cleandata = tsclean(count_ts)

ggplot() + geom_line(data=bikedaily_data, aes(x=Date, y = cleandata)) + ylab("Cleaned Bicycle Count")


#smoothing the series as daily data is still volatile.

bikedaily_data$cnt_ma = ma(bikedaily_data$cleandata, order = 7) #using the clean data with no outliers
bikedaily_data$cnt_ma30 = ma(bikedaily_data$cleandata, order = 30)

ggplot() + geom_line(data = bikedaily_data, aes(x=Date, y = cleandata , color = "Counts")) + 
  geom_line(data = bikedaily_data, aes(x=Date, y = cnt_ma , color = "Weekly Moving Average")) + 
  geom_line(data = bikedaily_data, aes(x=Date, y = cnt_ma30 , color = "Monthly Moving Average")) +
  ylab('Bicycle Count')


#Decomposing Data

#The building blocks of a time series analysis are seasonality, trend, and cycle.

#we calculate seasonal component of the data using stl(). STL is a flexible function for decomposing 
#and forecasting the series. It calculates the seasonal component of the series using smoothing, 
#and adjusts the original series by subtracting seasonality in two simple lines

count_ma = ts(na.omit(bikedaily_data$cnt_ma), frequency = 30)
decomp_dailydata = stl(count_ma,s.window = "periodic")
deseasonal_cnt <- seasadj(decomp_dailydata)
plot(decomp_dailydata)


#Making data stationary

#using augmenteddickeyfullertest (ADF)

adf.test(count_ma, alternative = "stationary")


#Autocorrelation function and choosing model order

Acf(count_ma, main = '')

Pacf(count_ma, main = '')


count_diff = diff(deseasonal_cnt, differences = 1)
plot(count_diff)
adf.test(count_diff, alternative = 'stationary')

Acf(count_diff, main = 'ACF for Differenced Series')

Pacf(count_diff, main = 'PACF for differenced series')


#Fitting an ARIMA Model

auto.arima(deseasonal_cnt, seasonal = FALSE)


#evaluate and iterating model

fitted_model <-  auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fitted_model),lag.max = 45, main = '(1,1,1)Model Residuals')


fitted_model1 <- arima(deseasonal_cnt, order = c(1,1,7))
fitted_model1


tsdisplay(residuals(fitted_model1), lag.max = 15, main = 'Seasonal Model Residuals')


#forecasting

fcast <- forecast(fitted_model1, h=30)
plot(fcast)


#Comparing forecast to actual observed values

hold <- window(ts(deseasonal_cnt), start = 700)

fit_no_hold = arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))

fcast_no_hold <- forecast(fit_no_hold, h=25)

plot(fcast_no_hold,main = '')

lines(ts(deseasonal_cnt))

#fittingmodelwithseasonality

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal = TRUE)
fit_w_seasonality

seasonal_fcast <- forecast(fit_w_seasonality, h=30)
plot(seasonal_fcast)


tsdisplay(residuals(fit_w_seasonality), lag.max = 15, main = 'Seasonal Model Residuals')
