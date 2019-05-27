# 1. Loading all required libraries and data
library("ggplot2")
library("forecast")
library("tseries")

daily_data = read.csv("../../dat/dataset_bike_day.csv", header=T, stringsAsFactors=F)

# 2. How do my data look like?
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) +
  geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts")

# convert to time series
count_ts = ts(daily_data[, c("cnt")])

# run tsclean
daily_data$clean_cnt = tsclean(count_ts)

ggplot(daily_data, aes(Date, clean_cnt)) +
  geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts")

# using a weekly moving average
daily_data$ma_cnt = ma(daily_data$clean_cnt, order=7)

# plot the ma result against the previous result
ggplot() +
  geom_line(data=daily_data, aes(x=Date, y=clean_cnt, colour="Counts"), size=1) +
  geom_line(data=daily_data, aes(x=Date, y=ma_cnt, colour="Weekly Moving Average"), size=1)  +
  scale_color_manual(values=c("#000000","#F25CAC")) +
  ylab("Bicycle Count")

# 3. Remove the seasonality

# clean the data to remove NAs by month
clean_count_ma = ts(na.omit(daily_data$ma_cnt), frequency=30)

# decompose the series to calculate the seasonal component
# run plot(decomp) to see the decomposition result
decomp = stl(clean_count_ma, s.window="periodic")

# deseasonalize the series
deseasonal_cnt = seasadj(decomp)

# 4. How to determine the parameters? - skipped, check the wiki

# 5. Fitting the model
fit117 = arima(deseasonal_cnt, order=c(1,1,7))
checkresiduals(fit117, lag.max=30)