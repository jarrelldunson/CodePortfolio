

#=======================================================
# Portfolio Notes - Jarrell Dunson
#=======================================================
#------------------------------- import these
library(ggplot2) 
library(ggfortify)
library(forecast)
library(fpp2)
library(astsa)

#=======================================================
# 1) importing/processing time series data
#=======================================================
# There are five ways to inport files: web, csv, xlsx, txt

#------------------------------- Clear data
rm(list=ls())

#------------------------------- first setup path 
path <- 'C:/Users/hp/OneDrive/db/j5/IU/ts/project/'	#--- change per your directory

#------------------------------- (1) read from the web
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
cbe_1 <- read.table(www, header = T)


#------------------------------- (2) read from csv
csv_file <- paste(path, 'cbe.csv', sep = "")		#--- paste concatenates text

cbe_2  = read.table(csv_file, header=TRUE, sep=",", stringsAsFactors = FALSE)
cbe_2b = read.csv(csv_file, header = FALSE)


#------------------------------- (3) read from xlsx (install 'gdata' first)
#install.packages('gdata')
require(gdata)
xls_file <- paste(path, 'cbe.xlsx', sep = "")
cbe_3 = read.xls(xls_file, sheet = 1, header = TRUE)


#------------------------------- (4) read from txt
txt_file <- paste(path, 'cbe.txt', sep = "")
cbe_4 = read.table(txt_file, header=TRUE)


#------------------------------- (5) read.csv
library("zoo")
setwd(path)
cbe_5 <- read.csv('cbe.csv', header = TRUE) 



#=======================================================
# 2) exploratory visualization of time series 
#=======================================================

#------------------------------- sample data
Elec.ts <- ts(cbe_1[, 3], start = 1958, freq = 12)
Beer.ts <- ts(cbe_1[, 2], start = 1958, freq = 12)
Choc.ts <- ts(cbe_1[, 1], start = 1958, freq = 12)

#------------------------------- review the data
x <- Choc.ts
summary(x)
str(x);class(x)
length(x);head(x,10); tail(x,12); start(x); end(x)
mean(x); sd(x);
frequency(x); cycle(x); aggregate.ts(x, frequency=12, median)
time(x)
print(x)
#------------------------------- check for null data
is.null(x)
is.na(x)

#------------------------------- some plots
plot(Choc.ts)
autoplot(Choc.ts) #-- ensure library are install above
hist(Choc.ts)
#------------------------------- plot multiple series 
plot(cbind(Elec.ts, Beer.ts, Choc.ts))
autoplot(cbind(Elec.ts, Beer.ts, Choc.ts), facets = FALSE)
autoplot(cbind(Elec.ts, Beer.ts, Choc.ts), facets = TRUE)

#------------------------------- log, diff plots
plot(log(x))
plot(diff(x))
#------------------------------- boxplots
boxplot(Choc.ts)
boxplot(Choc.ts ~ cycle(Choc.ts))
#------------------------------- check for normal distribution
qqnorm(Choc.ts)
qqline(Choc.ts)
plot(density(Choc.ts))

#------------------------------- seasonal plots (Hyndman, 2019)
ggseasonplot(x, year.labels=FALSE)
ggseasonplot(x, year.labels=FALSE, polar = TRUE)
ggseasonplot(x, year.labels=FALSE, continuous=TRUE)
ggseasonplot(x, year.labels=FALSE, continuous=TRUE, polar = TRUE)
ggsubseriesplot(x)

#------------------------------- seasonal plots (Hyndman, 2019)

choc_10yr = window(Choc.ts,start=1980,NULL)
ggseasonplot(choc_10yr, year.labels = TRUE) + geom_point() + theme_bw()
ggseasonplot(choc_10yr, year.labels = FALSE, continuous = FALSE) + geom_point() + theme_bw()

#------------------------------- lag plot
gglagplot(x)
gglagplot(x, lags=12, seasonal = TRUE)

#------------------------------- ACF plot
ggAcf(x)
ggAcf(diff(x))
#------------------------------- Ljung-Box Test 
#--- small p < .05 =  NOT a white-noise series

Box.test(Choc.ts, lag = 24, fitdf = 0, type = "Ljung")


#------------------------------- last 10 years + forecast
choc_10yr = window(Choc.ts,start=1980,NULL)
future_choc = snaive(choc_10yr, h = 2 * frequency(choc_10yr))
autoplot(future_choc)
summary(future_choc)


#=======================================================
# 3) Time series analysis
#=======================================================
# The following are various notes from the class 
# I also included examples from sources, as these I thought useful 



#================================================================ 
# Module 2 notes
#================================================================

#------------------------------- Clear data
rm(list=ls())

#------------------------------- Import Data  (Metcalfe & Cowpertwait, 2009, 7-9)
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Maine.dat"
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)

class(Maine.month) 

#=============================== Exploratory Visualization Of Time Series 
x = Nile 
#------------------------------- Explore the data
print(x)
head(x,10);tail(x,12)  
start(x); end(x)
summary(x)
length(x) 
mean(x);median(x)

aggregate(x)
cycle(x)
frequency(x)
deltat(x)


#=============================== Plot Time Series 
plot(x) # plot time series
boxplot(x)
hist(x)
#------------------------------- Customize a plot
plot(x
     , main = "Title"
     , xlab = "xtitle", ylab = "ytitle"
     , type ="b"   
)
#------------------------------- Plot Types 
# p-points, l-lines, b-both, c-the lines part alone of b, o-both 'overplotted', 
# h-'histogram', # s-stair steps, S-other steps, n-no plotting.
# see help(plot)
#------------------------------- Set Bottom, Left, Top, Right margins
par(mar=c(5,5,5,5))  
#------------------------------- Layout
layout(1:1)

#=============================== Sequences
rm(list=ls())
#------------------------------- Create a sequence 
x <- seq(1,20)  
layout(1:1)
x <- seq(1,10,.5)
plot(2*x, sin(x))

#=============================== AirPassengers Data (Sample Monthly/Seasonal Data) 
rm(list=ls())
#------------------------------- from (Metcalfe & Cowpertwait, 2009, 4-7)

#------------------------------- get data
data(AirPassengers)
AP <- AirPassengers
head(AP)
#------------------------------- data exploration
head(AP)
summary(AP)
start(AP); end(AP)
frequency(AP); 
cycle(AP)
aggregate(AP)
mean(AP, na.rm = TRUE)

#------------------------------- layout, margins- Bottom, Left, Top, Right
par(mar=c(2,1,1,1))  
layout(1:2)
#------------------------------- plot the data
plot(AP, ylab = "Passengers (1000's)")
boxplot(AP)
#------------------------------- plot aggregate
plot(AP, ylab = "Passengers (1000's)")
plot(aggregate(AP))

# remove seasonal effects by aggregating data at the annual level
#------------------------------- aggregate data   
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

#------------------------------- Reference on margin size 
# layout(matrix(c(1,2), nrow=2, byrow=TRUE))
#------------------ Ref Margins
# r - Error in plot.new() : figure margins too large, Scatter plot - Stack Overflow
# https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot


#=============================== Maine Unemployment data (Sample Monthly Data)
#------------------------------- (Metcalfe & Cowpertwait, 2009, 7-10)
rm(list=ls())
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Maine.dat"
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)
class(Maine.month) 
#------------------------------- All the dates
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.month.ts 
#------------------------------- Divide by 12 = mean annual rate
Maine.annual.ts <- aggregate(Maine.month.ts)/12
Maine.annual.ts
#------------------------------- Plot
par(mar=c(2,2,2,2)) 
layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")

#------------------------------- Window function:
# Extract part of the time series between start / end points
# Iinterval = frequency if its argument (if set to TRUE)

mean(Maine.month.ts)
#------------------------------- get totals for a month, accross years:
Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)


#=============================== US Unemployment data
#------------------------------- (Metcalfe & Cowpertwait, 2009, 10)
rm(list=ls())

#-------- monthly unemployment rate for all of the US, Jan 1996 until Oct 2006
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/USunemp.dat"
US.month <- read.table(www, header = T)
attach(US.month)
#------------------------------- Create /plot time series
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
layout(1:1)
plot(US.month.ts, ylab = "unemployed (%)")


#=============================== Multiple time series: Electricity, Beer, Chocolate
#------------------------------- (Metcalfe & Cowpertwait, 2009, 11)
rm(list=ls())

#-------- Multiple time series: Electricity, beer and chocolate data
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www, header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
#------------------------------- plot multiple series 
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

#------------------------------- Air Passenger Data & Electricity Data
data(AirPassengers)
AP <- AirPassengers
AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
#------------------------------- first three rows
AP.elec[1:3, ] 
#------------------------------- column data 
AP.elec[,1]
AP <- AP.elec[,1]
Elec <- AP.elec[,2]
#------------------------------- dual plot
par(mar=c(2,5,2,2))
layout(1:2)
plot(AP, main = "Air passengers by Year", ylab = "Air passengers / 1000's")
plot(Elec, main = "Electricity By Year", ylab = "Electricity production / MkWh")

#------------------------------- scatter plot 
par(mar=c(5,5,2,2))  #-- Bottom, Left, Top, Right
layout(1:1)
plot(as.vector(AP), as.vector(Elec),
     xlab = "Air passengers / 1000's",
     ylab = "Electricity production / MWh")
#------------------------------- linear regression
abline(reg = lm(Elec ~ AP))
#------------------------------- correlation
cor(AP, Elec)


#=============================== Exchange rates
#------------------------------- (Metcalfe & Cowpertwait, 2009, 14)
rm(list=ls())
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/pounds_nz.dat"
Z <- read.table(www, header = T)
Z[1:4, ]
head(Z)

#------------------------------- plot / quarterly time series
Z.ts <- ts(Z, st = 1991, fr = 4)
plot(Z.ts, xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")

#------------------------------- plt / two sub-series 
par(mar=c(5,5,1,2)) #-- Bottom, Left, Top, Right
Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))
layout (1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pound",
     xlab = "Time (years)" )
plot(Z.96.98, ylab = "Exchange rate in $NZ/pound",
     xlab = "Time (years)" )


#=============================== Global
#------------------------------- (Metcalfe & Cowpertwait, 2009, 17)
rm(list=ls())
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat"
Global <- scan(www)

#------------------------------- plot / monthly data & yearly aggregate mean
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
layout(1:2)
plot(Global.ts)
plot(Global.annual)
#------------------------------- time 
New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
head(New.time,24)
#------------------------------- plot 
layout(1:1)
plot(New.series)
#------------------------------- plot regression line
abline(reg=lm(New.series ~ New.time))

#------------------------------- create own time series, start =2004, freq is quarter
t <- seq(from = 1, to = 100, by = 1) + 10 + rnorm(100, sd = 7)
Z.ts <- ts(t, st = 2004, fr = 4)
plot(Z.ts)

#================================================================ 
# Module 3 notes
#================================================================
#------------------------------- Some stats
median(x,na.rm = TRUE)
sd(x,na.rm = TRUE)
IQR(x,na.rm = TRUE)
mad(x,na.rm = TRUE)

#=============================== Decomposition
#------------------------------- (Metcalfe & Cowpertwait, 2009, 23)
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www, header = T)

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts)
plot(decompose(Elec.ts))

Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)


#================================================================ 
# Module 4 notes 
#================================================================
#------------------------------- (Matteson, 2019) 
rm(list=ls())

#------------------------------- Removing trends with differencing
diff(x)
#------------------------------- Removing seasonal trends with seasonal differencing
diff(x, lag = 4)  #-- quarters


#=============================== White Noise
#------------------------------- use arima.sim, order = c(0, 0, 0); Optional mean/sd
wn <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
wn_2 <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 50, sd = 10)
ts.plot(wn)
ts.plot(wn_2)
#------------------------------- Fit a WN model 
arima(wn_2, order = c(0, 0, 0))
#------------------------------- Sample mean and variance 
mean(wn_2)
var(wn_2)
sd(wn_2)

#=============================== Random walk

#------------------------------- use arima.sim, order = c(0, 1, 0)
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
#------------------------------- Differencing (t, t-1)
random_walk_diff <- diff(random_walk)
#------------------------------- plot
layout(1:2)
ts.plot(random_walk)
ts.plot(random_walk_diff)

#------------------------------- Fit a RW model / use diff
model_wn <- arima(random_walk_diff, order = c(0, 0, 0))
#------------------------------- Coefficients - est. time trend (intercept)
int_wn <- model_wn$coef
#------------------------------- Plot original random_walk data and trend-line 
layout(1:1)
ts.plot(random_walk)
abline(0,int_wn)

#=============================== Random walk with a drift (add a mean)
#------------------------------- 
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = .3)
#------------------------------- Differencing  (t, t-1)
rw_drift_diff <- diff(rw_drift)
layout(1:2)
ts.plot(rw_drift)
ts.plot(rw_drift_diff )

#------------------------------- Fit a RW model / use diff
model2_wn <- arima(rw_drift_diff, order = c(0, 0, 0))
#------------------------------- Coefficients - estimated time trend (intercept)
int_wn2 <- model2_wn$coef
#------------------------------- Plot original random_walk data and trend-line (estimate)
ts.plot(rw_drift)
abline(0,int_wn2)

#=============================== Compare WN, RW, WN_drift, RW_drit
# white noise, wn with a drift, random walk, random walk with a drift

wn <- arima.sim(list(order = c(0, 0, 0)), n = 100)
rw <- cumsum(wn)
wn_drft <- arima.sim(list(order = c(0, 0, 0)), n = 100, mean = 5)
rw_drft <- cumsum(wn_drft)  

plot.ts(cbind(wn, rw, wn_drft, rw_drft))

#=============================== Random walk without arima.sim
#------------------------------- (Mutyalama & Huber, 2018)
set.seed(14) 
x=NULL
x[1]=0
for (i in 2:100) {
  x[i] = x[i-1] + rnorm(1,0,1)
}
ts.plot(x, main = 'Random walk 1(Xt)', xlab = 'Time', ylab = '', col='blue', lwd = 2)
plot(diff(x), type='l')

#------------------------------- use cumsum
ts.plot(cumsum(rnorm(100)))


#================================================================ 
# Module 5 notes
#================================================================

#=============================== covariance, correlation 
rm(list=ls())

#----------------------------------------- sample covariance
sum((x - mean(x))*(y - mean(y))) / (n - 1)
#----------------------------------------- population covariance
mean((x - mean(x)) * (y - mean(y)))
#------------------------------- cov() function
cov(x, y)
#------------------------------- cor() function
cor(x, y)

#------------------------------- Example
data(faithful)
f <- faithful
x = f$eruptions
y = f$waiting
n = length(f$eruptions)

#=============================== Covariance
#------------------------------- sample covariance
sum((x - mean(x))*(y - mean(y))) / (n - 1)
#------------------------------- population covariance
mean((x - mean(x)) * (y - mean(y)))
#------------------------------- cov() function
cov(x, y)
cov(f$eruptions,f$waiting)

#=============================== Correlation 

#------------------------------- standardized covariance
cov(x,y) / (sd(x)*sd(y))
cov(f$eruptions,f$waiting) / (sd(f$eruptions)*sd(f$waiting))

#------------------------------- cor() function
cor(x, y)
cor(f$eruptions,f$waiting)

plot(f$eruptions,f$waiting)
abline(v=mean(mean(f$eruptions)), col=4)
abline(h=mean(mean(f$waiting)), col=3)

#=============================== acf
acf(x, plot = FALSE)
acf(x, plot = TRUE)


#=============================== Accounting for adjustments in time periods
#------------------------------- (Metcalfe & Cowpertwait, 2009, 38)
rm(list=ls())
data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])


#-- standard deviation of the original series from July until June
sd(AP[7:138])

#-- the standard deviation of the series after subtracting the trend estimate 
sd(AP[7:138] - AP.decom$trend[7:138])

#-- the standard deviation after seasonal adjustment.
sd(AP.decom$random[7:138])


#================================================================ 
# Module 6 notes
#================================================================

#=============================== plot data, residuals, fitted data 
rm(list=ls())

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = 100) #--naive forecast
#------------------------------- fit arima model
AR_1 <- arima(x, order = c(1, 0, 0))
#------------------------------- subtract residuals to find fitted line
AR_1_fitted <- x - residuals(AR_1)
#------------------------------- plot
ts.plot(x)
points(AR_1_fitted, type = "l", col = "red", lty = 2)

#------------------------------- Example (Matteson, 2019) TS Series
#install.packages("Ecdat")

data(Mishkin, package = "Ecdat")
interest <- as.ts(Mishkin[, 4])
ts.plot(interest)
acf(interest)
AR_interest <- arima(interest, order = c(1, 0, 0))
print(AR_interest)

par(mar=c(3,3,3,2)) #-- Bottom, Left, Top, Right
par(mfrow=c(1,1))
ts.plot(interest)
AR_interest_fitted <- interest - residuals(AR_interest)
points(AR_interest_fitted, type = "l", col = "red", lty = 2)


#=============================== create xts object (Ryan, 2019; Willems, 2017)
library(xts)
rm(list=ls())

data  <- rnorm(10)
dates <- seq(as.Date("2017-05-01"),length=10,by="days")
xts1  <- xts(x=data, order.by=dates)

#=============================== Deconstructing xts objects

#------------------------------- extract core data again
xts1_core <- coredata(xts1)
#------------------------------- class of core
class(xts1)
#------------------------------- extract index
xts1_index <- index(xts1)
#------------------------------- class of index
class(xts1_index)

#=============================== Example
rm(list=ls())
data(Mishkin, package = "Ecdat")
interest <- as.ts(Mishkin[, 4])
#------------------------------- create xts object 
interest_xts <- as.xts(interest)
summary(interest_xts)

#------------------------------- explore xts object (Ryan, 2019) 
nmonths(interest_xts) 		   #-- nbr months 
nyears(interest_xts)  		   #-- nbr years
start(interest_xts) 		   #-- start
end(interest_xts)  		   #-- end

indexFormat(interest_xts) <- "%m/%d/%y"  #--- index format


#================================================================ 
# Module 7 notes
#================================================================

#=============================== Linear Regression

rm(list=ls())
library("astsa")

data("chicken")
summary(fit <- lm(chicken~time(chicken), na.action=NULL))
summary(aov(fit))
par(mfrow=c(1,1)) # plot the data
par(mar=c(5,3,2,2)) #-- Bottom, Left, Top, Right
plot(chicken, ylab="cents per pound")
abline(fit)

#------------------------------- Samples (Shumway & Stoffer, 2017, p46,57-58)
fit = lm(chicken~time(chicken), na.action=NULL) # regress chicken on time

#------------------------------- plot chicked data, fit, residuals, diff
par(mfrow=c(3,1))
plot(chicken)
abline(fit)
plot(resid(fit), type="l", main="detrended")
abline(h=0)
plot(diff(chicken), type="l", main="first difference")
abline(h=0)

dev.new()
#------------------------------- plot acf()
par(mfrow=c(3,1)) # plot ACFs
acf(chicken, 48, main="chicken")
acf(resid(fit), 48, main="detrended")
acf(diff(chicken), 48, main="first difference")

#------------------------------- Samples (Shumway & Stoffer, 2017, p53)
data("unemp")
summary(fit <- lm(unemp~time(unemp), na.action=NULL))
plot(unemp, ylab="Monthly unemployment rate")
abline(fit)
data("cmort")
data("tempr")
data("part")

par(mar=c(1,3,3,2)) #-- Bottom, Left, Top, Right
par(mfrow=c(3,1)) # plot the data
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")

temp = tempr-mean(tempr) # center temperature
temp2 = temp^2
trend = time(cmort) # time


#------------------------------- plots
dev.new() # open a new graphic device
ts.plot(cmort,tempr,part, col=1:3) # all on same plot (not shown)

dev.new()
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))

#------------------------------- plot basic fit
par(mfrow=c(1,1))
plot(cmort)
fit = lm(cmort~ trend, na.action=NULL)
abline(fit)

#------------------------------- analysis of different models (use lowest R squared value)

fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit)/num - log(2*pi) # AIC
BIC(fit)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc

fit2 = lm(cmort~ trend + temp + temp2, na.action=NULL)
summary(fit2) # regression results
summary(aov(fit2)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit2)/num - log(2*pi) # AIC
BIC(fit2)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit2)^2)/num) + (num+5)/(num-5-2)) # AICc

fit3 = lm(cmort~ trend, na.action=NULL)
summary(fit3) # regression results
summary(aov(fit3)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit3)/num - log(2*pi) # AIC
BIC(fit3)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit3)^2)/num) + (num+5)/(num-5-2)) # AICc


#------------------------------- Samples - Econ Data (Scrivner, 2019)
rm(list=ls())
data(econ5)
head(econ5)

#------------------------------- plots
par(mar=c(1,3,3,2)) #-- Bottom, Left, Top, Right
par(mfrow=c(3,1)) # plot the data
plot(econ5$unemp, main="Unemployment", xlab="", ylab="")
plot(econ5$gnp, main="GNP", xlab="", ylab="")
plot(econ5$consum, main="Consum", xlab="", ylab="")

dev.new()
pairs(cbind(Unemployment=econ5$unemp, GNP=econ5$gnp, Consum=econ5$consum))

econdata <- ts(econ5, start=c(1948, 3), end=c(1988, 2), frequency=4)
trend = time(econdata)
gnp <- econdata[,2]
consum <- econdata[,3]
response <- econdata[,1]
govinv<- econdata[,4]

fit4 = lm(response~ trend + gnp + consum, na.action=NULL)
summary(fit4) # regression results
summary(aov(fit4)) # ANOVA table (compare to next line)
summary(aov(lm(response~cbind(trend, gnp, consum)))) # Table 2.1
num = length(response) # sample size
AIC(fit4)/num - log(2*pi) # AIC
BIC(fit4)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit4)^2)/num) + (num+5)/(num-5-2)) # AICc


fit5 = lm(response~ trend + gnp + govinv, na.action=NULL)
summary(fit5) # regression results
summary(aov(fit5)) # ANOVA table (compare to next line)
summary(aov(lm(response~cbind(trend, gnp, govinv)))) # Table 2.1
num = length(response) # sample size
AIC(fit5)/num - log(2*pi) # AIC
BIC(fit5)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit5)^2)/num) + (num+5)/(num-5-2)) # AICc

#------------------------------- plots
dev.new()
pairs(cbind(Unemployment=econ5$unemp, GNP=econ5$gnp, GovInvest=econ5$govinv))


#------------------------------- Sample Linear Regression (Cryer & Chan, 2008)
rwalk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
model1=lm(rwalk~time(rwalk))
summary(model1)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(rwalk,type='l',ylab='y')
abline(model1) 

#------------------------------- residuals analysis
par(mfrow=c(1,1)) 
plot(resid(model1),type='l',ylab='y')
hist(resid(model1),xlab='Standardized Residuals')
qqnorm(resid(model1));qqline(resid(model1))
acf(resid(model1))


#================================================================ 
# ARIMA notes
#================================================================

library('astsa')
library('tseries')
#------------------------------- Clear data
rm(list=ls())

#=============================== Stationarity (Stoffer, 2019)

par(mar=c(1,1,1,1))  #-- Bottom, Left, Top, Right
layout(1:3)
#------------------------------- plot
plot(gnp)
#------------------------------- detrend data: diff()
plot(diff(gnp))
#------------------------------- growth rate: diff(log())
plot(diff(log(gnp)))

#=============================== Sample Arima Models (Stoffer, 2019)

#------------------------------- Simulate white noise
rm(list=ls())
x <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
plot(x)
x_fit <- sarima(x,0,0,0)
x_fit$ttable

x_fit <- sarima(x,2,0,0)  #-- compare
x_fit$ttable
#------------------------------- Simulate AR(2)
rm(list=ls())
x <- arima.sim(list(order = c(2, 0,  0), ar = c(0, -0.9)), n = 100)
plot(x)
#-------------------------------  Plot P/ACF
acf2(x)
y_fit <- sarima(x,2,0,0)
y_fit$ttable

#------------------------------- Simulate MA(1)
rm(list=ls())
x <- arima.sim(list(order = c(0, 0, 1), ma = 0.9), n = 100)
plot(x)
#-------------------------------  Plot P/ACF
acf2(x)
y_fit <- sarima(x, 0, 0, 1)
y_fit$ttable

#----  / AR(p)     /   MA(q)   / ARMA(p,q)
# ACF  / Tails off / Cuts off  / Tails off
# PACF / Cuts off  / Tails off / Tails off

par(mar=c(1,1,1,1))  #-- Bottom, Left, Top, Right
layout(1:1)
#------------------------------- ARMA(1, 1)
x <- arima.sim(list(order = c(1, 0, 1),
                    ar = .9,
                    ma = -.4),
               n = 200)
plot(x, main = "ARMA(1, 1)")
acf2(x)
#------------------------------- Model Estimation(Fit)
x_fit <- sarima(x, 1, 0, 1)
x_fit$ttable

#------------------------------- Model Selection (compare AIC/BIC)
gnpgr <- diff(log(gnp))
sarima(gnpgr, 1,0,0)  # -- AR(1)
sarima(gnpgr, 0,0,2)  # -- MA(2)

#------------------------------- ARIMA(1,1,0)
x <- arima.sim(list(order = c(1, 1, 0),
                    ar = .9), n = 200)

#------------------------------- 
oil <- window(astsa::oil, end = 2006)
sarima(oil,1,1,0)
sarima(oil,0,1,2)

#------------------------------- forecasting
oil <- window(astsa::oil, end =  2006)
oilf <- window(astsa::oil, end =  2007)
sarima.for(oil, n.ahead = 52, 1,  1, 1)
lines(oilf)

#------------------------------- Seasonal forecasting

#----  / SAR(p)    /  SMA(q)   / SARMA(p,q)
# ACF  / Tails off / Cuts off  / Tails off
# PACF / Cuts off  / Tails off / Tails off

#------------------------------- Pure Seasonal Model 
sarima(x, 0,0,0, P = 1, D = 0, Q = 1, S = 12)

#------------------------------- Mixed Model 
sarima(x, 0,0,1, 1,0,0, 12)

#------------------------------- Sample 
data(AirPassengers)
AP <- AirPassengers
head(AP)

airpass_fit1 <-  sarima(log(AP), 1,1,1, 0,1,1, 12)
airpass_fit1$ttable


#=============================== Mixed Seasonal Models 
rm(list=ls())
#------------------------------- Plot unemp
plot(unemp)
#------------------------------- Difference your data and plot it
d_unemp <- diff(unemp)
plot(d_unemp)
#------------------------------- Seasonally difference d_unemp and plot it
dd_unemp <- diff(d_unemp, lag = 12)  
plot(dd_unemp)
#------------------------------- P/ACF pair of fully differenced data to lag 60
acf2(dd_unemp, max.lag=60)
#------------------------------- Fit an appropriate model
sarima(unemp,
       p = 2,d = 1, q = 0,
       P = 0, D = 1, Q = 1, S = 12)

#------------------------------- Mixed Seasonal Model 
sarima(chicken,2,1,0,1,0,0,12)

#------------------------------- five years Mixed Seasonal Model 
sarima.for(chicken, n.ahead = 5, 2,1,0, 1,0,0, 12)


#================================================================ 
# Module 11 notes
#================================================================
#------------------------------- Notes from (Metcalfe & Cowpertwait, 2009)
rm(list=ls())

#------------------------------- Generate white noise 
wn_1 <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)
plot(wn_1)

set.seed(1)
x <- arima.sim(n = 1000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1)))
plot(x)


#=============================== Model assessment from AIC
#------------------------------- (Example from Metcalfe & Cowpertwait, 2009, 131)

rm(list=ls())
#www <- "http://www.massey.ac.nz/~pscowper/ts/pounds_nz.dat"
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/pounds_nz.dat"
x <- read.table(www, header = T)

x.ts <- ts(x, st = 1991, fr = 4)
plot(x.ts)
x.ma <- arima(x.ts, order = c(0, 0, 1))
x.ar <- arima(x.ts, order = c(1, 0, 0))
x.arma <- arima(x.ts, order = c(1, 0, 1))
AIC(x.ma)
AIC(x.ar)
AIC(x.arma)
x.arma
acf(resid(x.arma))

#=============================== Model assessment using functions

rm(list=ls())
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www, header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

plot(Elec.ts)
plot(diff(Elec.ts))
plot(log(Elec.ts))

Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth))
acf(resid(Elec.lm))

#------------------------------- find best order (ARMA)
#------------------------------- (Metcalfe & Cowpertwait, 2009, 131)
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(Elec.lm), order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(Elec.lm), order = best.order)
    best.aic <- fit.aic
  }
}
best.aic 
best.order
acf(resid(best.arma))

#--- find best order (ARIMA)- See Metcalfe & Cowpertwait, 2009, page 145)


#================================================================ 
# Module 12 notes - Forcecasting (Hyndman, 2019)
#================================================================

library(ggplot2) 
library(ggfortify)
library(forecast)
library(fpp2)
library(astsa)
#------------------------------- Clear data
rm(list=ls())
www <- "http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat"
CBE <- read.table(www, header = T)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
layout(1:1)
plot(Choc.ts)

autoplot(Choc.ts)
fc <- naive(Choc.ts)
plot(fc)
autoplot(residuals(fc)) 
checkresiduals(fc)

frequency(Choc.ts)
#------------------------------- train/test data 
train <- window(Choc.ts, end = c(1980,12))
test <- window(Choc.ts, start = c(1981,1))
#------------------------------- naive model
fc_train <- naive(train, h=10) 
#------------------------------- model accuracy
accuracy(fc_train, Choc.ts)
autoplot(fc) + autolayer(test, series = "Test data")





#------------------------------- Example (Hyndman, 2019)
rm(list=ls())

# --- from https://fred.stlouisfed.org/series/USNIM  [use a filter for 2002, download]
dat<- read.csv("C:/Users/hp/OneDrive/db/j5/IU/ts/modules/USNIM_2002.csv"
               , sep=",", header=TRUE)

# ts(data, start, frequency, ...)  --- c(Year, Qtr#)
usnim_2002 = ts(dat, start = c(2002, 1), frequency = 4)
usnim_2002[,2]

#-------------------------- ts plot
autoplot(a10)
autoplot(ausbeer)
#-------------------------- season plot
ggseasonplot(a10)  
#-------------------------- polar season plot
ggseasonplot(a10, polar = TRUE)

#-------------------------- subset - use window(x,Start,End)
# Restrict data to start in 1992 
beer = window(ausbeer,1992,NULL)
autoplot(beer)
# -------------------------- subseries plot
ggsubseriesplot(beer)



# =========================================== References

# Cornelissen, J. (2019). Introduction to R Online Course. Retrieved from https://www.datacamp.com/courses/free-introduction-to-r

# Cryer, J., & Chan, K. (2008). Time series analysis (2nd ed.). New York: Springer Science+Business Media, LLC.

# Hyndman, R. (2019). Forecasting Using R. Retrieved from https://www.datacamp.com/courses/forecasting-using-r

# Matteson, D. (2019). Introduction to Time Series Analysis. Retrieved from https://www.datacamp.com/courses/introduction-to-time-series-analysis

# Metcalfe, A., & Cowpertwait, P. (2009). Introductory Time Series with R (1st ed.). New York, NY: Springer-Verlag New York.

# Mutyalama, & Huber, W. (2018). How to generate uncorrelated white noise sequence in R without using arima.sim?. Retrieved from https://stats.stackexchange.com/questions/152461/how-to-generate-uncorrelated-white-noise-sequence-in-r-without-using-arima-sim

# Net Interest Margin for all U.S. Banks. (2018). Retrieved from https://fred.stlouisfed.org/series/USNIM

# Ryan, J. (2019). Time Series in R. Retrieved from https://www.datacamp.com/courses/manipulating-time-series-data-in-r-with-xts-zoo

# Scrivner, O. (2019). Linear Regression Model. Retrieved from https://iu.instructure.com/courses/1774378/pages/week-7-practice?module_item_id=18424073

# Shumway, R., & Stoffer, D. (2017). Time series analysis and its applications (4th ed.). Cham: Springer.

# Stoffer, D. (2019). R: Arima Modeling. Retrieved from https://www.datacamp.com/courses/arima-modeling-with-r

# Willems, K. (2017). xts Cheat Sheet: Time Series in R. Retrieved from https://www.datacamp.com/community/blog/r-xts-cheat-sheet



