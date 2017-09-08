install.packages("autobox_64.0.1.zip", repos = NULL)
library(autobox)
autoboxInitPackage() 
help(autobox)

write.csv(tickets0913, "RLCTickets.csv")

### Reading tickets.csv ###
tickets <- read.csv("tickets.csv")

### Aggregating tickets by month ###
tickets.month <- tickets %>%
  group_by(month.index) %>%
  summarize(
    count = n()
  )
write.csv(tickets.month, "ticketsmonth.csv")

tickets.month <- read.csv("ticketsmonth.csv")
tickets.month$count <- tickets.month$count / 10000
tickets.month <- filter(tickets.month, month.index < 85)
monthly.ts <- ts(tickets.month$count, start = c(2007, 1), frequency = 12)

ggplot(data = tickets.month) + geom_line(mapping = aes(x = month.index, y = count)) +
  labs(x = "Month Index", y = "Tickets (in 10,000s)") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

### Create a matrix ###
monthly.mat <- matrix(tickets.month$count, ncol = 1, dimnames = list(c(1:length(tickets.month$count)), c("RLC")))
monthly.mat

### Creating month time series ###
monthly.ts <- ts(monthly.mat, start = c(2007, 1), frequency = 12)
monthly.ts <- window(monthly.ts, c(2007, 1), c(2013, 12))
monthly.ts

mnth.ts <- ts(tickets.month$count, start = c(2007, 1), frequency = 12)
mnth.ts

  ### Closing window to 2010 - 2014 ###
  monthly2010.ts <- window(monthly.ts, c(2009, 1), c(2013, 12))
  monthly2010.ts  

### Creating Autobox object ###
monthly.auto <- autobox(monthly.ts, iDataType = c(0), iObjective = c(0, 0, 0),
                        iNumberOfRetainedValues = 0, iNumberOfForecastValues = 24, cPath=".\\RLCOutput")
monthly2010.auto <- autobox(monthly2010.ts, iDataType = c(0), iObjective = c(0, 0, 0),
                            iNumberOfRetainedValues = 0, iNumberOfForecastValues = 36, cPath=".\\RLCOutput\\ts2010")
monthly.auto

### Returning Autobox calculation ###
monthly.auto.cal <- autoboxRun(monthly.auto)
autoboxPrint(monthly.auto.cal)
autoboxPlot(monthly.auto.cal, selection = "aft", displayLegend = FALSE)
monthly.auto.cal$cFinalEquation

###Autobox calc for 2010-2014 ###
monthly2010.auto.cal <- autoboxRun(monthly2010.auto)
autoboxPrint(monthly2010.auto.cal)
autoboxPlot(monthly2010.auto.cal, selection="afhinrtu", displayLegend = FALSE)


### Auto.arima forecast ###
aa.fit <- auto.arima(monthly.ts, seasonal = TRUE)
aa.fit
manual.fit
tsdisplay(residuals(aa.fit), lag.max = 50)
plot(forecast(aa.fit, h = 36))
plot(forecast(manual.fit, h = 36))
par(mfrow = c(1, 1))

aa2010.fit <- auto.arima(monthly2010.ts, seasonal = TRUE)
aa2010.fit
tsdisplay(residuals(aa2010.fit), lag.max = 50)
plot(forecast(aa20102.fit, h = 36))
Box.test(residuals(aa2010.fit), lag = 24, fitdf = 1, type = "Ljung")

tsdisplay(residuals(aa20102.fit))
aa2010.fit$coef




### Auto.arima forecast 2 ###
aa20102.fit <- auto.arima(monthly2010.ts, seasonal = TRUE, stepwise = FALSE, d = 1, D = 1)
aa20102.fit
tsdisplay(residuals(aa20102.fit), lag.max = 50)

### Auto.arima forecast 3 ###
aa20103.fit <- auto.arima(monthly2010.ts, seasonal = TRUE, d = 1, D = 1, max.P = 0, max.Q = 1, start.Q = 1)
aa20103.fit

### Import ChiPop, taken from ZipAtlas ###
ChiPop <- read.csv("ChiPop.csv")
ChiPop
ChiPop <- rename(ChiPop, People.Sq.Mile = People...Sq..Mile)
ChiPop <- ChiPop %>% select(-X.)


### Test ###
# generate a single example address
lonlat_sample <- as.numeric(geocode("the hollyood bowl"))
lonlat_sample  # note the order is longitude, latitiude

res <- revgeocode(lonlat_sample, output="more")
# can then access zip and neighborhood where populated
res$postal_code
res$neighborhood

## Export ChiPop ##
write.csv(ChiPop, "ChiPop1.csv")

### Running Autobox on quad data ###
## Importing and separating data ##
quads <- read.csv("Quads.csv")
quads$adjcount <- quads$count / 1000

quad.NE <- filter(quads, quad == "NE")
quad.NW <- filter(quads, quad == "NW")
quad.SW <- filter(quads, quad == "SW")
quad.SE <- filter(quads, quad == "SE")

## Creating matrixes ##
matrix.NE <- matrix(quad.NE$adjcount, ncol = 1, dimnames = list(c(1:length(quad.NE$adjcount)), c("quad.NE")))
matrix.NW <- matrix(quad.NW$adjcount, ncol = 1, dimnames = list(c(1:length(quad.NW$adjcount)), c("quad.NW")))
matrix.SW <- matrix(quad.SW$adjcount, ncol = 1, dimnames = list(c(1:length(quad.SW$adjcount)), c("quad.SW")))
matrix.SE <- matrix(quad.SE$adjcount, ncol = 1, dimnames = list(c(1:length(quad.SE$adjcount)), c("quad.SE")))

### Creating month time series ###
ts.NE <- ts(matrix.NE, start = c(2007, 1), frequency = 12)
ts.NW <- ts(matrix.NW, start = c(2007, 1), frequency = 12)
ts.SW <- ts(matrix.SW, start = c(2007, 1), frequency = 12)
ts.SE <- ts(matrix.SE, start = c(2007, 1), frequency = 12)

ts.NE <- window(ts.NE, c(2009, 1), c(2013, 12))
ts.NW <- window(ts.NW, c(2009, 1), c(2013, 12))
ts.SW <- window(ts.SW, c(2009, 1), c(2013, 12))
ts.SE <- window(ts.SE, c(2009, 1), c(2013, 12))

### Creating Autobox objects ###
auto.NE <- autobox(ts.NE, iDataType = c(0), iObjective = c(0, 0, 0),
                   iNumberOfRetainedValues = 0, iNumberOfForecastValues = 36, cPath=".\\RLCOutput\\NE09")
auto.NW <- autobox(ts.NW, iDataType = c(0), iObjective = c(0, 0, 0),
                   iNumberOfRetainedValues = 0, iNumberOfForecastValues = 36, cPath=".\\RLCOutput\\NW09")
auto.SW <- autobox(ts.SW, iDataType = c(0), iObjective = c(0, 0, 0),
                   iNumberOfRetainedValues = 0, iNumberOfForecastValues = 36, cPath=".\\RLCOutput\\SW09")
auto.SE <- autobox(ts.SE, iDataType = c(0), iObjective = c(0, 0, 0),
                   iNumberOfRetainedValues = 0, iNumberOfForecastValues = 36, cPath=".\\RLCOutput\\SE09")

### Running Autobox calculation ###
autocal.NE <- autoboxRun(auto.NE)
autoboxPrint(autocal.NE)
autoboxPlot(autocal.NE, selection = "afhinrtu", displayLegend = FALSE)

autocal.NW <- autoboxRun(auto.NW)
autoboxPlot(autocal.NW, selection = "afhinrtu", displayLegend = FALSE)

autocal.SW <- autoboxRun(auto.SW)
autoboxPlot(autocal.SW, selection = "afhinrtu", displayLegend = FALSE)

autocal.SE <- autoboxRun(auto.SE)
autoboxPlot(autocal.SE, selection = "afhinrtu", displayLegend = FALSE)

### Importing quadcams.csv ###
quad.cams <- read.csv("quadcams.csv")

### Calculating average tickets ###
quads <- merge(quads, quad.cams, by = c("quad", "month.index"))
colnames(quads)[3] <- "tixcount"
colnames(quads)[6] <- "camcount"
quads$avgcount <- quads$tixcount / quads$camcount
quads <- quads[order(quads$quad, quads$month.index),]

### Plot averages ###
ggplot(data = quads) + geom_line(mapping = aes(x = month.index, y = avgcount, group = quad, color = quad))
ggplot(data = quads) + geom_line(mapping = aes(x = month.index, y = tixcount, group = quad, color = quad))
ggplot(data = quads) + geom_line(mapping = aes(x = month.index, y = camcount, group = quad, color = quad))





### March 7, 2017 ###

### Plotting raw data ###
ggplot(data = tickets.month) +
  geom_line(mapping = aes(x = month.index, y = count))

### Plotting camera data ###
cams.month <- quads %>%
  group_by(month.index) %>%
  summarize(
    cams = sum(camcount)
  )
ggplot(data = cams.month) +
  geom_line(mapping = aes(x = month.index, y = cams)) + 
  geom_vline(xintercept = c(12, 24, 36, 48, 60, 72, 84), linetype = 2)

### Plotting 2009 - 2013 ###
tickets0913 <- filter(tickets.month, month.index > 24, month.index < 85)

ggplot(data = tickets0913) +
  geom_line(mapping = aes(x = month.index, y = count))

### ACF and PACF of 2009 - 2013 ###

Acf(monthly2010.ts)
Pacf(monthly2010.ts)

### Finding best model ###
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(monthly2010.ts, order = c(i, 0, j), method = "ML"))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(monthly2010.ts, order = best.order, method = "ML")
    best.aic <- fit.aic
  }
}

best.arma

### Plotting forecast ###
new.time <- seq(length(monthly2010.ts), length = 36)
predict.arma <- predict(best.arma, n.ahead = 36)
monthly.pred <- ts((predict.arma$pred), start = 2014, freq = 12)
manual.ts <- ts.union(monthly2010.ts, monthly.pred, dframe = FALSE)
manual.ts <- pmax(manual.ts[,1], manual.ts[,2], na.rm = TRUE)
autoplot(as.zoo(manual.ts), geom = c("line", "point"))




### Season differencing ###
diff.ts <-diff(monthly.ts, differences = 1)
diff2.ts <- diff(monthly.ts, lag = 12, differences = 1)
diff3.ts <- diff(diff(monthly.ts, lag = 12))

rm(seasonal3.ts)

plot(seasonal3.ts)
plot(seasonal2.ts)
plot(seasonal.ts)

Acf(seasonal2.ts, lag.max = 50)
Pacf(seasonal2.ts, lag.max = 50)

### Plotting all four time series ###
par(mfrow = c(1,1))
p1 <- autoplot(as.zoo(monthly.ts), geom = "line") + theme_bw() + theme(axis.title = element_text(size = 12), axis.title.x = element_blank()) + labs(y = "No differencing", title = "(1)")
p2 <- autoplot(as.zoo(diff.ts), geom = "line") + theme_bw() + theme(axis.title = element_text(size = 12), axis.title.x = element_blank()) + labs(y = "1st diff", title = "(2)")
p3 <- autoplot(as.zoo(diff2.ts), geom = "line") + theme_bw() + theme(axis.title = element_text(size = 12), axis.title.x = element_blank()) + labs(y = "1st seasonal diff", title = "(3)")
p4 <- autoplot(as.zoo(diff3.ts), geom = "line") + theme_bw() + theme(axis.title.y = element_text(size = 10), axis.title.x = element_blank()) + labs(y = "1st diff and 1st seasonal diff", title = "(4)")
                                                                                                                                                    

multiplot(p1, p3, p2, p4, cols = 2)

plot(monthly2010.ts)
plot(seasonal3.ts)
plot(seasonal.ts)
plot(seasonal2.ts)

par(mfrow = c(1, 2))
Acf(diff.ts, lag.max = 70, main = "")
Pacf(diff.ts, lag.max = 70, main = "")

Acf(monthly.ts)

Acf(diff3.ts, lag.max = 50)
Pacf(diff3.ts, lag.max = 50)

tsdisplay(diff.ts)

best.order <- c(0, 1, 0)
best.seasonal <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(Arima(monthly.ts, order = c(2, 1, 2), seasonal = c(i, 0, j), method = "ML"))
  if (fit.aic < best.aic) {
    best.seasonal <- c(i, 0, j)
    best.arma <- Arima(monthly.ts, order = c(2, 1, 2), seasonal = c(i, 0, j), method = "ML")
    best.aic <- fit.aic
  }
}
 
best.order1 <- c(0, 0, 0)
best.seasonal1 <- c(0, 0, 0)
best.aic1 <- Inf
for (i in 0:2) for (j in 0:2) for (x in 0:2) for (y in 0:2) {
  fit.aic <- AIC(Arima(monthly.ts, order = c(i, 1, j), seasonal = c(x, 0, y), method = "ML"))
  if (fit.aic < best.aic) {
    best.order1 <- c(i, 1, j)
    best.seasonal1 <- c(x, 0, y)
    best.arma1 <- Arima(monthly.ts, order = c(i, 1, j), seasonal = c(x, 0, y), method = "ML")
    best.aic1 <- fit.aic
  }
}



best.arma

adf.test(diff(monthly.ts))

manual1.fit <- Arima(monthly.ts, order = c(1, 1, 0), seasonal = c(4, 0, 0), method = "ML")
plot(forecast(manual.fit, h = 36))
manual.fit <- Arima(monthly.ts, order = c(0, 1, 0), seasonal = c(4, 0, 0), method = "ML")
manual.fit
manual1.fit
summary(manual.fit)
tsdisplay(residuals(manual.fit), lag.max = 50, main = "(0, 1, 0)x(4, 0, 0)[12] Model Residuals")

tsdisplay(residuals(manual1.fit), lag.max = 50)

manual2.fit <- Arima(monthly.ts, order = c(0, 1, 0), seasonal = c(2, 0, 0), method = "ML")
summary(manual2.fit)


manual.fit <- Arima(monthly.ts, order = c(0, 1, 0), seasonal = c(0, 1, 1), method = "ML")
manual.fit
tsdisplay(residuals(manual.fit))
manual.res <- residuals(manual.fit)
tsdisplay(manual.res, lag.max = 50)
Box.test(residuals(manual.fit), lag = 24, fitdf = 1, type = "Ljung")
plot(forecast(manual.fit, h = 36))

manual2.fit <- Arima(monthly2010.ts, order = c(0, 1, 0), seasonal = c(0, 1, 1), method = "ML")
manual2.fit

manual3.fit <- arima(monthly2010.ts, order = c(0, 1, 0), seasonal = list(order = c(0, 1, 1)))
manual3.fit

manual4.fit <- Arima(monthly2010.ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))
manual4.fit

manual5.fit <- Arima(monthly2010.ts, order = c(1, 1, 1), seasonal = c(1, 1, 1))
manual5.fit

manual6.fit <- Arima(monthly2010.ts, order = c(0, 1, 0), seasonal = c(1, 0, 0))
manual6.fit

manual6.fit <- Arima(monthly2010.ts, order = c(0, 0, 0), seasonal = c(1, 0, 0))
manual6.fit

plot(forecast(manual6.fit, h = 36))
tsdisplay(residuals(manual6.fit), lag.max = 50)

tsdisplay(residuals(manual5.fit))
tsdisplay(residuals(aa2010.fit))
aa2010.fit
Box.test(residuals(manual6.fit), lag = 48, fitdf = 1, type = "Ljung")

Acf(monthly2010.ts, lag.max = 50)
Pacf(monthly2010.ts, lag.max = 50)

unique(tickets0913$)

### Testing for errors in difference-only models ###
test1.fit <- Arima(monthly2010.ts, order = c(0, 1, 0), seasonal = c(0, 1, 0))
test2.fit <- Arima(monthly2010.ts, order = c(0, 0, 0), seasonal = c(0, 1, 0))
test1.fit
test2.fit

### Reverse Engineering Autobox ###
autotest.ts <- window(monthly.ts, c(2012, 1), c(2013, 12))
autotest.fit <- Arima(autotest.ts, order = c(0, 0, 0), seasonal = c(1, 0, 0))
plot(forecast(autotest.fit, h = 60))

### Ticket averages ###
tickets.averages <- read.csv("tixavg.csv")
tickets.averages <- filter(tickets.averages, month.index < 85)

ggplot(data = tickets.averages) +
  geom_line(mapping = aes(x = month.index, y = median, linetype = "Median")) +
  geom_line(mapping = aes(x = month.index, y = mean, linetype = "Mean")) +
  labs(x = "Month Index", y = "Tickets") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

### Modeling average data ###


### Create a matrix ###
mean.mat <- matrix(tickets.averages$mean, ncol = 1, dimnames = list(c(1:length(tickets.averages$mean)), c("RLC-Mean")))
mean.mat

### Creating month time series ###
mean.ts <- ts(mean.mat, start = c(2007, 1), frequency = 12)
mean.ts <- window(mean.ts, c(2007, 1), c(2013, 12))
mean.ts

mean2.ts <- window(mean.ts, c(2009, 1), c(2013, 12))

### Creating Autobox object ###
mean.auto <- autobox(mean.ts, iDataType = c(0), iObjective = c(0, 0, 0),
                        iNumberOfRetainedValues = 0, iNumberOfForecastValues = 24, cPath=".\\MeanRLCOutput")
mean.auto

mean2.auto <- autobox(mean2.ts, iDataType = c(0), iObjective = c(0, 0, 0),
                      iNumberOfRetainedValues = 0, iNumberOfForecastValues = 24, cPath=".\\MeanRLCOutput2")

### Returning Autobox calculation ###
mean.autocal <- autoboxRun(mean.auto)
autoboxPrint(mean.autocal)
autoboxPlot(mean.autocal, selection = "aft", displayLegend = FALSE)
mean.autocal$cFinalEquation

mean2.autocal <- autoboxRun(mean2.auto)
autoboxPrint(mean2.autocal)
autoboxPlot(mean2.autocal, selection = "aft", displayLegend = FALSE)
mean.autocal$cFinalEquation

### Auto.arima forecast ###
aa.mean.fit <- auto.arima(mean.ts, seasonal = TRUE)
aa.mean.fit
plot(forecast(aa.mean.fit, h = 24))
tsdisplay(residuals(aa.mean.fit), lag.max = 50)
Box.test(residuals(aa.mean.fit), lag = 24, fitdf = 1, type = "Ljung")
aa.mean.fit$coef


fitted.lines <- as.data.frame(as.numeric(fitted(manual.fit)))
fitted.lines$orig <- as.vector(manual.fit$x)
colnames(fitted.lines) <- c("fit", "orig")
fitted.lines$month.index <- 1:84
fitted.lines$aa.fit <- as.vector(fitted(aa.fit))

t1 <- ggplot(data = fitted.lines) + 
  geom_line(mapping = aes(x = month.index, y = orig, linetype = "1. Original")) +
  geom_line(mapping = aes(x = month.index, y = fit, linetype = "2. Manual Fit")) +
  labs(x = "Month Index", y = "Tickets (in 10,000s)") +
  theme(axis.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme_bw()
t2 <- ggplot(data = fitted.lines) +
  geom_line(mapping = aes(x = month.index, y = orig, linetype = "1. Original")) +
  geom_line(mapping = aes(x = month.index, y = aa.fit, linetype = "2. Auto Arima Fit")) +
  labs(x = "Month Index", y = "Tickets (in 10,000s)") +
  theme(axis.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme_bw()

ggplot(data = fitted.lines) +
  geom_line(mapping = aes(x = month.index, y = orig, color = "1. Original")) +
  geom_line(mapping = aes(x = month.index, y = fit, color = "2. Manual Fit")) +
  geom_line(mapping = aes(x = month.index, y = aa.fit, color = "3. Auto Arima Fit")) +
  labs(x = "Month Index", y = "Tickets (in 10,000s)") +
  guides(color = guide_legend("Legend")) +
  theme_bw()

multiplot(t1, t2)

par(mfrow = c(1, 2))
plot(forecast(manual.fit, h = 36))
plot(forecast(aa.fit, h = 36), main = "", ylab = "Tickets (in 10,000s)")

manual.fit
aa.fit
tsdisplay(residuals(aa.fit), lag.max = 50)

plot(forecast(aa.fit, h = 36), main = "", ylab = "Mean Tickets")

tickets.month$year <- c(rep(1:7, each = 12))
tickets.month %>%
  group_by(year) %>%
  summarise(
    revenue = sum(count) * 10000 * 100
  )

