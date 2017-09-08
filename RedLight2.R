mean1.ts <- ts(as.vector(tickets.averages$mean)[1:84], start = c(2007, 1), frequency = 12)
adf.test(mean1.ts)
plot(mean1.ts)
par(mfrow = c(1,2))
Acf(mean1.ts, lag.max = 50, main = "")
Pacf(mean1.ts, lag.max = 50, main = "")
mean.decom <- stl(mean1.ts, s.window = "periodic")
plot(mean.decom)

plot(diff(mean1.ts))
plot(diff(monthly.ts, lag = 12, differences = 1))
plot(diff(diff(monthly.ts, lag = 12, differences = 1)))

adf.test(diff(mean1.ts))
Acf(diff(mean1.ts), lag.max = 50, main = "")
Pacf(diff(mean1.ts), lag.max = 50, main = "")
plot(stl(diff(mean1.ts), s.window = "periodic"))

mean.manual.fit <- Arima(mean1.ts, order = c(1, 0, 0), method = "ML")
mean.manual.fit
tsdisplay(residuals(mean.manual.fit), lag.max = 50)
mean.manual.fit <- Arima(mean1.ts, order = c(1, 0, 0), seasonal = c(1, 0, 0), method = "ML")
mean.manual.fit
tsdisplay(residuals(mean.manual.fit), lag.max = 50)
mean.manual.fit <- Arima(mean1.ts, order = c(1, 0, 0), seasonal = c(2, 0, 0), method = "ML")
mean.manual.fit
tsdisplay(residuals(mean.manual.fit), lag.max = 50)
plot(forecast(mean.manual.fit, h = 36))



mean.diff.fit <- Arima(mean1.ts, order = c(0, 1, 1), seasonal = c(2, 0, 0), method = "ML")
mean.diff.fit
tsdisplay(residuals(mean.diff.fit), lag.max = 50)
plot(forecast(mean.diff.fit, h = 36))
mean.diff.lines <- as.data.frame(fitted(mean.diff.fit))
mean.diff.lines$fit <- as.numeric(fitted(mean.diff.fit))
mean.diff.lines$original <- as.vector(mean.diff.fit$x)
colnames(mean.diff.lines) <- c("fit", "orig")
mean.diff.lines$month.index <- 1:84
mean.diff.lines$aa.fit <- as.vector(fitted(mean.aa.fit))
ggplot(data = mean.diff.lines) + 
  geom_line(mapping = aes(x = month.index, y = orig, color = "1. Original")) +
  geom_line(mapping = aes(x = month.index, y = fit, color = "2. Manual Fit")) +
  geom_line(mapping = aes(x = month.index, y = aa.fit, color = "3. Auto Arima Fit")) +
  labs(x = "Month Index", y = "Mean Number of Tickets") +
  guides(color = guide_legend("Legend")) +
  theme_bw()
mean.diff.lines$NA <- NA

mean.aa.fit <- auto.arima(mean1.ts, seasonal = TRUE)
mean.aa.fit
tsdisplay(residuals(mean.aa.fit), lag.max = 50)
plot(forecast(mean.aa.fit, h = 36), main = "", xlab = "Year", ylab = "Tickets")

mean.diff.fit
mean.aa.fit
