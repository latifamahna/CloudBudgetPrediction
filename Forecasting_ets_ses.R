#### My CODE BELOW ####

# Keep data of host about cpu.user for one of the snapshots
historical.data.host.cpu.user <- historical.data[historical.data$metric.name == "cpu.user" & historical.data$type.of.snapshot == "host" & historical.data$snapshot.id == "tpVsMf2dm8NIjEdl_KMcMkds00Q", ] 

# Setting up the R environment
install.packages("xts")
install.packages("forecast")
library(xts)
library(forecast)

### Forecast method: Simple exponential smoothing ses (tried ets too)
historical.data.host.cpu.user.sliced <- historical.data.host.cpu.user[c("timestamp","value")]
#as.POSIXct(historical.data.host.cpu.user.sliced$timestamp/1000, origin = "1970-01-01")
historical.data.host.cpu.user.sliced[historical.data.host.cpu.user.sliced$timestamp <- as.POSIXct(historical.data.host.cpu.user.sliced$timestamp/1000, origin = "1970-01-01")]
historical.data.host.cpu.user.sliced
historical.data.host.cpu.user.sliced.ts <- xts(historical.data.host.cpu.user.sliced$value, order.by = historical.data.host.cpu.user.sliced$timestamp)
# Fitting an ets model
host.cpu.user.ets <- ets(historical.data.host.cpu.user.sliced.ts)
summary(host.cpu.user.ets)
# Fitting an ses model
host.cpu.user.ses <- ses(historical.data.host.cpu.user.sliced.ts, h=3)
summary(host.cpu.user.ses)
# Plotting the ses model
plot(host.cpu.user.ses, ylab="Host CPU User", xlab="Time")
lines(fitted(host.cpu.user.ses), col="blue", type="l")
legend("topleft", c("observed","fitted"), lty=1, col=c(1,4))

