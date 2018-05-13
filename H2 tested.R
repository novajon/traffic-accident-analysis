source("GM assumption test.R")
library(prais)
library(tseries)
library(sandwich)
library(ggplot2)
load("traffic2.RData")
describe(data)

# assumption: wkends includes fridays

g <- ggplot(data = data, aes(x = t, y = lrtotacc, colour=(beltlaw > 0), group=1)) + geom_line() + xlab("Time Period") + ylab("PRC Fat ACC")
g + geom_smooth() + geom_smooth(method="lm")

# H2: The total number of accidents on rural 65 mph roads increased after the increase of the speed limit

#1st test: model with timetrend
model_data <- data[, c("rtotacc","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
## Test with time trend and indicating variables (without tsq)
base_model <- lm(data = model_data, rtotacc ~ .)
independent_data <- model_data[,-1]
summary(base_model)
test_base_model <- test_GM_assumptions(independent_data, base_model)
plot(base_model$residuals)
# CONCLUSION: H4, H5 violated: heteroscedasticity, serial correlation - Model coefficients are unbiased but not BLUE, statistical inference not valid
# H4 could be violated as a cause of the violation of H5 -> necessity to correct for serial correlation

#2nd test: Correcting for serial correlation using Prais-Winsten method to correct for SC 1 (Idea: correct for SC=1 and do checks again)
sc_corrected_model <- prais.winsten(rtotacc ~ ., model_data, iter = 50, rho = 0, tol = 1e-08)
test_sc_corrected_model <- test_GM_assumptions(independent_data, sc_corrected_model[[1]])
plot(sc_corrected_model[[1]]$residuals)
# comparison of standard errors: corrected nidek generally yields to higher standard errors (slightly)
sc_corrected_model[[1]]$coefficients[,2]/summary(base_model)$coefficients[,2]*100
# CONCLUSION: after correcting for SC - biased but more efficient estimators of corrected model

#3rd test: Test for unit root on the base model (alternativde to 2nd test)
# rtotacc
acf(base_model$model[,1], lag.max=20) # visual indication of non-stationarity
adf.test(model_data$rtotacc) # not enough indication of non-stationarity for prcfat for lag order 4
#unem
acf(base_model$model[,3], lag.max=20) # visual indication of non-stationarity
adf.test(model_data$unem) # not enough evidence to reject non-stationarity hypothesis
# other variables are naturally not assumed to be relevant for stationarity test
# differentiation to correct for serial correlation
differentiated_data <- model_data[2:nrow(model_data),]
differentiated_data$rtotacc <- diff(x = model_data$rtotacc)
differentiated_data$unem <- diff(x = model_data$unem)
acf(differentiated_data$unem, lag.max=20)
adf.test(differentiated_data$unem)
acf(differentiated_data$rtotacc, lag.max=20)
adf.test(differentiated_data$rtotacc)
differentiated_model <- lm(data = differentiated_data, rtotacc ~ .)
test_differentiated_model <- test_GM_assumptions(differentiated_data, differentiated_model)
#3rd test: Adjust for heteroscedascity
nw_vcovHAC <- NeweyWest(base_model, lag = 5)
homo_scCorrected_model <- coeftest(base_model, vcov. = nw_vcovHAC)
homo_scCorrected_model
# assumption of homoscedasticity (H4) and no serial correlation (H5) hold because of Newey-West robust technique

#4th test: Test for seasonality in the model
model_data_rest <- model_data[,c("rtotacc","t","unem" , "spdlaw", "beltlaw", "wkends")]
base_model_rest <- lm(data = model_data_rest, rtotacc ~ .)
nw_vcovHAC_rest <- NeweyWest(base_model_rest, lag = 5)
homo_scCorrected_model_rest <- coeftest(base_model_rest, vcov. = nw_vcovHAC_rest)
homo_scCorrected_model_rest
# test not possible due to lack of information on R²

#5th test: Adjust time trend to fit other forms
ggplot(data = data, aes(x = t, y = rtotacc, colour=(beltlaw > 0), group=1)) + geom_line() + xlab("Time Period") + ylab("PRC Fat ACC") + geom_smooth() + geom_smooth(method="lm")
# The graph shows evidence of exponential trend (visually)
adjusted_model_data <- model_data
adjusted_model_data$tsq <- adjusted_model_data$t^2
trend_adjusted_base_model <- lm(data = adjusted_model_data, rtotacc ~ .)
adjusted_independent_data <- adjusted_model_data[,-1]
summary(trend_adjusted_base_model)
test_base_model_trend_adjusted <- test_GM_assumptions(adjusted_independent_data, trend_adjusted_base_model)

adjusted_nw_vcovHAC <- NeweyWest(trend_adjusted_base_model, lag = 5)
adjusted_homo_scCorrected_model <- coeftest(trend_adjusted_base_model, vcov. = adjusted_nw_vcovHAC)
adjusted_homo_scCorrected_model
homo_scCorrected_model

#6th test: Use linear time trend on logarithmic data
ggplot(data = data, aes(x = t, y = lrtotacc, colour=(beltlaw > 0), group=1)) + geom_line() + xlab("Time Period") + ylab("PRC Fat ACC") + geom_smooth() + geom_smooth(method="lm")
# The graph shows evidence of exponential trend (visually)
log_adjusted_model_data <- model_data
log_adjusted_model_data$rtotacc <- log(log_adjusted_model_data$rtotacc)
log_adjusted_base_model <- lm(data = log_adjusted_model_data, rtotacc ~ .)
log_adjusted_independent_data <- log_adjusted_model_data[,-1]
summary(log_adjusted_base_model)
test_base_model_log_adjusted <- test_GM_assumptions(log_adjusted_independent_data, log_adjusted_base_model)

acf(log_adjusted_model_data$rtotacc, lag.max=20)
adf.test(log_adjusted_model_data$rtotacc) # assume stationarity
# other variables are naturally not assumed to be relevant for stationarity test
# differentiation to correct for serial correlation
log_adjusted_differentiated_data <- log_adjusted_model_data[2:nrow(log_adjusted_model_data),]
log_adjusted_differentiated_data$rtotacc <- diff(x = log_adjusted_model_data$rtotacc)
log_adjusted_differentiated_data$unem <- diff(x = model_data$unem)
acf(differentiated_data$unem, lag.max=20)
adf.test(differentiated_data$unem)
acf(differentiated_data$prcfat, lag.max=20)
adf.test(differentiated_data$prcfat)
log_adjusted_differentiated_model <- lm(data = log_adjusted_differentiated_data, rtotacc ~ .)
log_adjusted_test_differentiated_model <- test_GM_assumptions(log_adjusted_differentiated_data, log_adjusted_differentiated_model)


# CONCLUSION ON H1: Accounting for autocorrelation, stationarity, time trend and seasonality and other factors, there is no evidence that beltlaw has any effect on the change in percentage of fatal accidents (prcfat) from the sample data.


