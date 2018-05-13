source("GM assumption test.R")
library(prais)
library(tseries)
library(stargazer)
library(stats)
library(psych)
library(ggplot2)
load("traffic2.RData")
describe(data)

# assumption: wkends includes fridays
data$prcfat <- data$fatacc/data$totacc*100

g <- ggplot(data = data, aes(x = t, y = prcfat, colour=(beltlaw > 0), group=1)) + geom_line() + xlab("Time Period") + ylab("% Fatal Accidents")
g + geom_smooth() + geom_smooth(method="lm")

# H1:The percent of fatal accidents decreased after the introduction of the belt law (with lag)
  #1st test: model with timetrend
    model_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    ## Test with time trend and indicating variables (without tsq)
    base_model <- lm(data = model_data, prcfat ~ .)
    independent_data <- model_data[,-1]
    
    stargazer(base_model, title="Base Model OLS results", align=TRUE, summary = F, report = "vc*p")
    test_base_model <- test_GM_assumptions(independent_data, base_model)
    # CONCLUSION: H5 violated: serial correlation - Model coefficients are unbiased but not BLUE, statistical inference not valid
  
  #2nd test: Correcting for serial correlation using Prais-Winsten method to correct for SC 1 (Idea: correct for SC=1 and do checks again)
    sc_corrected_model <- prais.winsten(prcfat ~ ., model_data, iter = 50, rho = 0, tol = 1e-08)
    test_sc_corrected_model <- test_GM_assumptions(independent_data, sc_corrected_model[[1]])
    
    stargazer(sc_corrected_model[[1]]$r.squared, title="Base Model OLS results", align=TRUE, summary = F, report = "vc*p")
    # comparison of standard errors: corrected nidek generally yields to higher standard errors (slightly)
    sc_corrected_model[[1]]$coefficients[,2]/summary(base_model)$coefficients[,2]*100
    # CONCLUSION: after correcting for SC - biased but more efficient estimators of corrected model
  
  #3rd test: Test for unit root on the base model (alternativde to 2nd test)
    # prcfat
    acf(base_model$model[,1], lag.max=20) # visual indication of non-stationarity
    adf.test(model_data$prcfat) # not enough indication of non-stationarity for prcfat for lag order 4
    #unem
    acf(base_model$model[,3], lag.max=20) # visual indication of non-stationarity
    adf.test(model_data$unem) # not enough evidence to reject non-stationarity hypothesis
    # other variables are naturally not assumed to be relevant for stationarity test
    # differentiation to correct for serial correlation
    differentiated_data <- model_data[2:nrow(model_data),]
    differentiated_data$prcfat <- diff(x = model_data$prcfat)
    differentiated_data$unem <- diff(x = model_data$unem)
    acf(differentiated_data$unem, lag.max=20)
    adf.test(differentiated_data$unem)
    acf(differentiated_data$prcfat, lag.max=20)
    adf.test(differentiated_data$prcfat)
    differentiated_model <- lm(data = differentiated_data, prcfat ~ .)
    stargazer(differentiated_model, title="Base Model OLS results", align=TRUE, summary = T, report = "vc*p")
    test_differentiated_model <- test_GM_assumptions(differentiated_data, differentiated_model)
  #4th test: Test for seasonality in the model
    differentiated_data_rest <- differentiated_data[,c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends")]
    differentiated_model_rest <- lm(data = differentiated_data_rest, prcfat ~ .)
    waldtest(differentiated_model, differentiated_model_rest)
  # CONCLUSION ON H1: Accounting for autocorrelation, stationarity, time trend and seasonality and other factors, there is no evidence that beltlaw has any effect on the change in percentage of fatal accidents (prcfat) from the sample data.
  # trend is insignificant due to stationarity
    
  #5th test: Test for influence of quadratic time trend
    adj_model_data <- model_data
    adj_model_data$tsq <- adj_model_data$t^2
    adj_independent_data <- adj_model_data[-1]
    adj_sc_corrected_model <- prais.winsten(prcfat ~ ., adj_model_data, iter = 50, rho = 0, tol = 1e-08)
    adj_test_sc_corrected_model <- test_GM_assumptions(independent_data, adj_sc_corrected_model[[1]])
    
  ## 6th test for belt law times t
  
  txbeltlaw_model_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
  txbeltlaw_model_data$txbeltlaw <- txbeltlaw_model_data$t * txbeltlaw_model_data$beltlaw
  ## Test with time trend and indicating variables (without tsq)
  txbeltlaw_base_model <- lm(data = txbeltlaw_model_data, prcfat ~ .)
  stargazer(txbeltlaw_base_model, title="Base Model OLS results", align=TRUE, summary = T, report = "vc*p")
  
  txbeltlaw_independent_data <- txbeltlaw_model_data[,-1]
  summary(txbeltlaw_base_model)
  test_txbeltlaw_base_model <- test_GM_assumptions(txbeltlaw_independent_data, txbeltlaw_base_model)
  # interesting: BG order reduces to 1
  
  #7th test: correcting for serial correlation using Prais Winsten
  txbeltlaw_sc_corrected_model <- prais.winsten(prcfat ~ ., txbeltlaw_model_data, iter = 50, rho = 0, tol = 1e-08)
  test_txbeltlaw_sc_corrected_model <- test_GM_assumptions(txbeltlaw_independent_data, txbeltlaw_sc_corrected_model[[1]])
  stargazer(txbeltlaw_sc_corrected_model[[1]]$coefficients, title="Serially Corrected Model including t*beltlaw as dep. var.", align=TRUE, summary = F, report = "vc*p")
  # comparison of standard errors: corrected nidek generally yields to higher standard errors (slightly)
  txbeltlaw_sc_corrected_model[[1]]$coefficients[,2]/summary(base_model)$coefficients[,2]*100
  # CONCLUSION: after correcting for SC - biased but more efficient estimators of corrected model
  
  #8th test: b(t) = -b(txbeltlaw)
  t_txbeltlaw_model_data <- txbeltlaw_model_data
  t_txbeltlaw_model_data$txbeltlaw <- t_txbeltlaw_model_data$txbeltlaw - t_txbeltlaw_model_data$t
  t_txbeltlaw_sc_corrected_model <- prais.winsten(prcfat ~ ., t_txbeltlaw_model_data, iter = 50, rho = 0, tol = 1e-08)
  t_txbeltlaw_independent_data <- t_txbeltlaw_model_data[,-1]
  t_test_txbeltlaw_sc_corrected_model <- test_GM_assumptions(t_txbeltlaw_independent_data, t_txbeltlaw_sc_corrected_model[[1]])
  stargazer(t_txbeltlaw_sc_corrected_model[[1]]$coefficients, title="Serially Corrected Model including t*beltlaw as dep. var.", align=TRUE, summary = F, report = "vc*p")
  # comparison of standard errors: corrected nidek generally yields to higher standard errors (slightly)
  txbeltlaw_sc_corrected_model[[1]]$coefficients[,2]/summary(base_model)$coefficients[,2]*100
  # CONCLUSION: after correcting for SC - biased but more efficient estimators of corrected model
  
  
  #9th test: correcting for serial correlation in txbeltlaw model using differentiation
  txbeltlaw_differentiated_data <- txbeltlaw_model_data[2:nrow(txbeltlaw_model_data),]
  txbeltlaw_differentiated_data$prcfat <- diff(x = txbeltlaw_model_data$prcfat)
  txbeltlaw_differentiated_data$unem <- diff(x = txbeltlaw_model_data$unem)
  acf(txbeltlaw_differentiated_data$unem, lag.max=20)
  adf.test(txbeltlaw_differentiated_data$unem)
  acf(txbeltlaw_differentiated_data$prcfat, lag.max=20)
  adf.test(txbeltlaw_differentiated_data$prcfat)
  txbeltlaw_differentiated_model <- lm(data = txbeltlaw_differentiated_data, prcfat ~ .)
  txbeltlaw_test_differentiated_model <- test_GM_assumptions(txbeltlaw_differentiated_data, txbeltlaw_differentiated_model)
  
  #10th test: test model 7 for seasonality:
  r_txbeltlaw_model_data <- txbeltlaw_model_data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "txbeltlaw")]
  r_txbeltlaw_sc_corrected_model <- prais.winsten(prcfat ~ ., r_txbeltlaw_model_data, iter = 50, rho = 0, tol = 1e-08)
  
  summary(differentiated_model)
  plot(differentiated_model$residuals)
  test_differentiated_model
  
  plot(sc_corrected_model[[1]]$residuals)
  test_sc_corrected_model
  
  txbeltlaw_sc_corrected_model[[1]]
  test_txbeltlaw_sc_corrected_model
  
  # seasonality
  ## Test with time trend and indicating variables (without tsq)
    