source("GM assumption test.R")
library(prais)
load("traffic2.RData")
describe(data)

# assumption: wkends includes fridays

g <- ggplot(data = data, aes(x = t, y = prcfat, colour=(beltlaw > 0), group=1)) + geom_line() + xlab("Time Period") + ylab("PRC Fat ACC")
g + geom_smooth() + geom_smooth(method="lm")

# H1:The percent of fatal accidents decreased after the introduction of the belt law (with lag)
  #1st test: model with timetrend
    model_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    ## Test with time trend and indicating variables (without tsq)
    base_model <- lm(data = model_data, prcfat ~ .)
    independent_data <- model_data[,-1]
    summary(base_model)
    test_base_model <- test_GM_assumptions(independent_data, base_model)
    # CONCLUSION: H5 violated: serial correlation - Model coefficients are unbiased but not BLUE, statistical inference not valid
  
  #2nd test: Correcting for serial correlation using Prais-Winsten method to correct for SC 1 (Idea: correct for SC=1 and do checks again)
    sc_corrected_model <- prais.winsten(prcfat ~ ., model_data, iter = 50, rho = 0, tol = 1e-08)
    test_sc_corrected_model <- test_GM_assumptions(independent_data, sc_corrected_model[[1]])
    # comparison of standard errors: corrected nidek generally yields to higher standard errors (slightly)
    sc_corrected_model[[1]]$coefficients[,2]/summary(base_model)$coefficients[,2]*100
    # CONCLUSION: after correcting for SC - biased but more efficient estimators of corrected model
  
  #3rd test: Test for unit root
    acf(base_model$model[,1], lag.max=20)
    
  #2nd test (Beta): Include serial correlation of order 3
    dependent_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    dependent_data["prcfat_1"] <- lag(dependent_data$prcfat)
    dependent_data["prcfat_2"] <- lag(dependent_data$prcfat_1)
    dependent_data["prcfat_3"] <- lag(dependent_data$prcfat_2)
    dependent_data <- dependent_data[4:nrow(dependent_data),]
    fatacc_model <- lm(data = dependent_data, prcfat ~ .)
    dependent_data <- dependent_data[,-1]
    t <- test_GM_assumptions(dependent_data, fatacc_model)
    summary(fatacc_model)
    #CONCLUSION: NO GM assumptions violated. but prcfat_2 and 3 found individually insignificant
  
  #3rd test: Include serial correlation of order 1
    dependent_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    dependent_data["prcfat_1"] <- lag(dependent_data$prcfat)
    dependent_data <- dependent_data[4:nrow(dependent_data),]
    restricted_fatacc_model <- lm(data = dependent_data, prcfat ~ .)
    dependent_data <- dependent_data[,-1]
    t <- test_GM_assumptions(dependent_data, restricted_fatacc_model)
    summary(restricted_fatacc_model)
    waldtest(fatacc_model, restricted_fatacc_model)
    # cannot reject H0 --> no joint significance identified
  #4th test: Test for seasonality
    # H0: monthly dummies jointly insignificant
    # H1: monthly dummies jointly significant
    dependent_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    dependent_data["prcfat_1"] <- lag(dependent_data$prcfat)
    dependent_data <- dependent_data[2:nrow(dependent_data),]
    ur_fatacc_model <- lm(data = dependent_data, prcfat ~ .)
    r_dependent_data <- data[, c("prcfat","t","unem" , "spdlaw", "beltlaw", "wkends")]
    r_dependent_data["prcfat_1"] <- lag(r_dependent_data$prcfat)
    r_dependent_data <- r_dependent_data[2:nrow(r_dependent_data),]
    r_fatacc_model <- lm(data = r_dependent_data, prcfat ~ .)
    waldtest(ur_fatacc_model, r_fatacc_model)
    # reject H0 at 0.0000001 level --> probably jointly significant
  #4th test: Differentiate timeseries
    # correct for timetrend and seasonality
    dependent_data <- data[, c("prcfat","t", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
    # correct for time trend and seasonality
    time_corrected_fatacc_model <- lm(data = dependent_data, prcfat ~ t)
    test_time <- test_GM_assumptions(dependent_data, time_corrected_fatacc_model)
    dependent_data <- dependent_data[,-c(1,2)]
    dependent_data$prcfat_res <- time_corrected_fatacc_model$residuals
    seasonal_corrected_fatacc_model <- lm(data = dependent_data, prcfat_res ~ .)
    test_season <- test_GM_assumptions(dependent_data, seasonal_corrected_fatacc_model)
    dependent_data <- data[, c("unem" , "spdlaw", "beltlaw", "wkends")]
    dependent_data$prcfat_res <- seasonal_corrected_fatacc_model$residuals
    dependent_data["prcfat_res_1"] <- lag(dependent_data$prcfat_res)
    dependent_data <- dependent_data[2:nrow(dependent_data),]
    fatacc_model <- lm(data = dependent_data, prcfat_res ~ .)
    t <- test_GM_assumptions(fatacc_model, dependent_data = dependent_data)
    
    adf.test(x = dependent_data$feb, k=1)
    
    diff_prcfat <- diff(dependent_data$prcfat)
    
  
  