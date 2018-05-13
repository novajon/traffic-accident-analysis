library(corrplot)
library(het.test)
library(Matrix)
library(lmtest)
library(stats)

rm(list=ls())

test_GM_assumptions <- function (independent_data, model, significance=0.05) {
  assumptions <- list()
  assumptions$H2 <- test_multicollinearity(independent_data)
  assumptions$H3 <- test_zeroConditionalMean(independent_data, model)
  assumptions$H4 <- test_homoscedasticity(independent_data, model, significance)
  assumptions$H5 <- test_serialCorrelation(model, significance)
  assumptions$H6 <- test_errorNormality(model, significance)
  return (assumptions)
}

# h1: Linear in parameters (Y)
# H2: No Multicollinearity (Y)
  
  test_multicollinearity <- function(independent_data) {
    dep_cor <- cor(independent_data)
    corrplot(dep_cor, type="upper")
    rank <- rankMatrix(independent_data)
    return(rank==ncol(independent_data))
  }
# H3: Zero Conditional Mean: Strict Exogenity, Contemporaneous Exogeneity (Y)
  test_zeroConditionalMean <- function(independent_data, model) {
    # Error at time t uncorrelated with each explanatory variable in every time perio
    res <- model$residuals
    res_include_dep_data <- cbind(independent_data, res)
    # visual plotting for initial insights
    plot(res)
    # looking at the correlation
    res_cor <- cor(res_include_dep_data)
    corrplot(res_cor, type="upper")
    return (sum(res_cor==1)==ncol(res_cor))
  }
  
# H4: Homoscedasticity (Y)
  test_homoscedasticity <- function (independent_data, model, sig=0.05) {
    ressq <- model$residuals^2
    ressq_include_dep_data <- cbind(independent_data, ressq)
    # White Test: Not conducted due to complexity and amount of df in the model
    # white <- lm(data = data, ressq ~ t + unem + spdlaw + beltlaw + wkends + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + t^2 + unem^2 + spdlaw^2 + beltlaw^2 + wkends^2 + feb^2 + mar^2 + apr^2 + may^2 + jun^2 + jul^2 + aug^2 + sep^2 + oct^2 + nov^2 + dec^2)
    # Breusch-Pagan:
    return (bptest(model)$p.value>sig)
  }
# H5 No serial correlation
  test_serialCorrelation <- function (model, sig=0.05) {
    results <- list()
    # acf(model$model[,1], lag.max=20)
    # Durbin Watson: Reject H0 --> Serial Correlation
    results$DW_Boolean <- dwtest(model)$p.value > sig
    # Breusch Godfrey
    index <- 0
    continue <- T
    while (continue) {
      index <- index + 1
      bg_test <- bgtest(model, order=index)
      if (bg_test$p.value > sig) continue = F
    }
    results$BG_Order <- index-1
    return (results)
  }
  # --> we assume serial correlation of order 3 based on Breusch-Godfrey test
# H6 Normal distribution of the errors
  test_errorNormality <- function (model, sig=0.05) {
    res <- model$residuals
    normality <- shapiro.test(res)
    return (normality$p.value > sig)
  }  