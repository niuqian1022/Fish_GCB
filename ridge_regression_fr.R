library(caret)
library(ggplot2)
library(glmnet)

# Brush creek fall data with species Etheastima caeruleum and E. spectabile ('etca' and 'etsp') analyzed
brFall = read.csv('br_fall13.csv')
# check the available data points for each species
sapply(br_fall[,2:11], function(y) sum(!is.na(y)))
# Brush spring data with 'cysp', 'etsp', 'etbl' analized
brSpr = read.csv('br_spr13.csv')
# check the available data points for each species
sapply(br_spr[,2:11], function(y) sum(!is.na(y)))
#===================================================================================
# define a function to fit the Ridge regression
# input: species name, dataset to analyze, and lambda range to search; 
# output: MRSE plot against lambda, and return the coefficient fitted to the data points;
# lambda is tuned based on cross-validation error (i.e. MSRE, mean square root error), 
# when lambda is selected, coefficient is fit to whole dataset with fixed lambda
glmnet_fit <- function (species = 'etca', dataset, lambda = c(0:10)/20) {
  X_Y = cbind(dataset[paste0(species, '.fr')], 
              dataset[paste0(species, '.size')], dataset['maxcv'], dataset['general'], dataset[species])
  colnames(X_Y) = c('fr', 'size', 'maxcv', 'general', 'density')
  fit_crl <- trainControl(
    method = "LOOCV",
    verboseIter=FALSE) 
  searchGrid = expand.grid(alpha = 1, lambda = lambda)
  fit<-caret::train(X_Y$fr ~ ., data = X_Y,
                    method = "glmnet", 
                    trControl = fit_crl,
                    tuneGrid = searchGrid)
  return (fit)
}
# run the model on etca, etsp in dataset br_fall 
etca_brFall = glmnet_fit(species = 'etca', dataset = brFall)
plot(etca_brFall) 
coef.etca_brFall = coef(etca_brFall$finalModel, s = etca_brFall$bestTune$lambda)
coef.etca_brFall

etsp_brFall = glmnet_fit(species = 'etsp', dataset = brFall)
plot(etsp_brFall) 
coef.etsp_brFall = coef(etsp_brFall$finalModel, s = etsp_brFall$bestTune$lambda)
coef.etsp_brFall
# Store the results in a dataframe
result_br = NULL
result_br = cbind(coef.etca_brFall[1:5], coef.etsp_brFall[1:5])
result_br = as.data.frame(result_br)
colnames(result_br) = c('etca_BR_Fall', 'etsp_BR_Fall')
rownames(result_br) = c('intercept', 'body size', 
                        'max current velocity', 'food resource', 'conspecific density')
# for both species in brush fall data set, coefficents turns out to be nearly zero
# if you change the lambda to coarser and larger range, you'll fine the RMSE 
# only decreases to a low limit when lambda is very large (meaning all the weights should be nearly zero)

# run the model on 'cysp', 'etsp', 'etbl' in dataset br_spr 
cysp_brSpr = glmnet_fit(species = 'cysp', dataset = brSpr)
plot(cysp_brSpr) 
coef.cysp_brSpr = coef(cysp_brSpr$finalModel, s = cysp_brSpr$bestTune$lambda)
coef.cysp_brSpr
result_br$cysp_BR_Spring = coef.cysp_brSpr[1:5]
#*********************************************************************************
etbl_brSpr = glmnet_fit(species = 'etbl', dataset = brSpr)
plot(etbl_brSpr) 
coef.etbl_brSpr = coef(etbl_brSpr$finalModel, s = etbl_brSpr$bestTune$lambda)
coef.etbl_brSpr
result_br$etbl_BR_Spring = coef.etbl_brSpr[1:5]

etsp_brSpr = glmnet_fit(species = 'etsp', dataset = brSpr)
plot(etsp_brSpr) 
coef.etsp_brSpr = coef(etsp_brSpr$finalModel, s = etsp_brSpr$bestTune$lambda)
coef.etsp_brSpr
result_br$etsp_BR_Spring = coef.etsp_brSpr[1:5]


#for huzzah creek
#for all the season etca, etfl, coba and luzo were analyzed
hzFall = read.csv('hz_fall11.csv')
hzWin = read.csv('hz_win11.csv')
hzSum = read.csv('hz_sum12.csv')
hzSpr = read.csv('hz_spr12.csv')

sapply(hzFall[,2:11], function(y) sum(!is.na(y)))
sapply(hzWin[,2:11], function(y) sum(!is.na(y)))
sapply(hzSum[,2:11], function(y) sum(!is.na(y)))
sapply(hzSpr[,2:11], function(y) sum(!is.na(y)))

#Huzzah creek fall
#===============================================================================
etca_hzFall = glmnet_fit(species = 'etca', dataset = hzFall)
plot(etca_hzFall) 
coef.etca_hzFall = coef(etca_hzFall$finalModel, s = etca_hzFall$bestTune$lambda)
coef.etca_hzFall

etfl_hzFall = glmnet_fit(species = 'etfl', dataset = hzFall)
plot(etfl_hzFall) 
coef.etfl_hzFall = coef(etfl_hzFall$finalModel, s = etfl_hzFall$bestTune$lambda)
coef.etfl_hzFall

coba_hzFall = glmnet_fit(species = 'coba', dataset = hzFall)
plot(coba_hzFall) 
coef.coba_hzFall = coef(coba_hzFall$finalModel, s = coba_hzFall$bestTune$lambda)
coef.coba_hzFall
#******************************************************************************
#******************************************************************************
luzo_hzFall = glmnet_fit(species = 'luzo', dataset = hzFall, lambda = c(1:10)/40)
plot(luzo_hzFall) 
coef.luzo_hzFall = coef(luzo_hzFall$finalModel, s = luzo_hzFall$bestTune$lambda)
coef.luzo_hzFall
#===============================================================================
result_hzFall = cbind(coef.coba_hzFall[1:5], coef.etca_hzFall[1:5], 
                coef.etfl_hzFall[1:5], coef.luzo_hzFall[1:5])
result_hzFall = as.data.frame(result_hzFall)
colnames(result_hzFall) = c('coba_HZ_Fall', 'etca_HZ_Fall', 'etfl_HZ_Fall', 'luzo_HZ_Fall')
rownames(result_hzFall) = c('intercept', 'body size', 'max current velocity', 
                      'food resource', 'conspecific density')
fit_results = data.frame('object' = 'luzo_hzFall', 'lambda' = 0.175, 'RMSE' = 0.7671, 'X' = 'size',
                         'Rsquared' = 0.3652, stringsAsFactors = FALSE)
#Huzzah creek winter
#===============================================================================
etca_hzWin = glmnet_fit(species = 'etca', dataset = hzWin)
plot(etca_hzWin) 
coef.etca_hzWin = coef(etca_hzWin$finalModel, s = etca_hzWin$bestTune$lambda)
coef.etca_hzWin

etfl_hzWin = glmnet_fit(species = 'etfl', dataset = hzWin, lambda = c(1:20)/20)
plot(etfl_hzWin) 
coef.etfl_hzWin = coef(etfl_hzWin$finalModel, s = etfl_hzWin$bestTune$lambda)
coef.etfl_hzWin

coba_hzWin = glmnet_fit(species = 'coba', dataset = hzWin, lambda = c(1:20)/20)
plot(coba_hzWin) 
coef.coba_hzWin = coef(coba_hzWin$finalModel, s = coba_hzWin$bestTune$lambda)
coef.coba_hzWin
#******************************************************************************
#******************************************************************************
luzo_hzWin = glmnet_fit(species = 'luzo', dataset = hzWin, lambda = c(1:20)/40)
plot(luzo_hzWin) 
coef.luzo_hzWin = coef(luzo_hzWin$finalModel, s = luzo_hzWin$bestTune$lambda)
coef.luzo_hzWin
#===============================================================================
result_hzWin = cbind(coef.coba_hzWin[1:5], coef.etca_hzWin[1:5], 
                      coef.etfl_hzWin[1:5], coef.luzo_hzWin[1:5])
result_hzWin = as.data.frame(result_hzWin)
colnames(result_hzWin) = c('coba_HZ_Win', 'etca_HZ_Win', 'etfl_HZ_Fall', 'luzo_HZ_Win')
rownames(result_hzWin) = c('intercept', 'body size', 'max current velocity', 
                            'food resource', 'conspecific density')
rownames(result_hzFall) = c('intercept', 'body size', 'max current velocity', 
                            'food resource', 'conspecific density')

fit_results[2,] = c('luzo_hzWin',  0.375, 1.0862, 'density', 0.3622)
#Huzzah creek summer
#===============================================================================
#******************************************************************************
#******************************************************************************
etca_hzSum = glmnet_fit(species = 'etca', dataset = hzSum, lambda = c(1:10)/40)
plot(etca_hzSum) 
coef.etca_hzSum = coef(etca_hzSum$finalModel, s = etca_hzSum$bestTune$lambda)
coef.etca_hzSum
fit_results[3,] = c('etca_hzSum',  0.200,   0.8066567, 'maxcv',  0.2923550)
#******************************************************************************
#******************************************************************************
etfl_hzSum = glmnet_fit(species = 'etfl', dataset = hzSum, lambda = c(1:20)/40)
plot(etfl_hzSum) 
coef.etfl_hzSum = coef(etfl_hzSum$finalModel, s = etfl_hzSum$bestTune$lambda)
coef.etfl_hzSum
fit_results[4,] = c('etfl_hzSum',  0.275,   0.7442221, 'size, density',  0.3724109)
#******************************************************************************
#******************************************************************************
coba_hzSum = glmnet_fit(species = 'coba', dataset = hzSum, lambda = c(1:10)/40)
plot(coba_hzSum) 
coef.coba_hzSum = coef(coba_hzSum$finalModel, s = coba_hzSum$bestTune$lambda)
coef.coba_hzSum
fit_results[5,] = c('coba_hzSum', 0.075,  0.9451070, 'all',  0.062688776)
#******************************************************************************
#******************************************************************************
luzo_hzSum = glmnet_fit(species = 'luzo', dataset = hzSum, lambda = c(10:20)/50)
plot(luzo_hzSum) 
coef.luzo_hzSum = coef(luzo_hzSum$finalModel, s = luzo_hzSum$bestTune$lambda)
coef.luzo_hzSum
fit_results[6,] = c('luzo_hzSum', 0.075,  0.9451070, 'density',  0.062688776)
#===============================================================================
result_hzSum = cbind(coef.coba_hzSum[1:5], coef.etca_hzSum[1:5], 
                     coef.etfl_hzSum[1:5], coef.luzo_hzSum[1:5])
result_hzSum = as.data.frame(result_hzSum)
colnames(result_hzSum) = c('coba_HZ_Win', 'etca_HZ_Win', 'etfl_HZ_Fall', 'luzo_HZ_Win')
rownames(result_hzSum) = c('intercept', 'body size', 'max current velocity', 
                           'food resource', 'conspecific density')
# Huzzah creek Spring
#=============================================================================
etca_hzSpr = glmnet_fit(species = 'etca', dataset = hzSpr, lambda = c(1:10)/10)
plot(etca_hzSpr) 
coef.etca_hzSpr = coef(etca_hzSpr$finalModel, s = etca_hzSpr$bestTune$lambda)
coef.etca_hzSpr
#******************************************************************************
#******************************************************************************
etfl_hzSpr = glmnet_fit(species = 'etfl', dataset = hzSpr, lambda = c(1:10)/40)
plot(etfl_hzSpr) 
coef.etfl_hzSpr = coef(etfl_hzSpr$finalModel, s = etfl_hzSpr$bestTune$lambda)
coef.etfl_hzSpr
fit_results[7,] = c('etfl_hzSpr',  0.025,   0.5789019, 'size, resource, density' ,0.6263819225)
#******************************************************************************
#******************************************************************************
coba_hzSpr = glmnet_fit(species = 'coba', dataset = hzSpr, lambda = c(1:20)/40)
plot(coba_hzSpr) 
coef.coba_hzSpr = coef(coba_hzSpr$finalModel, s = coba_hzSpr$bestTune$lambda)
coef.coba_hzSpr
fit_results[8,] = c('coba_hzSpr',  0.275, 0.9704859, 'maxcv',  0.07620606)
#******************************************************************************
#******************************************************************************
luzo_hzSpr = glmnet_fit(species = 'luzo', dataset = hzSpr, lambda = c(0:10)/40)
plot(luzo_hzSpr) 
coef.luzo_hzSpr = coef(luzo_hzSpr$finalModel, s = luzo_hzSpr$bestTune$lambda)
coef.luzo_hzSpr
fit_results[8,] = c('luzo_hzSpr',  0.200, 0.8601539, 'maxcv, size', 0.19405810)
#===============================================================================
result_hzSpr = cbind(coef.coba_hzSpr[1:5], coef.etca_hzSpr[1:5], 
                     coef.etfl_hzSpr[1:5], coef.luzo_hzSpr[1:5])
result_hzSpr = as.data.frame(result_hzSpr)
colnames(result_hzSpr) = c('coba_HZ_Win', 'etca_HZ_Win', 'etfl_HZ_Fall', 'luzo_HZ_Win')
rownames(result_hzSpr) = c('intercept', 'body size', 'max current velocity', 
                           'food resource', 'conspecific density')

#==============================================================================

sumData = read.csv('FR_sum_nor.csv')
sprData = read.csv('FR_spr_nor.csv')
sapply(sumData, function(y) sum(!is.na(y)))
sapply(sprData, function(y) sum(!is.na(y)))

# etca etfl etbl luzo (n >= 9)
# Summer
#======================================================================

etblSum = glmnet_fit(species = 'etbl', dataset = sumData, lambda = c(0:20)/20)
plot(etblSum) 
coef.etblSum = coef(etblSum$finalModel, s = etblSum$bestTune$lambda)
coef.etblSum
#*********************************************
etcaSum = glmnet_fit(species = 'etca', dataset = sumData, lambda = c(0:20)/40)
plot(etcaSum) 
coef.etcaSum = coef(etcaSum$finalModel, s = etcaSum$bestTune$lambda)
coef.etcaSum
#*********************************************
etspSum = glmnet_fit(species = 'etsp', dataset = sumData, lambda = c(0:20)/40)
plot(etspSum) 
coef.etspSum = coef(etspSum$finalModel, s = etspSum$bestTune$lambda)
coef.etspSum

etflSum = glmnet_fit(species = 'etfl', dataset = sumData, lambda = c(0:10)/10)
plot(etflSum) 
coef.etflSum = coef(etflSum$finalModel, s = etflSum$bestTune$lambda)
coef.etflSum
#*********************************************
luzoSum = glmnet_fit(species = 'luzo', dataset = sumData, lambda = c(0:20)/40)
plot(luzoSum) 
coef.luzoSum = coef(luzoSum$finalModel, s = luzoSum$bestTune$lambda)
coef.luzoSum
#======================================================================
result_overallSum= cbind(coef.etblSum[1:5], coef.etcaSum[1:5],  coef.etflSum[1:5],
                     coef.etspSum[1:5], coef.luzoSum[1:5])
result_overallSum = as.data.frame(result_overallSum)
colnames(result_overallSum) = c('etbl_Sum', 'etca_Sum', 'etfl_Sum', 'etsp_Sum', 'luzo_Sum')
rownames(result_overallSum) = c('intercept', 'body size', 'max current velocity', 
                           'food resource', 'conspecific density')
fit_results[9,] = c('etbl_allSites_sum', 0.225, 1.109171, 'maxcv, density',  0.016522345)
fit_results[10,] = c('etca_allSites_sum', 0.250,   0.8185674, 'resource',  0.27597259)
fit_results[11,] = c('etsp_allSites_sum', 0.200,   0.9959462, 'size',  0.0053175266)
fit_results[12,] = c('luzo_allSites_sum', 0.125,   0.8921783, 'size, maxcv, resource',  0.1842983)

#===========================================================================
# spring
#*********************************************
etblSpr = glmnet_fit(species = 'etbl', dataset = sprData, lambda = c(0:20)/40)
plot(etblSpr) 
coef.etblSpr = coef(etblSpr$finalModel, s = etblSpr$bestTune$lambda)
coef.etblSpr
#*********************************************
etcaSpr = glmnet_fit(species = 'etca', dataset = sprData, lambda = c(0:20)/40)
plot(etcaSpr) 
coef.etcaSpr = coef(etcaSpr$finalModel, s = etcaSpr$bestTune$lambda)
coef.etcaSpr

etspSpr = glmnet_fit(species = 'etsp', dataset = sprData, lambda = c(0:20)/40)
plot(etspSpr) 
coef.etspSpr = coef(etspSpr$finalModel, s = etspSpr$bestTune$lambda)
coef.etspSpr

etflSpr = glmnet_fit(species = 'etfl', dataset = sprData, lambda = c(0:10)/10)
plot(etflSpr) 
coef.etflSpr = coef(etflSpr$finalModel, s = etflSpr$bestTune$lambda)
coef.etflSpr
#*********************************************
luzoSpr = glmnet_fit(species = 'luzo', dataset = sprData, lambda = c(0:20)/40)
plot(luzoSpr) 
coef.luzoSpr = coef(luzoSpr$finalModel, s = luzoSpr$bestTune$lambda)
coef.luzoSpr
#======================================================================
result_overallSpr= cbind(coef.etblSpr[1:5], coef.etcaSpr[1:5],  coef.etflSpr[1:5],
                         coef.etspSpr[1:5], coef.luzoSpr[1:5])
result_overallSpr = as.data.frame(result_overallSpr)
colnames(result_overallSpr) = c('etbl_Spr', 'etca_Spr', 'etfl_Spr', 'etsp_Spr', 'luzo_Spr')
rownames(result_overallSpr) = c('intercept', 'body size', 'max current velocity', 
                                'food resource', 'conspecific density')
fit_results[13,] = c('etbl_allSites_spr', 0.000,   1.141133, 'size, maxcv, resource',  0.0329474706)
fit_results[14,] = c('etca_allSites_spr', 0.300,   1.012950, 'density',  1.322322e-03)
fit_results[15,] = c('luzo_allSites_spr', 0.275,   1.001639, 'size',   0.0005827925)
fit_results[16,] = c('etbl_br_Spr', 0.05,     0.8533004, 'resource,  density',  0.2402027627)
#==========================================================================
# save results into csv files
write.csv(fit_results, 'hyper_para.csv')
coef_results = cbind(result_br, result_hzFall, result_hzSpr, result_hzSum, result_hzWin, 
                     result_overallSpr, result_overallSum)
write.csv(coef_results, 'coefs.csv')

#===========================================================================
# generate parametric bootstrap confidence interval for Rsquare and coefs 
library(boot)
set.seed(0) # make sure the results are reproducible
# Bootstrap 95% CI for R-Squared
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary.lm(fit)$r.squared)
} 
adj_rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary.lm(fit)$adj.r.squared)
} 

# Bootstrap 95% CI for coeffiecents
bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}
#===================================================
summary(lm(formula=luzo.fr~luzo.size, data=hzFall))
rConf <- boot(data=hzFall, statistic=rsq, R = 1000, formula=luzo.fr~luzo.size)
coefConf <- boot(data=hzFall, statistic=bs, R = 1000, formula=luzo.fr~luzo.size)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index = 2) # index indicate the position of variable of interests

#===================================================
summary(lm(formula=luzo.fr~luzo, data=hzWin))
rConf <- boot(data=hzWin, statistic=rsq, R=1000, formula=luzo.fr~luzo)
coefConf <- boot(data=hzWin, statistic=bs, R=1000, formula=luzo.fr~luzo)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index = 2)
#======================================================
summary(lm(formula=etca.fr~maxcv, data=hzSum))
rConf <- boot(data=hzSum, statistic=rsq, R=1000, formula=etca.fr~maxcv)
coefConf <- boot(data=hzSum, statistic=bs, R=1000, formula=etca.fr~maxcv)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index = 2)

#======================================================
summary(lm(formula=etfl.fr~etfl.size + etfl, data=hzSum))
rConf <- boot(data=hzSum, statistic=adj_rsq, R=1000, formula=etfl.fr~etfl.size + etfl)
coefConf <- boot(data=hzSum, statistic=bs, R=1000, formula=etfl.fr~etfl.size + etfl)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)
boot.ci(coefConf, type="basic", index=3)

#======================================================
summary(lm(formula=coba.fr~coba.size + maxcv + general + coba, data=hzSum))
rConf <- boot(data=hzSum, statistic=adj_rsq, R=1000, formula=coba.fr~coba.size + maxcv + general + coba)
coefConf <- boot(data=hzSum, statistic=bs, R=1000, formula=coba.fr~coba.size + maxcv + general + coba)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]
boot.ci(coefConf, type="basic", index=4)$basic[,4:5]
boot.ci(coefConf, type="basic", index=5)$basic[,4:5]

#=================================================================
summary(lm(formula=luzo.fr~ luzo, data=hzSum))
rConf <- boot(data=hzSum, statistic=rsq, R=1000, formula=luzo.fr~ luzo)
coefConf <- boot(data=hzSum, statistic=bs, R=1000, formula=luzo.fr~ luzo)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)

#=================================================================
summary(lm(formula=etfl.fr~ etfl.size + general + etfl, data=hzSpr))
rConf <- boot(data=hzSpr, statistic=adj_rsq, R=1000, formula=etfl.fr~ etfl.size + general + etfl)
coefConf <- boot(data=hzSpr, statistic=bs, R=1000, formula=etfl.fr~ etfl.size + general + etfl)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]
boot.ci(coefConf, type="basic", index=4)$basic[,4:5]

#=================================================================
summary(lm(formula=luzo.fr~ luzo.size + maxcv, data=hzSpr))
rConf <- boot(data=hzSpr, statistic=adj_rsq, R=1000, formula=luzo.fr~ luzo.size + maxcv)
coefConf <- boot(data=hzSpr, statistic=bs, R=1000, formula=luzo.fr~ luzo.size + maxcv)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]

#=================================================================
summary(lm(formula=etbl.fr~ general + etbl, data=brSpr))
rConf <- boot(data=brSpr, statistic=adj_rsq, R=1000, formula=etbl.fr~ general + etbl)
coefConf <- boot(data=brSpr, statistic=bs, R=1000, formula=etbl.fr~ general + etbl)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]

#=================================================================
summary(lm(formula=etbl.fr~ maxcv + etbl, data=sumData))
rConf <- boot(data=sumData, statistic=adj_rsq, R=1000, formula=etbl.fr~ maxcv + etbl)
coefConf <- boot(data=sumData, statistic=bs, R=1000, formula=etbl.fr~ maxcv + etbl)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]

#=================================================================
summary(lm(formula=etca.fr~ general, data=sumData))
rConf <- boot(data=sumData, statistic=rsq, R=1000, formula=etca.fr~ general)
coefConf <- boot(data=sumData, statistic=bs, R=1000, formula=etca.fr~ general)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]

#=================================================================
summary(lm(formula=etsp.fr~ etsp.size, data=sumData))
rConf <- boot(data=sumData, statistic=rsq, R=1000, formula=etsp.fr~ etsp.size)
coefConf <- boot(data=sumData, statistic=bs, R=1000, formula=etsp.fr~ etsp.size)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]

#=================================================================
summary(lm(formula=luzo.fr~ luzo.size + maxcv + general, data=sumData))
rConf <- boot(data=sumData, statistic=adj_rsq, R=1000, formula=luzo.fr~ luzo.size + maxcv + general)
coefConf <- boot(data=sumData, statistic=bs, R=1000, formula=luzo.fr~ luzo.size + maxcv + general)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]
boot.ci(coefConf, type="basic", index=4)$basic[,4:5]

#=================================================================
summary(lm(formula=etbl.fr~ etbl.size + maxcv + general, data=sprData))
rConf <- boot(data=sprData, statistic=adj_rsq, R=1000, formula=etbl.fr~ etbl.size + maxcv + general)
coefConf <- boot(data=sprData, statistic=bs, R=1000, formula=etbl.fr~ etbl.size + maxcv + general)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
boot.ci(coefConf, type="basic", index=3)$basic[,4:5]
boot.ci(coefConf, type="basic", index=4)$basic[,4:5]

#=================================================================
summary(lm(formula=etca.fr~ etca, data=sprData))
rConf <- boot(data=sprData, statistic=rsq, R=500, formula=etca.fr~ etca)
coefConf <- boot(data=sprData, statistic=bs, R=500, formula=etca.fr~ etca)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]

#=================================================================
summary(lm(formula=luzo.fr~ luzo.size, data=sprData))
rConf <- boot(data=sprData, statistic=rsq, R=500, formula=luzo.fr~ luzo.size)
coefConf <- boot(data=sprData, statistic=bs, R=500, formula=luzo.fr~ luzo.size)
boot.ci(rConf, type="basic")
boot.ci(coefConf, type="basic", index=2)$basic[,4:5]
