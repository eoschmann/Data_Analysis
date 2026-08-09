####                        Supplementary R Script                          ####
#------------------------------------------------------------------------------#
###                         Energy Price Analysis                            ###
#------------------------------------------------------------------------------#

#------------------------------Research Question-------------------------------#

# Does including the 2022 energy crisis period degrade forecast accuracy 
# of energy prices and does the same hold for countries relying on different 
# primary energy consumption patterns?

#------------------------------Table of Contents-------------------------------#

# 1 Setup .............................................................. 34-54
#   1.1 Preliminary Data Check ......................................... 55-73
#   1.2 Creating Time Series ........................................... 74-89
# 2 Decomposition of Log-Series ........................................ 90-101
#   2.1 (P)ACF of Random Component ..................................... 102-110
#   2.2 Naive OLS Approach: Trend + Seasonal .. ........................ 111-124
#   2.3 Holt-Winters Forecast .......................................... 125-154
#   2.4 Random Walk Check .............................................. 155-162
# 3 SARIMA Model & Forecast ............................................ 163-197
#   3.1 SARIMA 1-Year Forecast ......................................... 198-219
#   3.2 Forecast Degradation: Pre-Crisis Fit vs. Actual ................ 221-253
#   3.3 Different Forecast Training Windows ............................ 254-304
# 4 Volatility: GARCH and ARCH ......................................... 305-325
#   4.1 GARCH on SARIMA residuals ...................................... 326-349
#   4.2 Fitted Conditional Volatility .................................. 350-363
# 5 All Plots and Visualizations ....................................... 364-463
#   5.1 Slide Reproducibility .......................................... 464-484
# 6  References ........................................................ 484-495

#------------------------------------------------------------------------------#
####                                1. Setup                                ####
#------------------------------------------------------------------------------#

## Packages 
#install.packages("readxl")
#install.packages("tseries")
#install.packages("stargazer")
#install.packages("readxl")
library(readxl)     
library(nlme)       
library(tseries)    
library(stargazer)  

## Read the data
weekly <- read_excel("File_Name")

## Subset the German/Luxembourg price
weeklyDE <- weekly$`Deutschland/Luxemburg [€/MWh]`


#------------------------------------------------------------------------------#
####                    1.1 Preliminary Data Check                          ####
#------------------------------------------------------------------------------#

## Check summary of weeklyDE subset 
summary(weeklyDE)     # min 11.19 / mean 93.23 / max 585.92 
head(weeklyDE)        # Starts at 51.45 €/MWh on October 1st, 2018
tail(weeklyDE)        # Ends at 73.85 €/MWh on January 6th, 2026
is.numeric(weeklyDE)  # TRUE                   
sum(is.na(weeklyDE))  # No NA's                    

## Cross-country correlation check 
cor(weeklyDE, weekly$`Frankreich [€/MWh]`)     # ~0.965
cor(weeklyDE, weekly$`Österreich [€/MWh]`)     # ~0.990
cor(weeklyDE, weekly$`Niederlande [€/MWh]`)    # ~0.991
cor(weeklyDE, weekly$`Norwegen 2 [€/MWh]`)     # ~0.953


#------------------------------------------------------------------------------#
####                       1.2 Creating Time Series                         ####
#------------------------------------------------------------------------------#

## Create Time Series
weekly.ts <- ts(weeklyDE, start = c(2018, 40), frequency = 52)

## Control success of ts creation
class(weekly.ts)     # ts
frequency(weekly.ts) # 52
length(weekly.ts)    # 379

## Log-transform time series to normalize variance and large spikes
weekly.ln <- log(weekly.ts)


#------------------------------------------------------------------------------#
####                      2. Decomposition of Log Series                    ####
#------------------------------------------------------------------------------#

## Series Decomposition
weekly.dec <- decompose(weekly.ln, type = "additive")

## Permanent level shift, pre- and post-crisis prices
exp(mean(window(Trend, start = c(2019,1), end = c(2020,52)), na.rm = TRUE)) # 32.58
exp(mean(window(Trend, start = c(2024,1), end = c(2025,40)), na.rm = TRUE)) # 80.82


#------------------------------------------------------------------------------#
####                    2.1 (P)ACF of Random Component                      ####
#------------------------------------------------------------------------------#

## Numerical acf value 
acf(weekly.random)$acf[2]      # ~0.49                # lag 1 (previous week)      
acf(weekly.random)$acf[5]      # ~0.12                # lag 4 (one month earlier) 


#------------------------------------------------------------------------------#
####                  2.2 Naive OLS Approach: Trend + Seasonal              ####
#------------------------------------------------------------------------------#

## OLS regression for log-transformed model 
weekly.lm.ln <- lm(as.numeric(weekly.ln) ~ time(weekly.ln))
summary(weekly.lm.ln, vcov = vcovHC)

## Latex-ready OLS regression output
stargazer(weekly.lm.ln, type = "latex",
          dep.var.labels = "log weekly electricity price", digits = 3,
          keep.stat = c("n", "rsq", "adj.rsq"))


#------------------------------------------------------------------------------#
####                    2.3 Holt-Winters Forecast                           ####
#------------------------------------------------------------------------------#

## Standard Holt-Winters (HW) application
weekly.hw <- HoltWinters(weekly.ln, seasonal = "additive")
weekly.hw
weekly.hw$SSE

## HW coefficients
weekly.hw$coefficients # alpha ~0.39, beta ~0.00, gamma ~0.46

##Exponential Smoothing alone 
weekly.es <- HoltWinters(weekly.ln, beta = FALSE,gamma = FALSE)
weekly.es$SSE  # ~27.84

## Prediction on Holt Winters model
weekly.hw.pred <- predict(weekly.hw, n.ahead = 104, prediction.interval = TRUE)

## 2-year forecast with prediction interval
ts.plot(weekly.ln, weekly.hw.pred[, "fit"],
        weekly.hw.pred[, "upr"], weekly.hw.pred[, "lwr"],
        lty = c(1, 2, 3, 3),
        col = c("black", "#009260", "grey", "grey"),
        ylab = "log(€/MWh)", lwd = 1.8)

## Check whether Holt-Winters residuals are white noise
acf(resid(weekly.hw), lag.max = 104, main = "ACF: Holt-Winters residuals")


#------------------------------------------------------------------------------#
####                    2.4 Random-Walk Check                               ####
#------------------------------------------------------------------------------#

## ACF of first differences
acf(diff(weekly.ln), lag.max = 104,, main = "ACF: diff(weekly.ln)")


#------------------------------------------------------------------------------#
####                  3. SARIMA Model & Forecast                            ####
#------------------------------------------------------------------------------#

## Using get.best.arima, AIC search to find best SARIMA specification
get.best.arima <- function(x.ts, maxord = c(2,2,2,2,2,2)) {
  best.aic <- 1e8; n <- length(x.ts)
  for (p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
    for (P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6]) 
      { fit <- tryCatch(arima(x.ts, order = c(p,d,q),
                      seas = list(order = c(P,D,Q), frequency(x.ts)), 
                      method = "CSS"),
                      error = function(e) NULL)
      if (is.null(fit)) next
      fit.aic <- -2*fit$loglik + (log(n)+1)*length(fit$coef)
      if (fit.aic < best.aic) { best.aic <- fit.aic; best.fit <- fit
      best.model <- c(p,d,q,P,D,Q) }}
  list(best.aic, best.fit, best.model)}

## Best SARIMA
best <- get.best.arima(weekly.ln, maxord = c(2,2,2,2,2,2))
best  # [1] 1 1 1 0 0 1
best[[3]]

## SARIMA coefficients
best.arima <- arima(weekly.ln, order = best[[3]][1:3],
                    seas = list(order = best[[3]][4:6], 52), method = "ML")
best.arima                                  

## Residual correlogram of the chosen model - closer to white noise
par(mfrow = c(1, 2))
acf(resid(best.arima),lag.max = 104, main = "ACF: SARIMA residuals")
acf(resid(best.arima)^2, lag.max = 104, main = "ACF: Squared SARIMA residuals")
par(mfrow = c(1, 1))   

#------------------------------------------------------------------------------#
####                  3.1 SARIMA 1-Year Forecast                            ####
#------------------------------------------------------------------------------#

## Setting forecast horizon and defining forecast baseline
h  <- 52                                   # forecast horizon: 1 year ahead
fc <- predict(best.arima, n.ahead = h)      

## Whole-series SARIMA forecast, 1 year ahead
fc.pred <- predict(best.arima, n.ahead = 52)
fc  <- exp(ts(fc.pred$pred, start = c(2026,1), frequency = 52))
fcu <- exp(ts(fc.pred$pred + 1.96*fc.pred$se, start = c(2026,1), frequency = 52))
fcl <- exp(ts(fc.pred$pred - 1.96*fc.pred$se, start = c(2026,1), frequency = 52))

## Time Series SARIMA Forecast visualization
ts.plot(weekly.ts, fc, fcu, fcl,
        lty = c(1,2,2,2), col = c("black","#009260","grey","grey"),
        lwd = c(1.2,2,1,1),
        xlim = c(2022, 2027), ylim = c(0, 500),
        ylab = "€/MWh", main = "SARIMA (1,1,1)(0,0,1) Forecast")
abline(v = 2026, lty = 3)


#------------------------------------------------------------------------------#
####         3.2 Forecast Degradation: Pre-Crisis Fit vs. Actual            ####
#------------------------------------------------------------------------------#

## Defining training and comparison window
train <- window(weekly.ln, end = c(2021, 52))    # calm period
h     <- length(weekly.ln) - length(train)
pre   <- arima(train, order = best[[3]][1:3],
               seas = list(order = best[[3]][4:6], 52), method = "ML")
pp    <- predict(pre, n.ahead = h)

## Pre-crisis forecast (dashed) vs. actual (solid)
up <- exp(pp$pred + 1.96*pp$se); lo <- exp(pp$pred - 1.96*pp$se)
ts.plot(weekly.ts,
        exp(ts(pp$pred, start = c(2022,1), frequency = 52)),
        ts(up, start = c(2022,1), frequency = 52),
        ts(lo, start = c(2022,1), frequency = 52),
        lty = c(1,2,3,3), col = c("black","#009260","grey","grey"),
        lwd = c(1.5,2,1,1), ylab = "€/MWh")
abline(v = 2022.15, lty = 3)

## Post Crisis Training Window starting May 2023
train <- window( weekly.ln,start = c(2023, 5),end = c(2025, 52))

h <- 52

post <- arima(train,order = c(1, 1, 1),method = "ML")

pp <- predict( post, n.ahead = h)

fc <- exp(pp$pred)


#------------------------------------------------------------------------------#
####            3.3 Different Forecast Training Windows                     ####
#------------------------------------------------------------------------------#

## Defining conditional standard deviation
cond.sd <- ts(weekly.garch$fitted.values[, 1],
              start = start(weekly.ln), frequency = 52)

## Visualizing conditional standard deviation
plot(cond.sd, ylab = "Conditional SD (log scale)",
     main = "Fitted GARCH Conditional Volatility")
abline(v = 2022.15, lty = 2, col = "#009260", lwd = 2)   

## Full Period Window, starting 1.October, 2018
full.fit <- arima(weekly.ln, order = c(1,1,1),
                  seas = list(order = c(0,0,1), 52), method = "ML")        

## Post Crisis Window starting December 2023
post.tr  <- window(weekly.ln, start = c(2023, 12))                        
post.fit <- arima(post.tr, order = c(1,1,1),
                  seas = list(order = c(0,0,1), 52), method = "ML")        

## Forecast 1 year ahead, back-transformed to €/MWh
h  <- 52
fp <- predict(full.fit, n.ahead = h); pp <- predict(post.fit, n.ahead = h)
mk <- function(x) exp(ts(x, start = c(2026,1), frequency = 52))
full.fc <- mk(fp$pred); full.u <- mk(fp$pred+1.96*fp$se); full.l <- mk(fp$pred-1.96*fp$se)
post.fc <- mk(pp$pred); post.u <- mk(pp$pred+1.96*pp$se); post.l <- mk(pp$pred-1.96*pp$se)

## POST-crisis model 
post.tr  <- window(weekly.ln, start = c(2023, 27))          # ~2.5 yrs 
post.fit <- arima(post.tr, order = c(1,1,1),
                  seas = list(order = c(0,0,1), 52), method = "ML")
print(post.fit)                                            

## Forecast Perimeter
h  <- 52
pp <- predict(post.fit, n.ahead = h)
mk <- function(x) exp(ts(x, start = c(2026,1), frequency = 52))
post.fc <- mk(pp$pred); post.u <- mk(pp$pred+1.96*pp$se); post.l <- mk(pp$pred-1.96*pp$se)

## Plotting post crisis model
plot(weekly.ts, type="n", xlim=c(2024,2027), ylim=c(0,350), xlab="Time", ylab="€/MWh",
     main="Post-crisis model (1,1,1)(0,1,1)[52]")
lines(weekly.ts, col="grey", lwd=1)
polygon(c(time(post.fc), rev(time(post.fc))), c(post.u, rev(post.l)),
        col=adjustcolor("#009260", alpha.f=0.18), border=NA)
lines(post.fc, col="#009260", lwd=2.5)
abline(v=2026, lty=3)


#------------------------------------------------------------------------------#
####               4. Volatility: GARCH and ARCH                            ####
#------------------------------------------------------------------------------#.

## SARIMA residuals
res <- na.omit(resid(best.arima))            

## Fit GARCH(1,1) on the residuals
weekly.garch <- garch(res, trace = FALSE, grad = "numerical")

## Coefficients with 95% CIs
confint(weekly.garch)

## Residual check and visualization
weekly.garch.res <- resid(weekly.garch)[-1]  # first value is NA by construction
par(mfrow = c(1, 2))
acf(weekly.garch.res,   lag.max = 52, main = "ACF: GARCH residuals")
acf(weekly.garch.res^2, lag.max = 52, main = "ACF: Squared GARCH residuals")
par(mfrow = c(1, 1))
# BOTH approximately white noise ->  GARCH has absorbed the volatility

#------------------------------------------------------------------------------#
####                  4.1 GARCH on SARIMA residuals                         ####
#------------------------------------------------------------------------------#

res <- na.omit(resid(best.arima))

## PLOT 12a: residual ACF vs SQUARED-residual ACF
par(mfrow = c(1, 2))
acf(res,  lag = 52, main = "ACF: SARIMA residuals")
acf(res^2, lag = 52, main = "ACF: Squared SARIMA residuals")     # correlated: volatility clustering
par(mfrow = c(1, 1))

## GARCH(1,1) fit and diagnostics
weekly.garch <- garch(res, trace = FALSE, grad = "numerical")
confint(weekly.garch)                              # a1 significant, b1 not


## PLOT 12b: fitted conditional standard deviation
cond.sd <- ts(weekly.garch$fitted.values[, 1],
              start = start(weekly.ln), frequency = 52)
plot(cond.sd, ylab = "Conditional SD (log)", main = "Fitted GARCH volatility")
abline(v = 2022.15, lty = 2, col = "#009260")


#------------------------------------------------------------------------------#
####              4.2 Fitted Conditional Volatility                         ####
#------------------------------------------------------------------------------#

## Defining conditional standard deviation
cond.sd <- ts(weekly.garch$fitted.values[, 1],
              start = start(weekly.ln), frequency = 52)

## Visualizing conditional standard deviation
plot(cond.sd, ylab = "Conditional SD (log scale)",
     main = "Fitted GARCH Conditional Volatility")
abline(v = 2022.15, lty = 2, col = "#009260", lwd = 2)   


#------------------------------------------------------------------------------#
####                 5. All Plots and Visualizations                        ####
#------------------------------------------------------------------------------#

# Plots and output replication for slides in "Time_Series_Presentation.pptx"


## PLOT 1: Observed series
plot(weekly.ts, ylab = "€/MWh", xlab = "Time")

## PLOT 2: Level vs. log series
par(mfrow = c(1, 2))
plot(weekly.ts, ylab = "€/MWh", main = "Level Series")
plot(weekly.ln, ylab = "log(€/MWh)", main = "Log Series")
par(mfrow = c(1, 1))

## PLOT 3: Full decomposition visualization
plot(weekly.dec)

## PLOT 3.1 & 3.2: Separate illustration of trend and seasonal component
plot(weekly.dec$trend, ylab = "Trend")
plot(weekly.dec$seasonal, ylab = "Seasonal")

## PLOT 4: Visualization of trend and trend+seasonal component in one graph
Trend    <- weekly.dec$trend
Seasonal <- weekly.dec$seasonal
ts.plot(cbind(Trend, Trend + Seasonal), lty= 1:2,  ylab = "log(€/MWh)")

## PLOT 5: Random component
plot(weekly.dec$random, ylab = "Random component")

# PLOT 6: ACF of Random Component
acf (weekly.random, lag.max = 104)

## PLOT 6.1: ACF and PACF together
par(mfrow = c(1, 2))
acf (weekly.random, lag.max = 104, main = "ACF: random component")
pacf(weekly.random, lag.max = 104, main = "PACF: random component")
par(mfrow = c(1, 1))

## PLOT 7: log series + OLS line.
plot(weekly.ln, type = "l", ylab = "log(€/MWh)", xlab = "Time",
     main = "Log weekly prices with linear trend")
abline(weekly.lm.ln, col = "#009260", lwd = 2.5)

## PLOT 7.1: residual correlogram - justifies AR(1) step
acf(resid(weekly.lm.ln), lag.max = 104, main = "ACF: OLS residuals")

## Plot 8: actual vs pre-crisis forecast, y-axis fixed so it stays readable
plot(weekly.ts, type = "n", xlim = c(2023, 2027), ylim = c(0, 600),
     ylab = "€/MWh", xlab = "Time", main = "Pre-Crisis Model vs. Reality")
lines(weekly.ts, col = "grey30", lwd = 1)
lines(fc, col = "#009260", lwd = 2.5, lty = 2)
abline(v = 2021, lty = 3)

## PLOT 9: Holt Winters (HW) with fitted values
plot(weekly.hw)

## PLOT 9.1: 2-year HW forecast with prediction interval
ts.plot(weekly.ln, weekly.hw.pred[, "fit"],
        weekly.hw.pred[, "upr"], weekly.hw.pred[, "lwr"],
        lty = c(1, 2, 3, 3),
        col = c("black", "#009260", "grey", "grey"),
        ylab = "log(€/MWh)", lwd = 1.8)

## PLOT 9.2: Holt-Winters residuals structure
acf(resid(weekly.hw), lag.max = 104, main = "ACF: Holt-Winters residuals")

## PLOT 10: ACF of first differences
acf(diff(weekly.ln), lag.max = 104,, main = "ACF: diff(weekly.ln)")

## PLOT 11: Residual correlogram of the chosen model
par(mfrow = c(1, 2))
acf(resid(best.arima),lag.max = 104, main = "ACF: SARIMA residuals")
acf(resid(best.arima)^2, lag.max = 104, main = "ACF: Squared SARIMA residuals")
par(mfrow = c(1, 1))   

## PLOT 12: Time Series SARIMA Forecast visualization
ts.plot(weekly.ts, fc, fcu, fcl,
        lty = c(1,2,2,2), col = c("black","#009260","grey","grey"),
        lwd = c(1.2,2,1,1),
        xlim = c(2022, 2027), ylim = c(0, 500),
        ylab = "€/MWh", main = "SARIMA (1,1,1)(0,0,1) Forecast")
abline(v = 2026, lty = 3)

## PLOT 13: Post-crisis ARIMA forecast visualization
plot(weekly.ts,type = "l",
     xlim = c(2023, 2027),
     ylim = c(0, 600),
     ylab = "€/MWh",
     xlab = "Time",
     main = "Post-Crisis ARIMA Forecast")
lines(fc, col = "#009260",lwd = 2.5,lty = 2)

## PLOT 14: Comparison of full-period and after-crisis model
par(mfrow = c(1, 2))
draw(full.fc, full.u, full.l, "#009260",  "Full-period model")
draw(post.fc, post.u, post.l, "#009260", "After-crisis model - April 2023")
par(mfrow = c(1, 1))


#------------------------------------------------------------------------------#
####                   5.1 Slides Reproducibility                           ####
#------------------------------------------------------------------------------#

## SLIDE 2: Overview
head(weekly[c("Datum von", "Datum bis", "Deutschland/Luxemburg [€/MWh]")], 3)

## Slide 9: OLS regression output
stargazer(weekly.ols, type = "text",
          dep.var.labels = "log weekly electricity price",
          keep = c("Tc", "Crisis"), digits = 3,
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes = "Report GLS CIs (intervals(weekly.gls)); OLS SEs biased")

## SLIDE 14: Numerical acf values at lag 1 and lag 4
acf(weekly.random)$acf[2]     # lag 1 (previous week)      
acf(weekly.random)$acf[5]     # lag 4 (one month earlier) 

## SLIDE 23: Coefficients with 95% CIs
confint(weekly.garch)

#------------------------------------------------------------------------------#
####                            6.References                                ####
#------------------------------------------------------------------------------#

#-----------------------------------Data---------------------------------------#
# Bundesnetzagentur. SMARD: Electricity and gas market data. 
# https://www.smard.de/home, accessed June 23rd,2026.


#----------------------------Method and Code-----------------------------------#
#  Cowpertwait, P.S.P. & Metcalfe, A.V. (2009),
# "Introductory Time Series with R", Springer.
