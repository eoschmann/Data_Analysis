####                        Supplementary R Script                          ####
#------------------------------------------------------------------------------#
###                       OLS Bivariate Regressions                          ###
#------------------------------------------------------------------------------#

#------------------------------Research Question-------------------------------#

# Is rural electricity access determined by government effectiveness?

#------------------------------------------------------------------------------#
####                                1. Setup                                ####
#------------------------------------------------------------------------------#

if(!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")}
library("ggplot2")
if(!require("stargazer")) {install.packages("stargazer"); library("stargazer")}
library("stargazer")
if(!require("coefplot")) {install.packages("coefplot"); library("coefplot")}
library("coefplot")
if(!require("ggeffects")) {install.packages("ggeffects"); library("ggeffects")}
library("ggeffects")
if(!require("car")) {install.packages("car"); library("car")}
library("car")
if(!require("rio")) {install.packages("rio"); library("rio")}
library("rio")
if(!require("rio")) {install.packages("rio"); library("rio")}
library("rio")
if(!require("Rmisc")) {install.packages("Rmisc"); library("Rmisc")}
library("Rmisc")
if(!require("knitr")) {install.packages("knitr"); library("knitr")}
library("knitr")
if(!require("kableExtra")) {install.packages("kableExtra"); library("kableExtra")}
library("kableExtra")
if(!require("patchwork")) {install.packages("patchwork"); library("patchwork")}
library("patchwork")
if(!require("lmtest")) {install.packages("lmtest"); library("lmtest")}
library("lmtest")
if(!require("formatR")) {install.packages("formatR"); library("formatR")}
library("formatR")
if(!require("jtools")) {install.packages("jtools"); library("jtools")}
library("jtools")
if(!require("readr")) {install.packages("readr"); library("readr")}
library("readr")

# Load dataset 
dta <- read_csv("Downloads/THE_DATASET_EO(1).csv", na = c("NA", ".."))

# Rename variables 
newnames <- c(
  "country", "country_code", "elec_rural_perc", "gov_eff", 
  "gdp_pc", "grid_loss", "elec_urban_perc", "elec_total", 
  "rural_pop_share", "urban_pop_share", "pop_dens")

# Apply the names
colnames(dta) <- newnames


#------------------------------------------------------------------------------#
####                    1.1 Preliminary Data Check                          ####
#------------------------------------------------------------------------------#

# Check that values are numerical
is.numeric(dta$elec_rural_perc)
is.numeric(dta$gov_eff)

# Dimensions of the data
dim(dta)

# Creating new variable for logGDP
dta$logGDP <-  log(dta$gdp_pc)


# Checking the DV
summary(dta$elec_rural_perc)
dta$country[which.min(dta$elec_rural_perc)]
min(dta$elec_rural_perc, na.rm=TRUE) 
sum(dta$elec_rural_perc == 100, na.rm = TRUE)

# Subset for rural electrification below 100%
dta_low <- subset(dta ,elec_rural_perc < 100)

# Comparing full data and subset distribution
par(mfrow = c(1, 2)) 
boxplot(dta$elec_rural_perc, 
        main = "Full Sample", 
        col = "lightgrey", 
        ylim = c(0 , 100))
boxplot(dta_low$elec_rural_perc, 
        main = "Subset < 100%", 
        col = "lightgreen", 
        ylim = c(0 , 100))

# Visualization of dependent variable (DV)
ggplot(data = dta_low, mapping = aes(x = elec_rural_perc)) +
  geom_density(bw = 1, fill = "lightblue", alpha = 1) +
  theme_minimal() +
  xlab("Rural Electricity Access %") + ylab("Density") +
  ggtitle("Distribution of Rural Electricity Access") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + ggtitle("Density Plot Rural Access")

# Visualization of independent variable (IV)
par(mfrow = c(1, 2)) 
boxplot(dta$gov_eff, 
        main = "Full Sample", 
        col = "lightgrey", 
        ylim = c(-2.5, 2.5))
boxplot(dta_low$gov_eff, 
        main = "Subset < 100%", 
        col = "lightgreen", 
        ylim = c(-2.5,2.5))

# Countries with best and worst rated government efficiency
dta$country[which.max(dta$gov_eff)]
max(dta$gov_eff, na.rm=TRUE)

dta$country[which.min(dta$gov_eff)]
min(dta$gov_eff, na.rm=TRUE)

# Best and worst rated countries in the subset <100%
dta_low$country[which.max(dta_low$gov_eff)]
max(dta_low$gov_eff, na.rm=TRUE)

dta_low$country[which.min(dta_low$gov_eff)]
min(dta_low$gov_eff, na.rm=TRUE)

# Visualization of distribution of government effectiveness scores
p1 <- ggplot(data = dta, mapping = aes(x = gov_eff)) +
  geom_histogram(binwidth = 1, fill = "grey",
                 col = "black", alpha = 0.1) +
  theme_minimal() +
  ggtitle("Full Sample") + 
  xlab("Government Effectiveness Score") + ylab("Number of countries") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  geom_vline(xintercept = median(dta$gov_eff, na.rm=TRUE), col = "red") +
  geom_vline(xintercept = mean(dta$gov_eff, na.rm=TRUE), col = "blue") +
  geom_vline(xintercept = mean(dta$gov_eff, na.rm=TRUE) +
               sd(dta$gov_eff, na.rm=TRUE), col = "darkorange", lty = 2) +
  geom_vline(xintercept = mean(dta$gov_eff, na.rm=TRUE)-
               sd(dta$gov_eff, na.rm=TRUE), col = "darkorange", lty = 2)

p2 <- ggplot(data = dta_low, mapping = aes(x = gov_eff)) +
  geom_histogram(binwidth = 1, fill = "lightgreen",
                 col = "black", alpha = 0.1) +
  theme_minimal() +
  ggtitle("Subset <100%") + 
  xlab("Government Effectiveness Score") + ylab("Number of countries") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_vline(xintercept = median(dta_low$gov_eff, na.rm=TRUE), col = "red") +
  geom_vline(xintercept = mean(dta_low$gov_eff, na.rm=TRUE), col = "blue") +
  geom_vline(xintercept = mean(dta_low$gov_eff, na.rm=TRUE) +
               sd(dta_low$gov_eff, na.rm=TRUE), col = "darkorange", lty = 2) +
  geom_vline(xintercept = mean(dta_low$gov_eff, na.rm=TRUE)-
               sd(dta_low$gov_eff, na.rm=TRUE), col = "darkorange", lty = 2)

p1 + p2


# Comparing subset and full dataset
summary(dta$gov_eff)
summary(dta_low$gov_eff)


# Correlation Plot 
cor(dta_low$elec_rural_perc,dta_low$gov_eff)
plot(jitter(dta_low$gov_eff, factor = 2), dta_low$elec_rural_perc,
     ylab = "Rural Electrification (%)", xlab = "Government Effectiveness",
     main = "Rural Electrification and Government Effectiveness")




#------------------------------------------------------------------------------#
####                    2. OLS Regression Models                            ####
#------------------------------------------------------------------------------#

# Initial model
model_1 <- lm(elec_rural_perc ~ gov_eff, data = dta_low)
stargazer(model_1, report = "vcsp*")

# For Output in R Console
stargazer(model_1, report = "vcsp*", type = "text")


#------------------------------------------------------------------------------#
####                    2.1 Control Variables                               ####
#------------------------------------------------------------------------------#

# Transmission loss control
model_2 <- lm(elec_rural_perc ~ gov_eff + grid_loss, data = dta_low)
# For R Console Output
stargazer(model_2, report = "vcsp*", type = "text")

# logGDP control variable
model_3 <-  lm(elec_rural_perc ~ gov_eff + logGDP, data = dta_low)
stargazer(model_3, report = "vcsp*")
# For R Console Output
stargazer(model_3, report = "vcsp*", type = "text")

# Multiple control variables (CHECK)
model_4_all <-  lm(elec_rural_perc ~ gov_eff + rural_pop_share + logGDP, data = dta_low)
# For R Console Output
stargazer(model_4_all, report = "vcsp*", type = "text")


# Model Comparison
stargazer(model_1,model_2,model_3, type = "latex",
          dep.var.labels = "Rural Electrification (%)",
          covariate.labels = c("Government Efficiency","Grid Losses",
                               "log GDP per Capita"))



# Coefficient Plot
c2 <- coefplot(model_2,
               intercept = FALSE,
               title = "Coefficient Plot Grid Loss Model",
               xlab = "Estimate with CIs",
               ylab = "Coefficient",
               decreasing = TRUE,
               col = "blue") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
c3 <-  coefplot(model_3,
                intercept = FALSE,
                title = "Coefficient Plot logGDP Model",
                xlab = "Estimate with CIs",
                ylab = "Coefficient",
                decreasing = TRUE,
                col = "blue") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
c2 + c3


# Predicted Values of model_3 (prefered specification)
effect_plot(model_3,
            pred        = logGDP,
            interval    = TRUE,
            int.width   = 0.95,
            x.label     = "logGDP per Capita",
            y.label     = "Predicted Rural Electrification (%)",
            main.title  = "Model 3 - Predicted Values with 95% Confidence Intervals")


#------------------------------------------------------------------------------#
####                    2.2 Interaction Term                                ####
#------------------------------------------------------------------------------#

# Model with interaction term gov_eff * rural_pop_share
model_interact <-  lm(elec_rural_perc ~ gov_eff  + 
                        logGDP + (gov_eff * rural_pop_share), data = dta_low)
summary(model_interact)

# For result visualization
stargazer(model_interact, report = "vcsp*")
stargazer(model_interact, report = "vcsp*", type = "text")


#------------------------------------------------------------------------------#
####          3. Robustness and Assumption Checks                           ####
#------------------------------------------------------------------------------#

# Zero Conditional Mean
residualPlot(model_3, variable = "gov_eff")
residualPlot(model_3, variable = "logGDP")

# Homoskedacity
spreadLevelPlot(model_3, col.lines = "red")
bptest(model_3)
robust_mod_3 <- hccm(model_3, type="hc1")
mod_3_robust <- coeftest(model_3, robust_mod_3)

# Comparison Output
stargazer(model_3, robust_mod_3)

# Multicollinearity
vif(model_2) 
vif(model_3)
