install.packages("dpylr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("moments")
install.packages("GGally")
install.packages("readxl")
install.packages("lmtest")
install.packages("car")
install.packages("tseries")
install.packages("stargazer")
install.packages("caret")
install.packages("randomForest")
install.packages("forecast")
install.packages("vars")
library(dplyr)
library(ggplot2)
library(moments)
library(corrplot)
library(GGally)
library(readxl)
library(stargazer)
library(lmtest)
library(readxl)
library(car)
library(tseries)
library(caret)
library(forecast)
library(vars)
Remittance_inflow<-read.table(pipe("pbpaste"),sep="\t",header=TRUE)
str(Remittance_inflow)
head(Remittance_inflow)
tail(Remittance_inflow)
summary(Remittance_inflow)
skewness(Remittance_inflow$Remittance)
skewness(Remittance_inflow$GDP)
class(Remittance_inflow$GDP)
Remittance_inflow$GDP <- as.numeric(as.character(Remittance_inflow$GDP))
which(is.na(as.numeric(as.character(Remittance_inflow$GDP))))
Remittance_inflow[which(is.na(as.numeric(as.character(Remittance_inflow$GDP)))), ]
str(Remittance_inflow$GDP)
skewness(Remittance_inflow$GDP, na.rm = TRUE)
class(Remittance_inflow$Net.migration)
Remittance_inflow$Net.migration <- as.numeric(Remittance_inflow$Net.migration)
skewness(Remittance_inflow$Net.migration)
unique(Remittance_inflow$Net.migration)
bad_values <- Remittance_inflow$Net.migration[
  is.na(as.numeric(Remittance_inflow$Net.migration))
]
unique(bad_values)
Remittance_inflow[which(is.na(as.numeric(as.character(Remittance_inflow$Net.migration)))), ]
str(Remittance_inflow$Net.migration)
skewness(Remittance_inflow$GDP, na.rm = TRUE)
skewness(Remittance_inflow$Inflation)
skewness(Remittance_inflow$Exchange.rate.US..to.NPR.)
dim(Remittance_inflow)
str(Remittance_inflow)
hist(Remittance_inflow$Remittance)
hist(Remittance_inflow$GDP)
hist(Remittance_inflow$Net.migration)
hist(Remittance_inflow$Inflation)
hist(Remittance_inflow$Exchange.rate.US..to.NPR.)
plot(Remittance~Net.migration,data=Remittance_inflow)
plot(Remittance~Exchange.rate.US..to.NPR.,data=Remittance_inflow)
cor_matrix <- cor(Remittance_inflow[, c("Remittance",
                                        "Net.migration",
                                        "Exchange.rate.US..to.NPR.",
                                        "GDP",
                                        "Inflation")])
print(cor_matrix)
ggplot(Remittance_inflow,aes(Year,Remittance))+
  geom_line()+
  geom_point()
ggplot(Remittance_inflow,aes(Year,GDP))+
  geom_line()
ggplot(Remittance_inflow,aes(Year,Exchange.rate.US..to.NPR.))+
  geom_line()

corrplot(cor_matrix)
ggplot(Remittance_inflow,
       aes(y = Remittance)) +
  geom_boxplot()
ggplot(Remittance_inflow,
       aes(y = GDP)) +
  geom_boxplot()
ggplot(Remittance_inflow,
       aes(x = Remittance,
           y = Inflation)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(Remittance_inflow,
       aes(x = Remittance,
           y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm")
getwd()
stats <- data.frame(
  Variable = c("Remittance","GDP","NetMigration","Inflation","ExchangeRate"),
  
  Mean = c(mean(Remittance_inflow$Remittance),
           mean(Remittance_inflow$GDP),
           mean(Remittance_inflow$Net.migration),
           mean(Remittance_inflow$Inflation),
           mean(Remittance_inflow$Exchange.rate.US..to.NPR.)),
  
  Median = c(median(Remittance_inflow$Remittance),
             median(Remittance_inflow$GDP),
             median(Remittance_inflow$Net.migration),
             median(Remittance_inflow$Inflation),
             median(Remittance_inflow$Exchange.rate.US..to.NPR.)),
  
  SD = c(sd(Remittance_inflow$Remittance),
         sd(Remittance_inflow$GDP),
         sd(Remittance_inflow$Net.migration),
         sd(Remittance_inflow$Inflation),
         sd(Remittance_inflow$Exchange.rate.US..to.NPR.))
)
print(stats)
library(moments)
kurtosis(Remittance_inflow$Remittance)
kurtosis(Remittance_inflow$GDP)
kurtosis(Remittance_inflow$Net.migration)
kurtosis(Remittance_inflow$Inflation)
kurtosis(Remittance_inflow$Exchange.rate.US..to.NPR.)
library(ggplot2)

ggplot(Remittance_inflow,
       aes(x = Remittance, y = Net.migration)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship between Remittance and Net Migration",
    x = "Remittance",
    y = "Net Migration"
  ) +
  theme_minimal()
library(dplyr)

data_plot <- Remittance_inflow %>%
  filter(!is.na(Year),
         !is.na(Remittance),
         !is.na(Net.migration))


ggplot(Remittance_inflow,
       aes(x = Net.migration,
           y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(Remittance_inflow,
       aes(x = Exchange.rate.US..to.NPR.,
           y = Remittance)) +
  geom_point() +
  geom_smooth(method = "lm")
ggpairs(Remittance_inflow[, c("Remittance",
                              "GDP",
                              "Inflation",
                              "Net.migration",
                              "Exchange.rate.US..to.NPR.")])
colSums(is.na(Remittance_inflow))
shapiro.test(Remittance_inflow$Remittance)
shapiro.test(Remittance_inflow$GDP)
shapiro.test(Remittance_inflow$Inflation)
ggplot(Remittance_inflow,
       aes(Year, Inflation)) +
  geom_line() +
  geom_point()
ggplot(Remittance_inflow,
       aes(Year, Net.migration)) +
  geom_line() +
  geom_point()
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)
boxplot.stats(Remittance_inflow$Remittance)$out
boxplot.stats(Remittance_inflow$GDP)$out
boxplot.stats(Remittance_inflow$Inflation)$out
cv <- data.frame(
  Variable = c("Remittance","GDP","Inflation","NetMigration","ExchangeRate"),
  
  CV = c(sd(Remittance_inflow$Remittance)/mean(Remittance_inflow$Remittance),
         
         sd(Remittance_inflow$GDP)/mean(Remittance_inflow$GDP),
         
         sd(Remittance_inflow$Inflation)/mean(Remittance_inflow$Inflation),
         
         sd(Remittance_inflow$Net.migration)/mean(Remittance_inflow$Net.migration),
         
         sd(Remittance_inflow$Exchange.rate.US..to.NPR.)/
           mean(Remittance_inflow$Exchange.rate.US..to.NPR.))
)
print(cv)
quantile(Remittance_inflow$Remittance)
quantile(Remittance_inflow$GDP)
quantile(Remittance_inflow$Inflation)
write.csv(stats,"EDA_summary.csv")
file.show("EDA_summary.csv")
var(Remittance_inflow$Remittance)
var(Remittance_inflow$GDP)
var(Remittance_inflow$Inflation)
var(Remittance_inflow$Net.migration)
var(Remittance_inflow$Exchange.rate.US..to.NPR.)

# 1. STATIONARY DATA PREPARATION (The Pro Way)

# Create a clean, unified dataframe of differenced/stationary variables
clean_diff_data <- data.frame(
  Year          = Remittance_inflow$Year[-1], # Align years with differences
  d_Remittance  = diff(Remittance_inflow$Remittance),
  d_Net_mig     = diff(Remittance_inflow$Net.migration),
  d_Ex_rate     = diff(Remittance_inflow$Exchange.rate.US..to.NPR.),
  g_GDP         = diff(log(Remittance_inflow$GDP)),
  g_Remittance  =diff(log(Remittance_inflow$Remittance)),
  d_Inflation   = diff(Remittance_inflow$Inflation)
)
clean_diff_data <- na.omit(clean_diff_data)


# 2. FIXED ECONOMETRIC REGRESSIONS

# Model 1: Remittance Change driven by Migration and Exchange rate changes
model1_fixed <- lm(d_Remittance ~ d_Net_mig + d_Ex_rate, data = clean_diff_data)

# Model 2: GDP Growth driven by Remittance Growth and Exchange rate changes
model2_fixed <- lm(g_GDP ~ g_Remittance + d_Ex_rate, data = clean_diff_data)

# Model 3: Inflation Change driven by Remittance Changes (Stationary version)
model3_fixed <- lm(d_Inflation ~ d_Remittance, data = clean_diff_data)

d_Inflation  <- c(NA, diff(Remittance_inflow$Inflation)) 
g_Remittance <- c(NA, diff(log(Remittance_inflow$Remittance))) 


model3_data  <- data.frame(d_Inflation, g_Remittance)
model3_data  <- na.omit(model3_data)


model3       <- lm(d_Inflation ~ g_Remittance, data = model3_data)

summary(model3)
dwtest(model3)
bptest(model3)

# Model 4: Elasticity Model (GDP Growth vs Remittance Growth)
model4_fixed <- lm(g_GDP ~ g_Remittance, data = clean_diff_data)

# Print beautifully consolidated, statistically sound results
stargazer(model1_fixed, model2_fixed, model3_fixed, model4_fixed,
          type = "text", title = "Corrected Regression Results (Stationary Models)")


# 3. PRO DIAGNOSTIC CHECKS (Neither Overfit nor Underfit Checking)

cat("\n--- MULTICOLLINEARITY (VIF < 5 prevents Overfitting) ---\n")
print(vif(model1_fixed))
print(vif(model2_fixed)

cat("\n--- HETEROSKEDASTICITY (BP Test p > 0.05 is ideal) ---\n")
print(bptest(model1_fixed))
print(bptest(model2_fixed))

cat("\n--- AUTOCORRELATION (DW Test near 2 indicates no tracking errors) ---\n")
print(dwtest(model1_fixed))
print(dwtest(model2_fixed))

cat("\n--- RESIDUAL NORMALITY (JB Test p > 0.05) ---\n")
print(jarque.bera.test(residuals(model1_fixed)))
print(jarque.bera.test(residuals(model2_fixed)))

# ALIGNED MACHINE LEARNING SECTION (Using YOUR Macroeconomic Data)

set.seed(123)

# 1. Use your clean stationary data for ML split
train_index <- createDataPartition(clean_diff_data$g_GDP, p = 0.8, list = FALSE)
train_ml    <- clean_diff_data[train_index, ]
test_ml     <- clean_diff_data[-train_index, ]

# 2. Train a Decision Tree to predict GDP Growth
tune_grid  <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))
tree_model <- train(g_GDP ~ g_Remittance + d_Ex_rate + d_Net_mig + d_Inflation, 
                    data = train_ml, method = "rpart",
                    trControl = trainControl(method = "cv", number = 5),
                    tuneGrid = tune_grid)

# 3. Train a Random Forest to predict GDP Growth
library(randomForest)
rf_ml_model <- randomForest(g_GDP ~ g_Remittance + d_Ex_rate + d_Net_mig + d_Inflation, 
                            data = train_ml, ntree = 500)

# 4. Evaluate both on Unseen Test Data
pred_tree <- predict(tree_model, newdata = test_ml)
pred_rf   <- predict(rf_ml_model, newdata = test_ml)

cat("\n--- MACRO MACHINE LEARNING BENCHMARK ---\n")
cat("Decision Tree Test RMSE (GDP Growth):", RMSE(pred_tree, test_ml$g_GDP), "\n")
cat("Random Forest Test RMSE (GDP Growth):", RMSE(pred_rf, test_ml$g_GDP), "\n")


# 5. MACRO TIME-SERIES: VAR MODEL (Using Stationary Growth/Diff Data)

var_ts_data <- ts(clean_diff_data[, c("g_GDP", "d_Remittance", "d_Ex_rate")], 
                  start = min(clean_diff_data$Year), frequency = 1)

# Select optimal lags cleanly based on stationary data criteria
print(VARselect(var_ts_data)) 

# Fit VAR model safely without trend-overfitting
var_model_fixed <- VAR(var_ts_data, p = 1, type = "const") 
summary(var_model_fixed)

# Reliable Impulse Response Function
irf_result <- irf(var_model_fixed, impulse = "d_Remittance", response = "g_GDP", n.ahead = 5)
plot(irf_result)
remit_ts <- ts(Remittance_inflow$Remittance,
               start = min(Remittance_inflow$Year),
               frequency = 1)
print(remit_ts)
plot(remit_ts)
adf.test(Remittance_inflow$Remittance)
diff_remit <- diff(Remittance_inflow$Remittance)
print(diff_remit)
adf.test(diff_remit)
model_arima <- auto.arima(remit_ts)
summary(model_arima)
future <- forecast(model_arima,
                   h = 5)
print(future)
plot(future)
grangertest(Remittance ~ Exchange.rate.US..to.NPR.,
            order = 2,
            data = Remittance_inflow)
grangertest(Remittance ~ Net.migration,
            order = 2,
            data = Remittance_inflow)
var_data <- Remittance_inflow[, c("Remittance",
                                  "GDP",
                                  "Inflation")]
var_data <- na.omit(var_data)
colSums(is.na(var_data))
var_data <- ts(var_data,
               start = min(Remittance_inflow$Year),
               frequency = 1)
VARselect(var_data)
var_model <- VAR(var_data,
                 p = 1,
                 type = "const")
summary(var_model)
irf_result <- irf(var_model)
print(irf_result$irf)

# OPTIMIZED VAR MODEL (Preventing Overfitting via Stationarity)

library(vars)

# 1. Create a clean stationary matrix for this specific variable combination
# We use log-differences for large values (GDP, Remittances) to get growth rates
# We use standard difference for the exchange rate
var_optimal_data <- data.frame(
  g_GDP        = diff(log(Remittance_inflow$GDP)),
  g_Remittance = diff(log(Remittance_inflow$Remittance)),
  d_Ex_rate    = diff(Remittance_inflow$Exchange.rate.US..to.NPR.)
)

# Remove any missing values caused by differencing
var_optimal_data <- na.omit(var_optimal_data)

# Convert to a Time Series object for the VAR package
var_ts_optimal <- ts(var_optimal_data, 
                     start = min(Remittance_inflow$Year) + 1, 
                     frequency = 1)

# 2. Check the optimal lag selection automatically (Prevents Underfitting)
# This selects the best lag 'p' using AIC/BIC criteria so we don't guess blindly
lag_selection <- VARselect(var_ts_optimal, lag.max = 4, type = "const")
print(lag_selection)

# 3. Fit the Optimized VAR Model
# (Using p = 1 or the optimal lag suggested by AIC above)
var_model_optimal <- VAR(var_ts_optimal, p = 1, type = "const")
summary(var_model_optimal)

# 4. Generate a Reliable, Unbiased Impulse Response Function (IRF)
# This tracks how a shock in Remittances truly impacts GDP Growth over 5 years
irf_optimal <- irf(var_model_optimal, 
                   impulse = "g_Remittance", 
                   response = "g_GDP", 
                   n.ahead = 5, 
                   boot = TRUE)

# Plot the clean, optimal IRF
plot(irf_optimal)

# 5. Variance Decomposition (FEVD) to see percentage contributions
fevd_optimal <- fevd(var_model_optimal, n.ahead = 5)
plot(fevd_optimal)
accuracy(model_arima)
checkresiduals(model_arima)
gdp_ts <- ts(Remittance_inflow$GDP,
             start=min(Remittance_inflow$Year),
             frequency=1)
gdp_model <- auto.arima(gdp_ts)
sum(is.na(gdp_ts))
class(gdp_ts)
length(gdp_ts)
summary(gdp_ts)
sum(is.infinite(gdp_ts))
head(gdp_ts)
tail(gdp_ts)
summary(gdp_model)
str(gdp_ts)
summary(gdp_ts)
sum(is.infinite(gdp_ts))
any(gdp_ts <= 0)
plot(gdp_ts)
summary(gdp_ts)
sum(is.infinite(gdp_ts))
library(forecast)
any(gdp_ts <= 0)
typeof(gdp_ts)
mode(gdp_ts)
class(gdp_ts)
is.numeric(gdp_ts)
gdp_ts <- ts(as.numeric(gdp_ts),
             start = 1993,
             frequency = 1)
is.numeric(gdp_ts)
gdp_model <- Arima(gdp_ts, order = c(1,1,1))
future_gdp <- forecast(gdp_model,
                       h=5)
plot(future_gdp)
fevd_result <- fevd(var_model)
plot(fevd_result)
library(ggplot2)

tail(Remittance_inflow[, c("Year", "Remittance", "Net.migration")])
library(dplyr)
library(ggplot2)

plot_data <- Remittance_inflow %>%
  filter(!is.na(Net.migration)) %>%
  mutate(
    Remittance_z = scale(Remittance),
    NetMigration_z = scale(Net.migration)
  )

ggplot(plot_data) +
  geom_line(aes(Year, Remittance_z, colour = "Remittance"), linewidth = 1) +
  geom_line(aes(Year, NetMigration_z, colour = "Net Migration"), linewidth = 1) +
  labs(
    title = "Standardized Remittance and Net Migration",
    x = "Year",
    y = "Standardized Value (Z-score)"
  ) +
  theme_minimal()    
