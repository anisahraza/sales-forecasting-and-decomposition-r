
# ============================================================
# 1. LOAD DATA
# ============================================================

# Set working directory to access dataset locally
> setwd("/Users/anisah/Desktop/Semester 2/Stats")

# Load monthly sales data
# Data contains a single column representing observed process values
> data <-read.table("Assessed Data 20-21.txt", col.names=c("process"))

# Extract time series values as numeric vector
> X <- data$process

# Inspect raw time series
> X


# ============================================================
# 2. VISUALISE RAW TIME SERIES
# ============================================================

# Plot full dataset to understand overall structure and variability
> ts.plot(X, main = "Raw Data", xlab = "Time", ylab = "Monthly Sales")

# Optional zoomed view for clearer local pattern inspection
> ts.plot(X[1:100], main = "Raw Data", xlab = "Time", ylab = "Monthly Sales")


# ============================================================
# 3. TREND ESTIMATION (QUADRATIC MODEL)
# ============================================================

# Create time index for regression modelling
> t <- 1:504 # time vector

# Fit quadratic trend model to capture long-term pattern in data
> quadfit <- lm(X~ t + I(t^2))

# Inspect fitted model coefficients
> quadfit


# ============================================================
# 4. LOCAL TREND ESTIMATION (5-POINT MOVING AVERAGE)
# ============================================================

# Define equal weights for local smoothing window
> a5 <- c(1/5, 1/5, 1/5, 1/5, 1/5)

# Initialise vector for storing smoothed trend values
> linear5 <- rep(NA, 504)

# Compute local moving average trend estimate
> for (t in 3:502)
+{
+ linear5[t] <- sum(a5*X[(t-2):(t+2)])
+}

# Inspect smoothed trend estimates
> linear5


# ============================================================
# 5. VISUALISING TREND COMPARISON
# ============================================================

# Compare raw data with fitted quadratic and moving average trends
> ts.plot(X, main = "Raw Data with Trend Estimates", xlab = "Time", ylab = "Monthly Sales")

# Overlay quadratic trend fit
> lines(1:504, quadfit$fitted.values, col = 2)

# Overlay local moving average trend estimate
> lines(linear5, col=3)

# Add legend for interpretation
> legend("topleft", legend = c("Raw Data", "Quadratic Fitted Values", "5 Observations Estimate"),
+ col = c(1,2,3), lty=c(1,1,1))


# ============================================================
# 6. RESIDUAL ANALYSIS (QUADRATIC MODEL)
# ============================================================

# Evaluate model fit by analysing residuals
> ts.plot(X- quadfit$fitted.values, main = "Residuals of Quadratic Estimate", xlab = "Time", ylab = "Residuals")

# Check distribution of residuals
> hist(X- quadfit$fitted.values, main = "Residuals of Quadratic Estimate", xlab = "Residuals", ylab = "Frequency")


# ============================================================
# 7. RESIDUAL ANALYSIS (MOVING AVERAGE MODEL)
# ============================================================

# Compute residuals from moving average trend
> residualsof5OE<-X-linear5

# Visualise residual structure over time
> ts.plot(residualsof5OE, main = "Residuals of Trend Estimates", xlab = "Time", ylab = "Residuals")

# Inspect stability of residuals in earlier segment
> ts.plot(residualsof5OE[1:250], main = "Residuals of Trend Estimates", xlab = "Time", ylab = "Residuals")

# Distribution check of residuals
> hist(residualsof5OE, main = "Residuals of Trend Estimates", xlab = "Residuals", ylab = "Frequency")


# ============================================================
# 8. SEASONAL DECOMPOSITION (PERIOD = 12)
# ============================================================

# Remove trend to isolate seasonal component
> Y<-X- quadfit$fitted.values

# Create seasonal index (monthly grouping)
> a <- rep(1:12,42)

# Compute average seasonal effect for each month
> p <- c(mean(Y[a==1]), mean(Y[a==2]), mean(Y[a==3]), mean(Y[a==4]), + mean(Y[a==5]), mean(Y[a==6]), mean(Y[a==7]), mean(Y[a==8]),
+ mean(Y[a==9]), mean(Y[a==10]), mean(Y[a==11]), mean(Y[a==12]))

# Expand seasonal pattern across full dataset
> seasonalfit <- rep(p, 42)

# Inspect seasonal estimates
> seasonalfit


# ============================================================
# 9. COMBINED TREND + SEASONAL ANALYSIS
# ============================================================

# Visual comparison of detrended data and seasonal structure
> ts.plot(Y, main = "Raw Data (Detrended using Quadratic Fit)", xlab = "Time", ylab = "Residuals")

# Overlay seasonal pattern
> lines(1:504, seasonalfit, col=4)

# Add legend for clarity
> legend("topleft", legend = c("Detrended Data", "Seasonal Estimate"),
+ col = c(1,4), lty=c(1,1))


# ============================================================
# 10. FINAL RESIDUAL CHECKS (MODEL ADEQUACY)
# ============================================================

# Remove trend and seasonality to isolate residual noise
> ts.plot(Y-seasonalfit, main = "Raw Data without Trend and Seasonality", xlab = "Time", ylab = "Residuals")

# First differencing to assess stationarity
> ts.plot(diff(Y-seasonalfit), main = "First Difference Transformation", xlab = "Time", ylab = "Residuals")

# Define detrended + deseasonalised series
> dsX<-Y-seasonalfit

# Second difference transformation for stronger trend removal
> dsX_d2 <-diff(X, differences = 2)

# Visualise transformed series
> ts.plot(dsX_d2, main = "Second Difference Transformation", xlab = "Time", ylab = "Residuals")

# Distribution check
> hist(Y-seasonalfit, main = "Residuals after Trend + Seasonality Removal", xlab = "Residuals", ylab = "Frequency")

# QQ plot for normality assessment
> qqnorm(dsX_d2,main="QQ Plot of Detrended & Deseasonalised Series",xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch=19)
> qqline(dsX_d2)


# ============================================================
# 11. ALTERNATIVE TREND ESTIMATION (15-POINT MOVING AVERAGE)
# ============================================================

# Remove seasonal component
> X_ds <- X – seasonalfit

# Visualise seasonally adjusted series
> ts.plot(X_ds, main = "Seasonally Adjusted Data", xlab = "Time", ylab = "Residuals")

# Define wider smoothing window
> a15 <- c(1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15)

# Initialise trend estimate vector
> linear15 <- rep(NA, 504)

# Compute long-window moving average trend
> for(t in 8:497)
+{
+ linear15[t] <- sum(a15*X[(t-7):(t+7)])
+}

# Inspect estimated long-term trend
> linear15

# Residual diagnostics after full decomposition
> ts.plot(X_ds - linear15, main = "Final Residuals (Full Decomposition)", xlab = "Time", ylab = "Residuals")

# Distribution check
> hist(X_ds - linear15, main = "Final Residual Distribution", xlab = "Residuals", ylab = "Frequency")

# Normality check
> qqnorm(X_ds - linear15,main="QQ Plot (Final Residuals)",xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch=19)
> qqline(X_ds - linear15)
