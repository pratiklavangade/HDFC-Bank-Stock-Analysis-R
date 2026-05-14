
# ============================================================================
# HDFC BANK - COMPLETE FINANCIAL ANALYSIS PROJECT
# Part 1: Data Acquisition, Visualization, Time Series Analysis
# Part 2: Algorithmic Trading
# ============================================================================

# ---- Install & Load Packages ----
packages <- c("quantmod","tidyverse","ggplot2","dplyr","lubridate",
              "tseries","forecast","TTR","PerformanceAnalytics",
              "xts","zoo","writexl","readxl","scales")

for(pkg in packages){
  if(!require(pkg, character.only=TRUE)){
    install.packages(pkg, dependencies=TRUE)
    library(pkg, character.only=TRUE)
  }
}

# ============================================================================
# PART 1A: FINANCIAL DATA ACQUISITION & HANDLING
# ============================================================================

cat("\n========== PART 1A: DATA ACQUISITION ==========\n")

# --- 1. Download HDFC Bank data from Yahoo Finance API (last 10 years) ---
start_date <- Sys.Date() - (10*365)
end_date   <- Sys.Date()

getSymbols("HDFCBANK.NS", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)
hdfc_xts <- HDFCBANK.NS
cat("Data downloaded from Yahoo Finance API\n")
cat("Date range:", as.character(start(hdfc_xts)), "to", as.character(end(hdfc_xts)), "\n")
cat("Total observations:", nrow(hdfc_xts), "\n")

# --- 2. Convert to data frame for easy handling ---
hdfc_df <- data.frame(Date=index(hdfc_xts), coredata(hdfc_xts))
colnames(hdfc_df) <- c("Date","Open","High","Low","Close","Volume","Adjusted")

# --- 3. Save as CSV ---
write.csv(hdfc_df, "HDFC_Bank_Data.csv", row.names=FALSE)
cat("Saved as CSV: HDFC_Bank_Data.csv\n")

# --- 4. Save as Excel ---
writexl::write_xlsx(hdfc_df, "HDFC_Bank_Data.xlsx")
cat("Saved as Excel: HDFC_Bank_Data.xlsx\n")

# --- 5. Read back from CSV ---
hdfc_csv <- read.csv("HDFC_Bank_Data.csv")
hdfc_csv$Date <- as.Date(hdfc_csv$Date)
cat("Read back from CSV: ", nrow(hdfc_csv), " rows\n")

# --- 6. Read back from Excel ---
hdfc_excel <- readxl::read_xlsx("HDFC_Bank_Data.xlsx")
hdfc_excel$Date <- as.Date(hdfc_excel$Date)
cat("Read back from Excel: ", nrow(hdfc_excel), " rows\n")

# --- 7. Data Cleaning ---
cat("\n--- Data Cleaning ---\n")
cat("Missing values per column:\n")
print(colSums(is.na(hdfc_df)))

# Remove rows with NA
hdfc_clean <- na.omit(hdfc_df)
cat("Rows after cleaning:", nrow(hdfc_clean), "\n")

# Check for duplicates
cat("Duplicate rows:", sum(duplicated(hdfc_clean$Date)), "\n")
hdfc_clean <- hdfc_clean[!duplicated(hdfc_clean$Date),]

# Add derived columns
hdfc_clean <- hdfc_clean %>%
  mutate(
    Daily_Return = (Close - lag(Close)) / lag(Close) * 100,
    Log_Return   = log(Close / lag(Close)) * 100,
    Price_Range  = High - Low,
    Year         = year(Date),
    Month        = month(Date, label=TRUE),
    Day          = wday(Date, label=TRUE)
  )

# Summary statistics
cat("\n--- Summary Statistics ---\n")
print(summary(hdfc_clean[,c("Open","High","Low","Close","Volume","Daily_Return")]))

# ============================================================================
# PART 1B: DATA VISUALIZATION (ggplot2)
# ============================================================================

cat("\n========== PART 1B: DATA VISUALIZATION ==========\n")

# --- Plot 1: Closing Price Line Chart ---
p1 <- ggplot(hdfc_clean, aes(x=Date, y=Close)) +
  geom_line(color="#1E88E5", linewidth=0.5) +
  labs(title="HDFC Bank - Closing Price (10 Years)",
       x="Date", y="Price (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p1)
ggsave("01_closing_price.png", p1, width=12, height=6, dpi=150)
cat("Saved: 01_closing_price.png\n")
cat("\n>> EXPLANATION (Plot 1 - Closing Price Line Chart):\n")
cat("   This line chart shows the daily closing price of HDFC Bank over the last 10 years.\n")
cat("   It helps us visualize the overall long-term price trend - whether the stock has been\n")
cat("   rising (uptrend), falling (downtrend), or moving sideways. Steep upward slopes indicate\n")
cat("   periods of strong growth, while sharp drops indicate market corrections or crashes.\n")
cat("   Investors use this to understand the historical price trajectory before making decisions.\n\n")

# --- Plot 2: Volume Bar Plot ---
hdfc_monthly_vol <- hdfc_clean %>%
  mutate(YM=floor_date(Date,"month")) %>%
  group_by(YM) %>%
  summarise(Avg_Volume=mean(Volume, na.rm=TRUE))

p2 <- ggplot(hdfc_monthly_vol, aes(x=YM, y=Avg_Volume)) +
  geom_bar(stat="identity", fill="#43A047", alpha=0.7) +
  labs(title="HDFC Bank - Monthly Average Trading Volume",
       x="Date", y="Volume") +
  scale_y_continuous(labels=comma) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p2)
ggsave("02_volume_barplot.png", p2, width=12, height=6, dpi=150)
cat("Saved: 02_volume_barplot.png\n")
cat("\n>> EXPLANATION (Plot 2 - Monthly Average Volume Bar Plot):\n")
cat("   This bar chart displays the monthly average trading volume of HDFC Bank shares.\n")
cat("   High volume bars indicate strong market interest (buying/selling activity), often\n")
cat("   occurring during major news events, earnings announcements, or market-wide movements.\n")
cat("   Low volume periods suggest reduced investor interest. Volume confirms price trends -\n")
cat("   a price rise with high volume is more reliable than one with low volume.\n\n")

# --- Plot 3: Candlestick-style OHLC Chart (last 60 days) ---
last60 <- tail(hdfc_clean, 60)
last60$Color <- ifelse(last60$Close >= last60$Open, "green", "red")

p3 <- ggplot(last60) +
  geom_segment(aes(x=Date, xend=Date, y=Low, yend=High, color=Color), linewidth=0.4) +
  geom_segment(aes(x=Date, xend=Date, y=Open, yend=Close, color=Color), linewidth=2) +
  scale_color_identity() +
  labs(title="HDFC Bank - OHLC Chart (Last 60 Trading Days)",
       x="Date", y="Price (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p3)
ggsave("03_ohlc_chart.png", p3, width=12, height=6, dpi=150)
cat("Saved: 03_ohlc_chart.png\n")
cat("\n>> EXPLANATION (Plot 3 - OHLC Candlestick Chart, Last 60 Days):\n")
cat("   This candlestick chart shows Open, High, Low, Close (OHLC) prices for the last 60 trading days.\n")
cat("   GREEN bars = bullish days (Close > Open, price went up during the day).\n")
cat("   RED bars = bearish days (Close < Open, price went down during the day).\n")
cat("   The thin line (wick) shows the full High-Low range; the thick body shows Open-Close range.\n")
cat("   Long wicks indicate high volatility. This is a standard chart used by traders for short-term analysis.\n\n")

# --- Plot 4: Daily Returns Distribution ---
p4 <- ggplot(hdfc_clean, aes(x=Daily_Return)) +
  geom_histogram(bins=80, fill="#7B1FA2", alpha=0.7, color="white") +
  geom_vline(xintercept=0, linetype="dashed", color="red") +
  labs(title="HDFC Bank - Daily Returns Distribution",
       x="Daily Return (%)", y="Frequency") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p4)
ggsave("04_returns_distribution.png", p4, width=10, height=6, dpi=150)
cat("Saved: 04_returns_distribution.png\n")
cat("\n>> EXPLANATION (Plot 4 - Daily Returns Distribution):\n")
cat("   This histogram shows the frequency distribution of daily percentage returns.\n")
cat("   The red dashed line at 0% separates gains (right) from losses (left).\n")
cat("   A bell-shaped (normal) distribution centered slightly above 0 suggests the stock has a\n")
cat("   small positive average daily return. Fat tails (extreme values) indicate occasional large\n")
cat("   gains or losses. The spread (width) of the histogram reflects the stock's volatility -\n")
cat("   wider distribution = higher risk. This is essential for risk assessment.\n\n")

# --- Plot 5: Yearly Average Close Price ---
yearly_avg <- hdfc_clean %>% group_by(Year) %>%
  summarise(Avg_Close=mean(Close, na.rm=TRUE))

p5 <- ggplot(yearly_avg, aes(x=factor(Year), y=Avg_Close)) +
  geom_bar(stat="identity", fill="#FF7043", alpha=0.8) +
  geom_text(aes(label=round(Avg_Close,0)), vjust=-0.5, size=3) +
  labs(title="HDFC Bank - Yearly Average Closing Price",
       x="Year", y="Avg Close (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p5)
ggsave("05_yearly_avg_price.png", p5, width=10, height=6, dpi=150)
cat("Saved: 05_yearly_avg_price.png\n")
cat("\n>> EXPLANATION (Plot 5 - Yearly Average Closing Price):\n")
cat("   This bar chart compares the average closing price of HDFC Bank for each year.\n")
cat("   Rising bars year-over-year indicate consistent long-term growth in the stock price.\n")
cat("   Any dip (lower bar than previous year) highlights years of poor performance, often\n")
cat("   linked to economic events (e.g., COVID-19 in 2020). The values on top of each bar\n")
cat("   show the exact average price, making it easy to calculate year-on-year growth rates.\n\n")

# --- Plot 6: Moving Averages ---
hdfc_clean <- hdfc_clean %>%
  arrange(Date) %>%
  mutate(
    SMA_50  = SMA(Close, n=50),
    SMA_200 = SMA(Close, n=200)
  )

p6 <- ggplot(hdfc_clean, aes(x=Date)) +
  geom_line(aes(y=Close, color="Close"), linewidth=0.4) +
  geom_line(aes(y=SMA_50, color="SMA 50"), linewidth=0.6) +
  geom_line(aes(y=SMA_200, color="SMA 200"), linewidth=0.6) +
  scale_color_manual(values=c("Close"="grey50","SMA 50"="#1E88E5","SMA 200"="#E53935")) +
  labs(title="HDFC Bank - Price with 50 & 200 Day Moving Averages",
       x="Date", y="Price (INR)", color="Legend") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p6)
ggsave("06_moving_averages.png", p6, width=12, height=6, dpi=150)
cat("Saved: 06_moving_averages.png\n")
cat("\n>> EXPLANATION (Plot 6 - 50 & 200 Day Moving Averages):\n")
cat("   This chart overlays the 50-day SMA (short-term trend) and 200-day SMA (long-term trend)\n")
cat("   on the closing price. Moving averages smooth out daily noise to reveal the true trend.\n")
cat("   GOLDEN CROSS: When SMA-50 crosses ABOVE SMA-200 = bullish signal (potential uptrend).\n")
cat("   DEATH CROSS:  When SMA-50 crosses BELOW SMA-200 = bearish signal (potential downtrend).\n")
cat("   When the price is above both SMAs, the stock is in a strong uptrend. This is one of the\n")
cat("   most widely used technical indicators by traders and analysts worldwide.\n\n")

# --- Plot 7: Bollinger Bands (last 1 year) ---
hdfc_1yr <- tail(hdfc_clean, 252)
bb <- BBands(hdfc_1yr$Close, n=20, sd=2)
hdfc_1yr$BB_Up   <- bb[,"up"]
hdfc_1yr$BB_Mid  <- bb[,"mavg"]
hdfc_1yr$BB_Down <- bb[,"dn"]

p7 <- ggplot(hdfc_1yr, aes(x=Date)) +
  geom_ribbon(aes(ymin=BB_Down, ymax=BB_Up), fill="#90CAF9", alpha=0.3) +
  geom_line(aes(y=Close, color="Close"), linewidth=0.5) +
  geom_line(aes(y=BB_Mid, color="Middle Band"), linetype="dashed") +
  scale_color_manual(values=c("Close"="black","Middle Band"="blue")) +
  labs(title="HDFC Bank - Bollinger Bands (Last 1 Year)",
       x="Date", y="Price (INR)", color="") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p7)
ggsave("07_bollinger_bands.png", p7, width=12, height=6, dpi=150)
cat("Saved: 07_bollinger_bands.png\n")
cat("\n>> EXPLANATION (Plot 7 - Bollinger Bands, Last 1 Year):\n")
cat("   Bollinger Bands consist of a 20-day moving average (middle band) and upper/lower bands\n")
cat("   at +/- 2 standard deviations. The shaded area represents the band width.\n")
cat("   When price touches the UPPER band = stock may be overbought (potential sell signal).\n")
cat("   When price touches the LOWER band = stock may be oversold (potential buy signal).\n")
cat("   Narrow bands (squeeze) = low volatility, often followed by a big price move.\n")
cat("   Wide bands = high volatility period. About 95% of price action stays within the bands.\n\n")

# ============================================================================
# PART 1C: BASIC TIME SERIES ANALYSIS
# ============================================================================

cat("\n========== PART 1C: TIME SERIES ANALYSIS ==========\n")

# --- Create monthly time series ---
monthly_data <- hdfc_clean %>%
  mutate(YM=floor_date(Date, "month")) %>%
  group_by(YM) %>%
  summarise(Avg_Close=mean(Close, na.rm=TRUE)) %>%
  arrange(YM)

ts_monthly <- ts(monthly_data$Avg_Close,
                 start=c(year(min(monthly_data$YM)), month(min(monthly_data$YM))),
                 frequency=12)

# --- Trend Analysis ---
cat("\n--- Trend Analysis ---\n")
# Save to file
png("08_trend_analysis.png", width=1200, height=600, res=150)
plot(ts_monthly, main="HDFC Bank - Monthly Avg Close Price Trend",
     ylab="Price (INR)", xlab="Year", col="blue", lwd=1.5)
trend_line <- lm(ts_monthly ~ time(ts_monthly))
abline(trend_line, col="red", lwd=2, lty=2)
legend("topleft", c("Price","Trend"), col=c("blue","red"), lty=c(1,2), lwd=2)
dev.off()
# Display in RStudio Plots pane
plot(ts_monthly, main="HDFC Bank - Monthly Avg Close Price Trend",
     ylab="Price (INR)", xlab="Year", col="blue", lwd=1.5)
abline(trend_line, col="red", lwd=2, lty=2)
legend("topleft", c("Price","Trend"), col=c("blue","red"), lty=c(1,2), lwd=2)
cat("Saved: 08_trend_analysis.png\n")
cat("Trend slope:", round(coef(trend_line)[2], 2), "per year\n")
cat("\n>> EXPLANATION (Plot 8 - Trend Analysis):\n")
cat("   This plot shows the monthly average closing price with a linear regression trend line (red).\n")
cat("   The slope of the trend line tells us the average price increase per year. A positive slope\n")
cat("   confirms that HDFC Bank has been on an upward trajectory over the 10-year period.\n")
cat("   Deviations from the trend line show periods where the stock was overvalued (above line)\n")
cat("   or undervalued (below line) relative to its long-term trend.\n\n")

# --- Seasonal Decomposition ---
cat("\n--- Seasonal Decomposition ---\n")
decomp <- decompose(ts_monthly, type="multiplicative")

# Save to file
png("09_seasonal_decomposition.png", width=1200, height=800, res=150)
plot(decomp, col="blue")
title(main="HDFC Bank - Seasonal Decomposition (Multiplicative)")
dev.off()
# Display in RStudio Plots pane
plot(decomp, col="blue")
title(main="HDFC Bank - Seasonal Decomposition (Multiplicative)")
cat("Saved: 09_seasonal_decomposition.png\n")
cat("\n>> EXPLANATION (Plot 9 - Seasonal Decomposition):\n")
cat("   Multiplicative decomposition splits the time series into 4 components:\n")
cat("   1. OBSERVED: Original monthly price data.\n")
cat("   2. TREND: The long-term direction of the stock price (upward/downward).\n")
cat("   3. SEASONAL: Repeating monthly patterns (e.g., stock may perform better in certain months).\n")
cat("   4. RANDOM (Remainder): Irregular fluctuations that cannot be explained by trend or season.\n")
cat("   This helps identify whether price movements are driven by long-term trends, seasonal\n")
cat("   patterns, or random market noise.\n\n")

# --- Stationarity Tests ---
cat("\n--- Stationarity Tests ---\n")
adf_result <- adf.test(na.omit(ts_monthly))
cat("ADF Test p-value:", adf_result$p.value, "\n")
cat("Stationary?", ifelse(adf_result$p.value < 0.05, "Yes", "No - differencing needed"), "\n")

# Difference the series
ts_diff <- diff(ts_monthly)
adf_diff <- adf.test(na.omit(ts_diff))
cat("ADF after differencing p-value:", adf_diff$p.value, "\n")

# --- ACF and PACF ---
# Save to file
png("10_acf_pacf.png", width=1200, height=600, res=150)
par(mfrow=c(1,2))
acf(na.omit(ts_diff), main="ACF - Differenced Series", lag.max=36)
pacf(na.omit(ts_diff), main="PACF - Differenced Series", lag.max=36)
dev.off()
# Display in RStudio Plots pane
par(mfrow=c(1,2))
acf(na.omit(ts_diff), main="ACF - Differenced Series", lag.max=36)
pacf(na.omit(ts_diff), main="PACF - Differenced Series", lag.max=36)
par(mfrow=c(1,1))
cat("Saved: 10_acf_pacf.png\n")
cat("\n>> EXPLANATION (Plot 10 - ACF and PACF):\n")
cat("   ACF (Autocorrelation Function): Shows how the current value correlates with past values.\n")
cat("   Bars exceeding the blue dashed lines are statistically significant correlations.\n")
cat("   PACF (Partial ACF): Shows correlation at each lag after removing effects of shorter lags.\n")
cat("   These plots are used to determine the order (p, q) of the ARIMA model:\n")
cat("   - PACF helps choose 'p' (AR order): Count significant spikes in PACF.\n")
cat("   - ACF helps choose 'q' (MA order): Count significant spikes in ACF.\n")
cat("   The differenced series is used because ARIMA requires stationary data.\n\n")

# --- ARIMA Model ---
cat("\n--- ARIMA Modeling ---\n")
arima_auto <- auto.arima(ts_monthly, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
cat("Best ARIMA model:\n")
print(summary(arima_auto))

# Forecast next 12 months
fc <- forecast(arima_auto, h=12)

# Save to file
png("11_arima_forecast.png", width=1200, height=600, res=150)
plot(fc, main="HDFC Bank - ARIMA Forecast (Next 12 Months)",
     xlab="Year", ylab="Price (INR)", col="blue", lwd=1.5)
dev.off()
# Display in RStudio Plots pane
plot(fc, main="HDFC Bank - ARIMA Forecast (Next 12 Months)",
     xlab="Year", ylab="Price (INR)", col="blue", lwd=1.5)
cat("Saved: 11_arima_forecast.png\n")
cat("\n>> EXPLANATION (Plot 11 - ARIMA Forecast):\n")
cat("   This chart shows the ARIMA model's forecast for the next 12 months.\n")
cat("   The blue line is the historical data; the forecast extends beyond it.\n")
cat("   Dark shaded area = 80% confidence interval (price is 80% likely to fall within this range).\n")
cat("   Light shaded area = 95% confidence interval (wider, more conservative estimate).\n")
cat("   The widening of confidence intervals over time reflects increasing uncertainty\n")
cat("   in longer-term predictions. ARIMA captures patterns from past data to project future values.\n\n")

# Residual diagnostics
# Save to file
png("12_residual_diagnostics.png", width=1200, height=800, res=150)
checkresiduals(arima_auto)
dev.off()
# Display in RStudio Plots pane
checkresiduals(arima_auto)
cat("Saved: 12_residual_diagnostics.png\n")
cat("\n>> EXPLANATION (Plot 12 - Residual Diagnostics):\n")
cat("   This diagnostic plot checks whether the ARIMA model is a good fit:\n")
cat("   1. RESIDUAL PLOT: Residuals should look random with no pattern. Patterns suggest the\n")
cat("      model is missing some structure in the data.\n")
cat("   2. ACF OF RESIDUALS: All bars should be within the blue lines (no significant autocorrelation).\n")
cat("      If bars exceed the lines, the model has not captured all the dependencies.\n")
cat("   3. HISTOGRAM: Residuals should be approximately normally distributed (bell-shaped).\n")
cat("   A good model produces residuals that look like white noise (random, uncorrelated).\n\n")

# ============================================================================
# PART 2: ALGORITHMIC TRADING
# ============================================================================

cat("\n========== PART 2: ALGORITHMIC TRADING ==========\n")

# --- Strategy 1: SMA Crossover (50/200) ---
cat("\n--- Strategy 1: SMA Crossover ---\n")

trade_df <- hdfc_clean %>%
  arrange(Date) %>%
  filter(!is.na(SMA_50) & !is.na(SMA_200)) %>%
  mutate(
    Signal = ifelse(SMA_50 > SMA_200, 1, 0),
    Signal = lag(Signal, 1),
    Strategy_Return = Signal * Daily_Return / 100,
    BuyHold_Return  = Daily_Return / 100
  ) %>%
  filter(!is.na(Signal))

trade_df <- trade_df %>%
  mutate(
    Cum_Strategy = cumprod(1 + replace_na(Strategy_Return, 0)),
    Cum_BuyHold  = cumprod(1 + replace_na(BuyHold_Return, 0))
  )

p8 <- ggplot(trade_df, aes(x=Date)) +
  geom_line(aes(y=Cum_Strategy, color="SMA Crossover Strategy"), linewidth=0.6) +
  geom_line(aes(y=Cum_BuyHold, color="Buy & Hold"), linewidth=0.6) +
  scale_color_manual(values=c("SMA Crossover Strategy"="#1E88E5","Buy & Hold"="#E53935")) +
  labs(title="Strategy 1: SMA 50/200 Crossover vs Buy & Hold",
       x="Date", y="Cumulative Returns", color="") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p8)
ggsave("13_sma_crossover_strategy.png", p8, width=12, height=6, dpi=150)
cat("Saved: 13_sma_crossover_strategy.png\n")
cat("\n>> EXPLANATION (Plot 13 - SMA Crossover Strategy vs Buy & Hold):\n")
cat("   This chart compares cumulative returns of two approaches:\n")
cat("   BLUE LINE (SMA Crossover): Buy when 50-day SMA crosses above 200-day SMA (Golden Cross),\n")
cat("   sell/exit when it crosses below (Death Cross). This is a trend-following strategy.\n")
cat("   RED LINE (Buy & Hold): Simply buy on day 1 and hold throughout the entire period.\n")
cat("   If the blue line is above the red line, the SMA strategy outperformed passive holding.\n")
cat("   The SMA strategy aims to avoid major downturns by exiting during bearish periods,\n")
cat("   but may miss some gains due to delayed signals (lag in moving averages).\n\n")

sma_total <- (tail(trade_df$Cum_Strategy,1) - 1) * 100
bh_total  <- (tail(trade_df$Cum_BuyHold,1) - 1) * 100
cat("SMA Crossover Total Return:", round(sma_total,2), "%\n")
cat("Buy & Hold Total Return:", round(bh_total,2), "%\n")

# --- Strategy 2: RSI Strategy ---
cat("\n--- Strategy 2: RSI Strategy ---\n")

rsi_df <- hdfc_clean %>% arrange(Date) %>%
  mutate(RSI = RSI(Close, n=14)) %>%
  filter(!is.na(RSI)) %>%
  mutate(
    RSI_Signal = case_when(RSI < 30 ~ 1, RSI > 70 ~ 0, TRUE ~ NA_real_)
  )
rsi_df$RSI_Signal <- zoo::na.locf(rsi_df$RSI_Signal, na.rm=FALSE)
rsi_df$RSI_Signal[is.na(rsi_df$RSI_Signal)] <- 0
rsi_df <- rsi_df %>%
  mutate(
    RSI_Signal = lag(RSI_Signal, 1),
    RSI_Return = RSI_Signal * Daily_Return / 100,
    BH_Return  = Daily_Return / 100
  ) %>%
  filter(!is.na(RSI_Signal)) %>%
  mutate(
    Cum_RSI = cumprod(1 + replace_na(RSI_Return, 0)),
    Cum_BH  = cumprod(1 + replace_na(BH_Return, 0))
  )

p9 <- ggplot(rsi_df, aes(x=Date)) +
  geom_line(aes(y=Cum_RSI, color="RSI Strategy"), linewidth=0.6) +
  geom_line(aes(y=Cum_BH, color="Buy & Hold"), linewidth=0.6) +
  scale_color_manual(values=c("RSI Strategy"="#7B1FA2","Buy & Hold"="#E53935")) +
  labs(title="Strategy 2: RSI (14) Trading Strategy vs Buy & Hold",
       x="Date", y="Cumulative Returns", color="") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p9)
ggsave("14_rsi_strategy.png", p9, width=12, height=6, dpi=150)
cat("Saved: 14_rsi_strategy.png\n")
cat("\n>> EXPLANATION (Plot 14 - RSI Strategy vs Buy & Hold):\n")
cat("   This compares the RSI-based trading strategy against Buy & Hold.\n")
cat("   The RSI strategy buys when RSI drops below 30 (oversold = stock is undervalued)\n")
cat("   and sells when RSI rises above 70 (overbought = stock is overvalued).\n")
cat("   RSI is a momentum oscillator that measures the speed and magnitude of price changes.\n")
cat("   This is a mean-reversion strategy - it assumes prices return to their average after extremes.\n")
cat("   It works well in ranging markets but may underperform in strong trending markets.\n\n")

# RSI visualization
p9b <- ggplot(tail(rsi_df, 252), aes(x=Date, y=RSI)) +
  geom_line(color="#7B1FA2", linewidth=0.5) +
  geom_hline(yintercept=70, linetype="dashed", color="red") +
  geom_hline(yintercept=30, linetype="dashed", color="green") +
  geom_ribbon(aes(ymin=30, ymax=pmin(RSI,30)), fill="green", alpha=0.2) +
  geom_ribbon(aes(ymin=pmax(RSI,70), ymax=70), fill="red", alpha=0.2) +
  labs(title="HDFC Bank - RSI Indicator (Last 1 Year)",
       x="Date", y="RSI Value") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p9b)
ggsave("15_rsi_indicator.png", p9b, width=12, height=6, dpi=150)
cat("Saved: 15_rsi_indicator.png\n")
cat("\n>> EXPLANATION (Plot 15 - RSI Indicator, Last 1 Year):\n")
cat("   This chart shows the RSI (14-period) values over the last year.\n")
cat("   RED dashed line at 70 = Overbought zone. When RSI is above 70, the stock has risen\n")
cat("   too fast and may be due for a pullback (sell signal).\n")
cat("   GREEN dashed line at 30 = Oversold zone. When RSI is below 30, the stock has fallen\n")
cat("   too fast and may bounce back (buy signal).\n")
cat("   RSI between 30-70 = neutral zone. The shaded areas highlight when RSI enters extreme zones.\n")
cat("   RSI = 50 indicates no clear momentum direction.\n\n")

# --- Strategy 3: MACD Strategy ---
cat("\n--- Strategy 3: MACD Strategy ---\n")

macd_vals <- MACD(hdfc_clean$Close, nFast=12, nSlow=26, nSig=9)
macd_df <- hdfc_clean %>%
  mutate(MACD=macd_vals[,"macd"], MACD_Signal=macd_vals[,"signal"]) %>%
  filter(!is.na(MACD) & !is.na(MACD_Signal)) %>%
  mutate(
    Trade_Signal = ifelse(MACD > MACD_Signal, 1, 0),
    Trade_Signal = lag(Trade_Signal, 1),
    MACD_Return  = Trade_Signal * Daily_Return / 100,
    BH_Return    = Daily_Return / 100
  ) %>%
  filter(!is.na(Trade_Signal)) %>%
  mutate(
    Cum_MACD = cumprod(1 + replace_na(MACD_Return, 0)),
    Cum_BH   = cumprod(1 + replace_na(BH_Return, 0))
  )

p10 <- ggplot(macd_df, aes(x=Date)) +
  geom_line(aes(y=Cum_MACD, color="MACD Strategy"), linewidth=0.6) +
  geom_line(aes(y=Cum_BH, color="Buy & Hold"), linewidth=0.6) +
  scale_color_manual(values=c("MACD Strategy"="#FF6F00","Buy & Hold"="#E53935")) +
  labs(title="Strategy 3: MACD Trading Strategy vs Buy & Hold",
       x="Date", y="Cumulative Returns", color="") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p10)
ggsave("16_macd_strategy.png", p10, width=12, height=6, dpi=150)
cat("Saved: 16_macd_strategy.png\n")
cat("\n>> EXPLANATION (Plot 16 - MACD Strategy vs Buy & Hold):\n")
cat("   This compares the MACD-based trading strategy against Buy & Hold.\n")
cat("   MACD (Moving Average Convergence Divergence) uses 12-day and 26-day EMAs.\n")
cat("   BUY signal: When MACD line crosses ABOVE the signal line (bullish momentum).\n")
cat("   SELL signal: When MACD line crosses BELOW the signal line (bearish momentum).\n")
cat("   MACD is a trend-following momentum indicator that shows the relationship between\n")
cat("   two moving averages. It is one of the most popular indicators in technical analysis.\n")
cat("   It works best in trending markets and may generate false signals in sideways markets.\n\n")

# --- Performance Comparison Table ---
cat("\n========== PERFORMANCE SUMMARY ==========\n")

calc_sharpe <- function(returns) {
  r <- na.omit(returns)
  if(length(r) == 0 || sd(r) == 0) return(0)
  return(mean(r) / sd(r) * sqrt(252))
}

calc_maxdd <- function(cum_returns) {
  peak <- cummax(cum_returns)
  dd <- (cum_returns - peak) / peak
  return(min(dd) * 100)
}

results <- data.frame(
  Strategy = c("Buy & Hold", "SMA 50/200 Crossover", "RSI (14)", "MACD (12/26/9)"),
  Total_Return_Pct = round(c(
    bh_total,
    sma_total,
    (tail(rsi_df$Cum_RSI,1)-1)*100,
    (tail(macd_df$Cum_MACD,1)-1)*100
  ), 2),
  Sharpe_Ratio = round(c(
    calc_sharpe(trade_df$BuyHold_Return),
    calc_sharpe(trade_df$Strategy_Return),
    calc_sharpe(rsi_df$RSI_Return),
    calc_sharpe(macd_df$MACD_Return)
  ), 3),
  Max_Drawdown_Pct = round(c(
    calc_maxdd(trade_df$Cum_BuyHold),
    calc_maxdd(trade_df$Cum_Strategy),
    calc_maxdd(rsi_df$Cum_RSI),
    calc_maxdd(macd_df$Cum_MACD)
  ), 2)
)

print(results)

# Save performance table
write.csv(results, "Performance_Summary.csv", row.names=FALSE)
cat("\nSaved: Performance_Summary.csv\n")

# --- Final Combined Signal Chart ---
p11 <- ggplot(trade_df, aes(x=Date)) +
  geom_line(aes(y=Close), color="grey50", linewidth=0.3) +
  geom_line(aes(y=SMA_50), color="blue", linewidth=0.5) +
  geom_line(aes(y=SMA_200), color="red", linewidth=0.5) +
  geom_point(data=trade_df %>% filter(Signal==1 & lag(Signal)==0),
             aes(y=Close), color="green", size=2, shape=24) +
  geom_point(data=trade_df %>% filter(Signal==0 & lag(Signal)==1),
             aes(y=Close), color="red", size=2, shape=25) +
  labs(title="HDFC Bank - Buy/Sell Signals (SMA Crossover)",
       subtitle="Green=Buy, Red=Sell",
       x="Date", y="Price (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p11)
ggsave("17_buy_sell_signals.png", p11, width=12, height=6, dpi=150)
cat("Saved: 17_buy_sell_signals.png\n")
cat("\n>> EXPLANATION (Plot 17 - Buy/Sell Signal Chart):\n")
cat("   This chart marks the exact entry (buy) and exit (sell) points of the SMA Crossover strategy.\n")
cat("   GREEN triangles (pointing up) = BUY signals where the 50-day SMA crossed above the 200-day SMA.\n")
cat("   RED triangles (pointing down) = SELL signals where the 50-day SMA crossed below the 200-day SMA.\n")
cat("   Blue line = 50-day SMA (short-term trend), Red line = 200-day SMA (long-term trend).\n")
cat("   This visualization helps traders see how frequently the strategy generates trades\n")
cat("   and whether the signals were profitable (buy low, sell high) or not.\n\n")

cat("\n============================================\n")
cat("ALL TASKS COMPLETED SUCCESSFULLY!\n")
cat("============================================\n")
cat("Files generated:\n")
cat(" Data:   HDFC_Bank_Data.csv, HDFC_Bank_Data.xlsx\n")
cat(" Plots:  01-17 PNG files\n")
cat(" Report: Performance_Summary.csv\n")
