setwd("/Users/emmaheath/Downloads")
# ============================================================
# PART A: CAPM regression for 10 stocks
# ============================================================

# ---- 1. Load data ----
# mkt file: header on row 2, first row is a source comment -> skip=1
mkt <- read.table("m_sp500ret_3mtcm.txt", header = TRUE, sep = "", skip = 1)
stocks <- read.table("m_logret_10stocks.txt", header = TRUE, sep = "", 
                     fill = TRUE, stringsAsFactors = FALSE)

# Drop any trailing blank rows (the file has blank lines at the end)
stocks <- stocks[!is.na(stocks$Date) & stocks$Date != "", ]

names(mkt)      # "Date" "sp500" "X3mTCM"  (R renames "3mTCM" since it starts with a digit)
names(stocks)   # "Date" "AAPL" "ADBE" ... "ORCL"

# ---- 2. Standardize dates to "YYYY-MM" for merging ----
# mkt Date format: "Jan-94"
mkt$ym <- format(as.Date(paste0("01-", mkt$Date), format = "%d-%b-%y"), "%Y-%m")

# stocks Date format: "1/3/1994" (M/D/YYYY)
stocks$ym <- format(as.Date(stocks$Date, format = "%m/%d/%Y"), "%Y-%m")

# ---- 3. Rename T-bill column and convert annualized % to monthly decimal ----
names(mkt)[names(mkt) == "X3mTCM"] <- "tcm3m"   # adjust if names(mkt) shows a different name
mkt$rf <- mkt$tcm3m / 12 / 100

# ---- 4. Market excess return ----
mkt$mkt_excess <- mkt$sp500 - mkt$rf

# ---- 5. Merge on standardized year-month ----
data_all <- merge(mkt[, c("ym", "sp500", "rf", "mkt_excess")],
                  stocks,
                  by = "ym")

nrow(data_all)   # sanity check: should be 156 (Jan-94 to Dec-06)

# ---- 6. Run CAPM regression for each stock ----
stock_names <- c("AAPL","ADBE","ADP","AMD","DELL","GTW","HP","IBM","MSFT","ORCL")
results <- list()

for (nm in stock_names) {
  y <- data_all[[nm]] - data_all$rf      # stock excess return
  x <- data_all$mkt_excess               # market excess return
  fit <- lm(y ~ x)
  results[[nm]] <- summary(fit)
}

# ---- 7. Build summary table ----
summary_table <- data.frame(
  Stock = character(), Alpha = numeric(), Alpha_p = numeric(),
  Beta = numeric(), Beta_p = numeric(), CAPM_holds = character(),
  stringsAsFactors = FALSE
)

for (nm in stock_names) {
  s <- results[[nm]]$coefficients
  alpha   <- s[1, 1]
  alpha_p <- s[1, 4]
  beta    <- s[2, 1]
  beta_p  <- s[2, 4]
  verdict <- ifelse(alpha_p > 0.05, "Yes (alpha=0 not rejected)", "No (alpha != 0)")
  
  summary_table <- rbind(summary_table,
                         data.frame(Stock = nm, Alpha = round(alpha,5), Alpha_p = round(alpha_p,4),
                                    Beta = round(beta,4), Beta_p = round(beta_p,4), CAPM_holds = verdict))
}

print(summary_table)

# ---- 8. Full regression output for each stock (paste into report) ----
for (nm in stock_names) {
  cat("\n============================\n")
  cat("Stock:", nm, "\n")
  print(results[[nm]])
}

# Excess returns for stock #3 (ADP)
y <- data_all$ADP - data_all$rf     # ADP excess return
x <- data_all$mkt_excess            # market excess return
x
y
# Get the two numbers you need
cov(y, x)
var(x)

# Also print the regression slope so you can compare at the end
summary(lm(y ~ x))$coefficients
