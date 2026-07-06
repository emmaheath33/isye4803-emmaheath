setwd("/Users/emmaheath/Downloads")
# Read the data
# ---- 1. Read data ----
df <- read.table("w_logret_3automanu.txt", header = TRUE, sep = "\t")
str(df)
summary(df)

# ---- 2. Fit the model ----
fit <- lm(GM ~ Toyota + Ford, data = df)
summary(fit)
confint(fit)
anova(fit)

# ---- 3. Diagnostic plots ----
par(mfrow = c(2, 2))
plot(fit)   # Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage

# ---- 4. Normality of residuals ----
shapiro.test(residuals(fit))

# ---- 5. Autocorrelation (Durbin-Watson, by hand — no CRAN access needed) ----
res <- residuals(fit)
dw <- sum(diff(res)^2) / sum(res^2)
dw

# ---- 6. Multicollinearity (VIF, by hand) ----
vif_toyota <- 1 / (1 - summary(lm(Toyota ~ Ford, data = df))$r.squared)
vif_ford   <- 1 / (1 - summary(lm(Ford ~ Toyota, data = df))$r.squared)
c(VIF_Toyota = vif_toyota, VIF_Ford = vif_ford)

# ---- 7. Influential points ----
cooksd <- cooks.distance(fit)
df[which(cooksd > 4 / nrow(df)), ]

# ---- 8. Raw correlations ----
cor(df[, c("Toyota", "Ford", "GM")])
