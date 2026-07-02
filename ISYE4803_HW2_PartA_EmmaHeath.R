df <- read.table("roe01.txt", header = TRUE, sep = "")

head(df)

#Column Means
colMeans(df)

#Correlation
cor(df$ROEt, df$ROE)
cor(df)

#Simple Linear Model
fit <- lm(ROE ~ ROEt, data =df)
summary(fit)

predict(fit, newdata = data.frame(ROEt = 0.20))

returns <- read.csv("returns.csv")
returns$date <- as.factor(returns$date)
avg <- mean(as.numeric(as.character(returns$stockA)), na.rm = TRUE)
print(avg)
