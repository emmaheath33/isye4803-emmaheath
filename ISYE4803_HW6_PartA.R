# =====================================================
# Logistic Regression on the Titanic dataset (Base R only)
# =====================================================

# -----------------------------------------------------
# 1. Load data
# -----------------------------------------------------
df <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------
# 2. Select predictors
# -----------------------------------------------------
data <- df[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

# -----------------------------------------------------
# 3. Handle missing values
# -----------------------------------------------------
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm = TRUE)
data$Embarked[data$Embarked == "" | is.na(data$Embarked)] <- names(sort(table(data$Embarked), decreasing = TRUE))[1]

# -----------------------------------------------------
# 4. Encode categorical variables as factors
# -----------------------------------------------------
data$Survived <- as.factor(data$Survived)
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
data$Pclass <- as.factor(data$Pclass)

# NOTE: FamilySize = SibSp + Parch + 1 is left out of the model since it is an
# exact linear combination of SibSp and Parch (which are already predictors).
# Including it causes a singularity and R drops it automatically anyway.

# -----------------------------------------------------
# 5. Train/test split (80/20) using base R
# -----------------------------------------------------
set.seed(42)
n <- nrow(data)
train_size <- floor(0.8 * n)
train_idx <- sample(seq_len(n), size = train_size)
train <- data[train_idx, ]
test  <- data[-train_idx, ]

# -----------------------------------------------------
# 6. Fit logistic regression
# -----------------------------------------------------
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, family = binomial(link = "logit"))

cat("=== Model Summary (coefficients, p-values) ===\n")
print(summary(model))

cat("\n=== Odds Ratios ===\n")
print(exp(coef(model)))

cat("\n=== Odds Ratios with 95% Confidence Intervals ===\n")
print(exp(cbind(OR = coef(model), confint(model))))

# -----------------------------------------------------
# 7. Predictions on test set
# -----------------------------------------------------
test$prob <- predict(model, newdata = test, type = "response")
test$pred <- ifelse(test$prob > 0.5, 1, 0)

# -----------------------------------------------------
# 8. Confusion matrix (base R)
# -----------------------------------------------------
cat("\n=== Confusion Matrix ===\n")
actual <- as.numeric(as.character(test$Survived))
cm <- table(Predicted = test$pred, Actual = actual)
print(cm)

TP <- cm["1", "1"]
TN <- cm["0", "0"]
FP <- cm["1", "0"]
FN <- cm["0", "1"]

accuracy    <- (TP + TN) / sum(cm)
precision   <- TP / (TP + FP)
recall      <- TP / (TP + FN)   # sensitivity
specificity <- TN / (TN + FP)
f1          <- 2 * precision * recall / (precision + recall)

cat("\n=== Classification Metrics ===\n")
cat(sprintf("Accuracy:    %.4f\n", accuracy))
cat(sprintf("Precision:   %.4f\n", precision))
cat(sprintf("Recall:      %.4f\n", recall))
cat(sprintf("Specificity: %.4f\n", specificity))
cat(sprintf("F1 Score:    %.4f\n", f1))

# -----------------------------------------------------
# 9. ROC AUC (base R, via trapezoidal rule)
# -----------------------------------------------------
thresholds <- seq(0, 1, by = 0.01)
tpr <- numeric(length(thresholds))
fpr <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  pred_t <- ifelse(test$prob > thresholds[i], 1, 0)
  tp <- sum(pred_t == 1 & actual == 1)
  fn <- sum(pred_t == 0 & actual == 1)
  fp <- sum(pred_t == 1 & actual == 0)
  tn <- sum(pred_t == 0 & actual == 0)
  tpr[i] <- tp / (tp + fn)
  fpr[i] <- fp / (fp + tn)
}

# Sort by fpr for proper integration, then compute AUC via trapezoidal rule
ord <- order(fpr)
fpr_sorted <- fpr[ord]
tpr_sorted <- tpr[ord]
auc <- sum(diff(fpr_sorted) * (head(tpr_sorted, -1) + tail(tpr_sorted, -1)) / 2)

cat(sprintf("\nAUC: %.4f\n", auc))

predict(model, newdata = data[1, ], type = "response")

