# ============================================================
# Clustering & Classification in R
#   Based on the lecture clustering.ipynb (K-means, DBSCAN,
#   silhouette analysis) + the real swap-rate data (d_swap.txt)
#
#   (1) K-means vs DBSCAN  -- a case where DBSCAN wins
#   (2) Silhouette analysis to pick the number of clusters
#   (3) 1-Nearest-Neighbor classifier on a labeled set
# ============================================================

# ---- 0. Packages ----
# REQUIRED packages:
install.packages(c("dbscan", "cluster", "class", "ggplot2"))

library(dbscan)      # DBSCAN + kNNdistplot for choosing eps
library(cluster)     # silhouette()
library(class)       # knn()
library(ggplot2)     # plotting

# OPTIONAL (nicer plots) -- auto-detected, code falls back to base R if missing
# install.packages(c("factoextra"))
use_factoextra <- requireNamespace("factoextra", quietly = TRUE)
if (use_factoextra) library(factoextra)

set.seed(42)


# ============================================================
# PART 1: K-MEANS vs DBSCAN
# A case where DBSCAN wins: non-convex ("moon-shaped") clusters.
# NOTE: the real swap data is roughly one convex blob (rates move
# together across tenors), so it won't show this effect -- this
# mirrors the make_moons() demo in clustering.ipynb, just in R.
# ============================================================

## ---- 1a. Generate two interleaving "moon" clusters ----
n <- 300
noise_sd <- 0.08

# upper moon
t1 <- runif(n/2, 0, pi)
moon1 <- data.frame(
  x = cos(t1) + rnorm(n/2, 0, noise_sd),
  y = sin(t1) + rnorm(n/2, 0, noise_sd),
  true_label = 1
)

# lower moon, shifted right and down, flipped
t2 <- runif(n/2, 0, pi)
moon2 <- data.frame(
  x = 1 - cos(t2) + rnorm(n/2, 0, noise_sd),
  y = 0.5 - sin(t2) + rnorm(n/2, 0, noise_sd),
  true_label = 2
)

df <- rbind(moon1, moon2)

ggplot(df, aes(x, y, color = factor(true_label))) +
  geom_point() + coord_equal() + theme_minimal() +
  labs(title = "True structure: two interleaving moons", color = "true label")

## ---- 1b. Scale features ----
feature_cols <- c("x", "y")
X <- scale(df[, feature_cols])

## ---- 1c. K-means (k = 2) ----
km <- kmeans(X, centers = 2, nstart = 25)
df$kmeans_cluster <- factor(km$cluster)

ggplot(df, aes(x, y, color = kmeans_cluster)) +
  geom_point() + coord_equal() + theme_minimal() +
  labs(title = "K-means result (fails on non-convex shapes)")

## ---- 1d. DBSCAN ----
# Inspect a k-NN distance plot to help choose eps (look for the elbow)
kNNdistplot(X, k = 4)
abline(h = 0.15, lty = 2, col = "red")   # adjust after inspecting the plot

eps_val <- 0.15
minPts_val <- 4

db <- dbscan(X, eps = eps_val, minPts = minPts_val)
df$dbscan_cluster <- factor(db$cluster)   # cluster "0" = noise

ggplot(df, aes(x, y, color = dbscan_cluster)) +
  geom_point() + coord_equal() + theme_minimal() +
  labs(title = "DBSCAN result (correctly finds the two moons)")

## ---- 1e. Quantify the win: accuracy against true labels ----
# Simple matching: for 2 clusters we can just check which
# label-to-cluster mapping gives higher agreement.
match_acc <- function(true_label, cluster) {
  t1 <- table(true_label, cluster)
  max(sum(diag(t1)), sum(t1) - sum(diag(t1))) / sum(t1)
}

cat("K-means agreement with true moon labels:",
    round(match_acc(df$true_label, df$kmeans_cluster), 3), "\n")
cat("DBSCAN agreement with true moon labels:",
    round(match_acc(df$true_label, df$dbscan_cluster), 3), "\n")
cat("(DBSCAN should score far higher -- it isn't restricted to convex clusters)\n")


# ============================================================
# PART 2: SILHOUETTE ANALYSIS ON THE REAL SWAP DATA
# ============================================================

## ---- 2a. Load d_swap.txt (skip the %%% and % comment lines) ----
lines <- readLines("/Users/emmaheath/Downloads/d_swap.txt")          # <- update path if needed
header_line_idx <- grep("^%[^%]", lines)[1]     # the line "%swp1y\tswp2y\t..."
col_names <- strsplit(gsub("^%", "", lines[header_line_idx]), "\t")[[1]]
data_lines <- lines[(header_line_idx + 1):length(lines)]
data_lines <- data_lines[nzchar(data_lines)]     # drop any blank lines

swap <- read.table(text = data_lines, sep = "\t", col.names = col_names)
str(swap)   # sanity check: should be numeric columns swp1y ... swp30y

## ---- 2b. Scale features ----
# (Even though all tenors are quoted in similar units here, scaling
# is still good practice whenever distances are compared across
# columns with different variances -- e.g. short tenors are much
# less volatile than long tenors in this data.)
Xswap <- scale(swap)

## ---- 2c. Silhouette analysis across k = 2..10 ----
sil_scores <- numeric()
for (k_try in 2:10) {
  km_try <- kmeans(Xswap, centers = k_try, nstart = 25)
  sil <- silhouette(km_try$cluster, dist(Xswap))
  sil_scores[k_try] <- mean(sil[, "sil_width"])
}

sil_df <- data.frame(k = 2:10, avg_silhouette = sil_scores[2:10])
print(sil_df)

ggplot(sil_df, aes(k, avg_silhouette)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Silhouette analysis on swap rate data",
       x = "Number of clusters (k)", y = "Average silhouette width")

best_k <- sil_df$k[which.max(sil_df$avg_silhouette)]
cat("Best k by silhouette score:", best_k, "\n")

## ---- 2d. Silhouette plot for the chosen k ----
km_best <- kmeans(Xswap, centers = best_k, nstart = 25)
sil_best <- silhouette(km_best$cluster, dist(Xswap))

if (use_factoextra) {
  print(fviz_silhouette(sil_best) + labs(title = paste("Silhouette plot, k =", best_k)))
} else {
  plot(sil_best, main = paste("Silhouette plot, k =", best_k))
}


# ============================================================
# PART 3: 1-NEAREST-NEIGHBOR CLASSIFIER ON A LABELED SET
# ============================================================
# The swap data has no built-in class labels, so we create a
# natural, defensible label from the data itself: whether the
# swap curve is "steep" or "flat" that day, based on the
# 30y - 1y spread. This gives a genuinely labeled real dataset.

## ---- 3a. Build labels ----
swap$spread_30y_1y <- swap$sw30y - swap$swp1y
swap$curve_shape <- factor(
  ifelse(swap$spread_30y_1y > median(swap$spread_30y_1y), "Steep", "Flat")
)
table(swap$curve_shape)   # check class balance

## ---- 3b. Train/test split ----
feature_cols_knn <- c("swp1y", "swp2y", "swp3y", "sw4y",
                      "sw5y", "sw7y", "sw10y", "sw30y")

set.seed(42)
train_idx <- sample(seq_len(nrow(swap)), size = 0.7 * nrow(swap))

train_X <- scale(swap[train_idx, feature_cols_knn])
test_X  <- scale(swap[-train_idx, feature_cols_knn],
                 center = attr(train_X, "scaled:center"),
                 scale  = attr(train_X, "scaled:scale"))

train_y <- swap$curve_shape[train_idx]
test_y  <- swap$curve_shape[-train_idx]

## ---- 3c. Run 1-NN ----
pred_1nn <- knn(train = train_X, test = test_X, cl = train_y, k = 1)

## ---- 3d. Evaluate ----
conf_mat <- table(Predicted = pred_1nn, Actual = test_y)
print(conf_mat)

accuracy <- mean(pred_1nn == test_y)
cat("1-NN accuracy on swap curve shape (Steep vs Flat):", round(accuracy, 3), "\n")

