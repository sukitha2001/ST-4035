set.seed(42)
n_maj <- 1000
n_min <- 100

# majority class (y = 0)
X_maj <- data.frame(
  x1 = rnorm(n_maj, mean = 0),
  x2 = rnorm(n_maj, mean = 0),
  y  = 0
)

# minority class (y = 1) — shifted mean to create signal
X_min <- data.frame(
  x1 = rnorm(n_min, mean = 2),
  x2 = rnorm(n_min, mean = 2),
  y  = 1
)

df <- rbind(X_maj, X_min)
df$y <- as.factor(df$y)
table(df$y)


# ==============================================================================
# 1. Split Data & Apply Sampling Techniques
# ==============================================================================

# Stratified train-test split (70% train, 30% test)
# This prevents data leakage (we only resample the training set, not the test set)
set.seed(42)
train_idx <- c(
  sample(which(df$y == 0), size = round(0.7 * sum(df$y == 0))),
  sample(which(df$y == 1), size = round(0.7 * sum(df$y == 1)))
)
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

# --- Under-sampling Implementation ---
# Downsamples the majority class to match the minority class size
under_sample <- function(data, target_var) {
  maj_class <- names(which.max(table(data[[target_var]]))) # <--- MATCHES INPUT
  min_class <- names(which.min(table(data[[target_var]]))) # <--- MATCHES INPUT

  df_maj <- data[data[[target_var]] == maj_class, ]
  df_min <- data[data[[target_var]] == min_class, ]
  n_min <- nrow(df_min)

  idx_maj_under <- sample(1:nrow(df_maj), size = n_min, replace = FALSE)
  return(rbind(df_maj[idx_maj_under, ], df_min))
}

# --- Over-sampling Implementation ---
# Upsamples the minority class (with replacement) to match the majority class size
over_sample <- function(data, target_var) {
  maj_class <- names(which.max(table(data[[target_var]]))) # <--- MATCHES INPUT
  min_class <- names(which.min(table(data[[target_var]]))) # <--- MATCHES INPUT

  df_maj <- data[data[[target_var]] == maj_class, ]
  df_min <- data[data[[target_var]] == min_class, ]
  n_maj <- nrow(df_maj)

  idx_min_over <- sample(1:nrow(df_min), size = n_maj, replace = TRUE)
  return(rbind(df_maj, df_min[idx_min_over, ]))
}

# --- SMOTE Implementation ---
# Synthetic Minority Over-sampling Technique (k-Nearest Neighbors)
smote_sample <- function(data, target_var, k = 5) {
  maj_class <- names(which.max(table(data[[target_var]]))) # <--- MATCHES INPUT
  min_class <- names(which.min(table(data[[target_var]]))) # <--- MATCHES INPUT

  df_maj <- data[data[[target_var]] == maj_class, ]
  df_min <- data[data[[target_var]] == min_class, ]
  n_maj <- nrow(df_maj)
  n_min <- nrow(df_min)
  n_syn <- n_maj - n_min

  feat_cols <- setdiff(names(data), target_var)
  X_min <- as.matrix(df_min[, feat_cols])

  synthetic_rows <- matrix(0, nrow = n_syn, ncol = length(feat_cols))
  colnames(synthetic_rows) <- feat_cols

  k <- min(k, n_min - 1)
  for (s in 1:n_syn) {
    # Randomly pick a minority class point
    idx <- sample(1:n_min, 1)
    orig_point <- X_min[idx, ]

    # Calculate Euclidean distances to all other minority points
    dists <- sqrt(rowSums(sweep(X_min, 2, orig_point, "-")^2))

    # Order distances and select from the k-nearest neighbors (excluding itself)
    nearest_indices <- order(dists)[2:(k + 1)]
    neighbor_idx <- sample(nearest_indices, 1)
    neighbor_point <- X_min[neighbor_idx, ]

    # Generate synthetic point by interpolating
    synthetic_rows[s, ] <- orig_point + runif(1) * (neighbor_point - orig_point)
  }

  df_syn <- as.data.frame(synthetic_rows)
  df_syn[[target_var]] <- factor(rep(min_class, n_syn), levels = levels(data[[target_var]]))
  return(rbind(df_maj, df_min, df_syn))
}

# Apply sampling methods only to the training set
train_under <- under_sample(train_df, "y")
train_over <- over_sample(train_df, "y")
train_smote <- smote_sample(train_df, "y", k = 5)


# ==============================================================================
# 2. Evaluation Helpers
# ==============================================================================

# Custom AUC function using Wilcoxon Rank-Sum formula
calc_auc <- function(probs, labels) {
  labels <- as.numeric(as.character(labels))
  n_pos <- sum(labels == 1)
  n_neg <- sum(labels == 0)
  if (n_pos == 0 || n_neg == 0) {
    return(NA)
  }
  ranks <- rank(probs)
  pos_ranks <- ranks[labels == 1]
  auc <- (sum(pos_ranks) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(auc)
}

# Model evaluation function
evaluate_model <- function(model, test_data, target_var, threshold = 0.5) {
  probs <- predict(model, newdata = test_data, type = "response")
  preds <- ifelse(probs >= threshold, 1, 0)
  actuals <- as.numeric(as.character(test_data[[target_var]]))

  TP <- sum(preds == 1 & actuals == 1)
  TN <- sum(preds == 0 & actuals == 0)
  FP <- sum(preds == 1 & actuals == 0)
  FN <- sum(preds == 0 & actuals == 1)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), 0) # Recall
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  f1_score <- ifelse((precision + sensitivity) > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0)
  auc <- calc_auc(probs, actuals)

  return(c(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = f1_score,
    AUC = auc
  ))
}

# ROC coordinates calculator
get_roc_coords <- function(probs, labels) {
  labels <- as.numeric(as.character(labels))
  thresholds <- seq(1, 0, length.out = 201)
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    preds <- ifelse(probs >= t, 1, 0)
    TP <- sum(preds == 1 & labels == 1)
    FP <- sum(preds == 1 & labels == 0)
    TN <- sum(preds == 0 & labels == 0)
    FN <- sum(preds == 0 & labels == 1)

    tpr[i] <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    fpr[i] <- ifelse((FP + TN) > 0, FP / (FP + TN), 0)
  }
  return(data.frame(FPR = fpr, TPR = tpr))
}


# ==============================================================================
# 3. Fit LR Models & Compare Sampling Techniques (Logit Link)
# ==============================================================================

model_orig <- glm(y ~ x1 + x2, data = train_df, family = binomial(link = "logit"))
model_under <- glm(y ~ x1 + x2, data = train_under, family = binomial(link = "logit"))
model_over <- glm(y ~ x1 + x2, data = train_over, family = binomial(link = "logit"))
model_smote <- glm(y ~ x1 + x2, data = train_smote, family = binomial(link = "logit"))

metrics_orig <- evaluate_model(model_orig, test_df, "y")
metrics_under <- evaluate_model(model_under, test_df, "y")
metrics_over <- evaluate_model(model_over, test_df, "y")
metrics_smote <- evaluate_model(model_smote, test_df, "y")

sampling_comp <- data.frame(
  Original = metrics_orig,
  UnderSampled = metrics_under,
  OverSampled = metrics_over,
  SMOTE = metrics_smote
)

cat("\n======================================================================\n")
cat("1. COMPARISON OF SAMPLING METHODS (Logit Link)\n")
cat("======================================================================\n")
print(round(t(sampling_comp), 4))


# ==============================================================================
# 4. Fit LR Models & Compare Link Functions (on SMOTE dataset)
# ==============================================================================

model_probit <- glm(y ~ x1 + x2, data = train_smote, family = binomial(link = "probit"))
model_cloglog <- glm(y ~ x1 + x2, data = train_smote, family = binomial(link = "cloglog"))

metrics_probit <- evaluate_model(model_probit, test_df, "y")
metrics_cloglog <- evaluate_model(model_cloglog, test_df, "y")

link_comp <- data.frame(
  Logit = metrics_smote,
  Probit = metrics_probit,
  Cloglog = metrics_cloglog
)

cat("\n======================================================================\n")
cat("2. COMPARISON OF LINK FUNCTIONS (on SMOTE-balanced training set)\n")
cat("======================================================================\n")
print(round(t(link_comp), 4))


# ==============================================================================
# 5. Generate and Save ROC Curve Comparison Plot
# ==============================================================================

png("roc_comparison.png", width = 800, height = 600, res = 120)
plot(1,
  type = "n", xlim = c(0, 1), ylim = c(0, 1),
  xlab = "False Positive Rate (1 - Specificity)",
  ylab = "True Positive Rate (Sensitivity)",
  main = "ROC Curves: Sampling Methods & Link Functions",
  col.main = "#2C3E50", col.lab = "#34495E"
)
abline(a = 0, b = 1, lty = 2, col = "grey50")

# Plot ROC curves for each key model
probs_orig <- predict(model_orig, newdata = test_df, type = "response")
roc_orig <- get_roc_coords(probs_orig, test_df$y)
lines(roc_orig$FPR, roc_orig$TPR, col = "#E74C3C", lwd = 2)

probs_smote <- predict(model_smote, newdata = test_df, type = "response")
roc_smote <- get_roc_coords(probs_smote, test_df$y)
lines(roc_smote$FPR, roc_smote$TPR, col = "#2ECC71", lwd = 2)

probs_probit <- predict(model_probit, newdata = test_df, type = "response")
roc_probit <- get_roc_coords(probs_probit, test_df$y)
lines(roc_probit$FPR, roc_probit$TPR, col = "#3498DB", lwd = 2)

probs_cloglog <- predict(model_cloglog, newdata = test_df, type = "response")
roc_cloglog <- get_roc_coords(probs_cloglog, test_df$y)
lines(roc_cloglog$FPR, roc_cloglog$TPR, col = "#9B59B6", lwd = 2)

legend("bottomright", legend = c(
  paste0("Original Logit (AUC: ", round(metrics_orig["AUC"], 3), ")"),
  paste0("SMOTE Logit (AUC: ", round(metrics_smote["AUC"], 3), ")"),
  paste0("SMOTE Probit (AUC: ", round(metrics_probit["AUC"], 3), ")"),
  paste0("SMOTE Cloglog (AUC: ", round(metrics_cloglog["AUC"], 3), ")")
), col = c("#E74C3C", "#2ECC71", "#3498DB", "#9B59B6"), lwd = 2, lty = 1, cex = 0.7, bty = "n")

dev.off()
cat("\nBeautiful ROC curve plot saved as 'roc_comparison.png' in your workspace!\n")
