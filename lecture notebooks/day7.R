# ── Simulate data with high multicollinearity ──
set.seed(42)
n <- 200

# hours_studied is the "true" driver
hours_studied <- rnorm(n, mean = 5, sd = 1.5)

# practice_tests is almost a copy of hours_studied + tiny noise
practice_tests <- 0.95 * hours_studied + rnorm(n, 0, 0.3)

# TRUE model: only hours_studied matters (coef = 8), not practice_tests
exam_score <- 40 + 8 * hours_studied + rnorm(n, 0, 5)

df <- data.frame(exam_score, hours_studied, practice_tests)

# Check the correlation — it's very high!
print(cor(hours_studied, practice_tests))


model_bad <- lm(exam_score ~ hours_studied + practice_tests, data = df)
print(summary(model_bad))

library(car)
print(vif(model_bad))

kappa_val <- kappa(model.matrix(model_bad))
print(kappa_val)

model_good <- lm(exam_score ~ hours_studied, data = df)
print(summary(model_good))


kappa_val <- kappa(model.matrix(model_good))
print(kappa_val)

df$study_effort <- (df$hours_studied + df$practice_tests) / 2

model_average <- lm(exam_score ~ study_effort, data = df)
print(summary(model_average))

pca_result <- prcomp(df[, c("hours_studied", "practice_tests")], scale. = TRUE)

print(summary(pca_result))

df$pc1 <- pca_result$x[, 1]

model_pca <- lm(exam_score ~ pc1, data = df)
print(summary(model_pca))

