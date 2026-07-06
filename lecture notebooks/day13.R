ratings <- data.frame(
    row.names = c("Alice","Bob","Charlie","David","Emma"),
    Avengers = c(5,5,4,1,2),
    IronMan = c(4,5,5,2,1),
    Titanic = c(1,1,1,5,4),
    Notebook = c(1,1,2,5,5),
    Interstellar = c(NA,4,5,2,1),
    Gravity = c(NA,3,4,1,2)
)

print(ratings)

## Collaborative filtering using pearson correlation coefficient
similarity <- function(x, y) {
    common <- !is.na(x) & !is.na(y)
    if (sum(common) == 0) {
        return(NA)
    }
    cor(x[common], y[common])
}

print(similarity(ratings["Alice",], ratings["Bob",]))
print(similarity(ratings["Alice",], ratings["Charlie",]))
print(similarity(ratings["Alice",], ratings["David",]))
print(similarity(ratings["Alice",], ratings["Emma",]))

## Predict missing ratings for a user-item using user-based CF (Pearson)
predict_rating <- function(user, item, ratings_df) {
    # return existing rating if present
    if (!is.na(ratings_df[user, item])) return(ratings_df[user, item])

    user_mean <- mean(as.numeric(ratings_df[user,]), na.rm = TRUE)
    sims <- c()
    diffs <- c()

    for (u in rownames(ratings_df)) {
        if (u == user) next
        r_ui <- ratings_df[u, item]
        if (is.na(r_ui)) next
        s <- similarity(as.numeric(ratings_df[user,]), as.numeric(ratings_df[u,]))
        if (is.na(s) || s == 0) next
        neigh_mean <- mean(as.numeric(ratings_df[u,]), na.rm = TRUE)
        sims <- c(sims, s)
        diffs <- c(diffs, (r_ui - neigh_mean))
    }

    if (length(sims) == 0) return(user_mean)
    pred <- user_mean + sum(sims * diffs) / sum(abs(sims))
    return(pred)
}

# Predictions for Alice
pred_interstellar <- predict_rating("Alice", "Interstellar", ratings)
pred_gravity <- predict_rating("Alice", "Gravity", ratings)

print(sprintf("Predicted Interstellar for Alice: %.2f", pred_interstellar))
print(sprintf("Predicted Gravity for Alice: %.2f", pred_gravity))

print('=========================================================================')

## Content based filtering using pearson correlation coefficient

movies <- data.frame(
row.names = c(
    "Avengers",
    "IronMan",
    "Titanic",
    "Notebook",
    "Interstellar",
    "Gravity"
),
Action = c(1,1,0,0,0,0),
SciFi = c(0,1,0,0,1,1),
Romance = c(0,0,1,1,0,0)
)

print(movies)

alice_ratings <- c(5,4,1,1)

profile <- colSums(movies[1:4,]*alice_ratings/sum(alice_ratings))

print(profile)



