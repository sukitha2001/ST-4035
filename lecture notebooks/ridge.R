library(ISLR)
library(glmnet)
data("Hitters")

# Variables
print(names(Hitters))

# number of observations and variables
print(dim(Hitters))

# missing values in the salary variable
print(sum(is.na(Hitters$Salary)))

#removing missing values
myHitters <- na.omit(Hitters)

x = model.matrix(Salary~.,myHitters )[,-1]
y = myHitters$Salary

ridge.mod = glmnet (x,y, alpha=0)
print(ridge.mod)

grid = 10^seq(10,-2, length=100)
ridge.mod =glmnet (x, y, alpha=0, lambda=grid)
plot(ridge.mod)

print(ridge.mod$lambda[50])
print(coef(ridge.mod)[,50])

print(predict(ridge.mod, s=50, type="coefficients")[1:20,])