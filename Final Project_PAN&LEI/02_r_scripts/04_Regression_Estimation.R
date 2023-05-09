# this r file is to write a function for regression model coefficient estimation!
# for any multiple linear model!

# this function takes the dependent variable Y vector and the augmented matrix M_aug as 
# input
reg <- function(Y, X){
  # Note RSS is a function of the estimated coefficients
  # first convert the X matrix into a augmented matrix
  Y <- as.matrix(Y)
  Intercept <- rep(1, nrow(X))
  X_aug <- as.matrix(cbind(Intercept, X))
  X_ss_inverse <- solve(t(X_aug) %*% X_aug)
  B_hat <- X_ss_inverse %*% t(X_aug) %*% Y
  # return(B_hat)
  # next write a function to estimate the variance of the error term
  sig_sq <-t(Y- X_aug %*% B_hat)%*%(Y- X_aug %*% B_hat)/ (nrow(X_aug) - ncol(X)-1)
  outcome_m <- rbind(B_hat, sig_sq)
  # let each entry of X_Aug matrix to be inverted
  X_cen <- apply(X, 2, function(x) x- mean(x))
  X_inv <- apply(X_cen, 2, function(x) x^(-1))
  Y_cen <- apply(Y, 2, function(x) x-mean(x))
  B_hat_a <- t(Y_cen) %*% X_inv / nrow(X)
  outcome_m <- rbind(outcome_m, t(B_hat_a))
  return(outcome_m)
}

load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")

reg(hsls_sub[,1], hsls_sub[,2:3])

P <- hsls_sub[,1]
P <- as.matrix(P)
X1 <- as.matrix(hsls_sub[,2])
mean(P)
Y_cen <- apply(P, 2, function(x) x-mean(x))
X1_cen <- apply(X1, 2, function(x) x-mean(x))
Y_cen/X1_cen

sum(Y_cen*X1_cen)/sum(X1_cen^2)

sum(Y_cen/X1_cen)/nrow(X1)
X1_cen <- apply(X1, 2, function(x) (x-mean(x))^(-1))
hist(X1)
hist(X1_cen)
summary(P)

t(Y_cen) %*% X1_cen / nrow(X1_cen)


a <- as.matrix(c(2,4,6,8),byrow=T)
b <- as.matrix(c(2,2,2,2),byrow=T)

b^2

X_cen <- apply(X, 2, function(x) x- mean(x))
X_inv <- apply(X_cen, 2, function(x) x^(-1))

head(X_cen)
head(X_inv)
head(X_aug)
1/-0.8891900
# import the data
load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
# run a simple linear model
model <- lm(X3TGPASTEM~., data = hsls_sub)
summary(model)

cov(hsls_sub[,2:3])
X <- hsls_sub[,2:3]
one_vec <- rep(1, nrow(X))
X_aug <- as.matrix(cbind(one_vec, X))
cov(X_aug)
col_means <-apply(X_aug, 2, mean)
col_means_m <- matrix(col_means,nrow = nrow(X_aug),ncol = 3,byrow = T)
X_aug_centered <-  X_aug - col_means_m
head(X_aug_centered)
t(X_aug_centered) %*% X_aug_centered/ (nrow(X_aug_centered)-1)