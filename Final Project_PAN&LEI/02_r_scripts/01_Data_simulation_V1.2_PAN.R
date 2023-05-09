# V1.2 allows user run this function without a given covariance matrix
# The covariance matrix will be set to 1 at the diagonal, 0 at the other entries by default

# before running the function , please load the package first
library(mvtnorm)
# write a function to generate data
dat_gen <- function(size=500, # datasize sets to 500 by default,you can change to any size
                    beta_ = NULL, # necessary, input the betas in the seuqnce of c(b0, b1, b2...)
                    pred_means = NULL, # not necessary,input the means of each variables
                    pred_cov = NULL, # not necessary, input the cov matrix of variables
                    error_sd = 1){
  if (is.null(beta_) == TRUE){
    # if user did not give a coefficients array, return warning.
    return("Error: You need to give the coefficients in the sequence of beta0, beta1,...")
  }else {
    if (is.null(pred_means)== TRUE){
      # if user did not give means of each variables,
      # use 0 as means by default
      predictor_nums <- length(beta_)
      if (is.null(pred_cov)== TRUE){
        print("Using the variance 1 and covariance 0 by default")
        X <- rmvnorm(n=size,sigma=diag(predictor_nums-1))
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      } else{ ### user gives the predictors covariance matrix
        X <- rmvnorm(n=size,sigma=pred_cov)
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      }
    }else{ ## user gives the means of predictors 
      if (is.null(pred_cov)==TRUE){
        print("Using the variance 1 and covariance 0 by default")
        X <- rmvnorm(n=size,mean= pred_means,sigma=diag(predictor_nums-1))
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      } else{ ### user gives the predictors covariance matrix
        X <- rmvnorm(n=size,mean= pred_means,sigma=pred_cov)
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      }
    }
    # give columns names in "Y","X1","X2",...
    n_ = predictor_nums - 1
    x_vars <- c("Y")
    for (i in 1:n_) {
      x_vars[i+1] <- paste0("X",i)
    }
    colnames(out_data) <- x_vars
    return(as.data.frame(out_data))
  }
}

#################################################
##
## Test area
##
################################################

# test
data(mtcars)
# Fit a linear regression model to predict miles per gallon (mpg) based on horsepower (hp)
model <- lm(mpg ~ hp+wt, data = mtcars)

# View the summary of the model
summary(model)$coefficient
  
cvo <- var(mtcars[,c("hp","wt")])

cars_data <- dat_gen(size=100,
                     beta_=c(37.23,-0.03177295,3.87783074),
                     error_sd = summary(model)$sigma)

cars <- lm(Y~X1+X2, data = cars_data )
summary(cars)