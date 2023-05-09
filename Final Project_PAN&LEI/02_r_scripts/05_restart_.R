# since I stuck at the estimation for the alternative method
# I restart this project strictly follow the Final project's requirement


load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
model_lm <- lm(X3TGPASTEM ~ X1TXMTSCOR, data = hsls_sub)
summary(model_lm)
nrow(hsls_sub)
mean(hsls_sub$X1TXMTSCOR)
var(hsls_sub$X1TXMTSCOR)
# the model for this current project would be
# X3TGPASTEM = -0.265 + 0.053*X1TXMTSCOR
# sd for residual: 0.7693
# number of observations: 19948
# variance of the predictor: 100.6209
# mean of the predictor: 51.24985

# write the data gen function for dat_gen

dat_gen <- function(size= 500,
                    betas,
                    iv_mean,
                    iv_var,
                    error_sd){
  X <- rnorm(size, mean = iv_mean, sd= sqrt(iv_var))
  X_aug <- cbind(1, X)
  Error <- rnorm(size, mean=0, sd=error_sd)
  Y <- X_aug %*% as.matrix(betas) + Error
  out <- cbind(Y, X)
  colnames(out) <- c("Y", "X1")
  return(as.data.frame(out))
}


# dat_gen runs well, next write the function of coefficient estimation
reg <- function(ds) {
  x <- as.matrix(ds[,2])
  y <- as.matrix(ds[,1])
  y_cen <- apply(y, 2, function(x) x-mean(x))
  x_cen <- apply(x, 2, function(x) x-mean(x))
  b1 <- sum(x_cen*y_cen)/sum(x_cen^2)
  b0 <- mean(y - x*b1)
  
  y_hat <- b0 + x*b1
  sse <- sum((y-y_hat)^2)
  sig_sq <- sse/(nrow(x)-2)
  b1_a <- sum(y_cen/x_cen)/nrow(x)
  b0_a <- mean(y - x*b1_a)
  y_hat_a <- b0_a + x*b1_a
  sse_a <- sum((y-y_hat_a)^2)
  sig_sq_a <- sse_a/nrow(x)
  out_ <- cbind(b0, b1, sig_sq, b0_a, b1_a,sig_sq_a)
  return(out_)
}


# ----------------------------
# Monte Carlo Simulation
# ----------------------------
# the function runs well. Next, using Monte Carlo method to do simulation
# set the iteration time
R <- 1000
set.seed(666)
dat_list <- replicate(n = R,
                      expr = dat_gen(size = 40,
                                     betas = c(-0.265,0.053),
                                     iv_mean = 51.24985, iv_var = 100.6209,
                                     error_sd = 0.7693),
                      simplify = FALSE)

estimates <- sapply(X = dat_list,
                    FUN = reg,
                    simplify = TRUE)
estimates <- t(estimates)
colnames(estimates) <- c("b0", "b1", "sig_sq", "b0_a", "b1_a", "sig_sq_a")

hist(estimates[,2])
hist(estimates[,3])
plot(estimates[,3])
plot(estimates[,5])
plot(estimates[,6])

(estimates_hat_median <- round(apply(estimates,2,median),3))
(estimates_hat_mean <- round(apply(estimates,2,mean),3))
(estimates_hat_sd <- round(apply(estimates,2,sd),3))


# ----------------------------
# bootstrap method 06 
# ----------------------------

# generate a single dataset
data_b <-dat_gen(size = 40,betas = c(-0.265,0.053),
                iv_mean = 51.24985, iv_var = 100.6209,
                error_sd = 0.7693)
# run bootstrapping on this single dataset
B = 1000
# shuffle the 1:40 index rather than data_b
boot_index <- replicate(n=B,
                       expr = sample(1:40, 40, TRUE),
                       simplify = FALSE)
# use the bootstapped index to exracted the data
boot_samp <- list()
for (i in 1:1000) {
  boot_unit <- data_b[boot_index[[i]],]
  boot_samp[[i]] <- boot_unit
}
estimates <- sapply(X = boot_samp,
                    FUN = reg,
                    simplify = TRUE)
estimates <- t(estimates)
colnames(estimates) <- c("b0", "b1", "sig_sq", "b0_a", "b1_a", "sig_sq_a")
head(estimates)
hist(estimates[,2])
hist(estimates[,3])
plot(estimates[,3])
plot(estimates[,5])
plot(estimates[,6])

(estimates_hat_median <- round(apply(estimates,2,median),3))
(estimates_hat_mean <- round(apply(estimates,2,mean),3))


# ----------------------------
# jacknife method 06 leave one observation each time
# ----------------------------
head(data_b)

jack_list <- list()
for (i in 1:40) {
  data_loov <- data_b[-i,]
  jack_list[[i]] <- data_loov
}

estimates <- sapply(X = jack_list,
                    FUN = reg,
                    simplify = TRUE)
estimates <- t(estimates)
colnames(estimates) <- c("b0", "b1", "sig_sq", "b0_a", "b1_a", "sig_sq_a")
head(estimates)
hist(estimates[,2])
hist(estimates[,3])
plot(estimates[,3])
plot(estimates[,5])
plot(estimates[,6])

(estimates_hat_median <- round(apply(estimates,2,median),3))
(estimates_hat_mean <- round(apply(estimates,2,mean),3))



# ----------------------------
# test area
# ----------------------------

d1 <- dat_list[[1]]
d1 <- as.matrix(d1)
x <- d1[,1]
class(x)
reg(d1)

test <- dat_gen(size = 400000, betas = c(-0.265,0.053), 
                iv_mean = 51.24985, iv_var = 100.6209, error_sd = 0.7693)
test <- dat_gen(size = 200000, betas = c(-0.265,0.053), 
                iv_mean = 0, iv_var = 1, error_sd = 1)


