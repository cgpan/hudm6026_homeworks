# import the data
load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
# run a simple linear model
model <- lm(X3TGPASTEM~., data = hsls_sub)
summary(model)

# the results show that the Y_i = .110 + .222*X_1 + .045*X_2 + e_i
# the sd of e_i = .7534
# from the dataset, the variance covariance matrix is as follows
cov(hsls_sub[,2:3])
mean(hsls_sub[,2])
mean(hsls_sub[,3])

# import the data gen function from another r file
source("01_Data_simulation_V1.2_PAN.R")
# now we can use the dat_gen function
# before re-sampling write the regression estimation function
