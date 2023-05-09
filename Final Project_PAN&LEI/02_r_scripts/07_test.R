
# read the HSLS open dataset first
load("~/Desktop/PhD_Learning/HSLS-09_Project/public_dataset/hsls_17_student_pets_sr_v1_0.rdata")

df <- hsls_17_student_pets_sr_v1_0
dim(df)

load("~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
names(hsls_sub)
dim(hsls_sub)
model_lm <- lm(X3TGPASTEM ~ X1TXMTSCOR, data = hsls_sub)
summary(model_lm)

a <- matrix(c(1,2,3,4),4,4)
a
b <- matrix(c(1,2,3,4),4,4,byrow = T)
ab <- a-b
ab^2
sum(ab[,1]^2)
theta_m <- matrix(c(-0.265, 0.053,0.7693), nrow(estimates),6 , byrow = T)
dim(theta)
head(theta)
head(estimates)
est_cent <- estimates-theta_m
par_m <- matrix(c(-0.265, 0.053,0.7693), 6,1)

dim(estimates)

121.135**2+875788.198
0.093**2 + 1.317
0.007*0.007 + 0.017

install.packages('tinytex')
tinytex::install_tinytex()
